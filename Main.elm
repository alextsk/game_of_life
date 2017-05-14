module Main exposing (..)

import Debug exposing (log)
import Array exposing (set, get, fromList, Array)
import Html exposing (Html, program)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)

(cWidth, cHeight) = (800, 800)

type alias Grid a = List (List a)
type alias Model = (Time, Grid Int)
type Msg = Tick Time

grid : Grid Int
grid = [
  [1,1,1,0,0,0],
  [0,0,0,0,0,0],
  [0,0,1,1,1,0],
  [0,1,1,1,0,0],
  [0,0,0,0,0,0],
  [0,0,0,0,0,0]
  ]

cellSize = cWidth // (List.length grid)

init : (Model, Cmd Msg)
init =
  ((0, grid), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ((newTime, gridToList <| nextState <| gridToArray <| Tuple.second model), Cmd.none)

view : Model -> Html Msg
view model = 
  svg
    [ version "1.1", width <| toString cWidth, height <| toString cHeight
    ] 
    ( Array.toList <| makeGrid <| gridToArray <| Tuple.second model 
    )

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second Tick

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- VIEW 


gridToArray : Grid a -> Array (Array a)
gridToArray grid = 
  Array.fromList <| List.map Array.fromList grid

gridToList : Array (Array a) -> Grid a
gridToList array =
  Array.toList <| Array.map Array.toList array

makeGrid : Array (Array Int) -> Array (Html msg)
makeGrid = 
  Array.indexedMap (\i value -> 
    svg [y <| toString (i * (cellSize + 1))] (Array.toList <| makeRow value) 
  )

makeRow = 
  Array.indexedMap (\i value -> makeCell i (if value==1 then True else False)) 

makeCell i lit = 
  rect [
      width <| toString cellSize, 
      height <| toString cellSize, 
      x <| toString (i * (cellSize + 1)),
      fill (if lit then ("#000000") else ("#ffffff")),
      stroke "#00000022"
      ] []

-- UPDATE 

nextState currentState = 
  currentState
  |> Array.indexedMap (\y row -> 
      Array.indexedMap (\x cell -> calcCellState cell x y currentState) row)

calcCellState cell x y array =
  let anoc = aliveNeighbours x y array 
  in
    case cell of 
      0 -> if anoc == 3 then 1 else 0
      1 -> if anoc > 3 || anoc < 2 then 0 else 1
      _ -> 1

aliveNeighbours x y array =
  let 
      getRow y =
        case get y array of 
          Maybe.Nothing -> fromList []
          Maybe.Just a -> a

      prevRow = getRow (y - 1) 
      currRow = getRow y 
      nextRow = getRow (y + 1) 

      neighbours = 
        get (x - 1) prevRow ::
        get (  x  ) prevRow ::
        get (x + 1) prevRow ::
        get (x - 1) currRow ::
        get (x + 1) currRow ::
        get (x - 1) nextRow ::
        get (  x  ) nextRow ::
        get (x + 1) nextRow :: 
        [Maybe.Nothing]
  in 
    List.foldl maybeToInt 0 neighbours

maybeToInt : Maybe Int -> Int -> Int
maybeToInt curr acc =
      case curr of 
        Nothing -> acc + 0
        Just x -> acc + x
      

