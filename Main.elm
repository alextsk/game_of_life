module Main exposing (..)

import Debug exposing (log)
import Array exposing (set, get, fromList, Array)
import Html exposing (Html, program, div, h2, input, label, button)
-- import Html.Attributes as HA exposing (..)
import Json.Decode as Json
import Html.Events  as HE exposing (onClick, onInput, on)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Random

-- IDEA:
-- X grid as a list remains only on init, everything accepts an array
--   sphered top-bottom right-left

-- TODO: 
-- X random grid generation (enter dimensions)
-- X do not change size of cell until generated
--   bundle w webpack
--   add styles
--   make a gh-page 


(cWidth, cHeight) = (800, 800)
type alias Grid a = Array (Array a)
type alias State = {cols: Int, rows: Int}
type alias Model = {time: Time, grid: Grid Int, state: State, uiState: State }
type Msg 
    = Tick Time 
    | ChangeRows String 
    | ChangeCols String
    | Generate 
    | GenValue (List (List Int))

initialState = State 8 8

listToGrid : List (List a) -> Grid a
listToGrid grid = 
  Array.fromList <| List.map Array.fromList grid

gridToList : Grid a -> List (List a)
gridToList array =
  Array.toList <| Array.map Array.toList array

gridList : List ( List Int )
gridList = [
  [0,1,1,0,0,0,0,0],
  [1,1,1,0,0,0,1,1],
  [0,1,0,0,1,0,0,1],
  [1,0,1,0,0,0,0,0],
  [1,0,0,0,0,0,0,0],
  [1,1,1,0,0,1,1,0],
  [1,0,1,0,0,0,0,0],
  [1,1,1,0,0,1,0,0]
  ]

grid = listToGrid gridList

cellHeight rows = cHeight // rows 
cellWidth cols = cWidth // cols

init : (Model, Cmd Msg)
init =
  (Model 0 grid initialState initialState, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ({model | time = newTime, grid = nextState <| model.grid }, Cmd.none)

    ChangeRows string -> 
      let state = model.state
          rows =  string |> String.toInt |> Result.withDefault 0 
          newState = {state | rows = rows}
      in 
        ({model | state = newState}, Cmd.none)
      
    ChangeCols string -> 
      let state = model.state
          cols =  string |> String.toInt |> Result.withDefault 0 
          newState = {state | cols = cols}
      in 
        ({model | state = newState}, Cmd.none)
    
    Generate -> 
      let 
        newModel = {model | uiState = model.state}
        latticeGenerator = Random.list newModel.uiState.rows (
          Random.list newModel.uiState.cols (Random.int 0 1)
        )
      in
        (newModel, Random.generate GenValue latticeGenerator)    

    GenValue x -> 
      ({model | grid = listToGrid x}, Cmd.none)

onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
   HE.on "change" <| Json.map handler <| Json.at ["target", "value"] Json.string

view : Model -> Html Msg
view model = 
  div [] [
    h2 [] [text "Conway's game of life " ],
    label [] [
      text "columns",
      input [onChange ChangeCols] []
    ],
    label [] [
      text "rows",
      input [onChange ChangeRows] []
    ],
    button [onClick Generate] [text "Generate"],
    div [] [
      svg
      [ version "1.1", width <| toString cWidth, height <| toString cHeight
      ] 
      ( Array.toList <| renderGrid  model
      )
    ]
  ] 

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


renderGrid : Model -> Array (Html msg)
renderGrid model = 
  Array.indexedMap (\i value -> 
    svg [y <| toString (i * (cellHeight model.uiState.rows))] (Array.toList <| makeRow value model.uiState) 
  ) model.grid

makeRow : Array Int -> {rows: Int, cols: Int} -> Array (Svg.Svg b)
makeRow value state = 
  Array.indexedMap (\i value -> makeCell i (if value==1 then True else False) state) value

makeCell i lit state = 
  rect [
      width <| toString <| cellWidth state.cols, 
      height <| toString <| cellHeight state.rows, 
      x <| toString (i * (cellWidth state.cols)),
      fill (if lit then ("#000000") else ("#ffffff")),
      stroke "#00000022"
      ] []

-- UPDATE 

nextState currentState = 
  currentState
  |>  Array.indexedMap (\y row -> 
        Array.indexedMap (\x cell -> calcCellState cell x y currentState) row
      )

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
      

