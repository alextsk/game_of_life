module Main exposing (..)


import Array exposing (set, get, fromList, Array)
import Html exposing (Html, program, div, h1, input, label, button, aside)
import Html.Attributes as HA exposing (class)
import Json.Decode as Json
import Html.Events  as HE exposing (onClick, onInput, on)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)
import Random


-- TODO: 
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

initialState : State
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

grid : Grid Int
grid = listToGrid gridList

cellHeight : Int -> Int
cellHeight rows = round <| cHeight / (toFloat rows )

cellWidth : Int -> Int
cellWidth cols = round <| cWidth / (toFloat cols)

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
view model = div [HA.class "container"] [
    div [HA.class "header"] [
      h1 [HA.class "header__title"] [text "Conway's game of life " ]
    ],
    aside [HA.class "control-panel"] [
      div [HA.class "form"] [
        div [HA.class "form-element"] [
          label [HA.class "label"] [
            text "columns",
            input [onChange ChangeCols] []
          ]
        ],
        div [HA.class "form-element"] [
          label [HA.class "label"] [
            text "rows",
            input [onChange ChangeRows] []
          ]
        ],
        div [HA.class "form-element"] [
          button [onClick Generate, HA.class "btn"] [text "Generate"]
        ]
      ]
    ],
    
    div [HA.class "content"] [
      svg
      [ version "1.1", width <| toString cWidth, height <| toString cHeight, viewBox "0 0 800 800"
      ] 
      ( Array.toList <| renderGrid  model
      )
    ]
  ] 
  

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second Tick

main : Program Never Model Msg
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

makeCell : Int -> Bool -> State -> Svg msg
makeCell i lit state = 
  rect [
      width <| toString <| cellWidth state.cols, 
      height <| toString <| cellHeight state.rows, 
      x <| toString (i * (cellWidth state.cols)),
      Svg.Attributes.class (if lit then ("on") else ("off"))
      ] []

-- UPDATE 

nextState : Array (Array Int) -> Array (Array number)
nextState currentState = 
  currentState
  |>  Array.indexedMap (\y row -> 
        Array.indexedMap (\x cell -> calcCellState cell x y currentState) row
      )

calcCellState : Int -> Int -> Int -> Array (Array Int) -> number
calcCellState cell x y array =
  let anoc = aliveNeighbours x y array 
  in
    case cell of 
      0 -> if anoc == 3 then 1 else 0
      1 -> if anoc > 3 || anoc < 2 then 0 else 1
      _ -> 1

aliveNeighbours : Int -> Int -> Array (Array Int) -> Int
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
      

