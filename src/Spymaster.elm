port module Main exposing (Model, Msg(..), init, inputPort, main, outputPort, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (id, style, type_, attribute, placeholder, value, class, name, for)
import Html.Events exposing (onInput, onSubmit, onClick)

import Time
import Json.Encode
import Json.Decode



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias JSONMessage = 
  { action : String 
  , content : Json.Encode.Value
  }

type alias Model =
  { nameInProgress : String
  , board : List (Int)
  , lastCell : Int
  , turn : Int
  , currentTimer : Int
  , debugString : String
  , toggleSidebar : Bool
  }

buildDefault : List (Int)
buildDefault = 
  [-1,  0,  0, -1, -1, -1, -1, -1, -1, -1,  0,  0, -1,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  -1,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, -1,
  -1, -1, -1,  0,  0,  0,  0,  0,  0,  0, -1, -1, -1]

init : () -> (Model, Cmd Msg)
init _ =
  (Model
    "board not really implemented"
    buildDefault
    3
    1
    0
    "agpgenpaiengapie"
    True
  , Cmd.none )



-- UPDATE


type Msg
  = SetName String
  --| SetColor String
  --| UpdateSettings
  --| SetMessage String
  --| SendMessage
  | ToggleSidebar
  | NewGame
  --| SetScore Int Int
  --| ToggleEmoticons
  --| InsertEmoticon String
  | Tick Time.Posix
  | Ping Time.Posix
  | GetJSON Json.Encode.Value              -- Parse incoming JSON
  | ConnectToServer Json.Encode.Value      -- 000
  --| GetBoard Json.Encode.Value             -- 100
  --| GetPegs Json.Encode.Value         -- 101
  --| GetScore Json.Encode.Value         -- 200
  --| GetChat Json.Encode.Value              -- 202
  --| SetActiveColor (Maybe Color)
  | AddMove Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case Debug.log "MESSAGE: " msg of
    SetName name ->
      ( { model | nameInProgress = name }, Cmd.none )
      
    ToggleSidebar ->
        ( { model | toggleSidebar = not model.toggleSidebar }
        , Cmd.none
        )
      

    NewGame -> -- TODO!
      ( { model | nameInProgress = "New game" }, Cmd.none )

    Tick newTime ->
      let
          currentTimerDisplay = model.currentTimer + 1
      in
          ( { model | currentTimer = currentTimerDisplay }, Cmd.none )

    Ping newTime ->
      ( { model | currentTimer = (model.currentTimer + 1) }
        , outputPort (Json.Encode.encode
                        0
                      ( Json.Encode.object
                      [ ( "action", Json.Encode.string "ping"),
                        ( "content", Json.Encode.string "ping" ) ] ) )
      )

    GetJSON json ->
      case (Json.Decode.decodeValue decodeJSON json) of
        Ok {action, content} ->
          case action of
            "update_chat" ->
              ((Debug.log "Error: unknown code in JSON message" model), Cmd.none ) -- Error: missing code
            _ ->
              ((Debug.log "Error: unknown code in JSON message" model), Cmd.none ) -- Error: missing code

        Err _ ->
          ( { model | debugString = ("Bad JSON: " ++ (Json.Encode.encode 0 json))}, Cmd.none )

    ConnectToServer json ->
      ( model,
        outputPort
          ( Json.Encode.encode
            0
            ( Json.Encode.object
              [ ("action", Json.Encode.string "create_user")
              , ("content", Json.Encode.string "") ] ))
        )


    -- Add move.
    AddMove n ->
      let
        newTurn = -1*model.turn+3
        newBoard = updateBoard 0 n newTurn model.board
      in
        ( { model | debugString = String.fromInt n, turn = newTurn, board = newBoard}, 
          outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "submit_movelist")
                , ("content", Json.Encode.string "todo") ] ))
        )
    

updateBoard : Int -> Int -> Int -> List (Int) -> List (Int)
updateBoard current target value board =
  case board of
    x::xs ->
      if current == target then
        value :: xs
      else
        x :: updateBoard (current+1) target value xs
    _ ->
      []

decodeJSON : Json.Decode.Decoder JSONMessage
decodeJSON =
  Json.Decode.map2
    JSONMessage
    (Json.Decode.field "action" Json.Decode.string)
    (Json.Decode.field "content" Json.Decode.value)


-- SUBSCRIPTIONS

port outputPort : (String) -> Cmd msg
port inputPort : (Json.Encode.Value -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Time.every 50000 Ping
    , Time.every 1000 Tick
    , inputPort GetJSON
    ]

{- 
        <div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div>
        <div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div>
        <div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div>
        <div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s red last"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s blue"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div>
        <div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s red"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div>
        <div class="c"></div><div class="c"></div><div class="c"></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"><div class="s"></div></div><div class="c"></div><div class="c"></div><div class="c"></div>
-}

-- VIEW
drawCells : Int -> List (Int) -> List (Html Msg)
drawCells index remainingCells =
  case remainingCells of
    [] ->
      []
    v::vs ->
      case v of
        0 ->
          div [class "c"]
          [ div [ class "s", onClick (AddMove index) ] [] ] :: drawCells (index+1) vs
        1 ->
          div [class "c"]
          [ div [ class "s red" ] [] ] :: drawCells (index+1) vs
        2 ->
          div [class "c"]
          [ div [ class "s blue" ] [] ] :: drawCells (index+1) vs
        _ ->
          div [class "c"] [] :: drawCells (index+1) vs

{- div [ class "settings_button" ] [ text "S" ] -}
view : Model -> Html Msg
view model =
  let
    drawBoard board = drawCells 0 board

  in 
    div [ class "container"]
    [ div [ class "debug" ] []
    , div [ class ("sidebar" ++ if model.toggleSidebar then " hidden" else ""), onClick ToggleSidebar]
    [ text " yo "
    , a [ class "sidebar_tab" ] [ ]
    ]
    , div [ class "center" ]
      [ main_ []
        [ div [ class "card_border", id "c00" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Four" ] ] ] ]
        , div [ class "card_border", id "c01" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Score" ] ] ] ]
        , div [ class "card_border", id "c02" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "And" ] ] ] ]
        , div [ class "card_border", id "c03" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Seven" ] ] ] ]
        , div [ class "card_border", id "c04" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Years" ] ] ] ]
        , div [ class "card_border", id "c05" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Ago" ] ] ] ]
        , div [ class "card_border", id "c06" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Our" ] ] ] ]
        , div [ class "card_border", id "c07" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Fathers" ] ] ] ]
        , div [ class "card_border", id "c08" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Brought" ] ] ] ]
        , div [ class "card_border", id "c09" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Forth" ] ] ] ]
        , div [ class "card_border", id "c10" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "On" ] ] ] ]
        , div [ class "card_border", id "c11" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "This" ] ] ] ]
        , div [ class "card_border", id "c12" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Continent" ] ] ] ]
        , div [ class "card_border", id "c13" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "A" ] ] ] ]
        , div [ class "card_border", id "c14" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "New" ] ] ] ]
        , div [ class "card_border", id "c15" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Nation" ] ] ] ]
        , div [ class "card_border", id "c16" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Conceived" ] ] ] ]
        , div [ class "card_border", id "c17" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "In" ] ] ] ]
        , div [ class "card_border", id "c18" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Liberty" ] ] ] ]
        , div [ class "card_border", id "c19" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "And" ] ] ] ]
        , div [ class "card_border", id "c20" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Dedicated" ] ] ] ]
        , div [ class "card_border", id "c21" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "To" ] ] ] ]
        , div [ class "card_border", id "c22" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "The" ] ] ] ]
        , div [ class "card_border", id "c23" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Proposition" ] ] ] ]
        , div [ class "card_border", id "c24" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class "spy" ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "That" ] ] ] ]
        ]
      , div [ class "bottom" ]
        [ div [ class "bottom_left cards_remaining" ]
          [ span [ class "red_remaining" ] [ text "0 remaining" ]
          ,  span [ class "blue_remaining" ] [ text "10 remaining" ]
          ]
        ,  div [ class "bottom_left" ]
          [ span [ class "turn_text" ] [ text "Red team's turn!", span [ class "bottom_span"] [ text "Press space to pass" ] ]
          ]
        , div [ class "bottom_center" ]
          [ a [ class "pass" ]
            [ text "Pass Turn"
            , span [] [ text "Spacebar" ]
            ]
          ]
        , div [ class "bottom_right" ] [ a [] [ text "New game", span [ class "bottom_span"] [ text "Click here" ] ] ]
        , div [ class "bottom_right" ] [ a [] [ text model.debugString, span [ class "bottom_span"] [ text "Click for QR code" ] ] ]
        ]
      ]
    ]