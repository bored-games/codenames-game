module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, id, name, placeholder, style, type_, value)
import Html.Events exposing (onClick)
import Time



-- MAIN

main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL
type alias Card =
  { word : String
  , team : Int
  , uncovered : Bool
  }

type alias Model =
    { turn : Bool
    , currentTimer : Int
    , debugString : String
    , toggleLightbox : Bool
    , toggleQR : Bool
    , toggleSidebar : Bool
    , toggleSpies : Bool
    , redRemaining : Int
    , blueRemaining : Int
    , password : String
    , cards : List (Card)
    }


init : Model
init =
    Model
        False
        0
        "debug"
        False
        False
        True
        True
        1
        1
        "1w6mvdnr6vj"
        [ Card "Gate" 2 False, Card "Quilt" -1 False, Card "Party" 0 False, Card "Adjustment" 1 False, Card "Cloth" 2 False,
        Card "Orange" 0 False, Card "Rod" 1 False, Card "Tomatoes" 2 False, Card "Flowers" 1 False, Card "Rabbits" 2 False,
        Card "Crook" 2 False, Card "Toad" 2 False, Card "Order" 0 False, Card "Scissors" 1 False, Card "Tank" 1 False,
        Card "Hotdog" 1 False, Card "Scent" 2 False, Card "Distance" 0 False, Card "Stitch" 2 False, Card "Suit" 0 False,
        Card "Squirrel" 0 False, Card "Design" 0 False, Card "Business" 1 False, Card "Flesh" 1 False, Card "Beef" 2 False ]



-- UPDATE


type Msg
      --| SetColor String
      --| UpdateSettings
    = UncoverCard Int
    | ToggleLightbox
    | ToggleQR
    | ToggleSidebar
    | ToggleSpies
    | PassTurn
    | NewGame


update : Msg -> Model -> Model
update msg model =
    case msg of
        UncoverCard index ->
          let
              newCards = uncover 0 index model.cards
              redRemaining = List.length (List.filter hiddenRed newCards)
              blueRemaining = List.length (List.filter hiddenBlue newCards)
          in
            { model | cards = newCards, redRemaining = redRemaining, blueRemaining = blueRemaining }

        ToggleLightbox ->
            { model | toggleLightbox = not model.toggleLightbox }
            
        ToggleQR ->
            { model | toggleQR = not model.toggleQR }

        ToggleSidebar ->
            { model | toggleSidebar = not model.toggleSidebar }

        ToggleSpies ->
            { model | toggleSpies = not model.toggleSpies }

        PassTurn ->
            { model | turn = not model.turn }

        NewGame ->
            { model | turn = False }


uncover index target cards =
  case cards of
    c :: cs ->
      let
          newCard = if index == target then { c | uncovered = True} else c
      in
        newCard :: (uncover (index + 1) target cs)
    
    _ ->
      []

hiddenRed : Card -> Bool
hiddenRed c =
  not (.uncovered c) && (.team c == 1)

hiddenBlue : Card -> Bool
hiddenBlue c =
  not (.uncovered c) && (.team c == 2)

drawCard : Int -> List (Card) -> List (Html Msg)
drawCard index cards =
  case cards of
    c :: cs ->
      div [ class ("card_border team-" ++ String.fromInt (.team c) ++ if .uncovered c then " uncovered" else " covered")
          , id ("c" ++ String.fromInt index)
          , onClick (UncoverCard index) ]
      [ div [ class "card" ]
        [ div [ class "card_top" ]
          [ div [ class "spy" ] [] ]
        , span [ class "wordbox" ]
          [ span [] [ text (.word c) ] ]
        ]
      ] :: drawCard (index+1) cs

    _ ->
      []


countRemainingByTeam : List (Card) -> Int -> Int
countRemainingByTeam cards team =
  3

-- VIEW

view : Model -> Html Msg
view model =
  let
    addCards cards =
     drawCard 0 cards
  in
    div [ class "container" ]
      [ div [ class ("lightbox" ++ (if model.toggleLightbox then " show" else " hidden")), onClick ToggleLightbox ] [ div [] [] ]
      , div [ class ("lightbox" ++ (if model.toggleQR then " show" else " hidden")), onClick ToggleQR ] [ div [] [] ]
      , div [ class "debug" ] []
      , div
        [ class ("sidebar" ++ (if model.toggleSidebar then " hidden" else ""))]
          [ ul []
            [ li [] [ a [ class "", onClick ToggleSpies ] [ span [ class ("icon " ++ if model.toggleSpies then "checked" else "unchecked")] [], text "Show spies"] ]
            ]
          , ul []
            [ li [] [ a [ class "" ] [ span [ class "icon checked"] [], text "Use default words"] ]
            , li [] [ a [ class "" ] [ span [ class "icon checked"] [], text "Use adult words"] ]
            , li [] [ a [ class "" ] [ span [ class "icon checked"] [], text "Use custom words"] ]
            ]
          , ul []
            [ li [] [ a [ class "" ] [ span [ class "icon edit"] [], text "Edit custom wordlist (to do)"] ]
            , li [] [ a [ class "", onClick ToggleSidebar ] [ span [ class "icon close"] [], text "Close settings"] ]
            ]
          ]
      , div [ class "center" ]
        [ main_ [ class ((if model.turn then "red-turn" else "blue-turn") ++ (if model.toggleSpies then "" else " hide_spies")) ]
          (addCards model.cards)
        {- [ div [ class "card_border", id "c00" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Four" ] ] ] ]
          , div [ class "card_border", id "c01" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Score" ] ] ] ]
          , div [ class "card_border", id "c02" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "And" ] ] ] ]
          , div [ class "card_border", id "c03" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Seven" ] ] ] ]
          , div [ class "card_border", id "c04" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Years" ] ] ] ]
          , div [ class "card_border", id "c05" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Ago" ] ] ] ]
          , div [ class "card_border", id "c06" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Our" ] ] ] ]
          , div [ class "card_border", id "c07" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Fathers" ] ] ] ]
          , div [ class "card_border", id "c08" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Brought" ] ] ] ]
          , div [ class "card_border", id "c09" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Forth" ] ] ] ]
          , div [ class "card_border", id "c10" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "On" ] ] ] ]
          , div [ class "card_border", id "c11" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "This" ] ] ] ]
          , div [ class "card_border", id "c12" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Continent" ] ] ] ]
          , div [ class "card_border", id "c13" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "A" ] ] ] ]
          , div [ class "card_border", id "c14" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "New" ] ] ] ]
          , div [ class "card_border", id "c15" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Nation" ] ] ] ]
          , div [ class "card_border", id "c16" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Conceived" ] ] ] ]
          , div [ class "card_border", id "c17" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "In" ] ] ] ]
          , div [ class "card_border", id "c18" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Liberty" ] ] ] ]
          , div [ class "card_border", id "c19" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "And" ] ] ] ]
          , div [ class "card_border", id "c20" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Dedicated" ] ] ] ]
          , div [ class "card_border", id "c21" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "To" ] ] ] ]
          , div [ class "card_border", id "c22" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "The" ] ] ] ]
          , div [ class "card_border", id "c23" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "Proposition" ] ] ] ]
          , div [ class "card_border", id "c24" ] [ div [ class "card" ] [ div [ class "card_top" ] [ div [ class ("spy" ++ if model.toggleSpies then "" else " hidden") ] [], div [ class "decor_line" ] [] ], span [ class "wordbox" ] [ span [] [ text "That" ] ] ] ]
          ] -}
        , div [ class "bottom" ]
            [ div [ class "bottom_left bottom_no_stretch" ]
              [ span [ class "settings_button", onClick ToggleSidebar ] [] ]
            , div [ class "bottom_left" ]
              [ span [ class "turn_text button", onClick PassTurn ]
                [ text ((if model.turn then "Red" else "Blue") ++ " team's turn!")
                , span [ class "bottom_span" ] [ text "Click here or press space to pass" ]
                ]
              ]
            , div [ class "bottom_left cards_remaining" ]
                [ div [ class "red_remaining" ] [ text (String.fromInt model.redRemaining ++ " remaining") ]
                , div [ class "blue_remaining" ] [ text (String.fromInt model.blueRemaining ++ " remaining") ]
                ]
            , div [ class "bottom_right" ] [ div [ class "button" ] [ a [ onClick ToggleQR ] [ span [ class "password" ] [ text model.password ], span [ class "bottom_span" ] [ text "Click for QR code" ] ] ] ]
            , div [ class "bottom_right" ] [ div [ class "button" ] [ a [] [ text "New game", span [ class "bottom_span" ] [ text "Click here" ] ] ] ]
            , div [ class "bottom_right bottom_no_stretch" ] [ span [ class "info_button", onClick ToggleLightbox ] [] ]
            ]
          ]
      ]
