module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (attribute, class, for, id, name, placeholder, style, type_, value)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate, initialSeed, step)
import Array exposing (Array, fromList, get, slice)
import Random.Array exposing (shuffle)
import Random.List exposing (shuffle)
import Random.Extra exposing (bool)
import Time
import Bitwise exposing (shiftLeftBy, or)

import Wordlist exposing (wordlistAdvanced)
import BigInt exposing (BigInt, toString, divmod, fromHexString, fromIntString)
import Hex exposing (toString)

-- MAIN

main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL
type alias Card =
  { word : String
  , team : Int -- 0 = spectator, -1 = assassin, 1 = red, 2 = blue
  , uncovered : Bool
  }

type alias Model =
    { seed : Seed
    , turn : Bool -- True = red, False = blue
    , currentTimer : Int
    , debugString : String
    , toggleLightbox : Bool     -- True = show
    , toggleQR : Bool           -- True = show
    , toggleSidebar : Bool      -- True = show
    , toggleSoundEffects : Bool -- True = show
    , toggleSpies : Bool        -- True = show
    , redRemaining : Int
    , blueRemaining : Int
    , password : String
    , allWords : List (String)
    , cards : List (Card)
    }


init : () -> (Model, Cmd Msg)
init _ =
    update NewGame 
      (Model
        (Random.initialSeed 99999999)
        False
        0
        "debug"
        False
        False
        True
        False
        True
        1
        1
        "PASSWORD"
        wordlistAdvanced
        [ Card "Gate" 2 False, Card "Quilt" -1 False, Card "Party" 0 False, Card "Adjustment" 1 False, Card "Cloth" 2 False,
        Card "Orange" 0 False, Card "Rod" 1 False, Card "Tomatoes" 2 False, Card "Flowers" 1 False, Card "Rabbits" 2 False,
        Card "Crook" 2 False, Card "Toad" 2 False, Card "Order" 0 False, Card "Scissors" 1 False, Card "Tank" 1 False,
        Card "Hotdog" 1 False, Card "Scent" 2 False, Card "Distance" 0 False, Card "Stitch" 2 False, Card "Suit" 0 False,
        Card "Squirrel" 0 False, Card "Design" 0 False, Card "Business" 1 False, Card "Flesh" 1 False, Card "Beef" 2 False ]
      )


-- UPDATE


type Msg
      --| SetColor String
      --| UpdateSettings
    = UncoverCard Int
    | ToggleLightbox
    | ToggleQR
    | ToggleSidebar
    | ToggleSoundEffects
    | ToggleSpies
    | PassTurn
    | NewGame
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
      UncoverCard index ->
        let
          newCards = uncover 0 index model.cards
          redRemaining = List.length (List.filter hiddenRed newCards)
          blueRemaining = List.length (List.filter hiddenBlue newCards)
        in
          ( { model | cards = newCards, redRemaining = redRemaining, blueRemaining = blueRemaining },
          Cmd.none)

      ToggleLightbox ->
          ( { model | toggleLightbox = not model.toggleLightbox }
          , Cmd.none)
          
      ToggleQR ->
          ( { model | toggleQR = not model.toggleQR }
          , Cmd.none)

      ToggleSidebar ->
          ( { model | toggleSidebar = not model.toggleSidebar }
          , Cmd.none)

      ToggleSoundEffects ->
          ( { model | toggleSoundEffects = not model.toggleSoundEffects }
          , Cmd.none)

      ToggleSpies ->
          ( { model | toggleSpies = not model.toggleSpies }
          , Cmd.none)

      PassTurn ->
          ( { model | turn = not model.turn }
          , Cmd.none)

      NewGame ->
        let
            (newWords, seed1) = Random.step (Random.List.shuffle model.allWords) model.seed {- to do: use this seed below -}
            (newTurn, seed2) = Random.step Random.Extra.bool seed1
            (newIDs, seed3) = Random.step (Random.Array.shuffle (Array.fromList (List.range 0 24))) seed2
            assassinID = case Array.get 0 newIDs of
                           Just a -> a
                           Nothing -> 0
            redIDs = Array.toList <| Array.slice 1 (if newTurn then 10 else 9) newIDs
            blueIDs = Array.toList <| Array.slice (if newTurn then 10 else 9) 18 newIDs
            newShuffledCards = List.indexedMap (colorCards assassinID redIDs blueIDs) (populateCards model.cards newWords)
            benum = Array.repeat 53 False
            benum2 = Array.set 0 True benum       -- set to initial team
            benum3 = Array.set (51-(2*assassinID)) True (Array.set (50-(2*assassinID)) True benum2)
            benum4 = ammendArray redIDs benum3 True False
            benum5 = ammendArray blueIDs benum4 False True
            myPrime = [True, False, True, False, True, True, False, False, True, True, True, True, False, True, False, False, False, False, True, True, False, False, False, True, True, True, True, False, True, False, False, False, False, True, False, True, True, False, False, False, False, True, False, False, False, False, True, False, True, False, True, True]
            myPrimeX = [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False]
            myWhatever = List.map2 xor myPrime (Array.toList benum5)
            bigint1 = listBoolToBigInt myWhatever
        in
          ( { model | cards = newShuffledCards
                    , seed = seed3
                    , turn = newTurn
                    , redRemaining = List.length (List.filter hiddenRed newShuffledCards)
                    , blueRemaining = List.length (List.filter hiddenBlue newShuffledCards)
                    , password = base32Encode bigint1
                    }
          , Cmd.none)

      Tick newTime ->
        ( model, Cmd.none )

listBoolToBigInt : List (Bool) -> ( BigInt )
listBoolToBigInt digits =
  case BigInt.fromHexString (getHexString digits) of
     Just bi -> bi
     _ -> BigInt.fromInt 0

getHexString : List (Bool) -> String
getHexString digits =
  case digits of
    a :: b :: c :: d :: es -> Hex.toString ((if a then 8 else 0)+(if b then 4 else 0)+(if c then 2 else 0)+(if d then 1 else 0)) ++ getHexString es
    _ -> ""

debuga : List (Bool) -> String
debuga bools =
  case bools of
    b :: bs ->
      (if b then "1" else "0") ++ debuga bs
    
    _ ->
      ""

ammendArray : List (Int) -> Array ( Bool ) -> Bool -> Bool -> Array ( Bool )
ammendArray ids digits msb lsb =
  case ids of
    i :: is ->
      Array.set (51-(2*i)) msb (Array.set (50-(2*i)) lsb (ammendArray is digits msb lsb))

    _ ->
      digits

cShiftLeft : Int -> Int -> Int
cShiftLeft num bits = 
  shiftLeftBy bits (2 * num)


{- Encode a list of booleans representing a binary number into the string representing the number in base32 -}


base32Encode : BigInt -> String
base32Encode input =
  if BigInt.gt input (BigInt.fromInt 0) then
    let
      chars = Array.fromList ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"]
      (d, m) = case BigInt.divmod input (BigInt.fromInt 36) of
                 Just (dd, mm) -> (dd, mm)
                 _ -> (BigInt.fromInt 0, BigInt.fromInt 0)
      mAsInt = Maybe.withDefault 100 (String.toInt (BigInt.toString m))
      -- integer = input // 36
      -- remainder = get (modBy 36 input) chars
    in
      base32Encode d ++ Maybe.withDefault "" (Array.get mAsInt chars)
  else
    ""

{- -}
colorCards : Int -> List ( Int ) -> List ( Int ) -> Int -> Card -> Card
colorCards assassinID redIDs blueIDs index card  =
    if index == assassinID then
      { card | team = -1, uncovered = False }
      else
        if List.member index redIDs then
          { card | team = 1, uncovered = False }
        else
          if List.member index blueIDs then
            { card | team = 2, uncovered = False }
          else
            { card | team = 0, uncovered = False }



{- Add word from `words` to each card in `cards` -}
populateCards cards words =
  case cards of
    c :: cs ->
      case words of
        w :: ws ->
          {c | word = w} :: populateCards cs ws
        _ ->
          {c | word = "Error"} :: populateCards cs []
    
    _ -> []

{- Uncover the 'target' card -}
uncover : Int -> Int -> List (Card) -> List (Card)
uncover index target cards =
  case cards of
    c :: cs ->
      let
          newCard = if index == target then { c | uncovered = True} else c
      in
        newCard :: uncover (index + 1) target cs
    
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



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 Tick
  {-  , Browser.Events.onKeyUp (Json.Decode.map (KeyChanged False) (Json.Decode.field "key" Json.Decode.string))
    , Browser.Events.onKeyDown (Json.Decode.map (KeyChanged True) (Json.Decode.field "key" Json.Decode.string)) -}
    ]


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
      , div [ class "debug" ] [ {- text model.debugString -} ]
      , div
        [ class ("sidebar" ++ (if model.toggleSidebar then " hidden" else ""))]
          [ ul []
            [ li [] [ a [ class "", onClick ToggleSpies ] [ span [ class ("icon " ++ if model.toggleSpies then "checked" else "unchecked")] [], text "Show spies"] ]
            , li [] [ a [ class "", onClick ToggleSoundEffects ] [ span [ class ("icon " ++ if model.toggleSoundEffects then "checked" else "unchecked")] [], text "Enable sound effects"] ]
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
            , div [ class "bottom_right" ] [ div [ class "button" ] [ a [ onClick NewGame ] [ text "New game", span [ class "bottom_span" ] [ text "Click here" ] ] ] ]
            , div [ class "bottom_right bottom_no_stretch" ] [ span [ class "info_button", onClick ToggleLightbox ] [] ]
            ]
          ]
      ]
