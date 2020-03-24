module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Random exposing (Seed, generate, initialSeed, step)
import Array exposing (Array, fromList, get, slice)
import Random.Array exposing (shuffle)
import Random.List exposing (shuffle)
import Random.Extra exposing (bool)
import Time

import Wordlist exposing (wordlistAdvanced)
import BigInt exposing (BigInt, toString, divmod, fromHexString)
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
        (Random.initialSeed 99999999)      -- seed: to do: randomize
        False                              -- turn: True = red, False = blue
        0                                  -- currentTimer: to do: enable
        ""                                 -- debugString
        False                              -- toggleLightbox: for info, true = open
        False                              -- toggleQR: toggle QR display, true = open
        True                               -- toggleSidebar: toggle sidebar, true = open
        False                              -- toggleSoundEffects: toggle sound effects, if that's ever added
        True                               -- toggleSpies: toggle spy images, true = open
        0                                  -- remainingRed: remaining red cards
        0                                  -- remainingBlue: remaining blue cards
        "PASSWORD"                         -- password: the encoded game board string
        wordlistAdvanced                   -- allWords: list of all possible card words
        (List.repeat 25 (Card "" 0 False)) -- cards: list of 25 cards, initially blank
      )


-- UPDATE


type Msg
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
            (newWords, seed1) = Random.step (Random.List.shuffle model.allWords) model.seed
            (newTurn, seed2) = Random.step Random.Extra.bool seed1
            (newIDs, seed3) = Random.step (Random.Array.shuffle (Array.fromList (List.range 0 24))) seed2
            assassinID = Maybe.withDefault 0 (Array.get 0 newIDs)
            redIDs = Array.toList <| Array.slice 1 (if newTurn then 10 else 9) newIDs
            blueIDs = Array.toList <| Array.slice (if newTurn then 10 else 9) 18 newIDs
            newShuffledCards = List.indexedMap (colorCards assassinID redIDs blueIDs) (populateCards model.cards newWords)
            boolList1 = Array.set 0 newTurn (Array.repeat 53 False)       -- set to initial team
            boolList2 = Array.set (51-(2*assassinID)) True (Array.set (50-(2*assassinID)) True boolList1)
            boolList3 = ammendArray blueIDs (ammendArray redIDs boolList2 True False) False True
            myPrime = [True, False, True, False, True, True, False, False, True, True, True, True, False, True, False, False, False, False, True, True, False, False, False, True, True, True, True, False, True, False, False, False, False, True, False, True, True, False, False, False, False, True, False, False, False, False, True, False, True, False, True, True]
         {- myPrime = [False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False, False] -}
            bigint1 = listBoolToBigInt <| List.map2 xor myPrime (Array.toList boolList3)
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

debugBoolList : List (Bool) -> String
debugBoolList bools =
  case bools of
    b :: bs ->
      (if b then "1" else "0") ++ debugBoolList bs
    _ ->
      ""

ammendArray : List (Int) -> Array ( Bool ) -> Bool -> Bool -> Array ( Bool )
ammendArray ids digits msb lsb =
  case ids of
    i :: is ->
      Array.set (51-(2*i)) msb (Array.set (50-(2*i)) lsb (ammendArray is digits msb lsb))

    _ ->
      digits


{- Encode a large BigInt representing a binary board encoding into the string representing the number in base32 -}
base32Encode : BigInt -> String
base32Encode input =
  if BigInt.gt input (BigInt.fromInt 0) then
    let
      chars = Array.fromList ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"]
      (d, m) = case BigInt.divmod input (BigInt.fromInt 36) of
                 Just (dd, mm) -> (dd, mm)
                 _ -> (BigInt.fromInt 0, BigInt.fromInt 100)
      mAsInt = Maybe.withDefault 100 (String.toInt (BigInt.toString m))
    in
      base32Encode d ++ Maybe.withDefault "" (Array.get mAsInt chars)
  else
    ""

{- If current card index is in the list of Reds, Blues, Spectators, or Assassin, set its color accordingly -}
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
populateCards : List (Card) -> List (String) -> List (Card)
populateCards cards words =
  case cards of
    c :: cs ->
      case words of
        w :: ws ->
          {c | word = w} :: populateCards cs ws
        _ ->
          {c | word = "Not Enough Words!"} :: populateCards cs []
    
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


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every 1000 Tick
  {-  , Browser.Events.onKeyUp (Json.Decode.map (KeyChanged False) (Json.Decode.field "key" Json.Decode.string))
    , Browser.Events.onKeyDown (Json.Decode.map (KeyChanged True) (Json.Decode.field "key" Json.Decode.string)) -}
    ]


-- VIEW
lightboxInfo : List (Html Msg)
lightboxInfo = [ div [ class "leftside"]
                  [
                    h2 [] [ text "Spymaster"] ]
                , div [ class "rightside" ]
                [
                  h2 [] [ text "A Team Game for 4+ People"]
                , div []
                  [ div [] [ text "Divide into two teams and select one player from each team to be the Spymaster. She will have the decoded game board showing which words belong to her team. Copy the provided password or use the QR code to find the correct board with the Decryptor."]
                  , div [] [ text "The Spymaster's job each turn is to provide one word that is not on any card, along with one number, to connect as many words as possible for her team to guess. The number (plus one) determines the maximum number of words that team may guess this turn. At any time, a team can pass, and the other team's Spymaster begins their turn."]
                  , div [] [ text "Each guess is completed by selecting a card, revealing to which team it belongs. If a team selects a word that does not belong to their team, their turn is over. When all of one team's words are found, the team wins. If the *Assassin* is selected, the team loses immediately."]
                  ]
                , h2 [] [ text "Legal & copyright"]
                , div []
                  [ text "The US Government's Form Letter 108 emphasizes that "
                  , blockquote [] [ text "Copyright does not protect the idea for a game, its name or title, or the method or methods for playing it. Nor does copyright protect any idea, system, method, device, or trademark ma­terial involved in developing, merchandising, or playing a game." ]
                  , text "This program is free software: you can redistribute it or modify it under the GNU General Public License, version 3+. The source code is available on Github. Enjoy!"
                  ]
                ]
              ]

view : Model -> Html Msg
view model =
  let
    addCards cards =
     drawCard 0 cards
  in
    div [ class "container" ]
      [ div [ class ("lightbox" ++ (if model.toggleLightbox then " show" else " hidden")), onClick ToggleLightbox ] [ div [] lightboxInfo ]
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
