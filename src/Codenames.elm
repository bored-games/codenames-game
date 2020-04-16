port module Codenames exposing (Model, Msg(..), init, main, update, inputPort, outputPort, subscriptions, view)

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (Html, div, span, text, h2, blockquote, ul, li, a, main_, textarea, button, strong, br ,p)
import Html.Attributes exposing (class, id, placeholder, value, href, target)
import Html.Events exposing (onClick, onInput, onFocus, onBlur)
import Random exposing (Seed, initialSeed, step)
import Array exposing (Array, fromList, get, slice)
import Random.Array exposing (shuffle)
import Random.List exposing (shuffle)
import Random.Extra exposing (bool)
import Time
import Set exposing (fromList, toList)
import Wordlist exposing (wordlistBasic, wordlistAdvanced, wordlistColors, wordlistHalloween, wordlistDeutsch, wordlistFrancais, wordlistEspanol)
import BigInt exposing (BigInt, toString, divmod, fromHexString)
import Hex exposing (toString)
import Json.Encode
import Json.Decode
import Task

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


-- MODEL
type alias JSONMessage = 
  { action : String 
  , content : Json.Encode.Value
  }


type alias Card =
  { word : String
  , team : Int -- 0 = spectator, -1 = assassin, 1 = red, 2 = blue
  , uncovered : Bool
  }

type alias Wordlist =
  { key: Int
  , name: String
  , include: Bool
  , words: List (String)
  }

type alias Model =
    { seed : Seed
    , turn : Bool -- True = red, False = blue
    , currentTimer : Int
    , blockKeyShortcuts : Bool
    , debugString : String
    , toggleLightbox : Bool     -- True = show
    , toggleQR : Bool           -- True = show
    , toggleSidebar : Bool      -- True = show
    , toggleCustomWordsEntry : Bool -- True = show
    , toggleSoundEffects : Bool -- True = show
    , settings :
      { spies: Bool,
      customWords: Bool }
    , redRemaining : Int
    , blueRemaining : Int
    , password : String
    , customWordsString : String
    , customWords : List (String)
    , wordlists : List (Wordlist)
    , allWords : List (String)
    , cards : List (Card)
    }


init : () -> (Model, Cmd Msg)
init _ =
    (Model
      (Random.initialSeed 99999999)      -- seed: to do: randomize
      False                              -- turn: True = red, False = blue
      0                                  -- currentTimer: to do: enable
      False                              -- blockKeyShortcuts during Focus of textareas
      ""                                 -- debugString
      False                              -- toggleLightbox: for info, true = open
      False                              -- toggleQR: toggle QR display, true = open
      False                              -- toggleSidebar: toggle sidebar, true = open
      False                              -- toggleCustomWordsEntry: toggle edit-custom-words sidebar, true = open
      False                              -- toggleSoundEffects: toggle sound effects, if that's ever added
      { spies = True
      , customWords = True }            -- settings
      0                                  -- remainingRed: remaining red cards
      0                                  -- remainingBlue: remaining blue cards
      ""                                 -- password: the encoded game board string
      ""                                 -- customWordsString: initial textarea string
      []                                 -- customWords: initial list
      [ { key = 0, name = "basic words", include = True, words = wordlistBasic }
      , { key = 1, name = "advanced words", include = False, words = wordlistAdvanced }
      , { key = 2, name = "color words", include = False, words = wordlistColors }
      , { key = 3, name = "Halloween words", include = False, words = wordlistHalloween }
      , { key = 4, name = "German words", include = False, words = wordlistDeutsch }
      , { key = 5, name = "French words", include = False, words = wordlistFrancais }
      , { key = 6, name = "Spanish words", include = False, words = wordlistEspanol }
      ]
      []                                 -- allWords: list of all possible card words
      (List.repeat 25 (Card "" 0 False)) -- cards: list of 25 cards, initially blank
    , Cmd.none)


-- UPDATE


type Msg
    = UncoverCard Int
    | ToggleLightbox
    | ToggleQR
    | ToggleSidebar
    | ToggleCustomWordsEntry
    | ToggleSoundEffects
    | ToggleSpies
    | ToggleWordlist Int
    | ToggleCustomWords
    | SetCustomWords String
    | SaveCustomWords
    | CancelCustomWords
    | PassTurn
    | NewGame
    | Tick Time.Posix
    | ClearUI
    | BlockKeyShortcuts Bool
    | KeyChanged String
    | GetJSON Json.Encode.Value   -- Parse incoming JSON
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
      UncoverCard index ->
        let
          (newTurn, newCards) = uncover 0 index model.cards model.turn
          redRemaining = List.length (List.filter hiddenRed newCards)
          blueRemaining = List.length (List.filter hiddenBlue newCards)
        in
          ( { model | cards = newCards, redRemaining = redRemaining, blueRemaining = blueRemaining, turn = xor model.turn newTurn },
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

      ToggleCustomWordsEntry ->
          ( { model | toggleCustomWordsEntry = not model.toggleCustomWordsEntry }
          , Cmd.none)

      ToggleSoundEffects ->
          ( { model | toggleSoundEffects = not model.toggleSoundEffects }
          , Cmd.none)

      ToggleSpies ->
          let
            oldSettings = model.settings
            newSettings = { oldSettings | spies = not oldSettings.spies }
          in
            ( { model | settings = newSettings }
            , Cmd.none)


      ToggleWordlist key ->
          let
            wordlists = List.map (maybeToggle key) model.wordlists
          in
            ( { model | wordlists = wordlists }
            , Cmd.none)

      ToggleCustomWords ->
          let
            oldSettings = model.settings
            newSettings = { oldSettings | customWords = not oldSettings.customWords }
          in
            ( { model | settings = newSettings }
            , Cmd.none)

      SetCustomWords str ->
          ( { model | customWordsString = str }
          , Cmd.none)

      SaveCustomWords ->
        let
          customWords = Set.toList (Set.fromList  (List.filter isNotEmpty (List.map String.trim (String.split "\n" model.customWordsString))))
          customWordsString = String.join "\n" customWords
        in
          ( { model | customWords = customWords, customWordsString = customWordsString, toggleCustomWordsEntry = False }
          , Cmd.none)

      CancelCustomWords ->
        let
          customWordsString = String.join "\n" model.customWords
        in
          ( { model | customWordsString = customWordsString, toggleCustomWordsEntry = False }
          , Cmd.none)
    
      PassTurn ->
          ( { model | turn = not model.turn }
          , Cmd.none)

      NewGame ->
        let
            wordlist = List.append 
                          (List.concatMap .words (List.filter .include model.wordlists))
                          (if model.settings.customWords then model.customWords else [])
            
            (newWords, seed1) = Random.step (Random.List.shuffle wordlist) model.seed
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
            bigint1 = listBoolToBigInt <| List.map2 xor myPrime (Array.toList boolList3)
            newPassword = base32Encode bigint1
        in
          ( { model | cards = newShuffledCards
                    , seed = seed3
                    , turn = newTurn
                    , redRemaining = List.length (List.filter hiddenRed newShuffledCards)
                    , blueRemaining = List.length (List.filter hiddenBlue newShuffledCards)
                    , password = newPassword
                    , allWords = wordlist
                    }
        , outputPort (Json.Encode.encode 0 (Json.Encode.object [ ("action", Json.Encode.string "new_game"), ("content", Json.Encode.string newPassword) ] ) ) )

      Tick _ ->
        ( { model | currentTimer = model.currentTimer + 1 }, Cmd.none )

      ClearUI -> 
        ( { model | toggleLightbox = False, toggleQR = False, toggleSidebar = False, toggleCustomWordsEntry = False},
          Task.attempt (\_ -> NoOp) (Browser.Dom.blur "custom_words_entry") )

      BlockKeyShortcuts bool -> 
        ( { model | blockKeyShortcuts = bool}, Cmd.none )

      KeyChanged key ->
        let
          command =
            if model.blockKeyShortcuts then
              case key of
                "Escape"     -> Just (update ClearUI model)
                _ -> Nothing
            else
              case key of
                " "      -> Just (update PassTurn model)
                "Escape"     -> Just (update ClearUI model)
                "q"      -> Just (update ToggleQR model)
                "Q"      -> Just (update ToggleQR model)
                "s"      -> Just (update ToggleSidebar model)
                "S"      -> Just (update ToggleSidebar model)
                _ -> Nothing
        in
          case command of
            Just cmd ->
              cmd
            _ ->
              ( model, Cmd.none )


      GetJSON json ->
        case Json.Decode.decodeValue decodeJSON json of
          Ok {action, content} ->
            case action of
              "set_game"   ->
                case Json.Decode.decodeValue Json.Decode.int content of
                Ok num ->
                  update NewGame { model | seed = Random.initialSeed num }
                _ ->
                  update NewGame model

              _ ->
                (Debug.log "Error: unknown code in JSON message" model, Cmd.none ) -- Error: missing code

          Err _ ->
            ( { model | debugString = "Bad JSON: " ++ Json.Encode.encode 0 json}, Cmd.none )

      NoOp ->
          ( model, Cmd.none)



maybeToggle : Int -> Wordlist -> Wordlist
maybeToggle key wordlist =
  if key == wordlist.key then
    { wordlist | include = not wordlist.include}
  else
    wordlist


isNotEmpty : String -> Bool
isNotEmpty str = 
  not (String.isEmpty str)

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

{-
debugBoolList : List (Bool) -> String
debugBoolList bools =
  case bools of
    b :: bs ->
      (if b then "1" else "0") ++ debugBoolList bs
    _ ->
      ""
-}

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

{- Uncover the 'target' card; return new turn and list of new cards -}
uncover : Int -> Int -> List (Card) -> Bool -> (Bool, List (Card))
uncover index target cards turn =
  case cards of
    c :: cs ->
      let
          (switchTurn, newCard) = if index == target then ((turn && c.team /= 1) || (not turn && c.team /= 2), { c | uncovered = True }) else (False, c)
          (switchTurnLater, moreCards) = uncover (index + 1) target cs turn
          newCards = newCard :: moreCards
      in
        (xor switchTurn switchTurnLater, newCards)
    
    _ ->
      (False, [])

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
    [ Time.every 1000 Tick
    , inputPort GetJSON
    , Browser.Events.onKeyUp (Json.Decode.map KeyChanged (Json.Decode.field "key" Json.Decode.string))
    {-- , Browser.Events.onKeyDown (Json.Decode.map (KeyChanged True) (Json.Decode.field "key" Json.Decode.string)) --}
    ]


-- VIEW
lightboxInfo : String -> List (Html Msg)
lightboxInfo password = [ div [ class "instructions" ]
                 [ h2 [] [ text "A team game for 4+ people"]
                 , div []
                   [ p [] [ text "Divide into two teams and select one player from each team to be the Spymaster. The Spymaster has access to the decrypted board showing which words belong to the team. Each turn, they will provide one word that is not on any card, along with one number, to connect as many words as possible for the team to guess. The number (plus one) determines the maximum number of words that team may guess this turn."]
                   , p [] [ text "Each guess is completed by selecting a card, revealing to which team it belongs. If a team selects a word that does not belong to their team, their turn is over. At any time, a team can pass, and the other team's Spymaster begins their turn. When all of one team's words are found, the team wins. If the *Assassin* is selected, the team loses immediately."]
                   ]
                 ]
                 , div [ class "right-side"]
                  [ a  [ href "./spymaster", target "_blank", class "spymaster" ]
                    [ div [ class "spymaster-preview" ] []
                    , div [] [ strong [] [ text "Click here to access the Decryptor. " ]
                              , br [] []
                              , text ("Paste in the password provided at the bottom of the screen (" ++ password ++ ") to find the correct board.") ]
                    ]
                  , div [ class "legal" ]
                    [ h2 [] [ text "Legal & copyright"]
                    , div []
                      [ text "This program is free software: you can redistribute it or modify it under the GNU General Public License, version 3+. The source code is available on Github. Enjoy!"
                      , blockquote [] [ text "Copyright does not protect the idea for a game, its name or title, or the method or methods for playing it. Nor does copyright protect any idea, system, method, device, or trademark ma­terial involved in developing, merchandising, or playing a game." ]
                      ]
                    ]
                  ]
                ]

drawWordlistToggle : Wordlist -> Html Msg
drawWordlistToggle wordlist =
  li [] [ a [ class "", onClick (ToggleWordlist wordlist.key) ] [ span [ class ("icon " ++ if wordlist.include then "checked" else "unchecked")] [], text ("Use " ++ wordlist.name)] ]


view : Model -> Html Msg
view model =
  let
    wordlistToggles = List.map drawWordlistToggle model.wordlists
    addCards cards =
     drawCard 0 cards
  in
    div [ class "container" ]
      [ div [ class ("lightbox" ++ (if model.toggleLightbox then " show" else " hidden")), onClick ToggleLightbox ] [ div [] (lightboxInfo model.password) ]
      , div [ class ("lightbox" ++ (if model.toggleQR then " show" else " hidden")), onClick ToggleQR ] [ div [ id "qrcode" ] [] ]
      , div [ class "debug" ] [ {- text model.debugString -} ]
      , div
        [ class ("sidebar" ++ (if model.toggleSidebar then "" else " hidden"))]
          [ ul []
            [ li [] [ a [ class "", onClick ToggleSpies ] [ span [ class ("icon " ++ if model.settings.spies then "checked" else "unchecked")] [], text "Show spies"] ]
            , li [] [ a [ class "", onClick ToggleSoundEffects ] [ span [ class ("icon " ++ if model.toggleSoundEffects then "checked" else "unchecked")] [], text "Enable sound effects"] ]
            ]
          , ul []
            (List.append
              wordlistToggles
              [ li [] [ a [ class "", onClick ToggleCustomWords ] [ span [ class ("icon " ++ if model.settings.customWords then "checked" else "unchecked")] [], text "Use custom words"] ] ])
          , ul []
            [ li [] [ a [ class "", onClick ToggleCustomWordsEntry ] [ span [ class "icon edit"] [], text "Edit custom wordlist"] ]
            , li [] [ a [ class "", onClick ToggleSidebar ] [ span [ class "icon close"] [], text "Close settings"] ]
            ]
          ]
      , div
        [ class ("customWordsBar" ++ (if model.toggleCustomWordsEntry then "" else " hidden"))]
          [ div [ class "textarea_area" ] [ textarea [ id "custom_words_entry", placeholder "Enter one word per line", onInput SetCustomWords, value model.customWordsString, onFocus (BlockKeyShortcuts True), onBlur (BlockKeyShortcuts False) ] [] ]
          , div [ class "button_area" ]
            [ button [ onClick SaveCustomWords ] [ text "Save" ]
            , button [ onClick CancelCustomWords, class "button--cancel" ] [ text "Cancel" ]
            ]
          ]
      , div [ class "center" ]
        [ main_ [ class ((if model.turn then "red-turn" else "blue-turn") ++ (if model.settings.spies then "" else " hide_spies")) ]
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
