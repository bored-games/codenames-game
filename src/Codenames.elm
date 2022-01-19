port module Codenames exposing (Model, Msg(..), init, main, update, inputPort, outputPort, subscriptions, view)

import Toast

import Browser
import Browser.Dom
import Browser.Events
import Dict
import Html exposing (Html, div, span, text, h2, h3, h4, blockquote, ul, li, a, main_, textarea, button, strong, br, p, em, input, option, select)
import Html.Attributes exposing (class, type_, id, placeholder, value, href, target, attribute, disabled, selected)
import Html.Events exposing (onClick, onInput, onFocus, onBlur)
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

import Chat exposing (Chatline)
import User exposing (User, getUserByName)

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

type alias CardTwo =
  { word : String
  , team : Maybe Int
  , id : Int
  }

type alias Status =
  { game_over : Bool
  , turn : Bool -- True = red, False = blue
  , text : String
  , clue : Maybe String
  , remaining_guesses: Int
  }

type alias BoardInfo =
  { password: String,
    redRemaining: Int,
    blueRemaining: Int,
    cards: List (CardTwo)
  }

type alias Wordlist =
  { key: Int
  , name: String
  , include: Bool
  , words: List (String)
  }

type alias Model =
    { chat : List Chatline
    , currentTimer : Int
    , blockKeyShortcuts : Bool
    , debugString : String
    , topMessage : String
    , toastMessages :  Toast.Stack Toast.Toast
    , toggleLightbox : Bool     -- True = show
    , toggleQR : Bool          
    , toggleSidebar : Bool     
    , toggleCustomWordsEntry : Bool
    , toggleSoundEffects : Bool
    , toggleSpymasterModal : Bool
    , toggleTeamModal : Bool
    , settings :
      { spies: Bool, customWords: Bool, override: Bool }
    , redRemaining : Int
    , blueRemaining : Int
    , password : String
    , customWordsString : String
    , customWords : List (String)
    , wordlists : List (Wordlist)
    , allWords : List (String)
    , cards : List (Card)
    , user : Maybe User
    , users : List (User)
    , red_spymaster : Maybe User
    , blue_spymaster : Maybe User
    , status : Status
    , clueInProgress : String
    , guessesInProgress : String
    }


init : () -> (Model, Cmd Msg)
init _ =
    (Model
      [ ]                                -- chat
      0                                  -- currentTimer: to do: enable
      False                              -- blockKeyShortcuts during Focus of textareas
      ""                                 -- debugString
      ""                                 -- topMessage
      Toast.initialState                 -- initial toasts
      False                              -- toggleLightbox: for info, true = open
      False                              -- toggleQR: toggle QR display, true = open
      False                              -- toggleSidebar: toggle sidebar, true = open
      False                              -- toggleCustomWordsEntry: toggle edit-custom-words sidebar, true = open
      False                              -- toggleSoundEffects: toggle sound effects, if that's ever added
      False                              -- toggleSpymasterModal
      True                               -- toggleTeamModal
      { spies = False, customWords = True, override = False }            -- settings
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
      Nothing
      []
      Nothing                            -- red spymaster
      Nothing                            -- blue spymaster
      (Status True False "Connecting..." Nothing 0) -- status_msg
      ""
      ""
    , Cmd.none)


-- UPDATE


type Msg
    = ToggleLightbox
    | ToggleQR
    | ToggleSidebar
    | ToggleCustomWordsEntry
    | ToggleSpymasterModal
    | ToggleTeamModal
    | ToggleSoundEffects
    | ToggleSpies
    | ToggleOverride
    | ToggleWordlist Int
    | ToggleCustomWords
    | SetCustomWords String
    | SaveCustomWords
    | CancelCustomWords
    | PassTurn
    | NewGame
    | ClearUI
    | BlockKeyShortcuts Bool
    | KeyChanged String
    | Tick Time.Posix
    | Ping Time.Posix
    | GetJSON Json.Encode.Value   -- Parse incoming JSON
    | GetUsersList Json.Encode.Value
    | GetUser Json.Encode.Value
    | GetSpymasters Json.Encode.Value
    | GetSpymasterModal Json.Encode.Value
    | GetBoard Json.Encode.Value
    | GetChat Json.Encode.Value
    | GetStatus Json.Encode.Value
    | GetFlashMessage Json.Encode.Value
    | GetPassword Json.Encode.Value
    | AddToastMessage (Toast.Msg Toast.Toast)
    | SetTeam Int
    | SetSpymaster Int
    | UncoverCard Int
    | SubmitClue
    | SetClue String
    | SetGuesses String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    case msg of
      ToggleLightbox ->
          ( { model | toggleLightbox = not model.toggleLightbox }
          , Cmd.none)
          
      ToggleQR ->
          ( { model | toggleQR = not model.toggleQR }
          , Cmd.none)

      ToggleSidebar ->
          ( { model | toggleSidebar = not model.toggleSidebar }
          , Cmd.none)

      ToggleSpymasterModal ->
          ( { model | toggleSpymasterModal = not model.toggleSpymasterModal }
          , Cmd.none)

      ToggleTeamModal ->
          ( { model | toggleTeamModal = not model.toggleTeamModal }
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
            
      ToggleOverride ->
          let
            oldSettings = model.settings
            newSettings = { oldSettings | override = not oldSettings.override }
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
        ( model, outputPort
        ( Json.Encode.encode
          0
          ( Json.Encode.object
            [ ("action", Json.Encode.string "game_action")
            , ("content", Json.Encode.object
              [ ("action", Json.Encode.string "pass_turn"),
                ("content", Json.Encode.string "") ] ) ] ) ) )

      NewGame ->
        ( model, outputPort
        ( Json.Encode.encode
          0
          ( Json.Encode.object
            [ ("action", Json.Encode.string "game_action")
            , ("content", Json.Encode.object
              [ ("action", Json.Encode.string "new_game"),
                ("content", Json.Encode.string "") ] ) ] ) ) )

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

      GetUsersList json ->
        case Json.Decode.decodeValue User.decodeUsersList json of
          Ok usersList ->
            ( { model | users = Dict.values usersList}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Error parsing userlist JSON"}, Cmd.none )
            
      GetUser json ->
        case Json.Decode.decodeValue User.decodeUser json of
          Ok user ->
            ( { model | user = Just user}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Error parsing user JSON"}, Cmd.none )
            
      GetSpymasters json ->
        case Json.Decode.decodeValue decodeSpymasters json of
          Ok (red_sm, blue_sm) ->
            let
              users = model.users
            in
              ( { model | red_spymaster = getUserByName red_sm users, blue_spymaster = getUserByName blue_sm users}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Error parsing spymasters JSON"}, Cmd.none )
            
      GetSpymasterModal json ->
        case Json.Decode.decodeValue Json.Decode.bool json of
          Ok (val) ->
            ( { model | toggleSpymasterModal = val}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Error parsing spymasters JSON"}, Cmd.none )

      GetBoard json ->
        case Json.Decode.decodeValue decodeBoardInfo json of
          Ok boardInfo ->
            let
              newcards = List.map toCard boardInfo.cards
            in
              ( { model | password = boardInfo.password, cards = newcards, redRemaining = boardInfo.redRemaining, blueRemaining = boardInfo.blueRemaining}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Critical error getting board"}, Cmd.none )
            
      GetChat json ->
        case Json.Decode.decodeValue Chat.decodeChatline json of
          Ok chatline ->
            ( { model | chat = chatline::model.chat}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Error parsing chat JSON"}, Cmd.none )
            
      GetStatus json ->
        case Json.Decode.decodeValue decodeStatus json of
          Ok status ->
            let
              turn = status.turn
            in
              ( { model | status = status}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Error parsing status JSON"}, Cmd.none )

      GetFlashMessage json ->
        case Json.Decode.decodeValue Json.Decode.string json of
          Ok message ->
            ( { model | debugString = message}, Cmd.none )
              |> addToast (Toast.Success "" message)
          Err _ ->
            ( { model | debugString = "Error parsing Flash Message JSON"}, Cmd.none )

      GetPassword json ->
        case Json.Decode.decodeValue Json.Decode.string json of
          Ok password ->
            ( { model | password = password}, Cmd.none )
          Err _ ->
            ( { model | debugString = "Error parsing Password JSON"}, Cmd.none )
            
      AddToastMessage subMsg ->
          Toast.update toastConfig AddToastMessage subMsg model
          
      Tick _ ->
        ( { model | currentTimer = model.currentTimer + 1 }, Cmd.none )

      Ping newTime ->
        ( { model | currentTimer = (model.currentTimer + 1) }
          , outputPort (Json.Encode.encode
                          0
                        ( Json.Encode.object
                        [ ( "action", Json.Encode.string "ping"),
                          ( "content", Json.Encode.string "ping" ) ] ) )
        )

      GetJSON json ->
        case Json.Decode.decodeValue decodeJSON json of
          Ok {action, content} ->
            case action of
              "update_scoreboard" ->
                update (GetUsersList content) model
              "update_user" ->
                update (GetUser content) model
              "update_board" ->
                update (GetBoard content) model
              "update_status" ->
                update (GetStatus content) model
              "update_spymasters" ->
                update (GetSpymasters content) model
              "update_spymaster_modal" ->
                update (GetSpymasterModal content) model
              "connect_to_server" ->
                ( model, Cmd.none )
              "update_chat" ->
                update (GetChat content) model
              "player_chat_new_message" ->
                update (GetChat content) model
              "system_chat_new_message" ->
                update (GetChat content) model
              "system_chat_to_player_new_message" ->
                update (GetChat content) model
              "update_flash_msg" ->
                update (GetFlashMessage content) model
              "new_game" ->
                update (GetPassword content) model
              "ping" ->
                ( model, Cmd.none )

              _ ->
                (Debug.log "Error: unknown code in JSON message" model, Cmd.none ) -- Error: missing code

          Err _ ->
            ( { model | debugString = "Bad JSON: " ++ Json.Encode.encode 0 json}, Cmd.none )

      SetTeam team ->
        ( model, outputPort
              ( Json.Encode.encode
                0
                ( Json.Encode.object
                  [ ("action", Json.Encode.string "game_action")
                  , ("content", Json.Encode.object
                    [ ("action", Json.Encode.string "set_team"),
                      ("content", Json.Encode.int team) ] ) ] ) ) )

      SetSpymaster team ->
        ( model, outputPort
              ( Json.Encode.encode
                0
                ( Json.Encode.object
                  [ ("action", Json.Encode.string "game_action")
                  , ("content", Json.Encode.object
                    [ ("action", Json.Encode.string "set_spymaster"),
                      ("content", Json.Encode.int team) ] ) ] ) ) )
                      
      UncoverCard id ->
        ( model,
          outputPort
            ( Json.Encode.encode
              0
              ( Json.Encode.object
                [ ("action", Json.Encode.string "game_action")
                , ("content", Json.Encode.object
                  [ ("action", Json.Encode.string "uncover_card"),
                    ("content", Json.Encode.object
                    [ ("id", Json.Encode.int id)
                    , ("override", Json.Encode.bool model.settings.override)
                    ] ) ] ) ] ) ) )

      SubmitClue ->
        let
          clue = model.clueInProgress
          guesses = model.guessesInProgress
        in
          ( { model | clueInProgress = "", guessesInProgress = ""}, outputPort
                ( Json.Encode.encode
                0
                ( Json.Encode.object
                  [ ("action", Json.Encode.string "game_action")
                  , ("content", Json.Encode.object
                    [ ("action", Json.Encode.string "submit_clue"),
                      ("content", Json.Encode.object
                      [ ("clue", Json.Encode.string clue)
                      , ("guesses", Json.Encode.string guesses)
                      ] ) ] ) ] ) ) )

      SetClue word ->
        ( { model | clueInProgress = word }, Cmd.none )
          
      SetGuesses guesses ->
        ( { model | guessesInProgress = guesses }, Cmd.none )
        
      NoOp ->
          ( model, Cmd.none)



toCard : CardTwo -> Card
toCard c =
  Card
   c.word
   (Maybe.withDefault 0 c.team)
   (c.team /= Nothing)

decodeCard : Json.Decode.Decoder CardTwo
decodeCard =
  Json.Decode.map3
    CardTwo
      (Json.Decode.field "word" Json.Decode.string)
      (Json.Decode.field "team" (Json.Decode.nullable Json.Decode.int))
      (Json.Decode.field "id" Json.Decode.int)
  
decodeCardList : Json.Decode.Decoder (List CardTwo)
decodeCardList =
  Json.Decode.list decodeCard
  
decodeBoardInfo : Json.Decode.Decoder BoardInfo
decodeBoardInfo =
  Json.Decode.map4
    BoardInfo
      (Json.Decode.field "password" Json.Decode.string)
      (Json.Decode.field "red_remaining" Json.Decode.int)
      (Json.Decode.field "blue_remaining" Json.Decode.int)
      (Json.Decode.field "board" decodeCardList)

decodeStatus : Json.Decode.Decoder Status
decodeStatus =
  Json.Decode.map5
    Status
      (Json.Decode.field "game_over" Json.Decode.bool)
      (Json.Decode.field "turn" Json.Decode.bool)
      (Json.Decode.field "text" Json.Decode.string)
      (Json.Decode.field "clue" (Json.Decode.nullable Json.Decode.string))
      (Json.Decode.field "remaining_guesses" Json.Decode.int)

decodeSpymasters : Json.Decode.Decoder (Maybe String, Maybe String)
decodeSpymasters = 
  Json.Decode.map2 Tuple.pair
    (Json.Decode.index 0 (Json.Decode.nullable Json.Decode.string))
    (Json.Decode.index 1 (Json.Decode.nullable Json.Decode.string))

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

toastConfig : Toast.Config Msg
toastConfig =
    Toast.defaultConfig |> Toast.delay 3300

addToast : Toast.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toast.addToast toastConfig AddToastMessage toast ( model, cmd )

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
    [ Time.every 30000 Ping
    , Time.every 1000 Tick
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
                  [ a  [ href ("./spymaster/index.html?passphrase=" ++ password), target "_blank", class "spymaster" ]
                    [ div [ class "spymaster-preview" ] []
                    , div [] [ strong [] [ text "Click here to access the Decryptor. " ]
                              , br [] []
                              , text ("Paste in the password (" ++ password ++ ") to find the correct board.") ]
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

isSpymaster : Maybe User -> Maybe User -> Bool
isSpymaster user spymaster =
  case user of
    Nothing -> False
    Just u ->
      case spymaster of
        Nothing -> False
        Just s -> s.nickname == u.nickname


isSameUser : Maybe User -> Maybe User -> Bool
isSameUser u1 u2 =
  case u1 of
     Nothing -> False
     Just a ->
      case u2 of
        Nothing -> False
        Just b -> a.username == b.username

formatName : Maybe User -> String -> Bool -> List ( Html Msg )
formatName user color isUser =
  case user of
     Nothing ->
      [ div [ class ("s " ++ color) ] []
      , span []
        [ text "Waiting..."
        , em [] [ text "0" ]
        ]
      ]
     Just u  ->
      [ div [ class ("s " ++ color) ] []
        , span (if isUser then [class "bold", attribute "flow" "up", attribute "tooltip" "This is you!"] else [])
          [ text u.nickname
          , em [] [ text (String.fromInt u.score) ]
          ]
        ]

modalSpectators : Maybe User -> Maybe User -> List ( User )-> String
modalSpectators red_user blue_user all_users =
  let
    spectators = List.filter (\x -> x.team /= 1 && x.team /= 2) all_users
  in
    String.join ", " (List.map .nickname spectators)

modalUser : Maybe User -> Maybe User -> String -> Int -> List User -> Html Msg
modalUser user spymaster color teamid users =
  let
    spymaster_html = 
      case spymaster of
        Nothing -> text "No spymaster" 
        Just u -> text u.nickname
    team_text =
      if List.isEmpty (List.map .nickname (List.filter (\x -> x.team == teamid) users)) then
        "No members"
      else
        (String.join ", " (List.map .nickname (List.filter (\x -> x.team == teamid) users)))
  in
    div [ class ("modal_" ++ color)] [
      div [ class "pad" ] [
        h3 [] [ text (color ++ " team") ]
      , h4 [] [ spymaster_html ]
      , div [] [ text team_text ] ] ]

showModal : Maybe User -> Maybe User -> Maybe User -> List (User) -> Html Msg
showModal user red_sm blue_sm users = 
  let
    spectator_button =
      case user of
        Nothing -> text ""
        Just u ->
          if u.team /= 1 && u.team /= 2 then
            text ""
          else
            button [ onClick (SetTeam 0) ] [ text "Become spectator" ]
    red_button =
      case user of
        Nothing -> text ""
        Just u ->
          if u.team == 1 then
            if user == red_sm then
              text ""
            else
              button [ onClick (SetSpymaster 1) ] [ text "Become red spymaster" ]
          else
            button [ onClick (SetTeam 1) ] [ text "Join red team" ]
    blue_button =
      case user of
        Nothing -> text ""
        Just u ->
          if u.team == 2 then
            if user == blue_sm then
              text ""
            else
              button [ onClick (SetSpymaster 1) ] [ text "Become blue spymaster" ]
          else
            button [ onClick (SetTeam 2) ] [ text "Join blue team" ]
  in
    div [ class "lightbox" ]
    [ div [ class "modal"]
      [ div [ class "flex_row" ] 
        [ div [ class "flex_fill" ] []
        , span [ class "close_button", onClick ToggleTeamModal ] [] ]
      , div [ class "flex_container" ]
        [ modalUser user red_sm "red" 1 users
        , modalUser user blue_sm "blue" 2 users
        , div [ class "modal_spectators" ]
          [ h3 [] [ text "Spectators" ]
          , text (modalSpectators red_sm blue_sm users)
          ]
        , div [] [
            spectator_button
          , red_button
          , blue_button ]
        ]
      ]
    ]


spymasterModal : Maybe User -> Maybe User -> Maybe User -> String -> Html Msg
spymasterModal user red_sm blue_sm clueInProgress = 
  let
    is_red_sm = isSpymaster user red_sm
    is_blue_sm = isSpymaster user blue_sm
    header_msg =
      if is_red_sm then [ text "You are the red spymaster.", a [ href ("./spymaster/index.html?passphrase=" ++ password), target "_blank" ] [ text "Decryptor" ] ]
      else if is_blue_sm then [ text "You are the blue spymaster.", a [ href ("./spymaster/index.html?passphrase=" ++ password), target "_blank" ] [ text "Decryptor" ] ]
      else [ text "You are not a spymaster!" ]
    clue_html =
      if is_red_sm || is_blue_sm then
        [ h3 [] [ text "Clue" ]
        , input [ type_ "text", onInput SetClue, onFocus (BlockKeyShortcuts True), onBlur (BlockKeyShortcuts False), placeholder "Enter a clue", value clueInProgress ] []
        , h3 [] [ text "Guesses" ]
        , select [ onInput SetGuesses ]
          [ option [ value "", disabled True, selected True] [ text "Select a value" ]
          , option [ value "0"] [ text "0" ]
          , option [ value "1"] [ text "1" ]
          , option [ value "2"] [ text "2" ]
          , option [ value "3"] [ text "3" ]
          , option [ value "4"] [ text "4" ]
          , option [ value "5"] [ text "5" ]
          , option [ value "6"] [ text "6" ]
          , option [ value "7"] [ text "7" ]
          , option [ value "8"] [ text "8" ]
          , option [ value "9"] [ text "9 " ]
          , option [ value "infinity"] [ text "∞" ]
          ]
        , button [ onClick SubmitClue ] [ text "Submit clue" ]
        ]
      else
        []
  in
    div [ class "lightbox" ]
    [ div [ class "modal"]
      [ div [ class "flex_row" ] 
        [ div [ class "flex_fill" ] [  h3 [] header_msg ]
        , span [ class "close_button", onClick ToggleSpymasterModal ] [] ]
      , div [ class "flex_container" ]
        clue_html
      ]
    ]
  

drawWordlistToggle : Wordlist -> Html Msg
drawWordlistToggle wordlist =
  li [] [ a [ class "", onClick (ToggleWordlist wordlist.key) ] [ span [ class ("icon " ++ if wordlist.include then "checked" else "unchecked")] [], text ("Use " ++ wordlist.name)] ]


view : Model -> Html Msg
view model =
  let
    wordlistToggles = List.map drawWordlistToggle model.wordlists
    addCards cards = drawCard 0 cards
    left_turn_text =
      if model.status.game_over then
        "Select New Game to play again"
      else
        (if model.status.turn then "Red" else "Blue") ++ " team's turn"
    left_turn_text_bottom = 
      if model.status.game_over then
        ""
      else
        "Click here or press space to pass"
    right_turn_text_bottom =
      if model.status.game_over then
        ""
      else
        (if model.status.remaining_guesses == 1 then "1 guess" else String.fromInt model.status.remaining_guesses ++ " guesses") ++ " remaining"
  in
    div [ class "container" ]
      [ div [ class ("lightbox" ++ (if model.toggleLightbox then " show" else " hidden")), onClick ToggleLightbox ] [ div [] (lightboxInfo model.password) ]
      , div [ class ("lightbox" ++ (if model.toggleQR then " show" else " hidden")), onClick ToggleQR ] [ div [ id "qrcode" ] [] ]
      , if model.toggleTeamModal then (showModal model.user model.red_spymaster model.blue_spymaster model.users) else text ""
      , if model.toggleSpymasterModal then (spymasterModal model.user model.red_spymaster model.blue_spymaster model.clueInProgress) else text ""
      , div [ class "debug" ] [ {- text model.debugString -} ]
      , div [ class "top" ]
        [ div [ class "top_message" ] [ text model.topMessage ]
        , div [ class "toast_container"] [ Toast.view toastConfig Toast.defaultView AddToastMessage model.toastMessages ]
        ]
      , div
        [ class ("sidebar" ++ (if model.toggleSidebar then "" else " hidden"))]
          [ ul []
            [ li [] [ a [ class "", onClick ToggleSpies ] [ span [ class ("icon " ++ if model.settings.spies then "checked" else "unchecked")] [], text "Show spies"] ]
            , li [] [ a [ class "", onClick ToggleOverride ] [ span [ class ("icon " ++ if model.settings.override then "checked" else "unchecked")] [], text "Bypass turn control"] ]
            , li [] [ a [ class "disabled", onClick ToggleSoundEffects ] [ span [ class ("icon " ++ if model.toggleSoundEffects then "checked" else "unchecked")] [], text "Enable sound effects"] ]
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
        [ main_ [ class ((if model.status.turn then "red-turn" else "blue-turn") ++ (if model.settings.spies then "" else " hide_spies")) ]
          (addCards model.cards)
        , div [ class "bottom" ]
            [ div [ class "bottom_left bottom_no_stretch" ]
              [ span [ class "settings_button", onClick ToggleSidebar ] [] ]
            , div [ class "bottom_left" ]
              [ span [ class "turn_text button", onClick PassTurn ]
                [ text left_turn_text
                , span [ class "bottom_span" ] [ text left_turn_text_bottom ]
                ]
              ]
            , div [ class "bottom_left cards_remaining" ]
                [ div [ class "red_remaining" ] [ text (String.fromInt model.redRemaining ++ " remaining") ]
                , div [ class "blue_remaining" ] [ text (String.fromInt model.blueRemaining ++ " remaining") ]
                ]
            , div [ class "bottom_left turn_info" ]
              [ span [ class "turn_text button" ]
                [ text model.status.text
                , span [ class "bottom_span" ] [ text right_turn_text_bottom ]
                ]
              ]
            , if (isSpymaster model.user model.red_spymaster || isSpymaster model.user model.blue_spymaster) then div [ class "bottom_right bottom_no_stretch" ] [ span [ class "spymaster_button", onClick ToggleSpymasterModal ] [] ] else text ""
            , div [ class "bottom_right bottom_no_stretch" ] [ span [ class "qr_button", onClick ToggleQR ] [] ]
            , div [ class "bottom_right bottom_no_stretch" ] [ span [ class "new_game_button", onClick NewGame ] [] ]
            , div [ class "bottom_right bottom_no_stretch" ] [ span [ class "info_button", onClick ToggleLightbox ] [] ]
            , div [ class "bottom_right bottom_no_stretch" ] [ span [ class "teams_button", onClick ToggleTeamModal ] [] ]
            ]
          ]
      ]
