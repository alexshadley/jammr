port module Main exposing (..)

import Browser
import Dict
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, field, float, int, string)
import Task
import Process

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input

import Msg exposing (..)
import Model exposing (..)
import Track exposing (..)
import User exposing (User)
import PianoRoll


---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { track = Track.empty
      , currentNote = Nothing
      , currentUser = Nothing
      , users = []
      , usernameInput = ""
    } , Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UsernameUpdate new ->
      ( {model | usernameInput = new}, Cmd.none)
    
    SubmitUser ->
      ( model, addUser {name = model.usernameInput} )

    PlayTrack ->
      ( model, playNotes (generateInstructions 120 model.track ) )

    RemoveNote id ->
      ( { model | track = Track.removeNote id model.track}, removeNote {id = id} )
    
    StartDrawing pitch x ->
      ( { model | currentNote = Just (pitch, x, x) }, Cmd.none)
    
    MoveDrawing xCur ->
      ( { model | currentNote = Maybe.map (\(p, x, _) -> (p, x, xCur)) model.currentNote }, Cmd.none)
    
    EndDrawing xFinal ->
      case model.currentNote of
        Just (pitch, x, _) ->
          let
            (newStart, newDuration) = PianoRoll.calcStartAndDuration x xFinal
            newNote = { pitch = pitch, start = newStart, duration = newDuration, user = Maybe.map .name model.currentUser}
            (newTrack, newId) = Track.addNote newNote model.track
          in
            ( { model | currentNote = Nothing, track = newTrack}, addNote {id = newId, pitch = newNote.pitch, start = newNote.start, duration = newNote.duration})
        
        Nothing ->
          ( model, Cmd.none)

    MoveDrawingOnNote xCur ->
      ( { model | currentNote = Maybe.map (\(p, x, _) -> (p, x, x + xCur)) model.currentNote }, Cmd.none)

    EndDrawingOnNote xFinal ->
      case model.currentNote of
        Just (pitch, x, _) ->
          let
            (newStart, newDuration) = PianoRoll.calcStartAndDuration x (x + xFinal)
            newNote = { pitch = pitch, start = newStart, duration = newDuration, user = Maybe.map .name model.currentUser}
            (newTrack, newId) = Track.addNote newNote model.track
          in
            ( { model | currentNote = Nothing, track = newTrack}, addNote {id = newId, pitch = newNote.pitch, start = newNote.start, duration = newNote.duration})
        
        Nothing ->
          ( model, Cmd.none)
    
    SetNotesFromServer notes ->
      let
        newTrack = List.foldr (\(id, note) track -> Track.addNoteWithId id note track) model.track notes
      in
        ({model | track = newTrack}, Cmd.none)
    
    AddNoteFromServer result ->
      case result of
        Just (id, note) ->
          ({model | track = Track.addNoteWithId id note model.track}, Cmd.none)
        
        Nothing ->
          (model, Cmd.none)
      
    RemoveNoteFromServer result ->
      case result of
        Just id ->
          ({model | track = Track.removeNote id model.track}, Cmd.none)
        
        Nothing ->
          (model, Cmd.none)

    SetUsersFromServer users ->
      ({ model | users = users }, Cmd.none)
    
    UserRegisteredFromServer user ->
      ({ model | currentUser = user}, Cmd.none)


---- VIEW ----

-- constants
constLaneHeight = 25
constRollWidth = 1000
constLabelWidth = 50

constBeatCount = 16
constTopNote = 60
constPitchCount = 24


view : Model -> Html Msg
view model =
  let
    pitches =
      List.range (constTopNote - constPitchCount + 1) constTopNote |> List.reverse
    overlay =
      case model.currentUser of
        Just user -> []
        Nothing -> [ inFront (loginOverlay model) ]

  in
    layout [] <| 
      el
        ( [ width fill
          , height fill
          ] ++ overlay
        )
        ( row [ width fill, height fill ]
          [ column
            [ centerX
            , padding 50
            , spacing 50 
            ]
            [ Input.button [onClick PlayTrack, centerX] {onPress = Just PlayTrack, label = text "Play Track"}
            , PianoRoll.pianoRoll model
            ]
          , el [ alignRight, alignTop, moveLeft 50, moveDown 100 ] (usersWidget model)
          ]
        )

loginOverlay : Model -> Element Msg
loginOverlay model =
  el 
    [ height fill
    , width fill
    , Background.color <| rgba255 0 0 0 0.5
    ]
    ( column 
      [ centerX
      , moveDown 100
      , padding 50
      , spacing 20
      , Background.color <| rgb255 245 245 245
      , Border.rounded 7
      ] 
      [ Input.text []
        { onChange = UsernameUpdate
        , text = model.usernameInput
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Name")
        }
      , Input.button
          [ centerX
          , padding 8
          , Border.width 1
          , Border.rounded 7
          , Border.color <| rgb255 150 150 150
          ]
          { onPress = Just SubmitUser
          , label = (text "Join")
          }
      ]
    )
  
usersWidget : Model -> Element Msg
usersWidget model =
  let
    youRow = case model.currentUser of
      Just user -> [userRow user]
      Nothing   -> []

    others = case model.currentUser of
      Just user -> List.filter (\o -> o.name /= user.name) model.users
      Nothing   -> model.users
    
    othersRows = List.map userRow others
  in
    column 
      [ width (px 160)
      , Border.width 1
      , Border.rounded 7
      , spacing 10
      , paddingXY 0 10
      ]
      ( youRow ++
        [ row 
            [ centerX
            , width (px 100)
            , height (px 1)
            , Background.color <| rgb255 0 0 0
            ]
            []
        ] ++
        othersRows
      )

userRow : User -> Element Msg
userRow user =
  row 
    [ centerX
    , centerY
    , spacing 15
    ]
    [ text user.name
    , el
      [ Background.color <| User.getColor user
      , width (px 10)
      , height (px 10)
      ] none
    ]
 

---- PROGRAM ----

port setNotes : (Decode.Value -> msg) -> Sub msg
port addNoteFromServer : (Decode.Value -> msg) -> Sub msg
port removeNoteFromServer : (Decode.Value -> msg) -> Sub msg
port setUsersFromServer : (Decode.Value -> msg) -> Sub msg
port userRegisteredFromServer : (Decode.Value -> msg) -> Sub msg

port addNote : {id: Int, pitch: Int, start: Float, duration: Float} -> Cmd msg
port removeNote : {id: Int} -> Cmd msg
port addUser : {name: String} -> Cmd msg
port removeUser : {name: String} -> Cmd msg

{-| Tells JS to play a tone with the specified frequency, start time and
duration. Note that times are in seconds, not beats as with `Note`
-}
port playNotes : List NoteInstruction -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ setNotes (decodeNotes >> SetNotesFromServer)
    , addNoteFromServer (decodeNote >> AddNoteFromServer)
    , removeNoteFromServer (decodeDelId >> RemoveNoteFromServer)
    , setUsersFromServer (decodeUsers >> SetUsersFromServer)
    , userRegisteredFromServer (decodeUser >> UserRegisteredFromServer)
    ]

decodeDelId : Decode.Value -> Maybe Int
decodeDelId v =
  case Decode.decodeValue (field "id" int) v of
    Ok id -> Just id
    Err e -> Debug.log (Decode.errorToString e) Nothing

decodeNotes : Decode.Value -> List (Int, Note)
decodeNotes v =
  case Decode.decodeValue (Decode.list noteDecoder) v of
    Ok notes -> notes
    Err e -> Debug.log (Decode.errorToString e) []

decodeNote : Decode.Value -> Maybe (Int, Note)
decodeNote v =
  case Decode.decodeValue noteDecoder v of
    Ok note -> Just note
    Err e -> Debug.log (Decode.errorToString e) Nothing

noteDecoder : Decoder (Int, Note)
noteDecoder =
  Decode.map5 (\id p s d u -> (id, {pitch=p, start=s, duration=d, user=Just u}))
    (field "id" int)
    (field "pitch" int)
    (field "start" float)
    (field "duration" float)
    (field "user" string)

decodeUsers : Decode.Value -> List User
decodeUsers v =
  case Decode.decodeValue (Decode.list userDecoder) v of
    Ok users -> users
    Err e -> Debug.log (Decode.errorToString e) []

decodeUser : Decode.Value -> Maybe User
decodeUser v =
  case Decode.decodeValue userDecoder v of
    Ok user -> Just user
    Err e -> Debug.log (Decode.errorToString e) Nothing

userDecoder : Decoder User
userDecoder =
  Decode.map2 (\n c -> {name=n, color=c})
    (field "name" Decode.string)
    (field "color" colorDecoder)

colorDecoder : Decoder (Int, Int, Int)
colorDecoder =
  Decode.list int
    |> Decode.andThen 
      (\c -> case c of
        (r::g::b::[]) -> Decode.succeed (r, g, b)
        _             -> Decode.fail "Color was not a triplet of integers"
      )


main : Program () Model Msg
main =
  Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = subscriptions
    }
