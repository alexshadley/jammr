port module Main exposing (..)

import Browser
import Dict
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, field, float, int, string)
import Task
import Time
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
      , uiMode = Painting
      , currentNote = Nothing
      , lastNoteBeats = 1.0
      , currentSelection = Nothing

      , currentUser = Nothing
      , users = Dict.empty
      , usernameInput = ""
      , playbackBeat = Nothing
      , bpm = 120.0
    } , Cmd.none )


-- constants
subdivisions = 4


---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UsernameUpdate new ->
      ( {model | usernameInput = new}, Cmd.none)
    
    SubmitUser ->
      ( model, addUser {name = model.usernameInput} )

    PlayTrack ->
      ( { model | playbackBeat = Just 0.0 }, playNotes (generateInstructions model.bpm model.track ) )

    StopPlayback ->
      ( { model | playbackBeat = Nothing }, stopPlayback () )
    
    SetMode mode ->
      ( { model | uiMode = mode}, Cmd.none)
    
    PlayLabelKey voice pitch ->
      ( model, playNotes [ generatePitchInst voice pitch ] )

    RemoveNote id ->
      ( { model | track = Track.removeNote id model.track}, removeNote {id = id} )
    
    StartDrawing voice pitch x ->
      ( { model | 
          currentNote = Just 
            { voice = voice
            , pitch = pitch
            , startX = x
            , endX = x
            , leftStartArea = False
            }
        }
      , playNotes [ generatePitchInst voice pitch ]
      )
    
    -- TODO: make this less awful
    MoveDrawing xCur ->
      let
        newNote =
          Maybe.map (
            \cn -> 
              let
                leftStart = 
                  let
                    (_, duration) = PianoRoll.calcStartAndDuration cn model.lastNoteBeats
                  in
                    duration > model.lastNoteBeats
              in
                {cn | endX = xCur, leftStartArea = cn.leftStartArea || leftStart}
            ) model.currentNote
      in
        ( { model | currentNote = newNote }
        , Cmd.none)
    
    EndDrawing xFinal ->
      case model.currentNote of
        Just ({voice, pitch} as note) ->
          let
            (newStart, newDuration) = PianoRoll.calcStartAndDuration note model.lastNoteBeats
            newNote = { pitch = pitch, start = newStart, duration = newDuration, user = Maybe.map .name model.currentUser, voice = voice}
            (newTrack, newId) = Track.addNote newNote model.track
          in
            ( { model | currentNote = Nothing, track = newTrack}
            , addNote {id = newId, pitch = newNote.pitch, start = newNote.start, duration = newNote.duration, user = newNote.user, voice = newNote.voice} )
        
        Nothing ->
          ( model, Cmd.none)

    MoveDrawingOnNote xCur ->
      let
        newNote =
          Maybe.map (
            \cn -> 
              let
                leftStart = 
                  let
                    (_, duration) = PianoRoll.calcStartAndDuration cn model.lastNoteBeats
                  in
                    duration > model.lastNoteBeats
              in
                {cn | endX = cn.startX + xCur, leftStartArea = cn.leftStartArea || leftStart}
            ) model.currentNote
      in
        ( { model | currentNote = newNote }
        , Cmd.none)

    EndDrawingOnNote xFinal ->
      case model.currentNote of
        Just ({voice, pitch, startX} as note) ->
          let
            noteUpdated = { note | endX = startX + xFinal }
            (newStart, newDuration) = PianoRoll.calcStartAndDuration noteUpdated model.lastNoteBeats
            newNote = { pitch = pitch, start = newStart, duration = newDuration, user = Maybe.map .name model.currentUser, voice = voice}
            (newTrack, newId) = Track.addNote newNote model.track
          in
            ( { model 
                | currentNote = Nothing
                , lastNoteBeats = newNote.duration
                , track = newTrack
                }
            , addNote {id = newId, pitch = newNote.pitch, start = newNote.start, duration = newNote.duration, user = newNote.user, voice = newNote.voice} )
        
        Nothing ->
          (model, Cmd.none)
    
    StartSelection voice (x, y) ->
      (model, Cmd.none)

    MoveSelection (x, y) ->
      (model, Cmd.none)

    EndSelection (x, y) ->
      (model, Cmd.none)
    
    UpdateBeat delta ->
      ( { model | playbackBeat = Maybe.map (\b -> b + delta) model.playbackBeat}, Cmd.none)
    
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
      let
        userDict =
          users
            |> List.map (\u -> (u.name, u))
            |> Dict.fromList

      in
        ({ model | users = userDict }, Cmd.none)
    
    UserRegisteredFromServer user ->
      ({ model | currentUser = user}, Cmd.none)


---- VIEW ----



view : Model -> Html Msg
view model =
  let
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
            [ playButton model
            , modeButton model
            , PianoRoll.pianoRoll model {rollHeight=700, voice=0, topPitch=48, pitches=25}
            , PianoRoll.pianoRoll model {rollHeight=700, voice=1, topPitch=60, pitches=25}
            , PianoRoll.pianoRoll model {rollHeight=350, voice=2, topPitch=54, pitches=12}
            ]
          , el [ alignRight, alignTop, moveLeft 50, moveDown 100 ] (usersWidget model)
          ]
        )

playButton : Model -> Element Msg
playButton model =
  let
    buttonText =
      case model.playbackBeat of
        Just _  -> "Stop"
        Nothing -> "Play Track"
    action =
      case model.playbackBeat of
        Just _  -> StopPlayback
        Nothing -> PlayTrack
  in
    Input.button [onClick action, centerX] {onPress = Just PlayTrack, label = text buttonText}

modeButton : Model -> Element Msg
modeButton model =
  row
    []
    [ Input.button [] {onPress = Just (SetMode Painting), label = text "Paint"}
    , Input.button [] {onPress = Just (SetMode Selecting), label = text "Select"}
    ]

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
      Just user -> model.users |> Dict.toList |> List.map Tuple.second |> List.filter (\o -> o.name /= user.name)
      Nothing   -> model.users |> Dict.toList |> List.map Tuple.second
    
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

port addNote : {id: Int, pitch: Pitch, start: Float, duration: Float, user: Maybe String, voice: Voice} -> Cmd msg
port removeNote : {id: Int} -> Cmd msg
port addUser : {name: String} -> Cmd msg
port removeUser : {name: String} -> Cmd msg

{-| Tells JS to play a tone with the specified frequency, start time and
duration. Note that times are in seconds, not beats as with `Note`
-}
port playNotes : List NoteInstruction -> Cmd msg
port stopPlayback : () -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    tickMS = (60.0 / model.bpm) * (1000 / subdivisions)

    tick =
      case model.playbackBeat of
        Just _ -> [ Time.every tickMS (\_ -> UpdateBeat (1.0 / subdivisions)) ]
        Nothing -> []
  in
    Sub.batch
      ( [ setNotes (decodeNotes >> SetNotesFromServer)
        , addNoteFromServer (decodeNote >> AddNoteFromServer)
        , removeNoteFromServer (decodeDelId >> RemoveNoteFromServer)
        , setUsersFromServer (decodeUsers >> SetUsersFromServer)
        , userRegisteredFromServer (decodeUser >> UserRegisteredFromServer)
        ] ++ tick
      )

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
  Decode.map6 (\id p s d u v -> (id, {pitch=p, start=s, duration=d, user=Just u, voice=v}))
    (field "id" int)
    (field "pitch" int)
    (field "start" float)
    (field "duration" float)
    (field "user" string)
    (field "voice" int)

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
