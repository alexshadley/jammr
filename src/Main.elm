port module Main exposing (..)

import Browser
import Browser.Events
import Dict
import Set
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, field, float, int, string, list)
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
    ( { pianoRolls =
        [ {rollHeight=700, voice=0, topPitch=48, pitches=25}
        , {rollHeight=700, voice=1, topPitch=60, pitches=25}
        , {rollHeight=350, voice=2, topPitch=54, pitches=12}
        ]
      , track = Track.empty
      , uiMode = Painting
      , currentNote = Nothing
      , lastNoteBeats = 1.0
      , currentSelection = Nothing
      , selectedNotes = Set.empty

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
      ( { model | uiMode = mode, selectedNotes = Set.empty}, Cmd.none)
    
    PlayLabelKey voice pitch ->
      ( model, playNotes [ generatePitchInst voice pitch ] )

    RemoveNote id ->
      ( { model | track = Track.removeNote id model.track}, removeNotes {notes = [id]} )
    
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
      ( { model | currentSelection = Just {voice = voice, start = (x, y), end = (x, y)}}, Cmd.none )

    MoveSelection (x, y) ->
      ( { model | currentSelection = Maybe.map (\s -> {s | end = (x, y)}) model.currentSelection }, Cmd.none )

    EndSelection (x, y) ->
      let
        selectionParameters =
          model.currentSelection |> Maybe.andThen (\sel ->
            List.filter (\p -> p.voice == sel.voice) model.pianoRolls |> List.head |> Maybe.andThen (\p ->
            Just (sel, p)
            ))

      in
        case selectionParameters of
          Just (selection, params) ->
            let
              finalParams = PianoRoll.calcParams params
              (sx, sy) = selection.start
              finalSelection = ((sx, sy), (x, y))

              selectedNotes = 
                Dict.toList model.track.notes
                  |> List.filter (\(_, n) -> PianoRoll.noteInSelection finalParams finalSelection n)
                  |> List.map Tuple.first
                  |> Set.fromList
            in
              ( { model | selectedNotes = selectedNotes, currentSelection = Nothing }, Cmd.none)
        
          Nothing ->
            ( model, Cmd.none )

    StartNoteMove (x, y) ->
      ( { model | uiMode = Selecting (Moving {start = (x, y), end = (x, y)}) }, Cmd.none )

    MoveNoteMove (x, y) ->
      case model.uiMode of
        Selecting (Moving noteMove) ->
          ( { model | uiMode = Selecting (Moving {noteMove | end = (x, y)}) }, Cmd.none )

        _ ->
          ( model, Cmd.none )

    EndNoteMove (x, y) ->
      let
        selectedNotes = Track.getNotes (Set.toList model.selectedNotes) model.track
        
        selectionVoice = 
          case selectedNotes of
            n::_ -> Just n.voice
            _    -> Nothing

        selectionParameters =
          selectionVoice
            |> Maybe.andThen (\voice -> List.filter (\p -> p.voice == voice) model.pianoRolls |> List.head)
            |> Maybe.andThen (\p -> Just p)

      in
        case (selectionParameters, model.uiMode) of
          (Just params, Selecting (Moving noteMove)) ->
            let
              finalParams = PianoRoll.calcParams params
              ((sx, sy), (ex, ey)) = (noteMove.start, noteMove.end)
              offsetBeats = PianoRoll.calcOffsetBeats finalParams (ex - sx)
              offsetPitches = PianoRoll.calcOffsetPitches finalParams (ey - sy)

              noteUpdateFn note = { note | start = note.start + offsetBeats, pitch = note.pitch - offsetPitches}
              newTrack = 
                Set.toList model.selectedNotes
                  |> List.foldr (\id track -> Track.update noteUpdateFn id track) model.track

              updatedNotes =
                List.map2
                  Tuple.pair
                  ( Set.toList model.selectedNotes )
                  ( Track.getNotes (Set.toList model.selectedNotes) newTrack )
              
              payloadNotes =
                List.map
                  (\(id, newNote) -> { id = id, pitch = newNote.pitch, start = newNote.start, duration = newNote.duration, user = newNote.user, voice = newNote.voice } )
                  updatedNotes
            in
              ( { model | uiMode = Selecting SelectingBox, track = newTrack, selectedNotes = Set.empty }
              , updateNotes {notes = payloadNotes}
              )
        
          _ ->
            ( model, Cmd.none )
    
    KeyPressed key ->
      case key of
        "Delete" ->
          let
            newTrack =
              Set.foldr (\id track -> Track.removeNote id track) model.track model.selectedNotes
          in
            ( { model | track = newTrack }, removeNotes { notes = Set.toList model.selectedNotes } )

        _ -> 
          ( model, Cmd.none )
    
    UpdateBeat delta ->
      ( { model | playbackBeat = Maybe.map (\b -> b + delta) model.playbackBeat}, Cmd.none )
    
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

    UpdateNotesFromServer notes ->
      let
        newTrack = List.foldr (\(id, note) track -> Track.addNoteWithId id note track) model.track notes
      in
        ({model | track = newTrack}, Cmd.none)
      
    RemoveNotesFromServer notes ->
      let
        newTrack = List.foldr (\id track -> Track.removeNote id track) model.track notes
      in
        ({model | track = newTrack}, Cmd.none)

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
            ( [ playButton model
              , modeButton model
              ] ++ List.map (PianoRoll.pianoRoll model) model.pianoRolls
            )
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
    Input.button buttonAttr {onPress = Just action, label = text buttonText}

modeButton : Model -> Element Msg
modeButton model =
  let
    (paintStyle, selectStyle) =
      case model.uiMode of
        Painting ->    (primaryButtonAttr, buttonAttr)
        Selecting _ -> (buttonAttr, primaryButtonAttr)
  in
    row
      [ centerX, spacing 20 ]
      [ text "Mode:"
      , Input.button paintStyle {onPress = Just (SetMode Painting), label = text "Paint"}
      , Input.button selectStyle {onPress = Just (SetMode (Selecting SelectingBox)), label = text "Select"}
      ]

buttonAttr : List (Attribute msg)
buttonAttr =
  [ centerX
  , padding 6
  , Border.rounded 5
  , Border.width 1
  ]

primaryButtonAttr : List (Attribute msg)
primaryButtonAttr =
  [ centerX
  , padding 6
  , Border.rounded 5
  , Border.width 1
  , Border.color <| rgb255 0 123 255
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

port setNotesFromServer : (Decode.Value -> msg) -> Sub msg
port addNoteFromServer : (Decode.Value -> msg) -> Sub msg
port updateNotesFromServer : (Decode.Value -> msg) -> Sub msg
port removeNotesFromServer : (Decode.Value -> msg) -> Sub msg
port setUsersFromServer : (Decode.Value -> msg) -> Sub msg
port userRegisteredFromServer : (Decode.Value -> msg) -> Sub msg

port addNote : {id: Int, pitch: Pitch, start: Float, duration: Float, user: Maybe String, voice: Voice} -> Cmd msg
port updateNotes : {notes: List {id: Int, pitch: Pitch, start: Float, duration: Float, user: Maybe String, voice: Voice}} -> Cmd msg
port removeNotes : {notes: List Int} -> Cmd msg
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
      ( [ setNotesFromServer (decodeNotes >> SetNotesFromServer)
        , addNoteFromServer (decodeNote >> AddNoteFromServer)
        , updateNotesFromServer (decodeNotes >> UpdateNotesFromServer)
        , removeNotesFromServer (decodeDelIds >> RemoveNotesFromServer)
        , setUsersFromServer (decodeUsers >> SetUsersFromServer)
        , userRegisteredFromServer (decodeUser >> UserRegisteredFromServer)
        , Browser.Events.onKeyDown keyDecoder
        ] ++ tick
      )

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map (\s -> KeyPressed s)
    (field "key" string)

decodeDelIds : Decode.Value -> List Int
decodeDelIds v =
  case Decode.decodeValue (field "notes" (list int)) v of
    Ok ids -> ids
    Err e -> Debug.log (Decode.errorToString e) []

decodeNotes : Decode.Value -> List (Int, Note)
decodeNotes v =
  case Decode.decodeValue (field "notes" (Decode.list noteDecoder)) v of
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
