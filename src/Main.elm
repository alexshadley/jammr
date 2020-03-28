port module Main exposing (..)

import Browser
import Dict
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder, field, field, float, int)
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
      , usernameInput = ""
    } , Cmd.none )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UsernameUpdate new ->
      ( {model | usernameInput = new}, Cmd.none)

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
            newNote = { pitch = pitch, start = newStart, duration = newDuration }
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
            newNote = { pitch = pitch, start = newStart, duration = newDuration }
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
        ( column
          [ centerX
          , padding 50
          , spacing 50 
          ]
          [ Input.button [onClick PlayTrack, centerX] {onPress = Just PlayTrack, label = text "Play Track"}
          , PianoRoll.pianoRoll model
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
      , Background.color <| rgb255 245 245 245
      , Border.rounded 7
      ] 
      [ Input.text []
        { onChange = UsernameUpdate
        , text = model.usernameInput
        , placeholder = Nothing
        , label = Input.labelAbove [] (text "Name")
        }
      ]
    )


---- PROGRAM ----

port setNotes : (Decode.Value -> msg) -> Sub msg
port addNoteFromServer : (Decode.Value -> msg) -> Sub msg
port removeNoteFromServer : (Decode.Value -> msg) -> Sub msg

port addNote : {id: Int, pitch: Int, start: Float, duration: Float} -> Cmd msg
port removeNote : {id: Int} -> Cmd msg

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
  Decode.map4 (\id p s d -> (id, {pitch=p, start=s, duration=d}))
    (field "id" int)
    (field "pitch" int)
    (field "start" float)
    (field "duration" float)


main : Program () Model Msg
main =
  Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = subscriptions
    }
