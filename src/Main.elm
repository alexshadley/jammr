port module Main exposing (..)

import Browser
import Dict
import Html exposing (Html)
import Task
import Process

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input

import Track exposing (..)
import PianoRoll exposing (pianoRoll)


---- MODEL ----


type alias Model =
  { track : Track }


init : ( Model, Cmd Msg )
init =
    ( { track = Track.empty } , Cmd.none )



---- UPDATE ----


type Msg
    = PlayTrack
    | AddNote Note
    | RemoveNote Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PlayTrack ->
      ( model, playNotes (generateInstructions 120 model.track ) )

    AddNote note ->
      ( {model | track = Track.addNote note model.track}, Cmd.none )

    RemoveNote id ->
      ( {model | track = Track.removeNote id model.track}, Cmd.none )


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
  in
  layout [] <| column [ centerX, padding 50, spacing 50 ]
    [ Input.button [onClick PlayTrack, centerX] {onPress = Just PlayTrack, label = text "Play Track"}
    , pianoRoll AddNote model.track
    ]

overlay : Track -> List (Element.Attribute Msg)
overlay track =
  rollNotes track --++ rollDividers constBeatCount

rollNotes : Track -> List (Element.Attribute Msg)
rollNotes track =
  track.notes
    |> Dict.toList
    |> List.map rollNote
    |> List.map inFront

rollNote : (Int, Note) -> Element Msg
rollNote (id, note) =
  el [ paddingEach
      { top = (constTopNote - note.pitch) * constLaneHeight
      , left = round <| note.start * (constRollWidth / constBeatCount) + constLabelWidth
      , bottom = 0
      , right = 0} ]
    ( el 
      [ width (px <| round ((4.0 / 5.0) * (constRollWidth / constBeatCount )))
      , height (px <| round (constLaneHeight * (4.0 / 5.0)))
      , Background.color <| rgb255 150 240 150
      , onClick (RemoveNote id)
      ] none
    )


rollDividers : Int -> List (Element.Attribute Msg)
rollDividers n =
  List.range 0 (n - 1)
    |> List.map (\i -> divider (round ((toFloat i / toFloat n) * constRollWidth)))
    |> List.map inFront

divider : Int -> Element Msg
divider x =
  el [paddingEach {left = x, top = 0, right = 0, bottom = 0}]
    ( el [width (px 2), height (px <| constLaneHeight * constPitchCount), Background.color <| rgb255 100 100 100] none )

noteRow : Int -> Element Msg
noteRow note =
  row [height (px constLaneHeight), width fill]
    [ noteLabel note
    , noteLane note
    ]

noteLabel : Int -> Element Msg
noteLabel note =
  column [width (px constLabelWidth)] [el [alignRight] (text (pitchToString note))]

noteLane : Int -> Element Msg
noteLane note =
  let
    bgColor = case (modBy 2 note) == 0 of
      True -> rgb255 220 230 245
      False -> rgb255 230 230 230
    
    noteCells =
      List.range 0 (constBeatCount - 1)
        |> List.map (noteCell note)
  in
    row [height fill, width fill, Background.color bgColor] noteCells

noteCell : Int -> Int -> Element Msg
noteCell note beat =
  column 
    [ onClick (AddNote {pitch = note, start = toFloat beat, duration = 1.0})
    , width (px 60)
    , height fill
    ] []


---- PROGRAM ----


{-| Tells JS to play a tone with the specified frequency, start time and
duration. Note that times are in seconds, not beats as with `Note`
-}
port playNotes : List NoteInstruction -> Cmd msg


main : Program () Model Msg
main =
  Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = always Sub.none
    }
