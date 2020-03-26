module PianoRoll exposing (pianoRoll, calcStartAndDuration)


import Color
import Dict
import Element
import Html.Events.Extra.Mouse as Mouse
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


import Msg exposing (..)
import Model exposing (..)
import Track exposing (..)

rollHeight = 800
rollWidth = 1000
labelWidth = 50

beatCount = 16
topNote = 60
pitchCount = 24
subdivisions = 4

laneHeight = rollHeight / pitchCount
cellWidth = rollWidth / beatCount


{-| Returns (start, duration) from the start and end of a note drawing
-}
calcStartAndDuration : Float -> Float -> (Float, Float)
calcStartAndDuration start end =
  let
    startBin = truncate <| start / ( rollWidth / ( beatCount * subdivisions ) )
    startBeat = toFloat startBin / subdivisions
    endBin = truncate <| end / ( rollWidth / ( beatCount * subdivisions ) )
    endBeat = toFloat endBin / subdivisions
  in
    if endBin < startBin then
      (startBeat, 1)
    else
      if startBin == endBin then
        (startBeat, 1)
      else
        (startBeat, endBeat - startBeat)
    

-- for my sanity
px = String.fromInt

pianoRoll : Model -> Element.Element Msg
pianoRoll model =
  Element.html <|
    svg 
      [ width (px rollWidth)
      , height (px rollHeight)
      , viewBox ("-" ++ (px labelWidth) ++ " 0 " ++ (px rollWidth) ++ " " ++ (px rollHeight))
      ]
      [pitchLanes, dividers, rollNotes model.track, currentNote model.currentNote]

currentNote : Maybe (Int, Float, Float) -> Svg Msg
currentNote note =
  case note of
    Just (pitch, xStart, xCur) ->
      let
        (start, duration) = calcStartAndDuration xStart xCur
        xVal = start * cellWidth
        yVal = toFloat (topNote - pitch) * laneHeight
        widthVal = duration * cellWidth
      in
        rect 
          [ x (String.fromFloat xVal)
          , y (String.fromFloat yVal)
          , width (String.fromFloat widthVal)
          , height (String.fromFloat laneHeight)
          , fill "green"
          , Mouse.onMove (\event -> MoveDrawingOnNote (Tuple.first event.offsetPos) )
          , Mouse.onUp (\event -> EndDrawingOnNote (Tuple.first event.offsetPos) )
          ] []

    Nothing ->
      g [] []

rollNotes : Track -> Svg Msg
rollNotes track =
  let
    notes = Dict.toList track.notes
  in
    g [color "green"] (List.map rollNote notes)

rollNote : (Int, Note) -> Svg Msg
rollNote (id, note) =
  let
    xVal = note.start * cellWidth
    yVal = toFloat (topNote - note.pitch) * laneHeight
    widthVal = note.duration * cellWidth
  in
    rect 
      [ x (String.fromFloat xVal)
      , y (String.fromFloat yVal)
      , width (String.fromFloat widthVal)
      , height (String.fromFloat laneHeight)
      , fill "currentColor"
      , onClick (RemoveNote id)
      ] []

splitAlternating : List a -> (List a, List a)
splitAlternating xs =
  let
    tagged = List.indexedMap (\i x -> (i, x)) xs
    left = tagged |> List.filter (Tuple.first >> (\x -> modBy 2 x == 0)) |> List.map Tuple.second
    right = tagged |> List.filter (Tuple.first >> (\x -> modBy 2 x == 1)) |> List.map Tuple.second
  in
    (left, right)
    
pitchLanes : Svg Msg
pitchLanes = 
  let
    pitches = List.range (topNote - pitchCount) topNote
    lanes = List.map pitchRow pitches
    (white, gray) = splitAlternating lanes

  in
    g []
      [ g [ color "white" ] white
      , g [ color "lightgray" ] gray
      ]

pitchRow : Int -> Svg Msg
pitchRow pitch =
  g []
    [ pitchLane pitch
    , pitchLabel pitch
    ]

pitchLabel : Int -> Svg msg
pitchLabel pitch =
  let
    yVal = -10 + toFloat (topNote - pitch) * laneHeight
  in
    text_
      [ x (String.fromInt -50)
      , y (String.fromFloat yVal)
      , fill "black"
      ] [ text (pitchToString pitch) ]

pitchLane : Int -> Svg Msg
pitchLane pitch =
  let
    yVal = toFloat (topNote - pitch) * laneHeight
  in
    rect 
      [ x (String.fromInt 0)
      , y (String.fromFloat yVal)
      , width (String.fromInt rollWidth)
      , height (String.fromFloat laneHeight)
      , fill "currentColor"
      , Mouse.onDown (\event -> StartDrawing pitch (Tuple.first event.offsetPos) )
      , Mouse.onMove (\event -> MoveDrawing (Tuple.first event.offsetPos) )
      , Mouse.onUp (\event -> EndDrawing (Tuple.first event.offsetPos) )
      ] []

{-addNote : Mouse.Event -> Int -> Note
addNote event pitch =
  let
    (offX, _) = event.offsetPos
    bin = truncate <| offX / ( rollWidth / ( beatCount * subdivisions ) )
  in
    { pitch = pitch
    , start = toFloat bin / subdivisions
    , duration = 1
    }-}

dividers : Svg Msg
dividers = 
  let
    positions = List.range 1 beatCount |> List.map (\x -> toFloat x * (rollWidth / beatCount))
  in
    g [color "gray"] (List.map divider positions)

divider : Float -> Svg Msg
divider xVal =
  rect 
    [x (String.fromFloat xVal)
    , y (String.fromFloat 0)
    , width (String.fromFloat 2)
    , height (String.fromInt rollHeight)
    , fill "currentColor"
    ] []
