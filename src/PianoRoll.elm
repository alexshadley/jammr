module PianoRoll exposing (pianoRoll)



import Color
import Dict
import Element
import Html.Events.Extra.Mouse as Mouse
import Svg exposing (..)
import Svg.Attributes exposing (..)


import Track exposing (..)

rollHeight = 800
rollWidth = 1000
labelWidth = 50

beatCount = 16
topNote = 60
pitchCount = 24

laneHeight = rollHeight / pitchCount
cellWidth = rollWidth / beatCount

-- for my sanity
px = String.fromInt

pianoRoll : (Note -> msg) -> Track -> Element.Element msg
pianoRoll addNoteCmd track =
  Element.html <|
    svg 
      [ width (px rollWidth)
      , height (px rollHeight)
      , viewBox ("0 0 " ++ (px rollWidth) ++ " " ++ (px rollHeight))
      , Mouse.onDown (\event -> addNoteCmd ( addNote event ))
      ]
      ( pitchLanes )

addNote : Mouse.Event -> Note
addNote event =
  let
    (offX, offY) = event.offsetPos
  in
    {pitch = round (offX / 30), start = 0, duration = 1}

rollNotes : Track -> Svg msg
rollNotes track =
  let
    notes = Dict.toList track.notes
  in
    g [color "green"] [List.map rollNote notes]

rollNote : (Int, Note) -> Svg msg
rollNote (id, note) =
  let
    x = note.start * cellWidth
    y = (topNote - note.pitch) * laneHeight
    width = note.duration * cellWidth
  in
    rect (x, y) width laneHeight

splitAlternating : List a -> (List a, List a)
splitAlternating xs =
  let
    tagged = List.indexedMap (\i x -> (i, x)) xs
    left = tagged |> List.filter (Tuple.first >> (\x -> modBy 2 x == 0)) |> List.map Tuple.second
    right = tagged |> List.filter (Tuple.first >> (\x -> modBy 2 x == 1)) |> List.map Tuple.second
  in
    (left, right)
    
pitchLanes : List (Svg msg)
pitchLanes = 
  let
    positions = List.range 0 pitchCount |> List.map (\x -> toFloat x * laneHeight)
    lanes = List.map pitchLane positions
    (white, gray) = splitAlternating lanes
  in
    [ g [ color "white" ] white
    , g [ color "lightgray" ] gray
    ]

pitchLane : Float -> Svg msg
pitchLane yVal =
  rect 
    [ x (String.fromInt 0)
    , y (String.fromFloat yVal)
    , width (String.fromInt rollWidth)
    , height (String.fromFloat laneHeight)
    , fill "currentColor"
    ] []

{-dividers : Renderable
dividers = 
  let
    positions = List.range 1 beatCount |> List.map (\x -> x * 100)
  in
    shapes [fill Color.gray] (List.map divider positions)

divider : Int -> Shape
divider x =
  rect (toFloat x, 0) 2 rollHeight-}
