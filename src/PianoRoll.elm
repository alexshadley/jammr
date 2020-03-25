module PianoRoll exposing (pianoRoll)

import Html.Events.Extra.Mouse as Mouse

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Element


import Track exposing (..)

rollHeight = 800
rollWidth = 1000
labelWidth = 50

beatCount = 16
topNote = 60
pitchCount = 24

laneHeight = rollHeight / pitchCount
cellWidth = rollWidth / beatCount


pianoRoll : (Note -> msg) -> Track -> Element.Element msg
pianoRoll addNoteCmd track =
  Element.html <|
    Canvas.toHtml (rollWidth, rollHeight) 
      [ Mouse.onDown (\event -> addNoteCmd ( addNote event )) ]
      ( pitchLanes ++ [ dividers ] )

addNote : Mouse.Event -> Note
addNote event =
  let
    (offX, offY) = event.offsetPos
  in
    {pitch = round (offX / 30), start = 0, duration = 1}

rollNotes : Track -> Renderable

rollNote : Note -> Shape
rollNote note =
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
    
pitchLanes : List Renderable
pitchLanes = 
  let
    positions = List.range 0 pitchCount |> List.map (\x -> toFloat x * laneHeight)
    lanes = List.map pitchLane positions
    (white, gray) = splitAlternating lanes
  in
    [ shapes [fill Color.lightGray] gray
    , shapes [fill Color.white] white
    ]

pitchLane : Float -> Shape
pitchLane y =
  rect (0, y) rollWidth laneHeight

dividers : Renderable
dividers = 
  let
    positions = List.range 1 beatCount |> List.map (\x -> x * 100)
  in
    shapes [fill Color.gray] (List.map divider positions)

divider : Int -> Shape
divider x =
  rect (toFloat x, 0) 2 rollHeight
