module PianoRoll.Helper exposing (..)

import PianoRoll.Model exposing (..)

import Model exposing (..)
import Track exposing (..)

calcPitch : Params -> Float -> Int
calcPitch params y =
  params.topPitch - ( truncate <| y / params.laneHeight )


calcOffsetPitches : Params -> Float -> Int
calcOffsetPitches params dy =
  truncate <| dy / params.laneHeight


calcBeats : Params -> Float -> Float
calcBeats params x =
  let
    startBin = truncate <| x / ( rollWidth / ( beatCount * subdivisions ) )
  in
    toFloat startBin / subdivisions


calcNotePos : Params -> Note -> ((Float, Float), (Float, Float))
calcNotePos params note =
  let 
    sx = note.start * cellWidth
    sy = toFloat (params.topPitch - note.pitch) * params.laneHeight
    ex = sx + note.duration * cellWidth
    ey = sy + params.laneHeight
  in
    ((sx, sy), (ex, ey))

calcOffsetBeats : Params -> Float -> Float
calcOffsetBeats params dx =
  let
    bins = truncate <| dx / (params.cellWidth / subdivisions)
  in
    toFloat bins / subdivisions


{-| Calculates the four points of a note, checks to see if at least one is in
the provided selection box
-}
noteInSelection : Params -> BoxSelection -> Note -> Bool
noteInSelection params selection note =
  let
    ((sx, sy), (ex, ey)) = calcNotePos params note
    notePoints = [(sx, sy), (sx, ey), (ex, sy), (ex, ey)]

    (selx1, sely1) = selection.start
    (selx2, sely2) = selection.end

    (selsx, selex) = (Basics.min selx1 selx2, Basics.max selx1 selx2)
    (selsy, seley) = (Basics.min sely1 sely2, Basics.max sely1 sely2)

  in
    if selection.voice == note.voice then
      notePoints
        |> List.map (\(x, y) -> selsx <= x && x <= selex && selsy <= y && y <= seley)
        |> List.member True

    else
      False