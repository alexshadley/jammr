module Msg exposing (Msg(..))
{-| Separated because PianoRoll also needs access to Msg. There is almost
certainly a better way to do this

-}

import Track exposing (..)

type Msg
    = PlayTrack
    | AddNote Note
    | RemoveNote Int
    | StartDrawing Int Float
    | EndDrawing Float