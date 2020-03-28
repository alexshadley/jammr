module Msg exposing (Msg(..))
{-| Separated because PianoRoll also needs access to Msg. There is almost
certainly a better way to do this

-}

import Track exposing (..)

type Msg
    = UsernameUpdate String
    | PlayTrack
    | RemoveNote Int
    | StartDrawing Int Float
    | MoveDrawing Float
    | EndDrawing Float
    {- these hacks are necessary because mouse offset is always calculated
    from the parent, which messes up our math if it's the note being drawn
     -}
    | MoveDrawingOnNote Float
    | EndDrawingOnNote Float
    -- network
    | SetNotesFromServer (List (Int, Note))
    | AddNoteFromServer (Maybe (Int, Note))
    | RemoveNoteFromServer (Maybe Int)