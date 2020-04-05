module Msg exposing (Msg(..))
{-| Separated because PianoRoll also needs access to Msg. There is almost
certainly a better way to do this

-}

import Model exposing (..)
import Track exposing (..)
import User exposing (User)

type Msg
    = UsernameUpdate String
    | SubmitUser
    | PlayTrack
    | StopPlayback
    | SetMode UIMode
    | PlayLabelKey Voice Pitch

    | RemoveNote Int
    | StartDrawing Voice Pitch Float
    | MoveDrawing Float
    | EndDrawing Float
    {- these hacks are necessary because mouse offset is always calculated
    from the parent, which messes up our math if it's the note being drawn
     -}
    | MoveDrawingOnNote Float
    | EndDrawingOnNote Float

    | StartSelection Voice (Float, Float)
    | MoveSelection (Float, Float)
    | EndSelection (Float, Float)

    | StartNoteMove (Float, Float)
    | MoveNoteMove (Float, Float)
    | EndNoteMove (Float, Float)


    | UpdateBeat Float
    -- network
    | SetNotesFromServer (List (Int, Note))
    | AddNoteFromServer (Maybe (Int, Note))
    | RemoveNoteFromServer (Maybe Int)
    | UserRegisteredFromServer (Maybe User)
    | SetUsersFromServer (List User)