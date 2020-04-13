module Msg exposing (Msg(..))
{-| Separated because PianoRoll also needs access to Msg. There is almost
certainly a better way to do this

-}

import Model exposing (..)
import Track exposing (..)
import User exposing (User)

type Msg
    = GotRollPos Int (Float, Float)
    
    | UsernameUpdate String
    | SubmitUser
    | PlayTrack
    | StopPlayback
    | SetMode UIMode
    | ChangeBPM Float
    | PlayLabelKey Voice Pitch

    | RemoveNote NoteId
    | StartDrawing Voice Pitch Float
    | MoveDrawing Float
    | EndDrawing Float

    | StartSelection Voice (Float, Float)
    | MoveSelection (Float, Float)
    | EndSelection (Float, Float)

    | StartNoteMove (Float, Float)
    | MoveNoteMove (Float, Float)
    | EndNoteMove (Float, Float)

    | KeyPressed (String)

    | UpdateBeat Float
    -- network
    | SetNotesFromServer (List (NoteId, Note))
    | AddNoteFromServer (Maybe (NoteId, Note))
    | UpdateNotesFromServer (List (NoteId, Note))
    | RemoveNotesFromServer (List NoteId)
    | UserRegisteredFromServer (Maybe User)
    | SetUsersFromServer (List User)