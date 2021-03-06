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

    | ToggleVisible Int
    | ToggleMuted Int

    | RemoveNote NoteId
    | StartDrawing Voice Pitch Beats
    | MoveDrawing Beats
    | EndDrawing Beats
    | StartNoteEndAdjust NoteId Beats
    | MoveNoteEndAdjust NoteId Beats
    | EndNoteEndAdjust NoteId Beats
    | StartNoteStartAdjust NoteId Beats
    | MoveNoteStartAdjust NoteId Beats
    | EndNoteStartAdjust NoteId Beats

    | StartSelection Voice (Float, Float)
    | MoveSelection (Float, Float)
    | EndSelection (Float, Float)
    | ToggleInSelection NoteId

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