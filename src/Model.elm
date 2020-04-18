module Model exposing (Model, CurrentNote, BoxSelection, UIMode(..), DrawingMode(..), SelectingMode(..))

import Dict exposing (Dict)
import Set exposing (Set)

import Track exposing (..)
import PianoRoll.Model exposing (..)
import User exposing (User)

-- TODO: refactor mode to include more mode-specific state
type DrawingMode = Adding | AdjustingStart NoteId Beats | AdjustingEnd NoteId Beats
type SelectingMode = Moving | Copying
-- TODO: change painting into drawing
type UIMode = Painting DrawingMode | Selecting SelectingMode (Maybe {start: (Float, Float), end: (Float, Float)})

type alias CurrentNote =
  { voice         : Voice
  , pitch         : Pitch
  , start         : Beats
  }

type alias BoxSelection =
  { voice : Voice
  , start : (Float, Float)
  , end   : (Float, Float)
  }

type alias Model =
  { pianoRolls : List Params
  , track : Track 
  --pitch, start, and end of note currently being drawn
  , uiMode : UIMode
  , currentNote : Maybe CurrentNote
  , lastNoteBeats : Float
  , currentSelection : Maybe BoxSelection
  , selectedNotes : Set NoteId
  , currentUser : User
  , users : Dict String User
  , usernameInput : String
  -- the beat that playback is currently on, if track is playing
  , playbackBeat : Maybe Float
  , bpm : Float
  }