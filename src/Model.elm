module Model exposing (Model, InputParams, CurrentNote, BoxSelection, UIMode(..), SelectingMode(..))

import Dict exposing (Dict)
import Set exposing (Set)

import Track exposing (..)
import User exposing (User)

type alias InputParams =
  { id         : Int
  , pagePos    : (Float, Float)
  , rollHeight : Int
  , voice      : Voice
  , topPitch   : Pitch
  , pitches    : Int
  -- if set, overrides other settings
  , unpitchedVoices: Maybe (List String)
  }

-- TODO: refactor mode to include more mode-specific state
type SelectingMode = Moving | Copying
type UIMode = Painting | Selecting SelectingMode (Maybe {start: (Float, Float), end: (Float, Float)})

type alias CurrentNote =
  { voice         : Voice
  , pitch         : Pitch
  , x             : Float
  }

type alias BoxSelection =
  { voice : Voice
  , start : (Float, Float)
  , end   : (Float, Float)
  }

type alias Model =
  { pianoRolls : List InputParams
  , track : Track 
  --pitch, start, and end of note currently being drawn
  , uiMode : UIMode
  , currentNote : Maybe CurrentNote
  , lastNoteBeats : Float
  , currentSelection : Maybe BoxSelection
  , selectedNotes : Set NoteId
  -- `Maybe` because the user may not have logged in yet
  -- TODO: make not maybe
  , currentUser : Maybe User
  , users : Dict String User
  , usernameInput : String
  -- the beat that playback is currently on, if track is playing
  , playbackBeat : Maybe Float
  , bpm : Float
  }