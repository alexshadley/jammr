module Model exposing (Model, InputParams, CurrentNote, BoxSelection, UIMode(..), SelectingMode(..))

import Dict exposing (Dict)
import Set exposing (Set)

import Track exposing (..)
import User exposing (User)

type alias InputParams =
  { rollHeight: Int
  , voice: Int
  , topPitch: Int
  , pitches: Int
  -- if set, overrides other settings
  , unpitchedVoices: Maybe (List String)
  }

-- mode and mode-specific state need to be managed more reasonably
type SelectingMode = Moving | Copying
type UIMode = Painting | Selecting SelectingMode (Maybe {start: (Float, Float), end: (Float, Float)})

type alias CurrentNote =
  { voice         : Voice
  , pitch         : Pitch
  , startX        : Float
  , endX          : Float
  , leftStartArea : Bool
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
  , selectedNotes : Set Int
  -- `Maybe` because the user may not have logged in yet
  , currentUser : Maybe User
  , users : Dict String User
  , usernameInput : String
  -- the beat that playback is currently on, if track is playing
  , playbackBeat : Maybe Float
  , bpm : Float
  }