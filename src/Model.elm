module Model exposing (Model, CurrentNote, BoxSelection, UIMode(..))

import Dict exposing (Dict)

import Track exposing (..)
import User exposing (User)

type UIMode = Painting | Selecting

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
  { track : Track 
  --pitch, start, and end of note currently being drawn
  , uiMode : UIMode
  , currentNote : Maybe CurrentNote
  , lastNoteBeats : Float
  , currentSelection : Maybe BoxSelection
  -- `Maybe` because the user may not have logged in yet
  , currentUser : Maybe User
  , users : Dict String User
  , usernameInput : String
  -- the beat that playback is currently on, if track is playing
  , playbackBeat : Maybe Float
  , bpm : Float
  }