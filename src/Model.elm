module Model exposing (Model, CurrentNote)

import Dict exposing (Dict)

import Track exposing (..)
import User exposing (User)

type alias CurrentNote =
  { voice: Voice
  , pitch: Pitch
  , startX: Float
  , endX: Float
  }

type alias Model =
  { track : Track 
  --pitch, start, and end of note currently being drawn
  , currentNote : Maybe CurrentNote
  -- `Maybe` because the user may not have logged in yet
  , currentUser : Maybe User
  , users : Dict String User
  , usernameInput : String
  -- the beat that playback is currently on, if track is playing
  , playbackBeat : Maybe Float
  , bpm : Float
  }