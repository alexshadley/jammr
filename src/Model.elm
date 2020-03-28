module Model exposing (Model)

import Track exposing (..)
import User exposing (User)

type alias Model =
  { track : Track 
  --pitch, start, and end of note currently being drawn
  , currentNote : Maybe (Int, Float, Float)
  -- `Maybe` because the user may not have logged in yet
  , currentUser : Maybe User
  , users : List User
  , usernameInput : String
  }