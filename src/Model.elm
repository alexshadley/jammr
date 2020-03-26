module Model exposing (Model)

import Track exposing (..)

type alias Model =
  { track : Track 
  --pitch, start, and end of note currently being drawn
  , currentNote : Maybe (Int, Float, Float)
  }