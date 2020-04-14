module PianoRoll.Model exposing (..)

import Track exposing (..)

rollWidth = 1000
labelWidth = 90

beatCount = 24
subdivisions = 4

cellWidth = rollWidth / beatCount

type alias InputParams =
  { id         : Int
  , pagePos    : (Float, Float)
  , rollHeight : Int
  , voice      : Voice
  , voiceName  : String
  , visible    : Bool
  , muted      : Bool
  , topPitch   : Pitch
  , pitches    : Int
  -- if set, overrides other settings
  , unpitchedVoices: Maybe (List String)
  }

-- also includes calculated values
type alias Params =
  { id         : Int
  , pagePos    : (Float, Float)
  , rollHeight : Int
  , voice      : Voice
  , voiceName  : String
  , visible    : Bool
  , muted      : Bool
  , topPitch   : Pitch
  , pitches    : Int
  , laneHeight : Float
  , cellWidth  : Float
  , unpitchedVoices : Maybe (List String)
  }

calcParams : InputParams -> Params
calcParams input =
  { id = input.id
  , pagePos = input.pagePos
  , rollHeight = input.rollHeight
  , voice = input.voice
  , voiceName = input.voiceName
  , visible = input.visible
  , muted = input.muted
  , topPitch = input.topPitch
  , pitches = input.pitches
  , laneHeight = toFloat input.rollHeight / toFloat input.pitches
  , cellWidth = rollWidth / beatCount
  , unpitchedVoices = input.unpitchedVoices
  }