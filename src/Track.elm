module Track exposing (Note, NoteInstruction, Track, Voice, Pitch, empty, addNote, addNoteWithId, getNote, removeNote, pitchToString, generateInstructions, generatePitchInst)

import Dict exposing (Dict)
import Process
import Task


type alias Voice = Int
type alias Pitch = Int


a4Frq = 440

{-| usable by JS to play a note
-}
type alias NoteInstruction =
  { pitch : String
  , start : Float
  , duration : Float
  , voice : Int
  }

type alias Note =
  { pitch : Int
  , start : Float
  , duration : Float
  , user : Maybe String
  , voice : Int
  }

type alias Track = 
  { notes: Dict Int Note
  , nextId: Int }


empty : Track
empty = 
  { notes = Dict.empty
  , nextId = 0
  }
  

addNote : Note -> Track -> (Track, Int)
addNote note track =
  ( { notes = Dict.insert track.nextId note track.notes
    , nextId = track.nextId + 1
    }
  , track.nextId )


addNoteWithId : Int -> Note -> Track -> Track
addNoteWithId id note track =
  { notes = Dict.insert id note track.notes
  , nextId = 1 + max id track.nextId
  }

  
getNote : Int -> Track -> Maybe Note
getNote id track =
  Dict.get id track.notes


removeNote : Int -> Track -> Track
removeNote id track =
  { track | notes = Dict.remove id track.notes }


{-pitchToNumeric : Pitch -> Int
pitchToNumeric ( letter, octave ) =
  let
    stepsAbove =
      case letter of
        A  -> 0
        AS -> 1
        B  -> 2
        C  -> 3
        CS -> 4
        D  -> 5
        DS -> 6
        E  -> 7
        F  -> 8
        FS -> 9
        G  -> 10
        GS -> 11
  in
    octave * 12 + stepsAbove-}


pitchToString : Int -> String
pitchToString note =
  let
    letter =
      case modBy 12 note of
          0  -> "A"
          1  -> "A#"
          2  -> "B"
          3  -> "C"
          4  -> "C#"
          5  -> "D"
          6  -> "D#"
          7  -> "E"
          8  -> "F"
          9  -> "F#"
          10 -> "G"
          11 -> "G#"
          _  -> "impossible" -- is there a way to fix this?
  in
    letter ++ String.fromInt (note // 12)


{- pitchFrq : Int -> Float
pitchFrq pitch =
  a4Frq * (2 ^ (1 / 12)) ^ toFloat (pitch - 48) -}


delay : Float -> msg -> Cmd msg
delay time msg =
  Process.sleep time
    |> Task.perform (\_ -> msg)


generateNote : Float -> Note -> NoteInstruction
generateNote tempo note =
  let
    secPerBeat =
      (60 / tempo)
  in
    { pitch = pitchToString note.pitch
    , start     = note.start * secPerBeat
    , duration  = note.duration * secPerBeat
    , voice     = note.voice
    }


generateInstructions : Float -> Track -> List NoteInstruction
generateInstructions tempo track =
  track.notes
    |> Dict.toList 
    |> List.map Tuple.second
    |> List.map (generateNote tempo)

generatePitchInst : Int -> Int -> NoteInstruction
generatePitchInst voice pitch =
  { pitch = pitchToString pitch
  , start     = 0
  , duration  = 0.3
  , voice     = voice
  }
