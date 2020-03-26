module Track exposing (Note, NoteInstruction, Track, empty, addNote, getNote, removeNote, pitchToString, generateInstructions)

import Dict exposing (Dict)
import Process
import Task


{-exampleTrack =
  empty
    |> addNote { pitch = ( C, 4 ), start = 0, duration = 1 }
    |> addNote { pitch = ( D, 4 ), start = 1, duration = 1 }
    |> addNote { pitch = ( E, 4 ), start = 2, duration = 1 }-}


a4Frq = 440


{- type Letter = A | AS | B | C | CS | D | DS | E | F | FS | G | GS

type alias Pitch =
  ( Letter, Int )-}

{-| usable by JS to play a note
-}
type alias NoteInstruction =
  { frequency: Float
  , start : Float
  , duration : Float
  }

type alias Note =
  { pitch : Int
  , start : Float
  , duration : Float
  }

type alias Track = 
  { notes: Dict Int Note
  , nextId: Int }


empty : Track
empty = 
  { notes = Dict.empty
  , nextId = 0
  }
  

addNote : Note -> Track -> Track
addNote note track =
  { notes = Dict.insert track.nextId note track.notes
  , nextId = track.nextId + 1
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
    letter ++ " " ++ String.fromInt (note // 12)


pitchFrq : Int -> Float
pitchFrq pitch =
  a4Frq * (2 ^ (1 / 12)) ^ toFloat (pitch - 48)


delay : Float -> msg -> Cmd msg
delay time msg =
  Process.sleep time
    |> Task.perform (\_ -> msg)


playNote : (Float -> msg) -> msg -> Int -> Note -> Cmd msg
playNote playCmd stopCmd tempo note =
  let
    msPerBeat =
      (60 / toFloat tempo) * 1000
  in
    Cmd.batch
      [ delay (note.start * msPerBeat) <| playCmd (pitchFrq note.pitch)
      , delay ((note.start + note.duration - 0.01) * msPerBeat) stopCmd
      ]


{-| Produces a command to play the notes of a track

Note that we pass in the constructors for play and stop commands, that way we
don't have to import them. There's probably a better way to do this.

-}
{-playTrack : (Float -> msg) -> msg -> Int -> Track -> Cmd msg
playTrack playCmd stopCmd tempo track =
  track.notes
    |> Dict.toList 
    |> List.map Tuple.second
    |> List.map (playNote playCmd stopCmd tempo)
    |> Cmd.batch-}

generateNote : Float -> Note -> NoteInstruction
generateNote tempo note =
  let
    secPerBeat =
      (60 / tempo)
  in
    { frequency = pitchFrq note.pitch
    , start     = note.start * secPerBeat
    , duration  = note.duration * secPerBeat
    }


generateInstructions : Float -> Track -> List NoteInstruction
generateInstructions tempo track =
  track.notes
    |> Dict.toList 
    |> List.map Tuple.second
    |> List.map (generateNote tempo)
