module Track exposing (Note, NoteInstruction, Track, Voice, Pitch, empty, addNote, addNoteWithId, getNote, getNotes, removeNote, update, pitchToString, generateInstructions, generatePitchInst)

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
  , voice : Voice
  }

type alias Note =
  { pitch : Pitch
  , start : Float
  , duration : Float
  , user : Maybe String
  , voice : Voice
  }

-- TODO: enforce Track abstraction
-- TODO: key notes on id and user
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


getNotes : List Int -> Track -> List Note
getNotes ids track =
  ids
    |> List.map (\id -> Dict.get id track.notes)
    |> List.foldr (\maybeNote notes -> notes ++ (Maybe.map (\n -> [n]) maybeNote |> Maybe.withDefault [])) []


removeNote : Int -> Track -> Track
removeNote id track =
  { track | notes = Dict.remove id track.notes }


{-| maps a function over an entry, if it exists
-}
update : (Note -> Note) -> Int -> Track -> Track
update fn id track =
  let
    maybeFn maybeNote =
      case maybeNote of
        Just note -> Just (fn note)
        Nothing   -> Nothing
  in
    {track | notes = Dict.update id maybeFn track.notes}


pitchToString : Pitch -> String
pitchToString pitch =
  let
    letter =
      case modBy 12 pitch of
          0  -> "C"
          1  -> "C#"
          2  -> "D"
          3  -> "D#"
          4  -> "E"
          5  -> "F"
          6  -> "F#"
          7  -> "G"
          8  -> "G#"
          9  -> "A"
          10 -> "A#"
          11 -> "B"
          _  -> "impossible" -- is there a way to fix this?
  in
    letter ++ String.fromInt ((pitch // 12) - 1)


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

generatePitchInst : Voice -> Pitch -> NoteInstruction
generatePitchInst voice pitch =
  { pitch = pitchToString pitch
  , start     = 0
  , duration  = 0.3
  , voice     = voice
  }
