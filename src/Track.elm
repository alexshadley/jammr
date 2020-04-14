module Track exposing (Note, NoteId, NoteInstruction, Track, Voice, Pitch, Beats, empty, toList, addNote, addNoteWithId, getNote, getNotes, removeNote, duplicateNotes, update, pitchToString)

import Dict exposing (Dict)
import Process
import Task


type alias Voice = Int
type alias Pitch = Int
type alias Beats = Float


a4Frq = 440

{-| usable by JS to play a note
-}
type alias NoteInstruction =
  { pitch : String
  , start : Beats
  , duration : Beats
  , voice : Voice
  }

type alias NoteId = (Int, String)

-- TODO: include id in note
type alias Note =
  { pitch : Pitch
  , start : Float
  , duration : Float
  , user : String
  , voice : Voice
  }

type Track = 
  Track
    { notes: Dict (Int, String) Note
    , nextId: Int }


empty : Track
empty = 
  Track
    { notes = Dict.empty
    , nextId = 0
    }

toList : Track -> List ((Int, String), Note)
toList (Track track) =
  Dict.toList track.notes
  

addNote : Note -> Track -> (Track, (Int, String))
addNote note (Track track) =
  ( Track
      { notes = Dict.insert (track.nextId, note.user) note track.notes
      , nextId = track.nextId + 1
      }
  , (track.nextId, note.user) )


addNoteWithId : NoteId -> Note -> Track -> Track
addNoteWithId id note (Track track) =
  Track
    { notes = Dict.insert id note track.notes
    , nextId = 1 + max (Tuple.first id) track.nextId
    }

  
getNote : (Int, String) -> Track -> Maybe Note
getNote key (Track track) =
  Dict.get key track.notes


getNotes : List (Int, String) -> Track -> List Note
getNotes keys (Track track) =
  keys
    |> List.map (\key -> Dict.get key track.notes)
    |> List.foldr (\maybeNote notes -> notes ++ (Maybe.map (\n -> [n]) maybeNote |> Maybe.withDefault [])) []

duplicateNotes : List (Int, String) -> Track -> (Track, List (Int, String))
duplicateNotes keys (Track track) =
  List.foldr
    (\key (t, newKeys) -> 
      case Dict.get key track.notes of
        Just note ->
          let
            (newTrack, newKey) = addNote note t
          in
            (newTrack, newKeys ++ [newKey])

        Nothing ->
          (t, newKeys)
    )
    (Track track, []) keys

removeNote : (Int, String) -> Track -> Track
removeNote key (Track track) =
  Track { track | notes = Dict.remove key track.notes }


{-| maps a function over an entry, if it exists
-}
update : (Note -> Note) -> (Int, String) -> Track -> Track
update fn key (Track track) =
  let
    maybeFn maybeNote =
      case maybeNote of
        Just note -> Just (fn note)
        Nothing   -> Nothing
  in
    Track {track | notes = Dict.update key maybeFn track.notes}


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
