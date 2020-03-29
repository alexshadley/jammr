module PianoRoll exposing (pianoRoll, calcStartAndDuration)


import Dict
import Element
import Html.Events.Extra.Mouse as Mouse
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


import Msg exposing (..)
import Model exposing (..)
import Track exposing (..)
import User exposing (User)

rollHeight = 350
rollWidth = 1000
labelWidth = 50

beatCount = 24
topNote = 60
pitchCount = 12
subdivisions = 4

laneHeight = rollHeight / pitchCount
cellWidth = rollWidth / beatCount

defaultColor = "gray"

getUserColor : User -> String
getUserColor user =
  case user.color of
    (r, g, b) -> "rgb(" ++ (String.fromInt r) ++ ", " ++ (String.fromInt g) ++ ", " ++ (String.fromInt b) ++ ")"


{-| Returns (start, duration) from the start and end of a note drawing
-}
calcStartAndDuration : Float -> Float -> (Float, Float)
calcStartAndDuration start end =
  let
    startBin = truncate <| start / ( rollWidth / ( beatCount * subdivisions ) )
    startBeat = toFloat startBin / subdivisions
    endBin = 1 + ( truncate <| end / ( rollWidth / ( beatCount * subdivisions ) ) )
    endBeat = toFloat endBin / subdivisions
  in
    if endBin < startBin then
      (startBeat, 1)
    else
      if startBin == endBin then
        (startBeat, 1)
      else
        (startBeat, endBeat - startBeat)
    

-- for my sanity
px = String.fromInt

pianoRoll : Model -> Int -> Element.Element Msg
pianoRoll model voice =
  Element.el [] <| 
    Element.html <|
      svg 
        [ width (px rollWidth)
        , height (px rollHeight)
        , viewBox ("-" ++ (px labelWidth) ++ " 0 " ++ (px rollWidth) ++ " " ++ (px rollHeight))
        ]
        [pitchLanes voice, dividers, rollNotes model voice, currentNote model voice]

currentNote : Model -> Int -> Svg Msg
currentNote model rollVoice =
  let
    color = case model.currentUser of
      Just user -> getUserColor user
      Nothing -> defaultColor
  in
    case model.currentNote of
      Just {voice, pitch, startX, endX} ->
        case voice == rollVoice of
          True ->
            let
              (start, duration) = calcStartAndDuration startX endX
              xVal = start * cellWidth
              yVal = toFloat (topNote - pitch) * laneHeight
              widthVal = duration * cellWidth
            in
              rect 
                [ x (String.fromFloat xVal)
                , y (String.fromFloat yVal)
                , width (String.fromFloat widthVal)
                , height (String.fromFloat laneHeight)
                , fill color
                , Mouse.onMove (\event -> MoveDrawingOnNote (Tuple.first event.offsetPos) )
                , Mouse.onUp (\event -> EndDrawingOnNote (Tuple.first event.offsetPos) )
                ] []
          
          False ->
            g [] []

      Nothing ->
        g [] []

rollNotes : Model -> Int -> Svg Msg
rollNotes model rollVoice =
  let
    notes =
      Dict.toList model.track.notes
        |> List.filter (\(_, {voice}) -> voice == rollVoice)
  in
    g [] (List.map (rollNote model) notes)

rollNote : Model -> (Int, Note) -> Svg Msg
rollNote model (id, note) =
  let
    xVal = note.start * cellWidth
    yVal = toFloat (topNote - note.pitch) * laneHeight
    widthVal = note.duration * cellWidth

    color = case note.user of
      Just name ->
        case Dict.get name model.users of
          Just user -> getUserColor user
          Nothing -> defaultColor
      
      Nothing -> defaultColor

  in
    rect 
      [ x (String.fromFloat xVal)
      , y (String.fromFloat yVal)
      , width (String.fromFloat widthVal)
      , height (String.fromFloat laneHeight)
      , fill color
      , onClick (RemoveNote id)
      ] []

splitAlternating : List a -> (List a, List a)
splitAlternating xs =
  let
    tagged = List.indexedMap (\i x -> (i, x)) xs
    left = tagged |> List.filter (Tuple.first >> (\x -> modBy 2 x == 0)) |> List.map Tuple.second
    right = tagged |> List.filter (Tuple.first >> (\x -> modBy 2 x == 1)) |> List.map Tuple.second
  in
    (left, right)
    
pitchLanes : Int -> Svg Msg
pitchLanes voice = 
  let
    pitches = List.range (topNote - pitchCount) topNote
    lanes = List.map (pitchRow voice) pitches
    (white, gray) = splitAlternating lanes

  in
    g []
      [ g [ color "white" ] white
      , g [ color "lightgray" ] gray
      ]

pitchRow : Int -> Int -> Svg Msg
pitchRow voice pitch =
  g []
    [ pitchLane voice pitch
    , pitchLabel pitch
    ]

pitchLabel : Int -> Svg msg
pitchLabel pitch =
  let
    yVal = -10 + toFloat (topNote - pitch) * laneHeight
  in
    text_
      [ x (String.fromInt -50)
      , y (String.fromFloat yVal)
      , fill "black"
      ] [ text (pitchToString pitch) ]

pitchLane : Int -> Int -> Svg Msg
pitchLane voice pitch =
  let
    yVal = toFloat (topNote - pitch) * laneHeight
  in
    rect 
      [ x (String.fromInt 0)
      , y (String.fromFloat yVal)
      , width (String.fromInt rollWidth)
      , height (String.fromFloat laneHeight)
      , fill "currentColor"
      , Mouse.onDown (\event -> StartDrawing voice pitch (Tuple.first event.offsetPos) )
      , Mouse.onMove (\event -> MoveDrawing (Tuple.first event.offsetPos) )
      , Mouse.onUp (\event -> EndDrawing (Tuple.first event.offsetPos) )
      ] []

dividers : Svg Msg
dividers = 
  let
    positions = List.range 1 beatCount |> List.map (\x -> toFloat x * (rollWidth / beatCount))
  in
    g [color "gray"] (List.map divider positions)

divider : Float -> Svg Msg
divider xVal =
  rect 
    [x (String.fromFloat xVal)
    , y (String.fromFloat 0)
    , width (String.fromFloat 2)
    , height (String.fromInt rollHeight)
    , fill "currentColor"
    ] []
