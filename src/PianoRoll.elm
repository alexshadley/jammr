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

rollWidth = 1000
labelWidth = 50

beatCount = 24
subdivisions = 4

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



type alias InputParams =
  { rollHeight: Int
  , voice: Int
  , topPitch: Int
  , pitches: Int
  }

-- also includes calculated values
type alias Params =
  { rollHeight: Int
  , voice: Int
  , topPitch: Int
  , pitches: Int
  , laneHeight: Float
  }

calculateParams : InputParams -> Params
calculateParams input =
  { rollHeight = input.rollHeight
  , voice = input.voice
  , topPitch = input.topPitch
  , pitches = input.pitches
  , laneHeight = toFloat input.rollHeight / toFloat input.pitches
  }

pianoRoll : Model -> InputParams -> Element.Element Msg
pianoRoll model input =
  let
    params = calculateParams input
    totalWidth = rollWidth + labelWidth
  in
    Element.el [] <| 
      Element.html <|
        svg 
          [ width (String.fromInt totalWidth)
          , height (String.fromInt params.rollHeight)
          , viewBox ("-" ++ (String.fromInt labelWidth) ++ " 0 " ++ (String.fromInt totalWidth) ++ " " ++ (String.fromInt params.rollHeight))
          ]
          [pitchLanes params, dividers params, rollNotes model params, currentNote model params, playbackPosition model params]

playbackPosition : Model -> Params -> Svg Msg
playbackPosition model params =
  case model.playbackBeat of
    Just beat ->
      let
        xVal = cellWidth * beat
      in
        rect 
          [ x (String.fromFloat xVal)
          , y (String.fromFloat 0)
          , width (String.fromFloat 2)
          , height (String.fromInt params.rollHeight)
          , fill "red"
          ] []

    Nothing ->
      g [] []


currentNote : Model -> Params -> Svg Msg
currentNote model params =
  let
    color = case model.currentUser of
      Just user -> getUserColor user
      Nothing -> defaultColor
  in
    case model.currentNote of
      Just {voice, pitch, startX, endX} ->
        case voice == params.voice of
          True ->
            let
              (start, duration) = calcStartAndDuration startX endX
              xVal = start * cellWidth
              yVal = toFloat (params.topPitch - pitch) * params.laneHeight
              widthVal = duration * cellWidth
            in
              rect 
                [ x (String.fromFloat xVal)
                , y (String.fromFloat yVal)
                , width (String.fromFloat widthVal)
                , height (String.fromFloat params.laneHeight)
                , fill color
                , Mouse.onMove (\event -> MoveDrawingOnNote (Tuple.first event.offsetPos) )
                , Mouse.onUp (\event -> EndDrawingOnNote (Tuple.first event.offsetPos) )
                ] []
          
          False ->
            g [] []

      Nothing ->
        g [] []

rollNotes : Model -> Params -> Svg Msg
rollNotes model params =
  let
    notes =
      Dict.toList model.track.notes
        |> List.filter (\(_, {voice}) -> voice == params.voice)
  in
    g [] (List.map (rollNote model params) notes)

rollNote : Model -> Params -> (Int, Note) -> Svg Msg
rollNote model params (id, note) =
  let
    xVal = note.start * cellWidth
    yVal = toFloat (params.topPitch - note.pitch) * params.laneHeight
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
      , height (String.fromFloat params.laneHeight)
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
    
pitchLanes : Params -> Svg Msg
pitchLanes params = 
  let
    pitches = List.range (params.topPitch - params.pitches) params.topPitch
    lanes = List.map (pitchRow params) pitches
    (white, gray) = splitAlternating lanes

  in
    g []
      [ g [ color "white" ] white
      , g [ color "lightgray" ] gray
      ]

pitchRow : Params -> Int -> Svg Msg
pitchRow params pitch =
  g []
    [ pitchLane params pitch
    , pitchLabel params pitch
    ]

pitchLabel : Params -> Int -> Svg Msg
pitchLabel params pitch =
  let
    yVal = toFloat (params.topPitch - pitch) * params.laneHeight + ((4.0 / 5.0) * params.laneHeight)
  in
    text_
      [ x (String.fromInt -50)
      , y (String.fromFloat yVal)
      , fill "black"
      , Mouse.onDown (\_ -> PlayLabelKey params.voice pitch)
      ] [ text (pitchToString pitch) ]

pitchLane : Params -> Int -> Svg Msg
pitchLane params pitch =
  let
    yVal = toFloat (params.topPitch - pitch) * params.laneHeight
  in
    rect 
      [ x (String.fromInt 0)
      , y (String.fromFloat yVal)
      , width (String.fromInt rollWidth)
      , height (String.fromFloat params.laneHeight)
      , fill "currentColor"
      , Mouse.onDown (\event -> StartDrawing params.voice pitch (Tuple.first event.offsetPos) )
      , Mouse.onMove (\event -> MoveDrawing (Tuple.first event.offsetPos) )
      , Mouse.onUp (\event -> EndDrawing (Tuple.first event.offsetPos) )
      ] []

dividers : Params -> Svg Msg
dividers params = 
  let
    positions = List.range 1 beatCount |> List.map (\x -> toFloat x * (rollWidth / beatCount))
  in
    g [color "gray"] (List.map (divider params) positions)

divider : Params -> Float -> Svg Msg
divider params xVal =
  rect 
    [x (String.fromFloat xVal)
    , y (String.fromFloat 0)
    , width (String.fromFloat 2)
    , height (String.fromInt params.rollHeight)
    , fill "currentColor"
    ] []
