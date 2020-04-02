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

laneColor1 = "white"
laneColor2 = "lightgray"
dividerColor = "#bbbbbb"

{-| Returns (start, duration) from the start and end of a note drawing
-}
calcStartAndDuration : CurrentNote -> Float -> (Float, Float)
calcStartAndDuration {startX, endX, leftStartArea} prevSize =
  let
    startBin = truncate <| startX / ( rollWidth / ( beatCount * subdivisions ) )
    startBeat = toFloat startBin / subdivisions
    endBin = 1 + ( truncate <| endX / ( rollWidth / ( beatCount * subdivisions ) ) )
    endBeat = toFloat endBin / subdivisions

    duration =
      if leftStartArea then
        endBeat - startBeat
      else
        Basics.max prevSize (endBeat - startBeat)
  in
    (startBeat, duration)
  

-- gives fill and border colors
getUserColors : User -> (String, String)
getUserColors user =
  case user.color of
    (r, g, b) -> 
      let
        borderR = Basics.max 0 (r - 50)
        borderG = Basics.max 0 (g - 50)
        borderB = Basics.max 0 (b - 50)
      in
        ( "rgb(" ++ (String.fromInt r) ++ ", " ++ (String.fromInt g) ++ ", " ++ (String.fromInt b) ++ ")"
        , "rgb(" ++ (String.fromInt borderR) ++ ", " ++ (String.fromInt borderG) ++ ", " ++ (String.fromInt borderB) ++ ")"
        )


noteStyling : Maybe User -> List (Svg.Attribute msg)
noteStyling user =
  let
    (fillColor, borderColor) =
      case user of
        Just u -> getUserColors u
        Nothing   -> (defaultColor, defaultColor)

  in
    [ fill fillColor
    , stroke borderColor
    , strokeWidth "1.5"
    , rx "5"
    ]


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
  case model.currentNote of
    Just ({voice, pitch, startX, endX} as note) ->
      case voice == params.voice of
        True ->
          let
            (start, duration) = calcStartAndDuration note model.lastNoteBeats
            xVal = start * cellWidth
            yVal = toFloat (params.topPitch - pitch) * params.laneHeight
            widthVal = duration * cellWidth
          in
            rect 
              ( [ x (String.fromFloat xVal)
                , y (String.fromFloat yVal)
                , width (String.fromFloat widthVal)
                , height (String.fromFloat params.laneHeight)
                , Mouse.onMove (\event -> MoveDrawingOnNote (Tuple.first event.offsetPos) )
                , Mouse.onUp (\event -> EndDrawingOnNote (Tuple.first event.offsetPos) )
                ] ++ noteStyling model.currentUser
              ) []
        
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

    user = note.user |> Maybe.andThen (\name -> Dict.get name model.users)

  in
    rect 
      ( [ x (String.fromFloat xVal)
        , y (String.fromFloat yVal)
        , width (String.fromFloat widthVal)
        , height (String.fromFloat params.laneHeight)
        , onClick (RemoveNote id)
        ] ++ noteStyling user
      ) []

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
    dividerNumbers = List.range 1 beatCount
  in
    g [color dividerColor] (List.map (divider params) dividerNumbers)

divider : Params -> Int -> Svg Msg
divider params num =
  let
    widthVal = if modBy 4 num == 0 then 4 else 2
    xVal = toFloat num * (rollWidth / beatCount) - (widthVal / 2)
  in
    rect 
      [x (String.fromFloat xVal)
      , y (String.fromFloat 0)
      , width (String.fromFloat widthVal)
      , height (String.fromInt params.rollHeight)
      , fill "currentColor"
      ] []
