module PianoRoll exposing (pianoRoll, calcStartAndDuration, noteInSelection, calcOffsetBeats, calcOffsetPitches, calcParams)


import Dict
import Set
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
labelWidth = 90

beatCount = 24
subdivisions = 4

cellWidth = rollWidth / beatCount

defaultColor = "gray"
selectionColor = "red"

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

calcNotePos : Params -> Note -> ((Float, Float), (Float, Float))
calcNotePos params note =
  let 
    sx = note.start * cellWidth
    sy = toFloat (params.topPitch - note.pitch) * params.laneHeight
    ex = sx + note.duration * cellWidth
    ey = sy + params.laneHeight
  in
    ((sx, sy), (ex, ey))

calcOffsetBeats : Params -> Float -> Float
calcOffsetBeats params dx =
  let
    bins = truncate <| dx / (params.cellWidth / subdivisions)
  in
    toFloat bins / subdivisions

calcOffsetPitches : Params -> Float -> Int
calcOffsetPitches params dy =
  truncate <| dy / params.laneHeight

{-| Calculates the four points of a note, checks to see if at least one is in
the provided selection box
-}
noteInSelection : Params -> BoxSelection -> Note -> Bool
noteInSelection params selection note =
  let
    ((sx, sy), (ex, ey)) = calcNotePos params note
    notePoints = [(sx, sy), (sx, ey), (ex, sy), (ex, ey)]

    (selx1, sely1) = selection.start
    (selx2, sely2) = selection.end

    (selsx, selex) = (Basics.min selx1 selx2, Basics.max selx1 selx2)
    (selsy, seley) = (Basics.min sely1 sely2, Basics.max sely1 sely2)

  in
    if selection.voice == note.voice then
      notePoints
        |> List.map (\(x, y) -> selsx <= x && x <= selex && selsy <= y && y <= seley)
        |> List.member True

    else
      False
  

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


noteStyling : Model -> Maybe User -> Maybe NoteId -> List (Svg.Attribute msg)
noteStyling model user id =
  let
    (fillColor, borderColor) =
      case user of
        Just u  -> getUserColors u
        Nothing -> (defaultColor, defaultColor)
    
    opacityVal =
      case (Set.isEmpty model.selectedNotes, Set.member (Maybe.withDefault (-1, "") id) model.selectedNotes) of
        (False, False) -> 0.5
        _              -> 1.0

  in
    [ fill fillColor
    , opacity (String.fromFloat opacityVal)
    , stroke borderColor
    , strokeWidth "1.5"
    , rx "5"
    ]



-- also includes calculated values
type alias Params =
  { id         : Int
  , pagePos    : (Float, Float)
  , rollHeight : Int
  , voice      : Int
  , topPitch   : Int
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
  , topPitch = input.topPitch
  , pitches = input.pitches
  , laneHeight = toFloat input.rollHeight / toFloat input.pitches
  , cellWidth = rollWidth / beatCount
  , unpitchedVoices = input.unpitchedVoices
  }

pianoRoll : Model -> InputParams -> Element.Element Msg
pianoRoll model input =
  let
    params = calcParams input
    totalWidth = rollWidth + labelWidth
  in
    Element.el [] <| 
      Element.html <|
        svg 
          [ width (String.fromInt totalWidth)
          , height (String.fromInt params.rollHeight)
          , viewBox ("-" ++ (String.fromInt labelWidth) ++ " 0 " ++ (String.fromInt totalWidth) ++ " " ++ (String.fromInt params.rollHeight))
          , id ("piano-roll-" ++ String.fromInt params.id)
          ]
          [ pitchLanes params
          , dividers params
          , rollNotes model params
          , currentNote model params
          , playbackPosition model params
          , visualOverlay model params
          , controlOverlay model params
          ]


visualOverlay : Model -> Params -> Svg msg
visualOverlay model params =
  g [] [ selectionBox model params ]
  

selectionBox : Model -> Params -> Svg msg
selectionBox model params =
  case model.currentSelection of
    Just selection ->
      case selection.voice == params.voice of
        True ->
          let
            (sx, sy) = selection.start
            (ex, ey) = selection.end

            (xVal, yVal) = (Basics.min sx ex, Basics.min sy ey)
            (widthVal, heightVal) = (abs (sx - ex), abs (sy - ey))
          in
            rect
              [ x (String.fromFloat xVal)
              , y (String.fromFloat yVal)
              , width (String.fromFloat widthVal)
              , height (String.fromFloat heightVal)
              , stroke "black"
              , fill "rgba(255, 255, 255, 0)"
              ]
              []

        False ->
          g [] []

    Nothing ->
      g [] []


controlOverlay : Model -> Params -> Svg Msg
controlOverlay model params =
  let
    children =
      case model.uiMode of
        Selecting _ Nothing ->
          let
            selectedNotes = Track.getNotes (Set.toList model.selectedNotes) model.track
          in
            [ baseOverlay model params ] ++
            ( List.map (noteHandle model params) selectedNotes )

        Selecting _ (Just _) ->
          [ baseOverlay model params ]
        
        _ -> []
  in
    g [ opacity "0" ] children


tupleMinus : (Float, Float) -> (Float, Float) -> (Float, Float)
tupleMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

baseOverlay : Model -> Params -> Svg Msg
baseOverlay model params =
  let
    actions =
      case model.uiMode of
        Selecting _ Nothing ->
          [ Mouse.onDown (\e -> StartSelection params.voice (tupleMinus e.pagePos params.pagePos))
          , Mouse.onMove (\e -> MoveSelection (tupleMinus e.pagePos params.pagePos))
          , Mouse.onUp (\e -> EndSelection (tupleMinus e.pagePos params.pagePos))
          ]
        
        Selecting _ (Just _) ->
          [ Mouse.onMove (\e -> MoveNoteMove (tupleMinus e.pagePos params.pagePos))
          , Mouse.onUp (\e -> EndNoteMove (tupleMinus e.pagePos params.pagePos))
          ]
        
        _ -> []

  in
    rect 
      ( [ x "0"
        , y "0"
        , width (String.fromInt rollWidth)
        , height (String.fromInt params.rollHeight)
        ] ++ actions
      ) []


noteHandle : Model -> Params -> Note -> Svg Msg
noteHandle model params note =
  let
    ((sx, sy), (ex, ey)) = calcNotePos params note
  in
    rect 
      [ x (String.fromFloat sx)
      , y (String.fromFloat sy)
      , width (String.fromFloat (ex - sx))
      , height (String.fromFloat (ey - sy))
      , Mouse.onDown (\e ->
        case e.offsetPos of
          (offX, offY) -> StartNoteMove (tupleMinus e.pagePos params.pagePos)
        )
      ] []



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
                , Mouse.onMove (\event -> MoveDrawing (Tuple.first event.pagePos - Tuple.first params.pagePos) )
                , Mouse.onUp (\event -> EndDrawing (Tuple.first event.pagePos - Tuple.first params.pagePos) )
                ] ++ noteStyling model model.currentUser Nothing
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

rollNote : Model -> Params -> ((Int, String), Note) -> Svg Msg
rollNote model params (id, note) =
  let
    ((sx, sy), (ex, ey)) = calcNotePos params note
    (xOffset, yOffset) =
      case (model.uiMode, Set.member id model.selectedNotes) of
        (Selecting _ (Just {start, end}), True) ->
          let
            ((msx, msy), (mex, mey)) = (start, end)
          in
            ( calcOffsetBeats params (mex - msx) * params.cellWidth
            , toFloat ( calcOffsetPitches params (mey - msy) ) * params.laneHeight
            )

        _ -> (0.0, 0.0)

    user = Dict.get note.user model.users
  in
    rect 
      ( [ x (String.fromFloat (sx + xOffset))
        , y (String.fromFloat (sy + yOffset))
        , width (String.fromFloat (ex - sx))
        , height (String.fromFloat (ey - sy))
        , onClick (RemoveNote id)
        ] ++ noteStyling model user (Just id)
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
    pitches = List.range (params.topPitch - params.pitches + 1) params.topPitch
    lanes =
      case params.unpitchedVoices of
        Just labels -> List.map2 (unpitchedRow params) pitches labels
        Nothing     -> List.map (pitchRow params) pitches
    (white, gray) = splitAlternating lanes

  in
    g []
      [ g [ color "white" ] white
      , g [ color "lightgray" ] gray
      ]

    
unpitchedRow : Params -> Pitch -> String -> Svg Msg
unpitchedRow params pitch name =
  g []
    [ pitchLane params pitch
    , unpitchedLabel params pitch name
    ]

unpitchedLabel : Params -> Pitch -> String -> Svg Msg
unpitchedLabel params pitch name =
  let
    yVal = toFloat (params.topPitch - pitch) * params.laneHeight + ((4.0 / 5.0) * params.laneHeight)
  in
    text_
      [ x (String.fromInt -labelWidth)
      , y (String.fromFloat yVal)
      , fill "black"
      , Mouse.onDown (\_ -> PlayLabelKey params.voice pitch)
      ] [ text name ]
    

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
      , Mouse.onDown (\event -> StartDrawing params.voice pitch (Tuple.first event.pagePos - Tuple.first params.pagePos) )
      , Mouse.onMove (\event -> MoveDrawing (Tuple.first event.pagePos - Tuple.first params.pagePos) )
      , Mouse.onUp (\event -> EndDrawing (Tuple.first event.pagePos - Tuple.first params.pagePos) )
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
