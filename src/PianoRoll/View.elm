module PianoRoll.View exposing (view)

import PianoRoll.Helper exposing (..)
import PianoRoll.Model exposing (..)

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


defaultColor = "gray"
selectionColor = "red"

laneColor1 = "white"
laneColor2 = "lightgray"
dividerColor = "#bbbbbb"

  

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


view : Model -> InputParams -> Element.Element Msg
view model input =
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
          [ visualLayer model params
          , controlOverlay model params
          ]


controlOverlay : Model -> Params -> Svg Msg
controlOverlay model params =
  let
    noteHandles =
      case model.uiMode of
        Selecting _ Nothing ->
          let
            noteIds = Set.toList model.selectedNotes
            selectedNotes =
              List.map2 Tuple.pair
                ( noteIds )
                ( Track.getNotes noteIds model.track )

          in
            List.map ( noteHandle model params (\_ pos -> StartNoteMove pos) ) selectedNotes
        
        Painting ->
          List.map ( noteHandle model params (\(id, _) _ -> RemoveNote id) ) (Track.toList model.track)
        
        _ -> []
  in
    g [ opacity "0" ]
      ( [ baseOverlay model params ] ++ noteHandles )


tupleMinus : (Float, Float) -> (Float, Float) -> (Float, Float)
tupleMinus (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)


baseOverlay : Model -> Params -> Svg Msg
baseOverlay model params =
  let
    rollHandler on fn = on (\e -> fn (tupleMinus e.pagePos params.pagePos))
    onDown = rollHandler Mouse.onDown
    onMove = rollHandler Mouse.onMove
    onUp = rollHandler Mouse.onUp

    actions =
      case model.uiMode of
        Selecting _ Nothing ->
          [ onDown (\pos -> StartSelection params.voice pos)
          , onMove (\pos -> MoveSelection pos)
          , onUp (\pos -> EndSelection pos)
          ]
        
        Selecting _ (Just _) ->
          [ onMove (\pos -> MoveNoteMove pos)
          , onUp (\pos -> EndNoteMove pos)
          ]
        
        Painting ->
          [ onDown (\(x, y) -> StartDrawing params.voice (calcPitch params y) x )
          , onMove (\(x, _) -> MoveDrawing x )
          , onUp (\(x, _) -> EndDrawing x )
          ]

  in
    rect 
      ( [ x "0"
        , y "0"
        , width (String.fromInt rollWidth)
        , height (String.fromInt params.rollHeight)
        ] ++ actions
      ) []


noteHandle : Model -> Params -> (((Int, String), Note) -> (Float, Float) -> Msg) -> ((Int, String), Note) -> Svg Msg
noteHandle model params fn ((id, user), note) =
  let
    ((sx, sy), (ex, ey)) = calcNotePos params note
  in
    rect 
      [ x (String.fromFloat sx)
      , y (String.fromFloat sy)
      , width (String.fromFloat (ex - sx))
      , height (String.fromFloat (ey - sy))
      , Mouse.onDown (\e -> fn ((id, user), note) (tupleMinus e.pagePos params.pagePos))
      ] []


visualLayer : Model -> Params -> Svg msg
visualLayer model params =
  g []
    [ pitchLanes params
    , dividers params
    , rollNotes model params
    , currentNote model params
    , playbackPosition model params
    , selectionBox model params
    ]


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


playbackPosition : Model -> Params -> Svg msg
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


currentNote : Model -> Params -> Svg msg
currentNote model params =
  case model.currentNote of
    Just ({voice, pitch} as note) ->
      case voice == params.voice of
        True ->
          let
            (start, duration) = (calcBeats params note.x, model.lastNoteBeats)
            xVal = start * cellWidth
            yVal = toFloat (params.topPitch - pitch) * params.laneHeight
            widthVal = duration * cellWidth
          in
            rect 
              ( [ x (String.fromFloat xVal)
                , y (String.fromFloat yVal)
                , width (String.fromFloat widthVal)
                , height (String.fromFloat params.laneHeight)
                ] ++ noteStyling model model.currentUser Nothing
              ) []
        
        False ->
          g [] []

    Nothing ->
      g [] []


rollNotes : Model -> Params -> Svg msg
rollNotes model params =
  let
    notes =
      Track.toList model.track
        |> List.filter (\(_, {voice}) -> voice == params.voice)
  in
    g [] (List.map (rollNote model params) notes)


rollNote : Model -> Params -> ((Int, String), Note) -> Svg msg
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


pitchLanes : Params -> Svg msg
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

    
unpitchedRow : Params -> Pitch -> String -> Svg msg
unpitchedRow params pitch name =
  g []
    [ pitchLane params pitch
    , unpitchedLabel params pitch name
    ]


unpitchedLabel : Params -> Pitch -> String -> Svg msg
unpitchedLabel params pitch name =
  let
    yVal = toFloat (params.topPitch - pitch) * params.laneHeight + ((4.0 / 5.0) * params.laneHeight)
  in
    text_
      [ x (String.fromInt -labelWidth)
      , y (String.fromFloat yVal)
      , fill "black"
      --, Mouse.onDown (\_ -> PlayLabelKey params.voice pitch)
      ] [ text name ]
    

pitchRow : Params -> Int -> Svg msg
pitchRow params pitch =
  g []
    [ pitchLane params pitch
    , pitchLabel params pitch
    ]

pitchLabel : Params -> Int -> Svg msg
pitchLabel params pitch =
  let
    yVal = toFloat (params.topPitch - pitch) * params.laneHeight + ((4.0 / 5.0) * params.laneHeight)
  in
    text_
      [ x (String.fromInt -50)
      , y (String.fromFloat yVal)
      , fill "black"
      --, Mouse.onDown (\_ -> PlayLabelKey params.voice pitch)
      ] [ text (pitchToString pitch) ]

pitchLane : Params -> Int -> Svg msg
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
      ] []


dividers : Params -> Svg msg
dividers params = 
  let
    dividerNumbers = List.range 1 beatCount
  in
    g [color dividerColor] (List.map (divider params) dividerNumbers)


divider : Params -> Int -> Svg msg
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
