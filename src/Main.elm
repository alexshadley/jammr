port module Main exposing (..)

import Browser
import Process
import Task
import Html exposing (Html)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input


import Track exposing (..)



---- MODEL ----


type alias Model =
  { track : Track }


init : ( Model, Cmd Msg )
init =
    ( { track = Track.empty } , Cmd.none )



---- UPDATE ----


type Msg
    = PlayTrack
    | PlayNote Float
    | Stop
    | AddNote Note
    | RemoveNote Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    PlayTrack ->
      ( model, playTrack PlayNote Stop 120 model.track )

    PlayNote frq ->
      ( model, play frq )

    Stop ->
      ( model, stop () )
          
    AddNote note ->
      ( {model | track = Track.addNote note model.track}, Cmd.none )

    RemoveNote id ->
      ( {model | track = Track.removeNote id model.track}, Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =
  let
    pitches =
      List.range 48 60 |> List.reverse
  in
  layout [] <| column [ centerX, padding 50, spacing 50 ]
    [ Input.button [onClick PlayTrack, centerX] {onPress = Just PlayTrack, label = text "Play Track"}
    ,  column [width (px 1000)] (List.map noteRow pitches)
    ]


noteRow : Int -> Element Msg
noteRow note =
  row [height (px 50), width fill]
    [ noteLabel note
    , noteLane note
    ]

noteLabel : Int -> Element Msg
noteLabel note =
  column [width (px 50)] [el [alignRight] (text (pitchToString note))]

noteLane : Int -> Element Msg
noteLane note =
  let
    bgColor = case (modBy 2 note) == 0 of
      True -> rgb255 220 230 245
      False -> rgb255 230 230 230
    
    noteCells =
      List.range 0 15
        |> List.map (noteCell note)
        |> List.intersperse noteSpacer
  in
    row [height fill, width fill, Background.color bgColor] noteCells

noteCell : Int -> Int -> Element Msg
noteCell note beat =
  column 
    [ onClick (AddNote {pitch = note, start = toFloat beat, duration = 1.0})
    , width (px 60)
    , height fill
    ] []

noteSpacer : Element Msg
noteSpacer =
  column [width (px 2), height fill, Background.color <| rgb255 100 100 100] []


---- PROGRAM ----


port play : Float -> Cmd msg


port stop : () -> Cmd msg


main : Program () Model Msg
main =
  Browser.element
    { view = view
    , init = \_ -> init
    , update = update
    , subscriptions = always Sub.none
    }
