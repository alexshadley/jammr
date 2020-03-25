module PianoRoll exposing (pianoRoll)

import Html.Attributes exposing (onClick)

import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color
import Element


import Track exposing (..)

pianoRoll : Track -> Element.Element msg
pianoRoll track =
  Element.html <|
    Canvas.toHtml (1000, 800) 
      [ onClick ]
      [ Canvas.shapes [fill Color.black] [ Canvas.rect (500, 500) 50 50] ]