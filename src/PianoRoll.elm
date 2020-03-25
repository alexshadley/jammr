module PianoRoll exposing (pianoRoll)

import Element

import Canvas exposing (..)
import Canvas.Settings exposing (..)

import Track exposing (..)

pianoRoll : Track -> Element.Element
pianoRoll