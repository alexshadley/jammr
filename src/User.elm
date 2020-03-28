module User exposing (User, getColor)

import Element exposing (Color, rgb255)

type alias User =
  { name : String
  , color : (Int, Int, Int)
  }

getColor : User -> Color
getColor user =
  case user.color of
    (r, g, b) -> rgb255 r g b