module LineChart.Types exposing (..)

import TransparentColor exposing (TransparentColor)

type alias ChartPoint = { x : Float, y : Float }

type alias Box =
  { w : Float
  , h : Float
  , x : Float
  , y : Float
  }

type alias TextOptions =
  { color : TransparentColor
  , size : Int
  , rotation : Maybe Int
  }

type alias Dimensions =
  { width : Int
  , height : Int
  }

toPair : Dimensions -> (Int, Int)
toPair d = (d.width, d.height)

setWidth : Int -> Dimensions -> Dimensions
setWidth w dimensions = { dimensions | width = w }

setHeight : Int -> Dimensions -> Dimensions
setHeight h dimensions = { dimensions | height = h }
