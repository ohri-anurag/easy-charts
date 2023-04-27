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
