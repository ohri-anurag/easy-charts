module LineChart.Lines exposing (..)

import String exposing (fromFloat)
import Svg
import Svg.Attributes as SA
import TransparentColor exposing (TransparentColor, toRGBAString)

type alias LineOptions =
  { x1 : Float
  , y1 : Float
  , x2 : Float
  , y2 : Float
  , stroke : TransparentColor
  }

line : LineOptions -> Svg.Svg msg
line options =
  Svg.line
    [ SA.x1 <| fromFloat options.x1
    , SA.y1 <| fromFloat options.y1
    , SA.x2 <| fromFloat options.x2
    , SA.y2 <| fromFloat options.y2
    , SA.strokeWidth "1"
    , SA.shapeRendering "crispEdges"
    , SA.stroke <| toRGBAString options.stroke
    ]
    []