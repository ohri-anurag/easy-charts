module LineChart.Lines exposing (line, plotLine)

import LineChart.Types exposing (ChartPoint)
import List exposing (intersperse, map)
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

type alias PlotlineOptions =
  { points : List ChartPoint
  , color : TransparentColor
  }

plotLine : PlotlineOptions -> Svg.Svg msg
plotLine options =
  let
    pointsString = map (\cp -> String.fromFloat cp.x ++ "," ++ String.fromFloat cp.y) options.points
      |> intersperse " "
      |> String.concat
  in
    Svg.polyline
      [ SA.points pointsString
      , SA.fill "none"
      , options.color |> toRGBAString >> SA.stroke
      , SA.shapeRendering "geometricPrecision"
      ] []
