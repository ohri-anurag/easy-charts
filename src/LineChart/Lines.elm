module LineChart.Lines exposing (ShapeRendering(..), line, hline, plotLine, vline)

import LineChart.Types exposing (ChartPoint)
import List exposing (intersperse, map)
import String exposing (fromFloat, fromInt)
import Svg
import Svg.Attributes as SA
import TransparentColor exposing (TransparentColor, toRGBAString)

type ShapeRendering
  = CrispEdges
  | GeometricPrecision

show : ShapeRendering -> String
show s =
  case s of
    CrispEdges -> "crispEdges"
    GeometricPrecision -> "geometricPrecision"

type alias LineOptions =
  { x1 : Float
  , y1 : Float
  , x2 : Float
  , y2 : Float
  , stroke : TransparentColor
  , strokeWidth : Int
  , shapeRendering : ShapeRendering
  }

line : LineOptions -> Svg.Svg msg
line {x1, y1, x2, y2, stroke, strokeWidth, shapeRendering} =
  Svg.line
    [ SA.x1 <| fromFloat x1
    , SA.y1 <| fromFloat y1
    , SA.x2 <| fromFloat x2
    , SA.y2 <| fromFloat y2
    , strokeWidth |> fromInt >> SA.strokeWidth
    , shapeRendering |> show >> SA.shapeRendering
    , SA.stroke <| toRGBAString stroke
    ]
    []

type alias HLineOptions =
  { y : Float
  , x1 : Float
  , x2 : Float
  , stroke : TransparentColor
  }

hline : HLineOptions -> Svg.Svg msg
hline {y, x1, x2, stroke} =
  line
    { x1 = x1
    , y1 = y
    , x2 = x2
    , y2 = y
    , stroke = stroke
    , strokeWidth = 1
    , shapeRendering = CrispEdges
    }

type alias VLineOptions =
  { x : Float
  , y1 : Float
  , y2 : Float
  , stroke : TransparentColor
  }

vline : VLineOptions -> Svg.Svg msg
vline {x, y1, y2, stroke} =
  line
    { x1 = x
    , y1 = y1
    , x2 = x
    , y2 = y2
    , stroke = stroke
    , strokeWidth = 1
    , shapeRendering = CrispEdges
    }

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
