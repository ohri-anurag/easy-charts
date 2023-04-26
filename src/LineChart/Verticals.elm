module LineChart.Verticals exposing (..)

import LineChart.Labels exposing (createLabel)
import LineChart.Lines exposing (line)
import List exposing (..)
import Svg exposing (Svg)
import TransparentColor exposing (TransparentColor)

type alias VerticalGridOptions =
  { begin : Float
  , step : Float
  , labels : List String
  , yBounds : (Float, Float)
  , colors : (TransparentColor, TransparentColor)
  , textOptions :
    { color : TransparentColor
    , size : Int
    , rotation : Int
    }
  }

type alias VLineOptions =
  { x : Float
  , y1 : Float
  , y2 : Float
  , stroke : TransparentColor
  }

grid : VerticalGridOptions -> List (Svg msg)
grid options =
  let
    (light, dark) = options.colors
    (y1, y2) = options.yBounds
    xth i = options.begin + toFloat i * options.step
    y3 = y2 + 10
    y4 = y2 + 15
  in
  indexedMap (\i label ->
    [ line
      { x1 = xth i
      , y1 = y1
      , x2 = xth i
      , y2 = y2
      , stroke = if i == 0 then dark else light
      }
    , line
      { x1 = xth i
      , y1 = y2
      , x2 = xth i
      , y2 = y3
      , stroke = light
      }
    , createLabel
      { x = xth i
      , y = y4
      , label = label
      , color = options.textOptions.color
      , size = options.textOptions.size
      , rotation = Just options.textOptions.rotation
      }
    ]
  ) options.labels |> concat
