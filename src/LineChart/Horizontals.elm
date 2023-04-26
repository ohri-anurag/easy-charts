module LineChart.Horizontals exposing (..)

import LineChart.Labels exposing (createLabel)
import LineChart.Lines exposing (line)
import List exposing (..)
import Svg exposing (Svg)
import TransparentColor exposing (TransparentColor)

type alias HorizontalGridOptions =
  { begin : Float
  , step : Float
  , labels : List String
  , xBounds : (Float, Float)
  , colors : (TransparentColor, TransparentColor)
  , textOptions :
    { color : TransparentColor
    , size : Int
    }
  }

type alias HLineOptions =
  { y : Float
  , x1 : Float
  , x2 : Float
  , stroke : TransparentColor
  }

grid : HorizontalGridOptions -> List (Svg msg)
grid options =
  let
    (light, dark) = options.colors
    (x1, x2) = options.xBounds
    yth i = options.begin + toFloat i * options.step
    x3 = x1 - 10
    x4 = x1 - 15
  in
  indexedMap (\i label ->
    [ line
      { x1 = x1
      , y1 = yth i
      , x2 = x2
      , y2 = yth i
      , stroke = if i == 0 then dark else light
      }
    , line
      { x1 = x3
      , y1 = yth i
      , x2 = x1
      , y2 = yth i
      , stroke = light
      }
    , createLabel
      { x = x4
      , y = yth i
      , label = label
      , color = options.textOptions.color
      , size = options.textOptions.size
      , rotation = Nothing
      }
    ]
    ) options.labels |> concat