module LineChart.Horizontals exposing (..)

import LineChart.Labels exposing (createLabel)
import LineChart.Lines exposing (hline)
import List exposing (..)
import Svg exposing (Svg)
import TransparentColor exposing (TransparentColor)
import LineChart.Types exposing (TextOptions)

type alias HorizontalGridOptions =
  { begin : Float
  , step : Float
  , labels : List String
  , xBounds : (Float, Float)
  , color : TransparentColor
  , textOptions : TextOptions
  }

grid : HorizontalGridOptions -> List (Svg msg)
grid options =
  let
    (x1, x2) = options.xBounds
    yth i = options.begin + toFloat i * options.step
    x3 = x1 - 10
    x4 = x1 - 15
  in
  indexedMap (\i label ->
    [ hline
      { x1 = x1
      , x2 = x2
      , y = yth i
      , stroke = options.color
      }
    , hline
      { x1 = x3
      , x2 = x1
      , y = yth i
      , stroke = options.color
      }
    , createLabel
      { x = x4
      , y = yth i
      , label = label
      , textOptions = options.textOptions
      , anchor = "end"
      }
    ]
    ) options.labels |> concat