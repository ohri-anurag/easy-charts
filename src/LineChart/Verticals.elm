module LineChart.Verticals exposing (..)

import LineChart.Labels exposing (createLabel)
import LineChart.Lines exposing (vline)
import LineChart.Types exposing (TextOptions)
import List exposing (..)
import Svg exposing (Svg)
import TransparentColor exposing (TransparentColor)

type alias VerticalGridOptions =
  { begin : Float
  , step : Float
  , labels : List String
  , yBounds : (Float, Float)
  , color : TransparentColor
  , textOptions : TextOptions
  }

grid : VerticalGridOptions -> List (Svg msg)
grid options =
  let
    (y1, y2) = options.yBounds
    xth i = options.begin + toFloat i * options.step
    y3 = y2 + 10
    y4 = y2 + 20
  in
  indexedMap (\i label ->
    [ vline
      { x = xth i
      , y1 = y1
      , y2 = y2
      , stroke = options.color
      }
    , vline
      { x = xth i
      , y1 = y2
      , y2 = y3
      , stroke = options.color
      }
    , createLabel
      { x = xth i
      , y = y4
      , label = label
      , textOptions = options.textOptions
      , anchor = Maybe.withDefault "middle" (Maybe.map (always "end") options.textOptions.rotation)
      }
    ]
  ) options.labels |> concat
