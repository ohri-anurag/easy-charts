module LineChart.Legend exposing (..)

import List exposing (..)
import LineChart.Labels exposing (createLabel)
import LineChart.Types exposing (Box)
import Palette.X11 exposing (black)
import Svg
import Svg.Attributes as SA
import TransparentColor exposing (TransparentColor, fromColor, opaque, toRGBAString)
import String exposing (fromFloat)

type alias LegendOptions =
  { data : List (TransparentColor, String)
  , width : Int
  , spacing : Int
  }

-- For now, assume that the legend will fit inside the given width
legends : Box -> LegendOptions -> List (Svg.Svg msg)
legends box options =
  let
    len = length options.data
    dx = options.spacing + options.width |> toFloat
    basex = box.x + (box.w - toFloat (len * options.width) - toFloat ((len - 1) * options.spacing)) / 2
    xth i = basex + dx * toFloat i
    y = box.y + box.h / 2
  in
  indexedMap (\i (color, label) ->
    [ Svg.rect
      [ xth i |> fromFloat >> SA.x
      , y - 5 |> fromFloat >> SA.y
      , SA.height "10"
      , SA.width "10"
      , SA.fill <| toRGBAString color
      ]
      []
    , createLabel
      { x = xth i + 15
      , y = y
      , label = label
      , textOptions =
        { color = fromColor opaque black
        , size = 12
        , rotation = Nothing
        }
      , anchor = "start"
      }
    ]
  ) options.data |> concat
  