module LineChart.Labels exposing (..)

import Html.Attributes as HA
import LineChart.Types exposing (TextOptions)
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg, text, text_)
import Svg.Attributes as SA
import TransparentColor exposing (toRGBAString)

type alias LabelOptions =
  { x : Float
  , y : Float
  , label : String
  , textOptions : TextOptions
  , anchor : String
  }

createLabel : LabelOptions -> Svg msg
createLabel options =
  let
    xstr = fromFloat options.x
    ystr = fromFloat options.y
    txtOpt = options.textOptions
  in
  text_
    ( [ SA.x xstr
      , SA.y ystr
      , SA.dominantBaseline "middle"
      , SA.textAnchor options.anchor
      , HA.style "fill" <| toRGBAString txtOpt.color
      , HA.style "font-size" <| fromInt txtOpt.size ++ "px"
      ] ++ Maybe.withDefault [] (Maybe.map (\r -> [SA.transform ("rotate(" ++ fromInt r ++ ", " ++ xstr ++ ", " ++ ystr ++ ")")]) txtOpt.rotation)
    )
    [ text options.label ]