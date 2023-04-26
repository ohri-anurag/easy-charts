module LineChart.Labels exposing (..)

import Html.Attributes as HA
import String exposing (fromFloat, fromInt)
import Svg exposing (Svg, text, text_)
import Svg.Attributes as SA
import TransparentColor exposing (TransparentColor, toRGBAString)

type alias LabelOptions =
  { x : Float
  , y : Float
  , label : String
  , color : TransparentColor
  , size : Int
  , rotation : Maybe Int
  }

createLabel : LabelOptions -> Svg msg
createLabel options =
  let
    xstr = fromFloat options.x
    ystr = fromFloat options.y
  in
  text_
    ( [ SA.x xstr
      , SA.y ystr
      , SA.dominantBaseline "middle"
      , SA.textAnchor "end"
      , HA.style "fill" <| toRGBAString options.color
      , HA.style "font-size" <| fromInt options.size ++ "px"
      ] ++ Maybe.withDefault [] (Maybe.map (\r -> [SA.transform ("rotate(" ++ fromInt r ++ ", " ++ xstr ++ ", " ++ ystr ++ ")")]) options.rotation)
    )
    [ text options.label ]