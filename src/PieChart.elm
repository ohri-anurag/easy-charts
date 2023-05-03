module PieChart exposing (..)

{-|
This module is used for creating Pie/Doughnut Charts.
-}

import Html
import Html.Attributes as HA
import Html.Events exposing (onMouseOver, onMouseOut)
import LineChart.Types exposing (ChartPoint)
import List exposing (..)
import String exposing (fromFloat, fromInt)
import Svg
import Svg.Attributes as SA
import TransparentColor exposing (TransparentColor, toRGBAString)
import TransparentColor exposing (addLightness)

type PieModel =
  PieModelC PieModelR

type alias PieModelR =
  { hovered : Maybe Sector
  }

initPieModel : PieModel
initPieModel = PieModelC { hovered = Nothing }

type PieMsg
  = Focus Sector
  | Blur

updatePieModel : PieMsg -> PieModel -> PieModel
updatePieModel msg (PieModelC model) =
  case msg of
    Focus arc -> PieModelC { model | hovered = Just arc }
    Blur -> PieModelC { model | hovered = Nothing }

type PieChartOptions
  = PieChartOptionsC PieChartOptionsR

type alias PieChartOptionsR =
  { width : Int
  , height : Int
  }


type PieChartData
 = PieChartDataC (List PieChartDatum)

type alias PieChartDatum =
  { color : TransparentColor
  , value : Float
  , label : String
  }

toPairs : List a -> List (a, a)
toPairs xs =
  case xs of
    [] -> []
    (_ :: rest) -> map2 Tuple.pair xs rest

type alias Sector =
  { from : ChartCoordinate
  , to : ChartCoordinate
  , datum : PieChartDatum
  }

type alias TooltipOptions =
  { width : Int
  , height : Int
  , label : String
  , point : ChartCoordinate
  , value : Float
  , percentage : Float
  }

tooltip : TooltipOptions -> Svg.Svg msg
tooltip options =
  let
    (Chart {x, y}) = options.point
    tw = toFloat options.width
    th = toFloat options.height
  in
  Svg.g
    [ SA.fill "white"
    , HA.style "font-size" "14px"
    , SA.dominantBaseline "middle"
    , SA.textAnchor "middle"
    ]
    [ Svg.rect
      [ tw |> fromFloat >> SA.width
      , th |> fromFloat >> SA.height
      , SA.rx "10"
      , SA.fill "rgba(0,0,0,0.7)"
      , x - tw/2 |> fromFloat >> SA.x
      , y - th/2 |> fromFloat >> SA.y
      ]
      []
    , Svg.text_
      [ x |> fromFloat >> SA.x
      , y + th/4 |> fromFloat >> SA.y
      , HA.style "fontWeight" "bold"
      ]
      [ fromFloat options.percentage ++ "%" |> Svg.text
      ]
    , Svg.text_
      [ x |> fromFloat >> SA.x
      , y - th/4 |> fromFloat >> SA.y
      ]
      [ Svg.text <| options.label ++ ": " ++ fromFloat options.value
      ]
    ]

pieChart : PieChartOptions -> PieChartData -> (PieMsg -> msg) -> PieModel -> Html.Html msg
pieChart (PieChartOptionsC options) (PieChartDataC data) toMsg (PieModelC model) =
  let
    tw = 100
    th = 50

    w = options.width - 20 - 2*tw |> toFloat
    h = options.height - 20 - 2*th |> toFloat

    minDimension = if w < h then w else h
    r = minDimension / 2

    dx = toFloat tw + 10 + (w - 2*r) / 2
    dy = toFloat th + 10 + (h - 2*r) / 2

    origin = Chart { x = dx + r, y = dy + r }
    cx = dx + r
    cy = dy + r

    sortedData = sortBy .value data
    total = sum <| map .value data

    sectors : List Sector
    sectors = .sectorList <| foldl (\datum {accumAngle, sectorList} ->
        let
          newAccumAngle = accumAngle - (datum.value * 2 * pi / total)
          start = polarToCartesian (r, accumAngle)
          to = polarToCartesian (r, newAccumAngle)
          newSector =
            { from = toChartCoordinates origin start
            , to = toChartCoordinates origin to
            , datum = datum
            }
        in
        { accumAngle = newAccumAngle
        , sectorList = newSector :: sectorList
        }
      ) { accumAngle = pi/2, sectorList = [] } sortedData

    moveToCentre = "M " ++ fromFloat cx ++ "," ++ fromFloat cy
    lineToPoint (Chart p) = "L " ++ fromFloat p.x ++ "," ++ fromFloat p.y
    arc (Chart p) angle = intersperse " " [ "A ", fromFloat r, fromFloat r, fromFloat angle, "0", "1", fromFloat p.x, fromFloat p.y ] |> String.concat
    lineToCentre = "L " ++ fromFloat cx ++ "," ++ fromFloat cy

    drawSector : Sector -> Svg.Svg msg
    drawSector sector =
      Svg.path
        [ [moveToCentre, lineToPoint sector.from, arc sector.to (360 * sector.datum.value / total), lineToCentre] |> intersperse " " >> String.concat >> SA.d
        , SA.stroke "white"
        , SA.strokeWidth "2"
        , SA.shapeRendering "geometricPrecision"
        , toRGBAString >> SA.fill <|
          case model.hovered of
            Nothing -> sector.datum.color
            Just s -> if s == sector then addLightness -10 sector.datum.color else sector.datum.color
        , onMouseOver
            <| toMsg
            <| Focus sector
        , onMouseOut <| toMsg Blur
        ]
        []

    hoveredSector sector =
      let
        (Cartesian p1) =
          sector.from
          |> toCartesianCoordinate origin
        (Cartesian p2) =
          sector.to
          |> toCartesianCoordinate origin
        midPoint = { x = (p1.x + p2.x) / 2, y = (p1.y + p2.y) / 2}
        theta = getAngle (Cartesian midPoint)
        r2 = r + 5
        eqAngle a1 a2 = abs (a1 - a2) <= 0.0175
        p =
          if eqAngle theta 0
            then toCartesian (r2, 0)
                 |> translate (tw/2, 0)
          else if theta < pi/2
            then polarToCartesian (r2, theta)
                 |> translate (tw/2, th/2)
          else if eqAngle theta (pi/2)
            then toCartesian (0, r2)
                 |> translate (0, th/2)
          else if theta < pi
            then polarToCartesian (r2, theta)
                 |> translate (-tw/2, th/2)
          else if eqAngle theta pi
            then toCartesian (-r2, 0)
                 |> translate (-tw/2, 0)
          else if theta < 3/2*pi
            then polarToCartesian (r2, theta)
                 |> translate (-tw/2, -th/2)
          else if eqAngle theta (3*pi/2)
            then toCartesian (0, -r2)
                 |> translate (0, -th/2)
          else polarToCartesian (r2, theta)
               |> translate (tw/2, -th/2)
      in
        toChartCoordinates origin p

    toTooltipOptions : Sector -> TooltipOptions
    toTooltipOptions sector =
      { width = tw
      , height = th
      , label = sector.datum.label
      , point = hoveredSector sector
      , value = sector.datum.value
      , percentage = sector.datum.value * 100 / total
      }
  in
  Html.div
  []
  [ Svg.svg
    [ options.width |> fromInt >> SA.width
    , options.height |> fromInt >> SA.height
    , HA.style "font-family" "sans-serif"
    ]
    <| map drawSector sectors ++ filterMap (Maybe.map (toTooltipOptions >> tooltip)) [ model.hovered ]
  ]

type alias Point = { x : Float, y : Float }

type ChartCoordinate
  = Chart Point

type CartesianCoordinate
  = Cartesian Point

toCartesian : (Float, Float) -> CartesianCoordinate
toCartesian (x, y) = Cartesian { x = x, y = y }

-- Convert a Cartesian point to a Chart point for the screen
toChartCoordinates : ChartCoordinate -> CartesianCoordinate -> ChartCoordinate
toChartCoordinates (Chart origin) (Cartesian point) =
  let
    -- chart origin x + cartesian point x = chart point x
    x = origin.x + point.x
    -- chart origin y - cartesian point y = chart point y
    y = origin.y - point.y
  in
  Chart { x = x, y = y }

toCartesianCoordinate : ChartCoordinate -> ChartCoordinate ->  CartesianCoordinate
toCartesianCoordinate (Chart origin) (Chart point) =
  let
    -- cartesian point x = chart point x - chart origin x
    x = point.x - origin.x
    -- chart origin y - chart point y = cartesian point y
    y = origin.y - point.y
  in
  Cartesian { x = x, y = y }

polarToCartesian : (Float, Float) -> CartesianCoordinate
polarToCartesian (r, theta) = Cartesian
  { x = r * cos theta
  , y = r * sin theta
  }

translate : (Float, Float) -> CartesianCoordinate -> CartesianCoordinate
translate (dx, dy) (Cartesian {x, y}) = Cartesian { x = x + dx, y = y + dy }

getAngle : CartesianCoordinate -> Float
getAngle (Cartesian {x, y}) =
  let
    angle = atan (y / x)
  in
    if x >=0
      then if y >= 0 then angle else 2*pi + angle
      else pi + angle
