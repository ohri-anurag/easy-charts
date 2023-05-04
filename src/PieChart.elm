module PieChart exposing (..)

{-|
This module is used for creating Pie/Doughnut Charts.
-}

import Html
import Html.Attributes as HA
import Html.Events exposing (onMouseOver, onMouseOut)
import LineChart.Lines exposing (line, ShapeRendering(..))
import List exposing (..)
import Palette.X11 exposing (white)
import String exposing (fromFloat, fromInt)
import Svg
import Svg.Attributes as SA
import TransparentColor exposing (TransparentColor, toRGBAString)
import TransparentColor exposing (addLightness, fromColor, opaque)

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
  { datum : PieChartDatum
  , beginAngle : Float
  , endAngle : Float
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
    sectors = reverse <| .sectorList <| foldl (\datum {accumAngle, sectorList} ->
        let
          newAccumAngle = accumAngle - (datum.value * 2 * pi / total)
          newSector =
            { datum = datum
            , beginAngle = accumAngle
            , endAngle = newAccumAngle
            }
        in
        { accumAngle = newAccumAngle
        , sectorList = newSector :: sectorList
        }
      ) { accumAngle = pi/2, sectorList = [] } sortedData

    moveTo (Chart p) = "M " ++ fromFloat p.x ++ "," ++ fromFloat p.y
    moveToCentre = moveTo origin
    lineTo (Chart p) = "L " ++ fromFloat p.x ++ "," ++ fromFloat p.y
    arc (Chart p) angle direction = intersperse " " [ "A", fromFloat r, fromFloat r, fromFloat angle, "0", fromInt direction, fromFloat p.x, fromFloat p.y ] |> String.concat
    lineToCentre = "L " ++ fromFloat cx ++ "," ++ fromFloat cy

    fromExtended sector d =
      polarToCartesian (r + d, sector.beginAngle)
      |> toChartCoordinates origin
    toExtended sector d = 
      polarToCartesian (r + d, sector.endAngle)
      |> toChartCoordinates origin

    drawSector : Sector -> Svg.Svg msg
    drawSector sector =
      let
        angle = 360 * sector.datum.value / total
        color = toRGBAString <|
          case model.hovered of
            Nothing -> sector.datum.color
            Just s -> if s == sector then addLightness -10 sector.datum.color else sector.datum.color
      in
        Svg.path
          [ [ moveToCentre
            , lineTo <| fromExtended sector 0
            , arc (toExtended sector 0) angle 1
            , lineToCentre
            ]
            |> intersperse " " >> String.concat >> SA.d
          , SA.stroke color
          , SA.strokeWidth "0"
          , SA.shapeRendering "geometricPrecision"
          , SA.fill color
          , onMouseOver
              <| toMsg
              <| Focus sector
          , onMouseOut <| toMsg Blur
          ]
          []

    drawBoundary : Sector -> Svg.Svg msg
    drawBoundary sector =
      let
        (Chart {x, y}) = fromExtended sector 2
      in
      line
        { x1 = cx
        , y1 = cy
        , x2 = x
        , y2 = y
        , stroke = fromColor opaque white
        , strokeWidth = 2
        , shapeRendering = GeometricPrecision
        }

    tooltipPoint sector =
      let
        (Cartesian p1) =
          fromExtended sector 0
          |> toCartesianCoordinate origin
        (Cartesian p2) =
          toExtended sector 0
          |> toCartesianCoordinate origin
        midPoint = { x = (p1.x + p2.x) / 2, y = (p1.y + p2.y) / 2}
        theta = getAngle (Cartesian midPoint)
        r2 = r + 8
        eqAngle a1 a2 = abs (a1 - a2) <= 2*0.0175
        p = polarToCartesian (r2, theta) |>
          if eqAngle theta 0 || eqAngle theta (pi/2) || eqAngle theta pi || eqAngle theta (3*pi/2)
            then
              translate (tw/2 * cos theta, th/2 * sin theta)
            else
              translate (tw/2 * toFloat (signum midPoint.x), th/2 * toFloat (signum midPoint.y))
      in
        toChartCoordinates origin p

    toTooltipOptions : Sector -> TooltipOptions
    toTooltipOptions sector =
      { width = tw
      , height = th
      , label = sector.datum.label
      , point = tooltipPoint sector
      , value = sector.datum.value
      , percentage = sector.datum.value * 100 / total
      }
  
    hoveredSector =
      case model.hovered of
        Nothing -> []
        Just sector ->
          let
            angle = 360 * sector.datum.value / total
            outerPath =
              let
                color = addLightness -10 sector.datum.color |> toRGBAString
              in
              Svg.path
              [ [ moveTo <| fromExtended sector 2
                , lineTo <| fromExtended sector 7
                , arc (toExtended sector 7) angle 1
                , lineTo <| toExtended sector 2
                , arc (fromExtended sector 2) angle 0
                ]
                |> intersperse " " >> String.concat >> SA.d
              , SA.strokeWidth "2"
              , SA.shapeRendering "geometricPrecision"
              , SA.stroke "white"
              , SA.fill color
              ]
              []
          in
          [ outerPath
          , toTooltipOptions sector |> tooltip
          ]
  in
  Svg.svg
    [ options.width |> fromInt >> SA.width
    , options.height |> fromInt >> SA.height
    , HA.style "font-family" "sans-serif"
    ]
    <| map drawSector sectors
        ++ map drawBoundary sectors
        ++ hoveredSector

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
polarToCartesian = toCartesian << fromPolar

signum : Float -> Int
signum x =
  if x < 0
    then -1
    else if x > 0 then 1 else 0

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
