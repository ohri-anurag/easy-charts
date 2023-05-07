module PieChart exposing
  ( PieModel
  , PieMsg
  , initPieModel
  , updatePieModel
  , PieChartDataSet
  , createDataSet
  , PieChartData
  , createData
  , pieChart
  , PieChartOptions
  , defaultPieChartOptions
  , setChartWidth
  , setChartHeight
  , setTooltipWidth
  , setTooltipHeight
  , setLegendWidth
  , setLegendSpacing
  )

{-|
This module is used for creating Pie Charts.

NOTE: Go through the Setup section initially. It is required to get the tooltip functionality of this library.

# Setup (Model, Msg, init and update)
@docs PieModel, PieMsg, initPieModel, updatePieModel

# Creating data
@docs PieChartDataSet, createDataSet, PieChartData, createData

# Plotting
@docs pieChart

# Configuring
@docs PieChartOptions, defaultPieChartOptions, setChartWidth, setChartHeight, setTooltipWidth, setTooltipHeight, setLegendWidth, setLegendSpacing
-}

import Html
import Html.Attributes as HA
import Html.Events exposing (onMouseOver, onMouseOut)
import LineChart.Legend exposing (legends)
import LineChart.Lines exposing (line, ShapeRendering(..))
import LineChart.Types exposing (..)
import List exposing (..)
import Palette.X11 exposing (white)
import String exposing (fromFloat, fromInt)
import Svg
import Svg.Attributes as SA
import TransparentColor exposing (TransparentColor, toRGBAString)
import TransparentColor exposing (addLightness, fromColor, opaque)

{-|
Need this for determining if a sector on the chart has been hovered on. This should be a part of your model.

    type Model =
      { otherData : OtherData
      , pieModel : PieModel
      }
-}
type PieModel =
  PieModelC PieModelR

type alias PieModelR =
  { hovered : Maybe Sector
  }

{-|
Initial state for PieModel type. Should be a part of your init.

    init : Model
    init =
      { otherData = {...}
      , pieModel = initPieModel
      }

-}
initPieModel : PieModel
initPieModel = PieModelC { hovered = Nothing }

{-|
Used to handle tooltips for the pie chart. This should be added to your own msg type.

    type Msg
      = Msg1 Data1
      | Msg2 Data2
      | PieMessage PieMsg
-}
type PieMsg
  = Focus Sector
  | Blur

{-|
Used to update the PieModel type. This should be used inside your update function.

    update : Msg -> Model -> Model
    update msg model =
      case msg of
        PieMessage pieMsg ->
          { model
          | pieModel = updatePieModel pieMsg model.pieModel
          }
        NoOp -> model
-}
updatePieModel : PieMsg -> PieModel -> PieModel
updatePieModel msg (PieModelC model) =
  case msg of
    Focus arc -> PieModelC { model | hovered = Just arc }
    Blur -> PieModelC { model | hovered = Nothing }

{-|
Used to configure the pie chart.
-}
type PieChartOptions
  = PieChartOptionsC PieChartOptionsR

type alias PieChartOptionsR =
  { chartDimensions : Dimensions
  , tooltipDimensions : Dimensions
  , legendWidth : Int
  , legendSpacing : Int
  }

{-|
Default options for configuring the line chart.

    options : PieChartOptions
    options =
      setChartWidth 1200 defaultPieChartOptions
      |> setChartHeight 800
      |> setTooltipWidth 200
      |> setTooltipHeight 100
-}
defaultPieChartOptions : PieChartOptions
defaultPieChartOptions = PieChartOptionsC
  { chartDimensions =
    { width = 600
    , height = 600
    }
  , tooltipDimensions =
    { width = 120
    , height = 50
    }
  , legendWidth = 100
  , legendSpacing = 50
  }

{-|
Set the Chart Width.
-}
setChartWidth : Int -> PieChartOptions -> PieChartOptions
setChartWidth cw (PieChartOptionsC options) = PieChartOptionsC
  { options | chartDimensions = setWidth cw options.chartDimensions }

{-|
Set the Chart Height.
-}
setChartHeight : Int -> PieChartOptions -> PieChartOptions
setChartHeight ch (PieChartOptionsC options) = PieChartOptionsC
  { options | chartDimensions = setHeight ch options.chartDimensions }

{-|
Set the Tooltip Width.
-}
setTooltipWidth : Int -> PieChartOptions -> PieChartOptions
setTooltipWidth tw (PieChartOptionsC options) = PieChartOptionsC
  { options | tooltipDimensions = setWidth tw options.tooltipDimensions }

{-|
Set the Tooltip Height.
-}
setTooltipHeight : Int -> PieChartOptions -> PieChartOptions
setTooltipHeight th (PieChartOptionsC options) = PieChartOptionsC
  { options | tooltipDimensions = setHeight th options.tooltipDimensions }

{-|
Set the width of one legend key.
-}
setLegendWidth : Int -> PieChartOptions -> PieChartOptions
setLegendWidth lw (PieChartOptionsC options) = PieChartOptionsC
  { options | legendWidth = lw }

{-|
Set the spacing between two legend keys.
-}
setLegendSpacing : Int -> PieChartOptions -> PieChartOptions
setLegendSpacing ls (PieChartOptionsC options) = PieChartOptionsC
  { options | legendSpacing = ls }

{-|
Represents the data that will be used to draw the Pie Chart.
-}
type PieChartData
 = PieChartDataC (List PieChartDataSet)

{-|
Takes a List of data sets. Each data set represents a sector on the Pie Chart.

    import Palette.X11 exposing (green, red)
    import TransparentColor exposing (opaque, fromColor)

    data :: PieChartData
    data = createData
      [ createDataSet "Delivery Time" 30 (fromColor opaque green)
      , createDataSet "Manufacturing Time" 180 (fromColor opaque red)
      ]
-}
createData : List PieChartDataSet -> PieChartData
createData = PieChartDataC

{-|
Represents one sector of the Pie Chart.
-}
type PieChartDataSet =
  PieChartDataSetC PieChartDataSetR

type alias PieChartDataSetR =
  { color : TransparentColor
  , value : Float
  , label : String
  }

{-|
Create a PieChartDataSet by providing:
1. A label for the dataset
2. A value for the dataset
3. A color which will be used to plot this dataset on the chart. [Click here for the color module](https://package.elm-lang.org/packages/tesk9/palette/latest/TransparentColor).

One dataset corresponds to one sector on the chart.

    import Palette.X11 exposing (black)
    import TransparentColor exposing (opaque, fromColor)

    serverCost :: PieChartDataSet
    serverCost = createDataSet "Server Cost" 20.4 (fromColor opaque black)
-}
createDataSet : String -> Float -> TransparentColor -> PieChartDataSet
createDataSet label height color = PieChartDataSetC
  { label = label
  , value = height
  , color = color
  }

type alias Sector =
  { datum : PieChartDataSetR
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

{-|
Used to draw the pie chart. The arguments are:
1. Options to configure the pie chart.
2. The data to be displayed on the chart.
3. A msg constructor to convert PieMsg into your own msg type.
4. The pie chart model, which should be stored inside your Elm model.

Following code shows how to setup parts 3 and 4.

    import PieChart exposing (pieChart)

    type Model =
      { otherData : OtherData
      , pieModel : PieModel
      }

    type Msg
      = Msg1 Data1
      | Msg2 Data2
      | PieMessage PieMsg

    init : Model
    init =
      { otherData = {...}
      , pieModel = initPieModel
      }

    view : Model -> Html Msg
    view model = pieChart defaultPieChartOptions data PieMessage model.pieModel

    update : Msg -> Model -> Model
    update msg model =
      case msg of
        PieMessage pieMsg ->
          { model
          | pieModel = updatePieModel pieMsg model.pieModel
          }
        NoOp -> model
-}
pieChart : PieChartOptions -> PieChartData -> (PieMsg -> msg) -> PieModel -> Html.Html msg
pieChart (PieChartOptionsC options) (PieChartDataC data) toMsg (PieModelC model) =
  let
    (cw, ch) = toPair options.chartDimensions
    (tw, th) = toPair options.tooltipDimensions

    w = cw - 20 - 2*tw |> toFloat
    h = 0.95 * toFloat ch - 20 - 2 * toFloat th

    minDimension = if w < h then w else h
    r = minDimension / 2

    dx = toFloat tw + 10 + (w - 2*r) / 2
    dy = toFloat th + 10 + (h - 2*r) / 2

    origin = Chart { x = dx + r, y = dy + r }
    cx = dx + r
    cy = dy + r

    unwrappedData = map (\(PieChartDataSetC d) -> d) data
    sortedData = sortBy .value unwrappedData
    total = sum <| map .value unwrappedData

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
              translate (toFloat tw/2 * cos theta, toFloat th/2 * sin theta)
            else
              translate (toFloat tw/2 * toFloat (signum midPoint.x), toFloat th/2 * toFloat (signum midPoint.y))
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

    legendBox =
      { w = toFloat cw
      , h = 0.05 * toFloat ch
      , x = 0
      , y = 0
      }

    legendOptions =
      { data = map (\d -> (d.color, d.label)) unwrappedData
      , width = 50
      , spacing = 50
      }
  in
  Svg.svg
    [ cw |> fromInt >> SA.width
    , ch |> fromInt >> SA.height
    , HA.style "font-family" "sans-serif"
    ]
    <| legends legendBox legendOptions
        ++ map drawSector sectors
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
