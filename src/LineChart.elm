module LineChart exposing
  ( ChartMsg
  , ChartModel
  , LineChartOptions
  , lineChart
  , initChartModel
  , updateChartModel
  , LineChartData
  , LineChartDataSet
  , createData
  , createDataSet
  , defaultLineChartOptions
  , setChartWidth
  , setChartHeight
  , setTooltipWidth
  , setTooltipHeight
  , setLabelWidth
  , setLabelHeight
  , setPointRadius
  )

{-|
This module is for creating Line Charts.

NOTE: Go through the Setup section initially. It is required to get the tooltip functionality of this library.

# Setup (Model, Msg, init and update)
@docs ChartModel, ChartMsg, initChartModel, updateChartModel

# Creating data
@docs LineChartDataSet, createDataSet, LineChartData, createData

# Plotting
@docs lineChart

# Configuring
@docs LineChartOptions, defaultLineChartOptions, setChartWidth, setChartHeight, setTooltipWidth, setTooltipHeight, setLabelWidth, setLabelHeight, setPointRadius
-}

import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (..)
import List exposing (..)
import Svg exposing (Svg, circle, g, rect, svg, text, text_)
import Svg.Attributes as SA
import String exposing (fromFloat)
import TransparentColor exposing (..)
import LineChart.Lines as L
import LineChart.Horizontals as H
import LineChart.Types exposing (ChartPoint)
import LineChart.Verticals as V
import SolidColor exposing (fromRGB)
import String exposing (fromInt)

type alias HoveredPoint =
  { point : ChartPoint
  , label : String
  , height : Float
  , index : Int
  , dataSetLabel : String
  , dataSetColor : TransparentColor
  }

{-|
Represents one line of the Line Chart.
-}
type LineChartDataSet
  = LineChartDataSetC LineChartDataSetR

type alias LineChartDataSetR =
  { label : String
  , heights : List Float
  , color : TransparentColor
  }

{-|
Create a LineChartDataSet by providing:
1. A label for the dataset
2. A list of values
3. A color which will be used to plot this dataset on the chart. [Click here for the color module](https://package.elm-lang.org/packages/tesk9/palette/latest/TransparentColor).

One dataset corresponds to one line on the chart.

    import Palette.X11 exposing (black)
    import TransparentColor exposing (opaque, fromColor)

    serverCost :: LineChartDataSet
    serverCost = createDataSet "Server Cost" [ 20.4, 30.2, 10] (fromColor opaque black)
-}
createDataSet : String -> List Float -> TransparentColor -> LineChartDataSet
createDataSet label heights color = LineChartDataSetC
  { label = label
  , heights = heights
  , color = color
  }

{-|
Represents the data that will be used to draw the Line Chart. Many lines can be drawn in one chart.
-}
type LineChartData
  = LineChartDataC LineChartDataR

type alias LineChartDataR =
  { labels : List String
  , dataSets : List LineChartDataSet
  }

{-|
Takes a List of string labels, and data sets. Each data set represents a line on the Line Chart.

    import Palette.X11 exposing (green, red)
    import TransparentColor exposing (opaque, fromColor)

    data :: LineChartData
    data = createData ["Mon", "Tue", "Wed", "Thu", "Fri"]
      [ createDataSet "Wake up time" [7, 8, 9, 8, 7] (fromColor opaque green)
      , createDataSet "Sleeping time" [23, 22, 21, 22, 23] (fromColor opaque red)
      ]
-}
createData : List String -> List LineChartDataSet -> LineChartData
createData labels datasets = LineChartDataC
  { labels = labels
  , dataSets = datasets
  }

type alias LabelledDataSet =
  { label : String
  , dataSet : List (String, Float)
  , color : TransparentColor
  }
type alias LabelledData =
  { length : Int
  , labels : List String
  , labelledDataSets : List LabelledDataSet
  , max : Float
  , min : Float
  }

{-|
Used to handle tooltips for the line chart. This should be added to your own msg type.

    type Msg
      = Msg1 Data1
      | Msg2 Data2
      | ChartMessage ChartMsg
-}
type ChartMsg
  = Focus HoveredPoint
  | Blur

{-|
Need this for determining if a point on the chart has been hovered on. This should be a part of your model.

    type Model =
      { otherData : OtherData
      , chartModel : ChartModel
      }
-}
type ChartModel
  = ChartModelC ChartModelR

type alias ChartModelR = { hovered : Maybe HoveredPoint }

{-|
Initial state for ChartModel type. Should be a part of your init.

    init : Model
    init =
      { otherData = {...}
      , chartModel = initChartModel
      }

-}
initChartModel : ChartModel
initChartModel = ChartModelC { hovered = Nothing }

processInputData : LineChartData -> Maybe LabelledData
processInputData (LineChartDataC inputData) =
  let
    unwrap (LineChartDataSetC datasets) = datasets
  in
  map (unwrap >> .heights >> length) inputData.dataSets |> minimum
  |> Maybe.andThen (\minLength ->
      let
        labelledDataSets = map (\(LineChartDataSetC ds) ->
            { label = ds.label
            , dataSet = map2 Tuple.pair inputData.labels ds.heights |> take minLength
            , color = ds.color
            }
          ) inputData.dataSets
        minMaxMaybe = concatMap (.dataSet >> map Tuple.second) labelledDataSets
          |> findMinMax
      in
        Maybe.map (\(min, max) ->
          { length = minLength
          , labels = inputData.labels
          , labelledDataSets = labelledDataSets
          , min = min
          , max = max
          }
          ) minMaxMaybe
    )

findMinMax : List Float -> Maybe (Float, Float)
findMinMax list =
  case list of
    [] -> Nothing
    (a :: rest) ->
      foldl (\b (min, max) ->
        (if b < min then b else min, if b > max then b else max)
        ) (a, a) rest |> Just

type alias Box =
  { w : Float
  , h : Float
  , x : Float
  , y : Float
  }

tooltip : Float -> Float -> Float -> Int -> HoveredPoint -> Svg msg
tooltip w h cw pointRadius hp =
  let
    dr = toFloat pointRadius * 2 + 5
    basex = if hp.point.x < cw / 2
      then hp.point.x + dr
      else hp.point.x - dr - w
    basey = hp.point.y - h/2
    additionalClasses dx dy =
      [ basex + dx |> fromFloat >> SA.x
      , basey + dy |> fromFloat >> SA.y
      ]
  in
  g
    [ HA.style "fill" "white"
    , HA.style "font-size" "14px"
    , SA.dominantBaseline "middle"
    , SA.textAnchor "middle"
    ]
    [ rect
        ( [ w |> fromFloat >> SA.width
          , h |> fromFloat >> SA.height
          , SA.rx "10"
          , SA.fill "rgba(0,0,0,0.7)"
          ] ++ additionalClasses 0 0
        ) []
    , text_
        ( HA.style "font-weight" "bold"
          :: additionalClasses (w/2) (h/4)
        )
        [ text hp.label ]
    , text_
        ( SA.textAnchor "start"
          :: additionalClasses 20 (3*h/4))
        [ hp.dataSetLabel ++ ": " ++ fromFloat hp.height |> text ]
    , rect
        ( [ SA.width "10"
          , SA.height "10"
          , hp.dataSetColor |> toRGBAString >> SA.fill
          ] ++ additionalClasses 5 (3*h/4 - 5)
        )
        []
    ]

getFilteredLabels : Float -> List String -> (Int, List String)
getFilteredLabels width labels =
  let
    len = length labels
    indexedList = map2 Tuple.pair (range 0 len) labels
    isDivisibleBy n x = modBy (2 ^ n) x == 0
    helper n =
      let
        numLabels = len // (2 ^ n)
      in
        if width / toFloat numLabels < 30
          then helper (n + 1)
          else
            filter (Tuple.first >> isDivisibleBy n) indexedList
            |> \ls -> (2 ^ n, map Tuple.second ls)
  in
    if width < 30
      then (0, [])
      else helper 0

grid : Box -> List String -> (Float, Float) -> Float -> List (Svg msg)
grid box labels (l, u) step =
  let
    lightColor = fromColor opaque <| fromRGB (204, 204, 204)
    darkColor = fromColor opaque <| fromRGB (170, 170, 170)
    textColor = fromColor opaque <| fromRGB (119, 119, 119)
    numH = (u - l) / step |> round
    dy = box.h / toFloat numH
    numLabels = length labels
    dx = box.w / toFloat (numLabels - 1)
    (n, filteredLabels) = getFilteredLabels box.w labels
    hlines2 = H.grid
      { begin = box.y + box.h
      , step = -dy
      , labels = map (\i -> l + toFloat i * step |> fromFloat) (range 0 numH)
      , colors = (lightColor, darkColor)
      , xBounds = (box.x, box.x + box.w)
      , textOptions =
          { color = textColor
          , size = 12
          }
      }
    vlines2 = V.grid
      { begin = box.x
      , step = dx * toFloat n
      , labels = filteredLabels
      , colors = (lightColor, darkColor)
      , yBounds = (box.y, box.y + box.h)
      , textOptions =
          { color = textColor
          , size = 12
          , rotation = -50
          }
      }
  in
    hlines2 ++ vlines2

plotline : Box -> Int -> (Float, Float) -> (ChartMsg -> msg) -> (Int -> List (Svg.Attribute msg)) -> Int -> LabelledDataSet -> List (Svg msg)
plotline box len (l, u) toMsg hoverClasses i labelledDataSet =
  let
    dx = box.w / toFloat (len - 1)
    scale h = box.y + box.h * (u - h) / (u - l)
    points = indexedMap (\j (label, height) ->
      { point = { x = box.x + toFloat j * dx, y = scale height }
      , label = label
      , height = height
      , index = i * len + j
      , dataSetLabel = labelledDataSet.label
      , dataSetColor = labelledDataSet.color
      }) labelledDataSet.dataSet
  in
    L.plotLine
      { points = map .point points
      , color = labelledDataSet.color
      }
    ::
    map (\hp ->
      circle
        ( [ hp.point.y |> fromFloat |> SA.cy
          , hp.point.x |> fromFloat |> SA.cx
          , onMouseOver
            <| toMsg
            <| Focus hp
          , onMouseOut <| toMsg Blur
          , hp.dataSetColor |> toRGBAString >> SA.fill
          ]
          ++ hoverClasses hp.index
        )
        []
    ) points

calculateStep : (Float, Float) -> Float -> Float
calculateStep (l, u) m =
  let
    num = (u - l) / m
  in
    if num == 1
      then calculateStep (l, u) (m / 5)
      else if num < 5
      then calculateStep (l, u) (m / 2)
      else m

calculateLimits : (Float, Float) -> ((Float, Float), Float)
calculateLimits (min, max) =
  let
    range = max - min
    magnitude = 10 ^ (logBase 10 range |> floor |> toFloat)
    bounds m =
      ( m * toFloat(min / m |> floor)
      , m * toFloat(max / m |> ceiling)
      )
    (l, u) = bounds magnitude
  in
    if range / (l - u) < 0.7
      then (bounds (magnitude / 2), calculateStep (l, u) (magnitude / 2))
      else ((l, u), calculateStep (l, u) magnitude)

{-|
Used to draw the line chart. The arguments are:
1. Options to configure the line chart.
2. The data to be displayed on the chart.
3. A msg constructor to convert ChartMsg into your own msg type.
4. The chart model, which should be stored inside your Elm model.

Following code shows how to setup parts 3 and 4.

    import LineChart exposing (lineChart)

    type Model =
      { otherData : OtherData
      , chartModel : ChartModel
      }

    type Msg
      = Msg1 Data1
      | Msg2 Data2
      | ChartMessage ChartMsg

    init : Model
    init =
      { otherData = {...}
      , chartModel = initChartModel
      }

    view : Model -> Html Msg
    view model = lineChart defaultLineChartOptions data ChartMessage model.chartModel

    update : Msg -> Model -> Model
    update msg model =
      case msg of
        ChartMessage chartMsg ->
          { model
          | chartModel = updateChartModel chartMsg model.chartModel
          }
        NoOp -> model
-}
lineChart : LineChartOptions -> LineChartData -> (ChartMsg -> msg) -> ChartModel -> Html msg
lineChart (LineChartOptionsC options) data toMsg (ChartModelC model) =
  let
    hoverClass i =
      let
        r = fromInt options.pointRadius
        r2 = fromInt <| options.pointRadius * 2
      in
      case model.hovered of
        Nothing -> [SA.r r]
        Just hp -> [if i == hp.index then SA.r r2 else SA.r r]
    labelledDataMaybe = processInputData data
    (w, h) = toPair options.chartDimensions
    (tw, th) = toPair options.tooltipDimensions
    (lw, lh) = toPair options.labelDimensions
  in
  case labelledDataMaybe of
    Nothing -> svg [] []
    Just labelledData ->
      let
        (bounds, step) = calculateLimits (labelledData.min, labelledData.max)
        chartBox =
          let
            maxLabelDimension = toFloat <| if lw > lh then lw else lh
          in
          { w = 0.95 * toFloat w - maxLabelDimension
          , h = 0.95 * toFloat h - maxLabelDimension
          , x = maxLabelDimension
          , y = 0.05 * toFloat h
          }
        grids = grid chartBox labelledData.labels bounds step
        points = indexedMap (plotline chartBox labelledData.length bounds toMsg hoverClass) labelledData.labelledDataSets
          |> concat
      in
      svg [w |> String.fromInt >> SA.width, h |> String.fromInt >> SA.height, HA.style "font-family" "sans-serif"]
        <| grids ++ points ++ filterMap (Maybe.map (tooltip (toFloat tw) (toFloat th) (toFloat w) options.pointRadius)) [ model.hovered ]

{-|
Used to configure the line chart.
-}
type LineChartOptions
  = LineChartOptionsC LineChartOptionsR

type alias LineChartOptionsR =
  { chartDimensions : Dimensions
  , tooltipDimensions : Dimensions
  , labelDimensions : Dimensions
  , pointRadius : Int
  }

type alias Dimensions =
  { width : Int
  , height : Int
  }

toPair : Dimensions -> (Int, Int)
toPair d = (d.width, d.height)

setWidth : Int -> Dimensions -> Dimensions
setWidth w dimensions = { dimensions | width = w }

setHeight : Int -> Dimensions -> Dimensions
setHeight h dimensions = { dimensions | height = h }

{-|
Default options for configuring the line chart.

    options : LineChartOptions
    options =
      setChartWidth 1200 defaultLineChartOptions
      |> setChartHeight 800
      |> setTooltipWidth 200
      |> setTooltipHeight 100
      |> setLabelWidth 200
      |> setLabelHeight 200
      |> setPointRadius 4
-}
defaultLineChartOptions : LineChartOptions
defaultLineChartOptions = LineChartOptionsC
  { chartDimensions =
    { width = 600
    , height = 600
    }
  , tooltipDimensions =
    { width = 120
    , height = 50
    }
  , labelDimensions =
    { width = 100
    , height = 50
    }
  , pointRadius = 2
  }

{-|
Set the Chart Width.
-}
setChartWidth : Int -> LineChartOptions -> LineChartOptions
setChartWidth cw (LineChartOptionsC options) = LineChartOptionsC
  { options | chartDimensions = setWidth cw options.chartDimensions }

{-|
Set the Chart Height.
-}
setChartHeight : Int -> LineChartOptions -> LineChartOptions
setChartHeight ch (LineChartOptionsC options) = LineChartOptionsC
  { options | chartDimensions = setHeight ch options.chartDimensions }

{-|
Set the Tooltip Width.
-}
setTooltipWidth : Int -> LineChartOptions -> LineChartOptions
setTooltipWidth tw (LineChartOptionsC options) = LineChartOptionsC
  { options | tooltipDimensions = setWidth tw options.tooltipDimensions }

{-|
Set the Tooltip Height.
-}
setTooltipHeight : Int -> LineChartOptions -> LineChartOptions
setTooltipHeight th (LineChartOptionsC options) = LineChartOptionsC
  { options | tooltipDimensions = setHeight th options.tooltipDimensions }

{-|
Set the Label Width.
-}
setLabelWidth : Int -> LineChartOptions -> LineChartOptions
setLabelWidth lw (LineChartOptionsC options) = LineChartOptionsC
  { options | labelDimensions = setWidth lw options.labelDimensions }

{-|
Set the Label Height.
-}
setLabelHeight : Int -> LineChartOptions -> LineChartOptions
setLabelHeight lh (LineChartOptionsC options) = LineChartOptionsC
  { options | labelDimensions = setHeight lh options.labelDimensions }

{-|
Set the point radius on the chart. Hovered points double their radius.
-}
setPointRadius : Int -> LineChartOptions -> LineChartOptions
setPointRadius r (LineChartOptionsC options) = LineChartOptionsC
  { options | pointRadius = r }

{-|
Used to update the ChartModel type. This should be used inside your update function.

    update : Msg -> Model -> Model
    update msg model =
      case msg of
        ChartMessage chartMsg ->
          { model
          | chartModel = updateChartModel chartMsg model.chartModel
          }
        NoOp -> model
-}
updateChartModel : ChartMsg -> ChartModel -> ChartModel
updateChartModel msg (ChartModelC model) =
  case msg of
    Blur -> ChartModelC { model | hovered = Nothing }
    Focus hp -> ChartModelC { model | hovered = Just hp }
