module Main exposing (..)

import Browser exposing (sandbox)
import Html exposing (Html, div)
import List exposing (..)
import Palette.X11 exposing (..)
import String exposing (fromFloat, fromInt)
import SolidColor exposing (fromRGB)
import TransparentColor exposing (fromColor, opaque)

import LineChart as L
import PieChart as P

type alias Flags = { screenHeight : Int, screenWidth : Int }

type alias Model =
  { chartModel1 : L.ChartModel
  , chartModel2 : L.ChartModel
  , chartModel3 : L.ChartModel
  , chartModel4 : L.ChartModel
  , chartModel5 : L.ChartModel
  , pieModel1 : P.PieModel
  , pieModel2 : P.PieModel
  }

type Msg
  = NoOp
  | ChartMessage1 L.ChartMsg
  | ChartMessage2 L.ChartMsg
  | ChartMessage3 L.ChartMsg
  | ChartMessage4 L.ChartMsg
  | ChartMessage5 L.ChartMsg
  | PieMessage1 P.PieMsg
  | PieMessage2 P.PieMsg

init : Model
init =
  { chartModel1 = L.initChartModel
  , chartModel2 = L.initChartModel
  , chartModel3 = L.initChartModel
  , chartModel4 = L.initChartModel
  , chartModel5 = L.initChartModel
  , pieModel1 = P.initPieModel
  , pieModel2 = P.initPieModel
  }

view : Model -> Html Msg
view model = div
  []
  [ L.lineChart options data ChartMessage1 model.chartModel1
  , L.lineChart options2 data2 ChartMessage2 model.chartModel2
  , L.lineChart options3 data3 ChartMessage3 model.chartModel3
  , L.lineChart options4 data4 ChartMessage4 model.chartModel4
  , L.lineChart options5 data5 ChartMessage5 model.chartModel5
  , P.pieChart commonPieOptions pieData1 PieMessage1 model.pieModel1
  , P.pieChart pieOptions2 pieData2 PieMessage2 model.pieModel2
  ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChartMessage1 chartMsg -> { model | chartModel1 = L.updateChartModel chartMsg model.chartModel1 }
    ChartMessage2 chartMsg -> { model | chartModel2 = L.updateChartModel chartMsg model.chartModel2 }
    ChartMessage3 chartMsg -> { model | chartModel3 = L.updateChartModel chartMsg model.chartModel3 }
    ChartMessage4 chartMsg -> { model | chartModel4 = L.updateChartModel chartMsg model.chartModel4 }
    ChartMessage5 chartMsg -> { model | chartModel5 = L.updateChartModel chartMsg model.chartModel5 }
    PieMessage1 pieMsg1 -> { model | pieModel1 = P.updatePieModel pieMsg1 model.pieModel1 }
    PieMessage2 pieMsg2 -> { model | pieModel2 = P.updatePieModel pieMsg2 model.pieModel2 }
    NoOp -> model

commonOptions : L.LineChartOptions
commonOptions =
  L.setChartWidth 1200 L.defaultLineChartOptions
  |> L.setChartHeight 800
  |> L.setPointRadius 4

data : L.LineChartData
data =
  L.createData (List.map .date rawData)
    [ L.createDataSet "Anurag" (List.map .anuragWeight rawData) <| fromColor opaque green
    ]

options : L.LineChartOptions
options = commonOptions
  |> L.setLabelRotation -50

data2 : L.LineChartData
data2 =
  L.createData ["Mon", "Tue", "Wed", "Thu", "Fri"]
    [ L.createDataSet "Wakeup Time" [7,8,9,8,7] <| fromColor opaque blue
    , L.createDataSet "Sleeping Time" [23,22,21,22,23] <| fromColor opaque red
    ]

options2 : L.LineChartOptions
options2 = commonOptions
  |> L.setTooltipWidth 150

data3 : L.LineChartData
data3 =
  L.createData ["Mon", "Tue", "Wed", "Thu", "Fri"]
    [ L.createDataSet "Toosla Corp" [0.147,0.148,0.149,0.148,0.147] <| fromColor opaque brown
    , L.createDataSet "Abode Limited" [0.1523,0.1522,0.1521,0.1522,0.1523] <| fromColor opaque aquamarine
    ]

options3 : L.LineChartOptions
options3 = commonOptions
  |> L.setValueWidth 150
  |> L.setTooltipWidth 170

data4 : L.LineChartData
data4 =
  let
    xs = range 0 20
  in
  L.createData (map fromInt xs)
    [ L.createDataSet "x" (map (\x -> toFloat x) xs) <| fromColor opaque lightGreen
    , L.createDataSet "x^2" (map (\x -> toFloat (x^2)) xs) <| fromColor opaque darkGreen
    , L.createDataSet "x^3" (map (\x -> toFloat (x^3)) xs) <| fromColor opaque darkRed
    ]

options4 : L.LineChartOptions
options4 = commonOptions
  |> L.setLegendWidth 50

data5 : L.LineChartData
data5 =
  let
    ds = range 0 360
    xs = map (toFloat >> degrees) ds
  in
  L.createData (map fromInt ds)
    [ L.createDataSet "sin x" (map (\x -> sin x) xs) <| fromColor opaque brown
    , L.createDataSet "cos x" (map (\x -> cos x) xs) <| fromColor opaque aquamarine
    ]

options5 : L.LineChartOptions
options5 = commonOptions
  |> L.setPointRadius 2
  |> L.setTooltipWidth 210

commonPieOptions : P.PieChartOptions
commonPieOptions = P.setChartWidth 1200 P.defaultPieChartOptions
  |> P.setChartHeight 800

pieData1 : P.PieChartData
pieData1 = P.createData
  [ P.createDataSet "Q1" 25 (fromColor opaque blue)
  , P.createDataSet "Q2" 25 (fromColor opaque red)
  , P.createDataSet "Q3" 25 (fromColor opaque green)
  , P.createDataSet "Q4" 25 (fromColor opaque aquamarine)
  , P.createDataSet "Q5" 25 (fromColor opaque magenta)
  ]

pieData2 : P.PieChartData
pieData2 = P.createData
  [ P.createDataSet "Sleep" 8 (fromColor opaque <| fromRGB (142, 236, 245) )
  , P.createDataSet "Gym" 1 (fromColor opaque <| fromRGB (147, 129, 255))
  , P.createDataSet "Work" 8 (fromColor opaque <| fromRGB (255, 104, 107))
  , P.createDataSet "Wifey" 5 (fromColor opaque <| fromRGB (255, 112, 166))
  , P.createDataSet "Hobbies" 2 (fromColor opaque <| fromRGB (123, 241, 168))
  ]

pieOptions2 : P.PieChartOptions
pieOptions2 = P.setTooltipWidth 200 commonPieOptions

main = sandbox {view = view, update = update, init = init}

-- This is a week by week data of my own weight in kg.
rawData =
  [ { date = "16/01/2020", anuragWeight = 81.7 }
  , { date = "23/02/2020", anuragWeight = 81.5 }
  , { date = "08/03/2020", anuragWeight = 81.1 }
  , { date = "29/03/2020", anuragWeight = 81.5 }
  , { date = "08/04/2020", anuragWeight = 80.4 }
  , { date = "19/04/2020", anuragWeight = 79.3 }
  , { date = "15/05/2020", anuragWeight = 77.8 }
  , { date = "31/05/2020", anuragWeight = 78.6 }
  , { date = "08/06/2020", anuragWeight = 76.6 }
  , { date = "14/06/2020", anuragWeight = 77.0 }
  , { date = "21/06/2020", anuragWeight = 76.7 }
  , { date = "28/06/2020", anuragWeight = 76.6 }
  , { date = "29/07/2020", anuragWeight = 78.4 }
  , { date = "31/07/2020", anuragWeight = 78.5 }
  , { date = "10/08/2020", anuragWeight = 79.2 }
  , { date = "23/08/2020", anuragWeight = 78.4 }
  , { date = "30/08/2020", anuragWeight = 78.2 }
  , { date = "06/09/2020", anuragWeight = 78.2 }
  , { date = "20/09/2020", anuragWeight = 78.8 }
  , { date = "27/09/2020", anuragWeight = 79.2 }
  , { date = "21/10/2020", anuragWeight = 79.5 }
  , { date = "01/11/2020", anuragWeight = 80.4 }
  , { date = "08/11/2020", anuragWeight = 80.2 }
  , { date = "29/11/2020", anuragWeight = 80.9 }
  , { date = "06/12/2020", anuragWeight = 81.8 }
  , { date = "07/01/2021", anuragWeight = 81.1 }
  , { date = "17/01/2021", anuragWeight = 82.5 }
  , { date = "27/01/2021", anuragWeight = 81.6 }
  , { date = "31/01/2021", anuragWeight = 81.8 }
  , { date = "07/02/2021", anuragWeight = 80.5 }
  , { date = "14/02/2021", anuragWeight = 80.5 }
  , { date = "21/02/2021", anuragWeight = 80.2 }
  , { date = "03/03/2021", anuragWeight = 80.6 }
  , { date = "07/03/2021", anuragWeight = 79.9 }
  , { date = "14/03/2021", anuragWeight = 78.3 }
  , { date = "21/03/2021", anuragWeight = 77.8 }
  , { date = "28/03/2021", anuragWeight = 77.3 }
  , { date = "04/04/2021", anuragWeight = 77 }
  , { date = "14/05/2021", anuragWeight = 75.9 }
  , { date = "22/05/2021", anuragWeight = 76.1 }
  , { date = "30/05/2021", anuragWeight = 77.6 }
  , { date = "14/06/2021", anuragWeight = 77.4 }
  , { date = "20/06/2021", anuragWeight = 78 }
  , { date = "27/06/2021", anuragWeight = 77.3 }
  , { date = "04/07/2021", anuragWeight = 77.7 }
  , { date = "11/07/2021", anuragWeight = 77.7 }
  , { date = "18/07/2021", anuragWeight = 78.5 }
  , { date = "15/08/2021", anuragWeight = 78.1 }
  , { date = "05/09/2021", anuragWeight = 78.2 }
  , { date = "19/09/2021", anuragWeight = 78.9 }
  , { date = "26/09/2021", anuragWeight = 79.5 }
  , { date = "03/10/2021", anuragWeight = 78.6 }
  , { date = "10/10/2021", anuragWeight = 79.2 }
  , { date = "17/10/2021", anuragWeight = 79.6 }
  , { date = "24/10/2021", anuragWeight = 79.8 }
  , { date = "01/11/2021", anuragWeight = 79.7 }
  , { date = "08/11/2021", anuragWeight = 79.6 }
  , { date = "15/11/2021", anuragWeight = 80.7 }
  , { date = "22/11/2021", anuragWeight = 80.1 }
  , { date = "05/12/2021", anuragWeight = 79.5 }
  , { date = "12/12/2021", anuragWeight = 79.1 }
  , { date = "20/12/2021", anuragWeight = 80 }
  , { date = "26/12/2021", anuragWeight = 80.7 }
  , { date = "01/01/2022", anuragWeight = 79.7 }
  , { date = "09/01/2022", anuragWeight = 81.4 }
  , { date = "16/01/2022", anuragWeight = 81.85 }
  , { date = "06/02/2022", anuragWeight = 85.15 }
  , { date = "14/02/2022", anuragWeight = 84.85 }
  , { date = "20/02/2022", anuragWeight = 83.45 }
  , { date = "27/02/2022", anuragWeight = 82.3 }
  , { date = "06/03/2022", anuragWeight = 82.1 }
  , { date = "07/03/2022", anuragWeight = 81.8 }
  , { date = "12/03/2022", anuragWeight = 81 }
  , { date = "18/03/2022", anuragWeight = 80.5 }
  , { date = "28/03/2022", anuragWeight = 80.1 }
  , { date = "03/04/2022", anuragWeight = 80.05 }
  , { date = "10/04/2022", anuragWeight = 79 }
  , { date = "18/04/2022", anuragWeight = 78.6 }
  , { date = "25/04/2022", anuragWeight = 79.15 }
  , { date = "01/05/2022", anuragWeight = 78 }
  , { date = "08/05/2022", anuragWeight = 77.8 }
  , { date = "14/05/2022", anuragWeight = 77.4 }
  , { date = "22/05/2022", anuragWeight = 77.5 }
  , { date = "29/05/2022", anuragWeight = 76.35 }
  , { date = "05/06/2022", anuragWeight = 75.25 }
  , { date = "11/06/2022", anuragWeight = 74.9 }
  , { date = "19/06/2022", anuragWeight = 75.1 }
  , { date = "25/06/2022", anuragWeight = 74.4 }
  , { date = "28/06/2022", anuragWeight = 74.3 }
  , { date = "01/07/2022", anuragWeight = 73.8 }
  , { date = "10/07/2022", anuragWeight = 76 }
  , { date = "19/07/2022", anuragWeight = 74.1 }
  , { date = "22/07/2022", anuragWeight = 73.2 }
  , { date = "25/07/2022", anuragWeight = 72.6 }
  , { date = "07/08/2022", anuragWeight = 72.8 }
  , { date = "21/08/2022", anuragWeight = 71.9 }
  , { date = "29/08/2022", anuragWeight = 73.2 }
  , { date = "04/09/2022", anuragWeight = 74.2 }
  , { date = "11/09/2022", anuragWeight = 71.6 }
  , { date = "27/09/2022", anuragWeight = 73.6 }
  , { date = "02/10/2022", anuragWeight = 74.1 }
  , { date = "09/10/2022", anuragWeight = 72.7 }
  , { date = "15/10/2022", anuragWeight = 73 }
  , { date = "24/10/2022", anuragWeight = 74.3 }
  , { date = "30/10/2022", anuragWeight = 73.8 }
  , { date = "07/11/2022", anuragWeight = 74.3 }
  , { date = "13/11/2022", anuragWeight = 73.8 }
  , { date = "21/11/2022", anuragWeight = 75.4 }
  , { date = "24/11/2022", anuragWeight = 73.7 }
  , { date = "29/11/2022", anuragWeight = 73.5 }
  , { date = "10/12/2022", anuragWeight = 73.6 }
  , { date = "14/12/2022", anuragWeight = 73.9 }
  , { date = "17/12/2022", anuragWeight = 73.1 }
  , { date = "24/12/2022", anuragWeight = 74.2 }
  , { date = "01/01/2023", anuragWeight = 77.5 }
  , { date = "02/01/2023", anuragWeight = 75.6 }
  , { date = "07/01/2023", anuragWeight = 75.5 }
  , { date = "15/01/2023", anuragWeight = 76.6 }
  , { date = "20/01/2023", anuragWeight = 76.6 }
  , { date = "26/01/2023", anuragWeight = 76.7 }
  , { date = "30/01/2023", anuragWeight = 76.8 }
  , { date = "03/02/2023", anuragWeight = 75.5 }
  , { date = "04/02/2023", anuragWeight = 75.5 }
  , { date = "05/02/2023", anuragWeight = 75.6 }
  , { date = "12/02/2023", anuragWeight = 74.7 }
  , { date = "14/02/2023", anuragWeight = 74 }
  , { date = "18/02/2023", anuragWeight = 73.7 }
  , { date = "20/02/2023", anuragWeight = 73.9 }
  , { date = "23/02/2023", anuragWeight = 73.7 }
  , { date = "01/03/2023", anuragWeight = 74.2 }
  , { date = "05/03/2023", anuragWeight = 74.4 }
  , { date = "07/03/2023", anuragWeight = 74.4 }
  , { date = "09/03/2023", anuragWeight = 72.9 }
  , { date = "12/03/2023", anuragWeight = 74.2 }
  , { date = "13/03/2023", anuragWeight = 73.5 }
  , { date = "16/03/2023", anuragWeight = 73.1 }
  , { date = "19/03/2023", anuragWeight = 74.1 }
  , { date = "27/03/2023", anuragWeight = 73 }
  , { date = "03/04/2023", anuragWeight = 73.4 }
  , { date = "04/04/2023", anuragWeight = 72.6 }
  , { date = "10/04/2023", anuragWeight = 73.7 }
  , { date = "17/04/2023", anuragWeight = 74.8 }
  , { date = "24/04/2023", anuragWeight = 73.5 }
  ]
