# easy-charts

This is a Charting library with the goals of being
1. Easy to use
2. Customizable

Currently, we only support Line Charts.

# Required Setup
Using this library is still a multi-step process, even though we want to be easy to use.

This is enforced by the Elm architecture itself, which processes all the events in one application in one single function (update).

1. Install the library in your project:
```bash
elm install ohri-anurag/easy-charts
```

2. Check the module `LineCharts` for help on setting up:
    - `update`
    - `init`
    - `Model`
    - `Msg`

3. Customize your chart by using the options! Check module definition for more details.

# FAQs

1. How do we support multiple charts on one page?

Like the example below
```elm
type alias Model =
  { chartModel1: ChartModel
  , chartModel2: ChartModel
  }

type Msg
  = NoOp
  | ChartMessage1 ChartMsg
  | ChartMessage2 ChartMsg

init : Model
init = { chartModel1 = initChartModel, chartModel2 = initChartModel }

view : Model -> Html Msg
view model = div
  []
  [ lineChart options data ChartMessage1 model.chartModel1
  , lineChart options data2 ChartMessage2 model.chartModel2
  ]

update : Msg -> Model -> Model
update msg model =
  case msg of
    ChartMessage1 chartMsg -> { model | chartModel1 = updateChartModel chartMsg model.chartModel1 }
    ChartMessage2 chartMsg -> { model | chartModel2 = updateChartModel chartMsg model.chartModel2 }
    NoOp -> model
```