# easy-charts

This is a Charting library with the goals of being
1. Easy to use
2. Customizable

Currently, we only support Line Charts and Pie Charts.

# Required Setup
Using this library is still a multi-step process, even though we want to be easy to use.

This is enforced by the Elm architecture itself, which processes all the events in one application in one single function (update).

1. Install the library in your project:
```bash
elm install ohri-anurag/easy-charts
```

2. Check the module `LineChart`/`PieChart` for help on setting up:
    - `update`
    - `init`
    - `Model`
    - `Msg`

3. Customize your chart by using the options! Check the respective module definition for more details.

# FAQs

## General
### How do we support multiple charts on one page?

Like the example below. Same goes for Pie Charts. The example file uses both Line and Pie charts within a single page.
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

### Can I see some examples?

[Sure.](https://ohri-anurag.github.io/easy-charts/)

You can clone this repo, go inside the `examples` directory, and run `elm make src/Main.elm`. Open the `index.html` file thus generated.

## Line Charts
### How do I rotate the x-axis labels?

Sometimes, the labels on the horizontal axis (x axis) are too long. This can cause them to display over each other. You can set a rotation value in this case using `setLabelRotation`. Example 1 shows how to do this.

### My y-axis labels are too long, they are getting cropped off. How do I rectify this?

You can set an appropriate width for these labels, using `setValueWidth`. Example 3 shows how to do this.
