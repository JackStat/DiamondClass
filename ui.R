
ui <- fluidPage(
  titlePanel("Predicting Diamond Cuts"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("Depth", label = "Depth:", value = 62
                  , min = 43, max = 79, step = .1)
      ,sliderInput("Table", "Table:", value = 57
                    , min = 43, max = 95, step = .5)
      ,sliderInput("Price", "Price:", value = 2500
                    , min = 325, max = 19000, step = 25)
      ,sliderInput("X", "X:", value = 5.75
                    , min = 0, max = 12, step = .25)
      ,sliderInput("Y", "Y:", value = 5.75
                    , min = 0, max = 12, step = .25)
    ),
    mainPanel(
      DT::dataTableOutput("pred_table")
    )
  )
)