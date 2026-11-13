## Packages
library(shiny)   # build interactive dashboard
library(dplyr)   # filter selected dates

# Load saved test data
final_test <- readRDS(
  "final_test.rds"
)  # load 2024-2025 test predictions

# User interface
ui <- fluidPage(
  
  titlePanel("SPY 5-Day Volatility Prediction Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      dateInput(
        "selected_date",
        "Choose a trading date:",
        value = min(final_test$Date),
        min = min(final_test$Date),
        max = max(final_test$Date)
      )
    ),
    
    mainPanel(
      
      h3("Market Conditions"),
      
      tableOutput("market_conditions"),
      
      hr(),
      
      h3("Model Predictions"),
      
      tableOutput("model_predictions"),
      
      p(strong("The percentage shown is the model's estimated chance of high volatility over the next five trading days.")),
      
      p( "A value above 50% gives a High Volatility prediction; otherwise the prediction is Normal Volatility."),
      
      hr(),
      
      h3("Actual Outcome"),
      
      textOutput("actual_outcome")
    )
  )
)

## Server logic

server <- function(input, output) {
  
  # Get data for selected date
  selected_data <- reactive({
    final_test %>%
      filter(
        Date == input$selected_date
      )
  })
  
  ## Display market conditions
  output$market_conditions <- renderTable({
    
    data <- selected_data()
    
    req(nrow(data) > 0)  # only continue if selected date is a trading day
    
    data.frame(
      
      Return = paste0(
        round(data$Return * 100, 2),
        "%"
      ),
      
      `20-Day Volatility` = paste0(
        round(data$Volatility20 * 100, 2),
        "%"
      ),
      
      RSI = round(data$RSI14, 2),
      
      check.names = FALSE
    )
  },
  rownames = FALSE)
  
  # Display model predictions
  output$model_predictions <- renderTable({
    
    data <- selected_data()
    
    req(nrow(data) > 0)  # make sure selected date exists
    
    data.frame(
      
      Model = c("Logistic Regression", "XGBoost"),
      
      `Estimated Chance of High Volatility` = c(
        paste0( round(data$Prob_Logistic * 100, 1), "%"),
        
        paste0( round(data$Prob_XGB * 100, 1), "%")
      ),
      
      Prediction = c(
        ifelse( data$Pred_Logistic == 1,
          "High Volatility",
          "Normal Volatility"
        ),
        
        ifelse( data$Pred_XGB == 1,
          "High Volatility",
          "Normal Volatility"
        )
      ),
      
      check.names = FALSE
    )
  },
  rownames = FALSE)
  
  ## Display actual outcome
  output$actual_outcome <- renderText({
    
    data <- selected_data()
    
    req(nrow(data) > 0)  # make sure selected date exists
    
    ifelse(
      data$HighVol == 1,
      "The following 5 trading days were classified as HIGH VOLATILITY.",
      "The following 5 trading days were classified as NORMAL VOLATILITY."
    )
  })
}

## Run dashboard
shinyApp(ui = ui, server = server)