library(shiny)
library(glmnet)

df = readRDS("data.rds")

# Define UI
ui <- fluidPage(
  titlePanel("Lasso Regression with Heart Disease"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "lambda",
        label = "Lambda (Regularization Parameter):",
        min = 0,
        max = 1,
        value = 0.037649358,
        step = 0.01
      )
    ),
    
    mainPanel(
      tableOutput(outputId = "coefficients")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to fit the Lasso model
  lasso_model <- reactive({
    numerical_vars = c("age", "resting.bp.s", "cholesterol", "max.heart.rate", "oldpeak")
    
    scaled = as.data.frame(lapply(df.final[numerical_vars], function(x) {
      (x - median(x, na.rm = TRUE)) / IQR(x, na.rm = TRUE)
    }))
    
    # Combine scaled numerical variables with the rest of the dataset
    df.final.scaled <- cbind(scaled, df.final[setdiff(names(df.final), numerical_vars)])
    
    
    y = as.numeric(df.final.scaled$disease) - 1
    x = model.matrix(disease~., data=df.final.scaled)[, -1]
    glmnet(x, y, alpha = 1, lambda = input$lambda)
  })
  output$coefficients <- renderTable({
    coef_matrix <- as.matrix(coef(lasso_model()))
    coef_df <- data.frame(
      Predictor = rownames(coef_matrix),
      Coefficient = coef_matrix[, 1]
    )
    coef_df
  })
}

# Run the app
shinyApp(ui = ui, server = server)