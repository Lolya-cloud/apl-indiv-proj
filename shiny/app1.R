# Load necessary libraries
library(shiny)
library(networkD3)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Neural Network Influence on Prediction"),
  
  # Sidebar for adjusting input feature values
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age:", min = 20, max = 80, value = 50),
      sliderInput("bp", "Resting BP:", min = 80, max = 200, value = 120),
      sliderInput("chol", "Cholesterol:", min = 100, max = 400, value = 200),
      sliderInput("hr", "Max Heart Rate:", min = 50, max = 200, value = 150),
      sliderInput("oldpeak", "Oldpeak (ST Depression):", min = 0, max = 6, value = 1)
    ),
    
    # Main panel to display the neural network visualization and prediction output
    mainPanel(
      forceNetworkOutput("networkPlot"),  # Force-directed network plot
      textOutput("prediction")  # Predicted value output
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  
  # Reactive expression to calculate prediction based on input
  predicted_value <- reactive({
    # Dummy model formula for prediction (replace with actual model logic)
    0.5 * input$age + 0.2 * input$bp + 0.3 * input$chol + 0.1 * input$hr - 0.2 * input$oldpeak
  })
  
  # Output prediction text
  output$prediction <- renderText({
    paste("Predicted Value: ", round(predicted_value(), 2))
  })
  
  # Enhanced neural network visualization with dynamic edge thickness
  output$networkPlot <- renderForceNetwork({
    # Define the nodes (input features and output)
    nodes <- data.frame(name = c("Age", "Resting BP", "Cholesterol", "Max Heart Rate", "Oldpeak", "Output"),
                        group = rep(1, 6))  # Add a 'group' column with the same value for all nodes
    
    # Define the links (connections between input features and output)
    links <- data.frame(
      source = c(0, 1, 2, 3, 4),  # Indices of the input nodes
      target = c(5, 5, 5, 5, 5),  # Index of the output node
      value = c(input$age, input$bp, input$chol, input$hr, input$oldpeak)  # Activation strength
    )
    
    # Create force network visualization with dynamic edge thickness based on input values
    forceNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                 Value = "value", NodeID = "name", Group = "group",  # Specify the group column here
                 opacity = 0.9, fontSize = 12,
                 linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),  # Adjust link width based on value
                 linkColour = "#999", zoom = TRUE)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
