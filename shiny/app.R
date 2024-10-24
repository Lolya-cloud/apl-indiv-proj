library(shiny)
library(dplyr)
library(glmnet)
library(pROC)
library(MASS)        
library(caret)
set.seed(42)
# functions
convert_to_numerical = function(df) {
  # Convert 'sex' to 0 and 1
  df$sex[df$sex == "Female"] = 0
  df$sex[df$sex == "Male"] = 1
  
  # Convert 'chest.pain.type' to 1, 2, 3, 4
  df$chest.pain.type[df$chest.pain.type == "Typical angina"] = 1
  df$chest.pain.type[df$chest.pain.type == "Atypical angina"] = 2
  df$chest.pain.type[df$chest.pain.type == "Non-anginal"] = 3
  df$chest.pain.type[df$chest.pain.type == "Asymptomatic"] = 4
  
  # Convert 'fasting.blood.sugar' to 0 and 1
  df$fasting.blood.sugar[df$fasting.blood.sugar == "<120 mg/dl"] = 0
  df$fasting.blood.sugar[df$fasting.blood.sugar == "≥120 mg/dl"] = 1
  
  # Convert 'resting.ecg' to 0, 1, 2
  df$resting.ecg[df$resting.ecg == "Normal"] = 0
  df$resting.ecg[df$resting.ecg == "Wave abnormality"] = 1
  df$resting.ecg[df$resting.ecg == "Hypertrophy"] = 2
  
  # Convert 'exercise.angina' to 0 and 1
  df$exercise.angina[df$exercise.angina == "No"] = 0
  df$exercise.angina[df$exercise.angina == "Yes"] = 1
  
  # Convert 'ST.slope' to 1, 2, 3
  df$ST.slope[df$ST.slope == "Upsloping"] = 1
  df$ST.slope[df$ST.slope == "Flat"] = 2
  df$ST.slope[df$ST.slope == "Downsloping"] = 3
  
  return(df)
}

preprocess_data = function(df) {
  df = convert_to_numerical(df)
  df$sex = factor(df$sex, 
                  levels = c(0, 1), 
                  labels = c("Female", "Male"))
  
  df$chest.pain.type = factor(df$chest.pain.type, 
                              levels = c(1, 2, 3, 4), 
                              labels = c("Typical angina", "Atypical angina", "Non-anginal", "Asymptomatic"))
  
  df$fasting.blood.sugar = factor(df$fasting.blood.sugar, 
                                  levels = c(0, 1), 
                                  labels = c("Smaller 120 mg/dl", "Larger 120 mg/dl"))
  
  df$resting.ecg = factor(df$resting.ecg, 
                          levels = c(0, 1, 2), 
                          labels = c("Normal", "Wave abnormality", "Hypertrophy"))
  
  df$exercise.angina = factor(df$exercise.angina, 
                              levels = c(0, 1), 
                              labels = c("No", "Yes"))
  
  
  df$ST.slope = factor(df$ST.slope, 
                       levels = c(1, 2, 3), 
                       labels = c("Upsloping", "Flat", "Downsloping"))
  
  return(df)
}

run_model = function(input.data, lambda) {
  # scale input and prepare for the model
  newx = model.matrix(~ ., data = input.data)[, -1]
  
  # predict result, extract coefficients and predictors.
  prob = predict(lasso_model, newx = newx, type = "response", s = lambda)
  coeffs = coef(lasso_model, s = lambda)
  coeffs = as.matrix(coeffs)
  predictors = rownames(coeffs)
  coefficients = as.numeric(coeffs)
  predictors = predictors[-1]
  coefficients = coefficients[-1]
  
  result = list(
    coefficients = data.frame(
      Predictor = predictors,
      Coefficient = coefficients
    ),
    probability = as.numeric(prob)
  )
  return(result)
}

validation_auc = function(x, y, lambda) {
  train_indices = sample(1:nrow(x), size = 0.7 * nrow(x))
  test_indices = setdiff(1:nrow(x), train_indices)
  
  x_train = x[train_indices, ]
  y_train = y[train_indices]
  
  x_test = x[test_indices, ]
  y_test = y[test_indices]
  
  lasso.model = glmnet(x_train, y_train, alpha = 1, lambda = lambda, family = "binomial")
  
  pred.matrix = predict(lasso.model, newx = x_test, type = "response", s = lambda)
  pred = as.vector(pred.matrix)
  roc = roc(y_test, pred)
  auc = auc(roc)
  return(list(roc = roc, auc = auc))
}

# Shiny UI
createVisualizationUI = function() {
  plotOutput("model.plot", height = "900px")
}

# load and scale original dataset.
heart.data = readRDS("data.rds")

y = as.numeric(heart.data$disease) - 1
x = model.matrix(disease ~ ., data = heart.data)[, -1]
# fit lasso for a bunch of lambdas (for ui optimization purposes, we do as much pre-computing as possible)
grid = seq(0, 0.4, by=0.01)
lasso_model = glmnet(x, y, alpha = 1, lambda=grid, standardize=TRUE, family = "binomial")

ui = fluidPage(
  titlePanel("Heart Disease Risk Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age:", min = 30, max = 70, value = 30, step = 1),
      
      selectInput("sex", "Sex:", choices = c("Female", "Male"), selected = "Male"),
      
      selectInput("chest.pain.type", "Chest Pain Type:", 
                  choices = c("Typical angina", "Atypical angina", "Non-anginal", "Asymptomatic"),
                  selected = "Non-anginal"),
      
      sliderInput("resting.bp.s", "Resting Blood Pressure (mm Hg):", min = 50, max = 200, value = 120, step = 1),
      
      sliderInput("cholesterol", "Cholesterol (mg/dl):", min = 50, max = 600, value = 200, step = 1),
      
      selectInput("fasting.blood.sugar", "Fasting Blood Sugar:", 
                  choices = c("<120 mg/dl", "≥120 mg/dl"),
                  selected = "<120 mg/dl"),
      
      selectInput("resting.ecg", "Resting ECG:", 
                  choices = c("Normal", "Wave abnormality", "Hypertrophy"), 
                  selected = "Normal"),
      sliderInput("max.heart.rate", "Max Heart Rate:", min = 60, max = 200, value = 150, step = 1),
  
      selectInput("exercise.angina", "Exercise Induced Angina:", 
                  choices = c("No", "Yes"),
                  selected = "No"),

      sliderInput("oldpeak", "Oldpeak (ST depression):", min = -2, max = 6, value = 1.0, step = 0.5),

      selectInput("ST.slope", "ST slope type:", 
                  choices = c("Downsloping", "Flat", "Upsloping"),
                  selected = "Flat"),
      actionButton("predict", "Predict")
    ),
    # visual
    mainPanel(
      sliderInput("lambda", "Regularization strength (L1):", min = 0, max = 0.4, value = 0.1, step = 0.01),
      createVisualizationUI(),
      br(),
      plotOutput("roc", height = "400px", width = "400px")
    )
  )
)


server = function(input, output) {
  pred.prob = reactiveVal(NULL)
  model.coeffs = reactiveVal(NULL)
  roc = reactiveVal(NULL)
  auc = reactiveVal(NULL)
  
  # obtain inputs, process and pass down to the model function
  observeEvent(input$predict, {
    input_data = data.frame(
      age = input$age,
      sex = input$sex,
      chest.pain.type = input$chest.pain.type,
      resting.bp.s = input$resting.bp.s,
      cholesterol = input$cholesterol,
      fasting.blood.sugar = input$fasting.blood.sugar,
      resting.ecg = input$resting.ecg,
      max.heart.rate = input$max.heart.rate,
      exercise.angina = input$exercise.angina,
      oldpeak = input$oldpeak,
      ST.slope = input$ST.slope,
      stringsAsFactors = FALSE
    )
    adjusted_data = preprocess_data(input_data)
    # invoke model function (all of the ml logic happens inside), get prediction results
    model.result = run_model(adjusted_data, input$lambda)
    
    # update display
    pred.prob(model.result$probability)
    model.coeffs(model.result$coefficients)
    
    validation.result = validation_auc(x, y, input$lambda)
    
    # Store ROC and AUC for plotting
    roc(validation.result$roc)
    auc(validation.result$auc)
  })
  
  output$model.plot = renderPlot({
    coeffs = model.coeffs()
    # Placeholders to prevent errors on the first run
    if (is.null(coeffs)) {
      coeffs = data.frame(
        Predictor = c("Age", "Sex", "Chest Pain", "Resting BP", "Cholesterol",
                      "Fasting BS", "Resting ECG", "Max Heart Rate", "Exercise Angina", "Oldpeak"),
        Coefficient = rep(0, 10)
      )
    }
    
    predictors = coeffs$Predictor
    coefs = coeffs$Coefficient
    
    n_predictors = length(predictors)
    x_pred = rep(1, n_predictors)
    y_pred = seq(n_predictors, 1)
    x_out = 5
    y_out = mean(y_pred)
    plot(NULL, xlim = c(0, 6), ylim = c(0.5, n_predictors + 0.5), 
         xaxt = 'n', yaxt = 'n', xlab = '', ylab = '', bty = 'n')
    
    categorical_vars = c("Sex", "Chest Pain", "Fasting BS", "Resting ECG", "Exercise Angina")
    for (i in 1:n_predictors) {
      if (predictors[i] %in% categorical_vars) {
        rect(xleft = x_pred[i] - 0.1, ybottom = y_pred[i] - 0.1, 
             xright = x_pred[i] + 0.1, ytop = y_pred[i] + 0.1, 
             col = "lightblue", border = "black")
      } else {
        symbols(x = x_pred[i], y = y_pred[i], circles = 0.1, 
                add = TRUE, inches = FALSE, bg = "lightblue")
      }
      text(x_pred[i] - 0.2, y_pred[i], labels = predictors[i], pos = 2, cex = 1.1)
    }
    symbols(x = x_out, y = y_out, circles = 0.15, add = TRUE, inches = FALSE, bg = "lightgreen")
    prob_text = ifelse(is.null(pred.prob()), "Probability", paste("P =", round(pred.prob(), 4)))
    text(x_out + 0.2, y_out, labels = prob_text, pos = 4, cex = 1.2)
    
    # Draw lines and coefficients
    for (i in 1:n_predictors) {
      # Set line type and color based on coefficient
      if (abs(coefs[i]) < 1e-6) {
        line_type = "dashed"
        line_color = "red"
        coef_label = "0"
      } else {
        line_type = "solid"
        line_color = "green"
        coef_label = paste0("", round(coefs[i], 4))
      }
      
      # Draw lines from predictors to output
      segments(x_pred[i] + 0.1, y_pred[i], x_out - 0.15, y_out, 
               lwd = 1, col = line_color, lty = line_type)
      # Calculate midpoints for coefficient labels
      x_mid = (x_pred[i] + x_out) / 2
      y_mid = (y_pred[i] + y_out) / 2
      # Place coefficient labels above the lines
      text(x_mid, y_mid + 0.2, labels = coef_label, cex = 0.9)
    }
  })
  # validated roc
  output$roc = renderPlot({
    if (is.null(roc())) {
      plot.new()
      text(0.5, 0.5, "ROC curve will be displayed here after you click 'Predict'", cex = 1)
    } else {
      # Extract Sensitivity and Specificity
      sens = roc()$sensitivities
      spec = roc()$specificities
      
      fpr = 1 - spec
      plot(fpr, sens, type = "l", col = "#1f77b4", lwd = 2, 
           main = "ROC Curve",
           xlab = "1 - Specificity (False Positive Rate)",
           ylab = "Sensitivity (True Positive Rate)",
           xlim = c(0, 1),
           ylim = c(0, 1))
      
      abline(a = 0, b = 1, lty = 2, col = "gray")
      auc_text = paste("AUC =", round(auc(), 4))

      text_x = 0.5 
      text_y = 0.6  
      
      text(text_x, text_y, labels = auc_text, adj = c(0.5, 0), cex = 1.2, col = "black")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)


