# Linear regression UI
linearRegressionUI <- function(id){
      ns <- NS(id)
      tagList(
            sidebarPanel(
                  selectInput(inputId = ns("type"), "Select regression type : ", choices = c("simple", "multiple"), selected = "simple"),
                  tags$hr(),
                  conditionalPanel(
                        condition = sprintf("input['%s'] == 'simple'", ns("type")),
                        selectInput(inputId = ns("target"), "Select Target Variable (Y)", choices = NULL),
                        selectInput(inputId = ns("feature"), "Select Feature Variable (X) ", choices = NULL),
                        actionButton(ns("runRegression"), "Run Regression"),
                  ),
                  conditionalPanel(
                        condition = sprintf("input['%s'] == 'multiple'", ns("type")),
                        uiOutput(ns("variableSelector")),
                        actionButton(ns("runModel"), "Run Regression"),
                  ),
                  width = 2 # Adjust the sidebar width (default is 4)
            ),
            mainPanel(
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'simple'", ns("type")),
                    tabsetPanel(
                          tabPanel(title = "Summary of the model",
                                   h4("Regression Summary : "),
                                   textOutput(ns("rSquaredOutput")),       # For R-squared
                                   textOutput(ns("adjRSquaredOutput")),   # For Adjusted R-squared
                                   tags$hr(),
                                   h4("Estimated Coefficient : "),
                                   verbatimTextOutput(ns("coefficientsOutput")), # For Coefficients
                                   h4(" Confidence interval of the Estimated Coefficient (With 5% of risk): "),
                                   textOutput(ns("ci_estcoef")),
                          ),
                          tabPanel(title = "Visualization", 
                                   h4("Scatter Plot with Regression Line"),
                                   plotOutput(ns("regressionPlot"), width = "600px", height = "400px"),      # Scatter plot with trend line
                          ),
                          tabPanel(title = "Forecasting", 
                                   h4("Forecasting"),
                                   uiOutput(ns("forecastUI")),            # Dynamic input for new variables
                                   tags$hr(),
                                   h4("Predicted value : "),
                                   textOutput(ns("forecastResult")),       # Forecast result
                                   tags$hr(),
                                   h4("Confidence interval of the predicted value (With 5% of risk) : "),
                                   textOutput(ns("ci_predval"))
                          )
                    )
              ),
              conditionalPanel(
                    condition = sprintf("input['%s'] == 'multiple'", ns("type")),
                    tabsetPanel(
                          tabPanel(title = "Summary of the model",
                                   h4("Regression Summary : "),
                                   textOutput(ns("m_rSquaredOutput")),
                                   textOutput(ns("m_adjRSquaredOutput")),
                                   tags$hr(),
                                   h5("Estimated Coefficients Summary : "),
                                   verbatimTextOutput(ns("m_coefficientsOutput"))
                          ),
                          tabPanel(title = "Forecasting",
                                   uiOutput(ns("m_forecastInputs")),
                                   actionButton(ns("forecastButton"), "Forecast"),
                                   tags$hr(),
                                   textOutput(ns("m_forecastResult"))
                          )
                    )
              ),
              width = 7 # Adjust the sidebar width (default is 8)
            )
      )
}


# Linear regression Server
linearRegressionServer <- function(id, data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # server for SIMPLE LINEAR REGRESSION          
          # Update variable selection inputs for numerical variables
          observeEvent(data(), {
                req(data())  # Ensure data is available
                numeric_vars <- names(data())[sapply(data(), is.numeric)]  # Filter numeric columns
                updateSelectInput(session, "target", choices = numeric_vars)
                updateSelectInput(session, "feature", choices = numeric_vars)
          })
          
          # Run regression
          simpleModel <- eventReactive(input$runRegression, {
                req(data(), input$target, input$feature)
                lm(as.formula(paste(input$target, "~", input$feature)), data = data())
          })
          
          # Reactive outputs for summaries of the model
          # R-squared
          output$rSquaredOutput <- renderText({
                req(simpleModel())
                summary <- summary(simpleModel())
                paste("R-squared:", round(summary$r.squared, 3))
          })
          
          # Adjusted R-squared
          output$adjRSquaredOutput <- renderText({
                req(simpleModel())
                summary <- summary(simpleModel())
                paste("Adjusted R-squared:", round(summary$adj.r.squared, 3))
          })
          
          # Coefficients
          output$coefficientsOutput <- renderPrint({
                req(simpleModel())
                summary <- summary(simpleModel())
                summary$coefficients
          })
          
          # Confidence interval of the estimated coefficient
          output$ci_estcoef <- renderText({
                req(simpleModel())
                CI <- function(a, std_a, alpha, ddl) {
                  # t_theoric
                  t_theoric <- function(alpha, ddl){
                    return (qt(1-alpha/2, ddl))
                  }
                  
                  # CI of a^
                  born_inf = a - t_theoric(alpha, ddl)[2]*std_a
                  born_sup = a + t_theoric(alpha, ddl)[2]*std_a
                  return (paste("[",round(born_inf,3),";",round(born_sup,3),"]"))
                }
            
                # Application
                summary <- summary(simpleModel())
                a = summary$coefficients[2,1]
                std_a = summary$coefficients[2,2]
                alpha = 0.05
                ddl = summary$df
                CI(a, std_a, alpha, ddl)
          })
          
          # Scatter plot and trend line
          output$regressionPlot <- renderPlot({
                req(data(), input$target, input$feature)
                ggplot(data(), aes_string(x = input$feature, y = input$target)) +
                  geom_point(color = "blue") +
                  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add regression line with confidence interval
                  labs(
                    title = "Scatter Plot with Trend Line",
                    x = input$feature,
                    y = input$target
                  ) +
                  theme_minimal()
          })
          
          # Generate UI for forecasting input
          output$forecastUI <- renderUI({
                req(input$feature)  # Ensure predictor variable is selected
                numericInput(
                  ns("newPredictorValue"), 
                  paste("Enter new value for", input$feature), 
                  value = NULL
                )
          })
          
          # Forecasting result
          output$forecastResult <- renderText({
                req(simpleModel(), input$newPredictorValue)  # Ensure model and input are available
                # Create a new data frame with the input value
                new_data <- data.frame(setNames(list(input$newPredictorValue), input$feature))
                # Predict using the model
                predicted_value <- predict(simpleModel(), newdata = new_data, interval = "prediction", level = 0.95)
                round(predicted_value[1], 3)
          })
          
          # Confidence interval of the predicted value
          output$ci_predval <- renderText({
                req(simpleModel(), input$newPredictorValue)  # Ensure model and input are available
                new_data <- data.frame(setNames(list(input$newPredictorValue), input$feature))
                predicted_value <- predict(simpleModel(), newdata = new_data, interval = "prediction", level = 0.95)
                paste("[",round(predicted_value[2], 3),";",round(predicted_value[3], 3),"]")
          })
          
          
    # server for MULTIPLE LINEAR REGRESSION
          # Generate UI for variable selection
          output$variableSelector <- renderUI({
                req(data())
                vars <- names(data())
                tagList(
                  selectInput(ns("targetVar"), "Select Target Variable", choices = vars),
                  selectInput(ns("featureVars"), "Select Feature Variables", 
                              choices = vars, multiple = TRUE)
                )
          })
          
          # Reactive for model results
          multipleModel <- eventReactive(input$runModel, {
                req(data(), input$targetVar, input$featureVars)
                lm(as.formula(paste(input$targetVar, "~", paste(input$featureVars, collapse = "+"))), data = data())
          })
          
          # Outputs for R-squared, Adjusted R-squared, and Coefficients
          output$m_rSquaredOutput <- renderText({
                req(multipleModel())
                r_squared <- summary(multipleModel())$r.squared
                paste("R-squared:", round(r_squared, 3))
          })
          
          output$m_adjRSquaredOutput <- renderText({
                req(multipleModel())
                adj_r_squared <- summary(multipleModel())$adj.r.squared
                paste("Adjusted R-squared:", round(adj_r_squared, 3))
          })
          
          output$m_coefficientsOutput <- renderPrint({
                req(multipleModel())
                summary(multipleModel())$coefficients
          })
          
          # Generate UI for forecasting inputs
          output$m_forecastInputs <- renderUI({
                req(input$featureVars)
                lapply(input$featureVars, function(var) {
                  numericInput(ns(var), paste("Enter value for", var), value = NA)
                })
          })
          
          
          # Forecasting
          output$m_forecastResult <- renderText({
                req(multipleModel(), input$forecastButton, input$featureVars)
                
                # Validate feature variables
                valid_feature_vars <- input$featureVars[input$featureVars %in% names(data())]
                if (length(valid_feature_vars) == 0) {
                      stop("No valid feature variables selected for forecasting.")
                }
                
                # Construct new_data with proper error handling and type matching
                new_data <- data.frame(
                      lapply(valid_feature_vars, function(var) {
                            value <- input[[var]]
                            if (is.null(value)) {
                              stop(paste("Missing input for variable:", var))
                            }
                            
                            # Ensure the input value matches the data type of the corresponding column in `data()`
                            if (is.numeric(data()[[var]])) {
                              as.numeric(value)
                            } else if (is.factor(data()[[var]])) {
                              factor(value, levels = levels(data()[[var]]))
                            } else {
                              value
                            }
                          }),
                      stringsAsFactors = FALSE
                )
                
                # Assign column names to new_data
                names(new_data) <- valid_feature_vars
                
                # Check for missing or invalid data
                if (any(is.na(new_data))) {
                  stop("Some inputs are missing or invalid. Please ensure all inputs are provided and valid.")
                }
                
                # Generate the forecast
                forecast <- predict(multipleModel(), newdata = new_data)
                paste("Forecasted value:", round(forecast, 3))
          })
          
          # Combine models based on user selection
          selectedModel <- reactive({
                req(input$type)  # Ensure the regression type is selected
                if (input$type == "simple") {
                      simpleModel()
                } else {
                      multipleModel()
                }
          })
          
          return(selectedModel)
  })

}