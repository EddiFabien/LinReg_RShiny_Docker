residualUI <- function(id){
      ns <- NS(id)
      tagList(
            sidebarPanel(
                  selectInput(inputId = ns("num.var"), "Select Variable : ", choices = NULL),
                  checkboxGroupInput(inputId = ns("res.display"), "Choose that you would like to show : ",
                                     choices = list(
                                       "Residual mean" = "res.mean",
                                       "Residual graphical" = "res.graph",
                                       "Henry line (QQ-norm)" = "qqnorm",
                                       "Jarque-Bera test" = "jb.test"),
                                     selected = c("test"),
                  ),
                  width = 2 # Adjust the sidebar width (default is 4)
            ),
            mainPanel(title = "Result",
                  uiOutput(ns("res.result")),
                  width = 7 # Adjust the sidebar width (default is 8)
            )
      )
}

residualServer <- function(id, data, regressionResult){
      moduleServer(id, function(input, output, session){
            ns <- session$ns
            # Update variable selection inputs for residual analysis
            observe({
                  req(data())
                  numeric_vars <- names(data())[sapply(data(), is.numeric)]
                  updateSelectInput(session, "num.var", choices = numeric_vars)
            })
            
            # Generate complementary information dynamically
            output$res.result <- renderUI({
                  selected_info <- input$res.display  # Get user-selected options
                  info_list <- list()
                  # Add residual mean
                  if ("res.mean" %in% selected_info) {
                    info_list <- append(info_list, list(
                          h4("Residual mean : "),
                          verbatimTextOutput(ns("mean")),
                          h5("Notice that the mean of the residuals is necessary equal of zero in regression with Intercept."),
                          h5("Otherwise, there are problems there.")
                    ))
                  }
                  # Add residual graphical
                  if ("res.graph" %in% selected_info) {
                    info_list <- append(info_list, list(
                          tags$hr(),
                          h4("Residual graphical : "),
                          plotOutput(ns("graph"), width = "600px", height = "400px"),
                          h5("Notice that if the residual graphical has a regular shape, there are problems in the regression.")
                    ))
              }
              # Add QQ-norm
              if ("qqnorm" %in% selected_info){
                    info_list <- append(info_list, list(
                          tags$hr(),
                          h4("Henry line (QQ-norm) : "),
                          plotOutput(ns("henryline"), width = "600px", height = "400px"),
                          h5("Notice that the residuals would be more or less compatible with the normal distribution if the plot result was in line shape (less or more) .")
                    ))
              }
              # Add Jarque-Bera test
              if ("jb.test" %in% selected_info) {
                    info_list <- append(info_list, list(
                          tags$hr(),
                          h4("Jarque-Bera test (Normality test): "),
                          h5("HO : The residuals are compatible with the normal distribution."),
                          textOutput(ns("jbtest"))
                    ))
              }
              do.call(tagList, info_list)
            })
            
            # Reactive outputs for complementary information
            output$mean <- renderPrint({
                  req(regressionResult())
                  summary <- summary(regressionResult())
                  mean(summary$residuals)
            })
            output$graph <- renderPlot({
                  req(data(), regressionResult(), input$num.var)
                  summary <- summary(regressionResult())
                  ggplot(data(), aes_string(x = input$num.var, y = summary$residuals)) +
                        geom_point(color = "blue") +
                        geom_hline(yintercept = 0, linetype = "solid", color = "red") +
                        labs(
                          title = paste("residuals vs", input$num.var),
                          x = input$num.var,
                          y = "residuals"
                        ) +
                        theme_minimal()
            })
            output$henryline <- renderPlot({
                  req(regressionResult())
                  summary <- summary(regressionResult())
                  qqnorm(summary$residuals, col = "green", pch = 19)
            })
            output$jbtest <- renderText({
                  req(regressionResult())
                  summary <- summary(regressionResult())
                  residus <-summary$residuals
                  # Asymmetry
                  g1 <- mean(residus^3)/(mean(residus^2)^1.5)
                  # Flattening
                  g2 <- mean(residus^4)/(mean(residus^2)^2)-3
                  # Statistic of test of Jarque - Bera
                  Tjb <- (summary$df / 6) * (g1^2 + (g2^2) / 4)
                  # p-value of test of Jarque - Bera
                  p_value <- pchisq(Tjb, 2, lower.tail = FALSE)

                  # Decision based on p-value
                  if (p_value[2] < 0.05) {
                        result <- "Result: H0 rejected (Significant)"
                  } else if( 0.05 < p_value[2]) {
                        result <- "Result: H0 not rejected (Not significant)"
                  }
                  else {
                        result <- "Error: p_value is not valid."
                  }
                  result
            })
      })
}