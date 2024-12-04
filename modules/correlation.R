# Correlation UI
correlationUI <- function(id){
      ns <- NS(id)
      tagList(
            sidebarPanel(
                  selectInput(inputId = ns("var1"), "Select First Numerical Variable", choices = NULL),
                  selectInput(inputId = ns("var2"), "Select Second Numerical Variable", choices = NULL),
                  tags$hr(),
                  checkboxGroupInput(inputId = ns("display"), "Choose that you would like to show : ",
                                     choices = list(
                                       "Significance test" = "test",
                                       "Confidence interval" = "ci"),
                                     selected = c("test")
                  ),
                  width = 2 # Adjust the sidebar width (default is 4)
            ),
            mainPanel(
                  tabsetPanel(
                    tabPanel(title = "Correlation coefficient (Pearson)",
                             h4("Correlation coefficient :"),
                             textOutput(ns("correlation")),
                             tags$hr(),
                             h4("Visualization :"),
                             plotOutput(ns("scatterPlot"), width = "600px", height = "400px")
                    ), 
                    tabPanel(title = "Significance test of the correlation value", uiOutput(ns("significancetest")))
                  ),
                  width = 7 # Adjust the sidebar width (default is 8)
            )
      )
}


# Correlation Server
correlationServer <- function(id, data) {
      moduleServer(id, function(input, output, session) {
            ns <- session$ns
            
            # Update variable choices dynamically
            observeEvent(data(), {
                  req(data())  # Ensure data is available
                  numeric_vars <- names(data())[sapply(data(), is.numeric)]  # Filter numeric columns
                  updateSelectInput(session, "var1", choices = numeric_vars)
                  updateSelectInput(session, "var2", choices = numeric_vars)
            })
            
            # Render the correlation result
            output$correlation <- renderText({
                  req(input$var1, input$var2, data())
                  x <- data()[[input$var1]]
                  y <- data()[[input$var2]]
                  cor_test <- cor(x, y)
                  round(cor_test, 3)
            })
            
            # Render scatter plot
            output$scatterPlot <- renderPlot({
                  req(input$var1, input$var2, data())
                  ggplot(data(), aes_string(x = input$var1, y = input$var2)) +
                  geom_point(color = "#00abff", alpha = 0.7, size = 3) +
                  labs(title = paste("Scatter Plot of", input$var1, "vs", input$var2),
                  x = input$var1, y = input$var2) + theme_minimal()
            })
            
            # Generate significance test dynamically
            output$significancetest <- renderUI({
                  req(data(), input$display)
                  selected_display <- input$display  # Get user-selected options
                  info_list <- list()
                  
                  # Add significance test
                  if ("test" %in% selected_display) {
                        info_list <- append(info_list, list(
                              h4("Significance Test (5% Risk):"), 
                              h5("H0: The correlation coefficient value is null"),
                              tags$hr(),
                              textOutput(ns("p_value")),
                              textOutput(ns("signtest"))
                    ))
                  }
                  # Add confidence interval
                  if ("ci" %in% selected_display) {
                        info_list <- append(info_list, list(
                              tags$hr(),
                              h4("Confidence Interval (5% Risk):"),
                              textOutput(ns("confinter"))
                        ))
                  }
                  do.call(tagList, info_list)
            })
            
            # Render p-value
            output$p_value <- renderText({
                  req(input$var1, input$var2, data())
                  x <- data()[[input$var1]]
                  y <- data()[[input$var2]]
                  cor_val <- cor(x, y, use = "complete.obs")
                  t_val <- cor_val / sqrt((1 - cor_val^2) / (length(x) - 2))
                  p_val <- 2 * (1 - pt(abs(t_val), df = length(x) - 2))
                  paste("p-value:", round(p_val, 3))
            })
            
            # Render significance test result
            output$signtest <- renderText({
                  req(input$var1, input$var2, data())
                  x <- data()[[input$var1]]
                  y <- data()[[input$var2]]
                  cor_val <- cor(x, y, use = "complete.obs")
                  t_val <- cor_val / sqrt((1 - cor_val^2) / (length(x) - 2))
                  p_val <- 2 * (1 - pt(abs(t_val), df = length(x) - 2))
                  if (p_val < 0.05) {
                    "Result: H0 rejected (Significant)"
                  } else {
                    "Result: H0 not rejected (Not significant)"
                  }
            })
            
            # Render confidence interval
            output$confinter <- renderText({
                  req(input$var1, input$var2, data())
                  x <- data()[[input$var1]]
                  y <- data()[[input$var2]]
                  cor_val <- cor(x, y, use = "complete.obs")
                  n <- length(x)
                  z <- 0.5 * log((1 + cor_val) / (1 - cor_val))
                  se <- 1 / sqrt(n - 3)
                  ci_low <- tanh(z - 1.96 * se)
                  ci_high <- tanh(z + 1.96 * se)
                  paste("Confidence Interval: [", round(ci_low, 3), ",", round(ci_high, 3), "]")
            })
      })
}



