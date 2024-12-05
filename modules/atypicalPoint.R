
atypicalPointUI <- function(id){
      ns <- NS(id)
      tagList(
            sidebarPanel(
                  selectInput(inputId = ns("method"),"Select Method : ", 
                              choices = list(
                                "Studentized Residual" = "stud.residual",
                                "Lever" = "lever"),
                              selected = "st.residual"
                  ),
                  tags$hr(),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'stud.residual'", ns("method")),
                    checkboxGroupInput(inputId = ns("stud.display"), "Choose that you would like to show : ",
                                       choices = list(
                                         "Critical threshold" = "stud.threshold",
                                         "Atypical and Influential points list" = "stud.list",
                                         "Visualization" = "stud.visualization"),
                                       selected = c("stud.threshold")
                    )
                  ),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'lever'", ns("method")),
                    checkboxGroupInput(inputId = ns("lever.display"), "Choose that you would like to show : ",
                                       choices = list(
                                         "Critical threshold" = "lever.threshold",
                                         "Atypical and Influential points list" = "lever.list"),
                                       selected = c("lever.threshold")
                    )
                  ),
            width = 2 # Adjust the sidebar width (default is 4)
            ),
            mainPanel(
              conditionalPanel(
                condition = sprintf("input['%s'] == 'stud.residual'", ns("method")),
                title = "Result",
                uiOutput(ns("stud.result"))
                ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'lever'", ns("method")),
                title = "Result",
                uiOutput(ns("lever.result"))
              ),
            width = 7 # Adjust the sidebar width (default is 8) 
            )
            
      )
  
}



atypicalPointServer <- function(id, data, regressionResult){
  moduleServer(id, function(input, output, session){
        ns <- session$ns
        
# Server for the atypical and the influential points in the sense of the studentized residual
        # Reactive expression to extract numeric variable names
        numericVars <- reactive({
          req(data())
          numeric_cols <- names(data())[sapply(data(), is.numeric)]
          numeric_cols
        })
      
        # Generate atypical and influential points result dynamically
        output$stud.result <- renderUI({
          selected.print<- input$stud.display
          info_list <- list()
          # Add critical threshold
          if ("stud.threshold" %in% selected.print) {
            info_list <- append(info_list, list(
              h4("Critical Threshold (with 10% of risk): "),
              verbatimTextOutput(ns("stud.value"))
            ))
          }
          # Add list of atypical and influential points
          if ("stud.list" %in% selected.print) {
            info_list <- append(info_list, list(
              tags$hr(),
              h4("List of the apytical and the influential points : "),
              DTOutput(ns("stud.table"))
            ))
          }
          # Add visualization of the atypical and influential points
          if ("stud.visualization" %in% selected.print){
            info_list <- append(info_list, list(
              tags$hr(),
              h4("Visualization of the points: "),
              selectInput(inputId = ns("numericalVar"), 
                          "Select Variable:", 
                          choices = numericVars(), 
                          selected = NULL),
              plotOutput(ns("stud.visualized"), width = "600px", height = "400px"),
              h5("Note that the atypical and the influential points are in the under and the over of the red line.")
            ))
          }
          
          do.call(tagList, info_list)
        })
        
        # Reactive outputs for residual analysis result
        output$stud.value <- renderPrint({
          req(data(), regressionResult())
          summary <- summary(regressionResult())
          # p = number of the explicative  variables
          p <- length(rownames(summary$coefficients))-1
          studThreshold <- function(alpha, n , p){
            threshold = qt(1-alpha/2, n-p-2)
            return(threshold)
          }
          # Threshold with 10% of risk
          stud.threshold <- studThreshold(0.1, nrow(data()), p)
          stud.threshold
        })
        output$stud.table <- renderDT({
          req(regressionResult(), data())
          summary <- summary(regressionResult())
          # p = number of the explicative  variables
          p <- length(rownames(summary$coefficients))-1
          # Studentized residual of the regression
          studentized.res <- rstudent(regressionResult())
          # Critical threshold for the studentized residual with risk alpha
          studThreshold <- function(alpha, n , p){
            threshold = qt(1-alpha/2, n-p-2)
            return(threshold)
          }
          # Threshold with 10% of risk
          stud.threshold <- studThreshold(0.1, nrow(data()), p)
          atipycal.rstudent <- (studentized.res < -stud.threshold | studentized.res > +stud.threshold)
          atypical.influential.points <- data()[atipycal.rstudent,]
          atypical.influential.points
        })
        output$stud.visualized <- renderPlot({
          req(data(), regressionResult(), input$numericalVar)
          summary <- summary(regressionResult())
          # p = number of the explicative  variables
          p <- length(rownames(summary$coefficients))-1
          # Critical threshold for the studentized residual with risk alpha
          studThreshold <- function(alpha, n , p){
            threshold = qt(1-alpha/2, n-p-2)
            return(threshold)
          }
          # Threshold with 10% of risk
          stud.threshold <- studThreshold(0.1, nrow(data()), p)
          summary <- summary(regressionResult())
          ggplot(data(), aes_string(x = input$numericalVar, y = summary$residuals)) +
            geom_point(color = "blue") +
            geom_hline(yintercept = 0, linetype = "solid", color = "black") +
            geom_hline(yintercept = -stud.threshold, linetype = "solid", color = "red") +
            geom_hline(yintercept = +stud.threshold, linetype = "solid", color = "red") +
            labs(
              title = paste("residuals vs", input$numericalVar),
              x = input$numericalVar,
              y = "residuals"
            ) +
            theme_minimal()
        })
        
# Server for the atypical and the influential points in the sense of the lever
        # Generate atypical and influential points result dynamically
        output$lever.result <- renderUI({
          selected.print<- input$lever.display
          
          info_list <- list()
          # Add critical threshold
          if ("lever.threshold" %in% selected.print) {
            info_list <- append(info_list, list(
              h4("Critical Threshold : "),
              verbatimTextOutput(ns("lever.value"))
            ))
          }
          # Add list of atypical and influential points
          if ("lever.list" %in% selected.print) {
            info_list <- append(info_list, list(
              tags$hr(),
              h4("List of the apytical and the influential points : "),
              DTOutput(ns("lever.list"))
            ))
          }
          
          do.call(tagList, info_list)
        })
        
        # Reactive outputs for the atypical and the influential points in sense of the lever
        output$lever.value <- renderPrint({
          req(data(), regressionResult())
          summary <- summary(regressionResult())
          # p = number of the explicative  variables
          p <- length(rownames(summary$coefficients))-1
          # Threshold of lever
          lever.threshold <- 2*(p+1)/nrow(data())
          lever.threshold
        })
        output$lever.list <- renderDT({
          req(regressionResult(), data())
          model <- regressionResult()
          req(data(), regressionResult())
          summary <- summary(regressionResult())
          # p = number of the explicative  variables
          p <- length(rownames(summary$coefficients))-1
          # lever
          indicator <- influence.measures(model)
          # lever columns
          lever <- indicator$infmat[,"hat"]
          # Threshold of lever
          lever.threshold <- 2*(p+1)/nrow(data())
          atipycal.lever <- (lever > lever.threshold)
          atypical.influential.points <- data()[atipycal.lever,]
          atypical.influential.points
        })
        
  })

}
