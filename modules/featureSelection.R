
featureSelectionUI <- function(id){
  ns <- NS(id)
  tagList(
    sidebarPanel(
      selectInput(inputId = ns("model"),"Select Model: ", 
                  choices = list(
                    "Linear Regression" = "linReg",
                    "Logistic Regression" = "logReg"),
                  selected = "linReg"
      ),
      tags$hr(),
      selectInput(inputId = ns("method"), "Select feature selection method : ",
                  choices = list(
                    "AIC criterion (Akaike)" = "aic",
                    "BIC criterion (Schwartz)" = "bic"),
                  selected = "aic"
      ),
      tags$hr(),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'linReg'", ns("model")),
        checkboxGroupInput(inputId = ns("lingReg.display"), "Choose that you would like to show : ",
                           choices = list(
                             "Selectioned features" = "feature"),
                           selected = c("feature")
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'logReg'", ns("model")),
        checkboxGroupInput(inputId = ns("logReg.display"), "Choose that you would like to show : ",
                           choices = list(
                             "Selectioned features" = "feature"),
                           selected = c("feature")
        )
      ),
      width = 2 # Adjust the sidebar width (default is 4)
    ),
    mainPanel(
      conditionalPanel(
        condition = sprintf("input['%s'] == 'linReg'", ns("model")),
        title = "Result",
        uiOutput(ns("selectedFeature.linReg"))
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'logReg'", ns("model")),
        title = "Result",
        uiOutput(ns("selectedFeature.logReg"))
      ),
      width = 7 # Adjust the sidebar width (default is 8) 
    )
  )
}

featureSelectionServer <- function(id, data, model){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
# Server for the feature selection with the AIC criterion method
    # Generate feature selection AIC method result dynamically
    output$selectedFeature.linReg <- renderUI({
      selected.print <- input$lingReg.display
      select.method <- input$method
      info_list <- list()
      # Add feature selection with AIC criterion
      if ("feature" %in% selected.print && "aic" %in% select.method ) {
        info_list <- append(info_list, list(
          h4("Selected Features with AIC creterion: "),
          DTOutput(ns("selected.aic"))
        ))
      }
      # Add feature selection with AIC criterion
      if ("feature" %in% selected.print && "bic" %in% select.method ) {
        info_list <- append(info_list, list(
          h4("Selected Features with BIC creterion: "),
          DTOutput(ns("selected.bic"))
        ))
        
      }
      do.call(tagList, info_list)
    })
    
    # Reactive outputs for feature selection AIC method result
    output$selected.aic <- renderDT({
      req(data(), model())
      model_back <- stepAIC(model(), data = data(), direction="backward")
      feat.list <- sapply(tail(names(model_back$coefficients), length(model_back$coefficients-1)), function(x){substr(x, 1, nchar(x)-1)})
      feat.list <- feat.list[2: length(feat.list)]
      names(feat.list)<-NULL
      as.data.frame(feat.list)
    })
    
    # Reactive outputs for feature selection BIC method result
    output$selected.bic <- renderDT({
      req(data(), model())
      model_back <- stepAIC(model(), data = data(), direction="backward", k =log(nrow(data())))
      feat.list <- sapply(tail(names(model_back$coefficients), length(model_back$coefficients-1)), function(x){substr(x, 1, nchar(x)-1)})
      feat.list <- feat.list[2: length(feat.list)]
      names(feat.list)<-NULL
      as.data.frame(feat.list)
    })
    
    
  })
}