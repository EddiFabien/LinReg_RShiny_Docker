

# Package import
library(shiny)
library(shinythemes)
library(readxl)  # For reading Excel files
library(DT)      # For interactive tables
library(dplyr)
library(ggplot2)
library(stats)
library(MASS)


# Module import
source("modules/fileUpload.R")
source("modules/correlation.R")
source("modules/linearRegression.R")
source("modules/residualAnalysis.R")
source("modules/atypicalPoint.R")
source("modules/featureSelection.R")


# Define ui function
ui <- fluidPage(theme = shinytheme("flatly"),
                navbarPage(
                    title = "Linear regression analysis", # Title of the navigation bar page
                    tabPanel(title = "File upload and Summaries",
                             fileUploadUI("upload")
                    ),
                    tabPanel("Correlation Analysis",
                             correlationUI("correlation")
                    ),
                    tabPanel("Linear regression",
                             linearRegressionUI("linreg")
                    ),
                    tabPanel("Residual analysis",
                             residualUI("res")
                    ),
                    tabPanel("Atypical and influential points detection",
                             atypicalPointUI("detection")
                    ),
                    tabPanel("Feature selection",
                               featureSelectionUI("selection")
                    )
                ) # navbarPage
        ) # fluidPage



# Define server function  
server <- function(input, output, session) {
# server for FILE UPLOAD and FILE SUMMARIES
          uploaded.data <- fileUploadServer("upload")

# server for CORRELATION ANALYSIS          
          correlationServer("correlation", data = uploaded.data)
          
# server for LINEAR REGRESSION         
          regressionModel <- linearRegressionServer("linreg", data = uploaded.data)
          
          
# server for RESIDUAL ANALYSIS
          residualServer("res", data = uploaded.data, model = regressionModel)
          
# server for DETECTION OF ATYPICAL AND INFLUENTIAL POINTS
          atypicalPointServer("detection", data = uploaded.data, model = regressionModel)

# server for DETECTION OF ATYPICAL AND INFLUENTIAL POINTS
          featureSelectionServer("selection", data = uploaded.data, model = regressionModel)
          
}


# Create shiny object
shinyApp(ui, server)
