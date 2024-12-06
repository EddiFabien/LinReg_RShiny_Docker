# File upload UI
fileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarPanel(
        radioButtons(inputId = ns("fileType"), "File Type :",
                     choices = c("CSV" = "csv",
                                 "Excel" = "excel",
                                 "Text" = "txt"),
                     selected = "excel"),
        tags$hr(),
        fileInput(inputId = ns("file"), "Upload file:",
                  accept = c(".csv", ".xlsx", ".txt")),
        
        tags$hr(),
        checkboxInput(inputId = ns("header"), "Header", TRUE),
        tags$hr(),
        radioButtons(ns("encoding"), "Encoding :",
                     choices = c(Utf8 = "utf-8",
                                 Other = "other"),
                     selected = "utf-8"),
        conditionalPanel(condition = sprintf("input['%s'] == 'txt'", ns("fileType")),
                         radioButtons(ns("sep1"), "Choose Separator :", 
                                      choices = list("Comma (,)" = ",",
                                                     "Semicolon (;)" = ";",
                                                     "Tab" = "\t"),
                                      selected = "\t")
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'csv'", ns("fileType")),
                         radioButtons(ns("sep2"), "Choose Separator :", 
                                      choices = list("Comma (,)" = ",",
                                                     "Semicolon (;)" = ";",
                                                     "Tab" = "\t"),
                                      selected = ";")
        ),
        conditionalPanel(condition = sprintf("input['%s'] == 'excel'", ns("fileType")),
                         numericInput(ns("sheet"), "Sheet Number :", 
                                      value = 1, 
                                      min = 1, 
                                      step = 1),
                         textOutput(ns("sheetWarning"))  # Display warning if sheet number is invalid
        ),
        tags$hr(),
        checkboxGroupInput(inputId = ns("info"), "Choose Complementary Informations :",
                           choices = list(
                             "Dimension (Rows & Columns)" = "dimension",
                             "Column Names (Labels)" = "columns",
                             "Characteristics of the data" = "str",
                             "Summary statistics" = "summary"),
                           selected = c("dimension")),
        width = 2 # Adjust the sidebar width (default is 4)
  ),
  mainPanel(
    tabsetPanel(
      tabPanel(title = "Data Table", DTOutput(ns("table"), width = "100%", height = "600px")), # Output title and output Id
      tabPanel(title = "Complementary Informations", uiOutput(ns("complementaryInfo"))) # The same of above
    ),
    width = 7  # Adjust the main panel width (default is 8)
  ))
}


# File upload Server
fileUploadServer <- function(id) {
      moduleServer(id, function(input, output, session) {
            ns <- session$ns
            
            # Reactive expression to read the uploaded file
            data <- reactive({
                  req(input$file)  # Ensure a file is uploaded
                  ext <- tools::file_ext(input$file$name)  # Get the file extension
                  
                  tryCatch({
                        if (input$fileType == "csv") {
                              df <- read.csv(input$file$datapath, header = input$header, sep = input$sep2, stringsAsFactors = TRUE)
                        } else if (input$fileType == "txt") {
                              df <- read.table(input$file$datapath, header = input$header, sep = input$sep1, stringsAsFactors = TRUE)
                        } else if (input$fileType == "excel") {
                              validate(need(input$sheet > 0, "Sheet number must be greater than 0."))
                              df <- readxl::read_xlsx(input$file$datapath, sheet = as.numeric(input$sheet), stringsAsFactors = TRUE)
                        } else {
                              NULL}
                  }, 
                  error = function(e) {
                        showNotification(paste("Error reading file:", e$message), type = "error")
                        return(NULL)
                  }
                  )
                  names(df) <- make.names(names(df), unique = TRUE)  # Clean column names
                  df
                })
                
            # Reactive expression to get Excel sheet names
            sheet_names <- reactive({
                  req(input$fileType == "excel")
                  tryCatch(
                    readxl::excel_sheets(input$file$datapath),
                    error = function(e) { return(NULL) }
                  )
            })
            
            # Validation and warnings for sheet selection
            output$sheetWarning <- renderText({
                  req(input$fileType == "excel", sheet_names())
                  if (!is.null(sheet_names())) {
                    max_sheets <- length(sheet_names())
                    if (input$sheet > max_sheets || input$sheet < 1) {
                      paste("Invalid sheet number. Please enter a number between 1 and", max_sheets)
                    } else {
                      ""
                    }
                  }
            })
            
            # Render the data table
            output$table <- renderDT({
                  req(data())
                  datatable(data(),
                            datatable(data(),options = list(
                              scrollX = TRUE,  # Enable horizontal scrolling
                              scrollY = "500px",  # Enable vertical scrolling
                              paging = FALSE,  # Disable pagination to show all rows
                              autoWidth = TRUE  # Automatically adjust column widths
                            ))
                  )
            })
            
            # Generate complementary information dynamically
            output$complementaryInfo <- renderUI({
                  req(data())
                  selected_info <- input$info
                  info_list <- list()
                  if ("dimension" %in% selected_info) {
                    info_list <- append(info_list, list(h4("Dimensions:"), textOutput(ns("nrow")), textOutput(ns("ncol"))))
                  }
                  if ("columns" %in% selected_info) {
                    info_list <- append(info_list, list(tags$hr(), h4("Column Names:"), DTOutput(ns("columns"))))
                  }
                  if ("str" %in% selected_info) {
                    info_list <- append(info_list, list(tags$hr(), h4("Data Structure:"), verbatimTextOutput(ns("str"))))
                  }
                  if ("summary" %in% selected_info) {
                    info_list <- append(info_list, list(tags$hr(), h4("Summary Statistics:"), verbatimTextOutput(ns("summaryStat"))))
                  }
                  do.call(tagList, info_list)
            })
            
            # Complementary information outputs
            output$nrow <- renderText({ paste("Rows:", nrow(data())) })
            output$ncol <- renderText({ paste("Columns:", ncol(data())) })
            output$columns <- renderDT({
                  req(data())
                  my_list <- list(Column_names = colnames(data()))
                  as.data.frame(my_list)
            })
            output$str <- renderPrint({ str(data()) })
            output$summaryStat <- renderPrint({ summary(data()) })
            
            # Return the raw data to the parent module to use in another function
            return(data)
      })
}
