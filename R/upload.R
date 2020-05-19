#Upload CSV module



csvFileInput <- function(id, label = "CSV file") {
    # Create a namespace function using the provided id
    ns <- NS(id)
    
    tagList(
        fileInput(ns("file"), label),
        div(class = "container-fluid",
            div(class = 'row',
                div(class = 'col-md-4', id = 'project_code', style="float: left; vertical-align:top; width: 24%; margin-right: 2%",
                    br(),
                    prettyCheckbox(
                        NS(id, "header"),
                        label = "Header", 
                        value = TRUE,
                        status = "primary",
                        icon = icon("check"), 
                        plain = TRUE,
                        animation = "pulse",
                        outline = TRUE
                    )
                ),
                div(class = 'col-md-8', id = "report_type", style="float: left; vertical-align:top; width: 74%;",
                    prettyRadioButtons(
                        NS(id, "sep"),
                        label = "Separator:", 
                        choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                        selected = ",",
                        inline = TRUE, 
                        status = "primary",
                        animation = "pulse",
                        icon = icon("check")
                    )
                )
            )
        ),
        # checkboxInput(ns("heading"), "Has heading"),
        # selectInput(ns("quote"), "Quote", c(
        #     "None" = "",
        #     "Double quote" = "\"",
        #     "Single quote" = "'"
        # ))
    )
}


# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
    # The selected file, if any
    userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$file, message = FALSE))
        input$file
    })
    
    # The user's data, parsed into a data frame
    dataframe <- reactive({
        read.csv(userFile()$datapath,
                 header = input$header,
                 # quote = input$quote,
                 sep = input$sep,
                 stringsAsFactors = stringsAsFactors)
    })
    
    # We can run observers in here if we want to
    observe({
        msg <- sprintf("File %s was uploaded", userFile()$name)
        cat(msg, "\n")
    })
    
    # Return the reactive that yields the data frame
    return(dataframe)
}


# uploadUI <- function(id) {
#     tagList(
#         fileInput(NS(id, "upload"), "Choose CSV File", 
#                   multiple = FALSE,
#                   accept = c("text/csv",
#                              "text/comma-separated-values,text/plain",
#                              ".csv")),
#         checkboxInput(
#             NS(id, "header"),
#             label = "Header", 
#             value = TRUE#,
#             # status = "primary",
#             # icon = icon("check"), 
#             # plain = TRUE,
#             # outline = TRUE
#         ),
#         radioButtons(
#             NS(id, "sep"),
#             label = "Separator:", 
#             choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
#             selected = ",",
#             inline = TRUE#, 
#             # status = "primary",
#             # animation = "pulse",
#             # icon = icon("check")
#         ),
#         # Input: Select quotes ----
#         radioButtons(NS(id, "quote"), "Quote Type",
#                      choices = c(None = "",
#                                  "Double Quote" = '"',
#                                  "Single Quote" = "'"),
#                      selected = '"'),
#     )
# }
# 
# prettyuploadUI <- function(id) {
#     list(
#         fileInput(NS(id, "upload"), "Choose CSV File", 
#                   multiple = FALSE,
#                   accept = c("text/csv",
#                              "text/comma-separated-values,text/plain",
#                              ".csv")),
#         prettyCheckbox(
#             NS(id, "header"),
#             label = "Header", 
#             value = TRUE,
#             status = "primary",
#             icon = icon("check"), 
#             plain = TRUE,
#             outline = TRUE
#         ),
#         prettyRadioButtons(
#             NS(id, "sep"),
#             label = "Separator:", 
#             choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
#             selected = ",",
#             inline = TRUE, 
#             status = "primary",
#             animation = "pulse",
#             icon = icon("check")
#         )
#     )
# }
# 
# # uploadServer <- function(id) {
# uploadServer <- function(input, output, session) {
#     # moduleServer(id, function(input, output, session) {
#     output$contents <- DT::renderDataTable({
#         
#         # input$file1 will be NULL initially. After the user selects
#         # and uploads a file, head of that data file by default,
#         # or all rows if selected, will be shown.
#         if(input$builtin) {
#             df <- DT::datatable(iris, rownames = F, extensions = "Responsive", plugins = 'natural',
#                                 options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
#                                                pageLength = 3, scrollX = TRUE))
#             output$filetitle <- renderText("Using iris data")
#             return(df)
#         }
#         else {
#             
#             req(input$upload)
#             
#             output$filetitle <- renderText(paste0("Uploaded ", input$upload$name))
#             # when reading semicolon separated files,
#             # having a comma separator causes `read.csv` to error
#             tryCatch(
#                 {
#                     dat <- read.csv(input$upload$datapath,
#                                     header = input$header,
#                                     sep = input$sep,
#                                     quote = input$quote)
#                     
#                     df <- DT::datatable(dat, rownames = F, extensions = "Responsive", plugins = 'natural',
#                                         options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
#                                                        pageLength = 3, scrollX = TRUE))
#                 },
#                 error = function(e) {
#                     # return a safeError if a parsing error occurs
#                     stop(safeError(e))
#                 }
#             )
#         }
#         
#         # if(input$disp == "head") {
#         #     return(head(df$x$data))
#         # }
#         # else {
#         #     return(df$x$data)
#         # }
#         
#     })
#     
#     # data <- reactive(mtcars[[input$var]])
#     # output$hist <- renderPlot({
#     #     hist(data(), breaks = input$bins, main = input$var)
#     # }, res = 96)
#     # })
# }