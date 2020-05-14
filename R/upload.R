#Upload CSV module


uploadUI <- function(id) {
    list(
        fileInput(NS(id, "upload"), "Choose CSV File", 
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        checkboxInput(
            NS(id, "header"),
            label = "Header", 
            value = TRUE#,
            # status = "primary",
            # icon = icon("check"), 
            # plain = TRUE,
            # outline = TRUE
        ),
        radioButtons(
            NS(id, "sep"),
            label = "Separator:", 
            choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
            selected = ",",
            inline = TRUE#, 
            # status = "primary",
            # animation = "pulse",
            # icon = icon("check")
        ),
        # Input: Select quotes ----
        radioButtons(NS(id, "quote"), "Quote Type",
                     choices = c(None = "",
                                 "Double Quote" = '"',
                                 "Single Quote" = "'"),
                     selected = '"'),
    )
}

prettyuploadUI <- function(id) {
    list(
        fileInput(NS(id, "upload"), "Choose CSV File", 
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        prettyCheckbox(
            NS(id, "header"),
            label = "Header", 
            value = TRUE,
            status = "primary",
            icon = icon("check"), 
            plain = TRUE,
            outline = TRUE
        ),
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
}

uploadServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$contents <- DT::renderDataTable({
            
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.
            if(input$builtin) {
                df <- DT::datatable(iris, rownames = F, extensions = "Responsive", plugins = 'natural',
                                    options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
                                                   pageLength = 3, scrollX = TRUE))
                output$filetitle <- renderText("Using iris data")
                return(df)
            }
            else {
                
                req(input$file1)
                
                output$filetitle <- renderText(paste0("Uploaded ", input$file1$name))
                # when reading semicolon separated files,
                # having a comma separator causes `read.csv` to error
                tryCatch(
                    {
                        dat <- read.csv(input$file1$datapath,
                                        header = input$header,
                                        sep = input$sep,
                                        quote = input$quote)
                        
                        df <- DT::datatable(dat, rownames = F, extensions = "Responsive", plugins = 'natural',
                                            options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
                                                           pageLength = 3, scrollX = TRUE))
                    },
                    error = function(e) {
                        # return a safeError if a parsing error occurs
                        stop(safeError(e))
                    }
                )
            }
            
            # if(input$disp == "head") {
            #     return(head(df$x$data))
            # }
            # else {
            #     return(df$x$data)
            # }
            
        })
        
        data <- reactive(mtcars[[input$var]])
        output$hist <- renderPlot({
            hist(data(), breaks = input$bins, main = input$var)
        }, res = 96)
    })
}