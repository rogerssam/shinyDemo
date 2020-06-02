library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(shinyWidgets)
library(ggplot2)
# library(data.table)
library(rhandsontable)
library(lubridate)
source("R/upload.R")

# Define UI for application
ui <- shinyUI(dashboardPagePlus(
    dashboardHeaderPlus(title = tagList(
        span(class = "logo-lg", "Shiny Demo", style = "padding: 0;"), 
        img(src = "BH_square.jpg"))),
    # enable_preloader = TRUE,
    # loading_duration = 3,
    
    dashboardSidebar(collapsed = TRUE,# width = 300,
                     sidebarMenu(id = "tabs",
                                 menuItem(tabName = "upload", "Upload or Select Data", icon = icon("upload", class = "fa-lg")),
                                 menuItem(tabName = "plot", "Plot", icon = icon("chart-bar", class = "fa-lg")),
                                 menuItem(tabName = "table", "Table", icon = icon("table", class = "fa-lg")),
                                 menuItem(tabName = "report", "Generate a Report", icon = icon("file-alt", class = "fa-lg"))#,
                                 # menuItem(tabName = "bibtex", "Generate BibTex file", icon = icon("file-code", class = "fa-lg")),
                                 # div(class='footer',
                                 # menuItem(tabName = "support", "Help and support", icon = icon("question", class = "fa-lg"))
                                 # )
                     )
    ),
    
    dashboardBody(
        #Include the css
        tags$head(includeCSS("www/custom.css")),
        useShinyjs(),  # Include shinyjs
        useShinyalert(),  # Include shinyalert
        
        # Add scripts and favicons etc
        shiny::singleton(
            shiny::tags$head(
                tags$link(rel = "stylesheet", href = "styles.css"),
                tags$link(rel = "stylesheet", href = "snackbar.css"),
                tags$script(src="snackbar.js"),
                tags$link(rel = "shortcut icon", href = "favicon.ico"),
                tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "favicon.ico"),
                tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
                tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png")
            )
        ),
        
        tabItems(
            tabItem(tabName = "upload",
                    # hidden(
                    fluidRow(id = "entry",
                             column(width = 1),
                             column(width = 10,
                                    box(width = NULL, title = "Upload file", solidHeader = T, status = "primary",
                                        csvFileUI("datafile", "User data (.csv format)"),
                                        
                                        # Horizontal line ----
                                        tags$hr(),
                                        materialSwitch(
                                            inputId = "builtin",
                                            label = "Use built-in iris data", 
                                            value = TRUE,
                                            status = "success"
                                        )
                                        
                                        
                                        
                                        
                                        
                                    ), 
                                    # textInput("title", HTML(paste("Please enter your document title:", span("*", style="color:red"))), placeholder = "Document title"),
                                    # textInput("author", HTML(paste("Please enter at least one author of the document:", span("*", style="color:red"))), placeholder = "Author 1, Author 2, ..."),
                                    
                                    # div(class = "container-fluid",
                                    #     div(class = 'row',
                                    #         div(class = 'col-md', id = 'project_code', style="float: left; vertical-align:top; width: 49%; margin-right: 2%", 
                                    #             textInput("project_code", "Please enter a project code if relevant:", placeholder = "Project code (optional)")),
                                    #         div(class = 'col-md', id = "report_type", style="float: left; vertical-align:top; width: 49%;", 
                                    #             selectInput("report_type", "Please select the report type:", selectize = F, 
                                    #                         c("Analysis Report", "Design Report", "Technical Report", "Presentation", "Paper", "Poster", "Other"))
                                    #         )
                                    #     )
                                    # )
                                    
                                    
                                    # div(class = "container-fluid",
                                    #     div(class = 'row',
                                    #         div(class = 'col-md', id = 'related', style="float: left;vertical-align:top; width: 49%; margin-right: 1.5%", 
                                    #             pickerInput(
                                    #                 inputId = "related",
                                    #                 label = "Report number(s) of related reports (e.g. Design report):", 
                                    #                 choices = NA,
                                    #                 multiple = TRUE,
                                    #                 options = list(title = "Select or type (optional)",
                                    #                                `live-search` = TRUE)
                                    #             )),
                                    #         # selectizeInput(inputId = "related", label = "Report number(s) of related reports (e.g. Design report):", choices = NULL)),
                                    #         div(class = 'col-md', id = "keywords", style="float: right;vertical-align:top; width: 49%;", 
                                    #             textInput("keywords", "Keywords:", placeholder = "A few relevant keywords (optional)")
                                    #         )
                                    #     )
                                    # ),
                                    # div(class = "container-fluid",
                                    #     div(class = 'row',
                                    #         div(class = 'col-md', id = 'recipient', style="float: left;vertical-align:top; width: 49%; margin-right: 1.5%;", 
                                    #             textInput("recipient", "Recipient of this report:", placeholder = "Recipient name(s) or email(s) (optional)"),
                                    #             switchInput(
                                    #                 inputId = "sardi",
                                    #                 label = "SARDI Report", 
                                    #                 inline = TRUE,
                                    #                 labelWidth = "90px",
                                    #                 onStatus = "success",
                                    #                 onLabel = "Yes", offLabel = "No"
                                    #             )),
                                    #         div(class = 'col-md', id = "date", style="float: right;vertical-align:top; width: 49%;", 
                                    #             dateInput('date', value = Sys.Date(), "Date of report:", 
                                    #                       max = Sys.Date(), format = "MM yyyy", startview = "year")#,
                                    #             # p(), br(), 
                                    #             
                                    #             # prettySwitch(inputId = "sardi", label = "SARDI Report", status = "success", fill = TRUE, bigger = TRUE, inline = T)
                                    #         )
                                    #     )
                                    # ),
                                    # br(),
                                    # actionButton("submit", "Submit", align = 'centre', class="btn btn-disabled"),
                                    # span(actionButton("reset", "Reset Fields", align = 'centre'), HTML(paste0(span("*", style="color:red"), ": Required field")), style ="float: right")),
                                    # 
                                    # fluidRow(conditionalPanel(
                                    #     uiOutput("confirmation"), width = 12)),
                                    # br(),
                                    box(width = NULL,
                                        # title = textOutput("filetitle"), 
                                        solidHeader = T, status = "success",
                                        DT::dataTableOutput('contents')),
                                    column(width = 1)
                             )
                    )
            ),
            tabItem(tabName = "plot",
                    # hidden(
                    fluidRow(id = "search",
                             column(width = 2),
                             column(width = 8,
                                    box(width = NULL,
                                        title = "Plot the data", solidHeader = T, status = "success",
                                        selectInput("xval", "X values", choices = ""),
                                        selectInput("yval", "Y values", choices = ""),
                                        selectInput("colours", "Colour by", choices = ""),
                                        selectInput("type", "Plot type", choices = c("Scatter plot", "Box plot", "Bar plot")),
                                        selectInput("theme", "Plot theme", choices = c("Default", "Black and White" = "bw", "Classic", "Minimal", "Dark"), selected = "bw"),
                                        plotOutput("plot"))
                             ),
                             column(width=2)
                    )
            ),
            tabItem(tabName = "table",
                    # hidden(
                    fluidRow(id = "update_cols", 
                             column(width = 12,
                                    box(width = NULL,
                                        title = "Edit the column types", solidHeader = T, status = "info",
                                        div(class = "container-fluid",
                                            div(class = 'row',
                                                div(class = 'col-md', id = 'select_col', style="float: left; vertical-align:top; width: 49%; margin-right: 2%",
                                                    selectInput("col_select", label = "Select Column", choices = NULL)),
                                                div(class = 'col-md', id = "select_type", style="float: left; vertical-align:top; width: 49%;",
                                                    selectInput("col_type", label = "Select new column type", choices = c("Choose" = "", "Numeric", "Date", "Text", "Factor"))
                                                )
                                            )
                                        )
                                    )
                             )
                    ),
                    fluidRow(id = "tab",
                             column(width = 12,
                                    box(width = NULL,
                                        title = "Edit the data", solidHeader = T, status = "success",
                                        rhandsontable::rHandsontableOutput("rhtable", height = 400))
                             )
                    )
            )#,
            # tabItem(tabName = "update",
            #         # hidden(
            #         fluidRow(id = "update",
            #                  column(width = 2),
            #                  column(width = 8,
            #                         box(width = NULL,
            #                             title = "Update a Report", solidHeader = T, status = "primary")
            #                  ),
            #                  column(width=2)
            #         )
            #         # )
            # ),
            # tabItem(tabName = "bibtex",
            #         # hidden(
            #         fluidRow(id = "bibtex",
            #                  column(width = 2),
            #                  column(width = 8,
            #                         box(width = NULL, title = "Enter Report Number or Select Title", solidHeader = T, status = "primary",
            #                             div(class = "container-fluid",
            #                                 div(class = 'row',
            #                                     div(class = 'col-md', id = 'bibtex_no', style="float: left;vertical-align:top; width: 49%; margin-right: 1.5%;", 
            #                                         textInput("bibtex_no", "Enter Report Number:", placeholder = "Report Number")),
            #                                     div(class = 'col-md', id = "sel_title", style="float: right;vertical-align:top; width: 49%;", 
            #                                         pickerInput(
            #                                             inputId = "select_title",
            #                                             label = "Select a title (or type to narrow options)", 
            #                                             choices = NA,
            #                                             options = list(title = "Select or type title",
            #                                                            `live-search` = TRUE)
            #                                         ))
            #                                     # selectizeInput(inputId = "select_title", label = "Select a title (or start typing to narrow options)", choices = NULL))
            #                                 ),
            #                                 div(class = 'row',
            #                                     div(class = 'col-md', id = 'include_no',
            #                                         prettySwitch(inputId = "include_no", label = "Include Report Number in Bibtex Title", status = "success", fill = TRUE, bigger = TRUE)
            #                                     )
            #                                 )
            #                             ),
            #                             br(),
            #                             h4("You have selected"),
            #                             DT::dataTableOutput('report'),
            #                             br(),
            #                             # h4("The Bibtex entry for your selected report is"),
            #                             # uiOutput('bibtex'),
            #                             # br()#,
            #                             downloadButton('download')
            #                         )      
            #                  ),
            #                  column(width=2)
            #                  # )
            #         )
            # ),
            # tabItem(tabName = "support",
            #         # hidden(
            #         fluidRow(id = "support",
            #                  column(width = 2),
            #                  column(width = 8,
            #                         box(width = NULL, title = "Help and Support", solidHeader = TRUE,
            #                             status = "danger",
            #                             h4("For help contact ", a(href="mailto:s.rogers@adelaide.edu.au", "Sam Rogers"))
            #                         )
            #                  ),
            #                  column(width=2)
            #         )
            #         # )
            # )
        )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    rvs <- reactiveValues(data_table = NULL, data = NULL)
    
    # callModule(uploadServer, "file")
    datafile <- callModule(csvFileServer, "datafile",
                           stringsAsFactors = FALSE)
    
    
    output$contents <- DT::renderDataTable({
        if(input$builtin) {
            rvs$data_table <- DT::datatable(iris, rownames = F, extensions = "Responsive", plugins = 'natural',
                                            options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
                                                           pageLength = 3, scrollX = TRUE))
            rvs$data <- iris
        }
        else {
            rvs$data_table <- DT::datatable(datafile(), rownames = F, extensions = "Responsive", plugins = 'natural',
                                            options = list(lengthMenu = list(c(3, 10, -1), c('3', '10', 'All')),
                                                           pageLength = 3, scrollX = TRUE))
            rvs$data <- datafile()
        }
        return(rvs$data_table)
    })
    
    observeEvent(rvs$data, {
        updateSelectInput(session, "xval", choices=colnames(rvs$data))
        updateSelectInput(session, "yval", choices=colnames(rvs$data), selected = colnames(rvs$data)[2])
        updateSelectInput(session, "colours", choices=c("None", colnames(rvs$data)), selected = "None")
    })
    
    output$plot <- renderPlot({
        selected_colour <- NULL
        selected_fill <- NULL
        
        if(!is.na(input$colours) | !is.null(input$colours)) {
            if(input$colours == "None" | input$colours == "") {
                selected_colour <- NULL
                selected_fill <- NULL
            }
            else {
                selected_colour <- input$colours
                selected_fill <- input$colours
            }
        }
        
        p <- ggplot(rvs$data, aes_string(input$xval, input$yval, 
                                         colour = selected_colour,
                                         fill = selected_fill))
        
        switch(input$type,
               "Scatter plot" = p <- p + geom_point(),
               "Box plot" = p <- p + geom_boxplot(aes_string(fill = selected_colour, colour = NULL, group = selected_colour)),
               "Bar plot" = p <- p + geom_bar(stat = "identity", aes_string(fill = selected_colour, colour = NULL))#,
        )
        
        switch(input$theme,
               Default = p + theme_gray(),
               bw = p + theme_bw(),
               Classic = p + theme_classic(),
               Minimal = p + theme_minimal(),
               Dark = p + theme_dark()
        )
        
    })
    
    output$rhtable <-  rhandsontable::renderRHandsontable({
        rhandsontable::rhandsontable(rvs$data, stretchH = "all") %>% 
            hot_cols(columnSorting = T)
    })
    
    observeEvent(input$rhtable, {
        rvs$data <- hot_to_r(input$rhtable)
    })
    
    observe({
        updateSelectInput(session, inputId = "col_select", choices = colnames(rvs$data))
    })
    
    observeEvent(input$col_type, {
        if(input$col_type == "Numeric") {
            try(rvs$data[[input$col_select]] <- as.numeric(rvs$data[[input$col_select]]))
        }
        if(input$col_type == "Date") {
            tryCatch(
                {
                    rvs$data[[input$col_select]] <- as.Date(lubridate::parse_date_time(rvs$data[[input$col_select]], c("dmy", "mdy", "ymd")))
                }, 
                warning = function(w) {
                    print("warning")
                }, 
                error = function(e) {
                    print("error")
                }
            )
        }
        
        if(input$col_type == "Text")  {
            try(rvs$data[[input$col_select]] <- as.character(rvs$data[[input$col_select]]))
        }
        if(input$col_type == "Factor") {
            try(rvs$data[[input$col_select]] <- factor(rvs$data[[input$col_select]]))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
