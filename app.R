library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyalert)
library(shinyWidgets)

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
                                 menuItem(tabName = "table", "Table", icon = icon("table", class = "fa-lg"))#,
                                 # menuItem(tabName = "update", "Update a Report", icon = icon("edit", class = "fa-lg")),
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
        
        # source("sof-auth/sign-in.R", local = TRUE)$value,
        # source("sof-auth/register.R", local = TRUE)$value,
        # source("sof-auth/verify-email.R", local = TRUE)$value,
        
        tabItems(
            tabItem(tabName = "upload",
                    # hidden(
                    fluidRow(id = "entry",
                             column(width = 2),
                             column(width = 8,
                                    box(width = NULL, title = "Upload file", solidHeader = T, status = "primary",
                                        # Input: Select a file ----
                                        fileInput("file1", "Choose CSV File",
                                                  multiple = FALSE,
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv")),
                                        
                                        # Horizontal line ----
                                        tags$hr(),
                                        
                                        # Input: Checkbox if file has header ----
                                        checkboxInput("header", "Header", TRUE),
                                        
                                        # Input: Select separator ----
                                        radioButtons("sep", "Separator",
                                                     choices = c(Comma = ",",
                                                                 Semicolon = ";",
                                                                 Tab = "\t"),
                                                     selected = ","),
                                        
                                        # Input: Select quotes ----
                                        radioButtons("quote", "Quote",
                                                     choices = c(None = "",
                                                                 "Double Quote" = '"',
                                                                 "Single Quote" = "'"),
                                                     selected = '"'),
                                        
                                        # Horizontal line ----
                                        tags$hr(),
                                        
                                        # Input: Select number of rows to display ----
                                        radioButtons("disp", "Display",
                                                     choices = c(Head = "head",
                                                                 All = "all"),
                                                     selected = "head")
                                        
                                        # ),
                                        
                                        
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
                                        title = "Uploaded data", solidHeader = T, status = "success",
                                        DT::dataTableOutput('contents')),
                                    column(width = 2)
                             )
                             # )
                    )
            ),
            tabItem(tabName = "table",
                    # hidden(
                    fluidRow(id = "search",
                             column(width = 2),
                             column(width = 8,
                                    box(width = NULL,
                                        title = "Latest Report Submissions", solidHeader = T, status = "success",
                                        DT::dataTableOutput('full_table'))
                             ),
                             column(width=2)
                    )
                    # )
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
server <- function(input, output) {
    
    output$contents <- DT::renderDataTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
