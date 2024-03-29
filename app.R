library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(waiter)
library(shinyWidgets)
library(ggplot2)
library(rhandsontable)
library(lubridate)
library(Cairo)
library(plotly)
source("R/upload.R")

options(shiny.usecairo=T)
shinyOptions(plot.autocolors = TRUE)

# Define UI for application
ui <- shinyUI(shinydashboardPlus::dashboardPage(
    shinydashboardPlus::dashboardHeader(title = tagList(
    span(class = "logo-lg", "Shiny Demo", style = "padding: 0;"), 
    img(src = "BH_square.jpg")), 
    controlbarIcon = shiny::icon("cogs")  # Fix for incorrect icon error
  ),
  preloader = list(html = spin_loaders(16, color = "#12e050")),
  
  dashboardSidebar(collapsed = TRUE,# width = 300,
                   sidebarMenu(id = "tabs",
                               menuItem(tabName = "upload", "Upload or Select Data", icon = icon("upload", class = "fa-lg")),
                               menuItem(tabName = "plot", "Plot", icon = icon("chart-bar", class = "fa-lg")),
                               menuItem(tabName = "table", "Table", icon = icon("table", class = "fa-lg")),
                               menuItem(tabName = "report", "Generate a Report", icon = icon("file-alt", class = "fa-lg")),
                               menuItem(tabName = "about", "About This App", icon = icon("info-circle", class = "fa-lg"))
                   )
  ),
  
  dashboardBody(
    #Include the css
    tags$head(includeCSS("www/custom.css")),
    useShinyjs(),  # Include shinyjs
    useSweetAlert(),
    
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
              fluidRow(id = "entry",
                       column(width = 1),
                       column(width = 10,
                              box(width = NULL, title = "Upload file", solidHeader = T, status = "primary",
                                  HTML('<div class="alert alert-warning alert-dismissible" role="alert">
  <button type="button" class="close" data-dismiss="alert" aria-label="Close"><span aria-hidden="true">&times;</span></button>
  <strong>Please Note:</strong> Any data uploaded to this app is not retained in any way once the app is closed, except through any reports you have downloaded to your computer.
                                             </div>'),
                                  
                                  csvFileUI("datafile", "User data (.csv format)"),
                                  
                                  # Horizontal line ----
                                  tags$hr(),
                                  materialSwitch(
                                    inputId = "builtin",
                                    label = span("Use built-in", em(strong("iris")), "data"), 
                                    value = TRUE,
                                    status = "success"
                                  )
                                  
                              ), 
                              
                              box(width = NULL,
                                  solidHeader = T, status = "success",
                                  DT::dataTableOutput('contents')),
                              column(width = 1)
                       )
              )
      ),
      
      tabItem(tabName = "plot",
              fluidRow(id = "plot",
                       column(width = 12,
                              box(width = NULL,
                                  title = "Plot the data", solidHeader = T, status = "success",
                                  
                                  dropdown(
                                    tags$h3("Plot Options"),
                                    
                                    selectInput("xval", "X values", choices = ""),
                                    selectInput("yval", "Y values", choices = ""),
                                    selectInput("colours", "Colour by", choices = ""),
                                    selectInput("type", "Plot type", choices = c("Scatter plot", "Box plot", "Bar plot")),
                                    selectInput("theme", "Plot theme", choices = c("ggplot2 Default", "Black and White" = "bw", "Classic", "Minimal", "Dark"), selected = "bw"),
                                    sliderInput("size", "Point size", min = 1, max = 20, value = 4),
                                    materialSwitch(
                                      inputId = "interactive",
                                      label = "Interactive plot",
                                      value = FALSE,
                                      status = "success",
                                      width = '95%', 
                                      inline = T),
                                    
                                    style = "material-circle",
                                    icon = icon("cog"),
                                    status = "danger", width = "300px",
                                    animate = animateOptions(
                                      enter = animations$fading_entrances$fadeInLeftBig,
                                      exit = animations$fading_exits$fadeOutLeftBig
                                    ),
                                    tooltip = tooltipOptions(title = "Click to change plot options")
                                  ),
                                  conditionalPanel(
                                    condition = 'input.interactive == true',
                                    plotlyOutput('interactive_plot', height = "70vh")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = 'input.interactive == false',
                                    plotOutput('static_plot', height = "70vh")
                                  )
                              )
                       )
              )
      ),
      
      tabItem(tabName = "table",
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
                                  rhandsontable::rHandsontableOutput("rhtable", height = "60vh"))
                       )
              )
      ),
      
      tabItem(tabName = "report",
              # hidden(
              fluidRow(id = "report",
                       column(width = 1),
                       column(width = 10,
                              box(width = NULL, title = "Generate a Report", solidHeader = T, status = "primary",
                                  div(class = "container-fluid",
                                      div(class = 'row',
                                          div(class = 'col-md', id = 'title', style="float: left;vertical-align:top; width: 49%; margin-right: 1.5%;",
                                              textInput("title", "Enter Report Title:", placeholder = "Report Title")),
                                          div(class = 'col-md', id = "author", style="float: right;vertical-align:top; width: 49%;",
                                              textInput("author", "Enter Report Author:", placeholder = "Report Author"))
                                      )
                                  ),
                                  br(),
                                  actionBttn("generate", "Generate Report", 
                                             icon = icon("file-alt"), style = "fill", 
                                             color = "primary"),
                                  conditionalPanel(condition = "output.reportbuilt", id = "download_button", style="display: inline-block;",
                                                   downloadBttn("download", "Download now", style = "fill", color = "success"))
                              )
                       ),
                       column(width=1)
              )
      ),
      tabItem(tabName = "about",
              fluidRow(id = "about",
                       column(width = 2),
                       column(width = 8,
                              box(width = NULL, solidHeader = F,
                                  status = "danger",
                                  
                                  h3("Information about this app:"),
                                  "This application is a demonstration of some of the capabilities of the R shiny framework. It allows the user to upload a file (or use a built-in dataset), produce a plot, view and edit the data in an output table and then download an example report based on the input to the app. Any data uploaded to this app is not retained in any way once the app is closed, except through any reports you have downloaded to your computer.",
                                  
                                  h3("Packages:"),
                                  a(href="https://shiny.rstudio.com/", "shiny", .noWS = "after"), ": for the application", br(),
                                  a(href="https://rstudio.github.io/shinydashboard/", "shinydashboard", .noWS = "after"), ", ",
                                  a(href="https://rinterface.github.io/shinydashboardPlus/", "shinydashboardPlus", .noWS = "after"), ", ", 
                                  a(href="https://waiter.john-coene.com/#/", "waiter", .noWS = "after"), " and ", 
                                  a(href="https://dreamrs.github.io/shinyWidgets/index.html", "shinyWidgets", .noWS = "after"), ": to customise the appearance of the application", br(),
                                  a(href="https://ggplot2.tidyverse.org/", "ggplot2", .noWS = "after"), ": to produce the static plot in the", actionLink("switch_to_plot", "Plot"), "tab", br(),
                                  a(href="https://plotly.com/r/", "plotly", .noWS = "after"), ": to produce the interactive plot in the", actionLink("switch_to_plot", "Plot"), "tab", br(),
                                  a(href="http://www.rforge.net/Cairo/", "Cairo", .noWS = "after"), ": to produce higher resolution plots in the", actionLink("switch_to_plot", "Plot"), "tab", br(),
                                  a(href="https://jrowen.github.io/rhandsontable/", "rhandsontable", .noWS = "after"), ": for producing the editable table in the", actionLink("switch_to_table", "Table"), "tab", br(),
                                  a(href="https://lubridate.tidyverse.org/", "lubridate", .noWS = "after"), ": for translation of dates in the", actionLink("switch_to_table", "Table"), "tab", br(),
                                  
                                  h3("Author:"),
                                  "This app was developed by Sam Rogers.",
                                  br(), br(), 
                                  h3("More information:"),
                                  "For help with this app or requests to develop a Shiny application please contact", a(href="mailto:biometryhub@adelaide.edu.au", "The Biometry Hub"),
                                  br(),br(),
                                  actionBttn(
                                    inputId = "disclaimer",
                                    label = "Disclaimer", 
                                    style = "material-flat",
                                    color = "warning",
                                    icon = icon("exclamation-triangle"),
                                    size = "xs"
                                  )
                              )
                       ),
                       column(width=2)
              )
      )
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  rvs <- reactiveValues(data_table = NULL, data = NULL, plot = NULL, filepath = NULL)
  
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
  
  output$static_plot <- renderPlot({
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
           "Scatter plot" = p <- p + geom_point(size = input$size),
           "Box plot" = p <- p + geom_boxplot(aes_string(fill = selected_colour, colour = NULL, group = selected_colour)),
           "Bar plot" = p <- p + geom_bar(stat = "identity", aes_string(fill = selected_colour, colour = NULL))#,
    )
    
    switch(input$theme,
           "ggplot2 Default" = p <- p + theme_gray(base_size=16),
           bw = p <- p + theme_bw(base_size=16),
           Classic = p <- p + theme_classic(base_size=16),
           Minimal = p <- p + theme_minimal(base_size=16),
           Dark = p <- p + theme_dark(base_size=16)
    )
    rvs$plot <- p
    return(p)
  })
  
  output$interactive_plot <- renderPlotly({
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
           "Scatter plot" = p <- p + geom_point(size = input$size),
           "Box plot" = p <- p + geom_boxplot(aes_string(fill = selected_colour, colour = NULL, group = selected_colour)),
           "Bar plot" = p <- p + geom_bar(stat = "identity", aes_string(fill = selected_colour, colour = NULL))#,
    )
    
    switch(input$theme,
           "ggplot2 Default" = p <- p + theme_gray(base_size=16),
           bw = p <- p + theme_bw(base_size=16),
           Classic = p <- p + theme_classic(base_size=16),
           Minimal = p <- p + theme_minimal(base_size=16),
           Dark = p <- p + theme_dark(base_size=16)
    )
    rvs$plot <- p
    p <- ggplotly(p)
    
    return(p)
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
  
  
  observeEvent(input$generate, {
    # For PDF output, change this to "report.pdf"
    
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Building report.", 
                 detail = "This may take a while. This window will disappear  
                     when the report is ready.", value = 1)
    
    # filename <- paste0("SAGI-STH_training_report_", Sys.Date(), ".pdf")
    
    # content <- function(file) {
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- file.path(tempdir(), "report.Rmd")
    file.copy("report.Rmd", tempReport, overwrite = TRUE)
    
    tmp_file <- paste0(tempfile(), ".pdf")
    
    # Set up parameters to pass to Rmd document
    params <- list(data = rvs$data, plot = rvs$plot, x_val = input$xval, y_val = input$yval,
                   set_title = input$title, set_author = input$author)
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    rmarkdown::render(tempReport, 
                      output_file = tmp_file, 
                      params = params,
                      envir = new.env(parent = globalenv()))
    
    rvs$filepath <- tmp_file
    # }
  })
  
  output$reportbuilt <- reactive({
    return(!is.null(rvs$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
  
  # output$report <- downloadHandler(
  #     # For PDF output, change this to "report.pdf"
  #     filename = "report.pdf",
  #     content = function(file) {
  #         # Copy the report file to a temporary directory before processing it, in
  #         # case we don't have write permissions to the current working dir (which
  #         # can happen when deployed).
  #         tempReport <- file.path(tempdir(), "report.Rmd")
  #         file.copy("report.Rmd", tempReport, overwrite = TRUE)
  #         
  #         # Set up parameters to pass to Rmd document
  #         params <- list(data = rvs$data, plot = rvs$plot, x_val = input$xval, y_val = input$yval,
  #                        set_title = input$title, set_author = input$author)
  #         
  #         # Knit the document, passing in the `params` list, and eval it in a
  #         # child of the global environment (this isolates the code in the document
  #         # from the code in this app).
  #         rmarkdown::render(tempReport, output_file = file, 
  #                           params = params,
  #                           envir = new.env(parent = globalenv())
  #         )
  #     }
  # )
  output$download <- downloadHandler(
    # Downloading report currently doesn't work, see this issue:
    # https://github.com/rstudio/shiny/issues/2152
    # See also: 
    # https://github.com/rstudio/shiny-server/issues/197
    filename <- paste0("Report-", Sys.Date(), ".pdf"),
    content <- function(file) {
      file.copy(rvs$filepath, file)
    }
  )
  
  
  observeEvent(input$switch_to_table, {
    updateTabItems(session, "tabs", "table")
  })
  observeEvent(input$switch_to_plot, {
    updateTabsetPanel(session, "tabs", "plot")
  })
  
  observeEvent(input$disclaimer, {
    sendSweetAlert(
      session = session,
      title = "Disclaimer",
      text = tags$span(tags$p('Copyright (c) 2020 University of Adelaide Biometry Hub'),
                       tags$p('Permission is hereby granted, free of charge, to any person obtaining
          a copy of this software and associated documentation files (the
          "Software"), to deal in the Software without restriction, including
          without limitation the rights to use, copy, modify, merge, publish,
          distribute, sublicense, and/or sell copies of the Software, and to
          permit persons to whom the Software is furnished to do so, subject to
          the following conditions:'),
                       tags$ul(
                         tags$li('The above copyright notice and this permission notice shall
                  be included in all copies or substantial portions of the
                  Software.')
                       ),
                       tags$strong('THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
               EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
               OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
               NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
               HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
               WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
               FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
               OTHER DEALINGS IN THE SOFTWARE.')
      ),
      type = "warning"
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
