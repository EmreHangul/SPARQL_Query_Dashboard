library(shiny)
library(shinydashboard)
library(webreadr)
library(DT)
library(magrittr)
library(fs)
library(ggplot2)
library(dplyr)
library(plotly)
library(dashboardthemes)
library(shinybusy)
library(shinyFiles)
library(shinyWidgets)
library(rgeolocate)
library(stringr)
library(ggflags)
library(readr)
library(data.table)

# command line arguments, if supplied
args <- commandArgs(trailingOnly = TRUE)

# display system ip on the selectize input when the program is first initialized; corrected for OS type
if (.Platform$OS.type == "windows") {
  my_system_info <- system(command = "nslookup myip.opendns.com resolver1.opendns.com", intern=TRUE)
  my_ip <- str_extract(string = my_system_info[6], 
                       pattern = "\\d+\\.\\d+\\.\\d+\\.\\d+")
} else {
  my_ip <- system(command = "curl ifconfig.me", intern=TRUE)
}

################################DASHBOARD########################################

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = shinyDashboardLogo(theme = "poor_mans_flatly",
                                                               boldText = "Query Dashboard")),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(tabName = "Home", text = "Home"),
                        menuItem(tabName = "About", text = "About"))),
                    dashboardBody(
                      shinyDashboardThemes(theme = "poor_mans_flatly"),
                      tags$head(tags$style(HTML(".shiny-output-error-validation {color: #FF9900; font-size: 20px; font-weight: bold}"))),
                      tags$head(tags$style("#text {font-size: 18px;font-weight: bold}")),
                      tags$head(tags$style("#text2 {font-size: 18px;font-weight: bold}")),
                      tags$head(tags$style(".creator {margin: auto;background-color: #CCFFCC; width: 500px; height = 20px}")),
                      tags$head(tags$style(".creator h3 {text-align: center;font-family: Garamond, serif; font-weight: bold; font-size:20px}")),
                      tags$head(tags$style(".image {text-align: center}")),
                      tags$head(tags$style(".about h4 {font-family: Garamond, serif}")),
                      tags$head(tags$style("#live_text{width:1200px}")),
                      tabItems(
                        tabItem(tabName = "Home", 
                                fluidRow(
                                  tabBox(width = 12, 
                                         title = "", 
                                         tabPanel(title = "Live Data",
                                                  fluidRow(column(width = 12,
                                                                  tabBox(width = 12,
                                                                         tabPanel(title = "Overall",
                                                                                  fluidRow(column(width = 12,
                                                                                                  plotOutput("live_chart"))),
                                                                                  fluidRow(column(width = 4,offset = 2,
                                                                                                  wellPanel(strong("Number of lines total in the live file:"),
                                                                                                            textOutput("number_of_lines_total"),
                                                                                                            strong("Number of lines unread from the last session:"),
                                                                                                            textOutput("number_of_lines_unread"))),
                                                                                           column(width = 4,offset = 2,
                                                                                                  selectInput(inputId = "live_numeric",
                                                                                                              label = "Change Rate of Update(per minute)",
                                                                                                              choices = c("1","5","10","15","20","30","60","90","120","180"),
                                                                                                              selected = ifelse(length(args) == 0,
                                                                                                                                str_extract(string = readLines("app.conf")[1], pattern = "\\d+"),
                                                                                                                                args[1])))),
                                                                                  fluidRow(column(width = 12, align = "center",
                                                                                                  verbatimTextOutput("live_text"))),
                                                                                  fluidRow(column(width = 12,
                                                                                                  wellPanel(selectizeInput(inputId = "live_text_ip",
                                                                                                                           label = "Enter an IP Address:",
                                                                                                                           choices = my_ip,
                                                                                                                           selected = my_ip,
                                                                                                                           options = list(maxOptions = 10)),
                                                                                                            actionButton(inputId = "live_action_ip",
                                                                                                                         label = "Apply"),
                                                                                                            dataTableOutput("live_table_ip"))))),
                                                                         tabPanel(title = "Requests/Queries",
                                                                                  fluidRow(column(width = 6,
                                                                                                  plotOutput("live_request")),
                                                                                           column(width = 6,
                                                                                                  plotOutput("live_query")))),
                                                                         tabPanel(title = "HTTP Statuses",
                                                                                  fluidRow(column(width = 10,
                                                                                                  plotOutput("live_status")))),
                                                                         tabPanel(title = "User Agents/Browsers",
                                                                                  fluidRow(column(width = 10,
                                                                                                  plotOutput("live_agent")))),
                                                                         tabPanel(title = "IP Addresses",
                                                                                  fluidRow(column(width = 6,
                                                                                                  plotOutput("live_country")),
                                                                                           column(width = 6,
                                                                                                  plotOutput("live_company"))))))
                                                  )),
                                         tabPanel(title = "Files", add_busy_spinner(spin = "fading-circle", color = "green"), 
                                                  fluidRow(column(width = 12,
                                                                  align = "center",
                                                                  shinyFilesButton(id = "file",
                                                                                   label = "Select File",
                                                                                   title = "Please select one or multiple files:",
                                                                                   multiple = TRUE,
                                                                                   icon = icon(lib = "font-awesome",
                                                                                               name = "file-alt"),
                                                                                   viewtype = "detail"),
                                                                  shinyDirButton(id = "dir",
                                                                                 label = "Select Directory",
                                                                                 title = "Please select a directory",
                                                                                 allowDirCreate = FALSE,
                                                                                 icon = icon(lib = "font-awesome",
                                                                                             name = "folder-open"),
                                                                                 viewtype = "detail"),
                                                                  conditionalPanel(
                                                                    condition = "output.conddir==false",
                                                                    wellPanel("Selected directory name:",
                                                                              textOutput("text2")),
                                                                    pickerInput(
                                                                      inputId = "picker",
                                                                      label = h4("Select/Deselect Log Files (One, Multiple or All)"),
                                                                      choices = "",
                                                                      multiple = TRUE,
                                                                      width = "500px"),
                                                                    actionButton(inputId = "construct_data_frame",
                                                                                 label = "Construct Data Frame",
                                                                                 icon = icon("table")),
                                                                    actionButton(inputId = "construct_line_chart",
                                                                                 label = "Construct Line Chart",
                                                                                 icon = icon("chart-line")),
                                                                    conditionalPanel(
                                                                      condition = 'output.condplot=="chart"',
                                                                      plotlyOutput("picker_plot")),
                                                                    conditionalPanel(
                                                                      condition = 'output.condplot=="frame"',
                                                                      dataTableOutput("table2"))))),
                                                  fluidRow(column(width = 12, 
                                                                  align = "center",
                                                                  conditionalPanel(
                                                                    condition = "output.conddir==true",
                                                                    wellPanel("Selected file(s) name(s):",
                                                                              textOutput("text")),
                                                                    dataTableOutput("table")))
                                                  )),
                                         tabPanel(title = "Requests", add_busy_spinner(spin = "fading-circle", color = "green"),
                                                  fluidRow(column(width = 12,
                                                                  tabBox(width = 12,
                                                                         tabPanel("Requests Over Time",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Requests_1")),
                                                                                    column(width = 3,
                                                                                           dateRangeInput(inputId = "date_range_Requests_1",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Requests_1",
                                                                                                       label = "Select an HTTP Request Type:",
                                                                                                       choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                       selected = "GET"),
                                                                                           radioButtons(inputId = "radio_Requests_1",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day")))
                                                                         ),
                                                                         tabPanel("Status Codes",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Requests_2")),
                                                                                    column(width = 3,
                                                                                           dateRangeInput(inputId = "date_range_Requests_2",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Requests_2",
                                                                                                       label = "Select an HTTP Request Type:",
                                                                                                       choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                       selected = "GET"),
                                                                                           radioButtons(inputId = "radio_Requests_2",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day")))),
                                                                         tabPanel("User Agents",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Requests_3")),
                                                                                    column(width = 3, 
                                                                                           wellPanel(fluidRow(column(width = 12, 
                                                                                                                     selectizeInput(inputId = "selectize_Requests_3",
                                                                                                                                    label = "Select a single User-Agent:",
                                                                                                                                    options = list(maxOptions = 10),
                                                                                                                                    choices = NULL,
                                                                                                                                    selected = NULL))),
                                                                                                     fluidRow(column(width = 4, offset = 4,
                                                                                                                     actionButton(inputId = "action_Requests_3", label = "Reset")
                                                                                                     )),
                                                                                                     br(),
                                                                                                     fluidRow(column(width = 12,
                                                                                                                     numericInput(inputId = "numeric_Requests_3",
                                                                                                                                  label = "See top (n) User Agents:",
                                                                                                                                  value = 1,
                                                                                                                                  min = 1,
                                                                                                                                  max = NA)))),
                                                                                           dateRangeInput(inputId = "date_range_Requests_3",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Requests_3",
                                                                                                       label = "Select an HTTP Request Type:",
                                                                                                       choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                       selected = "GET"),
                                                                                           radioButtons(inputId = "radio_Requests_3",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day")))),
                                                                         tabPanel("IP Addresses",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Requests_4"),
                                                                                           dataTableOutput("table_Requests_4")),
                                                                                    column(width = 3, 
                                                                                           wellPanel(fluidRow(column(width = 12, 
                                                                                                                     selectizeInput(inputId = "selectize_Requests_4",
                                                                                                                                    label = "Select a single IP Address:",
                                                                                                                                    options = list(maxOptions = 10),
                                                                                                                                    choices = NULL,
                                                                                                                                    selected = NULL))),
                                                                                                     fluidRow(column(width = 4, offset = 4,
                                                                                                                     actionButton(inputId = "action_Requests_4", label = "Reset")
                                                                                                     )),
                                                                                                     br(),
                                                                                                     fluidRow(column(width = 12,
                                                                                                                     numericInput(inputId = "numeric_Requests_4",
                                                                                                                                  label = "See top (n) IP Addresses:",
                                                                                                                                  value = 1,
                                                                                                                                  min = 1,
                                                                                                                                  max = NA))),
                                                                                                     fluidRow(column(width = 12,
                                                                                                                     checkboxInput(inputId = "checkbox_Requests_4",
                                                                                                                                   label = "Aggregate IPs by Companies)",
                                                                                                                                   value = FALSE)))),
                                                                                           dateRangeInput(inputId = "date_range_Requests_4",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Requests_4",
                                                                                                       label = "Select an HTTP Request Type:",
                                                                                                       choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                       selected = "GET"),
                                                                                           radioButtons(inputId = "radio_Requests_4",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day")))))))),
                                         tabPanel(title = "Queries", add_busy_spinner(spin = "fading-circle", color = "green"),
                                                  fluidRow(column(width = 12,
                                                                  tabBox(width = 12,
                                                                         tabPanel("Queries Over Time",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Queries_1")),
                                                                                    column(width = 3,
                                                                                           dateRangeInput(inputId = "date_range_Queries_1",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Queries_1",
                                                                                                       label = "Select a SPARQL Query Type:",
                                                                                                       choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                           radioButtons(inputId = "radio_Queries_1",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day")))
                                                                                  
                                                                         ),
                                                                         tabPanel("Status Codes",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Queries_2")),
                                                                                    column(width = 3,
                                                                                           dateRangeInput(inputId = "date_range_Queries_2",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Queries_2",
                                                                                                       label = "Select a SPARQL Query Type:",
                                                                                                       choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                           radioButtons(inputId = "radio_Queries_2",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day")))),
                                                                         tabPanel("User Agents",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Queries_3")),
                                                                                    column(width = 3,
                                                                                           wellPanel(fluidRow(column(width = 12, 
                                                                                                                     selectizeInput(inputId = "selectize_Queries_3",
                                                                                                                                    label = "Select a single User-Agent:",
                                                                                                                                    options = list(maxOptions = 10),
                                                                                                                                    choices = NULL,
                                                                                                                                    selected = NULL))),
                                                                                                     fluidRow(column(width = 4, offset = 4,
                                                                                                                     actionButton(inputId = "action_Queries_3", label = "Reset")
                                                                                                     )),
                                                                                                     br(),
                                                                                                     fluidRow(column(width = 12,
                                                                                                                     numericInput(inputId = "numeric_Queries_3",
                                                                                                                                  label = "See top (n) User Agents:",
                                                                                                                                  value = 1,
                                                                                                                                  min = 1,
                                                                                                                                  max = NA)))),
                                                                                           dateRangeInput(inputId = "date_range_Queries_3",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Queries_3",
                                                                                                       label = "Select a SPARQL Query Type:",
                                                                                                       choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                           radioButtons(inputId = "radio_Queries_3",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day")))),
                                                                         tabPanel("IP Addresses",
                                                                                  fluidRow(
                                                                                    column(width = 9,
                                                                                           plotlyOutput("plot_Queries_4"),
                                                                                           dataTableOutput("table_Queries_4")),
                                                                                    column(width = 3,
                                                                                           wellPanel(fluidRow(column(width = 12, 
                                                                                                                     selectizeInput(inputId = "selectize_Queries_4",
                                                                                                                                    label = "Select a single IP Address:",
                                                                                                                                    options = list(maxOptions = 10),
                                                                                                                                    choices = NULL,
                                                                                                                                    selected = NULL))),
                                                                                                     fluidRow(column(width = 4, offset = 4,
                                                                                                                     actionButton(inputId = "action_Queries_4", label = "Reset")
                                                                                                     )),
                                                                                                     br(),
                                                                                                     fluidRow(column(width = 12,
                                                                                                                     numericInput(inputId = "numeric_Queries_4",
                                                                                                                                  label = "See top (n) IP Addresses:",
                                                                                                                                  value = 1,
                                                                                                                                  min = 1,
                                                                                                                                  max = NA))),
                                                                                                     fluidRow(column(width = 12,
                                                                                                                     checkboxInput(inputId = "checkbox_Queries_4",
                                                                                                                                   label = "Aggregate IPs by Companies",
                                                                                                                                   value = FALSE)))),
                                                                                           dateRangeInput(inputId = "date_range_Queries_4",
                                                                                                          label = "Choose Dates Between:",
                                                                                                          start = "2020-12-31",
                                                                                                          end = "2021-12-31"),
                                                                                           selectInput(inputId = "select_Queries_4",
                                                                                                       label = "Select a SPARQL Query Type:",
                                                                                                       choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                           radioButtons(inputId = "radio_Queries_4",
                                                                                                        label = "Select a time period:",
                                                                                                        choices = c("Year","Month","Day","Hour"),
                                                                                                        selected = "Day"))))))))))),
                        tabItem(tabName = "About",
                                tags$div(class = "image",
                                         tags$img(height = 300,
                                                  width = 300,
                                                  src = "https://bimerr.eu/wp-content/uploads/2019/03/Universidad-Polit%C3%A9cnica-De-Madrid-Logo.jpg")),
                                tags$div(class = "about",
                                         tags$h4("This application is created to visualize the HTTP requests related to SPARQL queries
                                     made through Apache servers. The main sources of data are Apache log files that 
                                     include necessary information for such visualizations."),
                                         br(),
                                         tags$h4(HTML("&rAarr;"), 'The log files include several important fields such as "timestamp", "IP address", "HTTP Requests", "User-Agents", and so on.'),
                                         tags$h4(HTML("&rAarr;"), 'This application uses these information to create line charts and data tables that can be
                                          interactively used and changed by the users.'),
                                         tags$h4(HTML("&rAarr;"), 'Users can change the type of HTTP requests and type of SPARQL queries within those requests
                                          to produce meaningful graphs.'),
                                         tags$h4(HTML("&rAarr;"), 'Using the configuration files given, the application can be made to display information from a live log file in a 
                                          local directory and show various related information simultaneously.')),
                                br(),
                                tags$div(class = "image",
                                         tags$img(height = 200,
                                                  width = 700,
                                                  src = "https://media.istockphoto.com/vectors/stock-chart-vector-id1087607772?k=6&m=1087607772&s=612x612&w=0&h=bgZWzRMdw1ziEeKnKIhGXET-pMDPshPkNr__BZ0BRgs=")),
                                tags$div(class = "creator",
                                         tags$h3("App Creator: Emre Hangul")),
                                tags$div(class = "creator",
                                         tags$h3("Institution: Universidad Politecnica de Madrid")),
                                tags$div(class = "creator",
                                         tags$h3("Release Date: June 2021")),
                                tags$div(class = "creator",
                                         tags$h3("For more information, please refer to/contact with: ", a(href = "https://github.com/EmreHangul/SPARQL_Query_Dashboard", "github/EmreHangul")))
                        ))))

################################SERVER###########################################

server <- function(input, output, session){
  
  # create home directory and volumes path
  volumes <- c(Home = path_wd(), 
               "R Installation" = R.home(), 
               getVolumes()())
  
  # choose file from the system
  shinyFileChoose(input, "file", 
                  roots = volumes, 
                  session = session,
                  filetypes = c("log", "gz"))
  
  # choose a directory containing files
  shinyDirChoose(input, "dir",
                 roots = volumes,
                 session = session,
                 filetypes = c("log", "gz"))
  
  # update conditional panel condition when "select directory" button is clicked
  output$conddir <- reactive({
    is.integer(tail(input$dir,1))
  })
  
  # update conditional panel condition when "construct data frame" button is/isn't clicked
  output$condplot <- reactive({
    rv$last_button
  })
  
  # output options for "picker input" conditional panel
  outputOptions(output,"conddir",suspendWhenHidden = FALSE)
  
  # output options for "picker input" conditional panel
  outputOptions(output,"condplot",suspendWhenHidden = FALSE)
  
  # initialization of reactive input data
  rv <- reactiveValues(log_data = data.frame(),
                       request = data.frame(),
                       plot_data = data.frame(),
                       last_button = character())
  
  # show files inside of a directory as pickerInput when a directory is selected
  observeEvent(input$dir,{
    
    input_dir <- parseDirPath(roots = volumes, 
                              input$dir)
    updatePickerInput(session = session, 
                      inputId = "picker",
                      choices = list.files(input_dir),
                      options = list(
                        "actions-box" = TRUE,
                        "deselect-all-text" = "Clear",
                        "select-all-text" = "Select All",
                        "none-selected-text" = "No file is selected",
                        "selected-text-format" = "count>3"
                      ))
  })
  
  # if the user wishes to see individual (or combined) log files on a line chart, do the following:
  observeEvent(input$construct_line_chart,{
    
    # if "construct line chart" button is clicked, update the conditional panel condition
    if(input$construct_line_chart > 0){
      rv$last_button = "chart"
    }
    
    # parse the file input into a more usable format
    input_d <- parseDirPath(roots = volumes,
                            input$dir)
    
    # extract the names of the files in the selected directory
    files <- list.files(input_d, pattern = "*.gz | *.log", full.names = FALSE)
    
    # validate that at least one file should be selected to construct the line chart
    validate(
      need(!is.na(input$picker), " ")
    )
    files_selected <- files[files == input$picker]
    
    # construct a new progress bar
    progress <- Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    
    # load the data into a list using "lapply", also show a progress bar
    data_list <- list()
    for (i in 1:length(files_selected)) {
      data_list[[i]] <- as.list(read_combined(file = paste0(input_d, "/", files_selected[i])))
      progress$set(value = 100*(i/length(files_selected)),
                   message = "Please wait while the selected files are loaded.",
                   detail = paste0("Current progress: ", round(100*(i/length(files_selected)), 0), "%"))
    }
    
    # load the data into the reactive data
    rv$plot_data <- rbindlist(data_list)
    
    # add "days" field to the reactive line chart data
    rv$plot_data$days <- as.Date(format(as.POSIXct(rv$plot_data$timestamp), "%Y-%m-%d"))
    
  })
  
  # if the user wishes to see individual (or combined) log files in a data frame, do the following:
  observeEvent(input$construct_data_frame,{
    
    # if "construct data frame" button is clicked, update the conditional panel condition
    if(input$construct_data_frame > 0){
      rv$last_button = "frame"
    }
    
    # parse the file input into a more usable format
    input_d <- parseDirPath(roots = volumes,
                            input$dir)
    
    # extract the names of the files in the selected directory
    files <- list.files(input_d, pattern = "*.gz | *.log", full.names = FALSE)
    
    # validate that at least one file should be selected to construct the data frame
    validate(
      need(!is.na(input$picker), " ")
    )
    # construct a list of all the data from the selected log files
    files_selected <- files[files == input$picker]
    
    # construct a new progress bar
    progress <- Progress$new(session, min = 0, max = 100)
    on.exit(progress$close())
    
    # load the data into a list using "lapply", also show a progress bar
    data_list <- list()
    for (i in 1:length(files_selected)) {
      data_list[[i]] <- as.list(read_combined(file = paste0(input_d, "/", files_selected[i])))
      progress$set(value = 100*(i/length(files_selected)),
                   message = "Please wait while the selected files are loaded.",
                   detail = paste0("Current progress: ", round(100*(i/length(files_selected)), 0), "%"))
    }
    
    # load the data into the reactive data
    rv$log_data <- rbindlist(data_list)
    
    # Update the corresponding data into solicited format
    if(length(rv$log_data) != 0){
      
      rv$log_data$year <- format(as.POSIXct(rv$log_data$timestamp), "%Y")
      rv$log_data$month <- format(as.POSIXct(rv$log_data$timestamp), "%Y-%m")
      rv$log_data$day <- format(as.POSIXct(rv$log_data$timestamp), "%Y-%m-%d")
      rv$log_data$hour <- format(as.POSIXct(rv$log_data$timestamp), "%Y-%m-%d %H:%M")
      
      requests <- split_clf(rv$log_data$request) %>% 
        cbind(year = rv$log_data$year,
              month = rv$log_data$month,
              day = rv$log_data$day, 
              hour = rv$log_data$hour,
              status = rv$log_data$status_code,
              user_agent = rv$log_data$user_agent,
              ip_address = rv$log_data$ip_address)
      
      rv$request <- requests
      
      # update selectize inputs for user agents and ip addresses
      updateSelectizeInput(session,
                           inputId = "selectize_Requests_3",
                           choices = levels(as.factor(rv$request$user_agent)),
                           server = TRUE)
      updateSelectizeInput(session,
                           inputId = "selectize_Queries_3",
                           choices = levels(as.factor(rv$request$user_agent)),
                           server = TRUE)
      updateSelectizeInput(session,
                           inputId = "selectize_Requests_4",
                           choices = levels(as.factor(rv$request$ip_address)),
                           server = TRUE)
      updateSelectizeInput(session,
                           inputId = "selectize_Queries_4",
                           choices = levels(as.factor(rv$request$ip_address)),
                           server = TRUE)
    }
  })
  
  # update the reactive data when an input file is selected (from select file)
  observeEvent(input$file,{
    
    # parse the file input into a more usable format
    input_file <- parseFilePaths(roots = volumes,
                                 input$file)
    
    # update the reactive data with the corresponding log file
    rv$log_data <- rbindlist(lapply(input_file$datapath, read_combined))
    
    # if a file is selected, update the corresponding data into solicited format
    if(length(rv$log_data) != 0){
      
      rv$log_data$year <- format(as.POSIXct(rv$log_data$timestamp), "%Y")
      rv$log_data$month <- format(as.POSIXct(rv$log_data$timestamp), "%Y-%m")
      rv$log_data$day <- format(as.POSIXct(rv$log_data$timestamp), "%Y-%m-%d")
      rv$log_data$hour <- format(as.POSIXct(rv$log_data$timestamp), "%Y-%m-%d %H:%M")
      
      requests <- split_clf(rv$log_data$request) %>% 
        cbind(year = rv$log_data$year,
              month = rv$log_data$month,
              day = rv$log_data$day, 
              hour = rv$log_data$hour,
              status = rv$log_data$status_code,
              user_agent = rv$log_data$user_agent,
              ip_address = rv$log_data$ip_address)
      
      rv$request <- requests
      
      # update selectize inputs for user agents and ip addresses
      updateSelectizeInput(session,
                           inputId = "selectize_Requests_3",
                           choices = levels(as.factor(rv$request$user_agent)),
                           selected = "",
                           server = TRUE)
      updateSelectizeInput(session,
                           inputId = "selectize_Queries_3",
                           choices = levels(as.factor(rv$request$user_agent)),
                           selected = "",
                           server = TRUE)
      updateSelectizeInput(session,
                           inputId = "selectize_Requests_4",
                           choices = levels(as.factor(rv$request$ip_address)),
                           selected = "",
                           server = TRUE)
      updateSelectizeInput(session,
                           inputId = "selectize_Queries_4",
                           choices = levels(as.factor(rv$request$ip_address)),
                           selected = "",
                           server = TRUE)
    }
  })
  
  # click on "reset" buttons to reset selectize inputs in "User-Agent" and "IP-Addresses" tabs
  observeEvent(input$action_Requests_3,{
    updateSelectizeInput(session,
                         inputId = "selectize_Requests_3",
                         choices = levels(as.factor(rv$request$user_agent)),
                         selected = "",
                         server = TRUE)
  })
  observeEvent(input$action_Queries_3,{
    updateSelectizeInput(session,
                         inputId = "selectize_Queries_3",
                         choices = levels(as.factor(rv$request$user_agent)),
                         selected = "",
                         server = TRUE)
  })
  observeEvent(input$action_Requests_4,{
    updateSelectizeInput(session,
                         inputId = "selectize_Requests_4",
                         choices = levels(as.factor(rv$request$ip_address)),
                         selected = "",
                         server = TRUE)
  })
  observeEvent(input$action_Queries_4,{
    updateSelectizeInput(session,
                         inputId = "selectize_Queries_4",
                         choices = levels(as.factor(rv$request$ip_address)),
                         selected = "",
                         server = TRUE)
  })
  
  # Reactive data for http requests (Over Time)
  rv_Requests_1 <- eventReactive(c(input$select_Requests_1, input$radio_Requests_1, input$date_range_Requests_1), {
    
    t <- tolower(input$radio_Requests_1)
    
    # return an error message if the solicited requests are not available.
    validate(
      need(grepl(input$select_Requests_1, rv$request$method)==TRUE,
           message = "No such requests are found! Please try another type of request.")
    )
    
    # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
    if(t == "hour"){
      
      # construct hourly intervals
      hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$day)), 
                                          by = "1 hour", 
                                          length.out = difftime(input$date_range_Requests_1[2], input$date_range_Requests_1[1], units = "hours")), "hours"))
      
      rv$request %>% 
        mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
        filter(grepl(input$select_Requests_1, rv$request$method)) %>%
        select(-c(asset,protocol)) %>% 
        filter(hour >= as.character(input$date_range_Requests_1[1]), hour <= as.character(input$date_range_Requests_1[2])) %>%
        group_by(hours) %>% 
        count(hours) %>% 
        rename(count = n, hour = hours)
      
    } else {
      
      rv$request %>% 
        filter(grepl(input$select_Requests_1, rv$request$method)) %>%
        select(-c(asset,protocol)) %>% 
        filter(.[t] >= as.character(input$date_range_Requests_1[1]), .[t] <= as.character(input$date_range_Requests_1[2])) %>% 
        group_by(.[t]) %>% 
        count(.[t]) %>% 
        rename(count = n)
    }
  })
  
  # Reactive data for http requests (Status Codes)
  rv_Requests_2 <- eventReactive(c(input$select_Requests_2, input$radio_Requests_2, input$date_range_Requests_2), {
    
    t <- tolower(input$radio_Requests_2)
    
    # return an error message if the solicited requests are not available.
    validate(
      need(grepl(input$select_Requests_2, rv$request$method)==TRUE,
           message = "No such requests are found! Please try another type of request.")
    )
    
    # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
    if(t == "hour"){
      
      # construct hourly intervals
      hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                          by = "1 hour", 
                                          length.out = difftime(input$date_range_Requests_2[2], input$date_range_Requests_2[1], units = "hours")), "hours"))
      
      rv$request %>% 
        mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
        filter(grepl(input$select_Requests_2, rv$request$method)) %>%
        select(-c(asset,protocol)) %>% 
        filter(hour >= as.character(input$date_range_Requests_2[1]), hour <= as.character(input$date_range_Requests_2[2])) %>%
        group_by(hours, status) %>% 
        count(hours) %>% 
        rename(count = n, hour = hours)
      
    } else {
      
      rv$request %>% 
        filter(grepl(input$select_Requests_2, rv$request$method)) %>%
        select(-c(asset,protocol)) %>% 
        filter(.[t] >= as.character(input$date_range_Requests_2[1]), .[t] <= as.character(input$date_range_Requests_2[2])) %>% 
        group_by(.[t], status) %>% 
        count(.[t]) %>% 
        rename(count = n)
    }
  })
  
  # Reactive data for http requests (User Agents)
  rv_Requests_3 <- eventReactive(c(input$select_Requests_3, input$radio_Requests_3, input$date_range_Requests_3, input$selectize_Requests_3, input$numeric_Requests_3),{
    
    t <- tolower(input$radio_Requests_3)
    
    # return an error message if the solicited requests are not available.
    validate(
      need(grepl(input$select_Requests_3, rv$request$method)==TRUE,
           message = "No such requests are found! Please try another type of request.")
    )
    
    # if no individual user-agent is selected, show the general view with top 10 most/least used user-agents
    if(input$selectize_Requests_3 == ""){
      
      validate(
        need(input$numeric_Requests_3 != "", "Please enter a number to display top (n) most used User-Agents.")
      )
      
      # create data frame with the top 10 most/least used user-agents (also including the mean)
      rv$request %>% 
        filter(grepl(input$select_Requests_3, rv$request$method)) %>%
        select(-c(asset,protocol, status)) %>%
        filter(!is.na(user_agent)) %>% 
        group_by(user_agent) %>% 
        count(user_agent) %>% 
        rename(count = n) %>% 
        as.data.frame(.) %>% 
        arrange(desc(count)) %>% 
        rbind(head(., input$numeric_Requests_3), data.frame(user_agent = "mean", count = as.integer(mean(.$count))), tail(., input$numeric_Requests_3)) %>% 
        tail(2 * input$numeric_Requests_3 + 1)
      
    } else {# in this case where an individual user-agent is selected, create user agent-specific graph
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(input$select_Requests_3, rv$request$method)==TRUE,
             message = "No such requests are found! Please try another type of request.")
      )
      
      # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
      if(t == "hour"){
        
        # construct hourly intervals
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                            by = "1 hour", 
                                            length.out = difftime(input$date_range_Requests_3[2], input$date_range_Requests_3[1], units = "hours")), "hours"))
        
        rv$request %>%
          filter(user_agent == input$selectize_Requests_3) %>% 
          mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
          filter(grepl(input$select_Requests_3, .$method)) %>%
          select(-c(asset,protocol, status)) %>% 
          filter(hour >= as.character(input$date_range_Requests_3[1]), hour <= as.character(input$date_range_Requests_3[2])) %>%
          group_by(hours) %>% 
          count(hours) %>% 
          rename(count = n, hour = hours)
        
      } else {
        
        rv$request %>%
          filter(user_agent == input$selectize_Requests_3) %>% 
          filter(grepl(input$select_Requests_3, .$method)) %>%
          select(-c(asset,protocol, status)) %>% 
          filter(.[t] >= as.character(input$date_range_Requests_3[1]), .[t] <= as.character(input$date_range_Requests_3[2])) %>% 
          group_by(.[t]) %>% 
          count(.[t]) %>% 
          rename(count = n)
      }
    }
  })
  
  # Reactive data for http requests (IP Addresses)
  rv_Requests_4 <- eventReactive(c(input$select_Requests_4, input$radio_Requests_4, input$date_range_Requests_4, input$selectize_Requests_4, input$numeric_Requests_4, input$checkbox_Requests_4),{
    
    t <- tolower(input$radio_Requests_4)
    
    # if "aggregate by companies" checkbox is checked, then produce company-specific stats and graphs
    if(input$checkbox_Requests_4 == TRUE){
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(input$select_Requests_4, rv$request$method)==TRUE,
             message = "No such requests are found! Please try another type of request.")
      )
      
      # if no single company is selected, then produce a general view of top 10 company IPs with the most/least requests
      if(input$selectize_Requests_4 == ""){
        
        validate(
          need(input$numeric_Requests_4 != "", "Please enter a number to display top (n) most used User-Agents.")
        )
        
        # create data frame with the top 10 most/least used ip addresses(companies) (also including the mean)
        rv$request %>% 
          filter(grepl(input$select_Requests_4, rv$request$method)) %>%
          select(-c(asset,protocol, status, user_agent)) %>%
          filter(!is.na(ip_address)) %>%
          group_by(ip_companies = str_extract(.$ip_address, pattern = "\\d+\\.\\d+\\.\\d+\\.")) %>% 
          count(ip_companies) %>% 
          rename(count = n) %>% 
          as.data.frame(.) %>% 
          arrange(desc(count)) %>% 
          rbind(head(., input$numeric_Requests_4), data.frame(ip_companies = "mean", count = as.integer(mean(.$count))), tail(., input$numeric_Requests_4)) %>% 
          tail(2 * input$numeric_Requests_4 + 1)
        
      } else {# if a single company is selected, then produce company-specific stats and graphs
        
        # return an error message if the solicited requests are not available.
        validate(
          need(grepl(input$select_Requests_4, rv$request$method)==TRUE,
               message = "No such requests are found! Please try another type of request.")
        )
        
        # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
        if(t == "hour"){
          
          # construct hourly intervals
          hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                              by = "1 hour", 
                                              length.out = difftime(input$date_range_Requests_4[2], input$date_range_Requests_4[1], units = "hours")), "hours"))
          
          rv$request %>%
            mutate(ip_companies = str_extract(.$ip_address, pattern = "\\d+\\.\\d+\\.\\d+\\.")) %>% 
            filter(ip_companies == str_sub(input$selectize_Requests_4, end = -2)) %>% 
            mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
            filter(grepl(input$select_Requests_4, .$method)) %>%
            select(-c(asset,protocol, status, user_agent)) %>% 
            filter(hour >= as.character(input$date_range_Requests_4[1]), hour <= as.character(input$date_range_Requests_4[2])) %>%
            group_by(hours) %>% 
            count(hours) %>% 
            rename(count = n, hour = hours)
          
        } else {
          
          rv$request %>%
            mutate(ip_companies = str_extract(.$ip_address, pattern = "\\d+\\.\\d+\\.\\d+\\.")) %>% 
            filter(ip_companies == str_sub(input$selectize_Requests_4, end = -2)) %>% 
            filter(grepl(input$select_Requests_4, .$method)) %>%
            select(-c(asset,protocol, status, user_agent)) %>% 
            filter(.[t] >= as.character(input$date_range_Requests_4[1]), .[t] <= as.character(input$date_range_Requests_4[2])) %>% 
            group_by(.[t]) %>% 
            count(.[t]) %>% 
            rename(count = n)
        }
      }
      
    } else {# in this case where the "aggregate by companies" is not checked, then produce stats-graphs for all IPs
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(input$select_Requests_4, rv$request$method)==TRUE,
             message = "No such requests are found! Please try another type of request.")
      )
      
      if(input$selectize_Requests_4 == ""){
        
        validate(
          need(input$numeric_Requests_4 != "", "Please enter a number to display top (n) most used User-Agents.")
        )
        
        # create data frame with the top 10 most/least used ip addresses (also including the mean)
        rv$request %>% 
          filter(grepl(input$select_Requests_4, rv$request$method)) %>%
          select(-c(asset,protocol, status, user_agent)) %>%
          filter(!is.na(ip_address)) %>%
          group_by(ip_address) %>% 
          count(ip_address) %>% 
          rename(count = n) %>% 
          as.data.frame(.) %>% 
          arrange(desc(count)) %>% 
          rbind(head(., input$numeric_Requests_4), data.frame(ip_address = "mean", count = as.integer(mean(.$count))), tail(., input$numeric_Requests_4)) %>% 
          tail(2 * input$numeric_Requests_4 + 1)
        
      } else {
        
        # return an error message if the solicited requests are not available.
        validate(
          need(grepl(input$select_Requests_4, rv$request$method)==TRUE,
               message = "No such requests are found! Please try another type of request.")
        )
        
        # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
        if(t == "hour"){
          
          # construct hourly intervals
          hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                              by = "1 hour", 
                                              length.out = difftime(input$date_range_Requests_4[2], input$date_range_Requests_4[1], units = "hours")), "hours"))
          
          rv$request %>%
            filter(ip_address == input$selectize_Requests_4) %>% 
            mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
            filter(grepl(input$select_Requests_4, .$method)) %>%
            select(-c(asset,protocol, status, user_agent)) %>% 
            filter(hour >= as.character(input$date_range_Requests_4[1]), hour <= as.character(input$date_range_Requests_4[2])) %>%
            group_by(hours) %>% 
            count(hours) %>% 
            rename(count = n, hour = hours)
          
        } else {
          
          rv$request %>%
            filter(ip_address == input$selectize_Requests_4) %>% 
            filter(grepl(input$select_Requests_4, .$method)) %>%
            select(-c(asset,protocol, status, user_agent)) %>% 
            filter(.[t] >= as.character(input$date_range_Requests_4[1]), .[t] <= as.character(input$date_range_Requests_4[2])) %>% 
            group_by(.[t]) %>% 
            count(.[t]) %>% 
            rename(count = n)
        }
      }
    }
  })
  
  # Reactive data for sparql types (Over Time)
  rv_Queries_1 <- eventReactive(c(input$select_Queries_1, input$radio_Queries_1, input$date_range_Queries_1), {
    
    t <- tolower(input$radio_Queries_1)
    
    # return an error message if the solicited requests are not available.
    validate(
      need(grepl(paste0("sparql.*", input$select_Queries_1), rv$request$asset)==TRUE,
           message = "No such queries are found! Please try another type of query.")
    )
    
    # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
    if(t == "hour"){
      
      # construct hourly intervals
      hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                          by = "1 hour", 
                                          length.out = difftime(input$date_range_Queries_1[2], input$date_range_Queries_1[1], units = "hours")), "hours"))
      
      rv$request %>% 
        mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
        filter(grepl(paste0("sparql.*", input$select_Queries_1), rv$request$asset)) %>% 
        select(-c(method,protocol)) %>%
        filter(hour >= as.character(input$date_range_Queries_1[1]), hour <= as.character(input$date_range_Queries_1[2])) %>%
        group_by(hours) %>% 
        count(hours) %>% 
        rename(count = n, hour = hours)
      
    } else {
      
      rv$request %>% 
        filter(grepl(paste0("sparql.*", input$select_Queries_1), rv$request$asset)) %>% 
        select(-c(method,protocol)) %>%
        filter(.[t] >= as.character(input$date_range_Queries_1[1]), .[t] <= as.character(input$date_range_Queries_1[2])) %>%
        group_by(.[t]) %>% 
        count(.[t]) %>% 
        rename(count = n)
    }
  })
  
  # Reactive data for sparql types (Status Codes)
  rv_Queries_2 <- eventReactive(c(input$select_Queries_2, input$radio_Queries_2, input$date_range_Queries_2), {
    
    t <- tolower(input$radio_Queries_2)
    
    # return an error message if the solicited requests are not available.
    validate(
      need(grepl(paste0("sparql.*", input$select_Queries_2), rv$request$asset)==TRUE,
           message = "No such queries are found! Please try another type of query.")
    )
    
    # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
    if(t == "hour"){
      
      # construct hourly intervals
      hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                          by = "1 hour", 
                                          length.out = difftime(input$date_range_Queries_2[2], input$date_range_Queries_2[1], units = "hours")), "hours"))
      
      rv$request %>% 
        mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
        filter(grepl(paste0("sparql.*", input$select_Queries_2), rv$request$asset)) %>% 
        select(-c(method,protocol)) %>%
        filter(hour >= as.character(input$date_range_Queries_2[1]), hour <= as.character(input$date_range_Queries_2[2])) %>%
        group_by(hours, status) %>% 
        count(hours) %>% 
        rename(count = n, hour = hours)
      
    } else {
      
      rv$request %>% 
        filter(grepl(paste0("sparql.*", input$select_Queries_2), rv$request$asset)) %>% 
        select(-c(method,protocol)) %>%
        filter(.[t] >= as.character(input$date_range_Queries_2[1]), .[t] <= as.character(input$date_range_Queries_2[2])) %>%
        group_by(.[t], status) %>% 
        count(.[t]) %>% 
        rename(count = n)
    }
  })
  
  # Reactive data for sparql types (User Agents)
  rv_Queries_3 <- eventReactive(c(input$select_Queries_3, input$radio_Queries_3, input$date_range_Queries_3, input$selectize_Queries_3, input$numeric_Queries_3),{
    
    t <- tolower(input$radio_Queries_3)
    
    # return an error message if the solicited requests are not available.
    validate(
      need(grepl(paste0("sparql.*", input$select_Queries_3), rv$request$asset)==TRUE,
           message = "No such queries are found! Please try another type of query.")
    )
    
    # if no individual user-agent is selected, show the general view with top 10 most/least used user-agents
    if(input$selectize_Queries_3 == ""){
      
      validate(
        need(input$numeric_Queries_3 != "", "Please enter a number to display top (n) most used User-Agents.")
      )
      
      # create data frame with the top 10 most/least used user-agents (also including the mean)
      rv$request %>% 
        filter(grepl(paste0("sparql.*", input$select_Queries_3), rv$request$asset)) %>%
        select(-c(method,protocol, status)) %>%
        filter(!is.na(user_agent)) %>% 
        group_by(user_agent) %>% 
        count(user_agent) %>% 
        rename(count = n) %>% 
        as.data.frame(.) %>% 
        arrange(desc(count)) %>% 
        rbind(head(., input$numeric_Queries_3), data.frame(user_agent = "mean", count = as.integer(mean(.$count))), tail(., input$numeric_Queries_3)) %>% 
        tail(2 * input$numeric_Queries_3 + 1)
      
    } else {# in this case where an individual user-agent is selected, create user agent-specific graph
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(paste0("sparql.*", input$select_Queries_3), rv$request$asset)==TRUE,
             message = "No such queries are found! Please try another type of query.")
      )
      
      # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
      if(t == "hour"){
        
        # construct hourly intervals
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                            by = "1 hour", 
                                            length.out = difftime(input$date_range_Queries_3[2], input$date_range_Queries_3[1], units = "hours")), "hours"))
        
        rv$request %>%
          filter(user_agent == input$selectize_Queries_3) %>% 
          mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
          filter(grepl(paste0("sparql.*", input$select_Queries_3), .$asset)) %>%
          select(-c(method,protocol, status)) %>% 
          filter(hour >= as.character(input$date_range_Queries_3[1]), hour <= as.character(input$date_range_Queries_3[2])) %>%
          group_by(hours) %>% 
          count(hours) %>% 
          rename(count = n, hour = hours)
        
      } else {
        
        rv$request %>%
          filter(user_agent == input$selectize_Queries_3) %>% 
          filter(grepl(paste0("sparql.*", input$select_Queries_3), .$asset)) %>%
          select(-c(method,protocol, status)) %>% 
          filter(.[t] >= as.character(input$date_range_Queries_3[1]), .[t] <= as.character(input$date_range_Queries_3[2])) %>% 
          group_by(.[t]) %>% 
          count(.[t]) %>% 
          rename(count = n)
      }
    }
  })
  
  # Reactive data for sparql types (IP Addresses)
  rv_Queries_4 <- eventReactive(c(input$select_Queries_4, input$radio_Queries_4, input$date_range_Queries_4, input$selectize_Queries_4, input$numeric_Queries_4, input$checkbox_Queries_4),{
    
    t <- tolower(input$radio_Queries_4)
    
    if(input$checkbox_Queries_4 == TRUE){
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(paste0("sparql.*", input$select_Queries_4), rv$request$asset)==TRUE,
             message = "No such queries are found! Please try another type of query.")
      )
      
      # if no single company is selected, then produce a general view of top 10 company IPs with the most/least requests
      if(input$selectize_Queries_4 == ""){
        
        validate(
          need(input$numeric_Queries_4 != "", "Please enter a number to display top (n) most used User-Agents.")
        )
        
        # create data frame with the top 10 most/least used ip addresses (also including the mean)
        rv$request %>% 
          filter(grepl(paste0("sparql.*", input$select_Queries_4), rv$request$asset)) %>%
          select(-c(method,protocol, status, user_agent)) %>%
          filter(!is.na(ip_address)) %>% 
          group_by(ip_companies = str_extract(.$ip_address, pattern = "\\d+\\.\\d+\\.\\d+\\.")) %>% 
          count(ip_companies) %>% 
          rename(count = n) %>% 
          as.data.frame(.) %>% 
          arrange(desc(count)) %>% 
          rbind(head(., input$numeric_Queries_4), data.frame(ip_companies = "mean", count = as.integer(mean(.$count))), tail(., input$numeric_Queries_4)) %>% 
          tail(2 * input$numeric_Queries_4 + 1)
        
      } else {# if a single company is selected, then produce company-specific stats and graphs
        
        # return an error message if the solicited requests are not available.
        validate(
          need(grepl(paste0("sparql.*", input$select_Queries_4), rv$request$asset)==TRUE,
               message = "No such queries are found! Please try another type of query.")
        )
        
        # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
        if(t == "hour"){
          
          # construct hourly intervals
          hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                              by = "1 hour", 
                                              length.out = difftime(input$date_range_Queries_4[2], input$date_range_Queries_4[1], units = "hours")), "hours"))
          
          rv$request %>%
            mutate(ip_companies = str_extract(.$ip_address, pattern = "\\d+\\.\\d+\\.\\d+\\.")) %>% 
            filter(ip_companies == str_sub(input$selectize_Queries_4, end = -2)) %>% 
            mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
            filter(grepl(paste0("sparql.*", input$select_Queries_4), .$asset)) %>%
            select(-c(method,protocol, status, user_agent)) %>% 
            filter(hour >= as.character(input$date_range_Queries_4[1]), hour <= as.character(input$date_range_Queries_4[2])) %>%
            group_by(hours) %>% 
            count(hours) %>% 
            rename(count = n, hour = hours)
          
        } else {
          
          rv$request %>%
            mutate(ip_companies = str_extract(.$ip_address, pattern = "\\d+\\.\\d+\\.\\d+\\.")) %>% 
            filter(ip_companies == str_sub(input$selectize_Queries_4, end = -2)) %>% 
            filter(grepl(paste0("sparql.*", input$select_Queries_4), .$asset)) %>%
            select(-c(method,protocol, status, user_agent)) %>% 
            filter(.[t] >= as.character(input$date_range_Queries_4[1]), .[t] <= as.character(input$date_range_Queries_4[2])) %>% 
            group_by(.[t]) %>% 
            count(.[t]) %>% 
            rename(count = n)
        }
      }
    } else {# in this case where the "aggregate by companies" is not checked, then produce stats-graphs for all IPs
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(paste0("sparql.*", input$select_Queries_4), rv$request$asset)==TRUE,
             message = "No such queries are found! Please try another type of query.")
      )
      
      if(input$selectize_Queries_4 == ""){
        
        validate(
          need(input$numeric_Queries_4 != "", "Please enter a number to display top (n) most used User-Agents.")
        )
        
        # create data frame with the top 10 most/least used ip addresses (also including the mean)
        rv$request %>% 
          filter(grepl(paste0("sparql.*", input$select_Queries_4), rv$request$asset)) %>%
          select(-c(method,protocol, status, user_agent)) %>%
          filter(!is.na(ip_address)) %>% 
          group_by(ip_address) %>% 
          count(ip_address) %>% 
          rename(count = n) %>% 
          as.data.frame(.) %>% 
          arrange(desc(count)) %>% 
          rbind(head(., input$numeric_Queries_4), data.frame(ip_address = "mean", count = as.integer(mean(.$count))), tail(., input$numeric_Queries_4)) %>% 
          tail(2 * input$numeric_Queries_4 + 1)
        
      } else {
        
        # return an error message if the solicited requests are not available.
        validate(
          need(grepl(paste0("sparql.*", input$select_Queries_4), rv$request$asset)==TRUE,
               message = "No such queries are found! Please try another type of query.")
        )
        
        # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
        if(t == "hour"){
          
          # construct hourly intervals
          hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), 
                                              by = "1 hour", 
                                              length.out = difftime(input$date_range_Queries_4[2], input$date_range_Queries_4[1], units = "hours")), "hours"))
          
          rv$request %>%
            filter(ip_address == input$selectize_Queries_4) %>% 
            mutate(hours = cut(as.POSIXct(.$hour), breaks = as.POSIXct(hour_groups))) %>% 
            filter(grepl(paste0("sparql.*", input$select_Queries_4), .$asset)) %>%
            select(-c(method,protocol, status, user_agent)) %>% 
            filter(hour >= as.character(input$date_range_Queries_4[1]), hour <= as.character(input$date_range_Queries_4[2])) %>%
            group_by(hours) %>% 
            count(hours) %>% 
            rename(count = n, hour = hours)
          
        } else {
          
          rv$request %>%
            filter(ip_address == input$selectize_Queries_4) %>% 
            filter(grepl(paste0("sparql.*", input$select_Queries_4), .$asset)) %>%
            select(-c(method,protocol, status, user_agent)) %>% 
            filter(.[t] >= as.character(input$date_range_Queries_4[1]), .[t] <= as.character(input$date_range_Queries_4[2])) %>% 
            group_by(.[t]) %>% 
            count(.[t]) %>% 
            rename(count = n)
        }
      }
    }
  })
  
  # Update the selectize inputs to list either the whole IP addresses or company IPs via the checkboxes
  observe({
    if(input$checkbox_Requests_4 == FALSE){
      updateSelectizeInput(session,
                           inputId = "selectize_Requests_4",
                           choices = levels(as.factor(rv$request$ip_address)),
                           selected = "",
                           server = TRUE)
    } else{
      updateSelectizeInput(session,
                           inputId = "selectize_Requests_4",
                           choices = paste0(str_extract(levels(as.factor(rv$request$ip_address)),
                                                        pattern = "\\d+\\.\\d+\\.\\d+\\."),  "*"),
                           selected = "",
                           server = TRUE)
    }
  })
  observe({
    if(input$checkbox_Queries_4 == FALSE){
      updateSelectizeInput(session,
                           inputId = "selectize_Queries_4",
                           choices = levels(as.factor(rv$request$ip_address)),
                           selected = "",
                           server = TRUE)
    } else{
      updateSelectizeInput(session,
                           inputId = "selectize_Queries_4",
                           choices = paste0(str_extract(levels(as.factor(rv$request$ip_address)),
                                                        pattern = "\\d+\\.\\d+\\.\\d+\\."),  "*"),
                           selected = "",
                           server = TRUE)
    }
  })
  
  # Update date range (Requests- Over Time) 
  observeEvent(input$date_range_Requests_1[1], {
    end_date = input$date_range_Requests_1[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Requests_1[2] < input$date_range_Requests_1[1]){
      end_date = input$date_range_Requests_1[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Requests_1",
                         start = input$date_range_Requests_1[1],
                         end = end_date)
    
  })
  
  # Update date range (Requests- Status Codes) 
  observeEvent(input$date_range_Requests_2[1], {
    end_date = input$date_range_Requests_2[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Requests_2[2] < input$date_range_Requests_2[1]){
      end_date = input$date_range_Requests_2[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Requests_2",
                         start = input$date_range_Requests_2[1],
                         end = end_date)
    
  })
  
  # Update date range (Requests- User Agent) 
  observeEvent(input$date_range_Requests_3[1], {
    end_date = input$date_range_Requests_3[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Requests_3[2] < input$date_range_Requests_3[1]){
      end_date = input$date_range_Requests_3[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Requests_3",
                         start = input$date_range_Requests_3[1],
                         end = end_date)
    
  })
  
  # Update date range (Requests- IP Addresses) 
  observeEvent(input$date_range_Requests_4[1], {
    end_date = input$date_range_Requests_4[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Requests_4[2] < input$date_range_Requests_4[1]){
      end_date = input$date_range_Requests_4[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Requests_4",
                         start = input$date_range_Requests_4[1],
                         end = end_date)
    
  })
  
  # Update date range (Queries- Over Time) 
  observeEvent(input$date_range_Queries_1[1], {
    end_date = input$date_range_Queries_1[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Queries_1[2] < input$date_range_Queries_1[1]){
      end_date = input$date_range_Queries_1[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Queries_1",
                         start = input$date_range_Queries_1[1],
                         end = end_date)
    
  })
  
  # Update date range (Queries- Status Codes) 
  observeEvent(input$date_range_Queries_2[1], {
    end_date = input$date_range_Queries_2[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Queries_2[2] < input$date_range_Queries_2[1]){
      end_date = input$date_range_Queries_2[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Queries_2",
                         start = input$date_range_Queries_2[1],
                         end = end_date)
    
  })
  
  # Update date range (Queries- User Agent) 
  observeEvent(input$date_range_Queries_3[1], {
    end_date = input$date_range_Queries_3[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Queries_3[2] < input$date_range_Queries_3[1]){
      end_date = input$date_range_Queries_3[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Queries_3",
                         start = input$date_range_Queries_3[1],
                         end = end_date)
    
  })
  
  # Update date range (Queries- IP Addresses) 
  observeEvent(input$date_range_Queries_4[1], {
    end_date = input$date_range_Queries_4[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_Queries_4[2] < input$date_range_Queries_4[1]){
      end_date = input$date_range_Queries_4[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_Queries_4",
                         start = input$date_range_Queries_4[1],
                         end = end_date)
    
  })
  
  # display the name of the selected file
  output$text <- renderPrint({
    # if the last element of input$file is integer (it means no file is currently selected), return warning.
    validate(
      need(!is.integer(tail(input$file,1)), "No file is currently selected. Please select a file.")
    )
    
    # show the name of the selected files
    for (i in 0:(length(input$file$files)-1)) {
      i <- as.character(i)
      cat(unlist(input$file$files[[i]]) %>% tail(1),"\t")
    }
    
  })
  
  # display the name of the selected directory
  output$text2 <- renderText({
    # if the last element of input$dir is integer (it means no file is currently selected), return warning.
    validate(
      need(!is.integer(input$dir), "No directory is currently selected. Please select a directory.")
    )
    input_dir <- parseDirPath(roots = volumes, input$dir)
    print(input_dir)
  })
  
  # show the input data as table (from selected file)
  output$table <- renderDataTable({
    
    # create a dataframe using log data  
    rv$log_data %>% datatable(rownames = FALSE,
                              options = list(
                                lengthMenu = c(5,10,20,50),
                                autoWidth = TRUE,
                                scrollX = TRUE),
                              filter = "top")
    
  })
  
  # show the input data as table (from selected directory)
  output$table2 <- renderDataTable({
    rv$log_data %>% datatable(rownames = FALSE,
                              options = list(
                                lengthMenu = c(5,10,20,50),
                                autoWidth = TRUE,
                                scrollX = TRUE),
                              filter = "top")
  })
  
  
  # show the location information of the ip addresses
  output$table_Requests_4 <- renderDataTable({
    ip_api(input$selectize_Requests_4) %>% datatable(rownames = FALSE,
                                                     options = list(
                                                       autoWidth = TRUE,
                                                       lengthChange = FALSE,
                                                       scrollX = TRUE))
  })
  output$table_Queries_4 <- renderDataTable({
    ip_api(input$selectize_Queries_4) %>% datatable(rownames = FALSE,
                                                    options = list(
                                                      autoWidth = TRUE,
                                                      lengthChange = FALSE,
                                                      scrollX = TRUE))
  })
  
  # construct line chart when the "construct line chart" button is clicked after a directory is selected
  output$picker_plot <- renderPlotly({
    
    # validation for the plot to work smoothly
    validate(
      need(nrow(rv$plot_data) > 0, " ")
    )
    
    ggplotly(rv$plot_data %>% 
               group_by(days) %>% 
               count(days) %>% 
               rename(count = n) %>% 
               ggplot(aes(x = days, y = count)) +
               geom_line(col = "#00FF00")+ 
               geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
               labs(title = "HTTP Requests over Time",
                    x = "Days",
                    y = "Total Requests") +
               theme_light() +
               theme(axis.text.x = element_text(angle = 90, hjust = 1),
                     axis.title.x = element_blank(),
                     axis.title.y = element_text(size = 1.5)),tooltip = c("y","x"))
  })
  
  # Interactive plot for HTTP Requests (Over time)
  output$plot_Requests_1 <- renderPlotly({
    
    # return an error message if the date ranges for rv_Requests_1() is not applicable.
    validate(
      need(nrow(rv_Requests_1()) > 0, "There is no data between selected dates. Please select a different date range.")
    )
    
    ggplotly(
      rv_Requests_1() %>% 
        ggplot(aes_string(x = tolower(input$radio_Requests_1), y = "count")) +
        geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
        geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
        labs(title = "Number of Requests Over Selected Timeframe",
             x = paste0("\nTime Interval(",tolower(input$radio_Requests_1),")")) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_text(size = 13),
              axis.title.y = element_text(size = 13)), tooltip = c("y","x")) %>% layout(yaxis = list(title = 
                                                                                                       list(text = "Number of Requests",
                                                                                                            standoff = 20L)))
  })
  
  # Interactive plot for HTTP Requests (Status Codes)
  output$plot_Requests_2 <- renderPlotly({
    
    # x and y axes parameters to be used in the 'layout' function
    x <- list(
      title = list(text = paste0("\nTime Interval(",tolower(input$radio_Requests_2),")"),
                   standoff = 15L)
    )
    y <- list(
      title = list(text = "Number of Requests",
                   standoff = 15L)
    )
    
    # return an error message if the date ranges for rv_Requests_2() is not applicable.
    validate(
      need(nrow(rv_Requests_2()) > 0, "There is no data between selected dates. Please select a different date range.")
    )
    
    ggplotly(
      rv_Requests_2() %>% 
        ggplot(aes_string(x = tolower(input$radio_Requests_2), y = "count", fill = "status")) +
        geom_bar(position = "dodge", width = 0.5, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
        geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
        facet_grid(~status, scales = "free_x", space = "free_x") +
        labs(title = "Number of Queries Over Selected Timeframe - Facet by Status Codes") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              strip.background = element_rect(fill = "#CCFFCC"),
              strip.text = element_text(color = "#003300",
                                        size = rel(1.25))), tooltip = c("y","x")) %>% layout(yaxis = y, xaxis = x)
  })
  
  # Interactive plot for HTTP Requests (User Agents)
  output$plot_Requests_3 <- renderPlotly({
    
    # return an error message if the date ranges for rv_Requests_3() is not applicable.
    validate(
      need(nrow(rv_Requests_3()) > 0 | input$selectize_Requests_3 != "", "There is no data between selected dates. Please select a different date range.")
    )
    
    if(input$selectize_Requests_3 == ""){
      
      # index of the data point "mean"
      index <- which(rv_Requests_3()$user_agent == "mean")
      
      ggplotly(
        rv_Requests_3() %>% 
          ggplot(aes(x = reorder(user_agent, count), y = count)) +
          geom_bar(aes(text = paste("User Agent Name: ", user_agent, "\nRequest Count: ", count)),
                   width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"),  col = "#003300", check_overlap = TRUE) +
          annotate("text", label = "Mean", x = rv_Requests_3()$user_agent[index], y = -2*(rv_Requests_3()$count[index]), size = 4.5, colour = "blue") +
          labs(title = "User Agents with the Most / the Least # of Requests",
               x = "User Agents",
               y = "Number of Requests") +
          theme_light() +
          theme(axis.text.x = element_blank(),
                axis.title.x = element_text(size = 13),
                axis.title.y = element_text(size = 13)), tooltip = "text")
      
    } else {
      
      # return an error message if the solicited requests are not available.
      validate(
        need(nrow(rv_Requests_3()) > 0, "No such requests are found! Please try another type of request..")
      )
      
      ggplotly(
        rv_Requests_3() %>% 
          ggplot(aes_string(x = tolower(input$radio_Requests_3), y = "count")) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
          labs(title = "Number of Requests Over Selected Timeframe",
               x = paste0("\nTime Interval(",tolower(input$radio_Requests_3),")"),
               y = "Total Requests") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title.x = element_text(size = 13),
                axis.title.y = element_text(size = 13)), tooltip = c("y","x"))
      
    }
  })
  
  # Interactive plot for HTTP Requests (IP Addresses)
  output$plot_Requests_4 <- renderPlotly({
    
    # construct company specific plots
    if(input$checkbox_Requests_4 == TRUE){
      
      # return an error message if the date ranges for rv_Requests_4() is not applicable.
      validate(
        need(nrow(rv_Requests_4()) > 0 | input$selectize_Requests_4 != "", "There is no data between selected dates. Please select a different date range.")
      )
      
      if(input$selectize_Requests_4 == ""){
        
        # index of the data point "mean"
        index <- which(rv_Requests_4()$ip_companies == "mean")
        
        ggplotly(
          rv_Requests_4() %>% 
            ggplot(aes(x = reorder(ip_companies, count), y = count)) +
            geom_bar(aes(text = paste("Company IP Address': ", ip_companies, "\nRequest Count: ", count)),
                     width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"),  col = "#003300", check_overlap = TRUE) +
            annotate("text", label = "Mean", x = rv_Requests_4()$ip_companies[index], y = -2*(rv_Requests_4()$count[index]), size = 4.5, colour = "blue") +
            labs(title = "Company IP Addresses with the Most / the Least # of Requests",
                 x = "Company IP Addresses",
                 y = "Number of Requests") +
            theme_light() +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = "text")
        
      } else {
        
        # return an error message if the solicited requests are not available.
        validate(
          need(nrow(rv_Requests_4()) > 0, "No such requests are found! Please try another type of request..")
        )
        
        ggplotly(
          rv_Requests_4() %>% 
            ggplot(aes_string(x = tolower(input$radio_Requests_4), y = "count")) +
            geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
            labs(title = "Number of Requests Over Selected Timeframe",
                 x = paste0("\nTime Interval(",tolower(input$radio_Requests_4),")"),
                 y = "Total Requests") +
            theme_light() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = c("y","x"))
        
      }
    } else {# construct individual IP plots
      
      # return an error message if the date ranges for rv_Requests_4() is not applicable.
      validate(
        need(nrow(rv_Requests_4()) > 0 | input$selectize_Requests_4 != "", "There is no data between selected dates. Please select a different date range.")
      )
      
      if(input$selectize_Requests_4 == ""){
        
        # index of the data point "mean"
        index <- which(rv_Requests_4()$ip_address == "mean")
        
        ggplotly(
          rv_Requests_4() %>% 
            ggplot(aes(x = reorder(ip_address, count), y = count)) +
            geom_bar(aes(text = paste("IP Address': ", ip_address, "\nRequest Count: ", count)),
                     width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"),  col = "#003300", check_overlap = TRUE) +
            annotate("text", label = "Mean", x = rv_Requests_4()$ip_address[index], y = -2*(rv_Requests_4()$count[index]), size = 4.5, colour = "blue") +
            labs(title = "IP Addresses with the Most / the Least # of Requests",
                 x = "IP Addresses",
                 y = "Number of Requests") +
            theme_light() +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = "text")
        
      } else {
        
        # return an error message if the solicited requests are not available.
        validate(
          need(nrow(rv_Requests_4()) > 0, "No such requests are found! Please try another type of request..")
        )
        
        ggplotly(
          rv_Requests_4() %>% 
            ggplot(aes_string(x = tolower(input$radio_Requests_4), y = "count")) +
            geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
            labs(title = "Number of Requests Over Selected Timeframe",
                 x = paste0("\nTime Interval(",tolower(input$radio_Requests_4),")"),
                 y = "Total Requests") +
            theme_light() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = c("y","x"))
      }
    }
  })
  
  # Interactive plot for SPARQL Queries (Over time)
  output$plot_Queries_1 <- renderPlotly({
    # return an error message if the date ranges for rv_Queries_1() is not applicable.
    validate(
      need(nrow(rv_Queries_1()) > 0, "There is no data between selected dates. Please select a different date range.")
    )
    ggplotly(
      rv_Queries_1() %>% 
        ggplot(aes_string(x = tolower(input$radio_Queries_1), y = "count")) +
        geom_bar(stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
        geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
        labs(title = "Number of Queries Over Selected Timeframe",
             x = paste0("\nTime Interval(",tolower(input$radio_Queries_1),")")) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_text(size = 13),
              axis.title.y = element_text(size = 13)), tooltip = c("y","x")) %>% layout(yaxis = list(title = 
                                                                                                       list(text = "Number of Requests",
                                                                                                            standoff = 20L)))
  })
  
  # Interactive plot for SPARQL Queries (Status Codes)
  output$plot_Queries_2 <- renderPlotly({
    
    # x and y axes parameters to be used in the 'layout' function
    x <- list(
      title = list(text = paste0("\nTime Interval(",tolower(input$radio_Requests_2),")"),
                   standoff = 15L)
    )
    y <- list(
      title = list(text = "Number of Requests",
                   standoff = 15L)
    )
    
    # return an error message if the date ranges for rv_Queries_2() is not applicable.
    validate(
      need(nrow(rv_Queries_2()) > 0, "There is no data between selected dates. Please select a different date range.")
    )
    
    ggplotly(
      rv_Queries_2() %>% 
        ggplot(aes_string(x = tolower(input$radio_Queries_2), y = "count", fill = "status")) +
        geom_bar(position = "dodge", stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
        geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
        facet_grid(~status) +
        labs(title = "Number of Queries Over Selected Timeframe - Facet by Status Codes") +
        theme_light() +
        theme(axis.text.x = element_blank(),
              axis.title.x = element_text(size = 13),
              axis.title.y = element_text(size = 13),
              strip.background = element_rect(fill = "#CCFFCC"),
              strip.text = element_text(color = "#003300",
                                        size = rel(1.25))), tooltip = c("y","x")) %>% layout(yaxis = y, xaxis = x)
  })
  
  # Interactive plot for SPARQL Queries (User Agents)
  output$plot_Queries_3 <- renderPlotly({
    
    # return an error message if the date ranges for rv_Queries_3() is not applicable.
    validate(
      need(nrow(rv_Queries_3()) > 0 | input$selectize_Queries_3 != "", "There is no data between selected dates. Please select a different date range.")
    )
    
    if(input$selectize_Queries_3 == ""){
      
      # index of the data point "mean"
      index <- which(rv_Queries_3()$user_agent == "mean")
      
      ggplotly(
        rv_Queries_3() %>% 
          ggplot(aes(x = reorder(user_agent, count), y = count)) +
          geom_bar(aes(text = paste("User Agent Name: ", user_agent, "\nQuery Count: ", count)),
                   width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99") + 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"),  col = "#003300", check_overlap = TRUE) +
          annotate("text", label = "Mean", x = rv_Queries_3()$user_agent[index], y = -2*(rv_Queries_3()$count[index]), size = 4.5, colour = "blue") +
          labs(title = "User Agents with the Most / the Least # of Queries",
               x = "User Agents",
               y = "Number of Queries") +
          theme_light() +
          theme(axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 13)), tooltip = "text")
      
    } else {
      
      # return an error message if the solicited requests are not available.
      validate(
        need(nrow(rv_Queries_3()) > 0, "No such queries are found! Please try another type of a query.")
      )
      
      ggplotly(
        rv_Queries_3() %>% 
          ggplot(aes_string(x = tolower(input$radio_Queries_3), y = "count")) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
          labs(title = "Number of Queries Over Selected Timeframe",
               x = paste0("\nTime Interval(",tolower(input$radio_Queries_3),")"),
               y = "Total Queries") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title.x = element_text(size = 13),
                axis.title.y = element_text(size = 13)), tooltip = c("y","x"))
      
    }
  })
  
  # Interactive plot for SPARQL Queries (IP Addresses)
  output$plot_Queries_4 <- renderPlotly({
    
    if(input$checkbox_Queries_4 == TRUE) {
      
      # return an error message if the date ranges for rv_Queries_4() is not applicable.
      validate(
        need(nrow(rv_Queries_4()) > 0 | input$selectize_Queries_4 != "", "There is no data between selected dates. Please select a different date range.")
      )
      
      if(input$selectize_Queries_4 == ""){
        
        # index of the data point "mean"
        index <- which(rv_Queries_4()$ip_companies == "mean")
        
        ggplotly(
          rv_Queries_4() %>% 
            ggplot(aes(x = reorder(ip_companies, count), y = count)) +
            geom_bar(aes(text = paste("Company IP Address: ", ip_companies, "\nQuery Count: ", count)),
                     width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99") + 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"),  col = "#003300", check_overlap = TRUE) +
            annotate("text", label = "Mean", x = rv_Queries_4()$ip_companies[index], y = -2*(rv_Queries_4()$count[index]), size = 4.5, colour = "blue") +
            labs(title = "Company IP Addresses with the Most / the Least # of Queries",
                 x = "Company IP Addresses",
                 y = "Number of Queries") +
            theme_light() +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = "text")
        
      } else {
        
        # return an error message if the solicited requests are not available.
        validate(
          need(nrow(rv_Queries_4()) > 0, "No such queries are found! Please try another type of a query.")
        )
        
        ggplotly(
          rv_Queries_4() %>% 
            ggplot(aes_string(x = tolower(input$radio_Queries_4), y = "count")) +
            geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
            labs(title = "Number of Queries Over Selected Timeframe",
                 x = paste0("\nTime Interval(",tolower(input$radio_Queries_4),")"),
                 y = "Total Queries") +
            theme_light() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = c("y","x"))
        
      }
    } else {
      # return an error message if the date ranges for rv_Queries_4() is not applicable.
      validate(
        need(nrow(rv_Queries_4()) > 0 | input$selectize_Queries_4 != "", "There is no data between selected dates. Please select a different date range.")
      )
      
      if(input$selectize_Queries_4 == ""){
        
        # index of the data point "mean"
        index <- which(rv_Queries_4()$ip_address == "mean")
        
        ggplotly(
          rv_Queries_4() %>% 
            ggplot(aes(x = reorder(ip_address, count), y = count)) +
            geom_bar(aes(text = paste("IP Address' Name: ", ip_address, "\nQuery Count: ", count)),
                     width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99") + 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"),  col = "#003300", check_overlap = TRUE) +
            annotate("text", label = "Mean", x = rv_Queries_4()$ip_address[index], y = -2*(rv_Queries_4()$count[index]), size = 4.5, colour = "blue") +
            labs(title = "IP Addresses with the Most / the Least # of Queries",
                 x = "IP Addresses",
                 y = "Number of Queries") +
            theme_light() +
            theme(axis.text.x = element_blank(),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = "text")
        
      } else {
        
        # return an error message if the solicited requests are not available.
        validate(
          need(nrow(rv_Queries_4()) > 0, "No such queries are found! Please try another type of a query.")
        )
        
        ggplotly(
          rv_Queries_4() %>% 
            ggplot(aes_string(x = tolower(input$radio_Queries_4), y = "count")) +
            geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
            geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
            labs(title = "Number of Queries Over Selected Timeframe",
                 x = paste0("\nTime Interval(",tolower(input$radio_Queries_4),")"),
                 y = "Total Queries") +
            theme_light() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1),
                  axis.title.x = element_text(size = 13),
                  axis.title.y = element_text(size = 13)), tooltip = c("y","x"))
      }
    }
  })
  
  ########################## Implementation of the Live Functionality  ##########################
  
  # read the lines of the live log file
  total_live_data <- reactiveFileReader( 
    intervalMillis = reactive({60000/as.numeric(input$live_numeric)}),
    session = session,
    filePath = paste0(getwd(),"/", readLines("app.conf")[6] %>% substr(8,10000L),  "live_logs.log"),
    readFunc = read_combined)
  
  # construct different versions of the live data using a .lock file
  observe({    
    
    # if there is not a .lock file in the first place, read all of the live data and construct a .lock file
    if(file.exists("live_logs.log.lock") == FALSE){
      
      # the .lock file value will be the number of lines in *all* of the live data
      lock_file_value <- reactive({nrow(total_live_data())}) 
      
      # show the # of total lines in the data and the # of unread data (in this case, they are the same!)
      output$number_of_lines_total <- renderText({
        lock_file_value()
      })
      output$number_of_lines_unread <- renderText({
        lock_file_value()
      })
      
      # construct a reactive object using all of the live data
      live_data <- reactive({total_live_data()})
      
      # when the user closes the application, write the number of total data to a .lock file
      session$onSessionEnded(function(){
        write_csv(isolate(nrow(live_data())) %>% as_tibble(),
                  file = "live_logs.log.lock",
                  col_names = FALSE)
      })
      
      # show the last 10 lines in the live data
      output$live_text <- renderPrint({
        live_data() %>% as.data.frame() %>% tail(10) %>% print 
      })
      
      # reactive for live line chart
      live_chart_reactive <- reactive({
        live_data() %>%
          mutate(seconds = lubridate::ymd_hms(live_data()$timestamp)) %>% 
          group_by(seconds) %>% 
          count(seconds) %>% 
          rename(count = n)
      })
      
      # reactive for live HTTP requests
      live_request_reactive <- reactive({
        live_data() %>% 
          mutate(request = sapply(strsplit(live_data()$request, split = " "), "[", 1)) %>%
          filter(!is.na(request)) %>% 
          select(request) %>% 
          group_by(request) %>% 
          count(request) %>% 
          rename(count = n)
      })
      
      # reactive for live SPARQL query types
      live_query_reactive <- reactive({
        live_data() %>%
          filter(grepl("sparql.*SELECT|sparql.*CONSTRUCT|sparql.*DESCRIBE|sparql.*ASK", request)) %>% 
          select(request) %>% 
          group_by(query_type = str_extract(string = .$request, pattern = "SELECT|CONSTRUCT|DESCRIBE|ASK")) %>% 
          count(query_type) %>% 
          rename(count = n)
      })
      
      # reactive for live HTTP Status codes
      live_status_reactive <- reactive({
        live_data() %>%
          mutate(agents = sapply(strsplit(live_data()$user_agent, split = "-|\\/"), "[", 1)) %>% 
          select(agents, status_code) %>% 
          group_by(agents, status_code) %>% 
          count(agents) %>% 
          rename(count = n)
      })
      
      # reactive for live browsers(user-agents)
      live_agent_reactive <- reactive({
        live_data() %>% 
          mutate(agents = sapply(strsplit(live_data()$user_agent, split = "-|\\/"), "[", 1)) %>%
          filter(!is.na(agents)) %>% 
          select(agents) %>% 
          group_by(agents) %>% 
          count(agents) %>% 
          rename(count = n)
      })
      
      # reactive data for displaying country codes&flags of the last 5 IP addresses
      live_country_reactive <- reactive({
        ip_api(tail(live_data(),5)$ip_address) %>% 
          select(country_code) %>% 
          group_by(country_code) %>% 
          count(country_code) %>% 
          rename(count = n) %>% 
          mutate(country_code = tolower(country_code))
        
      })
      
      # reactive data for displaying companies(ISP-Internet Service Providers) of the last 5 IP addresses
      live_company_reactive <- reactive({
        ip_api(tail(live_data(),5)$ip_address) %>% 
          select(isp) %>% 
          group_by(isp) %>% 
          count(isp) %>% 
          rename(count = n)
      })
      
      # reactive plot for live line chart
      output$live_chart <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_chart_reactive() %>% 
          ggplot(aes(x = seconds, y = count)) +
          geom_line(col = "#66FF66", size = 1.2)+ 
          labs(x = "Realtime(in seconds)",
               y = "Requests(per second)\n") +
          scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.x = element_text(size = 18, color = "#003366"),
                axis.title.y = element_text(size = 18, color = "#003366"))
      })
      
      # reactive plot for live HTTP requests
      output$live_request <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_request_reactive() %>% 
          ggplot(aes(x = request, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Number of live HTTP Requests") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for live SPARQL queries
      output$live_query <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_query_reactive() %>% 
          ggplot(aes(x = query_type, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Number of live SPARQL Queries ") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for live HTTP Status codes
      output$live_status <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_status_reactive() %>% 
          ggplot(aes(x = agents, y = count)) +
          geom_bar(position = "dodge", width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
          facet_grid(~status_code, scales = "free_x", space = "free_x") +
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Status Codes of the Requests") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for live browsers(user-agents)
      output$live_agent <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_agent_reactive() %>% 
          ggplot(aes(x = agents, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Most Used User-Agents") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for displaying country codes&flags of the last 5 IP addresses
      output$live_country <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_country_reactive() %>% 
          ggplot(aes(x = country_code, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_flag(aes(country = country_code), size = 15, y = 0.5) +
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "IPs by Countries") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for displaying companies(ISP-Internet Service Providers) of the last 5 IP addresses
      output$live_company <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_company_reactive() %>% 
          ggplot(aes(x = isp, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "IPs by Companies") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # create a reactive object to later show the information extracted from an individual IP
      rv_ip <- reactiveValues(ip = data.frame())
      
      observeEvent(input$live_action_ip, {
        
        # when the "apply" button in "live data" tab is clicked, update the selectize input to include all 
        # of the IP addresses found so far
        updateSelectizeInput(session = session,
                             inputId = "live_text_ip",
                             server = TRUE,
                             selected = input$live_text_ip,
                             choices = live_data()$ip_address)
        
        # to show the IP information, first there should be an individual IP present.
        validate(
          need(length(input$live_text_ip) != 0, "")
        )
        
        rv_ip$ip  <- ip_api(input$live_text_ip)
        
      })
      
      # show the IP information in a data table
      output$live_table_ip <- renderDataTable({
        
        validate(
          need(nrow(rv_ip$ip) > 0, "")
        )
        
        rv_ip$ip %>% datatable(rownames = FALSE,
                               options = list(
                                 autoWidth = TRUE,
                                 lengthChange = FALSE,
                                 scrollX = TRUE))
      })
      
    } else { # if there is a .lock file in the working directory, read only the *unread* portion of the live log data, 
      # update .lock file accordingly for the next sessions
      
      # lock file value that corresponds to *unread* data
      lock_file_value <- reactive({nrow(total_live_data())- read.table("live_logs.log.lock")[1,1]})
      
      # show the # of total lines in the data and the # of unread data (in this case, they are different!
      # Total number of data is greater than the amount of *unread* data!)
      output$number_of_lines_total <- renderText({
        
        nrow(total_live_data())
      })
      output$number_of_lines_unread <- renderText({
        lock_file_value()
      })
      
      # reactive data that corresponds to the *unread* portion of the live data
      live_data <- reactive({tail(total_live_data(), lock_file_value())})
      
      # show the last 10 lines of the unread data
      output$live_text <- renderPrint({
        
        live_data() %>% as.data.frame() %>% tail(10) %>% print
        
      })
      
      # when the application is closed, simultaneously write the total number of lines in the live file 
      session$onSessionEnded(function(){
        write_csv(length(readLines(paste0(getwd(),"/", readLines("app.conf")[6] %>% substr(8,10000L),  "live_logs.log"))) %>% as_tibble(),
                  file = "live_logs.log.lock",
                  col_names = FALSE)
      })
      
      # reactive for live line chart
      live_chart_reactive <- reactive({
        live_data() %>%
          mutate(seconds = lubridate::ymd_hms(live_data()$timestamp)) %>% 
          group_by(seconds) %>% 
          count(seconds) %>% 
          rename(count = n)
      })
      
      # reactive for live HTTP requests of unread data
      live_request_reactive <- reactive({
        live_data() %>% 
          mutate(request = sapply(strsplit(live_data()$request, split = " "), "[", 1)) %>%
          filter(!is.na(request)) %>% 
          select(request) %>% 
          group_by(request) %>% 
          count(request) %>% 
          rename(count = n)
      })
      
      # reactive for live SPARQL query types of unread data
      live_query_reactive <- reactive({
        live_data() %>%
          filter(grepl("sparql.*SELECT|sparql.*CONSTRUCT|sparql.*DESCRIBE|sparql.*ASK", request)) %>% 
          select(request) %>% 
          group_by(query_type = str_extract(string = .$request, pattern = "SELECT|CONSTRUCT|DESCRIBE|ASK")) %>% 
          count(query_type) %>% 
          rename(count = n)
      })
      
      # reactive for live HTTP Status codes of unread data
      live_status_reactive <- reactive({
        live_data() %>%
          mutate(agents = sapply(strsplit(live_data()$user_agent, split = "-|\\/"), "[", 1)) %>% 
          select(agents, status_code) %>% 
          group_by(agents, status_code) %>% 
          count(agents) %>% 
          rename(count = n)
      })
      
      # reactive for live browsers(user-agents) of unread data
      live_agent_reactive <- reactive({
        live_data() %>% 
          mutate(agents = sapply(strsplit(live_data()$user_agent, split = "-|\\/"), "[", 1)) %>%
          filter(!is.na(agents)) %>% 
          select(agents) %>% 
          group_by(agents) %>% 
          count(agents) %>% 
          rename(count = n)
      })
      
      # reactive data for displaying country codes&flags of the last 5 IP addresses
      live_country_reactive <- reactive({
        ip_api(tail(live_data(), 5)$ip_address) %>% 
          select(country_code) %>% 
          group_by(country_code) %>% 
          count(country_code) %>% 
          rename(count = n) %>% 
          mutate(country_code = tolower(country_code))
        
      })
      
      # reactive data for displaying companies(ISP-Internet Service Providers) of the last 5 IP addresses
      live_company_reactive <- reactive({
        ip_api(tail(live_data(),5)$ip_address) %>% 
          select(isp) %>% 
          group_by(isp) %>% 
          count(isp) %>% 
          rename(count = n)
      })
      
      # reactive plot for live line chart
      output$live_chart <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_chart_reactive() %>% 
          ggplot(aes(x = seconds, y = count)) +
          geom_line(col = "#66FF66", size = 1.2)+ 
          labs(x = "Realtime(in seconds)",
               y = "Requests(per second)\n") +
          scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, size = 15),
                axis.text.y = element_text(size = 15),
                axis.title.x = element_text(size = 18, color = "#003366"),
                axis.title.y = element_text(size = 18, color = "#003366"))
      })
      
      # reactive plot for live HTTP Requests of unread data
      output$live_request <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_request_reactive() %>% 
          ggplot(aes(x = request, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Number of live HTTP Requests") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for live SPARQL queries of unread data
      output$live_query <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_query_reactive() %>% 
          ggplot(aes(x = query_type, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Number of live SPARQL Queries") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for live HTTP Status codes of unread data
      output$live_status <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_status_reactive() %>% 
          ggplot(aes(x = agents, y = count)) +
          geom_bar(position = "dodge", width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
          facet_grid(~status_code, scales = "free_x", space = "free_x") +
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Status Codes of the Requests") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for live browsers(user-agents) of unread data
      output$live_agent <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_agent_reactive() %>% 
          ggplot(aes(x = agents, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "Most Used User-Agents") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for displaying country codes&flags of the last 5 IP addresses
      output$live_country <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_country_reactive() %>% 
          ggplot(aes(x = country_code, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_flag(aes(country = country_code), size = 15, y = 0.5) +
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "IPs by Countries") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # reactive plot for displaying companies(ISP-Internet Service Providers) of the last 5 IP addresses
      output$live_company <- renderPlot({
        
        validate(
          need(nrow(live_data()) > 0 & file.exists(paste0(getwd(),"/", 
                                                          readLines("app.conf")[6] %>% substr(8,10000L),  
                                                          "live_logs.log")), "Please wait.")
        )
        
        live_company_reactive() %>% 
          ggplot(aes(x = isp, y = count)) +
          geom_bar(width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300", size = 5) +
          labs(title = "IPs by Companies") +
          theme_light() +
          theme(axis.text = element_text(angle = 90, hjust = 1, size = 15),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                plot.title = element_text(size = 18),
                legend.position = "none")
      })
      
      # show the information extracted from an individual IP that the user chooses
      rv_ip <- reactiveValues(ip = data.frame())
      
      observeEvent(input$live_action_ip, {
        
        # when the "apply" button in "live data" tab is clicked, update the selectize input to include all 
        # of the IP addresses found so far
        updateSelectizeInput(session = session,
                             inputId = "live_text_ip",
                             server = TRUE,
                             selected = input$live_text_ip,
                             choices = live_data()$ip_address)
        
        # to show the IP information, first there should be an individual IP present.
        validate(
          need(length(input$live_text_ip) != 0, "")
        )
        
        rv_ip$ip  <- ip_api(input$live_text_ip)
        
      })
      
      # show the IP information in a data table
      output$live_table_ip <- renderDataTable({
        
        validate(
          need(nrow(rv_ip$ip) > 0, "")
        )
        
        rv_ip$ip %>% datatable(rownames = FALSE,
                               options = list(
                                 autoWidth = TRUE,
                                 lengthChange = FALSE,
                                 scrollX = TRUE))
      })
    }
  }) 
}

shinyApp(ui = ui,server = server)