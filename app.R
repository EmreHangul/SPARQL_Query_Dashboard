library(shiny)
library(shinydashboard)
library(webreadr)
library(DT)
library(magrittr)
library(ggplot2)
library(dplyr)
library(plotly)
library(dashboardthemes)
library(shinyFiles)
library(shinyWidgets)


# function for displaying "loading" icon
shiny_busy <- function() {
  # use &nbsp; for some alignment, if needed
  HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", paste0(
    '<span data-display-if="',
    '$(&#39;html&#39;).attr(&#39;class&#39;)==&#39;shiny-busy&#39;',
    '">',
    '<i class="fa fa-spinner fa-pulse fa-fw" style="color:green; font-size:20px"></i>',
    '</span>'
  ))
}

################################DASHBOARD########################################

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = shinyDashboardLogo(theme = "poor_mans_flatly",
                                                               boldText = "Query Dashboard")),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(tabName = "Home", text = "Home"),
                        menuItem(tabName = "About", text = "About the App"),
                        menuItem(tabName = "Guide", text = "Manual Guide")
                      ),
                      sidebarSearchForm(textId = "search", buttonId = "searchbutton", label = "Search")
                    ),
                    dashboardBody(shiny_busy(), 
                                  shinyDashboardThemes(theme = "poor_mans_flatly"),
                                  tags$head(tags$style(HTML(".shiny-output-error-validation {color: #FF9900; font-size: 20px; font-weight: bold}"))),
                                  tags$head(tags$style("#text{font-size: 18px;font-weight: bold}")),
                                  tags$head(tags$style("#text2{font-size: 18px;font-weight: bold}")),
                                  tabItems(
                                    tabItem(tabName = "Home", 
                                            fluidRow(
                                              tabBox(width = 12,
                                                     title = "",
                                                     tabPanel(title = "Files",
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
                                                                                             allowDirCreate = TRUE,
                                                                                             icon = icon(lib = "font-awesome",
                                                                                                         name = "folder-open"),
                                                                                             viewtype = "detail"),
                                                                              conditionalPanel(
                                                                                condition = "output.conddir==false",
                                                                                wellPanel("Selected directory name:",
                                                                                          textOutput("text2")),
                                                                                multiInput(inputId = "multi",
                                                                                           label = "Files:",
                                                                                           choices = "",
                                                                                           selected = "",
                                                                                           width = "900px",
                                                                                           options = list(
                                                                                             non_selected_header = "Choose Files:",
                                                                                             selected_header = "You have selected:"
                                                                                           )),
                                                                                dataTableOutput("table2")))),
                                                              fluidRow(column(width = 12, 
                                                                              align = "center",
                                                                              conditionalPanel(
                                                                                condition = "output.conddir==true",
                                                                                wellPanel("Selected file name:",
                                                                                          textOutput("text")),
                                                                                dataTableOutput("table")))
                                                              )),
                                                     tabPanel(title = "Requests", 
                                                              fluidRow(column(width = 12,
                                                                              tabBox(width = 12,
                                                                                tabPanel("Requests Over Time",
                                                                                         fluidRow(
                                                                                           column(width = 9,
                                                                                                  plotlyOutput("plot_Requests_1")),
                                                                                           column(width = 3,
                                                                                                  dateRangeInput(inputId = "date_range_Requests_1",
                                                                                                                 label = "Choose Dates Between:",
                                                                                                                 start = "2020-12-31"),
                                                                                                  selectInput(inputId = "select_Requests_1",
                                                                                                              label = "Select an HTTP Request Type:",
                                                                                                              choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                              selected = "GET"),
                                                                                                  radioButtons(inputId = "radio_Requests_1",
                                                                                                               label = "Select a date period:",
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
                                                                                                                 start = "2020-12-31"),
                                                                                                  selectInput(inputId = "select_Requests_2",
                                                                                                              label = "Select an HTTP Request Type:",
                                                                                                              choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                              selected = "GET"),
                                                                                                  radioButtons(inputId = "radio_Requests_2",
                                                                                                               label = "Select a date period:",
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
                                                                                                                 start = "2020-12-31"),
                                                                                                  selectInput(inputId = "select_Requests_3",
                                                                                                              label = "Select an HTTP Request Type:",
                                                                                                              choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                              selected = "GET"),
                                                                                                  radioButtons(inputId = "radio_Requests_3",
                                                                                                               label = "Select a date period:",
                                                                                                               choices = c("Year","Month","Day","Hour"),
                                                                                                               selected = "Day")))),
                                                                                tabPanel("IP Addresses",
                                                                                         fluidRow(
                                                                                           column(width = 9,
                                                                                                  plotlyOutput("plot_Requests_4")),
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
                                                                                                                                         max = NA)))),
                                                                                                  dateRangeInput(inputId = "date_range_Requests_4",
                                                                                                                 label = "Choose Dates Between:",
                                                                                                                 start = "2020-12-31"),
                                                                                                  selectInput(inputId = "select_Requests_4",
                                                                                                              label = "Select an HTTP Request Type:",
                                                                                                              choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                                                                              selected = "GET"),
                                                                                                  radioButtons(inputId = "radio_Requests_4",
                                                                                                               label = "Select a date period:",
                                                                                                               choices = c("Year","Month","Day","Hour"),
                                                                                                               selected = "Day")))))))),
                                                     tabPanel(title = "Queries",
                                                              fluidRow(column(width = 12,
                                                                              tabBox(width = 12,
                                                                                     tabPanel("Queries Over Time",
                                                                                              fluidRow(
                                                                                                column(width = 9,
                                                                                                       plotlyOutput("plot_Queries_1")),
                                                                                                column(width = 3,
                                                                                                       dateRangeInput(inputId = "date_range_Queries_1",
                                                                                                                      label = "Choose Dates Between:",
                                                                                                                      start = "2020-12-31"),
                                                                                                       selectInput(inputId = "select_Queries_1",
                                                                                                                   label = "Select a SPARQL Query Type:",
                                                                                                                   choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                                       radioButtons(inputId = "radio_Queries_1",
                                                                                                                    label = "Select a date period:",
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
                                                                                                                     start = "2020-12-31"),
                                                                                                      selectInput(inputId = "select_Queries_2",
                                                                                                                  label = "Select a SPARQL Query Type:",
                                                                                                                  choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                                      radioButtons(inputId = "radio_Queries_2",
                                                                                                                   label = "Select a date period:",
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
                                                                                                                     start = "2020-12-31"),
                                                                                                      selectInput(inputId = "select_Queries_3",
                                                                                                                  label = "Select a SPARQL Query Type:",
                                                                                                                  choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                                      radioButtons(inputId = "radio_Queries_3",
                                                                                                                   label = "Select a date period:",
                                                                                                                   choices = c("Year","Month","Day","Hour"),
                                                                                                                   selected = "Day")))),
                                                                                    tabPanel("IP Addresses",
                                                                                             fluidRow(
                                                                                               column(width = 9,
                                                                                                      plotlyOutput("plot_Queries_4")),
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
                                                                                                                                             max = NA)))),
                                                                                                      dateRangeInput(inputId = "date_range_Queries_4",
                                                                                                                     label = "Choose Dates Between:",
                                                                                                                     start = "2020-12-31"),
                                                                                                      selectInput(inputId = "select_Queries_4",
                                                                                                                  label = "Select a SPARQL Query Type:",
                                                                                                                  choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                                                                                                      radioButtons(inputId = "radio_Queries_4",
                                                                                                                   label = "Select a date period:",
                                                                                                                   choices = c("Year","Month","Day","Hour"),
                                                                                                                   selected = "Day"))))))))))),
                                    tabItem(tabName = "About",
                                            wellPanel(
                                              h4("--> This application is created to visualize the number of SPARQL queries
                 made by the users throughout....."))
                                    ),
                                    tabItem(tabName = "Guide",
                                            h2("this will be a manual guide.")
                                    )
                                  )
                    )
)

################################SERVER###########################################

server <- function(input, output, session){
  
  # create home directory and volumes path
  volumes <- c(Home = fs::path_wd(), 
               "R Installation" = R.home(), 
               getVolumes()())
  
  # choose file from the system
  shinyFileChoose(input, "file", 
                  roots = volumes, 
                  session = session)
  
  # choose a directory containing files
  shinyDirChoose(input, "dir",
                 roots = volumes,
                 session = session)
  
  # update conditional panel condition when directory button is clicked
  output$conddir <- reactive({
    is.integer(tail(input$dir,1))
  })
  
  # output options for multiInput conditional panel
  outputOptions(output,"conddir",suspendWhenHidden = FALSE)
  
  # initialization of reactive input data
  rv <- reactiveValues(log_data = data.frame(),
                       request = data.frame())
  
  # show files inside of a directory as multiInput when a directory is selected
  observeEvent(input$dir,{
    input_dir <- parseDirPath(roots = volumes, 
                              input$dir)
    updateMultiInput(session = session, inputId = "multi", choices = list.files(input_dir))
  })
  
  # update the reactive data when an input file is selected (from select directory)
  observeEvent(input$multi,{
    # parse the file input into a more usable format
    input_d <- parseDirPath(roots = volumes,
                            input$dir)
    
    # construct a list of all the data from the log files
    initial_d <- lapply(list.files(input_d)[list.files(input_d)==input$multi], read_combined)
    
    # add all the log data into a 1 common data object
    i <- 1
    logs <- data.frame()
    
    while (i<=length(initial_d)) {
      logs <- rbind(logs, initial_d[[i]])
      i = i + 1
    }
    
    # update the reactive data with the corresponding log file
    rv$log_data <- logs
    
    # if a file is selected, update the corresponding data into solicited format
    if(length(logs) != 0){
      
      logs$year <- format(as.POSIXct(logs$timestamp), "%Y")
      logs$month <- format(as.POSIXct(logs$timestamp), "%Y-%m")
      logs$day <- format(as.POSIXct(logs$timestamp), "%Y-%m-%d")
      logs$hour <- format(as.POSIXct(logs$timestamp), "%Y-%m-%d %H:%M")
      
      requests <- split_clf(logs$request) %>% 
        cbind(year = logs$year,
              month = logs$month,
              day = logs$day, 
              hour = logs$hour,
              status = logs$status_code,
              user_agent = logs$user_agent,
              ip_address = logs$ip_address)
      
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
    
    # construct a list of all the data from the log files
    initial_data <- lapply(input_file$datapath, read_combined)
    
    # add all the log data into a 1 common data object
    i <- 1
    logs <- data.frame()
    
    while (i<=length(initial_data)) {
      logs <- rbind(logs, initial_data[[i]])
      i = i + 1
    }
    
    
    # update the reactive data with the corresponding log file
    rv$log_data <- logs
    
    # if a file is selected, update the corresponding data into solicited format
    if(length(logs) != 0){
      
      logs$year <- format(as.POSIXct(logs$timestamp), "%Y")
      logs$month <- format(as.POSIXct(logs$timestamp), "%Y-%m")
      logs$day <- format(as.POSIXct(logs$timestamp), "%Y-%m-%d")
      logs$hour <- format(as.POSIXct(logs$timestamp), "%Y-%m-%d %H:%M")
      
      requests <- split_clf(logs$request) %>% 
        cbind(year = logs$year,
              month = logs$month,
              day = logs$day, 
              hour = logs$hour,
              status = logs$status_code,
              user_agent = logs$user_agent,
              ip_address = logs$ip_address)
      
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
      hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))
      
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
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))

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
      
    } else {
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(input$select_Requests_3, rv$request$method)==TRUE,
             message = "No such requests are found! Please try another type of request.")
      )
      
      # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
      if(t == "hour"){
        
        # construct hourly intervals
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))
        
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
  rv_Requests_4 <- eventReactive(c(input$select_Requests_4, input$radio_Requests_4, input$date_range_Requests_4, input$selectize_Requests_4, input$numeric_Requests_4),{
    
    t <- tolower(input$radio_Requests_4)
    
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
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))
        
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
      hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))
      
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
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))
        
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
      
    } else {
      
      # return an error message if the solicited requests are not available.
      validate(
        need(grepl(paste0("sparql.*", input$select_Queries_3), rv$request$asset)==TRUE,
             message = "No such queries are found! Please try another type of query.")
      )
      
      # construct hourly intervals(bins) to increase visual effects (otherwise: countless x-values)
      if(t == "hour"){
        
        # construct hourly intervals
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))
        
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
  rv_Queries_4 <- eventReactive(c(input$select_Queries_4, input$radio_Queries_4, input$date_range_Queries_4, input$selectize_Queries_4, input$numeric_Queries_4),{
    
    t <- tolower(input$radio_Queries_4)
    
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
        hour_groups <- as.character(cut(seq(as.POSIXct(min(rv$request$hour)), by = "1 hour", length.out = 24), "hours"))
        
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
  output$text <- renderText({
    # if the last element of input$file is integer (it means no file is currently selected), return warning.
    validate(
      need(!is.integer(tail(input$file,1)), "No file is currently selected. Please select a file.")
    )
    
    # return the name of the selected file
    tail(unlist(input$file$files),1)
    
  })
  
  # display the name of the selected directory
  output$text2 <- renderText({
    # if the last element of input$dir is integer (it means no file is currently selected), return warning.
    validate(
      need(!is.integer(input$dir), "No directory is currently selected. Please select a directory.")
    )
    input_dir <- parseDirPath(roots = volumes, input$dir)
    
    # set the working directory as the selected directory
    setwd(input_dir)
    
    # return the name of the selected directory(now also the working directory)
    print(getwd())
    
  })
  
  # show the input data as table (from selected file)
  output$table <- renderDataTable({
    
    # create a dataframe using log data  
    rv$log_data %>% datatable(rownames = FALSE,
                              options = list(
                                lengthMenu = c(5,10,20,50),
                                autoWidth = TRUE,
                                scrollX = TRUE,
                                columnDefs = list(list(width = '4%', targets = c(1,2,3)))),
                              filter = "top")
    
  })
  
  # show the input data as table (from selected directory)
  output$table2 <- renderDataTable({
    rv$log_data %>% datatable(rownames = FALSE,
                              options = list(
                                lengthMenu = c(5,10,20,50),
                                autoWidth = TRUE,
                                scrollX = TRUE,
                                columnDefs = list(list(width = '4%', targets = c(1,2,3)))),
                              filter = "top")
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
             x = "Days",
             y = "Total Requests") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 1.5)), tooltip = c("y","x"))
  })
  
  # Interactive plot for HTTP Requests (Status Codes)
  output$plot_Requests_2 <- renderPlotly({
    
    # return an error message if the date ranges for rv_Requests_2() is not applicable.
    validate(
      need(nrow(rv_Requests_2()) > 0, "There is no data between selected dates. Please select a different date range.")
    )
    
      ggplotly(
        rv_Requests_2() %>% 
          ggplot(aes_string(x = tolower(input$radio_Requests_2), y = "count", fill = "status")) +
          geom_bar(position = "dodge", width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#003300") +
          facet_grid(~status, scales = "free_x", space = "free_x") +
          labs(title = "Number of Requests Over Selected Timeframe - Facet by Status Codes", 
               x = "Days",
               y = "Total Requests") +
          theme_light() +
          theme(axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 1.5),
                strip.background = element_rect(fill = "#CCFFCC"),
                strip.text = element_text(color = "#003300",
                                          size = rel(1.25))), tooltip = c("y","x"))
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
                axis.title.x = element_blank(),
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
               x = "Days",
               y = "Total Requests") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 1.5)), tooltip = c("y","x"))
      
    }
  })
  
  # Interactive plot for HTTP Requests (IP Addresses)
  output$plot_Requests_4 <- renderPlotly({
    
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
          geom_bar(aes(text = paste("IP Address' Name: ", ip_address, "\nRequest Count: ", count)),
                   width = 0.75, stat = "identity", col = "#99FF99", fill = "#99FF99")+ 
          geom_text(aes(label = count, vjust = -0.5, fontface = "bold"),  col = "#003300", check_overlap = TRUE) +
          annotate("text", label = "Mean", x = rv_Requests_4()$ip_address[index], y = -2*(rv_Requests_4()$count[index]), size = 4.5, colour = "blue") +
          labs(title = "IP Addresses with the Most / the Least # of Requests",
               x = "IP Addresses",
               y = "Number of Requests") +
          theme_light() +
          theme(axis.text.x = element_blank(),
                axis.title.x = element_blank(),
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
               x = "Days",
               y = "Total Requests") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 1.5)), tooltip = c("y","x"))
      
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
             x = "Days",
             y = "Total Queries") +
        theme_light() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size = 1.5)), tooltip = c("y","x"))
  })
  
  # Interactive plot for SPARQL Queries (Status Codes)
  output$plot_Queries_2 <- renderPlotly({
    
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
          labs(title = "Number of Queries Over Selected Timeframe - Facet by Status Codes",
               x = "Days",
               y = "Total Queries") +
          theme_light() +
          theme(axis.text.x = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 1.5),
                strip.background = element_rect(fill = "#CCFFCC"),
                strip.text = element_text(color = "#003300",
                                          size = rel(1.25))), tooltip = c("y","x"))
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
               x = "Days",
               y = "Total Queries") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 1.5)), tooltip = c("y","x"))
      
    }
  })
  
  # Interactive plot for SPARQL Queries (IP Addresses)
  output$plot_Queries_4 <- renderPlotly({
    
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
                axis.title.x = element_blank(),
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
               x = "Days",
               y = "Total Queries") +
          theme_light() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title.x = element_blank(),
                axis.title.y = element_text(size = 1.5)), tooltip = c("y","x"))
      
    }
  })
}

shinyApp(ui = ui,server = server)