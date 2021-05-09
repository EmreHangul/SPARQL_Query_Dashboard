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

# function for displaying loading icon
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
    sidebarSearchForm(textId = "search", buttonId = "searchbutton", label = "Search data")
  ),
  dashboardBody(shiny_busy(), 
                shinyDashboardThemes(theme = "poor_mans_flatly"),
                tags$head(tags$style(HTML(".shiny-output-error-validation {color: #FF9900; font-size: 20px; font-weight: bold}"))),
                tags$head(tags$style("#text{font-size: 18px;font-weight: bold}")
                ),
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
                                                     viewtype = "detail"))),
                           fluidRow(column(width = 12, 
                                           align = "center", 
                                           wellPanel("Selected file name:",
                                                     textOutput("text")))),
                           fluidRow(dataTableOutput("table"))
                  ),
                  tabPanel(title = "Requests",
                           fluidRow(
                             column(width = 8,
                                    plotlyOutput("first")),
                             column(width = 4,
                                    dateRangeInput(inputId = "date_range_1",
                                                   label = "Choose Dates Between:",
                                                   start = "2020-01-01"),
                                    selectInput(inputId = "select_request",
                                                label = "Select an HTTP Request Type:",
                                                choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                                selected = "GET")))),
                  tabPanel(title = "Queries",
                           fluidRow(
                             column(width = 8,
                                    plotlyOutput("second")),
                             column(width = 4,
                                    dateRangeInput(inputId = "date_range_2",
                                                   label = "Choose Dates Between:",
                                                   start = "2020-01-01"),
                                    selectInput(inputId = "select_sparql",
                                                label = "Select a SPARQL Query Type:",
                                                choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")))))
              )
      )),
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
  
  # Server side file select code
  volumes <- c(Home = fs::path_wd(), 
               "R Installation" = R.home(), 
               getVolumes()())
  shinyFileChoose(input, "file", 
                  roots = volumes, 
                  session = session)
  
  # initialization of reactive input data
  rv <- reactiveValues(log_data = data.frame(),
                        request = data.frame())
  
  # update the reactive data when an input file is selected
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
              hour = logs$hour)
      
      rv$request <- requests
    
    }
  })

  # Reactive data for http requests
  rv_request <- eventReactive(input$select_request, {
    
    validate(
      need(grepl(input$select_request, rv$request$method)==TRUE,
            message = "No such requests are found! Please try another type of request.")
    )

    rv$request %>% 
      filter(grepl(input$select_request, rv$request$method)) %>% 
      select(method,day) %>% 
      filter(day >= input$date_range_1[1] & day <= input$date_range_1[2]) %>% 
      group_by(day) %>% 
      count(day) %>% 
      rename(count = n)
  })
  
  # Reactive data for sparql types
  rv_sparql <- eventReactive(input$select_sparql, {
    
    validate(
      need(grepl(paste0("sparql.*", input$select_sparql), rv$request$asset)==TRUE,
           message = "No such queries are found! Please try another type of query.")
    )
    
    rv$request %>% 
      filter(grepl(paste0("sparql.*", input$select_sparql), rv$request$asset)) %>% 
      select(method,day) %>%
      filter(day >= input$date_range_2[1] & day <= input$date_range_2[2]) %>%
      group_by(day) %>% 
      count(day) %>% 
      rename(count = n)
    
  })
  
  # Update date range 1 
  observeEvent(input$date_range_1[1], {
    end_date = input$date_range_1[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_1[2] < input$date_range_1[1]){
      end_date = input$date_range_1[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_1",
                         start = input$date_range_1[1],
                         end = end_date)
    
  })
  
  # Update date range 2
  observeEvent(input$date_range_2[1], {
    end_date = input$date_range_2[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range_2[2] < input$date_range_2[1]){
      end_date = input$date_range_2[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range_2",
                         start = input$date_range_2[1],
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
  
  # show the input data as table
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
  
  # Interactive plot for HTTP Requests
  output$first <- renderPlotly({
    
    ggplotly(
    rv_request() %>% 
      ggplot(aes(x=day,y=count)) +
      geom_bar(stat = "identity", col = "#CCFF99", fill = "#CCFF99")+ #use this with supplying both x,y; otherwise only x.
      geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#0000FF") +
      labs(title = "Number of Requests Over Days",
           x = "Days",
           y = "Total Requests") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=1.5)), tooltip = c("y","x"))
    })
  
  # Interactive plot for SPARQL Queries
  output$second <- renderPlotly({
    
    ggplotly(
    rv_sparql() %>% 
      ggplot(aes(x=day,y=count)) +
      geom_bar(stat = "identity", col = "#CCFF99", fill = "#CCFF99")+ #use this with supplying both x,y; otherwise only x.
      geom_text(aes(label = count, vjust = -0.5, fontface = "bold"), col = "#0000FF") +
      labs(title = "Number of Queries Over Days",
           x = "Days",
           y = "Total Queries") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=1.5)), tooltip = c("y","x"))
  })
  
}

shinyApp(ui = ui,server = server)