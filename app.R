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
library(fs)
#####Live content download. Later do this!##############
#x <- GET(url = URL)
#data <- content(x) 
########################################################
options(shiny.maxRequestSize = 200*1024^2)

# construct a list of all the data from the log files
initial_data <- lapply(unzip("data.zip"), read_combined)

# add all the log data into a 1 common data object
i <- 1
data <- data.frame()

while (i<=length(initial_data)) {
  data <- rbind(data, initial_data[[i]])
  i = i + 1
}

####################DATA WRANGLING#############################
##############################################################

data$year <- format(as.POSIXct(data$timestamp), "%Y")
data$month <- format(as.POSIXct(data$timestamp), "%Y-%m")
data$day <- format(as.POSIXct(data$timestamp), "%Y-%m-%d")
data$hour <- format(as.POSIXct(data$timestamp), "%Y-%m-%d %H:%M")

request <- split_clf(data$request)
request <- cbind(request,
                 year = data$year,
                 month = data$month,
                 day = data$day, 
                 hour = data$hour)

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

################################DASHBOARD#########################################
##################################################################################

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
    tabItems(
      tabItem(tabName = "Home", 
              fluidRow(
                tabBox(width = 12,
                  title = "",
                  tabPanel(title = "Files",
                           fluidRow(
                             column(width = 3,
                                    shinyFilesButton(id = "file",
                                                     label = "Select File",
                                                     title = "Please select one or multiple files:",
                                                     multiple = TRUE,
                                                     icon = icon(lib = "font-awesome",
                                                                 name = "file-alt"),
                                                     viewtype = "detail"),
                                    textOutput(outputId = "text")),
                             column(width = 9,
                                    dataTableOutput("table"))
                           )
                  ),
                  tabPanel(title = "Requests",
                           fluidRow(
                             column(width = 8,
                                    plotlyOutput("first")),
                             column(width = 4,
                                    dateRangeInput(inputId = "date_range_1",
                                                   label = "Choose Dates Between:",
                                                   start = min(data$day),
                                                   end   = max(data$day)),
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
                                                   start = min(data$day),
                                                   end   = max(data$day)),
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

server <- function(input, output, session){
  # Server side selectfile codes
  volumes <- c(Home = fs::path_wd(), 
               "R Installation" = R.home(), 
               getVolumes()())
  shinyFileChoose(input, "file", 
                  roots = volumes, 
                  session = session)
  
  output$text <- renderText({
    
    validate(
      need(input$file$datapath == character(0), message = "No files are currently selected. Please select a file.")
    )
    
    input$file$datapath
  })

  output$table <- renderDataTable({
    
    # parse the file input into a more usable format
    input_file <- parseFilePaths(roots = volumes,
                               input$file)
      
    # construct a list of all the data from the log files
    initial_d <- lapply(input_file$datapath, read_combined)
      
    # add all the log data into a 1 common data object
    i <- 1
    log_data <- data.frame()
      
    while (i<=length(initial_d)) {
      log_data <- rbind(log_data, initial_d[[i]])
      i = i + 1
    }
    
    # create a dataframe using log data  
    log_data %>% datatable(rownames = FALSE,
                           options = list(
                              lengthMenu = c(5,10,20,50),
                              autoWidth = TRUE,
                              scrollX = TRUE,
                              columnDefs = list(list(width = '4%', targets = c(1,2,3)))),
                           filter = "top")
    
  })
  
  # Update date ranges 
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
  
  # Reactive for http requests
  rv <- eventReactive(input$select_request, {
    
        validate(
          need(grepl(input$select_request, request$method)==TRUE,
               message = "No such requests are found! Please try another type of request.")
        )
    
        request %>% 
          filter(grepl(input$select_request,method)) %>%
          select(method,day) %>% 
          filter(day >= input$date_range_1[1] & day <= input$date_range_1[2]) %>% 
          group_by(day) %>% 
          count(day) %>% 
          rename(count = n)
        
  })
  
  # Reactive for sparql types
  rv2 <- eventReactive(input$select_sparql, {
    
    validate(
      need(grepl(paste0("sparql.*", input$select_sparql), request$asset)==TRUE,
           message = "No such queries are found! Please try another type of query.")
    )
    
    request %>% 
      filter(grepl(paste0("sparql.*", input$select_sparql), asset)) %>% 
      select(method,day) %>%
      filter(day >= input$date_range_2[1] & day <= input$date_range_2[2]) %>%
      group_by(day) %>% 
      count(day) %>% 
      rename(count = n)
    
  })
  
  output$first <- renderPlotly({
    
    ggplotly(
    rv() %>% 
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
  
  output$second <- renderPlotly({
    
    ggplotly(
    rv2() %>% 
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