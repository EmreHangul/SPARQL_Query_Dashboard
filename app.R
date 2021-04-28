library(shiny)
library(shinydashboard)
library(webreadr)
library(DT)
library(magrittr)
library(ggplot2)
library(ggplot2)
library(dplyr)
library(plotly)
library(dashboardthemes)
#####Live content download. Later do this!##############
#x <- GET(url = URL)
#data <- content(x) 
########################################################
options(shiny.maxRequestSize = 200*1024^2)

# read file names from the zipped file
file_names <- as.character(unzip("data.zip", list = TRUE)$Name)[-c(1:3)]

# delete the unwanted strings from the file names
file_names <- substr(file_names,44,3000)
file_names <- strsplit(file_names,".gz")
file_names <- unlist(file_names)

# construct a list of all the data from the log files
initial_data <- lapply(file_names, read_combined)

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
                tabBox(width = 8,
                  title = "",
                  tabPanel(title = "Files",
                    dataTableOutput("table")),
                  tabPanel(title = "Requests",
                    plotlyOutput("first")
                  ),
                  tabPanel(title = "Queries",
                    plotlyOutput("second")
                  )
                ),
                column(width = 4,
                  fluidRow(
                    title= "Choose Date Between",
                    fileInput(inputId = "file", 
                              label = "Choose a log-file: (Max. 200MB)",
                              buttonLabel = "Search"),
                    dateRangeInput(inputId = "date_range",
                                   label = "Choose Dates Between:",
                                   start = min(data$day),
                                   end   = max(data$day)),
                    selectInput(inputId = "select_request",
                                label = "Select an HTTP Request Type:",
                                choices = c("GET", "POST", "PUT", "PUSH", "HEAD", "OPTIONS", "DEBUG"),
                                selected = "GET"),
                    selectInput(inputId = "select_sparql",
                                label = "Select a SPARQL Query Type:",
                                choices = c("SELECT", "CONSTRUCT", "DESCRIBE", "ASK")),
                    verbatimTextOutput(outputId = "text")
                  )
                )
              )
      ),
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
  
  # Update date ranges 
  observeEvent(input$date_range[1], {
    end_date = input$date_range[2]
    
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if(input$date_range[2] < input$date_range[1]){
      end_date = input$date_range[1]
    }
    updateDateRangeInput(session,
                         inputId = "date_range",
                         start = input$date_range[1],
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
          filter(day >= input$date_range[1] & day <= input$date_range[2]) %>% 
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
      filter(day >= input$date_range[1] & day <= input$date_range[2]) %>%
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
 
  output$table <- renderDataTable({
      data %>% datatable(rownames = FALSE,
                         options = list(
                                        lengthMenu = c(5,10,20,50),
                                        autoWidth = TRUE,
                                        scrollX = TRUE,
                                        columnDefs = list(list(width = '4%', targets = c(1,2,3)))),
                         filter = "top")
  })
  
}

shinyApp(ui = ui,server = server)