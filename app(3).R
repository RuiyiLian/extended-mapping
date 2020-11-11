#Package install
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(usmap)

#data input
r <- read_csv("C:/Users/LXD/Documents/R/615 mapping/shiny/r(1).csv")


ui <- fluidPage(
   
   # Application title
    titlePanel("Hurricane Data Bank"),
    
    #storm select
    fluidRow(
      column(4,
             selectInput("storm",
                         "Disaster ID:",
                         c("All",
                           unique(as.character(r$disasterNumber))))),
      column(4,
             selectInput("state",
                         "State:",
                         c("All",
                           unique(as.character(r$stateCode))))),
      column(4,
             selectInput("type",
                         "IncidentType:",
                         c("All",
                           unique(as.character(r$incidentType))))),
    ),
  
 mainPanel(
   tabsetPanel(
     tabPanel("US Plot", plotOutput("plot")), 
     tabPanel("State Plot", plotOutput("plot1")), 
     tabPanel("Table",DT::dataTableOutput("table1"))
   )
 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #table
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- r
    if (input$storm != "All") {
      data <- data[data$disasterNumber == input$storm,]
    }
    if (input$state != "All") {
      data <- data[data$stateCode == input$state,]
    }
    if (input$type != "All") {
      data <- data[data$incidentType == input$type,]
    }
    
    data<-data%>%group_by(disasterNumber,county,state)%>%summarise(projectAmount=sum(projectAmount),countyCode=max(countyCode),stateNumberCode=max(stateNumberCode),incidentType=max(incidentType),.groups = 'drop')
    
      }))
  
  #US map
  output$plot <- renderPlot({
    data <- r
    if (input$storm != "All") {
      data <- data[data$disasterNumber == input$storm,]
    }
    if (input$state != "All") {
      data <- data[data$stateCode == input$state,]
    }
    if (input$type != "All") {
      data <- data[data$incidentType == input$type,]
    }
    
    data<-data%>%group_by(disasterNumber,county,state)%>%summarise(projectAmount=sum(projectAmount),countyCode=max(countyCode),stateNumberCode=max(stateNumberCode),incidentType=max(incidentType),.groups = 'drop')
    data<-data%>%group_by(state)%>%summarise(projectAmount=sum(projectAmount),countyCode=max(countyCode),stateNumberCode=max(stateNumberCode),incidentType=max(incidentType),.groups = 'drop')
    
     plot_usmap(data = data, values = "projectAmount", color = "red") + 
      scale_fill_continuous(
        low = "blue", high = "red", name = "Disaster", label = scales::comma
      ) + labs(title = "US Disaster") + theme(legend.position = "right")
  })
  
  #State map
  output$plot1 <- renderPlot({
    data <- r
    if (input$storm != "All") {
      data <- data[data$disasterNumber == input$storm,]
    }
    if (input$state != "All") {
      data <- data[data$stateCode == input$state,]
    }
    if (input$type != "All") {
      data <- data[data$incidentType == input$type,]
    }
    
    data<-data%>%group_by(disasterNumber,county,state)%>%summarise(projectAmount=sum(projectAmount),countyCode=max(countyCode),stateNumberCode=max(stateNumberCode),incidentType=max(incidentType),.groups = 'drop')
    a<-data$state
    data<-data%>%group_by(county)%>%summarise(projectAmount=sum(projectAmount),countyCode=max(countyCode),stateNumberCode=max(stateNumberCode),incidentType=max(incidentType),.groups = 'drop')
    data<-data%>%filter(county != "Statewide")
    data<-data %>% rowwise %>% mutate(fips = 1000*stateNumberCode+countyCode)
    plot_usmap(regions = "county", data = data, values = "projectAmount",include = a ,color = "red") + 
      scale_fill_continuous(
        low = "blue", high = "red", name = "Disaster", label = scales::comma
      ) + labs(title = "US Disaster") + theme(legend.position = "right")
  })
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)

