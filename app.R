# --- run these commands to install the packages
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("leaflet")
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("plotly")
library(shiny)
library(shinydashboard)
library(leaflet)
library(data.table)
library(ggplot2)
library(plotly)

ui <- pageWithSidebar(
  headerPanel("CSV Viewer"),
  sidebarPanel(
    fileInput('file1', 'CSV Viewer',
              accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    fluidRow(
      column(6,selectInput("xaxisGrp","X-Axis:", c("1"="1","2"="2"))),
      column(6,selectizeInput("yaxisGrp","Y-axis:", c("1"="1","2"="2")))
    ),
    radioButtons('sep', 'Separator',
                 c(Comma=',', Semicolon=';',Tab='\t'), ','),
    radioButtons('quote', 'Quote',
                 c(None='','Double Quote'='"','Single Quote'="'"),'"'),
    uiOutput("choose_columns")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot",plotlyOutput("plot")),
      tabPanel("Data", tableOutput('contents'))
    )
  )
)
server <- function(input, output,session) {
  dsnames <- c()
  
  data_set <- reactive({
    inFile <- input$file1
    
    if (is.null(inFile))
      return()
    
    data_set<-read.csv(inFile$datapath, header=input$header, 
                       sep=input$sep, quote=input$quote)
  })
  
  output$contents <- renderTable({data_set()})
  
  observe({
    dsnames <- names(data_set())
    cb_options <- list()
    cb_options[ dsnames] <- dsnames
    updateSelectInput(session, "xaxisGrp",
                       label = "X-Axis",
                       choices = cb_options,
                       selected = "")
    updateSelectizeInput(session, "yaxisGrp",
                             label = "Y-Axis",
                             choices = cb_options,
                             selected = "")
  })
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set", as.list(data_sets))
  })
  output$plot = renderPlotly(
    {
      df <- data_set()
      gp <- NULL
      if (!is.null(df)){
        xv <- input$xaxisGrp
        yv <- input$yaxisGrp
        if (!is.null(xv) & !is.null(yv)){
          if (sum(xv %in% names(df))>0){ # supress error when changing files
            mdf <- melt(df,id.vars=xv,measure.vars=yv)
            gp <- ggplotly (ggplot(data=mdf) + 
              geom_point(aes_string(x=xv,y="value",colour="variable")), width = 900, height = 600)
          }
        }
      }
      return(gp)
    }
  )
  output$choose_columns <- renderUI({
    
    if(is.null(input$dataset))
      return()
    colnames <- names(contents)
    checkboxGroupInput("columns", "Choose columns", 
                       choices  = colnames,
                       selected = colnames)
  }) 
}
shinyApp(ui, server)