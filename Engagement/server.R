library(shiny)
source("Engagement.R")

# Define server logic for random distribution application
shinyServer(function(input, output) { 
 
  dataFile <- reactive({
    inFile <- input$file

    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
         quote=input$quote)
  })

  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste('r', dist, '(', n, ')', sep=''))
  })
  
  # Generate an HTML table view of the data
  output$retentionMatrix <- renderTable({
    if (is.null(dataFile()))
      return(NULL)

    triggerEvent <- as.character(input$triggerEvent)
    followupEvent <- as.character(input$followupEvent)
    groupBy <- input$groupBy

    data <- dataFile()

    colnames(data)[input$colUID] <- "uid"
    colnames(data)[input$colTimestamp] <- "timestamp"
    colnames(data)[input$colEvents] <- "events"

    periodStart <- as.Date(input$startDate)
    periodEnd <- as.Date(input$endDate)

    #events <- filter(triggerEvent, followupEvent, data, periodStart, periodEnd)
    #data
    #data.frame(c(triggerEvent, followupEvent, groupBy, as.Date(periodStart), as.Date(periodEnd)))

    retention <- retentionMatrix(triggerEvent, followupEvent, data, periodStart = NULL, periodEnd = NULL, groupBy)

  })



  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.

    dataFile()
  })
  
})