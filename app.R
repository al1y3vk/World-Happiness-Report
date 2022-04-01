ui <- navbarPage(
  
  title = "Happiness", id = "main",
  
  tabPanel(title = "Maps", leafletOutput(outputId = "bbmap", height = "1220"), 
           absolutePanel(top = 75, right = 20, 
                         pickerInput(inputId = "year", label = "Select a Year", 
                                     choices = list("2015", "2016", "2017"),
                                     options = list(`live-search` = TRUE),
                                     multiple = FALSE))),
  
  tabPanel(title = "Analysis", 
           
           sidebarPanel(
             width = 2,
             
             pickerInput(inputId = "ayear", label = "Select a Year", 
                         choices = list("2015", "2016", "2017"),
                         options = list(`live-search` = TRUE),
                         multiple = FALSE),
           ),
           
           mainPanel(
             column(4, plotOutput("CorrPlots", width = "500px", height = "400px")),
             
             column(4, plotOutput("HappyCont", width = "550px", height = "400px")),
             
             column(1, plotOutput("PredPlot", width = "400px", height = "400px")),
           )
           
  ),
  
  tabPanel("Data", DT::dataTableOutput("data"))
)

server <- function(input, output, session) {

    correctData <- reactive({
      if (input$year == "2015") {
        New_2015
      }
      else if (input$year == "2016") {
        New_2016
      }
      else {
        New_2017
      }
    })
    
    corrData <- reactive({
      if (input$ayear == "2015") {
        CorData15
      }
      else if (input$ayear == "2016") {
        CorData16
      }
      else {
        CorData17
      }
    })
    
    barData <- reactive({
      if (input$ayear == "2015") {
        MeltedAggr15
      }
      else if (input$ayear == "2016") {
        MeltedAggr16
      }
      else {
        MeltedAggr17
      }
    })
    
    predData <- reactive({
      if (input$ayear == "2015") {
        actual15
      }
      else if (input$ayear == "2016") {
        actual16
      }
      else {
        actual17
      }
    })
    
  New_2015$popupText
    output$bbmap <- renderLeaflet({
        
      leaflet(correctData()) %>% addTiles() %>%
        addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~popupText)
      
    })
    
    observe({
      
      leafletProxy("bbmap", data = correctData()) %>% clearShapes() %>% 
        addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~popupText)
      
    })
    
    output$data <- DT::renderDataTable(datatable(
      
      correctData(), filter = 'top', colnames = colnames(correctData())
      
    ))
    
    output$CorrPlots <- renderPlot({
      corrplot(corrData(), method = "number")
    })
    
    output$HappyCont <- renderPlot({
      
      ggplot(data = barData(), aes(y = Value, x = Continent, color = Continent, fill = Continent)) + 
        geom_bar(stat = "identity") + facet_wrap(~Variable) + theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = "Average value of happiness variables for different continents", y = "Average value")
      
    })
    
    output$PredPlot <- renderPlot({
      
      ggplot(predData(), aes(Actual, Prediction)) +
        geom_point() + theme_bw() + geom_abline() +
        labs(title = "Multiple Linear Regression", 
             x = "Actual happiness score", y = "Predicted happiness score") +
        theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
              axis.title = element_text(family = "Helvetica", size = (10)))
      
    })
    
}

runApp(shinyApp(ui, server), launch.browser = TRUE)
