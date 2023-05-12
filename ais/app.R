library(shiny)
library(tidyverse)
library(echarts4r)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(shinyjs)

dataframe <- read.csv("file_ais_ext_aa.csv", na.strings = "")
#str(dataframe)
#view(dataframe)

dataframe$BaseDateTime = gsub("T", " ", dataframe$BaseDateTime)
dataframe$BaseDateTime = as.POSIXct(dataframe$BaseDateTime, format = "%Y-%m-%d %H:%M:%S")
dataframe$TransceiverClass <- as.factor(dataframe$TransceiverClass)
dataframe$VesselType <- as.factor(dataframe$VesselType)
dataframe$Status <- as.factor(dataframe$Status)
#str(dataframe)

vessel_Name = unique(dataframe$VesselName)
vessels_Name = c('All', vessel_Name)

vessel_Type = unique(dataframe$VesselType) %>% na.omit()
vessels_Type = c('All', vessel_Type)
vessels_Type

dataframe <- mutate(dataframe, cntnt=paste0('<strong>Name: </strong>',VesselName,
                                  '<br><strong>MMSI:</strong>', MMSI,
                                  '<br><strong>Last Observed:</strong>', BaseDateTime,
                                  '<br><strong>SOG:</strong>', SOG,
                                  '<br><strong>COG:</strong>', COG,
                                  '<br><strong>Heading:</strong>', Heading,
                                  '<br><strong>IMO:</strong>', IMO,
                                  '<br><strong>Call Sign:</strong>', CallSign,
                                  '<br><strong>Vessel Type:</strong>', VesselType,
                                  '<br><strong>Status:</strong>', Status,
                                  '<br><strong>Cargo:</strong>', Cargo,
                                  '<br><strong>Transceiver Class:</strong>', TransceiverClass))

ui <- function(request) {
  dashboardPage(
    
    # Application title
    dashboardHeader(title = "Posideon Project"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Main Dashboard", tabName = "mainDashboard", icon = icon("gauge")),
        menuItem("Vessels Traffic Analysis", tabName = "vesselsTraffic", icon = icon("ship"))
      )
    ),
    dashboardBody(
      tabItems(
        # halaman 1
        tabItem(
          tabName = "mainDashboard",
          bookmarkButton(id = "bookmark1"),
          h2("Main Dashboard"),
          fluidRow(
            box(
              airDatepickerInput(
                inputId = "after",
                label = "After Datetime",
                value = '2018-01-01 00:00:00',
                timepicker = T
              ),
              width = 3
            ),
            box(
              airDatepickerInput(
                inputId = "before",
                label = "Before Datetime",
                value = min(dataframe$BaseDateTime) + 10 * 3600,
                timepicker = T
              ),
              width = 3
            ),
            box(
              selectInput("vessel_name", "Vessel Name", vessels_Name),
              width = 3
            ),
            box(
              selectInput("vessel_type", "Vessel Type", vessels_Type),
              width = 3
            )
          ),
          fluidRow(
            valueBoxOutput("date", width = 3),
            valueBoxOutput("shipRecord", width = 3),
            valueBoxOutput("shipNotUnderCommand", width = 3),
            valueBoxOutput("shipUnStatus", width = 3)
          ),
          fluidRow(
            box(
              title = "Map (Last Position)",
              width = 12,
              leafletOutput("map")
            )
          ),
          fluidRow(
            box(
              title = "Activity Time Chart",
              width = 3,
              echarts4rOutput("activity")
            ),
            box(
              title = "Vessel Type Distribution",
              width = 3,
              echarts4rOutput("vesselType")
            ),
            box(
              title = "Vessel Status Distribution",
              width = 3,
              echarts4rOutput("vesselStatus")
            ),
            box(
              title = "Transceiver Class Distribution",
              width = 3,
              echarts4rOutput("transceiver")
            )
          )
        ),
        # halaman 2
        tabItem(
          tabName = "vesselsTraffic",
          bookmarkButton(id = "bookmark2"),
          h2("Vessel Analysis"),
          fluidRow(
            column(
              airDatepickerInput(
                inputId = "pickDate",
                label = "Pick a Date",
                value = '2018-01-01'
              ),
              width = 3
            ),
            column(
              selectInput("vessel_mmsi", "Vessel MMSI", unique(dataframe$MMSI)),
              width = 3
            )
          ),
          fluidRow(
            column(
              width = 3,
              actionButton("filter", "Filter"),
              style="margin-bottom : 10px"
            )
          ),
          fluidRow(
            column(
              shinyjs::hidden(
                div(
                  id = "traffic",
                  box(
                    title = "Vessel Traffic Map",
                    leafletOutput("mapTraffic")
                  )
                )
              ),
              width = 6
            ),
            column(
              infoBoxOutput("v_name", width = 6),
              infoBoxOutput("imo", width = 6),
              infoBoxOutput("lastAct", width = 6),
              infoBoxOutput("callSign", width = 6),
              infoBoxOutput("transClass", width = 6),
              infoBoxOutput("shipDim", width = 6),
              shinyjs::hidden(
                div(
                  id = "historyBox",
                  box(
                    title = "Historical Data",
                    DT::dataTableOutput("history"),
                    width = 12
                  )
                )
              ),
              width = 6
            )
          )
        )
      )
    )
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  setBookmarkExclude(c("bookmark1", "bookmark2"))
  
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
  
  data_filter = reactive({
    dataframe %>% 
      arrange(BaseDateTime) %>% 
      filter(
        BaseDateTime >= lubridate::as_datetime(input$after),
        BaseDateTime <= lubridate::as_datetime(input$before),
        if (input$vessel_name == "All") {
          # Return all rows if "All" is selected for vessel name
          TRUE
        } else {
          VesselName == input$vessel_name
        },
        if (input$vessel_type == "All") {
          # Return all rows if "All" is selected for vessel type
          TRUE
        } else {
          VesselType == input$vessel_type
        }
      )
  })
  dua = reactive({
    dataframe %>% filter(Status == 2)%>%nrow()
  })
  status = reactive({
    dataframe %>%
      group_by(Status) %>%
      summarize(count=n_distinct(MMSI))
  })
  trafficAnalysisOutput = eventReactive(input$filter,{
    dataframe %>%
      arrange(BaseDateTime) %>%
      filter(
        as.Date(BaseDateTime) == input$pickDate,
        MMSI == input$vessel_mmsi
      )
  })
  trafficAnalysisOutput2 = observeEvent(input$filter,{
    
    shinyjs::show(id="trafficMap")
    output$mapTraffic <- renderLeaflet({
      
      leaflet() %>% addTiles() %>% 
        addArrowhead(data = trafficAnalysisOutput(), lng = trafficAnalysisOutput()$LON, lat = trafficAnalysisOutput()$LAT)%>%
        
        addDrawToolbar()
    })  
    
    
    output$v_name <- renderInfoBox({
      infoBox(
        "Vessel Name", trafficAnalysisOutput()$VesselName[1], icon = icon("ship")
      )
    })
    output$imo <- renderInfoBox({
      infoBox(
        "IMO",trafficAnalysisOutput()$IMO[1], icon = icon("hashtag")
      )
    })
    output$lastAct <- renderInfoBox({
      infoBox(
        "Last Activity", max(trafficAnalysisOutput()$BaseDateTime), icon = icon("clock")
      )
    })
    output$callSign <- renderInfoBox({
      infoBox(
        "Call Sign", trafficAnalysisOutput()$CallSign[1], icon = icon("phone")
      )
    })
    output$transClass <- renderInfoBox({
      infoBox(
        "Transceiver Class", trafficAnalysisOutput()$TransceiverClass[1], icon = icon("signal")
      )
    })
    output$shipDim <- renderInfoBox({
      infoBox(
        "Ship Dimension", paste(trafficAnalysisOutput()$Length[1],"x",trafficAnalysisOutput()$Width[1]) , icon = icon("cube")
      )
    })
    
    #Data Table
    shinyjs::show(id="historyBox")
    output$history <- DT::renderDataTable(trafficAnalysisOutput()[c('BaseDateTime','SOG','COG','Heading','Status','Draft')])
    
  })
  # halaman 1
  output$date = renderValueBox({
    valueBox(
      value = tags$h3(max(dataframe$BaseDateTime), style="font-size : 24px"), 
      "Last Incoming Data", 
      icon = icon("calendar")
    )
  })
  output$shipRecord = renderValueBox({
    valueBox(
      value = shipRecord <- length(unique(dataframe$MMSI)),
      subtitle = "Number of Ships Recorder",
      icon = icon("ship")
    )
  })
  output$shipNotUnderCommand = renderValueBox({
    valueBox(
      value = dua(),
      subtitle = "Ships Not Under Command",
      icon = icon("times")
    )
  })
  output$shipUnStatus = renderValueBox({
    statusUnKnow = status() %>% filter(is.na(Status))
    valueBox(
      value = statusUnKnow$count,
      subtitle = "Ships with Unknow Status",
      icon = icon("question")
    )
  })
  output$map <- renderLeaflet({
    data_map <- data_filter() %>%
      distinct(MMSI, .keep_all = T)
    
    leaflet(data_map) %>% addTiles() %>%
      addCircleMarkers(
        clusterOptions = markerClusterOptions(),
        clusterId = "quakesCluster",
        popup = ~paste0(
          cntnt
        )) %>%
      addDrawToolbar()
  })
  observeEvent(input$leafmap_draw_start, {
    print("Start of drawing")
    print(input$leafmap_draw_start)
  })
  observeEvent(input$leafmap_draw_stop, {
    print("Stopped drawing")
    print(input$leafmap_draw_stop)
  })
  observeEvent(input$leafmap_draw_new_feature, {
    print("New Feature")
    print(input$leafmap_draw_new_feature)
  })
  
  # activity time belum
  output$activity <- renderEcharts4r({
    
    activityTime <- data_filter() %>%
      group_by(BaseDateTime=floor_date(BaseDateTime, '1 hour'))%>%
      summarise(count = n())
    
    activityTime %>% 
      e_charts(BaseDateTime) %>% 
      e_line(count, name = "activity count")%>%
      e_tooltip(trigger = 'axis')
  })
  
  output$vesselType <- renderEcharts4r({
    
    vesselType <- data_filter() %>%
      group_by(VesselType) %>% 
      summarize(count = n_distinct(MMSI)) %>%
      arrange(count)
    
    vesselType %>% 
      e_charts(VesselType) %>% 
      e_polar(count) %>% 
      e_angle_axis() %>% 
      e_radius_axis(VesselType) %>% 
      e_bar(count, coord_system = "polar", name = "n")%>%
      e_tooltip(trigger = 'axis')
  })
  output$vesselStatus = renderEcharts4r({
    statusData <- data_filter() %>%
      group_by(Status) %>% 
      summarize(count = n_distinct(MMSI)) %>%
      arrange(count)
    
    statusData %>% 
      e_charts(Status) %>% 
      e_polar(count) %>% 
      e_angle_axis() %>% 
      e_radius_axis(Status) %>% 
      e_bar(count, coord_system = "polar", name = "n")%>%
      e_tooltip(trigger = 'axis')
  })
  output$transceiver <- renderEcharts4r({
    
    pie_data <- data_filter() %>% 
      group_by(TransceiverClass) %>% 
      summarize(count = n_distinct(MMSI))%>%
      arrange(desc(TransceiverClass))
    
    pie_data %>%
      e_charts(TransceiverClass) %>%
      e_pie(count, name = "n") %>%
      e_legend(show = TRUE) %>%
      e_tooltip()
  })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")
