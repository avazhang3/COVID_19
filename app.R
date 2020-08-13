
library(shiny)
library(shinydashboard) 
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(quantmod)
library(scales)
library(plotly)
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(rvest)
library(geosphere)
library(dplyr)
library(ggmap)
library(mapproj)
library(maps) #package for US map information so we can reference in line 31
library(RCurl) #package for loading data from github



address1<-read.csv("geocoded.csv")
address1$ID=c(1:4295)
address1<-address1[c(13,1,2,3,4,5,6,7,8,9,10,11,12)]

icon.glyphicon <- makeAwesomeIcon(icon= 'flag', markerColor = 'blue', iconColor = 'black')
icon.fa <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', library='fa', iconColor = 'black')
icon.ion <- makeAwesomeIcon(icon = 'home', markerColor = 'green', library='ion')

register_google(key="AIzaSyD-KN6cZ6gSeJ3Emcnmf1wfWKp371h83fE")

ui <- dashboardPagePlus(
        header = dashboardHeaderPlus(title = "COVID-19", 
                                 enable_rightsidebar = TRUE, rightSidebarIcon = "gears"),
        sidebar = dashboardSidebar(
                            sidebarMenu(
                                      menuItem("Introduction",tabName="page1",icon=icon("home")),
                                      menuItem("COVID Map", icon = icon("map"),
                                               menuSubItem("Map", tabName = "page2"),
                                               menuSubItem("Compare Data", tabName = "page5")),
                                      menuItem("Drive-through Testing Site",tabName = "page3", icon = icon("search")),
                                               
                                      menuItem("Data Table Overview",tabName="page4",icon=icon("database"))
                                      #menuItem("Testing Center Map", tabName = "page2", icon = icon("thumb-tack")),
          
                                                    
                                       )
                                   ),
        body = dashboardBody(
          
                      tabItems(
                            tabItem(tabName="page1",
                    
                                    fluidRow(
                                                box(
                                                       title = "About the Project", solidHeader = TRUE,
                                                       status = "success", width = 12, collapsible = TRUE,
                                                       column(12, 
                                                              tags$div(
                                                                      tags$span("This project,",
                                                                               tags$strong("COVID 19,"), 
                                                                               "is a shiny dashboard application designed to provide user the information about the COVID-19 in the US starting in the early March till this day. 
                                                                               This site could be used as a COVID Guide site to track the total number of COVID cases and deaths as well as locate the nearest drive-through testing sites if you have any COVID symptoms.
                                                                               ", 
                                                                               style = "font-size:16px"
                                                                               ),
                                                                       br(),
                                                                       br(),
                                                                       fluidRow(
                                                                                       column(6, 
                                                                                              tags$li(
                                                                                                tags$strong("COVID Map"), 
                                                                                                "shows the total number of active COVID cases and total deaths in the US based on county till the selected date."
                                                                                                      )
                                                                                              ),
                                                                                        column(6, tags$li(tags$strong("Drive-through Testing Sites"),"shows the nearest testing location based on the input address or zipcode."),br()
                                                                                              ),
                                                                                       hr(),
                                                                                       column(6, tags$li(tags$strong("Data Table Overview"),"shows the overview of the data tables used in the COVID Map and Drive-though Testing Sites tabs."),br()
                                                                                       )
                                                                                 )
                                                                       )
                                                               )
                                                       )
                                                ),
                                    fluidRow(
                                      box(
                                        title = "Datasets", solidHeader = TRUE,
                                        status = "primary", width = 12, collapsible = TRUE,
                                        column(12, 
                                               tags$div(
                                                 tags$span("The dataset used for the COVID Map could be found on the",tags$i("github.com"),"and the dataset used for the drive-through sites are web scraped from the",tags$i("GoodRx.com."),
                                                 "The research team from GoodRx.com
                                                           compiled the data from a variety of sources including health department websites, online news websites, hospital and clinic websites, and social media.",style = "font-size:16px"),
                                                 br(), br(),
                                                 box(tags$li(tags$strong("COVID Map Source: "),tags$a(href ="https://github.com/CSSEGISandData/COVID-19",target="_blank","Github.com")),
                                                 tags$li("This merged dataset contains total",tags$strong("87949"), "rows and ", tags$strong("8"), "columns as of July 24th, 2020.")
                                                 ),
                                              
                                                 box(tags$li(tags$strong("Drive-through Testing Sites Source: "),tags$a(href ="http://www.goodrx.com/blog/drive-thru-coronavirus-testing-near-me/",target="_blank","GoodRx.com")),
                                                     tags$li("This dataset contains the data attributes from on the GoodRx website and the coordinates extracted using Google API. It contains ",tags$strong("4296"), "rows and ", 
                                                             tags$strong("12"), "columns as of July 24th, 2020."))
                                               ))
                                        )
                                      )
                                    ,
                                    
                                    fluidRow(
                                      box(
                                        title = "About Us", solidHeader = TRUE, 
                                        status = "info", width = 12, collapsible = TRUE,
                                        column(12, 
                                               tags$div(
                                                 fluidRow(
                                                   
                                                   column(12, tags$div("Team members are all from MSIS & MSBA program at Carey Business School, Johns Hopkins University:", style = "font-size:16px"),
                                                          br(),
                                                          tags$li(tags$strong("Hanxin Zhang:"), "A recent graduate from MSBA program."),br(),
                                                          tags$li(tags$strong("Zhihao Zhou:"), "A recent graduate from MSIS program."), br(),
                                                          tags$li(tags$strong("Yaosong Yu:"), "A recent graduate from MSIS program."),br(), 
                                                          tags$li(tags$strong("Yuxiang Hao:"), "A recent graduate from MSIS program.")
                                                   )
                                                 )
                                               )),
                                        br(),br(),
                                        
                                        box(tags$div("If you have any suggestions, questions, or comments for this app, 
                       please let us know by sending an email to ", 
                                                     tags$a(href = "mailto: hzhan139@jhu.edu", "hzhan139@jhu.edu"),".",style = "font-size:12px")),
                                        br())
                                    )#fluidrow
                                      
                                    ),#tabitem
                            
                            tabItem(tabName = "page2",
                                    fluidRow(
                                      tags$h2("COVID-19 in the US by State", align = "center"),
                                      tags$h3("This map shows COVID-19 confirmed cases and deaths in the US. The data is cumulative as of the selected date.
                                               The data starts on 2020-4-12. Click on the points to view detailed information. To compare data from different dates,
                                              go to 'Compare Map' tab.", align = "center")
                                    ),
                                    fluidRow(
                                      box(
                                          dateInput("mapdate","Select Date", value = as.Date("2020-07-01"),
                                                        min = as.Date("2020-04-12"), max = Sys.Date()-2, format = "mm-dd-yyyy"),width = 4,
                                          ),
                                          
                                      box(leafletOutput("dataMap"),width = 8)
                                      ),
                            ),
                            
                            tabItem(tabName = "page5",
                                    fluidRow(
                                      tags$h2("COVID-19 Compare Map", align = "center"),
                                      tags$h3("The two maps show COVID-19 confirmed cases and deaths in the US in different dates.
                                               The first date is 2020-4-12, and the second date is today. 
                                              Type in a state name to see the comparison graph ", align = "center")
                                    ),
                                    
                                    fluidRow(
                                      box(dateInput("comparedate1","Select Date", value = as.Date("2020-04-12"),
                                                    min = as.Date("2020-04-12"), max = Sys.Date()-1, format = "mm-dd-yyyy"),
                                          leafletOutput("compareMap1"), width = 6),
                                      box(dateInput("comparedate2","Select Date", value = as.Date("2020-07-23"),
                                                    min = as.Date("2020-04-12"), max = Sys.Date()-1, format = "mm-dd-yyyy"),
                                          leafletOutput("compareMap2"), width = 6)
                                    ),
                                    fluidRow(
                                      box(textInput("comparestate", "Type In the State","California"),width = 12)
                                    ),
                                    fluidRow(
                                      box(plotOutput("compareplot1")),
                                      box(plotOutput("compareplot2"))
                                    ),
                                    
                                    
                                    
                            ),
                          
                            
                            tabItem(tabName = "page3",
                                    h2("Search nearest Testing Sites"),
                                    textInput("text","Please enter your address or zipcode", value = "DC" ),
                                    
                                    numericInput("num", "Please enter range(miles)", value = as.numeric("2"), ),
                                    
                                    leafletOutput("myMap", width="100%")
                                    ),
                            
                            tabItem(tabName = "page4",
                                    h2("Data Tables of COVID Map & Testing Sites"),
                                    fluidRow(
                                      box(
                                        title = "About COVID Map Data Table", solidHeader = TRUE,
                                        status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                                        h5("This 'COVID Map Data Table' is a data table that shows the 8 attributes of Cases, Deaths, Region(state), Sub-region(county), Longtitude, 
                                           Latitude, Group Number, which represent the number of counties (sub regions) within the US and Order, which is the ranking of the total cases in the nation. 
                                           You can also search the data based on state in the search bar. ")
                                      )
                                    )
                                     ,
                                     fluidRow(
                                       dateInput("tabledata","Select date", value = as.Date("2020-07-01"),
                                                 min = as.Date("2020-04-12"), max = Sys.Date()-2, format = "mm-dd-yyyy"),width = 4,
                                       
                                       column(12, DT::dataTableOutput("COVIDMapTable"))
                                     ),
                                    
                                    fluidRow(
                                      box(
                                        title = "About Testing Sites Data Table", solidHeader = TRUE,
                                        status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                                        h5("In order to keep patients and healthcare providers safe, drive-thru coronavirus testing sites have
                                           been popping up across the country, If you are experiencing COVID-19 symptoms, such as fever, fatigue and shortness of breath, you can using the map to locate the nearest testing drive-thru to ensure your health.
                                           To find the type of testing site, look at the “Facility” column, or search one of the following: drive-thru, walk-thru, hospital, or clinic.While much is still unknown about the coronavirus disease (COVID-19), one thing is certain — it’s highly contagious. Patients who don’t have COVID-19 may be exposed to it while they are waiting for a test at a hospital or clinic. 
                                           
                                           Drive-thru testing for coronavirus was first implemented in South Korea and quickly adopted in other countries. The idea is simple: By keeping potentially sick patients in their cars and allowing plenty of ventilation throughout the testing facility, the risk of spreading COVID-19 is drastically reduced.
                                           This 'Testing Sites Data Table' is a data table that shows the 8 attributes:
                                             Cases, Deaths, Region(state), Sub-region(county), Longtitude, 
                                           Latitude, Group Number, which represent the number of counties (sub redions) within the US and Order, which is the ranking of the total cases in the nation. 
                                           You can also search the data based on state in the search bar. ")
                                      )
                                    ),
                                    fluidRow(
                                      dateInput("tabledata","Select date", value = as.Date("2020-07-01"),
                                                min = as.Date("2020-04-12"), max = Sys.Date()-2, format = "mm-dd-yyyy"),width = 4,
                                      
                                      column(12, DT::dataTableOutput("COVIDTestTable"))
                                    ),
                                    
                                    ########below is the dataset for testing center
                                    # fluidRow(
                                    #   column(12, DT::dataTableOutput("COVIDMapTable"))
                                    # ),
                                    
                                    
                                    )#this is the parenthesis for tab item 4 (not tab items)
     
                            
                            
                            
                            
                            
                            
                            )#this is the parenthesis for tab items 
              ),
          
        rightsidebar = rightSidebar(
          tags$a("COVID Map Data Source",href="https://github.com/CSSEGISandData/COVID-19",target="_blank"),
          
          br(),br(),
          #below write the code for the testing dataset source
          tags$a("Drive-through Testing Sites Data Source",href="http://www.goodrx.com/blog/drive-thru-coronavirus-testing-near-me/",target="_blank")
          
        ),
        title = ("DashboardPage"))
##############


#####################
server <- function(input, output, session) {
  
  getcoviddata = function(inputdate){
    date = format(inputdate, "%m-%d-%Y")
    
    url = paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports_us/",date,".csv")
    
    covid_raw_data = getURL(url)
    jhu_case = read.csv(text = covid_raw_data)
    
    covid_data = jhu_case[c(1,4,5,6,7)]
    
    colnames(covid_data)<- c("State", "Lat","Lng","Cases", "Deaths")
    covid_data$Date = date
    covid_data <- as_tibble(covid_data)
    
    return(covid_data)
    
  }


  
  
  output$dataMap = renderLeaflet({
    
    p = leaflet(getcoviddata(input$mapdate)) %>%
      addTiles() %>%
      setView(-105,39, zoom = 4.4) %>%
      addCircleMarkers(stroke = FALSE,
                       fillOpacity = 0.5,
                       fillColor = "red",
                       radius = ~sqrt(Cases/750),
                       popup = ~paste0(
                         "<b>", State, "</b><br/>",
                         "Total confirmed cases in ", State, " to this date: ", Cases, "<br/>",
                         "Total confirmed deaths in ", State, " to this date: ", Deaths
                       ))
    
    return(p)
    
  })
    
  output$compareMap1 = renderLeaflet({
    
    p = leaflet(getcoviddata(input$comparedate1)) %>%
      addTiles() %>%
      setView(-105,39, zoom = 4.4) %>%
      addCircleMarkers(stroke = FALSE,
                       fillOpacity = 0.5,
                       fillColor = "#007ACC",
                       radius = ~sqrt(Cases/750),
                       popup = ~paste0(
                         "<b>", State, "</b><br/>",
                         "Total confirmed cases in ", State, " to this date: ", Cases, "<br/>",
                         "Total confirmed deaths in ", State, " to this date: ", Deaths
                       ))%>%
      addCircleMarkers(stroke = FALSE,
                       fillOpacity = 0.5,
                       fillColor = "red",
                       radius = ~sqrt(Deaths/750),
      )
    
    
    return(p)
    
  })
  
  output$compareMap2 = renderLeaflet({
    
    p = leaflet(getcoviddata(input$comparedate2)) %>%
      addTiles() %>%
      setView(-105,39, zoom = 4.4) %>%
      addCircleMarkers(stroke = FALSE,
                       fillOpacity = 0.5,
                       fillColor = "#007ACC",
                       radius = ~sqrt(Cases/750),
                       popup = ~paste0(
                         "<b>", State, "</b><br/>",
                         "Total confirmed cases in ", State, " to this date: ", Cases, "<br/>",
                         "Total confirmed deaths in ", State, " to this date: ", Deaths
                       ))%>%
      addCircleMarkers(stroke = FALSE,
                       fillOpacity = 0.5,
                       fillColor = "red",
                       radius = ~sqrt(Deaths/750),
      )
    
    
    return(p)
    
  })
  
  output$compareplot1 = renderPlot({
    
    data1 = getcoviddata(input$comparedate1)
    data2 = getcoviddata(input$comparedate2)
    
    compare_dataset = rbind(data1, data2)
    
    compare_dataset = compare_dataset %>%filter(State == input$comparestate)
    
    j = ggplot(data = compare_dataset, mapping = aes(x= Date, y = Cases, fill = "#007ACC"))+
      geom_bar(stat = "identity")+
      geom_text(aes(label = Cases), vjust = 0.8 , color = "black", size = 6)+
      theme(plot.title = element_text(size = rel(2),hjust=0.5),
            axis.text.y=element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")+
      labs(title= paste0("The confirmed cases in ", input$comparestate , " on ", input$comparedate1, " and ", input$comparedate2))
    
    
    
    
    return(j)
  })
  
  output$compareplot2 = renderPlot({
    
    data1 = getcoviddata(input$comparedate1)
    data2 = getcoviddata(input$comparedate2)
    
    compare_dataset = rbind(data1, data2)
    
    compare_dataset = compare_dataset %>%filter(State == input$comparestate)
    
    j = ggplot(data = compare_dataset, mapping = aes(x= Date, y = Deaths, fill = "red"))+
      geom_bar(stat = "identity")+
      geom_text(aes(label = Deaths), vjust = 0.75 , color = "black", size = 6)+
      theme(plot.title = element_text(size = rel(2),hjust=0.5),
            axis.text.y=element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            legend.position = "none")+
      labs(title= paste0("Deaths in ", input$comparestate , " on ", input$comparedate1, " and ", input$comparedate2))
    
    return(j)
  })
   
  
  output$myMap = renderLeaflet({
   
      result <- geocode(input$text, output = "latlona", source = "google")
      address2<-data.frame(ID=1)
      address2$lon<-as.numeric(result[1])
      address2$lat<-as.numeric(result[2])
      
      address3<-distGeo(address2[,2:3],address1[,11:12])
      address4 <- data.frame(ID=address1[which.min(address3),1],DistanceBetween=min(address3)      )
      address5<-filter(address1, address4$ID==ID)
      lon1=address5$lon
      lat1=address5$lat
      if(input$num>0&input$num<3){
      address6<-filter(address1, lon<(lon1+input$num/40)&lon>(lon1-input$num/40), lat<(lat1+input$num/40)&lat>(lat1-input$num/40))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=13) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa) 
      tmap1
    } else if(input$num>2&input$num<7){
      address6<-filter(address1, lon<(lon1+input$num/40)&lon>(lon1-input$num/40), lat<(lat1+input$num/40)&lat>(lat1-input$num/40))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=11) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa) 
      tmap1
    } else if(input$num>6&input$num<10){
      address6<-filter(address1, lon<(lon1+input$num/40)&lon>(lon1-input$num/40), lat<(lat1+input$num/40)&lat>(lat1-input$num/40))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=10) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa) 
      tmap1
    } else if(input$num>9&input$num<16){
      address6<-filter(address1, lon<(lon1+input$num/40)&lon>(lon1-input$num/40), lat<(lat1+input$num/40)&lat>(lat1-input$num/40))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=9) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa) 
      tmap1
    } else if(input$num>15&input$num<21){
      address6<-filter(address1, lon<(lon1+input$num/40)&lon>(lon1-input$num/40), lat<(lat1+input$num/40)&lat>(lat1-input$num/40))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=8) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa) 
      tmap1
    } else if(input$num>19&input$num<70){
      address6<-filter(address1, lon<(lon1+input$num/40)&lon>(lon1-input$num/40), lat<(lat1+input$num/40)&lat>(lat1-input$num/40))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=8) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa) 
      tmap1
    } 
     else if (input$num>69&input$num<100){
      address6<-filter(address1, lon<(lon1+input$num/50)&lon>(lon1-input$num/50), lat<(lat1+input$num/50)&lat>(lat1-input$num/50))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=6) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa)
    } else {
      address6<-filter(address1, lon<(lon1+input$num/60)&lon>(lon1-input$num/60), lat<(lat1+input$num/60)&lat>(lat1-input$num/60))
      address6<-subset(address6,ID!=address5$ID)
      tmap1=address6 %>% leaflet() %>% addTiles() %>%
        setView(address2$lon,address2$lat, zoom=6) %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addLayersControl(baseGroups = c("OSM", "Toner"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addMarkers(label = address6$geoAddress) %>%
        addAwesomeMarkers(address2$lon, address2$lat, label = "Your Location", icon = icon.ion) %>%
        addAwesomeMarkers(lon1, lat1, label = address5$geoAddress, icon = icon.fa)
    }
    })
    
    
    
    output$COVIDMapTable=renderDataTable({
      
      return(datatable(getcoviddata(input$tabledata), rownames= FALSE))
   
    })
    
    output$COVIDTestTable=renderDataTable({
      
      return(datatable(address1, rownames= FALSE))
      
    })
    #######below needs output for testing center dataset
    
}

# Run the application 
shinyApp(ui = ui, server = server)
