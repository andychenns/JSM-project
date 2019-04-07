library(shiny)
library(leaflet)
library(geojsonio)
library(sp)
library(readr)
library(tidyverse)
#Read data

nycity <- geojsonio::geojson_read("~/Desktop/JSM project/JSM/Map/nyc_sub_borough_area.geojson", what = "sp")
load("~/Desktop/JSM project/JSM/Code/NY.Rda")

#Rest data format
NY$mgrent <- as.numeric(NY$mgrent)
NY$year <- as.numeric(NY$year)

#Construct dataframes
test <- NY %>% select("year","sba","mgrent","hhbirth","hhinc") %>% 
  rename(bor_subb=sba)%>% filter(mgrent<9999, hhinc <9000000) %>% 
  group_by(year,bor_subb) %>% summarise(averagerent = mean(mgrent),
                                 medianrent= median(mgrent),
                                 aveincome= mean(hhinc),
                                 medincome= median(hhinc))

#User interface                             

ui <- fluidPage(
  sliderInput(inputId = "slider",
              label = "Years",
              min = 1991,
              max = 2017 ,
              value = 1991),
  leafletOutput("my_leaf")
)


server <- function(input, output, session){
  
  ## create static plot
  output$my_leaf <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    
  })
  
  a<-test[test$year == 1994,]

  ## filter data
  df_filtered <- reactive({
    test<-test[test$year == input$slider, ]
    df <- merge(nycity,test, by = "bor_subb",duplicateGeoms = TRUE)    
  })

  ## respond to the filtered data
  observe({
    
    pal <- colorNumeric("viridis", NULL) #Deinfe color
    
    leafletProxy(mapId = "my_leaf", data = df_filtered()) %>%
      
      addPolygons( fillColor=~pal(log10(averagerent)),smoothFactor = 0.5, 
                   fillOpacity = 1,weight=1,highlightOptions = 
                     highlightOptions(color = "white", weight = 2.5,  
                bringToFront = TRUE),label = ~paste0(bor_subb, ": ", "Average rent  ",
                                                     formatC(averagerent, big.mark = ","),"  Median rent  ", 
                                                     formatC(medianrent,big.mark = ","))) %>%
      addLegend(pal=pal, values = ~log10(averagerent), opacity = 1.0,
                labFormat = labelFormat(transform = function(x) round(10^x)),layerId = "foo")
  })
  
  
  
}

shinyApp(ui, server)