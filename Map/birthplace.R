library(shiny)
library(leaflet)
library(geojsonio)
library(sp)
library(readr)
library(tidyverse)
#Read data

nycity <- geojsonio::geojson_read("~/Desktop/JSM project/JSM/Map/nyc_sub_borough_area.geojson", what = "sp")
load("~/Desktop/JSM project/JSM/Data/NY.Rda")

#Rest data format
NY$mgrent <- as.numeric(NY$mgrent)
NY$year <- as.numeric(NY$year)


#Construct dataframes
NY$count <- rep(1, nrow(NY))
NY$hh_birth <- as.factor(NY$hh_birth)

test <- NY %>% select("year","sba","hh_birth","dad_birth","mom_birth","count") %>% 
  rename(bor_subb=sba) %>%  
  group_by(year,bor_subb) %>% mutate(countT=sum(count)) %>%
  group_by(year,bor_subb,hh_birth) %>%
  mutate(countS= sum(count))%>%
  mutate(per=countS/countT) %>% distinct(year, bor_subb, hh_birth,per)

            

ui <- fluidPage(
  #Give it title
  titlePanel("Birthplace"),
  #Slider to slect which year
  sliderInput(inputId = "slider",
              label = "Years",
              min = 1991,
              max = 2017 ,
              value = 1991),
  sidebarLayout(
   #Select the exact birthplace
     sidebarPanel(
      selectInput("birthplace", "Birthplace:",
                  choices= unique(NY[,66])),
      selectInput("generation", "Generation:",
                  choices = c("First generation","Second generation"))
    
  ),
  mainPanel(
    leafletOutput("my_leaf")
    )
)
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
    test<-test[test$hh_birth == input$birthplace,]
    df <- merge(nycity,test, by = "bor_subb",duplicateGeoms = TRUE)    
  })
  
  ## respond to the filtered data
  observe({
    
    pal <- colorNumeric("viridis", NULL) #Deinfe color
    
    leafletProxy(mapId = "my_leaf", data = df_filtered()) %>%
      
      addPolygons( fillColor=~pal((per)),smoothFactor = 0.5, 
                   fillOpacity = 1,weight=1,highlightOptions = 
                     highlightOptions(color = "white", weight = 2.5,  
                                      bringToFront = TRUE)) %>%
      addLegend(pal=pal, values = ~(per), opacity = 1.0,
               layerId = "foo")
  })
  
  
  
}

shinyApp(ui, server)