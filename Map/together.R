library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(sp)
library(readr)
library(tidyverse)
library(mapview)

nycity <- geojsonio::geojson_read("nyc_sub_borough_area.geojson", what = "sp")
load("gentrification_Longhao.Rda")

nyc$Gentrified <- nyc$Gentrified+1
nyc$Year<-as.numeric(nyc$Year)


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(
    title = "ASA-JSM\nData Challenge\n2019 - NYCHVS",
    titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("The Project", tabName = "project", icon = icon(name = "book", lib = "glyphicon")),
      menuItem("Housing Conditions",
        tabName = "housing", icon = icon("home", lib = "glyphicon"),
        menuSubItem("Introduction", tabName = "intro", icon = icon("comments", lib = "font-awesome")),
        menuSubItem("Housing condition", tabName = "house", icon = icon("th"))
      ),
      menuItem("Gentrification",
        tabName = "gentrify", icon = icon("cog", lib = "glyphicon"),
        menuSubItem("Introduction", tabName = "gintro", icon = icon("comments", lib = "font-awesome")),
        menuSubItem("Gentrification Map", icon = icon("flag", lib = "glyphicon"), tabName = "gentrification")
        )
    )
  )
  ,
  dashboardBody(
    tabItems(
      # First tab
      tabItem(
        tabName = "project",
        h2("ASA-JSM Data Challenge 2019 - New York City Housing and Vacancy Survey"),
        h4("A few lines and good images on project introduction. ")
      ),
      tabItem(
        tabName = "intro",
        h3("Housing Conditions for First and Second Generation Immigrants in New York City"),
        h5("Several aspects of housing condition will be looked at for this project. ")
      ),
      tabItem(
        tabName = "house",
        h4("Housing condition"),
        fluidRow(
          column(width = 6, radioButtons(
            inputId = "year", label = "Select Year",
            choices = c(
              "1991", "1993", "1996", "1999",
              "2002", "2005", "2008", "2011",
              "2014", "2017"
            ), inline = TRUE, selected = "1991"
          )),
          column(
            width = 3,
            selectInput("statistics_condition", "Statistics",
              choices = c("Electricity & Gas", "Rent price", "Windows(broken, boarded)")
            )
          ),
          column(
            width = 3,
            selectInput("birthplace_condition", "Birthplace",
              choices = c("USA", "Immigrant", "China", "Mexican", "Indian")
            )
          )
        ),
        fluidRow(
          box(title = "First generation housing condition", "my_leaf"),
          box(title = "Second generation housing condition", "Box content")
        )
      ),
      tabItem(
        tabName = "gentrification",
        h4("Gentrification"),
        fluidPage(
          # Slider to slect which year
          radioButtons(
            inputId = "slider",
            label = "Years",
            c("1993", "1996", "1999", "2002", "2005", "2008", "2011", "2014", "2017", "26"),
            inline = TRUE
          ),
          sidebarLayout(
            # Select the exact birthplace
            sidebarPanel(
              selectInput("statistics_gentrification", "Statistics:",
                choices = c(
                  "Average age" = "Avg_Age","Percentage change of income"= "Pct_Chg_Income", 
                  "Percentage change of rent" = "Pct_Chg_Rent", "Rent income ratio" = "Rent_Inc",
                  "Percentage of white people" = "Pct_White", "Gentrification"="Gentrified"),
                selected = "Pct_Chg_Income"
              ),
              selectInput("sub_borough", "Sub borough", choices= unique(nyc$subb_name), multiple = TRUE)
              ),
              mainPanel(
                leafletOutput("my_leaf")
              )
            )
          )
        )
      )
    )
  )
#Picture
Lopez = "https://andrealizamablog.files.wordpress.com/2017/10/nyc.jpg"

csscode = HTML("
               .leaflet-popup-content {
               width: 150px !important;
               }")


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## create static plot
  output$my_leaf <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lat = 40.730610, lng = -73.935242, zoom = 10)%>%
      addMarkers( -73.923912, 40.809551,popup=c(popupImage(Lopez,embed=TRUE,width=250)),"Chinatown")
  })

  ## filter data
  df_filtered <- reactive({
    nyc <- nyc %>% select(
      Year, bor_subb,subb_name,
      input$statistics_gentrification
    )
    nyc <- nyc[nyc$Year == input$slider, ]
    colnames(nyc)[4] <- "per"
    df <- merge(nycity, nyc, by = "bor_subb", duplicateGeoms = TRUE)
  })

  ## respond to the filtered data
  observe({
    pal <- colorNumeric("viridis", NULL) # Deinfe color
    #Label of polygon
    
    leafletProxy(mapId = "my_leaf", data = df_filtered()) %>%
      addPolygons(
        fillColor = ~pal(per), smoothFactor = 0.5,
        fillOpacity = 1, weight = 2.5, highlightOptions =
          highlightOptions(
            color = "white", weight = 4,
            bringToFront = TRUE
          ),
        label = ~paste0(subb_name)
      ) %>%
      addLegend(
        pal = pal, values = ~(per), opacity = 1.0, title=NULL,
        layerId = "foo"
      )
  })
}

shinyApp(ui, server)

