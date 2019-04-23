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


nycity <- geojsonio::geojson_read("nyc_sub_borough_area.geojson", what = "sp")
load("birthplace.Rdata")


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
        menuSubItem("Birthplace", icon = icon("flag", lib = "glyphicon"), tabName = "gbirth"),
        menuSubItem("Income-Rent Ratio", icon = icon("usd", lib = "glyphicon"), tabName = "gratio"),
        menuSubItem("Condition of Building", icon = icon("building", lib = "font-awesome"), tabName = "gbuilding")
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
        h4("A few lines and good images on project introduction. Let's keep this tab only if necessary.")
      ),
      tabItem(
        tabName = "intro",
        h3("Housing Conditions for First and Second Generation Immigrants in New York City"),
        h5("Several aspects of housing condition will be looked at for this project. Broken windows...")
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
          column(width=3,
            selectInput("statistics", "Statistics", 
                        choices = c("Electricity & Gas", "Rent price","Windows(broken, boarded)"))),
          column(width=3,
            selectInput("birthplace", "Birthplace", choices = c("USA","Immigrant","China","Mexican","Indian"))
          )
        ),
        fluidRow(
          box(title = "First generation housing condition", "my_leaf"),
          box(title = "Second generation housing condition", "Box content")
        )
      ),
      tabItem(
        tabName = "gbirth",
        h4("Brithplace of immigrants"),
        fluidPage(
          # Give it title
          titlePanel("Birthplace"),
          # Slider to slect which year
          radioButtons(
            inputId = "slider",
            label = "Years",
            c("1991", "1993", "1996", "1999", "2002", "2005", "2008", "2011", "2014", "2017"), inline = TRUE
          ),
          sidebarLayout(
            # Select the exact birthplace
            sidebarPanel(
              selectInput("birthplace", "Birthplace:",
                choices = unique(test[, "hh_birth"])
              ),
              selectInput("generation", "Generation:",
                choices = c(
                  "First generation" = "per", "Second generation dad" = "per_dad",
                  "Second generation mom" = "per_mom"
                )
              )
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## create static plot
  output$my_leaf <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lat = 40.730610, lng = -73.935242, zoom = 10)
  })

  a <- test[test$year == 1994, ]

  ## filter data
  df_filtered <- reactive({
    test <- test %>% select(year, bor_subb, hh_birth, input$generation)
    test <- test[test$year == input$slider, ]
    test <- test[test$hh_birth == input$birthplace, ]
    colnames(test)[4] <- "per"
    df <- merge(nycity, test, by = "bor_subb", duplicateGeoms = TRUE)
  })

  ## respond to the filtered data
  observe({
    pal <- colorNumeric("viridis", NULL) # Deinfe color

    leafletProxy(mapId = "my_leaf", data = df_filtered()) %>%
      addPolygons(
        fillColor = ~pal(per), smoothFactor = 0.5,
        fillOpacity = 1, weight = 1, highlightOptions =
          highlightOptions(
            color = "white", weight = 2.5,
            bringToFront = TRUE
          )
      ) %>%
      addLegend(
        pal = pal, values = ~(100 * per), opacity = 1.0,
        layerId = "foo"
      )
  })
}

shinyApp(ui, server)
