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


# nyc$bor_subb
# nyc$subb_name
#
# nyc$subb_name <- as.character(nyc$subb_name)
# sub_name <- as.data.frame(cbind(unique(nyc$bor_subb),unique(nyc$subb_name)))
# sub_name<-sub_name %>% rename(sba=V1, subb_name =V2)

nycity <- geojsonio::geojson_read("nyc_sub_borough_area.geojson", what = "sp")
load("Longhao.Rdata")
load("NY.Rda")
nyc$subb_name <- as.character(nyc$subb_name)
a<-unique(nyc$subb_name)
nyc$Gentrified <- nyc$Gentrified + 1
nyc[496:550,1]<-"1991-2017"
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
        menuSubItem("Statistics", icon = icon("flag", lib = "glyphicon"), tabName = "gentrification")
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
        fluidRow(
          column(width = 6, radioButtons(
            inputId = "slider", label = "Select Year",
            choices = c(
              "1993", "1996", "1999",
              "2002", "2005", "2008", "2011",
              "2014", "2017", "1991-2017"
            ), inline = TRUE, selected = "1993"
          )),
          column(
            width = 3,
            selectInput("statistics_gentrification", "Statistics",
              choices = c(
                "Average age" = "Avg_Age", "Percentage change of income"
                = "Pct_Chg_Income",
                "Percentage change of rent" = "Pct_Chg_Rent",
                "Rent income ratio" = "Rent_Inc",
                "Percentage change of white people" = "Pct_Chg_White",
                "Gentrification" = "Gentrified", "Percent White"="Pct_White"
              ), selected = "Pct_Chg_Income"
            )
          ),
          column(
            width = 3,
            selectInput("sub_borough", "Sub borough:",
              choices = a, selected = a[30]
            )
          )
        ),
        fluidRow(
          box(title = "Map", leafletOutput("my_leaf")),
          box(title = "Statistics", plotOutput("plot2"))
        )
      )
    )
  )
)
# Picture
Lopez <- "https://andrealizamablog.files.wordpress.com/2017/10/nyc.jpg"
china <- "https://imgs.6sqft.com/wp-content/uploads/2015/02/21005101/On-Leong-Tong-Building.jpg"


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## create static plot
  output$my_leaf <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lat = 40.730610, lng = -73.935242, zoom = 10) %>%
      addMarkers(-73.923912, 40.809551, popup = c(popupImage(Lopez, embed = TRUE, width = 250))) %>%
      addMarkers(-73.997961, 40.716688, popup = c(popupImage(china, embed = TRUE, width = 250)))
  })

  ## filter data
  df_filtered <- reactive({
    nyc <- nyc %>% dplyr::select(
      Year, bor_subb, subb_name,
      input$statistics_gentrification
    )
    nyc <- nyc[nyc$Year == input$slider, ]
    colnames(nyc)[4] <- "per"
    df <- merge(nycity, nyc, by = "bor_subb", duplicateGeoms = TRUE)
  })

  ## respond to the filtered data
  observe({
    pal <- colorNumeric("viridis", NULL) # Define color
    # Label of polygon
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
      leaflet::addLegend(
        pal = pal, values = ~(per), opacity = 1.0, title = NULL, layerId = "foo"
      )
  })

  dat <- reactive({
    nyc$Year<-as.numeric(nyc$Year)
    nyc <- nyc %>% filter(Year >= 1000) 
    nyc <- nyc[nyc$subb_name == input$sub_borough, ]
  })


  output$plot2 <- renderPlot({
    ggplot(dat())+geom_col(aes(Year, 100*Pct_Chg_Income, fill = "mediumpurple"), size = 1.5) +
      scale_fill_manual(labels = c("mediumpurple" = "Pct Chg Income"), values = c("mediumpurple"))+
      geom_line(aes(Year, Avg_Age, color = "orange2"), size = 1.5) + 
      geom_line(aes(Year, 100*Pct_White, color = "firebrick2"), size = 1.5) +
      scale_color_manual(labels = c("orange2"="Average Age", 
                                    "firebrick2"="Pct of white"), 
                         values = c("orange2", "firebrick2"))+
      theme(legend.position="bottom") + ylab("")
  })
}

# nyc$Year<-as.numeric(nyc$Year)
# ggplot(nyc[nyc$subb_name=="Mott Haven/Hunts Point",]) + 
#   geom_point(aes(x=Year,y=Avg_Age)) + 
#   geom_line(aes(x=Year,y=Avg_Age)) +

shinyApp(ui, server)

