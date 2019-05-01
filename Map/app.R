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
library(sf)
library(raster)
library(spData)
library(tmap)
library(mapview)
library(magick)
library(htmlwidgets)
library(htmltools)
library(plotly)

#Loading the main data sets 
ny_city <- geojsonio::geojson_read("nyc_sub_borough_area.geojson", what = "sp")
nycity <- ny_city
load("hcdata.Rda")
load("NYC_avg.Rda")
load("ggdata.Rda")

load("Longhao.Rdata")
load("NY.Rda")
nyc$subb_name <- as.character(nyc$subb_name)
a<-unique(nyc$subb_name)
nyc$Gentrified <- nyc$Gentrified + 1
nyc[496:550,1]<-"1991-2017"

# Define UI for application
ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "ASA-JSM\nData Challenge\n2019 - NYCHVS",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("The Project", tabName = "project", icon = icon(name = "book", lib = "glyphicon")),
      menuItem("Housing Conditions", tabName = "housing", icon = icon("home", lib = "glyphicon"),
               menuSubItem("Introduction", tabName = "intro", icon = icon("comments", lib = "font-awesome")),
               menuSubItem("Statistics", tabName = "house", icon = icon("stats", lib = "glyphicon"))
            
      ),
      menuItem("Gentrification", tabName = "gentrify", icon = icon("cog", lib = "glyphicon"),
               menuSubItem("Introduction", tabName = "gintro", icon = icon("comments", lib = "font-awesome")),
               menuSubItem("Statistics", icon = icon("stats", lib = "glyphicon"), tabName = "gentrification")
      )
    )
    
  )
  ,
  dashboardBody(
    tabItems(
      #First tab
      tabItem(tabName = "project",
              tags$h2(style = "text-align:center;color:#000080", "ASA-JSM Data Challenge 2019",br(),"New York City Housing and Vacancy Survey"),
              HTML('<img src="nyc.png", height="400px", width="543px"    
          style="float:left;padding-bottom:50px"/>','<p style="color:black"></p>'),
              HTML('<img src="immi.jpg", height="400px", width="543px"    
          style="float:right;padding-bottom:50px"/>','<p style="color:black"></p>'),
              
              tags$h5(style = "text-align:center","The American Statistical Association (ASA) sponsors an annual Data Challenge as part of its Joint Statistical Meeting (JSM).",
                      "The data challenge for 2019 is on the New York City Housing and Vacancy Survey (NYCHVS).",
                      "The NYCHVS is a triennial survey that is conducted by the Department of Housing Preservation and Development (HPD) in partnership with the U.S. Census Bureau.",
                      "Through this project, we explore changes in the housing conditions of first and second generation immigrants in New York City in the time period of 1991-2017.",
                      "We also make an attempt to model the data to see gentrification trends in sub boroughs of NYC.")
      ),
      tabItem(tabName = "intro",
              tags$h3(style = "text-align:center;color:#000080","Housing Conditions for First and Second Generation Immigrants in New York City"),
              HTML('<img src="nycgif.gif", height="450px", width="800px"    
          style="padding-left:250px;padding-bottom:30px;padding-top:30px"/>','<p style="color:black"></p>'),
              tags$h5(style = "text-align:center","This section explores the changes in first and second generation immigrant housing in New York City.",
                      "Refer to Housing Condition Statistics to visualize these changes through leaflet maps.")
      ),
      tabItem(tabName = "house",
            fluidRow(column(6, radioButtons(inputId = "year", label = "Select Year",
                                             choices = c("1991", "1993", "1996", "1999",
                                                              "2002", "2005", "2008", "2011",
                                                              "2014", "2017"), inline = TRUE, selected = "1991")),
            column(width=3,
                   selectInput("variables", "Choose Variable", 
                               choices = c("Building Condition", "Windows(Broken/Boarded)", "Rats and Mice",
                                           "Kitchen Facilities", "Average Rent", "Immigrants"))), column(3,uiOutput("immigrants"))),
            fluidRow(
              box(title = "First Generation Immigrant Housing Conditions", leafletOutput("mymap"),
                  selectInput("subborough", "Choose Sub Borough", 
                              choices = c("Select",101:110, 201:218, 301:310, 401:414, 501:503)),
                  textOutput("Borough"), textOutput("SubBorough"),plotlyOutput("eda1")),
              box(title = "Second Generation Immigrant Housing Conditions", leafletOutput("mymap2"),
                  selectInput("subborough2", "Choose Sub Borough", 
                              choices = c("Select",101:110, 201:218, 301:310, 401:414, 501:503)),
                  textOutput("Borough2"), textOutput("SubBorough2"),plotlyOutput("eda2"))
            )
    ),
    tabItem(tabName = "gintro",
            tags$h3(style = "text-align:center;color:#000080","Gentrification in New York City"),
            HTML('<img src="gentr1.jpg", height="400px", width="500px"    
                 style="float:left;padding-left:180px;padding-bottom:30px;padding-top:30px"/>','<p style="color:black"></p>'),
            HTML('<img src="gentr2.jpg", height="400px", width="600px"    
                 style="float:right;padding-right:180px;padding-bottom:30px;padding-top:30px"/>','<p style="color:black"></p>'),
            tags$h5(style = "text-align:center;padding-bottom:50px","Gentrification is a general term for the arrival of wealthier people in an existing urban district, a related increase in rents and property values, and changes in the district's character and culture.",
                    "The term is often used negatively, suggesting the displacement of poor communities by rich outsiders. But the effects of gentrification are complex and contradictory, and its real impact varies.",
                    "Through this project, we make an attempt at identifying gentrified sub boroughs in NYC through a logistic regression model. The results of the model are visualized through maps in the Gentrification Statistics section."),
            tags$footer("http://archive.pov.org/flagwars/what-is-gentrification/")),
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
  Immigrants <- reactive({
    if (input$variables == "Immigrants"){
      return(selectInput("group", "Select Immigrant Group", choices = c("China", "India", "Mexico")))
    }
  })
  output$immigrants <- renderUI({
    Immigrants()
    })
  
  
  
  badwin_map <- merge(ny_city, hcdata2, by = "bor_subb", duplicateGeoms = TRUE)
  avg_map <- merge(ny_city, NYC, by = "bor_subb", duplicateGeoms = TRUE)
  
  
  col <- colorRamps::blue2red(12)
  pal_win <- colorBin(col, bins = c(0,5,10,15,20,25,30,35,40,45,50), na.color = "grey")
  pal_imm <- colorNumeric("viridis", NULL)
  pal_avg_rent <- colorBin("PuRd", bins = c(4,4.5,5,5.5,6,6.5,7,7.5,8))
  pal_rats <- colorBin(rev(grDevices::heat.colors(12)), bins = c(0,10,20,30,40,50,60,70,80,90,100))
  pal_bld <- colorBin("YlGn", bins = c(0,10,20,30,40,50))
  pal_kit <- colorBin("Reds", bins = c(0,5,10,15,20,25))
max(hcdata2$perc_rats)
  output$mymap <- renderLeaflet({
    if (input$variables == "Windows(Broken/Boarded)"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_win(perc_poorwin),smoothFactor = 0.5, color = "black", fillOpacity = 0.9,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                                        bringToFront = TRUE), label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_poorwin, "%"))%>%
        addLegend("topleft", pal=pal_win, values = ~perc_poorwin, opacity = 1.0, title = paste0(input$year,"<br>1st Gen<br>% of Houses with <br> Broken/Boarded Up<br> Windows"))%>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Building Condition"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_bld(perc_bld),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                              bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_bld, "%"))%>%
        addLegend("topleft", pal=pal_bld, values = ~perc_bld, opacity = 1.0, title = paste0(input$year,"<br>1st Gen", "<br>% of Buildings<br>in Dilapidated/<br>Deteriorating Condition")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Rats and Mice"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_rats(perc_rats),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                              bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_rats, "%"))%>%
        addLegend("topleft", pal=pal_rats, values = ~perc_rats, opacity = 1.0, title = paste0(input$year,"<br>1st Gen", "<br>% of Houses<br> with Rats & Mice")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Kitchen Facilities"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_kit(perc_kitchen),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                               bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_kitchen, "%"))%>%
        addLegend("topleft", pal=pal_kit, values = ~perc_kitchen, opacity = 1.0, title = paste0(input$year,"<br>1st Gen", "<br>% of Houses<br> with No/Non-functioning <br> Kitchen")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Average Rent"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_avg_rent(log_rent),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                                       bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": $",avg_rent))%>%
        addLegend("topleft", pal=pal_avg_rent, values = ~log_rent, opacity = 1.0, title = paste0(input$year,"<br>1st Gen", "<br>Log Average Rent")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Immigrants"){
    if (input$group == "China"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_imm(China),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                               bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",China, "%"))%>%
        addLegend("topleft", pal=pal_imm, values = ~China, opacity = 1.0, title = paste0(input$year,"<br>1st Gen", "<br>% of<br>Chinese Immigrants")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$group == "India"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_imm(India),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                               bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",India, "%"))%>%
        addLegend("topleft", pal=pal_imm, values = ~India, opacity = 1.0, title = paste0(input$year,"<br>1st Gen", "<br>% of<br>Indian Immigrants")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if(input$group == "Mexico"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 1,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_imm(Mexico),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                                bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",Mexico, "%"))%>%
        addLegend("topleft", pal=pal_imm, values = ~Mexico, opacity = 1.0, title = paste0(input$year,"<br>1st Gen", "<br>% of<br>Mexican Immigrants")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    }
    
  })
  
  output$Borough <- renderText({
    paste0("Borough: ", badwin_map$bor_name[badwin_map$bor_subb == input$subborough & badwin_map$year == input$year & badwin_map$gen == 1])
           
  })
  output$SubBorough <- renderText({
    paste0("Sub Borough: ", badwin_map$subb_name[badwin_map$bor_subb == input$subborough & badwin_map$year == input$year & badwin_map$gen == 1])
    
  })

  output$eda1 <- renderPlotly({
    plot_ly(data = ggdata[ggdata$year == input$year & ggdata$gen == 1 & ggdata$bor_subb == input$subborough,],
            x = ~perc, y = ~condition, type = "bar", color = ~condition)%>%
      layout(yaxis = list(title = "", tickfont = list(size = 9)),
             xaxis = list(title = 'Percentage of Households', range = c(0,100), tickangle = 0,
                          tickfont = list(size = 9), ticklength = 10), showlegend = FALSE,
             autosize = F, width = 500, height = 300)
  })
  
  
  output$mymap2 <- renderLeaflet({
    
    if (input$variables == "Windows(Broken/Boarded)"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_win(perc_poorwin),smoothFactor = 0.5, color = "black", fillOpacity = 0.9,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                                        bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_poorwin, "%"))%>%
        addLegend("topleft", pal=pal_win, values = ~perc_poorwin, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen<br>% of Houses with <br> Broken/Boarded Up<br> Windows"))%>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Building Condition"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_bld(perc_bld),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                              bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_bld, "%"))%>%
        addLegend("topleft", pal=pal_bld, values = ~perc_bld, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen", "<br>% of Buildings<br>in Dilapidated/<br>Deteriorating Condition")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Rats and Mice"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_rats(perc_rats),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                               bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_rats, "%"))%>%
        addLegend("topleft", pal=pal_rats, values = ~perc_rats, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen", "<br>% of Houses<br> with Rats & Mice")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Kitchen Facilities"){
      leaflet(badwin_map[badwin_map$year == input$year & badwin_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_kit(perc_kitchen),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                                  bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",perc_kitchen, "%"))%>%
        addLegend("topleft", pal=pal_kit, values = ~perc_kitchen, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen", "<br>% of Houses<br> with No/Non-functioning <br> Kitchen")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Average Rent"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_avg_rent(log_rent),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                                       bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": $",avg_rent))%>%
        addLegend("topleft", pal=pal_avg_rent, values = ~log_rent, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen", "<br>Log Average Rent")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$variables == "Immigrants"){
    if (input$group == "China"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_imm(China),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                               bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",China, "%"))%>%
        addLegend("topleft", pal=pal_imm, values = ~China, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen", "<br>% of<br>Chinese Immigrants")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if (input$group == "India"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_imm(India),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                               bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",India, "%"))%>%
        addLegend("topleft", pal=pal_imm, values = ~India, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen", "<br>% of<br>Indian Immigrants")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    else if(input$group == "Mexico"){
      leaflet(avg_map[avg_map$year == input$year & avg_map$gen == 2,]) %>%
        addTiles() %>% addPolygons(fillColor=~pal_imm(Mexico),smoothFactor = 0.5, color = "black", fillOpacity = 1,weight=1,highlightOptions = highlightOptions(color = "white", weight = 2.5,
                                                                                                                                                                bringToFront = TRUE),label = ~paste0(bor_name, ": ", bor_subb, ": ", subb_name, ": ",Mexico, "%"))%>%
        addLegend("topleft", pal=pal_imm, values = ~Mexico, opacity = 1.0, title = paste0(input$year,"<br>2nd Gen", "<br>% of<br>Mexican Immigrants")) %>%
        setView(lat =  40.730610, lng = -73.935242, zoom = 10)
    }
    }
  })
  
  output$Borough2 <- renderText({
    paste0("Borough: ", badwin_map$bor_name[badwin_map$bor_subb == input$subborough2 & badwin_map$year == input$year & badwin_map$gen == 2])
    
  })
  output$SubBorough2 <- renderText({
    paste0("Sub Borough: ", badwin_map$subb_name[badwin_map$bor_subb == input$subborough2 & badwin_map$year == input$year & badwin_map$gen == 2])
    
  })
  
  output$eda2 <- renderPlotly({
    plot_ly(data = ggdata[ggdata$year == input$year & ggdata$gen == 2 & ggdata$bor_subb == input$subborough2,],
            x = ~perc, y = ~condition, type = "bar", color = ~condition)%>%
      layout(yaxis = list(title = "", tickfont = list(size = 9)),
             xaxis = list(title = 'Percentage of Households', range = c(0,100), tickangle = 0,
                          tickfont = list(size = 9), ticklength = 10), showlegend = FALSE,
             autosize = F, width = 500, height = 300)
  })
  #Gentrification
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

shinyApp(ui, server)

