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


nycity <- geojsonio::geojson_read("C:/Users/GP/Desktop/MEGHA/SemII/JSM-ASA Challenge/JSM-project/Map/nyc_sub_borough_area.geojson", what = "sp")
load("C:/Users/GP/Desktop/MEGHA/SemII/JSM-ASA Challenge/JSM-project/Data/NY.Rda")


# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "ASA-JSM\nData Challenge\n2019 - NYCHVS",
                  titleWidth = 450),
  dashboardSidebar(
    sidebarMenu(
      menuItem("The Project", tabName = "project", icon = icon(name = "book", lib = "glyphicon")),
      menuItem("Housing Conditions", tabName = "housing", icon = icon("home", lib = "glyphicon"),
               menuSubItem("Introduction", tabName = "intro", icon = icon("comments", lib = "font-awesome")),
               menuSubItem("Broken Windows", tabName = "window", icon = icon("th")),
               menuSubItem("Condition of Building", tabName = "building", icon = icon("building", lib = "font-awesome")),
               menuSubItem("Number of Persons per Room", tabName = "crowded", icon = icon("users",
                                                                                          lib = "font-awesome"))
      ),
      menuItem("Gentrification", tabName = "gentrify", icon = icon("cog", lib = "glyphicon"),
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
      #First tab
      tabItem(tabName = "project",
              h2("ASA-JSM Data Challenge 2019 - New York City Housing and Vacancy Survey"),
              h4("A few lines and good images on project introduction. Let's keep this tab only if necessary.")
      ),
      tabItem(tabName = "intro",
              h3("Housing Conditions for First and Second Generation Immigrants in New York City"),
              h5("This section explores blah blah blah... Keep this tab only if needed.")
      ),
      tabItem(tabName = "window",
              h4("Percentage of Households with Broken Windows by Sub-Borough and Year"),
            fluidRow(column(12, radioButtons(inputId = "year", label = "Select Year",
                                             choices = c("1991", "1993", "1996", "1999",
                                                              "2002", "2005", "2008", "2011",
                                                              "2014", "2017"), inline = TRUE, selected = "1991")  ))
    )
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

shinyApp(ui, server)
