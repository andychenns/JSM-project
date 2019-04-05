library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)





ui <- dashboardPage(
  
  skin = "black",
  dashboardHeader(title = "Housing Condition of Immigrants and Gentrification"),
  dashboardSidebar(
    sidebarMenu(
      
      id = "sidebarmenu",
      menuItem("JSM", tabName = "a", icon = icon("certificate",lib = "glyphicon")),
      menuItem("Housing Concition",
               tabName = "Housing Condition Analysis", icon = icon("chevron-right")
               #add tab-item here
               )
      ),
      menuItem("Gentrification",
               tabName = "Gentrificastion Analysis", icon = icon("venus")
               #add tab-item here
      )
    ),
  
  dashboardBody(
    #add body design here
   
        )#end dashboardbody
  )#end ui



#############################################

##                            Server


server <- function(input,output){
  
  #add plot and map functions here
  
  
}

# Run the application 

shinyApp(ui = ui, server = server)
