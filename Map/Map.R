


nyc_fico$borough <- gsub('Staten', 'Staten Island', nyc_fico$borough) 
map_data_fico <- geo_join(nyc_boroughs, nyc_fico, "BoroName", "borough",how="inner") 
pal0 <- colorNumeric(palette = "YlOrRd", 
                     domain = range(map_data_fico@data$avg_fico)) 
centers0 <- data.frame(gCentroid(map_data_fico,byid=TRUE)) 
centers0$region <- row.names(map_data_fico) 
centers0$region <- ifelse(centers0$region=="0","Staten Island", 
                          ifelse(centers0$region=="1","Queens", 
                                 ifelse(centers0$region=="2","Brooklyn", 
                                        ifelse(centers0$region=="3","Manhattan", 
                                               "Bronx")))) 
leaflet(map_data_fico) %>% 
  addTiles() %>%  
  addPolygons(stroke=TRUE,weight=1,fillOpacity = 0.5, smoothFactor = 0.5, 
              color = "black",fillColor = ~pal0(avg_fico), popup = ~as.character(avg_fico)) %>% 
  addLabelOnlyMarkers(data=centers0,lng = ~x,lat = ~y, label = ~region,labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>% 
  addLegend("bottomright", pal = pal0, values = ~avg_fico, 
            title = "Average Fico Score", 
            opacity = 1 
  ) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  setView(-73.91, 40.70, zoom = 10)