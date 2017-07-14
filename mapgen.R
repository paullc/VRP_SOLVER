map<-function(dest, arcs){
  library(leaflet)
  
  #extract the terminal
  origin <- dest[1,]
  
  destin <- dest[2:nrow(dest),]
  
  # design icons
  
  icoorig = makeIcon(iconUrl = 'depot.png', 30, 30)
  
  icoclient = makeIcon(iconUrl = 'client.png', 30, 30)
  
  html_legend <- "<img src='depot.png' style='width:20px;height:20px;'> Depot<br>
                  <img src='client.png' style='width:20px;height:20px;'> Client" 
  
  #Design the map
  
  #Create the palette for the arcs
  factpal <- colorFactor(rainbow(max(arcs$route)), arcs$route)
  
  #get the basemap
  m <- leaflet() %>% addTiles(attribution = "Data: OpenStreetMap & contributors, ODBl; routes: OSRM") 
  
  #add the terminal
  m <- m %>% addMarkers(data = origin, ~Longitude, ~Latitude, popup = ~as.character(Name), 
                        icon = icoorig, group = "Nodos") 
  
  #add the gas station
  m <- m %>% addMarkers(data = destin, ~Longitude, ~Latitude, popup = ~as.character(Name), 
                        icon = icoclient, group = "Nodos") 
  
  #add the routes by arc
  routeslist <- vector()
  for (i in 1:nrow(arcs)) {
    arcaux= arcs[i,]
    routename <-  paste("Route", as.character(arcaux$route), sep = " ")
    m <- m %>% addPolylines(data=arcaux, layerId = NULL, group = routename, stroke = TRUE,
                            color = ~factpal(route), weight = 4, opacity = 0.5, popup = ~as.character(routename))
    if (any(routeslist==routename))
      routeslist <- routeslist
    else
      routeslist <- c(routeslist, routename)
  }
  
  # add a legend
  m <- m %>% addLegend("bottomright", factpal, arcs$route, labels = routeslist,
                       labFormat = labelFormat(prefix = 'Route ')) %>%   
             addControl(html = html_legend, position = "bottomright") %>%
    
    # add layers control
    addLayersControl(
      overlayGroups = routeslist,
      position = "bottomleft",
      options = layersControlOptions(collapsed = TRUE)
    )
  m
}  