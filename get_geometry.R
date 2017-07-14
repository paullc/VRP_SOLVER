#Get the necessary arcs
get_geometry<-function(arcsgeom, nvord){
  routedf <- arcsgeom[[4]]
  rutas <- nvord[[1]]
  i=1
  for (a in 1:length(rutas)) {
    opt = unlist(strsplit(rutas[a],"-"))
    
    #Get the elements for each route
    for (e in 1:(length(opt)-1)){
      #routedf is the name of the data frame obtained with osrm
      index <- which(routedf$ID==as.numeric(opt[e]) & routedf$ID.1==as.numeric(opt[e+1]))
      routedata <- routedf[index,]
      routedata <- spChFIDs(routedata, as.character(i))
      routedata$route <- a
      routedata$order <- e
      if (exists("trip")){
        trip <- rbind(trip, routedata)
      }else{
        trip <- routedata
      }
      i=i+1
    }
  }
 trip  
}
