get_matrix_alt<-function(datos){
        library(osrm)
        library(sp)
        library(spatstat)
        result<-list()
        distance<-matrix(rep(0,nrow(datos)*nrow(datos)),nrow = nrow(datos),ncol = nrow(datos))
        time<-matrix(rep(0,nrow(datos)*nrow(datos)),nrow = nrow(datos),ncol = nrow(datos))
        datos$ID <- 0:(nrow(datos)-1)
        nodes <- datos[,c(5, 3, 2)]
        allarcs<-data.frame()
        suppressMessages(for(i in 1:nrow(datos)){
                for(j in 1:nrow(datos)){
                        if(i==j){
                                distance[i,j]<-0
                                time[i,j]<-0
                        }
                        else { 
                                route_dist<-data.frame()
                                route_dist<-osrmRoute(src = nodes[i, 1:3], 
                                                      dst = nodes[j, 1:3], 
                                                      overview = "full",
                                                      sp = TRUE)
                                distance[i,j]<-route_dist$distance
                                time[i,j]<-route_dist$duration
                                if (is.empty(allarcs)){
                                  allarcs <- route_dist
                                }else{
                                  allarcs <- rbind(allarcs, route_dist) 
                                }
                        }
                }
        })
        result[[1]]<-distance
        result[[2]]<-time
        result[[3]]<-c(datos[2:nrow(datos),4])
        result[[4]]<-allarcs
        names(result)<-c("Distance","Time","Demand", "Arcs")
        result
}