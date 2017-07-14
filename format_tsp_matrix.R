format_tsp_matrix<-function(distance,subtour){
        source('BNB.R', local = T)
        subroute_matrix<-distance[c(0,as.character(unlist(merge(colnames(distance),unlist(strsplit(subtour,"-")),all(TRUE))))),c(0,as.character(unlist(merge(colnames(distance),unlist(strsplit(subtour,"-")),all(TRUE)))))]
        diag(subroute_matrix)<-Inf
        nodes<-rownames(subroute_matrix)
        results<-BNB(subroute_matrix)
        subroute<-results$tour
        new_nodes<-unlist(strsplit(subroute,"-"))
        subroute<-paste(nodes[as.numeric(new_nodes)],collapse="-")
        results[[1]]<-subroute
        results
}