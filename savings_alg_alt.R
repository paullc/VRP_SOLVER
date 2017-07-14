## The function uses a list as argument, which includes the distance and time matrices with 
## a demand vector. The capacity should also be given and is set as 10000 as default.
## The cost variable given tells the function to optimize in terms of distance (D) or time (T).
savings_alg_alt<-function(datos,capacity=10000,cost="D"){
## Calculate savings matrix
        k<-capacity
        track<-vector()
        ref<-vector()
        new_ref<-vector()
        demand <- datos$Demand
        orig_demand<-demand
        mat_time<-datos$Time
        mat_distance<-datos$Distance
## This condition tells the program to optimize with respect to time or distance
        if(cost=="D")
                cost_mat<-mat_distance
        else if(cost=="T")
                cost_mat<-mat_time
        else {
                stop("Error: Argument for cost is not valid")
        }
                
## This condition checks if the any demand is higher than the imposed maximum vehicle capacity. If so, then new nodes
## are included in the distance matrix, so that the demand is divided amongst two nodes with the exact geographical
## location. 
        if(any(demand>k)){
                ref<-which(demand>k)+1
                init_length<-length(demand)
                new_ref<-seq(length(demand)+1,length(demand)+length(ref),1)
                cost_mat<-rbind(cost_mat,cost_mat[c(ref),])
                cost_mat<-cbind(cost_mat,cost_mat[,c(ref)])
                demand[(init_length+1):(init_length+length(ref))]<-c(demand[which(demand>k)]-k)
                demand[which(demand>k)]<-k
        }
        rownames(cost_mat)<-seq(0,nrow(cost_mat)-1,1)
        colnames(cost_mat)<-seq(0,ncol(cost_mat)-1,1)
        savings<-matrix(rep(0,(nrow(cost_mat)-1)*(ncol(cost_mat)-1)),nrow=nrow(cost_mat)-1,ncol=ncol(cost_mat)-1)
        rownames(savings)<-seq(1,nrow(savings),1)
        colnames(savings)<-seq(1,ncol(savings),1)
## Savings corresponding to "diagonal" elements should have a large negative value(-5000) so that they are not included in the solution        
        for(i in 1:nrow(savings)){
                for(j in 1:ncol(savings)){
                        if(cost_mat[i+1,j+1]==0)
                                savings[i,j]<-c(-5000)
                        else
                                savings[i,j]<-cost_mat[i+1,1]+cost_mat[1,j+1]-cost_mat[i+1,j+1]
                }
        }
## The elements from the savings matrix are ordered in decreasing order and then the address of each sorted element is identified with respect
## to the original distance matrix
        ord_savings<-order(savings,decreasing = TRUE)
        add_savings<-vector()
        for(i in 1:length(ord_savings)){
                if(ord_savings[i]%%nrow(savings)==0)
                        add_savings[i]<-paste(nrow(savings),ceiling(ord_savings[i]/ncol(savings)),sep="-")
                else
                        add_savings[i]<-paste(ord_savings[i]-(floor(ord_savings[i]/nrow(savings))*nrow(savings)),ceiling(ord_savings[i]/ncol(savings)),sep="-")
        }
## The routes will be stored in the solution vector. The extreme nodes from every subroute are stored in the trial_ext list. The demand from each subroute
## will be stored in the s_demand vector. 
## The solution vector is initialized to the arc with the highest savings. The extremes are also initialized. The demand for the current solution is also 
## calculated.
        solution<-vector()
        trial_ext<-list()
        s_demand<-vector()
        solution[1]<-add_savings[1]
        trial_ext[[1]]<-unlist(strsplit(add_savings[1],"-"))
        s_demand[1]<-sum(demand[c(as.numeric(unlist(strsplit(solution,"-"))))])
        test_capacity<-0
        signal<-TRUE
## Check if first solution meets capacity requirements
        if(s_demand[1]>k){
                solution[1]<-unlist(strsplit(add_savings[1],"-"))[which(demand[c(as.numeric(unlist(strsplit(add_savings[1],"-"))))]==min(demand[c(as.numeric(unlist(strsplit(add_savings[1],"-"))))]))[1]]
                trial_ext[[1]]<-c(solution[1],solution[1])
                s_demand[1]<-min(demand[c(as.numeric(unlist(strsplit(add_savings[1],"-"))))])
        }
## The iteration begins, in the worst case scenario all the elements of the add_savings vector will be analized.
        suppressWarnings(for(i in 2:length(add_savings)){
                iter<-add_savings[i]
                signal<-TRUE
## The first condition checks whether the current analyzed element can be part of the current solution
                if(any(mapply(function(x,y){x %in% y},strsplit(add_savings[i],"-"),trial_ext))){
                        if(length(solution)>1){
## This "for" loop triggers a signal when there is an arc that is already conceived within a trial_ext element
                                for(m in 1:length(trial_ext)){
                                        if(all(levels(as.factor(unlist(strsplit(add_savings[i],"-"))))==levels(as.factor(unlist(strsplit(trial_ext[[m]],"-")))))){
                                                signal<-FALSE
                                                break
                                        }
                                }
                                if(any(unlist(strsplit(add_savings[i],"-"))[1]==sapply(trial_ext, "[[", 2)) && any(unlist(strsplit(add_savings[i],"-"))[2]==sapply(trial_ext, "[[", 1)) && signal){
                                        test_capacity<-s_demand[which(unlist(strsplit(add_savings[i],"-"))[1]==sapply(trial_ext, "[[", 2))]+s_demand[which(unlist(strsplit(add_savings[i],"-"))[2]==sapply(trial_ext, "[[", 1))]
## In case the tour formed by two former tours is feasible then solution, s_demand and trial_ext are updated
                                        if(test_capacity<=k){
                                                solution[which(unlist(strsplit(add_savings[i],"-"))[1]==sapply(trial_ext, "[[", 2))]<-paste(solution[which(unlist(strsplit(add_savings[i],"-"))[1]==sapply(trial_ext, "[[", 2))],solution[which(unlist(strsplit(add_savings[i],"-"))[2]==sapply(trial_ext, "[[", 1))],sep="-")
                                                solution[which(unlist(strsplit(add_savings[i],"-"))[2]==sapply(trial_ext, "[[", 1))]<-NA
                                                solution<-solution[!is.na(solution)]
                                                s_demand[which(unlist(strsplit(add_savings[i],"-"))[1]==sapply(trial_ext, "[[", 2))]<-test_capacity
                                                s_demand[which(unlist(strsplit(add_savings[i],"-"))[2]==sapply(trial_ext, "[[", 1))]<-NA
                                                s_demand<-s_demand[!is.na(s_demand)]
                                                trial_ext[[which(unlist(strsplit(add_savings[i],"-"))[1]==sapply(trial_ext, "[[", 2))]]<-c(trial_ext[[which(unlist(strsplit(add_savings[i],"-"))[1]==sapply(trial_ext, "[[", 2))]][1],trial_ext[[which(unlist(strsplit(add_savings[i],"-"))[2]==sapply(trial_ext, "[[", 1))]][2])
                                                trial_ext[[which(unlist(strsplit(add_savings[i],"-"))[2]==sapply(trial_ext, "[[", 1))]]<-NA
                                                trial_ext<-trial_ext[!is.na(trial_ext)]
                                        } else 
                                                next
                                }
                        }
## If signal is set to FALSE it means that the current arc being analyzed is already part of a solution so we 
## should go on and continue with the next iteration                        
                        if(!signal)
                                next
## Another loop starts to check all the elements of the trial_ext list  
                        for(j in 1:length(trial_ext)){
## If the first element of an element from the trial_ext list is equal to the second element of the current arc that is being analyzed and the first element
## of the current analyzed arc is not included in the current solution then it is added to the solution and trial_ext and s_demand are updated.
                                if(trial_ext[[j]][1]==unlist(strsplit(add_savings[i],"-"))[2] && !(any(mapply(function(x,y){x %in% y},unlist(strsplit(add_savings[i],"-"))[1],unlist(strsplit(solution,"-")))))){
                                        if(sum(s_demand[j],demand[as.numeric(unlist(strsplit(add_savings[i],"-"))[1])])<=k){
                                                solution[j]<-paste(unlist(strsplit(add_savings[i],"-"))[1],solution[[j]],sep="-")
                                                trial_ext[[j]][1]<-c(unlist(strsplit(add_savings[i],"-"))[1])
                                                s_demand[j]<-sum(s_demand[j],demand[as.numeric(unlist(strsplit(add_savings[i],"-"))[1])])
                                                break
                                        }
                                        else
                                                break
## If the second element of an element from the trial_ext list is equal to the first element of the current arc that is being analyzed and the second element
## of the current analyzed arc is not included in the current solution then it is added to the solution and trial_ext and s_demand are updated.
                                } else if(trial_ext[[j]][2]==unlist(strsplit(add_savings[i],"-"))[1] && !(any(mapply(function(x,y){x %in% y},unlist(strsplit(add_savings[i],"-"))[2],unlist(strsplit(solution,"-")))))){
                                        if(sum(s_demand[j],demand[as.numeric(unlist(strsplit(add_savings[i],"-"))[2])])<=k){
                                                solution[j]<-paste(solution[[j]],unlist(strsplit(add_savings[i],"-"))[2],sep="-")
                                                trial_ext[[j]][length(trial_ext)]<-c(unlist(strsplit(add_savings[i],"-"))[2])
                                                s_demand[j]<-sum(s_demand[j],demand[as.numeric(unlist(strsplit(add_savings[i],"-"))[2])])
                                                break
                                        }
                                        else
                                                break
                                }
                        } 
## If the current arc being analyzed is not part of the solution then it becomes part of a new subroute
                } else if (!any(mapply(function(x,y){x %in% y},strsplit(add_savings[i],"-"),unlist(strsplit(solution,"-"))))){
                        test_capacity<-sum(demand[c(as.numeric(unlist(strsplit(add_savings[i],"-"))))])
                        if(test_capacity<=k){
                                solution[c(length(solution)+1)]<-add_savings[i]
                                trial_ext[[length(trial_ext)+1]]<-unlist(strsplit(add_savings[i],"-"))
                                s_demand[c(length(s_demand)+1)]<-sum(demand[c(as.numeric(unlist(strsplit(add_savings[i],"-"))))])
## If the total demand from the nodes being analyzed exceeds the capacity of the vehicle then the node with the least demand is selected as a solution
## and trial_ext and s_demand are also updated with regards to this solution
                        } else {
                                solution[c(length(solution)+1)]<-unlist(strsplit(add_savings[i],"-"))[which(demand[c(as.numeric(unlist(strsplit(add_savings[i],"-"))))]==min(demand[c(as.numeric(unlist(strsplit(add_savings[i],"-"))))]))[1]]
                                trial_ext[[length(trial_ext)+1]]<-c(solution[c(length(solution))],solution[c(length(solution))])
                                s_demand[c(length(s_demand)+1)]<-min(demand[c(as.numeric(unlist(strsplit(add_savings[i],"-"))))])
                        }
                }
        })

## All the optimized routes, sum of demands and distance travelled variables are stored in the results list. The function returns this list as output.
## If there is any node with demand higher than the given vechile capacity, then this condition, allows to relate the new added nodes to the original ones.        
        if(any(orig_demand>k)){
                for(i in 1:length(solution)){
                        nodes<-vector()
                        nodes<-c(as.numeric(unlist(strsplit(solution[i],"-"))))
                        for(j in 1:length(new_ref)){
                                if(any(new_ref[j]==nodes)){
                                        nodes[which(nodes==new_ref[j])]<-c(ref[j]-1) 
                                        solution[i]<-paste(nodes,collapse="-")
                                }
                        }
                }
        }        
## Once a solution is found, each tour conforming the solution is considered for further analysis, so that a function that solves TSP
## is executed for each tour that connects more than one client nodes.
## There is an additional calculation process that determines distance and time taken for each route
## in the solution.
        source('format_tsp_matrix.R', local = T)
        recorrido<-vector()
        tiempo<-vector()
        for(i in 1:length(solution)){
                if(length(levels(as.factor(unlist(strsplit(solution[i],"-")))))==1){
                        recorrido[i]<-sum(mat_distance[1,1+as.numeric(levels(as.factor(unlist(strsplit(solution[i],"-")))))],mat_distance[1+as.numeric(levels(as.factor(unlist(strsplit(solution[i],"-"))))),1])
                        tiempo[i]<-sum(mat_time[1,1+as.numeric(levels(as.factor(unlist(strsplit(solution[i],"-")))))],mat_time[1+as.numeric(levels(as.factor(unlist(strsplit(solution[i],"-"))))),1])
                        solution[i]<-paste(0,levels(as.factor(unlist(strsplit(solution[i],"-")))),0,sep = "-")
                        s_demand[i]<-s_demand[i]
                } else {
                        results<-list()
                        results<-format_tsp_matrix(cost_mat,solution[i])
                        solution[i]<-results$tour
                        if(cost=="D"){
                                recorrido[i]<-results$distance
                                nodes<-vector()
                                nodes<-c(as.numeric(unlist(strsplit(solution[i],"-"))))
                                tiempo[i]<-0
                                for(j in 1:(length(nodes)-1)){
                                        tiempo[i]<-tiempo[i]+mat_time[nodes[j]+1,nodes[j+1]+1]
                                }
                        } else {
                                tiempo[i]<-results$distance
                                nodes<-vector()
                                nodes<-c(as.numeric(unlist(strsplit(solution[i],"-"))))
                                recorrido[i]<-0
                                for(j in 1:(length(nodes)-1)){
                                        recorrido[i]<-recorrido[i]+mat_distance[nodes[j]+1,nodes[j+1]+1]
                                }
                        }
                        track[i]<-results$distance
                }
        }
## The function output is a list with 4 elements:routes, loads, trip distance and time
        results<-list()
        results[[1]]<-solution
        results[[2]]<-s_demand
        results[[3]]<-recorrido
        results[[4]]<-tiempo
        names(results)<-c("Rutas","Carga","Distancia","Tiempo")
        results
}