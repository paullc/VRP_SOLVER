## The function "get_subtours" generates a string that describes the route(s) associated
## with a set of arcs.
get_subtours<-function(tour){
## The variable trigger is employed so that the resulting route starts at node "1"
        trigger<-c("1")
## The route(s) will be stored in the vector subtours
        subtours<-vector()
## The variable test is introduced as a temporary storing device, and it is initialized 
## to the value of the starting node"1". The variable "start" is also set to "1". A tour
## starts and ends at the initial node, so that the variable "start" allows to determine 
## when the while loop should stop. Variable "l" is employed in order to allocate values
## to different "l" addresses of a vector.
        test<-trigger
        start<-trigger
        l<-0
## The while loop is employed in order to generate  all the existing subtours. The first
## subtour includes the arc which first node is equal to "1". The arc is stored in the 
## variable k. Within the repeat loop, the variables "trigger" and "k" change for every 
## iteration. For every iteration "k" is assigned the arc whose first element is equal 
## to "trigger". For every iteration "trigger" is assigned the value of the second node
## of the current "k" arc. The "repeat" loop stops when "trigger" is equal to the value
## that started the current subtour.
        while(length(levels(as.factor(unlist(strsplit(test,"-")))))<length(tour)){
                l<-l+1
                subtours[l]<-trigger
                repeat{
                        k<-which(unlist(lapply(strsplit(tour,"-"),"[[",1))==trigger)
                        subtours[l]<-paste(subtours[l],strsplit(tour,"-")[[k]][2],
                                sep = "-")
                        trigger<-strsplit(tour,"-")[[k]][2]
                        if(trigger==start)
                                break
                }
## "test" is a string formed upon the nodes of a subtour.
                test<-paste(subtours,collapse = "-")
## A sequence (by 1) is generated from the minimum and maximum of the nodes included in "test". This
## sequence is compared with the exaxt nodes of the "test" string. If there is any difference then 
## "trigger" is set as the minimum node not included in the "test" string and "start" is also set to 
## this value. Otherwise "trigger" is assigned a value corresponding to the maximum node of the "test"
## string incremented by one; "start" is also set to this value.
                if (length(setdiff(seq(min(as.numeric(unlist(strsplit(test,"-")))),
                        max(as.numeric(unlist(strsplit(test,"-")))),1),as.numeric
                        (levels(as.factor(unlist(strsplit(test,"-")))))))>0) {
                        trigger<-as.character(min(setdiff(seq(min(as.numeric(unlist
                                (strsplit(test,"-")))),max(as.numeric(unlist
                                (strsplit(test,"-")))),1),as.numeric(levels
                                (as.factor(unlist(strsplit(test,"-"))))))))
                        start<-trigger
                } else {
                        trigger<-as.character(max(as.numeric(unlist(strsplit(test,
                                "-"))))+1)
                        start<-trigger
                }
        }
## The function returns the vector "subtours" which corresponds to the set of arcs given as a function 
## argument.
        subtours
        
}