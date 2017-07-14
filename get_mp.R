## Function that obtains "mp" values that are considered for every iteration
## conceived in the branch and bound algorithm proposed by Little et.al. So that
## "m" and "p" are the final and initial node of a subtour that contains the arc "kl".
## The algorithm makes use of mps in order to include subtour elimination constrains
get_mp<-function(solution,kl){
## Variable "exit" is defined in order to test if all the nodes of the current subtour
## have already been compared to the nodes that define "kl"        
        exit<-TRUE
## The first condition tests if there is any solution, otherwise "mp" is defined as the 
## inverse of "kl"        
        if(length(solution)>0){
## The second condition tests if the first node "k" or the second node "l" of "kl" 
## are included in the current solution. The variable "path" is introduced in order 
## to store the resulting subtour that includes the arc "kl". It is initialized
## as the current value of "kl"
                if(any(unlist(strsplit(kl,"-"))[1]==unlist(strsplit(solution,"-")))
                   ||any(unlist(strsplit(kl,"-"))[2]==unlist(strsplit(solution,"-")))){
                        path<-kl
## The third condition tests whether the "l" node of "path" is equal to the first node 
## of any arc in the current solution.If the condition is TRUE then a while loop is 
## introduced so that all the arcs that satisfy the condition are included in the 
## variable "path". After the loop ends, the variable "mp" stores the ending node "m" 
## with the starting node "p" of the subtour that includes "kl".  

                        if(any(unlist(strsplit(path,"-"))[2]==unlist(lapply(strsplit
                                (solution,"-"),"[[",1)))){
                                path<-paste(path,unlist(strsplit(solution[which
                                        (unlist(lapply(strsplit(solution,"-"),
                                        "[[",1))==unlist(strsplit(path,"-"))
                                        [2])],"-"))[2],sep="-")        
                                while(any(unlist(strsplit(path,"-"))[length
                                        (unlist(strsplit(path,"-")))]==unlist
                                        (lapply(strsplit(solution,"-"),"[[",1)))){
                                        path<-paste(path,unlist(strsplit(solution
                                                [which(unlist(lapply(strsplit
                                                (solution,"-"),"[[",1))==unlist
                                                (strsplit(path,"-"))[length(unlist
                                                (strsplit(path,"-")))])],"-"))[2],
                                                sep="-")
                                }
                                mp<-paste(unlist(strsplit(path,"-"))[length(unlist
                                        (strsplit(path,"-")))],unlist(strsplit
                                        (path,"-"))[1],sep="-")
                        }
## The next condition tests whether all arcs in the current solution have already been explored.
                        if(all(levels(as.factor(unlist(strsplit(solution,"-"))))==
                               levels(as.factor(unlist(strsplit(path,"-")))))){
                                exit<-FALSE
                        }
## The next condition tests whether the "k" node of "path" is equal to the last node 
## of any arc in the current solution.If the condition is TRUE then a while loop is 
## introduced so that all the arcs that satisfy the condition are included in the 
## variable "path". After the loop ends, the variable "mp" stores the ending node "m" 
## with the starting node "p" of the subtour that includes "kl".  
                        if(exit && any(unlist(strsplit(path,"-"))[1]==unlist(lapply
                                (strsplit(solution,"-"),"[[",2)))){
                                path<-paste(unlist(strsplit(solution[which(unlist
                                        (lapply(strsplit(solution,"-"),"[[",2))==
                                        unlist(strsplit(path,"-"))[1])],"-"))[1],
                                        path,sep="-")        
                                while(any(unlist(strsplit(path,"-"))[1]==unlist
                                          (lapply(strsplit(solution,"-"),"[[",2)))){
                                        path<-paste(unlist(strsplit(solution[which
                                                (unlist(lapply(strsplit(solution,"-"),
                                                "[[",2))==unlist(strsplit(path,"-"))
                                                [1])],"-"))[1],path,sep="-")
                                }
                                mp<-paste(unlist(strsplit(path,"-"))[length(unlist
                                        (strsplit(path,"-")))],unlist(strsplit(path,
                                        "-"))[1],sep="-")
                        }
                        
                } else {
                        mp<-paste(unlist(strsplit(kl,"-"))[2],unlist(strsplit(kl,
                                "-"))[1],sep="-")        
                }
                
        } else {
                mp<-paste(unlist(strsplit(kl,"-"))[2],unlist(strsplit(kl,"-"))
                          [1],sep="-")
        }
## The function returns the variable mp, which will be firther set to Inf in the branch and bound 
## algorithm
        mp
}