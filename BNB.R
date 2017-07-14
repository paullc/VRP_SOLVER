## The function solves the Travelling Salesman Problem using the Branch and Bound 
## algorithm proposed by Little et.al. The only argument given to the function is 
## "distance" matrix.
BNB<-function(distance){
  source('get_mp.R', local = T)
  source('get_subtours_mod.R', local = T)
## Step 1: The "OriginalCM" matrix is created as a copy of the "distance" matrix.
## Rown and columns are assigned names, which will be later used in order to index
## elements that are included in several operations. The variable "x" corresponds 
## to the number of iterations, it is initialized to "1". The variable "k" is used
## in order to assign the address of the next arc that will be added to the solution.
## The variable "zo" allows the program to identify when the optimal solution has been
## achieved. The vector "w" is used to store the lower bounds for each iteration. The
## "solution" vector is employed to store the arcs corresponding to the optimal solution.
## The "trial_solution" vector is used to store temporary solutions, when a shift is made 
## from one major branch of the tree to another. Vectors "iter_solution" and "iter_neg_kl" 
## are auxiliary elements that allow to temporarily store current values of arcs that 
## should and should not be included in the solution.The "neg_kl" vector is employed to 
## store the arcs that should not be included in the optimal solution. The "trial_neg_kl" 
## vector is used to store temporary forbidden arcs, when a shift is made from one major 
## branch of the tree to another. The vector "first_alt_branch" stores the values for first
## lower limits corresponding to whether the first found arc should or should not be included
## in the solution.
        results<-list()
        OriginalCM<-distance
        rownames(OriginalCM)<-c(seq(1,ncol(OriginalCM),1))
        colnames(OriginalCM)<-c(seq(1,ncol(OriginalCM),1))
        x<-1
        k<-1
        zo<-Inf
        w<-vector()
        solution<-vector()
        trial_solution<-vector()
        iter_solution<-vector()
        iter_neg_kl<-vector()
        neg_kl<-vector()
        trial_neg_kl<-vector()
        first_alt_branch<-vector()
## Step 2: The matrix "ReducedCM" over which reduction operations will be perfomed is created as a 
## copy of "OriginalCM". The minimum value from each row is identified and subsequently substracted
## from the elements of every corresponding row. The minimum value from each column is identified 
## and subsequently substracted from the elements of every corresponding column. The initial lower
## bound is stored in w[1], which is equal to the sum of minimums.
        ReducedCM<-OriginalCM
        minrow<-apply(ReducedCM,1,min)
        ReducedCM<-ReducedCM-minrow
        mincol<-apply(ReducedCM,2,min)
        ReducedCM<-t(t(ReducedCM)-mincol)
        w[x]<-sum(minrow)+sum(mincol)
## Step 3: The main loop starts by setting penalties over the elements of the "ReducedCM"
## matrix which are equal to "0". The zero element with the highest penalty is selected 
## as the current "kl" element. This element will be tested, in order to decide if whether
## it should or should not be included in the solution. A condition is added at this instance,
## in order to determine if a returning "ReducedCM" coming from another major branch has already
## been reduced to a 2x2 size matrix.
        suppressWarnings(repeat{
                if(nrow(ReducedCM)==2){
                        solution[k]<-paste(rownames(ReducedCM)[which(ReducedCM==0,
                                arr.ind = TRUE)[1,1]], colnames(ReducedCM)
                                [which(ReducedCM==0,arr.ind = TRUE)[1,2]],sep="-")
                        solution[k+1]<-paste(rownames(ReducedCM)[which(ReducedCM==0,
                                arr.ind = TRUE)[2,1]],colnames(ReducedCM)
                                [which(ReducedCM==0, arr.ind = TRUE)[2,2]],sep="-")
                        break
                }
                pot_add<-matrix()
                penalties<-vector()
                pot_add<-which(ReducedCM==0,arr.ind = TRUE)
                for(i in 1:nrow(pot_add)){
                        penalties[i]<-min(ReducedCM[pot_add[i,1],-c(pot_add[i,2])])+
                                min(ReducedCM[-c(pot_add[i,1]),pot_add[i,2]])
                }
                kl<-paste(rownames(ReducedCM)[pot_add[which(penalties==max(penalties))
                                [1],1]],colnames(ReducedCM)[pot_add[which(penalties==
                                max(penalties))[1],2]],sep="-")
## Step 4: The lower limit "w_n" with respect to "kl" not included in the solution is calculated as the sum
## of the current lower bound plus the current calculated penalties. 
                w_n<-w[x]+penalties[which(penalties==max(penalties))[1]]
## Step 5: The function "get_mp" is loaded, and the "mp" value is obtained. The variable "mp" corresponds to
## ending node "m" and starting node "p" of a subtour containing the current "kl" arc. Row "k" and column "l" 
## from "ReducedCM" are eliminated. The cell with adress given by "mp" is set to Inf. Matrix reduction 
## operations are perfomed again over "ReducedCM". The lower limit "w_y" with respect to "kl" included in 
## the solution is calculated as the sum of the current lower bound plus the sum of minimums. 
                mp<-get_mp(solution,kl)
                ReducedCM<-ReducedCM[-pot_add[which(penalties==max(penalties))[1],1],
                                     -pot_add[which(penalties==max(penalties))[1],2]]
                if (any(rownames(ReducedCM)==unlist(strsplit(mp,"-"))[1]) && 
                    any(colnames(ReducedCM)==unlist(strsplit(mp,"-"))[2]))
                        ReducedCM[which(rownames(ReducedCM)==unlist(strsplit(mp,"-"))[1]),
                                  which(colnames(ReducedCM)==unlist(strsplit(mp,"-"))
                                  [2])]<-Inf
                minrow<-apply(ReducedCM,1,min)
                ReducedCM<-ReducedCM-minrow
                mincol<-apply(ReducedCM,2,min)
                ReducedCM<-t(t(ReducedCM)-mincol)
                w_y<-w[x]+sum(minrow)+sum(mincol)
## Step 6: The condition from this sections checks whether the size of "ReducedCM" is 2x2.
## If the condition is TRUE, the solution is stored and zo is given the value of w_y.
                if(nrow(ReducedCM)==2){
        ## Step 11
                        if(w_y<zo){
        ## Step 12              
                                zo<-w_y
                                first_solution<-solution
                                first_solution[k]<-kl
                                first_solution[k+1]<-paste(rownames(ReducedCM)
                                        [which(ReducedCM==0,arr.ind = TRUE)
                                        [1,1]],colnames(ReducedCM)
                                        [which(ReducedCM==0,arr.ind = TRUE)
                                        [1,2]],sep="-")
                                first_solution[k+2]<-paste(rownames(ReducedCM)
                                        [which(ReducedCM==0,arr.ind = TRUE)[2,1]],
                                        colnames(ReducedCM)[which(ReducedCM==0,
                                        arr.ind = TRUE)[2,2]],sep="-")
                                
                        }
                }
## Step 7: During the first iteration (x==1) the two major branches are generated. The bound 
## associated to the alternative that will not be selected for the next iteration is
## stored in the vector "first_alternative_branch". The variable "alt_branch" stores 
## the value of current bound that was not selected for the next iteration. The
## iteration number "x" is updated. The next lower bound w[x] is updated with the minimum
## between w_n and w_y.
                if(x==1){
                        first_alt_branch<-c(0,0)
                        if(w_n==max(w_n,w_y)){
                                first_alt_branch[1]<-c(w_n)
                                alt_branch<-first_alt_branch[1]
                                alt_kl<-kl
                        } else if(w_y==max(w_n,w_y)) {
                                first_alt_branch[2]<-c(w_y)
                                alt_branch<-first_alt_branch[2]
                                alt_kl<-kl
                        }        
                }
                x<-x+1
                w[x]<-min(w_n,w_y,alt_branch)
## Step 8: At this stage, the program tests if whether an optimal solution has been reached
## If the condition is TRUE, the repeat loop stops.
                if(zo<=w[x])
                        break
## Step 9: The condition tests if the next lower bound implies not selecting the current "kl" 
## arc as an element to the solution or if the w[x] was updated from a bound in the alternative
## major branch.
                if(w[x]!=w_y){
##The condition tests if the current "kl" will be forbidden from the actual solution
                        if (w[x] == w_n){
## Set "ReducedCM" equal to "OriginalCM"
                                ReducedCM<-OriginalCM
## Under this section the algorithm finds g (sum of costs from Orig.matrix), delete 
## rows corresponding to committed city pairs and set c(m,p) to Inf. c(k,l)<-Inf. 
## If w[x]==w_n then the current "kl" is stored in the vector "neg_kl", modifications
## of "ReducedCM" are performed taking into account which arcs are currently included 
## and forbidden from the solution
                                g<-0
                                if(length(solution)==0){
                                        if(length(neg_kl)==0){
                                                ReducedCM[which(rownames(ReducedCM)==
                                                        unlist(strsplit(kl,"-"))[1]),
                                                        which(colnames(ReducedCM)==
                                                        unlist(strsplit(kl,"-"))[2])]<-Inf
                                                m<-1
                                                neg_kl[m]<-kl
                                                m<-m+1
                                        } else {
                                                neg_kl[m]<-kl
                                                for(i in 1:length(neg_kl)){
                                                        if (any(rownames(ReducedCM)==
                                                                unlist(strsplit(neg_kl[i],
                                                                "-"))[1]) && 
                                                                any(colnames(ReducedCM)
                                                                ==unlist(strsplit(neg_kl[i],
                                                                "-"))[2]))
                                                                ReducedCM[
                                                                which(rownames(ReducedCM)==
                                                                unlist(strsplit(neg_kl[i],
                                                                "-"))[1]),which(
                                                                colnames(ReducedCM)==
                                                                unlist(strsplit(neg_kl[i],
                                                                "-"))[2])]<-Inf
                                                }
                                                m<-m+1
                                        }
                                } else if(length(solution)!=0){
                                        for(i in 1:length(solution)){
                                                g<-g + OriginalCM[c(as.numeric(unlist(strsplit
                                                        (solution[i],"-"))[1])),c(as.numeric
                                                        (unlist(strsplit(solution[i],"-"))[2]))]
                                                ReducedCM<-ReducedCM[-c(which(rownames(ReducedCM)
                                                                    ==unlist(strsplit(solution[i],
                                                                    "-"))[1])),-c(which(colnames
                                                                    (ReducedCM)==unlist(strsplit
                                                                    (solution[i], "-"))[2]))]
                                                mp<-get_mp(solution[0:c(i-1)],solution[i])
                                                if (any(rownames(ReducedCM)==unlist(strsplit
                                                        (mp,"-"))[1]) && any(colnames
                                                        (ReducedCM)==unlist(strsplit
                                                        (mp,"-"))[2]))
                                                        ReducedCM[which(rownames(ReducedCM)==
                                                                unlist(strsplit(mp,"-"))[1]),
                                                                which(colnames(ReducedCM)==
                                                                unlist(strsplit(mp,"-"))[2])]<-Inf
                                        }
                                        if(length(neg_kl)==0){
                                                ReducedCM[which(rownames(ReducedCM)==unlist
                                                        (strsplit(kl,"-"))[1]),which(colnames
                                                        (ReducedCM)==unlist(strsplit(kl,"-"))
                                                        [2])]<-Inf
                                                m<-1
                                                neg_kl[m]<-kl
                                                m<-m+1
                                        } else {
                                                neg_kl[m]<-kl
                                                for(i in 1:length(neg_kl)){
                                                        if (any(rownames(ReducedCM)==unlist
                                                                (strsplit(neg_kl[i],"-"))[1]) 
                                                                && any(colnames(ReducedCM)==
                                                                unlist(strsplit(neg_kl[i],"-"))
                                                                [2]))
                                                                ReducedCM[which(rownames
                                                                        (ReducedCM)==unlist
                                                                        (strsplit(neg_kl[i],"-"))
                                                                        [1]),which(colnames
                                                                        (ReducedCM)==unlist
                                                                        (strsplit(neg_kl[i],
                                                                        "-"))[2])]<-Inf
                                                }
                                                m<-m+1
                                        }
                                }
## After properly modifying "ReducedCM", it is again reduced before it enters the next 
## iteration
                                minrow<-apply(ReducedCM,1,min)
                                ReducedCM<-ReducedCM-minrow
                                mincol<-apply(ReducedCM,2,min)
                                ReducedCM<-t(t(ReducedCM)-mincol)
## w[x] is updated
                                w[x]<-g+sum(minrow)+sum(mincol)
## In case w[x] was updated from an alternative major branch then the values that should be
## included and forbidden from the solution need to be updated from what was previously stored
## under the vectors "trial_solution" and "trial_neg_kl". 
                        } else {
                                ReducedCM<-OriginalCM
                                g<-0
## For the first shift towards the alternative major branch the following operations are 
## carried out.
                                if(any(first_alt_branch==alt_branch)){
                                        k<-1
                                        trial_solution<-solution
                                        trial_neg_kl<-neg_kl
                                        solution<-vector()
                                        minrow<-apply(ReducedCM,1,min)
                                        ReducedCM<-ReducedCM-minrow
                                        mincol<-apply(ReducedCM,2,min)
                                        ReducedCM<-t(t(ReducedCM)-mincol)
                                        if(which(first_alt_branch!=0)==1){
                                                ReducedCM[as.numeric(unlist(strsplit(alt_kl,"-"))
                                                        [1]),as.numeric(unlist(strsplit(alt_kl,
                                                        "-"))[2])]<-Inf
                                                minrow<-apply(ReducedCM,1,min)
                                                ReducedCM<-ReducedCM-minrow
                                                mincol<-apply(ReducedCM,2,min)
                                                ReducedCM<-t(t(ReducedCM)-mincol)
                                                w[x]<-alt_branch
                                                m<-1
                                                neg_kl[m]<-alt_kl
                                                m<-m+1
                                                solution<-vector()
                                        } else if(which(first_alt_branch!=0)==2){
                                                mp<-get_mp(solution,alt_kl)
                                                ReducedCM<-ReducedCM[-c(as.numeric(unlist(strsplit
                                                        (alt_kl,"-"))[1])),-c(as.numeric(unlist
                                                        (strsplit(alt_kl,"-"))[2]))]
                                                if (any(rownames(ReducedCM)==unlist(strsplit(mp,
                                                        "-"))[1]) && any(colnames(ReducedCM)==
                                                        unlist(strsplit(mp,"-"))[2]))
                                                        ReducedCM[which(rownames(ReducedCM)==
                                                                unlist(strsplit(mp,"-"))[1]),
                                                                which(colnames(ReducedCM)==
                                                                unlist(strsplit(mp,"-"))[2])]<-Inf
                                                minrow<-apply(ReducedCM,1,min)
                                                ReducedCM<-ReducedCM-minrow
                                                mincol<-apply(ReducedCM,2,min)
                                                ReducedCM<-t(t(ReducedCM)-mincol)
                                                w[x]<-alt_branch
                                                neg_kl<-vector()
                                                solution[k]<-alt_kl
                                                k<-k+1
                                        }
                                        alt_branch<-min(w_y,w_n)
                                        if(alt_branch==w_y){
                                                trial_solution<-c(trial_solution,kl)
                                        } else if(alt_branch==w_n)
                                                trial_neg_kl<-c(trial_neg_kl,kl)
## Otherwise if there is a shift to the alternative major branch, but the bound associated
## with it is not one of the initial bounds defined in the first iteration.
                                } else {
                                        k<-length(trial_solution)+1
                                        iter_solution<-solution
                                        iter_neg_kl<-neg_kl
                                        solution<-vector()
                                        solution<-trial_solution
                                        neg_kl<-vector()
                                        neg_kl<-trial_neg_kl
                                        for(i in 1:length(trial_solution)){
                                                g<-g + OriginalCM[c(as.numeric(unlist(strsplit
                                                        (trial_solution[i],"-"))[1])),
                                                        c(as.numeric(unlist(strsplit
                                                        (trial_solution[i],"-"))[2]))]
                                                ReducedCM<-ReducedCM[-c(which(rownames
                                                        (ReducedCM)==unlist(strsplit
                                                        (trial_solution[i],"-"))[1])),
                                                        -c(which(colnames(ReducedCM)==
                                                        unlist(strsplit(trial_solution[i],
                                                        "-"))[2]))]
                                                mp<-get_mp(trial_solution[0:c(i-1)],
                                                           trial_solution[i])
                                                if (any(rownames(ReducedCM)==unlist(strsplit
                                                        (mp,"-"))[1]) && any(colnames(ReducedCM)
                                                        ==unlist(strsplit(mp,"-"))[2]))
                                                        ReducedCM[which(rownames(ReducedCM)==
                                                                unlist(strsplit(mp,"-"))[1]),
                                                                which(colnames(ReducedCM)==
                                                                unlist(strsplit(mp,"-"))[2])]<-Inf
                                        }
                                        if (length(trial_neg_kl)!=0){
                                                for(i in 1:length(trial_neg_kl)){
                                                        if (any(rownames(ReducedCM)==unlist
                                                                (strsplit(trial_neg_kl[i],"-"))
                                                                [1]) && any(colnames(ReducedCM)
                                                                ==unlist(strsplit(trial_neg_kl[i],
                                                                "-"))[2]))
                                                                ReducedCM[which(rownames(ReducedCM)
                                                                        ==unlist(strsplit
                                                                        (trial_neg_kl[i],"-"))[1]),
                                                                        which(colnames(ReducedCM)==
                                                                        unlist(strsplit
                                                                        (trial_neg_kl[i],"-"))[2])]<-Inf
                                                }
                                        }
                                        minrow<-apply(ReducedCM,1,min)
                                        ReducedCM<-ReducedCM-minrow
                                        mincol<-apply(ReducedCM,2,min)
                                        ReducedCM<-t(t(ReducedCM)-mincol)
                                        w[x]<-alt_branch
                                        alt_branch<-min(w_y,w_n)
                                        trial_solution<-vector()
                                        trial_neg_kl<-vector()
                                        trial_solution<-iter_solution
                                        trial_neg_kl<-iter_neg_kl
                                        if(alt_branch==w_y){
                                                trial_solution<-c(trial_solution,kl)
                                        } else if(alt_branch==w_n)
                                                trial_neg_kl<-c(trial_neg_kl,kl)
                                } 
                        }
## If w[x]==w_y then the current "kl" is added to the solution and k is updated for 
## the next iteration
                } else {
                        solution[k]<-kl
                        k<-k+1
                }
        })
## Before the solution is displayed/printed, the algorithm determines where the optimal solution
## was stored, and depending on the arcs selected it obtains the sum of the distances 
## from the selected arcs.
        distance<-c(0)
        if (length(solution)==nrow(OriginalCM)){
                # results[[1]]<-get_subtours(solution)
                print(paste("The optimal route is:",get_subtours(solution)))
                for (i in 1:length(solution))
                        distance<-distance+OriginalCM[as.numeric(unlist(strsplit
                                (solution[i],"-")))[1],as.numeric(unlist(strsplit
                                (solution[i],"-")))[2]]
                results[[2]]<-distance
                # print(paste("The distance travelled is:",distance))
        }
        else{
                results[[1]]<-get_subtours(first_solution)
                # print(paste("The optimal route is:",get_subtours(first_solution)))
                for (i in 1:length(first_solution))
                        distance<-distance+OriginalCM[as.numeric(unlist(strsplit
                                (first_solution[i],"-")))[1],as.numeric(unlist
                                (strsplit(first_solution[i],"-")))[2]]
                results[[2]]<-distance
                # print(paste("The distance travelled is:",distance))
        }
        names(results)<-c("tour","distance")
        results
}