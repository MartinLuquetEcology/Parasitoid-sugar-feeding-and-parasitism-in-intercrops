#### Functions 

#### Cf http://www.cookbook-r.com/Manipulating_data/Converting_between_data_frames_and_contingency_tables/
# Convert from data frame of counts to data frame of cases.
# `countcol` is the name of the column containing the counts
countsToCases <- function(x, countcol = "Freq") {
  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
  
  # Drop count column
  x[[countcol]] <- NULL
  
  # Get the rows from x
  x[idx, ]
}



#### Function for prevalence estimation via Adjusted Counting (AC) ####
### https://github.com/MartinLuquetEcology/Insect-feeding-inference.git ###
#### See Forman et al. (2008), Sebastiani (2015), Gonzalez et al. (2017)
#### Works for any number of classes (but only tested for 2 and 3 classes)


#### "unadj" is a vector containing the unadjusted counts (estimated via Classify and Count)
#### "table" is the confusion matrix returned by a classifier


adjust.prev <- function(unadj,table){
  
  #Let (C1, C2, C3...) be the real classes and (c1, c2, c3...) be the predicted classes
  #Let p0(C1) be the "unadjusted" prevalence for C1 (estimated via counting predicted samples) and p(C1) be the real prevalence
  #Then
  #p0(C1) = p(C1) * p(c1|C1) + p(C2) * p(c1|C2) + p(C3) * p(c1|C3) + ...
  #Same for p0(C2), p0(C3), ...
  #We can estimate the true and false positive error rates using the training set
  #We get "unadjusted" prevalences from the test set
  #So to estimate p(C1), p(C2), etc. (i. e. to get the adjusted prevalences)
  #We need to resolve the system "unadjusted prevalences" = "adjusted prevalences" * "error rates"
  
  options(error = NULL)  
  
  # If one class has a true positive rate of 0, we can't use Adjusted Counting
  # The classifier is probably too bad
  if (sum(diag(table) == 0)>0) {stop(
    "Prevalences cannot be adjusted if for at least one class, no individual was correctly detected in the training set",call.=F)
  }  
  
  
  #Error rates matrix
  rates <- matrix(0,nrow(table),ncol(table))
  
  for (i in 1:nrow(table))
  {
    falserates <- table[i,-i]/sum(table[i,])
    rates [-i,i] <- falserates
    rates [i,i] <- 1-sum(falserates)
    
  }
  
  #Unadjusted prevalences matrix
  nb.Ind <- sum(unadj)
  
  if(class(try(solve(rates)))[1] != "matrix"){stop(
    "Matrix is not solvable: impossible to adjust prevalences. Dataset may be too small or classifier is not adapted")
  }
  
  #Solving
  est.vector <- solve(rates,unadj)
  
  ### If one class is estimated as <0%, it is corrected as being 0% (Forman 2008)
  ### Other classes are-readjusted so that they sum to nb.Ind
  if (sum(est.vector>=0) != length(est.vector))
  {
    
    underZero <- rownames(table)[est.vector<0]
    
    for (i in 1:length(underZero)){
      cat(underZero[i],"class was estimated as negative - corrected as zero \n \n")
    }
    
    
    est.vector[est.vector<0] <- 0
    est.vector[est.vector>0] <- (est.vector[est.vector>0] / sum(est.vector[est.vector>0])) * nb.Ind
  }
  
  
  
  # Adjusted prevalence values are stored here
  adj.prev <- (round(est.vector))
  names(adj.prev) <- rownames(table)
  
  ### If there is a 1 difference between estimated number of individuals
  ## We round the class that is closest to the superior/inferior unit
  
  if(sum(adj.prev) - nb.Ind == 1) {  
    min.deci <- which.min(est.vector*nb.Ind-floor(est.vector*nb.Ind))
    adj.prev[min.deci] <- adj.prev[min.deci]-1
  } 
  
  if(sum(adj.prev) - nb.Ind == -1) {  
    max.deci <- which.max(est.vector*nb.Ind-floor(est.vector*nb.Ind))
    adj.prev[max.deci] <- adj.prev[max.deci]+1
  } 
  
  
  return(adj.prev)
}


