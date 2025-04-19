
# Functions for computing RMSE and RMSE-%
rmse <- function(y,yhat) { sqrt(sum((y-yhat)^2) / length(y)) }
relrmse <- function(y,yhat) { sqrt(sum((y-yhat)^2) / length(y)) / mean(y) }

#=============================================================
# Variable selection functions
#=============================================================

# A function that generates alternative predictor variable combinations

PickNewSolution <- function(s,kk,n_iterr){
  
  # s = current solution, k=number of iterations
  # Initially change 2/3 of the variable
  # When k/n >=.8, change only one variable
  s <- sort(s)
  keepmax <- prednum-1          # Maximum number of predictors to keep = prednum-1
  keepmin <- round(1/3*prednum) # Minimum number of predictors to keep = 1/3*length
  
  kperc <- kk/n_iterr           # Iteration progress
  
  # Current number of predictors to keep
  keepcount <- min(length(s)-1, round((keepmax-keepmin) / 0.8*kperc + keepmin)); keepcount
  
  idxx <- colmin:colmax                                 # List of variables to select from
  keep <- sort(sample(s,keepcount,replace=F))           # List of untouchable variables
  ssel <- idxx[! idxx %in% s]                           # List of selectable variables - current sample not allowed
  newvars <- sample(ssel, prednum-keepcount, replace=F) # Sampling from selectables
  sol<-sort(c(newvars,keep))                            # Merge
  sol                                                  # Return new variable list
}

# yaisel-function fits a KNN model and returns the weighted mean RMSE

yaisel<-function(x){ 
  
  xtrain <- as.data.frame(d[,x])                         # List X-variables
  knn <- yai(y=ytrain, x=xtrain, method = met, k=KNNK);  # Fit KNN model
  pred <- impute(knn,k=KNNK,method=wm)                   # Get fitted values
  
  # Initialize rmse vector
  rvec <- -999 
  
  # Calculate mean relative rmse for each response variable
  for(i in 1:length(yvar)) rvec[i] <- relrmse(pred[,i], pred[ , i+length(yvar)])
  
  # Returned value = mean of weighted RMSEs
  mean(rvec*weights) 
}

