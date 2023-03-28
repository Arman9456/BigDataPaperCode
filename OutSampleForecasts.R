


####### Rolling window cross validation and then out of sample Predictions!!!


OutOfSampleForecasts <- function(data) {
  BelgiumdataNew = data[1:52,] # we dont use last 13 observations at all in cross validation

  Lasso=TRUE# change to false if you want ridge!!!!!!!!

  window_size = 42 # use 52*0.8 years of data for training
  step_size = 1 # move the window forward by 1month each time

  # initialize vectors to store results
  cv_errors <- matrix(nrow= nrow(BelgiumdataNew) - window_size, ncol=100)
  lambda_seq <- rep(NA,100) # vector of 100 different lambdas for each validation period

  #Select the 100 lambdas based on e.g. the minimum and maximum lambda used below
  if(Lasso==TRUE){
    fit <- glmnet(x = as.matrix(BelgiumdataNew[, -1]), y = as.matrix(BelgiumdataNew[, 1]), alpha = 1, lambda.max.ratio = 0.7)
  } else { # if not Lasso, we use Ridge (alpha=0)
    fit <- glmnet(x = as.matrix(BelgiumdataNew[, -1]), y = as.matrix(BelgiumdataNew[, 1]), alpha = 0, lambda.max.ratio = 0.7)
  }

  lambda_seq <- seq(min(fit$lambda), max(fit$lambda), length.out = 100)

  for (i in 1:(nrow(BelgiumdataNew) - window_size)) {
    # Define training and test sets for this window
    train <- BelgiumdataNew[i:(i+window_size-1), ]
    test <- BelgiumdataNew[i+window_size, ]

    # Set up lambda sequence
    #fit <- glmnet(x = as.matrix(train[, -1]), y = train[, 1], alpha = 1, lambda.max.ratio = 0.7)
    #lambda_seq[i,] <- seq(min(fit$lambda), max(fit$lambda), length.out = 100)

    # for all lambdas of the current training set, get the fit
    for( j in 1:100){
      if(Lasso==TRUE){
        newFit = glmnet(x = as.matrix(train[, -1]), y = as.matrix(train[, 1]), alpha = 1, lambda = lambda_seq[j])
      } else{
        newFit = glmnet(x = as.matrix(train[, -1]), y = as.matrix(train[, 1]), alpha = 0, lambda = lambda_seq[j])
      }

      # make prediction on test set and calculate error (MSE) for next period for each lambda
      test_pred <- predict(newFit, newx = as.matrix(test[, -1]))
      cv_errors[i,j] <- unlist((test[, 1] - test_pred)^2)

    }

  }

  # Now for each lambda we calculate the average of the MSE's over all validation periods
  # Then we choose the lambda with the lowest average MSE across all validation periods.

  AverageMSEs = apply(cv_errors,2,mean)
  LowestAverageMSE = min(AverageMSEs)
  IndexLowestAverageMSE = which.min(AverageMSEs)

  Xtrain = as.matrix(BelgiumdataNew[, -1])
  Ytrain = as.matrix(BelgiumdataNew[, 1])

  Xtest= as.matrix(data[(nrow(BelgiumdataNew)+1):nrow(data), -1])
  Ytest= as.matrix(data[(nrow(BelgiumdataNew)+1):nrow(data), 1])

  if(Lasso==TRUE){
    fitOnTrain = glmnet(Xtrain,Ytrain,alpha=1)
  } else{
    fitOnTrain = glmnet(Xtrain,Ytrain,alpha=0)
  }

  #Bestfit Lasso or ridge
  if(Lasso==TRUE){
    BestFit = glmnet(Xtrain,Ytrain,alpha=1,lambda=lambda_seq[IndexLowestAverageMSE])
  } else{
    BestFit = glmnet(Xtrain,Ytrain,alpha=0,lambda=lambda_seq[IndexLowestAverageMSE])
  }

  df2 = as.matrix(coef(BestFit))
  NonZeroCoeffs = df2[rowSums(df2[])!=0,]
  nonZeroCoeffs = as.data.frame(as.table(NonZeroCoeffs),responseName='Coefficients')
  nonZeroCoeffs
  formattable(nonZeroCoeffs)

  sum(BestFit$beta !=0) # count number of variables selected by Lasso.

  ### Put it nicely in Latex ##################################
  rows <- seq_len(nrow(nonZeroCoeffs) %/% 2)
  kable(list(nonZeroCoeffs[rows,1:2],
             matrix(numeric(), nrow=0, ncol=1),
             nonZeroCoeffs[-rows, 1:2]),
        caption = "This is the caption.",
        label = "tables", format = "latex", booktabs = TRUE)
  ##################################################################

  #Predict test set with lambda from cross validation
  pred2= predict(fitOnTrain,s=lambda_seq[IndexLowestAverageMSE],newx=Xtest) # pretty decent estimates

  plot(1:13, Ytest, type = "l",ylab= expression(paste(Delta," Real GDP")),xlab="Test Period" )
  lines(1:13, pred2, col = "blue",type="l",lty=2)

  #RMSE
  RMSEs = sqrt(sum((Ytest-pred2)^2))
  print(RMSEs)
}
