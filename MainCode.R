library(forecast)
library("urca")
library("readxl")
library(ISLR2)
library(glmnet)
library(zoo)
library("FAVAR")
library("formattable")
library(knitr)
# The nowcasting package implements the information criteria of Bai & Ng
library(nowcasting) # I installed this manually through zip file and installed all required packages
# bootUR has a simple way to create appropriate differences
library(bootUR)
library("vars")
library(devtools)
#devtools::install_github("gabrielrvsc/HDeconometrics") #downloading HDeconometrics
library("HDeconometrics")
library(sjmisc)
library(hdi)
library(hdm)
library(selectiveInference)
library(PoSI)
library(tidyverse)
library(reshape)

# description variables: https://data-planet.libguides.com/pennworldtables#:~:text=%2C%20current%20prices).-,Expenditure%2Dside%20real%20GDP%20allows%20comparison%20of%20relative%20living%20standards,(ie%2C%20current%20prices).
source("createlags.R")
source("OutSampleForecasts.R")

data <- read_excel("pwt100.xlsx",sheet=3) # get 3rd sheet of excel file which contains the data


# Regress realGDP (rgdpna) on real consumption, real domestic consumption, capital stock, employment (emp), Avg annual hours worked, TFP (productivity), irr, delta

Belgiumdata = data[data$country %in% c("Belgium"), ]
# get rid of data with just 1 value or a lot of NA values
Belgiumdata = subset(Belgiumdata, select = -c(countrycode,currency_unit,country,i_cig,i_xm,i_xr,i_outlier,i_irr,cor_exp,statcap))
Belgiumdata = na.omit(Belgiumdata) # After doing na.omit we are left with observations from year 1954 to 2019

BelgiumDates = Belgiumdata$year
Belgiumdata = Belgiumdata[,-1] # get rid of years before making regressors

BelgiumTseries <- zoo(Belgiumdata, BelgiumDates) # turn into time series

apply(Belgiumdata,MARGIN=2,FUN=plot)

############# Perform unit root tests on the series to potentially first difference them ###########

UnitRootResults <- apply(BelgiumTseries,MARGIN=2,FUN=ur.df,type="trend",selectlags=c("AIC"))
summary(UnitRootResults[[1]])

# since we are going to be differencing some series, we delete the first rows since we lose one observation
NewBelgiumTseries = BelgiumTseries[-1,]
BelgiumdataNew = Belgiumdata[-1,]

for(i in 1:ncol(Belgiumdata)){


  testStat = attr(UnitRootResults[[i]],which='teststat')[1]
  CritValue = attr(UnitRootResults[[i]],which='cval')[1,2] # crit= -3.45 for all tests actually

  if(testStat>=CritValue){ # in this case we fail to reject the null of a unit root and thus have a unit root
    NewBelgiumTseries[,i] = diff(BelgiumTseries[,i])# we difference the corresponding series (but must use the old Tseries to take into account first row 1954)
    BelgiumdataNew[,i] = diff(Belgiumdata[[i]]) # this is for the FAVAR, because it doesn't accept zoo objects

  }
  # else NewBelgiumTseries[,i] remains the same namely the old series but with first row deleted

}

d = diff(Belgiumdata[,2])
# We see only xr,csh_m does not contain a unit root and therefore does not gets differenced
NewBelgiumTseries
BelgiumTseries

# teststatistics = apply(UnitRootResults,FUN=attr,which='teststat')
#   attr(UnitRootResults[[1]],which='teststat')[1]
#   attr(UnitRootResults[[1]],which='cval')[1,2]
#
y <- NewBelgiumTseries$rgdpe
x <- model.matrix(rgdpe~., NewBelgiumTseries)[,-1]

cv.outRidge <- cv.glmnet(x, y, alpha = 0)
minLambdaRidge <- cv.outRidge$lambda.min
ridge.mod <- glmnet(x,y,alpha=0, lambda=minLambdaRidge) # apply ridge with this lambda
coef(ridge.mod)


#outsample forecasts cv#
OutOfSampleForecasts(BelgiumdataNew)


########## Rolling window cross validation for insample forecasts #################

Lasso=FALSE# change to false if you want ridge!!!!!!!!

window_size = 52 # use 52 years of data for training
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

Xtrain = as.matrix(BelgiumdataNew[1:window_size, -1])
Ytrain = as.matrix(BelgiumdataNew[1:window_size, 1])

Xtest= as.matrix(BelgiumdataNew[(window_size+1):nrow(BelgiumdataNew), -1])
Ytest= as.matrix(BelgiumdataNew[(window_size+1):nrow(BelgiumdataNew), 1])

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
pred1= predict(fitOnTrain,s=lambda_seq[IndexLowestAverageMSE],newx=Xtest) # pretty decent estimates

plot(1:13, Ytest, type = "l",ylab= expression(paste(Delta," Real GDP")),xlab="Test Period" )
lines(1:13, pred1, col = "blue",type="l",lty=2)

#RMSE
sqrt(sum((Ytest-pred1)^2))


####################################### Out of sample forecasting ##########################
LassoOutOfSample = FALSE
if(LassoOutOfSample == TRUE){
  cv.outRidge <- cv.glmnet(Xtrain, Ytrain, alpha = 1)
} else{
  cv.outRidge <- cv.glmnet(Xtrain, Ytrain, alpha = 0)
}
minLambdaRidge <- cv.outRidge$lambda.min

coef(ridge.mod)

predRidge = predict(cv.outRidge,s=minLambdaRidge,newx=Xtest)
plot(1:13, Ytest, type = "l",ylab= expression(paste(Delta," Real GDP")),xlab="Test Period" )
lines(1:13, predRidge, col = "blue",type="l",lty=2)

# RMSE
sqrt(sum((Ytest-predRidge)^2))
################### INFORMATION CRITERIA FOR LASSO/RIDGE INSTEAD OF CV ###########################################
infocrit = "bic" # could put it to "bic" or "hqc" or "aic"
ICfit = ic.glmnet(x=Xtrain,y=Ytrain,crit=infocrit,alpha=0)
coef(ICfit)

dfIC = as.matrix(coef(ICfit))
NonZeroCoeffsIC = dfIC[rowSums(dfIC[])!=0,]
NonZeroCoeffsICframe = as.data.frame(as.table(NonZeroCoeffsIC),responseName='Coefficients')
NonZeroCoeffsICframe
formattable(NonZeroCoeffsICframe)

sum(coef(ICfit) !=0) # count number of variables selected by Lasso.

### Put it nicely in Latex ##################################
rows <- seq_len(nrow(NonZeroCoeffsICframe) %/% 2)
kable(list(NonZeroCoeffsICframe[rows,1:2],
           matrix(numeric(), nrow=0, ncol=1),
           NonZeroCoeffsICframe[-rows, 1:2]),
      caption = "This is the caption.",
      label = "tables", format = "latex", booktabs = TRUE)
##################################################################

#Predict test set with lambda from IC
fitOnTrainIC = glmnet(Xtrain,Ytrain,alpha=0)

predIC= predict(fitOnTrainIC,s=ICfit$lambda,newx=Xtest) # pretty decent estimates

plot(1:13, Ytest, type = "l",ylab= expression(paste(Delta," Real GDP")),xlab="Test Period" )
lines(1:13, predIC, col = "blue",type="l",lty=2)

#RMSE
sqrt(sum((Ytest-predIC)^2))



#cv.outLasso <- cv.glmnet(x,y,alpha=1) # Lasso Cross validation to get lambda that minimizes error
#minLambdaLasso <- cv.outLasso$lambda.min
#Lasso.mod <- glmnet(x,y,alpha=1, lambda=minLambdaLasso) # apply Lasso with this lambda
#coef(Lasso.mod)




################## ELASTIC NET ############
alphagrid <- seq(0,1,length=10)
ElasticNetMatrix <- Matrix(NA,ncol=length(alphagrid),nrow=ncol(Xtrain)) # each column will represent the coefficients corresponding to a certain alpha
colnames(ElasticNetMatrix) = round(alphagrid,digits=2)
RMSEvec = rep(NA,times=length(alphagrid)) # make a vector of RMSE's for each alpha
PredictionsElastMatrix = Matrix(0,ncol=length(alphagrid),nrow=nrow(Xtest)) # Make matrix where predictions are put for each column alpha
colnames(PredictionsElastMatrix) = round(alphagrid,digits=2)
BestLambdavec = rep(NA,times=length(alphagrid))
# First iteration will compute ridge regression coefficients, the iterations inbetween the elastic net for alpha between 0 and 1
# The last iteration will compute lasso regression coefficients
indexcounter = 0
for(i in alphagrid){
  indexcounter = indexcounter + 1

  cv.out2 <- cv.glmnet(Xtrain, Ytrain, alpha = i)
  out2 <- glmnet(Xtrain, Ytrain, alpha = i,lambda = cv.out2$lambda.min) # Refit elastic net regression model on full data set

  bestlam2 <- cv.out2$lambda.min
  BestLambdavec[indexcounter] = bestlam2
  # after having gotten lambda through crossvalidation, we get coefficients
  elasticnet.pred <- coef(out2)

  predicElastic= predict(out2,s=bestlam2,newx=Xtest)
  PredictionsElastMatrix[,indexcounter] = predicElastic

  RMSEvec[indexcounter] = sqrt(sum((Ytest-predicElastic)^2))

  for(j in 1:ncol(Xtrain)){ # fill up for each alpha column, 20 of the regressor coefficient rows
    ElasticNetMatrix[j,indexcounter] = elasticnet.pred[j]
  }
  rownames(ElasticNetMatrix) = names(elasticnet.pred)

}

# first column (alpha 0) indeed corresponds to ridge coefficients and last column (alpha=1) indeed corresponds to lasso coefficients
ElasticNetMatrix
RMSEvec
PredictionsElastMatrix
# Pick alpha with lowest RMSE
LowestIndex = which.min(RMSEvec)
BestLambdavec[LowestIndex]

BestElastNetPredictions = PredictionsElastMatrix[,LowestIndex]
LowestRMSElastic = sqrt(sum((Ytest-BestElastNetPredictions)^2))
LowestRMSElastic

#Plot elastic net predictions vs actual values
plot(1:13, Ytest, type = "l",ylab= expression(paste(Delta," Real GDP")),xlab="Test Period" )
lines(1:13, BestElastNetPredictions, col = "blue",type="l",lty=2)

CoeffsElastNet = ElasticNetMatrix[,LowestIndex]
CoeffsElastNet = data.frame(CoeffsElastNet)
colnames(CoeffsElastNet) = "Estimates"
ResultsElast = data.frame(colnames(Xtrain),CoeffsElastNet)
ResultsElast

rows <- seq_len(nrow(ResultsElast) %/% 2)
kable(list(ResultsElast[rows,1:2],
           matrix(numeric(), nrow=0, ncol=1),
           ResultsElast[-rows, 1:2]),
      caption = "This is the caption.",
      label = "tables", format = "latex", booktabs = TRUE)


################# Factor Models ###############################

newX = subset(NewBelgiumTseries, select = -c(emp))

ic <- ICfactors(newX,rmax=50) # somehow IC selects as many (or 1 less) factors as variables, which we dont want since we want to reduce dimensionality
ic
ic$r_star

# calculate principal components using prcomp with selected number of factors
pca <- prcomp(newX, rank = ic$r_star)

#Extract the factor estimates
factors <- as.data.frame(pca$x)
Factorregressors <- create_lags(factors,1+0:3,include_original = TRUE) # actually use IC to determine the number of lags

ynew<-y[-1:-4]
lm(ynew~Factorregressors)


############## FAVAR ###################
Yfavar = BelgiumdataNew$rgdpe
Xfavar = subset(BelgiumdataNew,select= -c(rgdpe))

fit <- FAVAR(Y=Yfavar,X=Xfavar,K=5,nburn=50,nrep=150,plag=2) # if nburn and nrep are set to standard numbers, it takes waaaayy too long


#Estimates
coef(fit)
fit$factorx
fit$varrlt


#Impulse response
library(patchwork)
dt_irf = FAVAR::irf(fit,resvar=c(1:10)) # have to use FAVAR:: because otherwise it uses irf from vars package
dt_irf2 = FAVAR::irf(fit,resvar=c(11:20))
dt_irf3 = FAVAR::irf(fit,resvar=c(21:30))
dt_irf4 = FAVAR::irf(fit,resvar=c(31:40))


# FAVAR with new observed factors Y ##################
YFavarNew = subset(BelgiumdataNew,select=c(rgdpe,emp,cda,csh_c,csh_g))
XfavarNew = subset(BelgiumdataNew,select= -c(rgdpe,emp,cda,csh_c,csh_g))

# Calculate optimal number of factors
icfac <- ICfactors(XfavarNew,rmax=50)

## calculate optimal number of laggs of the FAVAR ###

#In order to get number of laggs, first get estimated factors through PC
# and then merge them with the Y's to get the VAR
PC <- prcomp(XfavarNew, rank = 5)
factors <- PC$x

# append factors to Y to get the whole VAR setup
Yintomatrix = do.call(cbind, YFavarNew)
YandFactors = cbind(Yintomatrix,factors)

#Select optimal # of laggs according to AIC
NrOfLags = VARselect(YandFactors,lag.max=15)$selection[[1]]

# Fit the FAVAR model, using K=5 factors and p=2 laggs
fitNew <- FAVAR(Y=BelgiumdataNew[,c("rgdpe","emp","cda","csh_c","csh_g")],
                X=XfavarNew,K=5,nburn=50,nrep=150,plag=2)

# Get coefficients of the FAVAR
summary(fitNew,xvar=c(3,5))

dt_irfN = FAVAR::irf(fitNew,resvar=c(1:10),impvar= c(2)) # for impvar the first columns belong to Y and the last columns to the factors K
dt_irf2N = FAVAR::irf(fitNew,resvar=c(11:20),impvar= c(2))
dt_irf3N = FAVAR::irf(fitNew,resvar=c(21:30),impvar= c(2))
dt_irf4N = FAVAR::irf(fitNew,resvar=c(31:36),impvar= c(2))


####### Make predictions using FAVAR ############

XtrainFAVAR = as.matrix(BelgiumdataNew[1:window_size, -c(1,4,8,29,31)])
YtrainFAVAR = as.matrix(BelgiumdataNew[1:window_size, c(1,4,8,29,31)])

XtestFAVAR= as.matrix(BelgiumdataNew[(window_size+1):nrow(BelgiumdataNew), -c(1,4,8,29,31)])
YtestFAVAR= as.matrix(BelgiumdataNew[(window_size+1):nrow(BelgiumdataNew), c(1,4,8,29,31)])


fitTrain <- FAVAR(Y=YtrainFAVAR[,c("rgdpe","emp","cda","csh_c","csh_g")],
                  X=XtrainFAVAR,K=5,nburn=50,nrep=150,plag=2)


EquationRGDPE = coef(fitTrain)$varcoef[c("rgdpe"),]
EquationRGDPE

# DOESNT WORK
predict(EquationRGDPE)



summary(fitTrain)

########## example from package ############
data('regdata')
fit <- FAVAR(Y = regdata[,c("Inflation","Unemployment","Fed_funds")],
             X = regdata[,1:115], slowcode = slowcode,fctmethod = 'BBE',
             factorprior = list(b0 = 0, vb0 = NULL, c0 = 0.01, d0 = 0.01),
             varprior = list(b0 = 0,vb0 = 10, nu0 = 0, s0 = 0),
             nrep = 150, nburn = 50, K = 2, plag = 2)
#---- print FAVAR estimation results------
summary(fit,xvar = c(3,5))
#---- or extract coefficients------
coef(fit)
#---- plot impulse response figure------
library(patchwork)
dt_irf <- FAVAR::irf(fit,resvar = c(2,9,10)) # The column indices indicate the response variables through a one unit change in which impulse variable(s)?

################### High Dimensional Inference ##################################
#################### Naive inference #########################################


betas = BestFit$beta #look at coefficients from best fit of lasso

PositiveCoefs = c()
PositiveIndices = c()
for(i in 1:ncol(x)){
  if(!betas[i] == 0){ # checks which coefficients of lasso are nonzero
    PositiveCoefs = append(PositiveCoefs,betas[i])
    PositiveIndices = append(PositiveIndices,i)
  }
}

PositiveIndices
PositiveIndicesWithSmallConfInt = PositiveIndices[-c(11,12,8,9,10,2)]
PositiveIndicesWithSmallConfInt
Xtesttest = x[,PositiveIndicesWithSmallConfInt]
Xtesttest
#Apply OLS to only the relevant variables
XnewAfterSelection= x[,PositiveIndices]

fitPostLas = lm(y~XnewAfterSelection)
coef(fitPostLas)
ci_cvmin= confint(fitPostLas)[-1,] # not sure if we should look at confidence interval of intercept or not
ci_cvmin
# Get the hypothesis tests.
test_cvmin <- summary(fitPostLas)$coefficients[-1,] # not sure if we should look at confidence interval of intercept or not
print(test_cvmin, digits = 3)



# turn into plot
df_cvmin1 <- data.frame(names= factor(colnames(XnewAfterSelection)),
                        Estimate = test_cvmin[, 1], LowerBound = ci_cvmin[, 1],
                        UpperBound = ci_cvmin[, 2], pvalue = test_cvmin[, 4])
ggplot(df_cvmin1, aes(x = names, y = Estimate)) +
  geom_hline(yintercept = 0) +
  geom_crossbar(aes(ymin = LowerBound, ymax = UpperBound, fill = (Estimate > 0),
                    alpha = 1 - 5 * pvalue * (pvalue <= 0.1) -
                      (0.4 + pvalue) * (pvalue > 0.1) * (pvalue <= 0.5) - (pvalue > 0.5))) +
  labs(x = "Selected variables", y = "Estimated Effect") +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "none", axis.text.y = element_text(size = rel(0.8))) +
  coord_flip()+ ggtitle("Naive OLS after Lasso")

######## turn into plot with only small coefficient NOTE THAT I DONT CHANGE THE VARIABLE NAMES BECAUSE LAZYNESS
test_cvmin = summary(fitPostLas)$coefficients[-c(12,13,9,10,11,1,3),]
ci_cvmin = ci_cvmin[-c(11,12,8,9,10,2),]
df_cvmin1 <- data.frame(names= factor(colnames(Xtesttest)),
                        Estimate = test_cvmin[, 1], LowerBound = ci_cvmin[, 1],
                        UpperBound = ci_cvmin[, 2], pvalue = test_cvmin[, 4])
ggplot(df_cvmin1, aes(x = names, y = Estimate)) +
  geom_hline(yintercept = 0) +
  geom_crossbar(aes(ymin = LowerBound, ymax = UpperBound, fill = (Estimate > 0),
                    alpha = 1 - 5 * pvalue * (pvalue <= 0.1) -
                      (0.4 + pvalue) * (pvalue > 0.1) * (pvalue <= 0.5) - (pvalue > 0.5))) +
  labs(x = "Selected variables", y = "Estimated Effect") +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "none", axis.text.y = element_text(size = rel(0.8))) +
  coord_flip() + ggtitle("Naive OLS after Lasso")

############## Post double selection ##########

postdoublemult = rlassoEffects(x,y,index=c(PositiveIndices)) # index selects the treatment variables from x
test_cvmin2 = summary(postdoublemult)$coefficients
print(test_cvmin2, digits = 3)

ci_cvmin2 = confint(postdoublemult)
plot(postdoublemult) # plots coefficients of post double selection with confidence intervals

# turn into plot
df_cvmin2 <- data.frame(names= factor(colnames(XnewAfterSelection)),
                        Estimate = test_cvmin2[, 1], LowerBound = ci_cvmin2[, 1],
                        UpperBound = ci_cvmin2[, 2], pvalue = test_cvmin2[, 4])
ggplot(df_cvmin2, aes(x = names, y = Estimate)) +
  geom_hline(yintercept = 0) +
  geom_crossbar(aes(ymin = LowerBound, ymax = UpperBound, fill = (Estimate > 0),
                    alpha = 1 - 5 * pvalue * (pvalue <= 0.1) -
                      (0.4 + pvalue) * (pvalue > 0.1) * (pvalue <= 0.5) - (pvalue > 0.5))) +
  labs(x = "Selected variables", y = "Estimated Effect") +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "none", axis.text.y = element_text(size = rel(0.8))) +
  coord_flip() + ggtitle("Post Double Selection")


# turn into plot with only small coefficients
test_cvmin2 = summary(postdoublemult)$coefficients[-c(11,12,8,9,10,2),]
ci_cvmin2 = ci_cvmin2[-c(11,12,8,9,10,2),]

df_cvmin2 <- data.frame(names= factor(colnames(Xtesttest)),
                        Estimate = test_cvmin2[, 1], LowerBound = ci_cvmin2[, 1],
                        UpperBound = ci_cvmin2[, 2], pvalue = test_cvmin2[, 4])
ggplot(df_cvmin2, aes(x = names, y = Estimate)) +
  geom_hline(yintercept = 0) +
  geom_crossbar(aes(ymin = LowerBound, ymax = UpperBound, fill = (Estimate > 0),
                    alpha = 1 - 5 * pvalue * (pvalue <= 0.1) -
                      (0.4 + pvalue) * (pvalue > 0.1) * (pvalue <= 0.5) - (pvalue > 0.5))) +
  labs(x = "Selected variables", y = "Estimated Effect") +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "none", axis.text.y = element_text(size = rel(0.8))) +
  coord_flip() + ggtitle("Post Double Selection")


################################# PoSI ################################

pos = PoSI(X=XnewAfterSelection,modelSZ=1:ncol(XnewAfterSelection))# I changed modelSZ default to 1:5 because otherwise takes too long

summary(pos) # now use K to calculate confidence intervals [b-K*SE(b),b+K*SE(b)]
#################################################################################

summary(BestFit)

pos


########################## Selective Inference #################

BestLambda = BestFit$lambda
n = nrow(x) # nr observations
randomfit = glmnet(Xtrain,Ytrain,standardize=FALSE,alpha=1,lambda=BestLambda) # but this selects different variables than usual Lasso
s = BestLambda / n
# need to adjust according to description of package because glmnet uses different lasso objective
betaAdjusted = coef(randomfit,x=x,y=y, s=s)[-1]
# BELOW TAKES A VERY LONG TIME (like 5minutes)
res = fixedLassoInf(x,y,betaAdjusted,lambda=BestLambda) # inference with the lasso for a fixed lambda
res
#################################################################################
res$coef0



############################ Despars lasso ##############################
fitdesp = boot.lasso.proj(x,y,B=10,return.bootdist=TRUE) # return bootdist is necessary to construct confidence intervals afterwards
fitdesp
ci_cvmin3 =  confint(fitdesp,level=0.95)[PositiveIndices,]
ci_cvmin3
plot(fitdesp$betahat)

coeffs = fitdesp$bhat[PositiveIndices]
coeffs
Pvals = fitdesp$pval[PositiveIndices]


# Turn into plot

df_cvmin3 <- data.frame(names= factor(colnames(XnewAfterSelection)),
                        Estimate = coeffs, LowerBound = ci_cvmin3[, 1],
                        UpperBound = ci_cvmin3[, 2], pvalue = Pvals)
ggplot(df_cvmin3, aes(x = names, y = Estimate)) +
  geom_hline(yintercept = 0) +
  geom_crossbar(aes(ymin = LowerBound, ymax = UpperBound, fill = (Estimate > 0),
                    alpha = 1 - 5 * pvalue * (pvalue <= 0.1) -
                      (0.4 + pvalue) * (pvalue > 0.1) * (pvalue <= 0.5) - (pvalue > 0.5))) +
  labs(x = "Selected variables", y = "Estimated Effect") +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "none", axis.text.y = element_text(size = rel(0.8))) +
  coord_flip()+ ggtitle("Desparsified Lasso")

namesplot = rownames(ci_cvmin3)
plot(ci_cvmin3)
text(x=ci_cvmin3[,1],y=ci_cvmin3[,2],labels=namesplot,col='Blue')

# Turn into plot with only small coefficients
ci_cvmin3 =  confint(fitdesp,level=0.95)[PositiveIndicesWithSmallConfInt,]
ci_cvmin3
coeffs = fitdesp$bhat[PositiveIndices]
coeffs = fitdesp$bhat[PositiveIndicesWithSmallConfInt]

df_cvmin3 <- data.frame(names= factor(colnames(Xtesttest)),
                        Estimate = coeffs, LowerBound = ci_cvmin3[, 1],
                        UpperBound = ci_cvmin3[, 2], pvalue = Pvals)
ggplot(df_cvmin3, aes(x = names, y = Estimate)) +
  geom_hline(yintercept = 0) +
  geom_crossbar(aes(ymin = LowerBound, ymax = UpperBound, fill = (Estimate > 0),
                    alpha = 1 - 5 * pvalue * (pvalue <= 0.1) -
                      (0.4 + pvalue) * (pvalue > 0.1) * (pvalue <= 0.5) - (pvalue > 0.5))) +
  labs(x = "Selected variables", y = "Estimated Effect") +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "none", axis.text.y = element_text(size = rel(0.8))) +
  coord_flip()+ ggtitle("Desparsified Lasso")

namesplot = rownames(ci_cvmin3)
plot(ci_cvmin3)
