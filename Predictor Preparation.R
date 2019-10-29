#Preparing data for modeling


dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')
all <- all[,!(names(all) %in% dropVars)]


##Removing outliers
all <- all[-c(524, 1299),]


##PreProcessing predictor variables

#Before modeling I need to center and scale the 'true numeric' predictors
#(so not variables that have been label encoded), 
#and create dummy variables for the categorical predictors.
#Below, I am splitting the dataframe into one with all (true) numeric variables,
#and another dataframe holding the (ordinal) factors.

numVars1 <- numVars1[!(numVars1 %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numVars1 <- append(numVars1, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numVars1]

DFfactors <- all[, !(names(all) %in% numVars1)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

###Skewness and normalizing of the numeric predictors

for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

###One hot encoding the categorical variables
DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)
class(DFdummies)

#check if some values are absent in the test set
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] #removing predictors

#check if some values are absent in the train set
ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain]


#Also taking out variables with less than 10 'ones' in the train set.

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

#combining all (now numeric) predictors into one dataframe 
combined <- cbind(DFnorm, DFdummies)

###
#Dealing with skewness of response variable

skew(all$SalePrice)
all$SalePrice <- log(all$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$SalePrice)

### making the train and test sets
train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]

