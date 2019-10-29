###SECTION 1
#loading data
setwd('/Users/frankzhu/downloads')
train <- read.csv('train.csv')
test <- read.csv('test.csv')

#checking out data and installing and loading packages
str(train)
dim(train)
dim(test)

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)

###SECTION 1.1
#Getting rid of the IDs but keeping the test IDs in a vector. These are needed to compose the submission file
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train, test)
dim(all) 
all$MiscFeature

###SECTION 1.2

#explore response variable

#extract all non-missing rows of SalePrice
ggplot(data = all[!is.na(all$SalePrice),], aes(x = SalePrice)) + geom_histogram(fill = "blue", binwidth = 10000) + scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#scale_x_continous creates the scale for the x axis

#we see that the data is right skewed which makes sense for house prices

###SECTION 2

#first we look at numerical variables
numVars <- which(sapply(all, is.numeric))
numVarsNames <- names(numVar)
cat('There are', length(numVars), 'numeric variables')

#now we find variables that are highly correlated with SalePrice
all.numVars <- all[,numVars]
corr.numVars <- cor(all.numVars, use = "pairwise.complete.obs") #correlation of all numeric variables
corr.sort.numVars <- as.matrix(sort(corr.numVars[,'SalePrice'], decreasing = T)) #sort correlations to SalePrice

#selecting all the highly correlated variables
corr.high <- names(which(apply(corr.sort.numVars, 1, function(x) abs(x) > 0.5 )))
corr.numVars <- corr.numVars[corr.high,corr.high] #so that both rows and columns are of the same variables in the same order
corrplot.mixed(corr.numVars, tl.col = 'black',tl.pos = "lt") #correlation plot


###SECTION 3

#now we are going to look at the best correlated variables
#OverallQual and GrLivArea
attach(all)
summary(OverallQual) #integer with scale of 1-10

#visualize the variable
#since its kinda categorical we'll use boxplots
ggplot(data = all[!is.na(SalePrice), ], aes(x = factor(OverallQual), y = SalePrice)) + 
  geom_boxplot() + 
  labs(x = 'Overall Quality', y = 'Sale Price') + 
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#the data is postively exponentially distributed with few worrying outliers except maybe the outlier in column 4

#now look at GrLivArea
summary(GrLivArea)
str(GrLivArea)
class(GrLivArea)
all[is.na(SalePrice),] #select all the rows without a missing saleprice

ggplot(data = all[!is.na(SalePrice),], aes(x = GrLivArea, y = SalePrice)) + 
      geom_point(col = "blue") +
      geom_smooth(method = "lm", se = F, color = "black", aes(group =1)) +
      geom_text_repel(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)] >4500, rownames(all), "")) +
      scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)+
      labs(x = "Gr Living Area", y = "Sale Price")

#geom_text_repel labels a plot with the label not being overlapped with the point
#geom_text does the same but overlapps the point
#if u use aes in the argument it makes the labelling uniform
#the argument is label the point if the GrLivArea of those with non-missing Sale Price with 
#the corresponding row number of the point or else label it blank

#points 524 and 1299 are clear outliers so lets take a look at them
all[c(524,1299),c('SalePrice','OverallQual', 'GrLivArea')]
#it's weird because they have high quality and GrLivArea and low Saleprices so they must be outliers


