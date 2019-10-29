###SECTION 1
#finding the columns with missing values
NACol <- which(colSums(is.na(all)) > 0)
NACol <- sort(colSums(sapply(all[NACol],is.na)), decreasing = T)

#dealing with missingness by variable group

### 1.1 Pool Variables
# PoolQC, PoolArea

#NA means no pool so let's give the factor a level for no pool that replaces NA
PoolQC <- factor(ifelse(is.na(all$PoolQC), 'None', paste(all$PoolQC)), 
                 levels = c(levels(all$PoolQC), 'None'))

Qualities <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

all$PoolQC<- as.integer(revalue(PoolQC, Qualities))
#revaluing PoolQC into an ordinal factor variable

table(PoolQC)
table(PoolArea)

all[all$PoolQC == 4 & all$PoolArea != 0, c('OverallQual','PoolQC', 'PoolArea')]
#checking why there are 2909 '4's in PoolQC but 2906 '0's in PoolArea as they both represent no pool

ggplot(data = all, aes(x = PoolArea, y = PoolQC)) + geom_point(col = "blue") 
#There is no relation between PoolQC and PoolArea so we have to find a subsitute for these 3 points
#Lets use OverallQual because it's a measure of quality

impute <- rownames(all[all$PoolQC == 4 & all$PoolArea != 0, c('OverallQual','PoolQC', 'PoolArea')])

#since OverallQual of these houses are in the range of 3-6 I will just assign 'fair' or 2 for these houses
all$PoolQC[impute] <- 2

### 1.2 Miscellaneous Features
class(MiscFeature)

ggplot(all[!is.na(SalePrice), ], aes(x = MiscFeature, y = SalePrice)) + 
  geom_bar(stat = "summary", fun.y = "median", fill= "blue")
#the variable is not ordinal

#NA means none so we will replace NA with 'None'
MiscFeature <-  factor(ifelse(is.na(all$MiscFeature), 'None', paste(all$MiscFeature)), 
                       levels = c(levels(all$MiscFeature), 'None'))
all$MiscFeature <- MiscFeature
table(MiscFeature)


### 1.3 Alley Variable
class(Alley)
levels(Alley)

Alley <- factor(ifelse(is.na(all$Alley), 'None', paste(all$Alley)), levels = c(levels(Alley), 'None'))
 
all$Alley <- Alley
ggplot(all[!is.na(all$SalePrice),], aes(x = Alley, y = SalePrice)) + 
  geom_bar(stat = "summary", fun.y = "median", fill = "blue")
#there seems to be no ordinal relation for this variable so we keep it a nominal factor variable

table(Alley)


### 1.4 Fireplace Variables
summary(FireplaceQu)
class(FireplaceQu)
summary(Fireplaces)
class(Fireplaces)                
# the NAs for FireplaceQu represent no fireplaces so the # of NAs in FireplaceQu should
# be the same as the # of zeros in Fireplace

table(FireplaceQu)
table(Fireplaces)
# the NA's in FireplaceQu match the zeros in Fireplaces, so now we just have to convert NA's in
# FireplaceQu to a level 'None'

FireplaceQu <- factor(ifelse(is.na(all$FireplaceQu), 'None', paste(all$FireplaceQu)), levels = c(levels(FireplaceQu), 'None'))
table(FireplaceQu)
all$FireplaceQu <- FireplaceQu

### 1.5 Fence variable

summary(Fence)

# NA is no fence so replace with level 'None'

Fence <- factor(ifelse(is.na(all$Fence), 'None', paste(all$Fence)), levels= c(levels(all$Fence), 'None'))
table(Fence)

all$Fence <- Fence


### 1.6 Lot Variables
str(LotConfig)
str(LotFrontage)
str(LotShape)

all[is.na(LotFrontage), c("LotFrontage", "LotShape", "LotArea", "LotConfig" )]

ggplot(all[!is.na(SalePrice), ], aes(x= Neighborhood, y = LotFrontage)) + 
  geom_bar(stat = "summary", fun.y = "median", fill = "blue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#replace LotFrontage NAs with median LotFrontage in each neighborhood
 for( i in 1:nrow(all)){
  if(is.na(LotFrontage[i])){
    LotFrontage[i] <- as.integer(median(LotFrontage[Neighborhood == Neighborhood[i]], na.rm = T))
  }
 }
all$LotFrontage <- as.numeric(LotFrontage)
#median(LotFrontage[neighborhood == neighborhood[i]]) means
#take the median lotfrontage from each neighborhood level


ggplot(all[!is.na(SalePrice),], aes(x = LotShape, y = SalePrice)) +
  geom_bar(stat= "summary", fun.y = "median", fill = "blue")

ggplot(all[!is.na(SalePrice),], aes(x = LotConfig, y = SalePrice)) +
  geom_bar(stat= "summary", fun.y = "median", fill = "blue")

# These variables are not ordinal so keep them as factors

### 1.7 Garage Variables
NACol
str(GarageCond) #NA means no garage
str(GarageFinish) #NA means no garage
levels(GarageType) #NA means no garage
levels(GarageQual) #NA means no garage
str(GarageYrBlt) #integer
str(GarageArea)
str(GarageCars)

kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')]) 

#imputing modes
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

#imputing with yrbuilt
all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

table(GarageYrBlt)
#imputing as no garage
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

#changing to ordinal variables
all$GarageCond <- factor(ifelse(is.na(all$GarageCond), 'None', paste(all$GarageCond)), 
                     levels = c(levels(all$GarageCond), 'None'))
all$GarageCond<-as.integer(revalue(GarageCond, Qualities))


all$GarageFinish <- factor(ifelse(is.na(all$GarageFinish), 'None', paste(all$GarageFinish)), 
                       levels = c(levels(all$GarageFinish), 'None'))
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)
all$GarageFinish <- as.integer(revalue(GarageFinish, Finish))


all$GarageQual <- factor(ifelse(is.na(all$GarageQual), 'None', paste(all$GarageQual)), 
                     levels = c(levels(all$GarageQual), 'None'))
all$GarageQual<- as.integer(revalue(GarageQual,Qualities))


all$GarageType <- factor(ifelse(is.na(all$GarageType), 'None', paste(all$GarageType)), 
                     levels = c(levels(all$GarageType), 'None'))
all$GarageType <- as.factor(all$GarageType)


###1.8 Basement Variables
BsmtCond #NA means no basement
BsmtExposure #NA means no basement
BsmtQual #NA means no basement
BsmtFinType1 #NA means no basement
BsmtFinType2 #NA means no basement
BsmtFinSF1
BsmtFinSF2
BsmtUnfSF
TotalBsmtSF

#finding all the additional NA's from BsmtFinType1's 79 NA's
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)|is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)), 
    c('BsmtQual', 'BsmtCond', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]

#imputing modes
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

#replacing NA's with 'None' and making variables ordinal
all$BsmtQual <- factor(ifelse(is.na(all$BsmtQual), 'None', paste(all$BsmtQual)), 
                     levels = c(levels(all$BsmtQual), 'None'))
all$BsmtQual<-as.integer(revalue(BsmtQual, Qualities))



all$BsmtCond <- factor(ifelse(is.na(all$BsmtCond), 'None', paste(all$BsmtCond)), 
                   levels = c(levels(all$BsmtCond), 'None'))
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))


all$BsmtExposure <- factor(ifelse(is.na(all$BsmtExposure), 'None', paste(all$BsmtExposure)), 
                   levels = c(levels(all$BsmtExposure), 'None'))
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))


all$BsmtFinType1 <- factor(ifelse(is.na(all$BsmtFinType1), 'None', paste(all$BsmtFinType1)), 
                       levels = c(levels(all$BsmtFinType1), 'None'))
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
all$BsmtFinType1 <- as.integer(revalue(all$BsmtFinType1, FinType))


all$BsmtFinType2 <- factor(ifelse(is.na(all$BsmtFinType2), 'None', paste(all$BsmtFinType2)), 
                       levels = c(levels(all$BsmtFinType2), 'None'))
all$BsmtFinType2 <- as.integer(revalue(all$BsmtFinType2, FinType))

#display remaining NAs. Using BsmtQual as a reference for the 79 houses without basement agreed upon earlier
all[(BsmtQual & is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|
       is.na(all$BsmtFinSF2)|is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)),
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2',
      'BsmtUnfSF', 'TotalBsmtSF')]

all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0
all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0
all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0
all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0


### 1.9 MasVnrType variables

#finding the extra NA in MasVnrType
all[!is.na(all$MasVnrArea) & is.na(all$MasVnrType), c('MasVnrArea', 'MasVnrType')]

ggplot(all[!is.na(SalePrice),], aes(x = MasVnrType, y = MasVnrArea)) +
  geom_bar(stat= "summary", fun.y = "median", fill = "blue")

#imputing with the mode level that isn't None
all$MasVnrType[2611] <- names(table(all$MasVnrType)[2]) 

#imputing all NA's in type with 'None'
all$MasVnrType <- factor(ifelse(is.na(all$MasVnrType), 'None', paste(all$MasVnrType)))

#looking at the ordinality of MasVnrType
ggplot(all[!is.na(SalePrice),], aes(x = MasVnrType, y = SalePrice)) +
  geom_bar(stat= "summary", fun.y = "median", fill = "blue")
#seems ordinal with BrkCmn and None having the same median SalePrice so we will make them both
#1 level
Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))

all$MasVnrArea[is.na(all$MasVnrArea)] <- 0 
all$MasVnrArea <- as.factor(all$MasVnrArea)
sum(is.na(all$MasVnrArea))

### 2.0 MS Zoning
ggplot(all[!is.na(SalePrice),], aes(x = MSZoning, y = SalePrice)) +
  geom_bar(stat= "summary", fun.y = "median", fill = "blue")

#imputing mode seems acceptable here since theres only 4 NAs
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
sum(is.na(all$MSZoning))
### 2.1 Utilities

table(Utilities)
#the variable has no predictive value so we will delete it
all$Utilities <- NULL

### 2.2 Functionality
#impute mode for the 1 NA
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]

all$Functional <- as.integer(revalue(all$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(Functional)

### 2.3 Exterior Variables

all[is.na(Exterior1st)|is.na(Exterior2nd), c('ExterCond','Exterior2nd','ExterQual','Exterior1st')]

all$Exterior1st[2152] <- names(sort(-table(Exterior1st)[1]))
all$Exterior2nd[2152] <- names(sort(-table(Exterior2nd)[1]))

#visualizing for ordinality and they are both ordinal
ggplot(all[!is.na(SalePrice),], aes(x = ExterCond, y = SalePrice)) +
  geom_bar(stat= "summary", fun.y = "median", fill = "blue")
ggplot(all[!is.na(SalePrice),], aes(x = ExterQual, y = SalePrice)) +
  geom_bar(stat= "summary", fun.y = "median", fill = "blue")

all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))
all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))


### 2.4 Electrical
which(is.na(all$Electrical) == T)

#imputing with mode as only 1 NA
all$Electrical[1380] <- names(sort(-table(all$Electrical)[1]))
all$Electrical <- as.factor(all$Electrical)
table(Electrical)

### 2.5 SaleType and Condition
table(SaleType)
which(is.na(SaleType) == T)
all$SaleType[2490] <- names(sort(-table(all$SaleType)[1]))
all$SaleType <- as.factor(all$SaleType)

table(SaleCondition)
ggplot(all[!is.na(SalePrice),], aes(x= SaleCondition, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = "median", fill = "blue")
#not ordinal

### 2.6 Kitchen Variables

#imputing with mode and turning ordinal
which(is.na(all$KitchenQual) == T)
all$KitchenQual[1556] <- names(sort(-table(all$KitchenQual)[1]))
all$KitchenQual <- as.factor(all$KitchenQual)
all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)

ggplot(all[!is.na(SalePrice),], aes(x= KitchenQual, y = SalePrice)) +
  geom_bar(stat = 'summary', fun.y = "median", fill = "blue")

table(KitchenQual)
table(all$KitchenQual)
### 2.7 checking if all missing value columns are fixed
NACol1 <- which(colSums(is.na(all)) > 0)
NACol1
#all columns are non-missing except for SalePrice so done




