library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(MASS)
library(dplyr)
library(lars)
library(moments)
library(caret)
library(corrplot)
library(gbm)

# Read the data
hp_train<-read.csv("E:/Home price prediction kaggle/train.csv")
hp_test<-read.csv("E:/Home price prediction kaggle/test.csv")

# Checking % of missing value 
round(colSums(is.na(hp_train))/nrow(hp_train),4)*100

Missing_indices <- sapply(hp_train,function(x)sum(is.na(x)))
Missing_Summary <- data.frame(index = names(hp_train),Missing_Values=Missing_indices)
Missing_Summary[Missing_Summary$Missing_Values > 0,]

# combining train and test data
full<-bind_rows(hp_train,hp_test)

# Imputing missing value of Lot Frontage by the median
full$LotFrontage[which(is.na(full$LotFrontage))]<-median(full$LotFrontage,na.rm = T)

# putting No_ALLey in place of missing value of alley
class(full$Alley)
full$Alley<-as.character(full$Alley)
class(full$Alley)
full$Alley[which(is.na(full$Alley))] <- "No_Allay"
table(full$Alley)
full$Alley<-as.factor(full$Alley)

# putting  none in place of missing value of MasVnrType
class(full$MasVnrType)
full$MasVnrType <- as.character(full$MasVnrType)
class(full$MasVnrType)
full$MasVnrType[which(is.na(full$MasVnrType))]<-"None"
table(full$MasVnrType)
full$MasVnrType<-as.factor(full$MasVnrType)

# imputting MasVnrArea by mean  
full$MasVnrArea[which(is.na(full$MasVnrArea))] <- mean(full$MasVnrArea,na.rm=T)

# putting  nb in place of missing value of BsmtQual
class(full$BsmtQual)
full$BsmtQual <- as.character(full$BsmtQual)
class(full$BsmtQual)
full$BsmtQual[which(is.na(full$BsmtQual))]<-"nb"
table(full$BsmtQual)
full$BsmtQual<-as.factor(full$BsmtQual)

# putting  nb in place of missing value of BsmtCond
class(full$BsmtCond)
full$BsmtCond <- as.character(full$BsmtCond)
class(full$BsmtCond)
full$BsmtCond[which(is.na(full$BsmtCond))]<-"nb"
table(full$BsmtCond)
full$BsmtCond<-as.factor(full$BsmtCond)


# putting  nb in place of missing value of BsmtExposure
class(full$BsmtExposure)
full$BsmtExposure <- as.character(full$BsmtExposure)
class(full$BsmtExposure)
full$BsmtExposure[which(is.na(full$BsmtExposure))]<-"nb"
table(full$BsmtExposure)
full$BsmtExposure<-as.factor(full$BsmtExposure)

# putting  nb in place of missing value of BsmtFinSF1
class(full$BsmtFinType1)
full$BsmtFinType1 <- as.character(full$BsmtFinType1)
class(full$BsmtFinType1)
full$BsmtFinType1[which(is.na(full$BsmtFinType1))]<-"nb"
table(full$BsmtFinType1)
full$BsmtFinType1<-as.factor(full$BsmtFinType1)

# putting  nb in place of missing value of BsmtFinSF2
class(full$BsmtFinType2)
full$BsmtFinType2 <- as.character(full$BsmtFinType2)
class(full$BsmtFinType2)
full$BsmtFinType2[which(is.na(full$BsmtFinType2))]<-"nb"
table(full$BsmtFinType2)
full$BsmtFinType2<-as.factor(full$BsmtFinType2)

# putting  highest frequency in place of missing value and 'mix' of Electrical
table(full$Electrical)
class(full$Electrical)
full$Electrical <- as.character(full$Electrical)
class(full$Electrical)
full$Electrical[which(is.na(full$Electrical))]<-"SBrkr"
full$Electrical[which(full$Electrical=='Mix')]<-"SBrkr"
table(full$Electrical)
full$Electrical<-as.factor(full$Electrical)

# putting  NFP frequency in place of missing value FireplaceQu
table(full$FireplaceQu)
class(full$FireplaceQu)
full$FireplaceQu <- as.character(full$FireplaceQu)
class(full$FireplaceQu)
full$FireplaceQu[which(is.na(full$FireplaceQu))]<-"NEP"
table(full$FireplaceQu)
full$FireplaceQu<-as.factor(full$FireplaceQu)

# putting  NG for GarageType in case of no garrage
table(full$GarageType)
class(full$GarageType)
full$GarageType <- as.character(full$GarageType)
class(full$GarageType)
full$GarageType[which(is.na(full$GarageType))]<-"NG"
table(full$GarageType)
full$GarageType<-as.factor(full$GarageType)

# Changing NA in GarageYrBlt to 0
full$GarageYrBlt[which(is.na(full$GarageYrBlt))] <- 0 

# putting  NG for GarageFinish in case of no garrage
table(full$GarageFinish)
class(full$GarageFinish)
full$GarageFinish <- as.character(full$GarageFinish)
class(full$GarageFinish)
full$GarageFinish[which(is.na(full$GarageFinish))]<-"NG"
table(full$GarageFinish)
full$GarageFinish<-as.factor(full$GarageFinish)

# putting  NG for GarageQual in case of no garrage
table(full$GarageQual)
class(full$GarageQual)
full$GarageQual <- as.character(full$GarageQual)
class(full$GarageQual)
full$GarageQual[which(is.na(full$GarageQual))]<-"NG"
table(full$GarageQual)
full$GarageQual<-as.factor(full$GarageQual)

# putting  NG for GarageCond in case of no garrage
table(full$GarageCond)
class(full$GarageCond)
full$GarageCond <- as.character(full$GarageCond)
class(full$GarageCond)
full$GarageCond[which(is.na(full$GarageCond))]<-"NG"
table(full$GarageCond)
full$GarageCond<-as.factor(full$GarageCond)

# putting  NP for PoolQC in case of no garrage
table(full$PoolQC)
class(full$PoolQC)
full$PoolQC <- as.character(full$PoolQC)
class(full$PoolQC)
full$PoolQC[which(is.na(full$PoolQC))]<-"NP"
table(full$PoolQC)
full$PoolQC<-as.factor(full$PoolQC)

# putting  NF for Fence in case of no Fence
table(full$Fence)
class(full$Fence)
full$Fence <- as.character(full$Fence)
class(full$Fence)
full$Fence[which(is.na(full$Fence))]<-"NF"
table(full$Fence)
full$Fence<-as.factor(full$Fence)

# putting  NMF for MiscFeature in case of no gFence
table(full$MiscFeature)
class(full$MiscFeature)
full$MiscFeature <- as.character(full$MiscFeature)
class(full$MiscFeature)
full$MiscFeature[which(is.na(full$MiscFeature))]<-"NMF"
table(full$MiscFeature)
full$MiscFeature<-as.factor(full$MiscFeature)

# putting  RL for MiscFeature in case of missing value as it is most frequently occuring
table(full$MSZoning)
class(full$MSZoning)
full$MSZoning <- as.character(full$MSZoning)
class(full$MSZoning)
full$MSZoning[which(is.na(full$MSZoning))]<-"RL"
table(full$MSZoning)
full$MSZoning<-as.factor(full$MSZoning)

# putting  AllPub for Utilities in case of missing value as it is most frequently occuring
table(full$Utilities)
class(full$Utilities)
full$Utilities <- as.character(full$Utilities)
class(full$Utilities)
full$Utilities[which(is.na(full$Utilities))]<-"AllPub"
table(full$Utilities)
full$Utilities<-as.factor(full$Utilities)

# putting  VinylSd for Exterior1st in case of missing value as it is most frequently occuring
table(full$Exterior1st)
class(full$Exterior1st)
full$Exterior1st <- as.character(full$Exterior1st)
class(full$Exterior1st)
full$Exterior1st[which(is.na(full$Exterior1st))]<-"VinylSd"
table(full$Exterior1st)
full$Exterior1st<-as.factor(full$Exterior1st)

# putting  Wd Sdng	 for Exterior2nd in case of missing value as it is most frequently occuring
table(full$Exterior2nd)
class(full$Exterior2nd)
full$Exterior2nd <- as.character(full$Exterior2nd)
class(full$Exterior2nd)
full$Exterior2nd[which(is.na(full$Exterior2nd))]<-"Wd Sdng"
table(full$Exterior2nd)
full$Exterior2nd<-as.factor(full$Exterior2nd)

# putting  TA	 for KitchenQualin case of missing value as it is most frequently occuring
table(full$KitchenQual)
class(full$KitchenQual)
full$KitchenQual <- as.character(full$KitchenQual)
class(full$KitchenQual)
full$KitchenQual[which(is.na(full$KitchenQual))]<-"TA"
table(full$KitchenQual)
full$KitchenQual<-as.factor(full$KitchenQual)

# putting  Typ	 for Functional case of missing value as it is most frequently occuring
table(full$Functional)
class(full$Functional)
full$Functional <- as.character(full$Functional)
class(full$Functional)
full$Functional[which(is.na(full$Functional))]<-"Typ"
table(full$Functional)
full$Functional<-as.factor(full$Functional)

# putting  WD	 for SaleType case of missing value as it is most frequently occuring
table(full$SaleType)
class(full$SaleType)
full$SaleType <- as.character(full$SaleType)
class(full$SaleType)
full$SaleType[which(is.na(full$SaleType))]<-"WD"
table(full$SaleType)
full$SaleType<-as.factor(full$SaleType)
colSums(is.na(full))
#----------------------------------------------
# Replacing all na values in numeric variable with mode value  
full$BsmtFinSF1[which(is.na(full$BsmtFinSF1))] <- .75
full$BsmtFinSF2[which(is.na(full$BsmtFinSF2))] <- 0
full$TotalBsmtSF[which(is.na(full$TotalBsmtSF))] <- 0
full$BsmtUnfSF[which(is.na(full$BsmtUnfSF))] <- 0.5
full$BsmtFullBath[which(is.na(full$BsmtFullBath))] <- -1
full$BsmtHalfBath[which(is.na(full$BsmtHalfBath))] <- 0
full$GarageCars[which(is.na(full$GarageCars))] <- 0
full$GarageArea[which(is.na(full$GarageArea))] <- 0


# num<-(c("MSSubClass","LotFrontage","LotArea","OverallQual","OverallCond",
#        "YearBuilt","YearRemodAdd","MasVnrArea","BsmtFinSF1","BsmtFinSF2",
#        "BsmtUnfSF","TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF",
#        "GrLivArea","BsmtFullBath","BsmtHalfBath","FullBath","HalfBath",
#        "BedroomAbvGr","KitchenAbvGr","TotRmsAbvGrd","Fireplaces",
#        "GarageYrBlt","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF",
#        "EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","MiscVal",
#        "MoSold","YrSold"))
num<-sapply(full,function(x)is.numeric(x))
num_var<-full[,num]
ncol(num_var)
numvar<-names(num_var)
colSums(is.na(full))
# Lets check the distribution 
sapply(full[numvar],function(x)plot(density(x,na.rm = T)))
# Skewness of each variable 
skew <- sapply(full[numvar],function(x)skewness(x,na.rm = T))
# substtting skewness>.75
skew <- skew[skew > 0.75]

# transforming all skewed variable with log(x + 1)
for(x in names(skew)){
  full[x] <- log(full[x]+1)
}

full[numvar[-38]]<-scale(full[numvar[-38]],center = TRUE, scale = TRUE)
full<-full[,-1]

# Creating dummy variable
library(caret)
dummyify<-dummyVars(~.,data = full,fullRank = T)
nwdt<-predict(dummyify,full)
nwdt<-data.frame(nwdt)
str(nwdt)


# train test split
index = 1:1460
train<-nwdt[index,]
test<-nwdt[-index,]

#outlier test in continous vars
numvar<-numvar[-1]
numvar
bsplt<-function(x){
  for(i in which(sapply(x,is.numeric))){
    boxplot(x[,i],main = colnames(x[i]))
    
  }
}
par(mfrow=c(3,3))
bsplt(train[numvar[1:9]])
bsplt(train[numvar[10:18]])
bsplt(train[numvar[19:27]])
bsplt(train[numvar[28:36]])
bsplt(train[numvar[37]])
par(mfrow=c(1,1))

dev.off()

pcap<-function(x){
  qnt <- quantile(x, probs=c(.25, .75))
  caps<-quantile(x,probs=c(0.05,0.95))
  H <- 1.5 * IQR(x)
  y <- x
  y[x < (qnt[1] - H)] <- caps[1]
  y[x > (qnt[2] + H)] <- caps[2]
  return(y)
}

train[numvar[1:36]]<-sapply(train[numvar[1:36]],pcap)
str(train)

#outlier test for continous vars after imputation
bsplt<-function(x){
  for(i in which(sapply(x,is.numeric))){
    boxplot(x[,i],main = colnames(x[i]))
    
  }
}
par(mfrow=c(3,3))
bsplt(train[numvar[1:9]])
bsplt(train[numvar[10:18]])
bsplt(train[numvar[19:27]])
bsplt(train[numvar[28:36]])
bsplt(train[numvar[37]])
par(mfrow=c(1,1))

dev.off()
# Lets check the distribution 
par(mfrow=c(3,3))
sapply(train[numvar],function(x)plot(density(x,na.rm = T)))
dev.off()

# Train test split
split<-createDataPartition(y = train$SalePrice, p = 0.7, list = FALSE)
dev<-train[split,]
val<-train[-split,]

#ridge-elasticnet-lasso
myTrainControl <- trainControl(method="cv",number=5)
fit.glmnet <- train(SalePrice~.,train,trControl = myTrainControl,
                    method="glmnet",tuneGrid=expand.grid(.alpha = seq(0,1,by=0.05), .lambda = seq(0, 0.08, by = 0.01)))

fit.glmnet
fit.glmnet$finalModel
fit.glmnet$metric
fit.glmnet$coefnames

varImp(fit.glmnet) 

dim(test)
dim(train)
colSums(is.na(test))
fix(hp_test)

predicted<-predict(fit.glmnet,newdata = val)
rmse <- sqrt(mean((log(abs(predicted))-log(abs(val$SalePrice)))^2))
rmse



test$SalePrice<-predict(fit.glmnet,newdata = test)
test$ID<-c(1461:2919)
test$SalePrice=exp(test$SalePrice)
final<-data.frame("ID"=test$ID,"value"=test$SalePrice)
View(final)
write.csv(final,"C:/Users/Subhranil/Documents/final.csv")

