library(ROCR)
library(pROC)
library(ggplot2)
library("randomForest")
library("parallel")
library("boot")
library("xgboost")
library("Matrix")
library(Ckmeans.1d.dp)
library("parallel")
library(dplyr)

getwd()
setwd('/home/apadala/Documents/AV_competition')

#reading the data
data <- read.csv('/home/apadala/Documents/AV_competition/Train_pjb2QcD.csv',na.strings = "")
test_submit <- read.csv('/home/apadala/Documents/AV_competition/Test_wyCirpO.csv',na.strings = "")

#combinging both train and test datasets
data$flag <- T
test_submit$flag <- F
test_submit$Business_Sourced <- '1'
data_comb <- rbind(data,test_submit)

str(data_comb)
summary(data_comb)
head(data_comb)
dim(data_comb)

################
#Pre-processing#
################

#changing the formats of some variables to factors
data_comb$Office_PIN <- as.factor(data_comb$Office_PIN)
data_comb$Applicant_City_PIN <- as.factor(data_comb$Applicant_City_PIN)
data_comb$Manager_Grade <- as.factor(data_comb$Manager_Grade)
data_comb$Business_Sourced <- as.factor(data_comb$Business_Sourced)
data_comb$Application_Receipt_Date<-as.Date(data_comb$Application_Receipt_Date, format = "%m/%d/%Y")
data_comb$Applicant_BirthDate<-as.Date(data_comb$Applicant_BirthDate, format = "%m/%d/%Y")
data_comb$Manager_DOJ<-as.Date(data_comb$Manager_DOJ, format = "%m/%d/%Y")
data_comb$Manager_DoB <- as.Date(data_comb$Manager_DoB , format = "%m/%d/%Y")
data_comb$ID <- NULL

#checking for missing values
missing<-function(x){
  return (sum(is.na(x)))
}
apply(data_comb,2,missing)

#implementing the missing values for the applicant
data_comb[is.na(data_comb$Applicant_Gender),"Applicant_Gender"]<-'M'
data_comb[is.na(data_comb$Applicant_City_PIN),"Applicant_City_PIN"]<-data_comb[is.na(data_comb$Applicant_City_PIN),"Office_PIN"]
data_comb<-data_comb[!is.na(data_comb$Applicant_City_PIN),]

#data_comb[is.na(data_comb$Applicant_BirthDate),"Applicant_BirthDate"]<-'1975-04-02'

data_comb[data_comb$Applicant_Marital_Status %in% c('D','W') | is.na(data_comb$Applicant_Marital_Status),'Applicant_Marital_Status'] <- 'M'
data_comb$Applicant_Marital_Status <- factor(data_comb$Applicant_Marital_Status)
levels(data_comb$Applicant_Occupation) <- c(levels(data_comb$Applicant_Occupation), "Unknown")
data_comb[is.na(data_comb$Applicant_Occupation),"Applicant_Occupation"] <- "Unknown"
table(data_comb$Applicant_Occupation)
#data_comb[data_comb$Applicant_Occupation %in% c('Self Employed','Student'),'Applicant_Occupation'] <- 'Self Employed'
data_comb$Applicant_Occupation <- factor(data_comb$Applicant_Occupation) 
setdiff(unique(data$Applicant_Qualification),unique(test_submit$Applicant_Qualification))
#[1] "Associate/Fellow of Institute of Company Secretories of India"
#[2] "Associate/Fellow of Insurance Institute of India"             
#[3] "Associate/Fellow of Acturial Society of India"                
#[4] "Certified Associateship of Indian Institute of Bankers"   
data_comb[!(data_comb$Applicant_Qualification %in% c('Class XII','Graduate','Class X','Others','Masters of Business Administration')) | 
       is.na(data_comb$Applicant_Qualification),'Applicant_Qualification'] <- 'Others'
data_comb$Applicant_Qualification <- factor(data_comb$Applicant_Qualification)
setdiff(unique(data$Manager_Joining_Designation),unique(test_submit$Manager_Joining_Designation))
table(data_comb$Manager_Joining_Designation)
data_comb[data_comb$Manager_Joining_Designation %in% 
       c('Level 4','Level 5','Level 6','Level 7','Other'),'Manager_Joining_Designation'] <- 'Level 4'
data_comb$Manager_Joining_Designation <- factor(data_comb$Manager_Joining_Designation) 
setdiff(unique(data$Manager_Current_Designation),unique(test_submit$Manager_Current_Designation))
table(data_comb$Manager_Current_Designation)
data_comb$Manager_Current_Designation <- factor(data_comb$Manager_Current_Designation) 
setdiff(unique(data$Manager_Grade),unique(test_submit$Manager_Grade))
table(data_comb$Manager_Grade)
data_comb[data_comb$Manager_Grade %in% c(1,9,10),'Manager_Grade'] <- '2'
data_comb$Manager_Grade <- factor(data_comb$Manager_Grade) 

#imputing missing values for manager
levels(data_comb$Manager_Joining_Designation) <- c(levels(data_comb$Manager_Joining_Designation), "Unknown")
data_comb[is.na(data_comb$Manager_Joining_Designation),'Manager_Joining_Designation'] <- 'Unknown'
levels(data_comb$Manager_Current_Designation) <- c(levels(data_comb$Manager_Current_Designation), "Unknown")
data_comb[is.na(data_comb$Manager_Current_Designation),'Manager_Current_Designation'] <- 'Unknown'
levels(data_comb$Manager_Grade) <- c(levels(data_comb$Manager_Grade), "-1")
data_comb[is.na(data_comb$Manager_Grade),'Manager_Grade'] <- '-1'
levels(data_comb$Manager_Status) <- c(levels(data_comb$Manager_Status), "Unknown")
data_comb[is.na(data_comb$Manager_Status),'Manager_Status'] <- 'Unknown'
levels(data_comb$Manager_Gender) <- c(levels(data_comb$Manager_Gender), "O")
data_comb[is.na(data_comb$Manager_Gender),'Manager_Gender'] <- 'O'
data_comb[is.na(data_comb$Manager_Num_Application),'Manager_Num_Application'] <- -1
data_comb[is.na(data_comb$Manager_Num_Coded),'Manager_Num_Coded'] <- -1
data_comb[is.na(data_comb$Manager_Business),'Manager_Business'] <- -1
data_comb[is.na(data_comb$Manager_Num_Products),'Manager_Num_Products'] <- -1
data_comb[is.na(data_comb$Manager_Business2),'Manager_Business2'] <- -1
data_comb[is.na(data_comb$Manager_Num_Products2),'Manager_Num_Products2'] <- -1
#data_comb[is.na(data_comb$Manager_DOJ),'Manager_DOJ'] <- '2006-05-12'
#data_comb[is.na(data_comb$Manager_DoB),'Manager_DoB'] <- '1973-03-15'

apply(data_comb,2,missing)
dim(data_comb)


######################
# Feature engineering#
######################

#extracting the first digit in the pincode
data_comb$Office_PIN_code <- as.factor(substr(data_comb$Office_PIN,0,1))
data_comb$applicant_PIN_code <- as.factor(substr(data_comb$Applicant_City_PIN,0,1))
data_comb[data_comb$applicant_PIN_code == '9','applicant_PIN_code']<-'8'
data_comb$applicant_PIN_code <- factor(data_comb$applicant_PIN_code) 
#extracting month and year of the application date
data_comb$application_receipt_month <- as.factor(format(data_comb$Application_Receipt_Date,'%m'))
data_comb$application_receipt_year <- as.factor(format(data_comb$Application_Receipt_Date,'%Y'))
#calculating the age of the applicant
data_comb$applicant_age <- as.numeric(difftime(data_comb$Application_Receipt_Date, data_comb$Applicant_BirthDate, unit="weeks"))/52.25
#to calculate the distance b/w the office and the applicant
data_comb$distance <- abs(as.numeric(levels(data_comb$Office_PIN))[data_comb$Office_PIN] - as.numeric(levels(data_comb$Applicant_City_PIN))[data_comb$Applicant_City_PIN])
data_comb$dist_bins <- ifelse(data_comb$distance > 50000, 'V_far',
                         ifelse(data_comb$distance > 10000, 'far',
                                ifelse(data_comb$distance > 5000, 'notso_far',
                                       ifelse(data_comb$distance > 1000, 'ok_far',
                                              ifelse(data_comb$distance > 500, 'near',"V_near")))))
data_comb$dist_bins <- as.factor(data_comb$dist_bins)
#calculating the experience of the manager
data_comb$manager_exp <- as.numeric(difftime(data_comb$Application_Receipt_Date, data_comb$Manager_DOJ, unit="weeks"))/52.25
#calculating the age of the manager
data_comb$manager_age <- as.numeric(difftime(data_comb$Application_Receipt_Date, data_comb$Manager_DoB, unit="weeks"))/52.25

dim(data_comb)
#removing the date fields 
data_comb$Applicant_BirthDate <- NULL
data_comb$Manager_DOJ <- NULL
data_comb$Manager_DoB <- NULL
#imputing the missing values with -1
data_comb[is.na(data_comb)] <- -1

#saving the files after pre-processing and not considering variables that are used to create new ones
write.csv(data_comb,'/home/apadala/Documents/AV_competition/data_comb.csv')
data_comb_prep <- data_comb[,-c(1,3,23,24,26)]
write.csv(data_comb_prep,'/home/apadala/Documents/AV_competition/data_comb_prep.csv')
str(data_comb_prep)
summary(data_comb_prep)
dim(data_comb_prep)

#spliting the complete data into intial train and test data
data_train <- data_comb_prep[data_comb_prep$flag == T,]
data_train$flag <- NULL
dim(data_train)
data_test <- data_comb_prep[data_comb_prep$flag == F,]
data_test$flag <- NULL
data_test$Business_Sourced <- NULL
dim(data_test)


###################################################
#creating the order of applicants - most important#
###################################################
data_train$Application_Receipt_Days = as.numeric(as.Date("2016-01-01") - as.Date(data_train$Application_Receipt_Date, "%m/%d/%Y"))
data_test$Application_Receipt_Days = as.numeric(as.Date("2016-01-01") - as.Date(data_test$Application_Receipt_Date, "%m/%d/%Y"))

data_train$Order <- seq(0, nrow(data_train)-1)
data_test$Order <- seq(0, nrow(data_test)-1)

data_train_order <- 
  data_train %>%
    group_by(Application_Receipt_Days) %>%
    summarise(min_order = max(Order),
              max_order = min(Order))
  
data_test_order <- 
  data_test %>%
  group_by(Application_Receipt_Days) %>%
  summarise(min_order = max(Order),
            max_order = min(Order))

data_train <- merge(data_train, data_train_order, by.x = "Application_Receipt_Days", by.y = "Application_Receipt_Days")
data_test <- merge(data_test, data_test_order, by.x = "Application_Receipt_Days", by.y = "Application_Receipt_Days")

# normalizing order to [0,1]
data_train$Order_Percentile <- (data_train$Order - data_train$min_order)/(data_train$max_order - data_train$min_order)
data_test$Order_Percentile <- (data_test$Order - data_test$min_order)/(data_test$max_order - data_test$min_order)
data_train <- select(data_train, -c(Order, min_order, max_order, Application_Receipt_Days,Application_Receipt_Date))
data_test <- select(data_test, -c(Order, min_order, max_order, Application_Receipt_Days,Application_Receipt_Date))

data_train[is.na(data_train)] <- 1
data_test[is.na(data_test)] <- 1

#creating a function that calculates the auc#
auc_cal <- function(outcome, proba){
  N = length(proba)
  N_pos = sum(outcome)
  df = data.frame(out = outcome, prob = proba)
  df = df[order(-df$prob),]
  df$above = (1:N) - cumsum(df$out)
  return( 1- sum( df$above * df$out ) / (N_pos * (N-N_pos) ) )
}


#building a base model using manual k-fold technique
set.seed(235)
k=10
n=floor(nrow(data_train)/k)
log_auc_cv<-c()
for (i in 1:k){
  s1 = ((i-1)*n+1)
  s2 = (i*n)
  subset = s1:s2
  log_train<- data_train[-subset,]
  log_test<- data_train[subset,]
  log_fit<-glm(Business_Sourced ~ ., family=binomial, data = log_train)
  log_pred <- predict(log_fit, log_test,type = "response")
  log_auc_cv[i] <- auc_cal(as.numeric(log_test$Business_Sourced)-1,log_pred)
  print(paste("Logistic Accuracy: ",log_auc_cv[i]))
}
print(paste("Logistics Regression - Cross Validation AUC score: ", mean(log_auc_cv))) 
#0.57503837695554 #0.0.878527066131782

#using cv.glm funstion
logmodel.glm <- glm(Business_Sourced~. ,family=binomial, data = data_train)
logmodel_cv <- cv.glm(data = data_train, glmfit = logmodel.glm, cost = auc_cal, K = 10)
logmodel_cv_auc <- logmodel_cv$delta[[1]] #0.5958
print(paste("Logistic (cv.glm) - AUC score: ",logmodel_cv_auc))
#0.600145686523254 #0.881147488374305

#For submission
LogModel_pred <- predict(logmodel.glm, data_test, type = "response")
submit_av <- data.frame(ID = test_submit[,c(1)],Business_Sourced = LogModel_pred)
write.csv(submit_av,'/home/apadala/Documents/AV_competition/submit_AV.csv',row.names=FALSE)


#random forest

ntree = c(1000)
mtry = c(3,4,5,6)
node_size = c(1,2,3)

rf_oob = c()
rf_auc = c()
rf_accuracy_all=data.frame("Trees" = integer(0),"Features"=integer(0), "Node_size" = integer(0),
                           "OOB_err"= numeric(0),"AUC_ROC" = numeric(0))
for (t in ntree)
  {
  for (m in mtry)
    {
      for (n in node_size)
      {
        rf_fit <- randomForest(x = data_train[,!(colnames(data_train) == "Business_Sourced")],
                               y = data_train[,(colnames(data_train) == "Business_Sourced")],
                               ntree = t, mtry = m, importance=TRUE,nodesize = n,
                               strata = levels(data_train[,(colnames(data_train) == "Business_Sourced")]),
                               replace = F)
        m_oob <- rf_fit$err.rate[t]
        rf_oob < append(x = rf_oob, values = m_oob)
        m_auc <-auc_cal(as.numeric(data_train$Business_Sourced)-1,predict(rf_fit,type = "prob")[,2])
        rf_auc <-append(x = rf_auc, values = m_auc)
        print(paste("Trees: ",t,"Features: ", m,  "NodeSize: ",n ,"OOB_err: ",m_oob,
                    "Auc_Roc",m_auc))
        rf_accuracy_all<- rbind(rf_accuracy_all, data.frame("Trees" = t,"Features" = m,"Node_size" = n,
                                                            "OOB_err" = m_oob,"AUC_ROC"=m_auc))
      }
    }
  }
rf_accuracy_all
rf_accuracy_all[rf_accuracy_all$AUC_ROC == max(rf_accuracy_all$AUC_ROC),]
#Trees Features Node_size   OOB_err   AUC_ROC
#1000        3         2 0.3408566 0.6428248
#Trees Features Node_size   OOB_err   AUC_ROC
#1  1000        3         2 0.1855973 0.8915891
rf_fit_best <-   randomForest(x = data_train[,!(colnames(data_train) == "Business_Sourced")],
                              y = data_train[,(colnames(data_train) == "Business_Sourced")],
                              ntree = 1000, mtry = 6, importance=TRUE,nodesize = 3,
                              strata = levels(data_train[,(colnames(data_train) == "Business_Sourced")]),
                              replace = T, sampsize = nrow(data_train))
print(paste("Random Forest - AUC score:",auc_cal(as.numeric(data_train$Business_Sourced)-1,
                                                 predict(rf_fit_best,type = "prob")[,2])))
#auc: 0.891433
importance(rf_fit_best, type = 2)
imp <- data.frame(importance(rf_fit_best, type = 2))
imp$Variables <- rownames(imp)
rownames(imp)<- NULL
imp<-imp[order(-imp$MeanDecreaseGini),c("Variables","MeanDecreaseGini")]
plot(rf_fit_best, log="y")
varImpPlot(rf_fit_best)

#For submission
rf_test_data <- predict(rf_fit_best, data_test, type = "prob")[,2]
submit_av <- data.frame(ID = test_submit[,c(1)],Business_Sourced = rf_test_data)
write.csv(submit_av,'/home/apadala/Documents/AV_competition/submit_AV.csv',row.names=FALSE)


#Xgboost

set.seed(235)
subset <- sample(1:nrow(data_train), size = 0.9*nrow(data_train))

#creating a sparse matrix
x_train <- sparse.model.matrix(Business_Sourced ~ .-1, data = data_train[subset,])
x_test<- sparse.model.matrix(Business_Sourced ~ .-1, data = data_train[-subset,])
y_train<-as.integer(data_train[subset,colnames(data_train)=='Business_Sourced'])-1
y_test<-as.integer(data_train[-subset,colnames(data_train)=='Business_Sourced'])-1

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)
watchlist <- list(train=dtrain, test=dtest)

#creating a simple xgb model to check for performance before tuning the parameters

bst <- xgboost(data = x_train,label = y_train,
               nrounds = 1000, objective = "binary:logistic",
               eval_metric="auc",verbose = F)
xgb_pred <- predict(bst, dtest)
auc_cal(as.numeric(data_train[-subset, 'Business_Sourced'])-1,xgb_pred)
#train_auc : 0.926511     test_auc : 0.8804693


#cross-validation

x_train_cv <- sparse.model.matrix(Business_Sourced ~ .-1, data = data_train)
y_train_cv<-as.integer(data_train[,colnames(data_train)=='Business_Sourced'])-1
dtrain_cv <- xgb.DMatrix(data = x_train_cv, label = y_train_cv)

#defining the paramaters for the model
param = expand.grid(max_depth = c(3,4,5),
                    eta = c(0.005,0.01,0.05),
                    nrounds = c(1000),
                    subsample = c(0.7,0.75),
                    min_child_weight = c(1,2),
                    colsample_bytree = c(0.8,1),
                    scale_pos_weight = 1,
                    gamma = c(0,1))

p=data.frame()
  
for (i in 1:nrow(param))
{
  prm <- param[i,]
  cv.res <- xgb.cv(data = dtrain_cv,label = y_train_cv, nfold = 5,
                   objective = "binary:logistic",
                   eta = prm$eta,
                   max_depth = prm$max_depth,
                   nrounds = prm$nrounds,
                   subsample = prm$subsample,
                   gamma = prm$gamma,
                   colsample_bytree = prm$colsample_bytree,
                   scale_pos_weight = prm$scale_pos_weight,
                   min_child_weight = prm$min_child_weight,
                   nthread = detectCores(),
                   eval_metric = "auc",
                   verbose = F)
  p<- rbind(p,data.frame("rounds" = prm$nround, "max_depth"=prm$max_depth, "eta"=prm$eta, prm$subsample,
                         prm$colsample_bytree,prm$scale_pos_weight,prm$min_child_weight, prm$gamma,
                         "train_auc" = cv.res$train.auc.mean[prm$nrounds], 
                         "test_auc" = cv.res$test.auc.mean[prm$nrounds]))
}
p

#to print the model's important variables

bst <- xgboost(data = dtrain_cv,label = y_train_cv,
               nrounds = 300, objective = "binary:logistic",
               max_depth = 5,
               eta = 0.01,
               subsample = 0.7,
               min_child_weight = 1,
               colsample_bytree = 0.8,
               scale_pos_weight = 1,
               nthread = detectCores(),
               eval_metric="auc",
               verbose = F)

#to print the model's important variables

importance_matrix <- xgb.importance(dimnames(data_train)[[2]], model = bst)
importance_matrix
xgb.plot.importance(importance_matrix)


