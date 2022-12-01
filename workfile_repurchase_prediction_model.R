###############################################################################
# Re-Purchase Prediction Model
###############################################################################

# set working directory
setwd("D:/Machine Learning/Re-Purchase Prediction Model")

# read in the raw data
library(readr)
training_raw <- read.csv("repurchase_training.csv")

######################################
######## Undertake EDA
######################################
# Basic exploring the data in raw dataset
# Check the data
str(training_raw)
summary(training_raw)
head(training_raw)

### Note: there are 131337 record in the dataset,
###       age_band, gender, car_model, car_segment is chr.
###       have "NULL" as missing value

# new dataset with change data type and summary dataset again
training <- training_raw

training$age_band <- as.factor(training$age_band)
levels(training$age_band)

training$gender <- as.factor(training$gender)
levels(training$gender)

training$car_model <- as.factor(gsub("model_", "", training$car_model))
training$car_model <- as.numeric(as.character(training$car_model))
training$car_model <- as.factor(training$car_model)
levels(training$car_model)

training$car_segment <- as.factor(training$car_segment)
levels(training$car_segment)

str(training)
summary(training)

# check duplicates and check missing value
anyDuplicated(training)
anyNA(training)
### Note: cannot find missing value as it record as "NULL".

# list frequency for fields, could use to check the amount of NULL each field
apply(training[,-1], 2, table)
### Note:ID is unique.
###      age_band has 112375 NULL, $gender has 69308 NULL, keep it for now.
###      car model 19 has 2 record.


###############################################################################

######################################
######## Visualization Plots
######################################

# Part a - Target (percentage of repurchase)
ggplot(training, 
       aes(x=as.factor(Target), y= ..count.. / sum(..count..))) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  xlab("Target") +
  ylab("Percent") +
  ggtitle ("Target")

ggplot(training[training$age_band == "NULL" | training$gender == "NULL", ], 
       aes(x=as.factor(Target), y= ..count.. / sum(..count..))) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = scales::percent(round((..count..)/sum(..count..),2)),
                y= ((..count..)/sum(..count..))), stat="count",
            vjust = -.25) +
  xlab("Target") +
  ylab("Percent") +
  ggtitle ("Target for NULL")
### Note: only 3% of customers choose to repurchase from the same manufacturer

##################
# Part b - age_band
# Car segment distribution in each age band
library(dplyr)
library("ggplot2")
plot <- training %>%
  count(age_band, car_model, car_segment, gender) %>%
  group_by(age_band, car_model, car_segment) %>%
  mutate (percent = n/sum(n))
ggplot(plot, aes(x = age_band, y = percent, fill = car_segment)) + 
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust =1))+
  xlab("Age Band") +
  ylab("Percent") +
  ggtitle ("Car segment distribution in each age band")

# all data
ggplot(training, aes(x=age_band, y= ..count.. / sum(..count..), 
                     fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Age Band") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("Age Band Analysis")
### Note: Age Band = NULL is too large, need analysis without age_band = null

# age_band != NULL
ggplot(training[training$age_band != "NULL", ], 
       aes(x=age_band, y= ..count.. / sum(..count..), fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Age Band") +
  ylab("Percent") +
  theme(legend.position="bottom") +   
  ggtitle ("Age Band without age_band = NULL")

# Target = 1 (Market Segment for repurchase)
ggplot(training[training$Target == "1" , ], 
       aes(x=age_band, y= ..count.. / sum(..count..), fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Age Band") +
  ylab("Percent") +
  theme(legend.position="bottom") +   
  ggtitle ("Age_Band has purchase more than 1 vehicle")
### Note: Age Band = NULL is too large, need analysis without age_band = null

# Target = 1 and age_band != NULL
ggplot(training[training$Target == "1" & training$age_band != "NULL", ], 
       aes(x=age_band, y= ..count.. / sum(..count..), fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Age Band") +
  ylab("Percent") +
  theme(legend.position="bottom") +   
  ggtitle ("Age_Band has purchase more than 1 vehicle without age_band = NULL")
### Note: about 50% of customers who repurchase is between age 45-64 
###       excluding age_band = null
###       (age_band = 4 or 5)

##################
# Part c - Gender
# all data
ggplot(training, 
       aes(x=gender, y= ..count.. / sum(..count..), fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("Gender")

# Target = 1
ggplot(training[training$Target == "1" , ], 
       aes(x=gender, y= ..count.. / sum(..count..), fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Gender") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("Gender has purchase more than 1 vehicle")

##################
# Part d - car_model
ggplot(training, aes(x=car_model, y=..count.. / sum(..count..), 
                     fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Car Model") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("Car Models")

ggplot(training[training$Target == "1", ], 
       aes(x=car_model, y= ..count.. / sum(..count..), 
           fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("car_model") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("car_model for Target = 1")

ggplot(training[training$age_band == "NULL" | training$gender == "NULL", ], 
       aes(x=car_model, y= ..count.. / sum(..count..), 
           fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("car_model") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("car_model for NULL")

ggplot(training[training$age_band != "NULL" & training$gender != "NULL", ], 
       aes(x=car_model, y= ..count.. / sum(..count..), 
           fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("car_model") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("car_model without NULL")

##################
# Part e - age of vehicle
# all data
ggplot(training, 
       aes(x=as.factor(age_of_vehicle_years), y= ..count.. / sum(..count..), 
           fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Age of vehicle years") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("Age of vehicle years")

# Target = 1
ggplot(training[training$Target == "1", ], 
       aes(x=as.factor(age_of_vehicle_years), y= ..count.. / sum(..count..), 
           fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("Age of vehicle years") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("Age of vehicle years of Customer has purchase more than 1 vehicle")
### Note: year of their last vehicle is manly within 5 years.

# age_band = NULL or gender= NULL 
ggplot(training[training$age_band == "NULL" | training$gender == "NULL", ], 
       aes(x=age_of_vehicle_years, y= ..count.. / sum(..count..), 
           fill = car_segment)) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 90, hjust =1)) +
  xlab("Age of vehicle years") +
  ylab("Percent") +
  theme(legend.position="bottom") +
  ggtitle ("age of vehicle for NULL")

##################
# Part f - total paid service
ggplot(training, 
       aes(x=as.factor(total_paid_services), y= ..count.. / sum(..count..))) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("total_paid_services") +
  ylab("Percent") +
  ggtitle ("total_paid_services")

ggplot(training[training$age_band == "NULL" | training$gender == "NULL", ], 
       aes(x=as.factor(total_paid_services), y= ..count.. / sum(..count..))) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("total_paid_services") +
  ylab("Percent") +
  ggtitle ("total_paid_services for NULL")

ggplot(training[training$age_band != "NULL" & training$gender != "NULL", ], 
       aes(x=as.factor(total_paid_services), y= ..count.. / sum(..count..))) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  xlab("total_paid_services") +
  ylab("Percent") +
  ggtitle ("total_paid_services without NULL")


###############################################################################

######################################
######## Basic Data Analysis
######################################

# check relation between car_model and car_segment
output <- training %>%
  group_by(car_model, car_segment) %>%
  count(car_model, car_segment)
### Note: some car_model have more than one segment

# Correlation of numeric variables, corrplot, correlogram
library("corrplot")
numeric_var <- names(dplyr::select_if(training, is.numeric))
numeric_var_corr <- cor(training[, numeric_var])
numeric_var_corr

# find attributes that are highly corrected (ideally >0.75)
library("caret")
highlyCorrelated <- findCorrelation(numeric_var_corr, cutoff=0.75, 
                                    verbose = TRUE)
highlyCorrelated

# 75% of the sample size
set.seed(42)

trainset_size <- floor(0.75 * nrow(training))
trainset_indices <- sample(seq_len(nrow(training)), size = trainset_size)
trainset <- training[trainset_indices, ]
testset <- training[-trainset_indices, ]

testset$Target <- as.factor(testset$Target)

######################################
######## Logistic Regression
######################################
#              Reference
# Prediction     0     1
#          0 31850   645
#          1    79   261

#Precision: 0.767647059 
#Recall: 0.288079470 
#F1: 0.418940610 
#AUC£º0.911962538    

trainset_lr <- trainset
testset_lr <- testset

# Create model
lr <-  glm(formula = Target ~. -ID, data = trainset_lr, 
               family = "binomial")
summary(lr)
### Note: AIC = 15469

# predict the model
testset_lr$probability <- predict(lr, testset_lr, 
                                      type = "response")

# Set probability threshold to 0.4
testset_lr$prediction = "0"
testset_lr[testset_lr$probability >= 0.4, "prediction"] = "1"

# confusion matrix
cm_lr <- confusionMatrix(data = as.factor(testset_lr$prediction), 
                              testset_lr$Target, positive="1")
cm_lr

# precision, recall, F1, AUC
cm_lr$byClass

testset_lr$prediction <- as.numeric(testset_lr$prediction)
library("pROC")
roc_lr <- roc(response = testset_lr$Target, 
                  predictor = testset_lr$prediction)
roc_lr
plot(roc_lr)


# Plot the ROC curve
library(ROCR)
roc_lr_testset <- prediction(testset_lr$probability,testset_lr$Target)

trainset_lr$probability <- predict(lr,trainset_lr,type="response")
# Set probability threshold to 0.4
trainset_lr$prediction = "0"
trainset_lr[trainset_lr$probability >= 0.4, "prediction"] = "1"
roc_lr_trainset <- prediction(trainset_lr$probability,trainset_lr$Target)

# tpr and fpr for our training
train_lr_tpr_fpr = performance(roc_lr_trainset, "tpr","fpr")
train_lr_auc = performance(roc_lr_trainset, "auc")

#tpr and fpr for our testing
test_lr_tpr_fpr = performance(roc_lr_testset, "tpr","fpr")
test_lr_auc = performance(roc_lr_testset, "auc")

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_lr_tpr_fpr, main="Testing and Training ROC Curves", col = "blue")
plot(train_lr_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), 
       lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

# some AUC figures
train_lr_auc = unlist(slot(train_lr_auc, "y.values"))
train_lr_auc

# Area under the ROC curve
test_lr_auc = unlist(slot(test_lr_auc, "y.values"))
test_lr_auc


######################################
######## Decision Tree 
######################################
#             Reference
# Prediction     0     1
#          0 31829   397
#          1   100   509
#precision: 0.83579639
#recall: 0.56181015
#F1: 0.67194719
#AUC: 0.87858687    

set.seed(42)
trainset_dt <-  trainset
testset_dt <-  testset

# Create model
library("rpart")
dt <-  rpart(Target ~.-ID,data = trainset_dt, method="class") 

# Plot tree
library(rpart.plot)
prp(dt)

# Calculate prediction and probability
testset_dt_probability <- predict(dt,testset_dt,type="prob")
testset_dt$probability <- testset_dt_probability[,2]

# Set probability threshold to 0.4
testset_dt$prediction = "0"
testset_dt[testset_dt$probability >= 0.4, "prediction"] = "1"


library("caret")
# confusion matrix
cm_dt <- confusionMatrix(as.factor(testset_dt$prediction), 
                             testset_dt$Target, positive="1")
cm_dt

# precision, recall, F1, AUC
cm_dt$byClass

testset_dt$prediction <- as.numeric(testset_dt$prediction)
roc_dt <- roc(response = testset_dt$Target, 
                  predictor = testset_dt$prediction)
roc_dt
plot(roc_dt)


# Plot the ROC curve
library(ROCR)
roc_dt_testset <- prediction(testset_dt_probability[,2],testset_dt$Target)

trainset_dt_probability <- predict(dt,trainset_dt,type="prob")
trainset_dt$probability <- trainset_dt_probability[,2]
# Set probability threshold to 0.4
trainset_dt$prediction = "0"
trainset_dt[trainset_dt$probability >= 0.4, "prediction"] = "1"
roc_dt_trainset <- prediction(trainset_dt_probability[,2],trainset_dt$Target)

# tpr and fpr for our training
train_dt_tpr_fpr = performance(roc_dt_trainset, "tpr","fpr")
train_dt_auc = performance(roc_dt_trainset, "auc")

#tpr and fpr for our testing
test_dt_tpr_fpr = performance(roc_dt_testset, "tpr","fpr")
test_dt_auc = performance(roc_dt_testset, "auc")

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_dt_tpr_fpr, main="Testing and Training ROC Curves", col = "blue")
plot(train_dt_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), 
       lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

# some AUC figures
train_dt_auc = unlist(slot(train_dt_auc, "y.values"))
train_dt_auc

# Area under the ROC curve
test_dt_auc = unlist(slot(test_dt_auc, "y.values"))
test_dt_auc

######################################
######## LASSO Regression
######################################
#              Reference
# Prediction     0     1
#          0 31888   705
#          1    41   201
#precision: 0.830578512
#recall: 0.221854305
#F1:  0.350174216 
#AUC: 0.912386146

library("glmnet")
set.seed(42)
trainset_las <-  trainset
testset_las <-  testset

# Set up X and Y, and remove ID and Target
x <-  model.matrix(~ ., trainset_las[, c(-1,-2)])
y <-  trainset_las$Target

# Create model, Alpha = 1 specifies lasso regression
las <-  cv.glmnet(x, y, family = 'binomial', alpha = 1)
las

# Plot the model 
plot(las)

# Choose lambda.min or lambda.1se 
las$lambda.min 
las$lambda.1se 
coef(las, las$lambda.min)
max(abs(coef(las, las$lambda.1se)))

# Predict the model
testset_lasprediction <-  predict(las$glmnet.fit, 
                       newx = model.matrix(~ ., testset_las[, c(-1,-2)]), 
                       type = "class", s = las$lambda.1se)
testset_lasprobability <-  predict(las$glmnet.fit, 
                                       newx = model.matrix(~ ., 
                                                       testset_las[, c(-1,-2)]), 
                                       type = "response", s = las$lambda.1se)


# Calculate prediction and probability
testset_las$probability <-  predict(las$glmnet.fit, 
                       newx = model.matrix(~ ., testset_las[, c(-1,-2)]), 
                       type = "response", s = las$lambda.1se)
testset_las$probability <- testset_lasprobability
testset_las$prob <- round(testset_las$probability, 4)
# Set probability threshold to 0.4
testset_las$prediction = "0"
testset_las[testset_las$probability >= 0.4, "prediction"] = "1"

# confusion matrix
cm_las <-  confusionMatrix(data = as.factor(testset_las$prediction), 
                               testset_las$Target, positive="1")
cm_las
# precision, recall, F1, AUC
cm_las$byClass 

testset_las$prediction <- as.numeric(testset_las$prediction)
roc_las <- roc(response = testset_las$Target, 
                   predictor = testset_las$prediction)
roc_las 
plot(roc_las)


# Plot the ROC curve
library(ROCR)
roc_las_testset <- prediction(testset_las$probability,testset_las$Target)

trainset_las$probability <-  predict(las$glmnet.fit, 
                                    newx = model.matrix(~ ., 
                                                      trainset_las[, c(-1,-2)]), 
                                    type = "response", s = las$lambda.1se)
trainset_las$prob <- round(trainset_las$probability, 4)
# Set probability threshold to 0.4
trainset_las$prediction = "0"
trainset_las[trainset_las$probability >= 0.4, "prediction"] = "1"
roc_las_trainset <- prediction(trainset_las$probability,trainset_las$Target)

# tpr and fpr for our training
train_las_tpr_fpr = performance(roc_las_trainset, "tpr","fpr")
train_las_auc = performance(roc_las_trainset, "auc")

#tpr and fpr for our testing
test_las_tpr_fpr = performance(roc_las_testset, "tpr","fpr")
test_las_auc = performance(roc_las_testset, "auc")

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_las_tpr_fpr, main="Testing and Training ROC Curves", col = "blue")
plot(train_las_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), 
       lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

# some AUC figures
train_las_auc = unlist(slot(train_las_auc, "y.values"))
train_las_auc

# Area under the ROC curve
test_las_auc = unlist(slot(test_las_auc, "y.values"))
test_las_auc


######################################
######## Random Forests
######################################
# Reference
# Prediction     0     1
#          0 31870   149
#          1    59   757
#precision: 0.92769608 
#recall: 0.83554084
#F1: 0.87921022
#AUC: 0.99634295   

library("randomForest")
set.seed(42)
trainset_rf <-  trainset
testset_rf <-  testset

trainset_rf$Target <- as.character(trainset_rf$Target)
trainset_rf$Target <- as.factor(trainset_rf$Target)

# Create model
rf = randomForest(Target ~. -ID, data = trainset_rf, 
                      importance=TRUE, xtest=testset_rf[,c(-1,-2)], 
                      keep.forest=TRUE, ntree=500)
rf

# Calculate prediction and probability
testset_rf_probability <- predict(rf,testset_rf,type="prob")
testset_rf$probability <- testset_rf_probability[,2]
# Set probability threshold to 0.4
testset_rf$prediction = "0"
testset_rf[testset_rf$probability >= 0.4, "prediction"] = "1"

# confusion matrix
library(caret)
cm_rf = confusionMatrix(data = as.factor(testset_rf$prediction), 
                            testset_rf$Target, positive="1")
cm_rf

# precision, recall, F1, AUC
cm_rf$byClass

testset_rf$prediction <- as.numeric(testset_rf$prediction)
roc_rf <- roc(response = testset_rf$Target, 
                  predictor = testset_rf$prediction)
roc_rf 
plot(roc_rf)

# Measure of importancs   
importance(rf)

# Plot of most important variables
varImpPlot(rf)

# Plot the ROC curve
library(ROCR)
roc_rf_testset <- prediction(testset_rf_probability[,2],testset_rf$Target)

trainset_rf_probability <- predict(rf,trainset_rf,type="prob")
trainset_rf$probability <- trainset_rf_probability[,2]
# Set probability threshold to 0.4
trainset_rf$prediction = "0"
trainset_rf[trainset_rf$probability >= 0.4, "prediction"] = "1"
roc_rf_trainset <- prediction(trainset_rf_probability[,2],trainset_rf$Target)

# tpr and fpr for our training
train_rf_tpr_fpr = performance(roc_rf_trainset, "tpr","fpr")
train_rf_auc = performance(roc_rf_trainset, "auc")

#tpr and fpr for our testing
test_rf_tpr_fpr = performance(roc_rf_testset, "tpr","fpr")
test_rf_auc = performance(roc_rf_testset, "auc")

# Plot the tpr and fpr gains chart ROC for both testing and training data
plot(test_rf_tpr_fpr, main="Testing and Training ROC Curves", col = "blue")
plot(train_rf_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), 
       lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

# some AUC figures
train_rf_auc = unlist(slot(train_rf_auc, "y.values"))
train_rf_auc

# Area under the ROC curve
test_rf_auc = unlist(slot(test_rf_auc, "y.values"))
test_rf_auc


#Partial Dependency Plots for Random Forest Model
library("pdp")
autoplot(partial(rf, pred.var=c("annualised_mileage"), chull = TRUE))
autoplot(partial(rf, pred.var=c("mth_since_last_serv"), chull = TRUE))
autoplot(partial(rf, pred.var=c("gender"), chull = TRUE))
autoplot(partial(rf, pred.var=c("num_serv_dealer_purchased"), chull = TRUE))
autoplot(partial(rf, pred.var=c("age_of_vehicle_years"), chull = TRUE))


# Comparing Random Forest with other models
model_auc <- c(test_rf_auc, test_lr_auc, test_dt_auc, test_las_auc)
model_cm<-cbind(data.frame(cm_rf$byClass),
                data.frame(cm_lr$byClass),
                data.frame(cm_dt$byClass),
                data.frame(cm_las$byClass) 
                )
model_compare_metrics<-rbind(model_cm, AUC = model_auc)

model_compare_metrics


######################################
######## Validation Data Set
######################################

# Read validation Data set
validation_raw <- read.csv("repurchase_validation.csv")

# Check the data, nrow, ncol, str
str(validation_raw)
dim(validation_raw)
summary(validation_raw)

validation <- validation_raw

validation$age_band <- as.factor(validation$age_band)
levels(validation$age_band)

validation$gender <- as.factor(validation$gender)
levels(validation$gender)

validation$car_model <- as.factor(gsub("model_", "", validation$car_model))
validation$car_model <- as.numeric(as.character(validation$car_model))
validation$car_model <- as.factor(validation$car_model)
levels(validation$car_model)

validation$car_segment <- as.factor(validation$car_segment)
levels(validation$car_segment)


# Create ID column and target column in the data set
validation$ID <- seq.int(nrow(validation))
lvl<-c("0","1")
validation$Target <- factor(lvl,levels=c("0","1"))

validation <- rbind(training[1, ] , validation) 
validation <- validation[-1,] 

# Calculate target_probability and target_class  
validation_rf_probability <- predict(rf,validation,type="prob")
validation$target_probability <- validation_rf_probability[,2]

validation$target_class <- predict(rf,validation)

#Write output
library("dplyr")
write.csv((validation%>%select(ID=ID,target_probability = target_probability,
                               target_class = target_class)),
          file="repurchase_validation",row.names = FALSE)
forcast <- read.csv("repurchase_validation",header = TRUE)

count(forcast, target_class)
dim(forcast)
str(forcast)
summary(forcast)

