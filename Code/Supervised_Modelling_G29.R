FunLabelEncoder_factor <- function(df) {
  for (c in names(df)) {
    if (is.character(df[[c]])) {
      df[[c]] <- as.factor(df[[c]])
    }
  }
  return(df)
}
modelling_data <- read.csv("cleaned_data_v3", T)

data_factor <- FunLabelEncoder_factor(modelling_data)
data_factor <- data_factor[ , !(names(data_factor) %in% c("date","underwriter",
                                                          "commission_structure",
                                                          "adviser_id","be","severity",
                                                          "be_cover_amount",
                                                          "severity_cover_amount",
                                                          "life_cover_amount",
                                                          "tpd_cover_amount",
                                                          "trauma_cover_amount",
                                                          "ip_cover_amount",
                                                          "package"))]
str(data_factor)

#Separating the data into Train and Test sets for the different products
library(caret)
library(pROC)
library(glmnet)

set.seed(2)
myindex<-createDataPartition(data_factor$life, p = 0.7,list = FALSE)
train_life<-data_factor[myindex, ]
test_life<-data_factor[-myindex, ]
count_life_yes <- sum(train_life$life == "Yes")
count_life_all <- nrow(train_life)
cutoff_life <- count_life_yes/count_life_all

myindex<-createDataPartition(data_factor$trauma, p = 0.7,list = FALSE)
train_trauma<-data_factor[myindex, ]
test_trauma<-data_factor[-myindex, ]
count_trauma_yes <- sum(train_trauma$trauma == "Yes")
count_trauma_all <- nrow(train_trauma)
cutoff_trauma <- count_trauma_yes/count_trauma_all

myindex<-createDataPartition(data_factor$tpd, p = 0.7,list = FALSE)
train_tpd<-data_factor[myindex, ]
test_tpd<-data_factor[-myindex, ]
count_tpd_yes <- sum(train_tpd$tpd == "Yes")
count_tpd_all <- nrow(train_tpd)
cutoff_tpd <- count_tpd_yes/count_tpd_all

myindex<-createDataPartition(data_factor$ip, p = 0.7,list = FALSE)
train_ip<-data_factor[myindex, ]
test_ip<-data_factor[-myindex, ]
count_ip_yes <- sum(train_ip$ip == "Yes")
count_ip_all <- nrow(train_ip)
cutoff_ip <- count_ip_yes/count_ip_all

#Logistic Regression
#life
logistic_life <- glm(life ~ ., family = binomial, data = train_life)
logistic_life_pred <- predict(logistic_life,type="response",newdata=test_life)
summary(logistic_life)

logistic_life_pred <- ifelse(logistic_life_pred > cutoff_life, 'Yes', 'No')
logistic_life_pred <- factor(logistic_life_pred, levels = c('No', 'Yes'))
confusionMatrix(logistic_life_pred, as.factor(test_life$life), positive = 'Yes')

logistic_life_pred_numeric <- as.numeric(logistic_life_pred) - 1
roc_logistic_life <- roc(ifelse(test_life$life == "Yes", 1, 0), logistic_life_pred_numeric)
auc(roc_logistic_life)

#trauma
logistic_trauma <- glm(trauma ~ ., family = binomial, data = train_trauma)
logistic_trauma_pred <- predict(logistic_trauma,type="response",newdata=test_trauma)
summary(logistic_trauma)

logistic_trauma_pred <- ifelse(logistic_trauma_pred > cutoff_trauma, 'Yes', 'No')
logistic_trauma_pred <- factor(logistic_trauma_pred, levels = c('No', 'Yes'))
confusionMatrix(logistic_trauma_pred, as.factor(test_trauma$trauma), positive = 'Yes')

logistic_trauma_pred_numeric <- as.numeric(logistic_trauma_pred) - 1
roc_logistic_trauma <- roc(ifelse(test_trauma$trauma == "Yes", 1, 0), logistic_trauma_pred_numeric)
auc(roc_logistic_trauma)

#tpd
logistic_tpd <- glm(tpd ~ ., family = binomial, data = train_tpd)
logistic_tpd_pred <- predict(logistic_tpd,type="response",newdata=test_tpd)
summary(logistic_tpd)

logistic_tpd_pred <- ifelse(logistic_tpd_pred > cutoff_tpd, 'Yes', 'No')
logistic_tpd_pred <- factor(logistic_tpd_pred, levels = c('No', 'Yes'))
confusionMatrix(logistic_tpd_pred, as.factor(test_tpd$tpd), positive = 'Yes')

logistic_tpd_pred_numeric <- as.numeric(logistic_tpd_pred) - 1
roc_logistic_tpd <- roc(ifelse(test_tpd$tpd == "Yes", 1, 0), logistic_tpd_pred_numeric)
auc(roc_logistic_tpd)


#ip
logistic_ip <- glm(ip ~ ., family = binomial, data = train_ip)
logistic_ip_pred <- predict(logistic_ip,type="response",newdata=test_ip)
summary(logistic_ip)

logistic_ip_pred <- ifelse(logistic_ip_pred > cutoff_ip, 'Yes', 'No')
logistic_ip_pred <- factor(logistic_ip_pred, levels = c('No', 'Yes'))
confusionMatrix(logistic_ip_pred, as.factor(test_ip$ip), positive = 'Yes')

logistic_ip_pred_numeric <- as.numeric(logistic_ip_pred) - 1
roc_logistic_ip <- roc(ifelse(test_ip$ip == "Yes", 1, 0), logistic_ip_pred_numeric)
auc(roc_logistic_ip)

################################################################################
#K-nearest neighbors
library("parallel")
library("doParallel")
FunLabelEncoder_factor <- function(df) {
  for (c in names(df)) {
    if (is.character(df[[c]])) {
      df[[c]] <- as.factor(df[[c]])
      levels(df[[c]]) <- as.character(0:(length(levels(df[[c]])) - 1))
    }
  }
  return(df)
}

data_knn <- modelling_data
data_knn <- data_knn[ , !(names(data_knn) %in% c("date","underwriter",
                                                 "commission_structure",
                                                 "adviser_id","be","severity",
                                                 "be_cover_amount",
                                                 "severity_cover_amount",
                                                 "life_cover_amount",
                                                 "tpd_cover_amount",
                                                 "trauma_cover_amount",
                                                 "ip_cover_amount",
                                                 "package"))]
data_knn <- FunLabelEncoder_factor(data_knn)

str(data_knn)

numeric_vars <- sapply(data_knn, is.numeric)
scaled_numeric <- scale(data_knn[, numeric_vars])
scaled_data_knn <- data.frame(data_knn[!numeric_vars], scaled_numeric)
str(scaled_data_knn)

#life
# Train-test split
set.seed(3)
myindex_knn_life <- createDataPartition(scaled_data_knn$life, p = 0.7, list = FALSE)
train_knn_life <- scaled_data_knn[myindex_knn_life, ]
test_knn_life <- scaled_data_knn[-myindex_knn_life, ]
count_knn_life_yes <- sum(train_knn_life$life == 1)
count_all_knn_life <- nrow(train_knn_life)
cutoff_knn_life <- count_knn_life_yes / count_all_knn_life

# kNN Model Training and Cross-Validation
ctrl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(.k = c(1, 3, 5, 7))  # Try with a smaller range of k values

# Set up parallel processing
num_cores <- detectCores()  # Use all available cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Train kNN model
KNN_life <- train(life ~ ., data = train_knn_life, method = "knn", trControl = ctrl, tuneGrid = grid)

# Stop parallel processing
stopCluster(cl)

# Model Prediction and Evaluation
KNN_predictions_life <- predict(KNN_life, newdata = test_knn_life, type = "prob")

confusion_matrix_life <- confusionMatrix(as.factor(ifelse(KNN_predictions_life[,2] > cutoff_knn_life, "1", "0")),
                                         test_knn_life$life, positive = "1")
print(confusion_matrix_life)

roc_knn_life <- roc(test_knn_life$life, KNN_predictions_life[,2])
print(paste("AUC:", auc(roc_knn_life)))


#trauma
# Train-test split
set.seed(3)
myindex_knn_trauma <- createDataPartition(scaled_data_knn$trauma, p = 0.7, list = FALSE)
train_knn_trauma <- scaled_data_knn[myindex_knn_trauma, ]
test_knn_trauma <- scaled_data_knn[-myindex_knn_trauma, ]
count_knn_trauma_yes <- sum(train_knn_trauma$trauma == 1)
count_all_knn_trauma <- nrow(train_knn_trauma)
cutoff_knn_trauma <- count_knn_trauma_yes / count_all_knn_trauma

# kNN Model Training and Cross-Validation
ctrl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(.k = c(1, 3, 5, 7))  # Try with a smaller range of k values

# Set up parallel processing
num_cores <- detectCores()  # Use all available cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Train kNN model
KNN_trauma <- train(trauma ~ ., data = train_knn_trauma, method = "knn", trControl = ctrl, tuneGrid = grid)

# Stop parallel processing
stopCluster(cl)

# Model Prediction and Evaluation
KNN_predictions_trauma <- predict(KNN_trauma, newdata = test_knn_trauma, type = "prob")
KNN_predictions_trauma

confusion_matrix_trauma <- confusionMatrix(as.factor(ifelse(KNN_predictions_trauma[,2] > cutoff_knn_trauma, "1", "0")),
                                           test_knn_trauma$trauma, positive = "1")
print(confusion_matrix_trauma)

roc_knn_trauma <- roc(test_knn_trauma$trauma, KNN_predictions_trauma[,2])
print(paste("AUC:", auc(roc_knn_trauma)))

#tpd
# Train-test split
set.seed(3)
myindex_knn_tpd <- createDataPartition(scaled_data_knn$tpd, p = 0.7, list = FALSE)
train_knn_tpd <- scaled_data_knn[myindex_knn_tpd, ]
test_knn_tpd <- scaled_data_knn[-myindex_knn_tpd, ]
count_knn_tpd_yes <- sum(train_knn_tpd$tpd == 1)
count_all_knn_tpd <- nrow(train_knn_tpd)
cutoff_knn_tpd <- count_knn_tpd_yes / count_all_knn_tpd

# kNN Model Training and Cross-Validation
ctrl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(.k = c(1, 3, 5, 7))  # Try with a smaller range of k values

# Set up parallel processing
num_cores <- detectCores()  # Use all available cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Train kNN model
KNN_tpd <- train(tpd ~ ., data = train_knn_tpd, method = "knn", trControl = ctrl, tuneGrid = grid)

# Stop parallel processing
stopCluster(cl)

# Model Prediction and Evaluation
KNN_predictions_tpd <- predict(KNN_tpd, newdata = test_knn_tpd, type = "prob")
KNN_predictions_tpd

confusion_matrix_tpd <- confusionMatrix(as.factor(ifelse(KNN_predictions_tpd[,2] > cutoff_knn_tpd, "1", "0")),
                                        test_knn_tpd$tpd, positive = "1")
print(confusion_matrix_tpd)

roc_knn_tpd <- roc(test_knn_tpd$tpd, KNN_predictions_tpd[,2])
print(paste("AUC:", auc(roc_knn_tpd)))

#ip
# Train-test split
set.seed(3)
myindex_knn_ip <- createDataPartition(scaled_data_knn$ip, p = 0.7, list = FALSE)
train_knn_ip <- scaled_data_knn[myindex_knn_ip, ]
test_knn_ip <- scaled_data_knn[-myindex_knn_ip, ]
count_knn_ip_yes <- sum(train_knn_ip$ip == 1)
count_all_knn_ip <- nrow(train_knn_ip)
cutoff_knn_ip <- count_knn_ip_yes / count_all_knn_ip

# kNN Model Training and Cross-Validation
ctrl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(.k = c(1, 3, 5, 7))  # Try with a smaller range of k values

# Set up parallel processing
num_cores <- detectCores()  # Use all available cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Train kNN model
KNN_ip <- train(ip ~ ., data = train_knn_ip, method = "knn", trControl = ctrl, tuneGrid = grid)

# Stop parallel processing
stopCluster(cl)

# Model Prediction and Evaluation
KNN_predictions_ip <- predict(KNN_ip, newdata = test_knn_ip, type = "prob")
KNN_predictions_ip

confusion_matrix_ip <- confusionMatrix(as.factor(ifelse(KNN_predictions_ip[,2] > cutoff_knn_ip, "1", "0")),
                                       test_knn_ip$ip, positive = "1")
print(confusion_matrix_ip)

roc_knn_ip <- roc(test_knn_ip$ip, KNN_predictions_ip[,2])
print(paste("AUC:", auc(roc_knn_ip)))

################################################################################
library(randomForest)
library(pdp)
#random forest
#life
random_forest_life<-randomForest(life~.,data = train_life,ntree=100,importance=TRUE)
summary(random_forest_life)

varImpPlot(random_forest_life,type = 1)
rondomforest_predictions_life<-predict(random_forest_life,test_life,type = "prob")

predicted_classes_life <- ifelse(rondomforest_predictions_life[, "Yes"] > cutoff_life, "Yes", "No")
predicted_classes_life <- factor(predicted_classes_life, levels = c("No", "Yes"))
test_life$life <- factor(test_life$life, levels = c("No", "Yes"))

confusionMatrix(predicted_classes_life, test_life$life, positive = "Yes")

roc_randomforest_life <- roc(test_life$life, as.numeric(predicted_classes_life == "Yes"))
auc(roc_randomforest_life)

# pdps - non technical audience - likely not useful!
partialPlot(random_forest_life, train_life, age_next)
partialPlot(random_forest_life, train_life, annual_income)

#trauma
random_forest_trauma<-randomForest(trauma~.,data = train_trauma,ntree=100,importance=TRUE)
summary(random_forest_trauma)

varImpPlot(random_forest_trauma,type = 1)
rondomforest_predictions_trauma<-predict(random_forest_trauma,test_trauma,type = "prob")

predicted_classes_trauma <- ifelse(rondomforest_predictions_trauma[, "Yes"] > cutoff_trauma, "Yes", "No")
predicted_classes_trauma <- factor(predicted_classes_trauma, levels = c("No", "Yes"))
test_trauma$trauma <- factor(test_trauma$trauma, levels = c("No", "Yes"))

confusionMatrix(predicted_classes_trauma, test_trauma$trauma, positive = "Yes")

roc_randomforest_trauma <- roc(test_trauma$trauma, as.numeric(predicted_classes_trauma == "Yes"))
auc(roc_randomforest_trauma)

#tpd
random_forest_tpd<-randomForest(tpd~.,data = train_tpd,ntree=100,importance=TRUE)
summary(random_forest_tpd)

varImpPlot(random_forest_tpd,type = 1)
rondomforest_predictions_tpd<-predict(random_forest_tpd,test_tpd,type = "prob")

predicted_classes_tpd <- ifelse(rondomforest_predictions_tpd[, "Yes"] > cutoff_tpd, "Yes", "No")
predicted_classes_tpd <- factor(predicted_classes_tpd, levels = c("No", "Yes"))
test_tpd$tpd <- factor(test_tpd$tpd, levels = c("No", "Yes"))

confusionMatrix(predicted_classes_tpd, test_tpd$tpd, positive = "Yes")

roc_randomforest_tpd <- roc(test_tpd$tpd, as.numeric(predicted_classes_tpd == "Yes"))
auc(roc_randomforest_tpd)

#ip
random_forest_ip<-randomForest(ip~.,data = train_ip,ntree=100,importance=TRUE)
summary(random_forest_ip)

varImpPlot(random_forest_ip,type = 1)
rondomforest_predictions_ip<-predict(random_forest_ip,test_ip,type = "prob")

predicted_classes_ip <- ifelse(rondomforest_predictions_ip[, "Yes"] > cutoff_ip, "Yes", "No")
predicted_classes_ip <- factor(predicted_classes_ip, levels = c("No", "Yes"))
test_ip$ip <- factor(test_ip$ip, levels = c("No", "Yes"))

confusionMatrix(predicted_classes_ip, test_ip$ip, positive = "Yes")

roc_randomforest_ip <- roc(test_ip$ip, as.numeric(predicted_classes_ip == "Yes"))
auc(roc_randomforest_ip)

################################################################################
#classification tree
library(rpart)
library(rpart.plot)
unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

#life
initial_tree_life <- rpart(life ~ ., data = train_life)

custom_control <- data.frame(cp = seq(0.001, 0.1, by = 0.001))
ctrl <- trainControl(method = "cv", number = 10)
cv_model_life <- train(life ~ ., data = train_life, method = "rpart", trControl = ctrl, tuneGrid = custom_control)
optimal_cp_life <- cv_model_life$bestTune$cp

pruned_tree_life <- prune(initial_tree_life, cp = optimal_cp_life)
prune_predictions_life <- predict(pruned_tree_life, newdata = test_life, type = "prob")

predicted_classes_life <- ifelse(prune_predictions_life[, "Yes"] > cutoff_life, "Yes", "No")
predicted_classes_life <- factor(predicted_classes_life, levels = c("No", "Yes"))
test_life$life <- factor(test_life$life, levels = c("No", "Yes"))
confusionMatrix(predicted_classes_life, test_life$life, positive = "Yes")

roc_prune_life <- roc(test_life$life, prune_predictions_life[,2])
auc(roc_prune_life)

rpart.plot(pruned_tree_life, box.palette = "Blues", shadow.col = "gray", nn = TRUE)

#trauma
initial_tree_trauma <- rpart(trauma ~ ., data = train_trauma)

custom_control <- data.frame(cp = seq(0.001, 0.1, by = 0.001))
ctrl <- trainControl(method = "cv", number = 10)
cv_model_trauma <- train(trauma ~ ., data = train_trauma, method = "rpart", trControl = ctrl, tuneGrid = custom_control)
optimal_cp_trauma <- cv_model_trauma$bestTune$cp

pruned_tree_trauma <- prune(initial_tree_trauma, cp = optimal_cp_trauma)
prune_predictions_trauma <- predict(pruned_tree_trauma, newdata = test_trauma, type = "prob")

predicted_classes_trauma <- ifelse(prune_predictions_trauma[, "Yes"] > cutoff_trauma, "Yes", "No")
predicted_classes_trauma <- factor(predicted_classes_trauma, levels = c("No", "Yes"))
test_trauma$trauma <- factor(test_trauma$trauma, levels = c("No", "Yes"))
confusionMatrix(predicted_classes_trauma, test_trauma$trauma, positive = "Yes")

roc_prune_trauma <- roc(test_trauma$trauma, prune_predictions_trauma[,2])
auc(roc_prune_trauma)

rpart.plot(pruned_tree_trauma, box.palette = "Blues", shadow.col = "gray", nn = TRUE)

#tpd
initial_tree_tpd <- rpart(tpd ~ ., data = train_tpd)

custom_control <- data.frame(cp = seq(0.001, 0.1, by = 0.001))
ctrl <- trainControl(method = "cv", number = 10)
cv_model_tpd <- train(tpd ~ ., data = train_tpd, method = "rpart", trControl = ctrl, tuneGrid = custom_control)
optimal_cp_tpd <- cv_model_tpd$bestTune$cp

pruned_tree_tpd <- prune(initial_tree_tpd, cp = optimal_cp_tpd)
prune_predictions_tpd <- predict(pruned_tree_tpd, newdata = test_tpd, type = "prob")


predicted_classes_tpd <- ifelse(prune_predictions_tpd[, "Yes"] > cutoff_tpd, "Yes", "No")
predicted_classes_tpd <- factor(predicted_classes_tpd, levels = c("No", "Yes"))
test_tpd$tpd <- factor(test_tpd$tpd, levels = c("No", "Yes"))
confusionMatrix(predicted_classes_tpd, test_tpd$tpd, positive = "Yes")

roc_prune_tpd <- roc(test_tpd$tpd, prune_predictions_tpd[,2])
auc(roc_prune_tpd)

rpart.plot(pruned_tree_tpd, box.palette = "Blues", shadow.col = "gray", nn = TRUE)

#ip
initial_tree_ip <- rpart(ip ~ ., data = train_ip)

custom_control <- data.frame(cp = seq(0.001, 0.1, by = 0.001))
ctrl <- trainControl(method = "cv", number = 10)
cv_model_ip <- train(ip ~ ., data = train_ip, method = "rpart", trControl = ctrl, tuneGrid = custom_control)
optimal_cp_ip <- cv_model_ip$bestTune$cp

pruned_tree_ip <- prune(initial_tree_ip, cp = optimal_cp_ip)
prune_predictions_ip <- predict(pruned_tree_ip, newdata = test_ip, type = "prob")


predicted_classes_ip <- ifelse(prune_predictions_ip[, "Yes"] > cutoff_ip, "Yes", "No")
predicted_classes_ip <- factor(predicted_classes_ip, levels = c("No", "Yes"))
test_ip$ip <- factor(test_ip$ip, levels = c("No", "Yes"))
confusionMatrix(predicted_classes_ip, test_ip$ip, positive = "Yes")

roc_prune_ip <- roc(test_ip$ip, prune_predictions_ip[,2])
auc(roc_prune_ip)

rpart.plot(pruned_tree_ip, box.palette = "Blues", shadow.col = "gray", nn = TRUE)

################################################################################
library(e1071)

#Naive Bayes
#life
nb_life <- naiveBayes(life ~ ., data = train_life)
nb_predictions_life <- predict(nb_life, newdata = test_life)

confusionMatrix(nb_predictions_life, test_life$life)

nb_predictions_numeric <- as.numeric(nb_predictions_life)
roc_nb_life <- roc(test_life$life, nb_predictions_numeric)
auc(roc_nb_life)

#trauma
nb_trauma <- naiveBayes(trauma ~ ., data = train_trauma)
nb_predictions_trauma <- predict(nb_trauma, newdata = test_trauma)

confusionMatrix(nb_predictions_trauma, test_trauma$trauma)

nb_predictions_numeric <- as.numeric(nb_predictions_trauma)
roc_nb_trauma <- roc(test_trauma$trauma, nb_predictions_numeric)
auc(roc_nb_trauma)

#tpd
nb_tpd <- naiveBayes(tpd ~ ., data = train_tpd)
nb_predictions_tpd <- predict(nb_tpd, newdata = test_tpd)

confusionMatrix(nb_predictions_tpd, test_tpd$tpd)

nb_predictions_numeric <- as.numeric(nb_predictions_tpd)
roc_nb_tpd <- roc(test_tpd$tpd, nb_predictions_numeric)
auc(roc_nb_tpd)

#ip
nb_ip <- naiveBayes(ip ~ ., data = train_ip)
nb_predictions_ip <- predict(nb_ip, newdata = test_ip)

confusionMatrix(nb_predictions_ip, test_ip$ip)

nb_predictions_numeric <- as.numeric(nb_predictions_ip)
roc_nb_ip <- roc(test_ip$ip, nb_predictions_numeric)
auc(roc_nb_ip)