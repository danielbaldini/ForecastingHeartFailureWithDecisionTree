# To be able to reproduce the results
set.seed(2024)

#### TRAIN AND TEST SPLITING #################################################################
## Train -> 80% of the sample size
## Test  -> 20% of the sample size
train_size <- 0.8
smp_size <- floor(train_size * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size)

ds_train <- df[train_ind, ]
ds_test <- df[-train_ind, ]
#############################################################################################

#### TRAINING THE MODEL  ####################################################################
treeModel <- rpart::rpart(
                      data = ds_train,
                      method = 'class',
                      parms = list(split = 'information'), #Tells the algorithm we want the percentages for each class. Removing it the answer will be 0 or 1. Cutoff = 0.5
                      
                      formula =  DEATH_EVENT ~ .,
                     #formula =  DEATH_EVENT ~ age +anaemia, #Model will be trained using only age and anaemia as features
                     #formula =  DEATH_EVENT ~ . -time -diabetes, #Model will be trained with all variables but time and diabetes

                     #Model Parameters / Fine tuning the model! 
                     control=rpart.control(minsplit = 15, #Minimum amount of observation needed for splitting a node
                                           maxdepth = 5, #How deep your tree can go
                                           cp = 0 #Small numbers encourage large trees, large number encourage small trees (Complexity -> reduce de Sum of Squared errors / Grid search helps you to find the best cp)
                                           )
                     #control=rpart.control(minsplit = 15, maxdepth = 5, cp=0, xval=5  #How many slices you want your data to be divided to cross-validate / I did not present that because it does not change the result in this dataset.) 
                     
                     #Try Split 10, max 20, cp 0.1 (check the tree / well balanced train/test)
                     
                     # change cp to 0.01, how do you feel?
                     # change min split to 1, how do you feel? The false positive went from 6 to 5. Seems to be better we have so far. What about complexity and model's performance? Does it worth it?
)

###############################################################################################

#### Tree Visualization #######################
rpart.plot::rpart.plot(treeModel)
###############################################

#### PREDICTING USING THE TRAINING DATA ##########
prob = predict(treeModel, ds_train)
head(prob, 10)

#unique(prob[,2])
cutoff <- 0.50

### GENERATING THE CONFUSION MATRIX #############
predictedClass = prob[,2] > cutoff
tab_train <- table(ds_train$DEATH_EVENT, predictedClass)
tab_train
#################################################

###  METRICS ####################################
TrueNegative <- tab_train[1,1]
TruePositive <- tab_train[2,2]
FalseNegative <- tab_train[2,1]
FalsePositive <- tab_train[1,2]

train_accuracy <- (TrueNegative + TruePositive) / (TrueNegative + TruePositive + FalseNegative + FalsePositive)
train_sensitivity <- TruePositive / (TruePositive + FalseNegative)
train_specificity <- TrueNegative / (TrueNegative + FalsePositive)
train_precision <- TruePositive / (TruePositive + FalsePositive)

print(paste0("Train Accuracy:", train_accuracy))
print(paste0("Train Sensitivity:", train_sensitivity))
print(paste0("Train Specificity:", train_specificity))
print(paste0("Train Precision:", train_precision))
#################################################


#######################################################################
### Testing the model with the TEST sample                          ###
#######################################################################


#### PREDICTING USING THE TEST DATA ###################################
probTest = predict(treeModel, ds_test)
head(probTest, 10)

### GENERATING THE CONFUSION MATRIX #############
predictedClassTest = probTest[,2]> cutoff
tab_test <- table(ds_test$DEATH_EVENT, predictedClassTest)
tab_test
#################################################

### GENERATING METRICS #############
TrueNegative <- tab_test[1,1]
TruePositive <- tab_test[2,2]
FalseNegative <- tab_test[2,1]
FalsePositive <- tab_test[1,2]

test_accuracy <- (TrueNegative + TruePositive) / (TrueNegative + TruePositive + FalseNegative + FalsePositive)
test_sensitivity <- TruePositive / (TruePositive + FalseNegative)
test_specificity <- TrueNegative / (TrueNegative + FalsePositive)
test_precision <- TruePositive / (TruePositive + FalsePositive)

print(paste0("Test Accuracy:", test_accuracy))
print(paste0("Test Sensitivity:", test_sensitivity))
print(paste0("Test Specificity:", test_specificity))
print(paste0("Test Precision:", test_precision))
#################################################


#Printing for presentation purposes
df_tree_results <- data.frame(matrix(ncol = 3, nrow = 0))
columns <- c("Metric","Train","Test")
colnames(df_tree_results) <- columns

df_tree_results[nrow(df_tree_results) + 1,] <- c("Accuracy",round(train_accuracy, 4), round(test_accuracy, 4))
df_tree_results[nrow(df_tree_results) + 1,] <- c("Sensitivity",round(train_sensitivity, 4), round(test_sensitivity, 4))
df_tree_results[nrow(df_tree_results) + 1,] <- c("Specificity",round(train_specificity, 4), round(test_specificity, 4))
df_tree_results[nrow(df_tree_results) + 1,] <- c("Precision",round(train_precision, 4), round(test_precision, 4))



rbind(tab_train, tab_test) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 26)



df_tree_results %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 26)








### Bonus code -> ROC Curve Visualization + AUC ################################################
# pROC_tree_train <- pROC::roc(as.numeric(train$DEATH_EVENT),as.numeric(class),
#                              smoothed = TRUE,
#                              # arguments for ci
#                              ci=TRUE, ci.alpha=0.9, stratified=FALSE,
#                              # arguments for plot
#                              plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
#                              print.auc=TRUE, show.thres=TRUE)
# 
# pROC_tree_train$auc
# 
# 
# pROC_tree_test <- pROC::roc(as.numeric(test$DEATH_EVENT),as.numeric(classTest),
#                               smoothed = TRUE,
#                               # arguments for ci
#                               ci=TRUE, ci.alpha=0.9, stratified=FALSE,
#                               # arguments for plot
#                               plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
#                               print.auc=TRUE, show.thres=TRUE)
# 
# pROC_tree_test$auc
################################################################################################