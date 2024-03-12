#SVM
set.seed(2024)

df_svm <- df

## 80% of the sample size
smp_size <- floor(0.80 * nrow(df_svm))
train_ind <- sample(seq_len(nrow(df_svm)), size = smp_size)
train <- df_svm[train_ind, ]
test <- df_svm[-train_ind, ]
train <- balancearDataset(train)

svmfit = svm(train$DEATH_EVENT ~ ., data = train, kernel = "polynomial", cost = 3, scale = TRUE, cross = 5, gamma = 0.1, probability = TRUE)

predictedTrain <-  predict(svmfit, train, probability = TRUE)

#cut_svm_treino_bal <- testarCutoff(train$DEATH_EVENT, as.numeric((attr(predictedTrain, "probabilities")[,1]), 8))
#cut_svm_treino_bal

predictedTrain <- as.factor(if_else(attr(predictedTrain, "probabilities")[,1] >0.57,1,0))
table(predictedTrain, train$DEATH_EVENT)

# Matriz de confusão
caret::confusionMatrix(train$DEATH_EVENT, predictedTrain)

tab <- table(predictedTrain, train$DEATH_EVENT)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

pROC_svm <- pROC::roc(as.numeric(train$DEATH_EVENT),as.numeric(predictedTrain),
                      smoothed = TRUE,
                      # arguments for ci
                      ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                      # arguments for plot
                      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                      print.auc=TRUE, show.thres=TRUE)

pROC_svm$auc

predictedTest <- predict(svmfit, test, probability = TRUE)

#cut_svm_test_bal <- testarCutoff(test$DEATH_EVENT, as.numeric((attr(predictedTest, "probabilities")[,1]), 8))
#cut_svm_test_bal

predictedTest <- as.factor(if_else(attr(predictedTest, "probabilities")[,1] >0.57,1,0))

tab <- table(predictedTest, test$DEATH_EVENT)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

pROC_svm_test <- pROC::roc(as.numeric(test$DEATH_EVENT),as.numeric(predictedTest),
                           smoothed = TRUE,
                           # arguments for ci
                           ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                           # arguments for plot
                           plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                           print.auc=TRUE, show.thres=TRUE)

pROC_svm_test$auc
