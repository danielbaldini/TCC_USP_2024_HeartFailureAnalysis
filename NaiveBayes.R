#NaiveBayes
set.seed(2024)

df_nb <- df

## 80% of the sample size
smp_size <- floor(0.80 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

model_nb <- naivebayes::naive_bayes(DEATH_EVENT ~ ., data = train)

#plot(model_nb)

predictions_train <- predict(model_nb,  newdata = select(train,-DEATH_EVENT), type = "prob")

#cut_naive_treino <- testarCutoff(train$DEATH_EVENT, predictions_train[,1])
#cut_naive_treino

predictions_train <- as.factor(if_else(predictions_train[,1] >0.2,1,0))

tab <- table(predictions_train, train$DEATH_EVENT)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

pROC_Naive_Train <- pROC::roc(as.numeric(train$DEATH_EVENT),as.numeric(predictions_train),
                      smoothed = TRUE,
                      # arguments for ci
                      ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                      # arguments for plot
                      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                      print.auc=TRUE, show.thres=TRUE)

pROC_Naive_Train$auc

predictionsTest <- predict(model_nb,  newdata = select(test,-DEATH_EVENT), type = "prob")

#cut_naive_teste <- testarCutoff(test$DEATH_EVENT, predictionsTest[,1])
#cut_naive_teste

predictionsTest <- as.factor(if_else(predictionsTest[,1] >0.2,1,0))

tab <- table(predictionsTest, test$DEATH_EVENT)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

pROC_Naive_Test <- pROC::roc(as.numeric(test$DEATH_EVENT),as.numeric(predictionsTest),
                      smoothed = TRUE,
                      # arguments for ci
                      ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                      # arguments for plot
                      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                      print.auc=TRUE, show.thres=TRUE)

pROC_Naive_Test$auc
