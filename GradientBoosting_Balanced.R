#Binomial boosting
set.seed(2024)

df_glmboost <- df

## 85% of the sample size
smp_size <- floor(0.80 * nrow(df_glmboost))

train_ind <- sample(seq_len(nrow(df_glmboost)), size = smp_size)
train <- df_glmboost[train_ind, ]
train <- balancearDataset(train)

test <- df[-train_ind, ]

glm1 <-  glmboost(DEATH_EVENT ~ .,
                  data = train,
                  family = Binomial())

#logLik(glm1)
#coef(glm1, off2int=TRUE)

predito_boosting_train <- predict(glm1,train )

#cut_boost_treino_bal <- testarCutoff(train$DEATH_EVENT, predito_boosting_train)
#cut_boost_treino_bal

predito_boosting_train <- ifelse(predito_boosting_train >= 0.27,1,0)

tab <- table(train$DEATH_EVENT, predito_boosting_train)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

pROC_boosting_treino <- pROC::roc(as.numeric(train$DEATH_EVENT),as.numeric(predito_boosting_train),
                                  smoothed = TRUE,
                                  # arguments for ci
                                  ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                                  # arguments for plot
                                  plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                  print.auc=TRUE, show.thres=TRUE)

pROC_boosting_treino$auc

predito_boosting_test <- predict(glm1,test)

#cut_boost_teste_bal <- testarCutoff(test$DEATH_EVENT, predito_boosting_test)
#cut_boost_teste_bal

predito_boosting_test <- ifelse(predito_boosting_test>=0.27,1,0)

tab <- table(test$DEATH_EVENT, predito_boosting_test)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

pROC_boosting_test <- pROC::roc(as.numeric(test$DEATH_EVENT),as.numeric(predito_boosting_test),
                                smoothed = TRUE,
                                # arguments for ci
                                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                                # arguments for plot
                                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                                print.auc=TRUE, show.thres=TRUE)

pROC_boosting_test$auc
