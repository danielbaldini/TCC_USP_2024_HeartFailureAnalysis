#SVM
# O dataset Scaled eleva a acur??cia de 60% para 75% neste algoritmo!!!
# Ainda assim ele se mostra menos acertivo do que a ??rvore de decis??o, mas ?? um aumento expressivo.
set.seed(2024)

#colunasModelo <- c( "ejection_fraction", "serum_creatinine", "time","DEATH_EVENT")

df_svm <- df


## 80% of the sample size - COMO ESTOU USANDO O CROSS VALIDATION, N??O ?? PRECISO SEPARAR A BASE!
#smp_size <- floor(0.80 * nrow(df_svm))

#train_ind <- sample(seq_len(nrow(df_svm)), size = smp_size)
#train <- df_svm[train_ind, ]
#test <- df_svm[-train_ind, ]


#Trabalhar com o Gamma e adicionar ao texto
svmfit = svm(df_svm$DEATH_EVENT ~ ., data = df_svm, kernel = "polynomial", cost = 3, scale = TRUE, cross = 10, gamma = 0.1) 
#print(svmfit)

svmfit$coefs

predicted <-  predict(svmfit, df_svm)
table(predicted, df_svm$DEATH_EVENT)

print(svmfit$coefs)


# Matriz de confus??o
caret::confusionMatrix(df_svm$DEATH_EVENT, predicted)

#test$predicted <-  predict(svmfit, test)
#table(test$predicted, test$DEATH_EVENT)

# Matriz de confus??o
caret::confusionMatrix(predicted, df_svm$DEATH_EVENT)

tab <- table(predicted, df_svm$DEATH_EVENT)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc


pROC_svm <- pROC::roc(as.numeric(df_svm$DEATH_EVENT),as.numeric(predicted),
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)

pROC_svm$auc
#sens.ci <- ci.se(pROC_svm)
#plot(sens.ci, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
#plot(sens.ci, type="bars")

#test$DEATH_EVENT

#precrec_obj <-precrec::evalmod(scores = as.numeric(df_svm$DEATH_EVENT), labels = as.numeric(df_svm$predicted))
#autoplot(precrec_obj)





