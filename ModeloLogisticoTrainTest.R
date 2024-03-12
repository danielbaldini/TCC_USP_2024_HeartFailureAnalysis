set.seed(2024)

df <- df_bkp

## 85% of the sample size
smp_size <- floor(0.80 * nrow(df))

train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

modelo_logistico_binario <- glm(formula = DEATH_EVENT ~ .,
                                data = train,
                                family = "binomial")
#summary(modelo_logistico_binario)

#Procedimento Stepwise para identificar quais devem permanecer no modelo com 95%.
step_modelo_logistico_binario <- step(object = modelo_logistico_binario,
                                      k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
summ(model = step_modelo_logistico_binario, confint = T, digits = 4, ci.width = 0.95)

#Teste de resíduos do modelo
#plot(residuals(step_modelo_logistico_binario))
#shapiro.test(residuals(step_modelo_logistico_binario))

# Executando a predição do modelo a um novo df
colunasModelo <- c( "age", "ejection_fraction", "serum_creatinine","time","DEATH_EVENT")
df_predito_Logistico_Binario <- train[colunasModelo]
df_predito_Logistico_Binario$preditoTreino <- predict(object = step_modelo_logistico_binario, df_predito_Logistico_Binario, type = "response")

#cut_log_treino <- testarCutoff(df_predito_Logistico_Binario$DEATH_EVENT, df_predito_Logistico_Binario$preditoTreino)
#cut_log_treino

df_predito_Logistico_Binario$preditoTreinoBinario <- as.factor(if_else(df_predito_Logistico_Binario$preditoTreino >0.55,1,0))

# Matriz de confusão
accTrain <- caret::confusionMatrix(df_predito_Logistico_Binario$DEATH_EVENT, df_predito_Logistico_Binario$preditoTreinoBinario)
accTrain$overall[1]
accTrain$table
accTrain
#plot(accTrain$table)

# Curva ROC
pROC_Log_Train <- pROC::roc(as.numeric(df_predito_Logistico_Binario$DEATH_EVENT),as.numeric(df_predito_Logistico_Binario$preditoTreino),
                      smoothed = TRUE,
                      # arguments for ci
                      ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                      # arguments for plot
                      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                      print.auc=TRUE, show.thres=TRUE)

pROC_Log_Train$auc

df_predito_Logistico_Binario_test <- test[colunasModelo]
df_predito_Logistico_Binario_test$preditoTeste <- predict(step_modelo_logistico_binario, df_predito_Logistico_Binario_test, type = "response")

#cut_log_teste <- testarCutoff(df_predito_Logistico_Binario_test$DEATH_EVENT, df_predito_Logistico_Binario_test$preditoTeste)
#cut_log_teste

df_predito_Logistico_Binario_test$preditoTestBinario <- as.factor(if_else(df_predito_Logistico_Binario_test$preditoTeste >0.55,1,0))

# Matriz de confusão
accTest <- caret::confusionMatrix(df_predito_Logistico_Binario_test$DEATH_EVENT, df_predito_Logistico_Binario_test$preditoTestBinario)
accTest$overall[1]
accTest$table
accTest
plot(accTest$table)

# Curva ROC
pROC_Log_Test <- pROC::roc(as.numeric(df_predito_Logistico_Binario_test$DEATH_EVENT),as.numeric(df_predito_Logistico_Binario_test$preditoTestBinario),
                      smoothed = TRUE,
                      # arguments for ci
                      ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                      # arguments for plot
                      plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                      print.auc=TRUE, show.thres=TRUE)
pROC_Log_Test$auc
