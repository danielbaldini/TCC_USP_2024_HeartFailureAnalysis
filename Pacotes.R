##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
pacotes <- c("readr" #Ler arquivo CSV
             ,"jtools" #Obter detalhes sobre modelo de regressão gerado
             ,"dplyr" #função if_else
             ,"caret" #Criar matriz de confusão
             ,"pROC" #Visualizar a curva ROC
             ,"e1071" #Pacote para estimar Support Vector Machine
             ,"rpart","rpart.plot" #Estimar e visualizar Árvore de Decisão
             ,"mboost" #Estimar Modelo Binomial Boosting
             ,"naivebayes" #Estimar Modelo NaiveBayes
             ,"unbalanced" #Balancear Dataset
             )

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

