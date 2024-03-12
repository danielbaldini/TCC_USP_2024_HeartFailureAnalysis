########################## Funções de apoio ####################################
balancearDataset <- function(train){
  set.seed(2024)
  dfX <- as.data.frame(train[,1:12])
  dfY <- train[,13]
  balanced <- ubBalance(X=dfX, Y=dfY, type="ubSMOTE", percOver=120) #percOver -> Quantos registros sintéticos são gerados da menor classe
  train <- data.frame(balanced$X, DEATH_EVENT=balanced$Y)

  return(train)
}

testarCutoff <- function(valorOriginal, valorPredito) {
  dfResultado <- data.frame(matrix(ncol = 7, nrow = 0))
  dfResultado <- c("cutoff", "tamanhoAmostra", "totalCorretos", "FN","FP","VN","VP","ACC","SENS","ESPEC")
  colnames(df) <- dfResultado
  
  tamanhoAmostra <- length(valorOriginal)
  for (i in 10:90 ) {
    cutoff <- i/100
    valorPreditoTeste <- as.factor(if_else(valorPredito > cutoff,1,0))
    accTest <- caret::confusionMatrix(valorOriginal, valorPreditoTeste)
    print(paste("Cutoff:", cutoff))
    corretos <- accTest$table[1,1] + accTest$table[2,2]
    pessoasTratadas <- accTest$table[1,1] + accTest$table[2,2] + accTest$table[2,1]
    acuracia <- (accTest$table[1,1] + accTest$table[2,2]) / tamanhoAmostra
    sensibilidade <- accTest$table[2,2] / (accTest$table[2,2] + accTest$table[1,2])
    especificidade <- accTest$table[1,1] / (accTest$table[2,1] + accTest$table[1,1])

    new_row = list(cutoff, tamanhoAmostra, corretos, accTest$table[1,2], accTest$table[2,1], accTest$table[1,1], accTest$table[2,2], acuracia, sensibilidade, especificidade)
    dfResultado = rbind(dfResultado, new_row)
    
  }
  write.csv(dfResultado, "Cutoff.csv", row.names=TRUE)
  return(dfResultado)
}