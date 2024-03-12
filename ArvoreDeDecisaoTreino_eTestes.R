#Arvore de Decisao Treino e testes
set.seed(2024)
## 85% of the sample size
smp_size <- floor(0.8 * nrow(df))
colnames(train)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)
train <- df[train_ind, ]
test <- df[-train_ind, ]

arvore <- rpart::rpart(DEATH_EVENT ~ .,
                       data=train,
                       parms = list(split = 'information'), # podemos trocar para  'gini' mas a performance cai
                       method='class',
                       control=rpart.control(minsplit = 15, maxdepth = 3, cp=0, xval=5)
)


# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)

# Plotando a Árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta,) # Paleta de cores

prob = predict(arvore, train)

#cut_arv_treino <- testarCutoff(train$DEATH_EVENT, prob[,2])
#cut_arv_treino

class = prob[,2]>.6
tab <- table(class, train$DEATH_EVENT)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

pROC_arvore_train <- pROC::roc(as.numeric(train$DEATH_EVENT),as.numeric(class),
                               smoothed = TRUE,
                               # arguments for ci
                               ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                               # arguments for plot
                               plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                               print.auc=TRUE, show.thres=TRUE)

pROC_arvore_train$auc

probTest = predict(arvore, test)

#cut_arv_teste <- testarCutoff(test$DEATH_EVENT, probTest[,2])
#cut_arv_teste

classTest = probTest[,2]>.6
tab <- table(classTest, test$DEATH_EVENT)
tab
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

#tab_cp <- rpart::printcp(arvore)
#rpart::plotcp(arvore)

# Curva ROC
pROC_arvore_test <- pROC::roc(as.numeric(test$DEATH_EVENT),as.numeric(classTest),
                              smoothed = TRUE,
                              # arguments for ci
                              ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                              # arguments for plot
                              plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                              print.auc=TRUE, show.thres=TRUE)

pROC_arvore_test$auc