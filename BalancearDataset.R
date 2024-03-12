dfX <- as.data.frame(df[,1:12])
summary(dfX)
dfY <- df[,13]
summary(dfY)
count(dfX)
length(dfY)

resultsDummyX <- fastDummies::dummy_cols(dfX, remove_selected_columns = T, remove_first_dummy = T, omit_colname_prefix = T)

#rebalance the training set before building a model
#balanced <- ubBalance(X=resultsDummyX, Y=dfY, type="ubOSS") #, percOver=200, percUnder=150)
balanced <- ubBalance(X=resultsDummyX, Y=dfY, type="ubSMOTE", percOver=200, percUnder=150)
df_balanced <- data.frame(balanced$X, DEATH_EVENT=balanced$Y)

names(df_balanced)[names(df_balanced) == "anaemia_1"] <- "anaemia"
names(df_balanced)[names(df_balanced) == "diabetes_1"] <- "diabetes"
names(df_balanced)[names(df_balanced) == "high_blood_pressure_1"] <- "high_blood_pressure"
names(df_balanced)[names(df_balanced) == "sex_1"] <- "sex"
names(df_balanced)[names(df_balanced) == "smoking_1"] <- "smoking"

col_order <- colnames(df)
df_balanced <- df_balanced[, col_order]

#Atualiza DF com informação de se a observação está no DF balanceado - O balanceamento tira death = 0
#df$inBalanced <- do.call(paste0, df) %in% do.call(paste0, df_balanced)

#df_balanced_scaled <- df_balanced
#df_balanced_scaled$creatinine_phosphokinase <- scale(df_balanced$creatinine_phosphokinase)
#df_balanced_scaled$ejection_fraction <- scale(df_balanced$ejection_fraction)
#df_balanced_scaled$platelets <- scale(df_balanced$platelets)
#df_balanced_scaled$serum_creatinine <- scale(df_balanced$serum_creatinine)
#df_balanced_scaled$serum_sodium <- scale(df_balanced$serum_sodium)
#df_balanced_scaled$time <- scale(df_balanced$time)



