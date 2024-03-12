set.seed(2024)
# Carregando base de dados
PastaProjeto <- "/Users/danielbaldini//Documents/MBA/TCC/TCC_Arquivos/Projeto/"
df <- read.csv(paste0(PastaProjeto, "dataset/heart_failure_clinical_records_dataset.csv"))
#Preparando base de dados
df$age <- as.integer(df$age)
df$anaemia <- as.factor(df$anaemia)
df$diabetes <- as.factor(df$diabetes)
df$high_blood_pressure <- as.factor(df$high_blood_pressure)
df$sex <- as.factor(df$sex)
df$smoking <- as.factor(df$smoking)
df$DEATH_EVENT <- as.factor(df$DEATH_EVENT)

df_bkp <- df

#Visualizando a base de dados
head(df) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 7)

#Visualisando as estatísticas basicas
summary(df[colnames(df)][1:6])
summary(df[colnames(df)][7:13])
summary(df[c("anaemia","diabetes","high_blood_pressure","sex","smoking","DEATH_EVENT")])

