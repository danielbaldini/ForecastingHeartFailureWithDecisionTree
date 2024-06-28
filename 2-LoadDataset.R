set.seed(2024)
# Dataset source: https://archive.ics.uci.edu/dataset/519/heart+failure+clinical+records
# Loading Dataset
df <- read.csv("dataset/heart_failure_clinical_records_dataset.csv")

#Manipulating Data Types
df$age <- as.integer(df$age)
df$anaemia <- as.factor(df$anaemia)
df$diabetes <- as.factor(df$diabetes)
df$high_blood_pressure <- as.factor(df$high_blood_pressure)
df$sex <- as.factor(df$sex)
df$smoking <- as.factor(df$smoking)
df$DEATH_EVENT <- as.factor(df$DEATH_EVENT)

#Creating a backup just-in-case
df_bkp <- df

#Glimpse on the database
head(df) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 12)

#Basic statistics
summary(df[colnames(df)][1:6])
summary(df[colnames(df)][7:13])
summary(df[c("anaemia","diabetes","high_blood_pressure","sex","smoking","DEATH_EVENT")])

