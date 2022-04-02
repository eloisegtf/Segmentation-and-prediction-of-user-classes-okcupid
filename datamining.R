##Fancello Marie Clara
##Germini Eva
##Gutfreund Eloise

################################################################################
######################### PROJET DATA MINING ###################################
################################################################################

##########################################
######### Exploration de la Base #########
##########################################
rm(list=ls()) 
list.of.packages <- c( "wesanderson","ggplot2","psych","tidyr", "dplyr", "stringr","forcats","pacman", "FactoMineR", "factoextra","ggpubr","openxlsx","MASS","randomForest","rpart.plot","corrplot","Hmisc","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))
pacman::p_load(sentimentr, dplyr, magrittr)

#Define repertoire
setwd("/Users/fancellomarieclara/Downloads/")
getwd()

#Importe la base
data <- read.csv("okcupid_profiles.csv")
View(data)

#Nous regardons la taille de notre base
dim(data)

#Nous regardons le type de nos variables 
str(data)

#Nous regardons s'il existe des doublons dans la base :
sum(duplicated(data))

#Nous transformons les données manquantes en NA et regardons leur nombre par colonne 
data <- data %>% mutate_all(na_if,"")
na_pct <-sapply(data, function(data) sum(length(which(is.na(data)))/length(data))*100)
data.frame(na_pct)

#Nous separons la base entre les variables numerique et qualitative
num <- data[sapply(data,is.numeric)==T]


#### Traitement des variables numériques ####
#Nous regardons les statistiques descriptive des variables numeriques
summary(num)

#Boxplots des variables numeriques
for(x in seq(1,length(num),1)) boxplot(num[,x],xlab=names(num[x]),col=wes_palettes$Darjeeling2, main=names(num[x]))
boxplot(data$height~data$sex, col=wes_palettes$Darjeeling2, main = "Boxplot de la taille en fonction du genre")
#Nous supprimons le revenu 
data$income <- NULL

#Nous traitons les valeurs aberrantes de la taille : 
##crée les moyennes de taille par sexe
men <- subset(data, data$sex == "m")
women <- subset(data, data$sex == "f")
men_height <- mean(men$height, na.rm = TRUE)
women_height <- mean(women$height, na.rm = TRUE)
##remplace les outliers par les moyennes
data <- transform(data, height = ifelse(sex == "m" & height<=40, men_height,height))
data <- transform(data, height = ifelse(sex == "f"& height<=40, women_height, height))
##remplace les NA par les moyennes
data <- transform(data, height = ifelse(sex == "m" & is.na(data$height), men_height,height))
data <- transform(data, height = ifelse(sex == "f"& is.na(data$height), women_height, height))
##vérifie 
summary(data$height)

#Nous traitons les valeurs aberrantes de l'âge : 
data$age <- ifelse(data$age>100, median(data$age), data$age)
##vérifie 
summary(data$age)

##### Traitement des variables qualitatives ##### 
colnames(data)
#Nous imputons les données manquantes par le fait que les individus aient choisient de ne pas renseigner le champs
data$status <- fct_explicit_na(data$status, na_level = "didnt_answer")
data$orientation <- fct_explicit_na(data$orientation, na_level = "didnt_answer")
data$body_type <- fct_explicit_na(data$body_type, na_level = "didnt_answer")
data$diet <- fct_explicit_na(data$diet, na_level = "didnt_answer")
data$drinks <- fct_explicit_na(data$drinks, na_level = "didnt_answer")
data$drugs <- fct_explicit_na(data$drugs, na_level = "didnt_answer")
data$education <- fct_explicit_na(data$education, na_level = "didnt_answer")
data$ethnicity <- fct_explicit_na(data$ethnicity, na_level = "didnt_answer")
data$job <- fct_explicit_na(data$job, na_level = "didnt_answer")
data$location <- fct_explicit_na(data$location, na_level = "didnt_answer")
data$offspring <- fct_explicit_na(data$offspring, na_level = "didnt_answer")
data$pets <- fct_explicit_na(data$pets, na_level = "didnt_answer")
data$religion <- fct_explicit_na(data$religion, na_level = "didnt_answer")
data$sign <- fct_explicit_na(data$sign, na_level = "didnt_answer")
data$smokes <- fct_explicit_na(data$smokes, na_level = "didnt_answer")
data$speaks <- fct_explicit_na(data$speaks, na_level = "didnt_answer")

#Séparation des informations internes à certaines variables qualitatives
data<- data %>% 
  separate(education, into = c('edu', 'type_edu'), sep=" ")
data<- data %>% 
  separate(sign, into = c('sign', 'thoughts_astro'), sep=" ")
data<- data %>% 
  separate(location, into = c('city', 'state'), sep=",")
data<- data %>% 
  separate(job, into = c('job1', 'complementary_job'), sep=",")

colnames(data)
#Création d'une variable d'appréciation des animaux
data$cat <- as.factor(ifelse(grepl("likes cat",data$pets,fixed = T),"cat_friendly",
                             ifelse(grepl("didnt_answer",data$pets,fixed = T), "didnt_answer", 
                                    ifelse(grepl("has cat",data$pets,fixed = T), "cat_friendly", "not_cat_friendly"))))

data$dog <- as.factor(ifelse(grepl("likes dog",data$pets,fixed = T),"dog_friendly",
                             ifelse(grepl("didnt_answer",data$pets,fixed = T), "didnt_answer", 
                                    ifelse(grepl("has dog",data$pets,fixed = T), "dog_friendly", "not_dog_friendly"))))

#Création d'une variable sur la volonté d'avoir des enfants 
data$kids_friendly <- as.factor(ifelse(grepl("doesn't want",data$offspring,fixed = T),"not_kid_friendly",
                                        ifelse(grepl("didnt_answer",data$offspring,fixed = T), "didnt_answer", "kid_friendly")))

#Création des langues que l'on sélectionne
data$number_languages <- str_count(data$speaks, pattern = ",")
data$number_languages <- data$number_languages+1

#Création de l'éthnicité 
data$white <- as.factor(ifelse(grepl("white",data$ethnicity,fixed = T),"white",
                               ifelse(grepl("didnt_answer",data$ethnicity,fixed = T), "didnt_answer", "not_white")))
data$ethnicity <- NULL 

#Modification de la variable diet 
data$diet_anything <- ifelse(grepl("anything",data$diet,fixed = T),1,0)
data$diet_vegetarian <- ifelse(grepl("vegetarian",data$diet,fixed = T),1,0)
data$diet_vegan <- ifelse(grepl("vegan",data$diet,fixed = T),1,0)
data$diet_dontknow <- ifelse(grepl("didnt_answer",data$diet,fixed = T),1,0)

data$cat_diet <- as.factor(ifelse(data$diet_anything > 0, 'Anything',
                                 ifelse(data$diet_vegetarian > 0, 'Vegetarian/vegan', 
                                        ifelse(data$diet_vegan > 0, 'Vegetarian/vegan', 
                                               ifelse(data$diet_dontknow > 0, 'didnt_answer','other')))))
table(data$cat_diet)

#Crée la variable "religieux ou non" binaire (1 si religieux)
data$religious <- as.factor(ifelse(grepl("agnosticism",data$religion,fixed = T),"not_religious",
                                   ifelse(grepl("laughing",data$religion,fixed = T), "not_religious",ifelse(grepl("didnt_answer",data$religion,fixed = T),"didnt_answer", "religious"))))

#Crée la variable "fumer ou non" binaire (1 si fume)
data$smokes <- as.factor(ifelse(grepl("no",data$smokes,fixed = T),"no",
                               ifelse(grepl("didnt_answer",data$smokes,fixed = T), "didnt_answer", "yes")))

#Crée la variable "boire ou non" binaire (1 si bois)
data$drinks <- as.factor(ifelse(grepl("not at all",data$drinks,fixed = T),"no",
                               ifelse(grepl("didnt_answer",data$drinks,fixed = T), "didnt_answer", "yes")))

#Crée la variable "prendre des drogues ou non" binaire (1 si oui)
data$drugs <- as.factor(ifelse(grepl("never",data$drugs,fixed = T),"no",
                                ifelse(grepl("didnt_answer",data$drugs,fixed = T), "didnt_answer", "yes")))

#crée la variable graduate
data$graduate <- as.factor(ifelse(grepl("graduated",data$edu,fixed = T),"yes",
                               ifelse(grepl("didnt_answer",data$edu,fixed = T), "didnt_answer", "no")))


#Nous traitons les variables d'essay (se base sur l'exploration faite dans l'autre script R "datamining_exploration_mots_essay)
data$essay1 <- NULL
data$essay2 <- NULL
data$essay3 <- NULL
data$loves_music <- ifelse(grepl("music",data$essay4,fixed = T),1,0)
data$loves_movies <- ifelse(grepl("movies",data$essay4,fixed = T),1,0)
data$loves_food <- ifelse(grepl("food",data$essay4,fixed = T),1,0)
data$loves_books <- ifelse(grepl("books",data$essay4,fixed = T),1,0)
data$essay4 <- NULL
data$friends <- ifelse(grepl("friends",data$essay5,fixed = T),1,0)
data$family <- ifelse(grepl("family",data$essay5,fixed = T),1,0)
data$essay5 <- NULL
data$essay6 <- NULL
data$essay7 <- NULL
data$essay8 <- NULL
data$essay9 <- NULL 

#Crée la variable de sentiment sur la façon dont l'individu se décrit
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}
removeSpecialChars <- function(x) gsub("[^a-zA-Z]", " ", x)

data$essay0 <- sapply(data$essay0, fix.contractions)
data$essay0 <- sapply(data$essay0, removeSpecialChars)
data$essay0 <- sapply(data$essay0, tolower)


data$description_sentiment <- sentiment(data$essay0)$sentiment
head(data$description_sentiment)
#plot le boxplot des sentiments en fonction du genre
boxplot(data$description_sentiment~data$sex, col=wes_palettes$Darjeeling2, main = "Boxplot du sentiment de la description en fonction du genre")

summary(data$description_sentiment)

data$essay0 <- NULL 

#Nous supprimons les variables que nous n'utiliserons plus 
colnames(data)
drop <- c("diet_dontknow","edu","state","pets","diet_anything","diet_vegetarian","diet_vegan","diet_religious","city","diet", "offspring", "not_religious","laugh_religion","religion", "halal", "kosher", "type_edu","thoughts_astro","last_online","pets_dog_likes","pets_cat_likes","pets_cat_has","pets_dog_has", "no_kids", "job", "complementary_job", "speaks", "status", "job1", "body_type")
data <- data[,!(names(data) %in% drop)]
colnames(data)


#Nous modifions les types des variables n'ayant pas le bon type :
data <- data %>% mutate(across(where(is.character), as.factor))
data$number_languages <- as.factor(data$number_languages)
data$friends <- as.factor(data$friends)
data$family <- as.factor(data$family)
data$loves_food <- as.factor(data$loves_food)
data$loves_movies <- as.factor(data$loves_movies)
data$loves_books <- as.factor(data$loves_books)
data$loves_music <- as.factor(data$loves_music)

#Et faisons des analysons les variables
plot(data$sex, col = wes_palettes$Darjeeling2, main = "Nombre d'utilisateurs par genre")
plot(data$orientation, col = wes_palettes$Darjeeling2, main = "Nombre d'utilisateurs par orientation sexuelle")
plot(data$kids_friendly, col = wes_palettes$Darjeeling2, main = "Nombre d'uftilisateurs désirant ou ayant des enfants")
plot(data$number_languages, col = wes_palettes$Darjeeling2, main = "Nombre de langues parlées")
data$number_languages <- as.numeric(data$number_languages)
hist(data$description_sentiment, col = wes_palettes$Darjeeling2, main = "Distribution du sentiment associé à la description des individus")

write.csv(data,"datamining.csv", row.names = FALSE)

###### ACM ###### 
#Nous créons la base
data_acm <- data[sapply(data,is.factor)==T]
data_acm$sex <- NULL
colnames(data_acm)

#regarde l'occurence de chaque modalité dans les variables
table <- sapply(X = data_acm, FUN = table)
prop_table <- sapply(X=table, FUN = prop.table)

data_frame_prop <- prop_table

for (i in data_frame_prop) {
  data_frame_prop[i] <- data.frame(i)
}

write.xlsx(data_frame_prop, 'dataframe_prop.xlsx')

#Crée l'ACM 
res.mca <- MCA(data_acm,graph=FALSE,level.ventil=0.05)
eig <- get_eigenvalue(res.mca)
eig # on obtient 38 dimensions aux vues de toutes les varaibles + modalités des variables
#A partir de 26 dimensions, nous avons 80% d'inertie expliquée, et décidons dnoc de garder 69 dimensions
res.mca <- MCA(data_acm,graph=FALSE,level.ventil=0.05, ncp=26)
#Nous regardons les coordonnées des variables par dimensions
var <- res.mca$var
print(var)
var_mca <- data.frame(var$eta2)
#Nous exportons ces coordonnées pour les tests sur les membres du groupe 
write.xlsx(var_mca, 'var_mca.xlsx', rowNames = TRUE)
# Contributions des variables à les dimensions 1 et 2
fviz_contrib (res.mca, choice = "var", axes = 1, top = 15) #ne pas avoir renseigné son avis sur les chats
fviz_contrib (res.mca, choice = "var", axes = 3, top = 15) 
fviz_pca_var(res.mca,col.var="goldenrod3", axes = c(1,3))
#Nous récupérons les individus 
ind <- get_mca_ind(res.mca)
indiv <- data.frame(ind$coord)
View(indiv)

#Et venons joindre ces données aux variables numériques de la base 
numeric <- data[sapply(data,is.numeric)==T]
df <- data.frame(numeric,indiv)
head(df)

### CORRELATIONS ###
correlations=round(cor(df, use = "complete.obs",method="spearman"),2) #matrice avec les coefficients
corrplot(correlations) 
write.xlsx(correlations, 'correlations.xlsx')

### KMEANS ### 
#Nous regardons le nombre optimal de clusters 
#inertie=clus_between/clus_total
inertie.expl=rep(0,times=10)
for (k in 2:10){
  clus=kmeans(scale(df), centers=k)
  inertie.expl[k]=clus$betweenss/clus$totss
}
#graphique inertie expliqué
plot(1:10, inertie.expl,type="b",xlab="Nombre de groupes", main = "Nombre de groupes par inertie expliquée")

scaled_df <- scale(df)

sum(is.na(scaled_df))

set.seed(99)
groupes.kmeans <- kmeans(scaled_df, centers=6)

clusters <- groupes.kmeans$cluster

######### Crée un arbre de classification #########
clust_df <- cbind(clusters, df)
clust_df$clusters <- as.factor(clust_df$clusters)
#Nous séparons la base en train et test
trainDT=sample(1:nrow(clust_df),0.6*nrow(clust_df))
train=clust_df[trainDT,]
test=clust_df[-trainDT,]

set.seed(99)

model.rf<-randomForest(clusters~.,data=train)
print(model.rf)

#liste des variables importantes
varImpPlot(model.rf) 

#Regarde le nombre optimal d'arbres 
plot(model.rf$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB", main = "Nombre d'arbres par erreur OOB")
#l'erreur se stabilise à partir de 400 arbres, on garde donc 400 arbres 

#garde le mtry optimal 
set.seed(99)
model.rf<-randomForest(clusters~., ntree = 400, mtry = 4, data=train)
print(model.rf) 

p.rf<-predict(model.rf, newdata=test,type="class")
caret::confusionMatrix(p.rf,test$clusters)






