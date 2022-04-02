### Exploration des variables d'essay : ###

list.of.packages <- c("tidytext","ggplot2","dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))

data <- read.csv("okcupid_profiles.csv")

##### FREQUENCE MOTS #####

#Text cleaning
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
#Function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z]", " ", x)

####ESSAY1
#Fix (expand) contractions
data$essay1 <- sapply(data$essay1, fix.contractions)
#Remove special characters
data$essay1 <- sapply(data$essay1, removeSpecialChars)
#Convert everything to lower case
data$essay1 <- sapply(data$essay1, tolower)
head(data$essay1)
data_essay1 <- data %>%
  unnest_tokens(word, essay1) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay1[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les résumés descriptifs des individus") 

####ESSAY2
#Fix (expand) contractions
data$essay2 <- sapply(data$essay2, fix.contractions)
#Remove special characters
data$essay2 <- sapply(data$essay2, removeSpecialChars)
#Convert everything to lower case
data$essay2 <- sapply(data$essay2, tolower)
head(data$essay2)
data_essay2 <- data %>%
  unnest_tokens(word, essay2) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay2[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les aspirations personnelles des individus") 

####ESSAY3
#Fix (expand) contractions
data$essay3 <- sapply(data$essay3, fix.contractions)
#Remove special characters
data$essay3 <- sapply(data$essay3, removeSpecialChars)
#Convert everything to lower case
data$essay3 <- sapply(data$essay3, tolower)
head(data$essay3)
data_essay3 <- data %>%
  unnest_tokens(word, essay3) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay3[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les traits de caractères des individus") 

####ESSAY4
#Fix (expand) contractions
data$essay4 <- sapply(data$essay4, fix.contractions)
#Remove special characters
data$essay4 <- sapply(data$essay4, removeSpecialChars)
#Convert everything to lower case
data$essay4 <- sapply(data$essay4, tolower)
head(data$essay4)
data_essay4 <- data %>%
  unnest_tokens(word, essay4) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay4[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les passions des individus") 

####ESSAY5
#Fix (expand) contractions
data$essay5 <- sapply(data$essay5, fix.contractions)
#Remove special characters
data$essay5 <- sapply(data$essay5, removeSpecialChars)
#Convert everything to lower case
data$essay5 <- sapply(data$essay5, tolower)
head(data$essay5)
data_essay5 <- data %>%
  unnest_tokens(word, essay5) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay5[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les hobbies des individus") 

####ESSAY6
#Fix (expand) contractions
data$essay6 <- sapply(data$essay6, fix.contractions)
#Remove special characters
data$essay6 <- sapply(data$essay6, removeSpecialChars)
#Convert everything to lower case
data$essay6 <- sapply(data$essay6, tolower)
head(data$essay6)
data_essay6 <- data %>%
  unnest_tokens(word, essay6) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay6[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans un moment ou une journée parfaite") 

####ESSAY7
#Fix (expand) contractions
data$essay7 <- sapply(data$essay7, fix.contractions)
#Remove special characters
data$essay7 <- sapply(data$essay7, removeSpecialChars)
#Convert everything to lower case
data$essay7 <- sapply(data$essay7, tolower)
head(data$essay7)
data_essay7 <- data %>%
  unnest_tokens(word, essay7) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay7[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les besoins des individus") 

####ESSAY8
#Fix (expand) contractions
data$essay8 <- sapply(data$essay8, fix.contractions)
#Remove special characters
data$essay8 <- sapply(data$essay8, removeSpecialChars)
#Convert everything to lower case
data$essay8 <- sapply(data$essay8, tolower)
head(data$essay8)
data_essay8 <- data %>%
  unnest_tokens(word, essay8) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay8[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les choses privées des individus") 

####ESSAY9
#Fix (expand) contractions
data$essay9 <- sapply(data$essay9, fix.contractions)
#Remove special characters
data$essay9 <- sapply(data$essay9, removeSpecialChars)
#Convert everything to lower case
data$essay9 <- sapply(data$essay9, tolower)
head(data$essay9)
data_essay9 <- data %>%
  unnest_tokens(word, essay9) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Le nbr de mots qui apparait le plus
ggplot(data_essay9[1:25,], aes(reorder(word, n), n)) + 
  geom_bar(stat = "identity", fill = "darkgoldenrod3") +
  geom_text(aes(label= as.character(n)), check_overlap = TRUE, size = 4) + 
  coord_flip() + 
  xlab(" ") + 
  ylab("Volume") + 
  labs(title = "25 mots les plus récurrents dans les attentes amoureuses des individus") 
