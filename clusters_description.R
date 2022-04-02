### Description des clusters ### 
list.of.packages <- c( "wesanderson","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
invisible(lapply(list.of.packages, library, character.only = TRUE))
pacman::p_load(sentimentr, dplyr, magrittr)

data2 <- cbind(clusters, data)
sapply(data2, class)
data2$clusters <- as.factor(data2$clusters)

### Analyse descriptive par cluster ###
tapply(data2$age, data2$clusters, summary)
tapply(data2$sex, data2$clusters, summary)
tapply(data2$orientation, data2$clusters, summary)
tapply(data2$drinks, data2$clusters, summary)
tapply(data2$drugs, data2$clusters, summary)
tapply(data2$height, data2$clusters, summary)
tapply(data2$sign, data2$clusters, summary)
tapply(data2$number_languages, data2$clusters, summary)
tapply(data2$smokes, data2$clusters, summary)
tapply(data2$kids_friendly, data2$clusters, summary)
tapply(data2$white, data2$clusters, summary)
tapply(data2$cat_diet, data2$clusters, summary)
tapply(data2$religious, data2$clusters, summary)
tapply(data2$graduate, data2$clusters, summary)
tapply(data2$loves_movies, data2$clusters, summary)
tapply(data2$age, data2$clusters, summary)

#### Graphiques des variables qualitatives ####

#Orientation
ggplot(data2,  aes(x = clusters,  fill = orientation),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition de l'orientation sexuelle par cluster")

#Genre
ggplot(data2,  aes(x = clusters,  fill = sex),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du genre par cluster")

#drinks
ggplot(data2,  aes(x = clusters,  fill = drinks),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait de boire de l'alcool par cluster")

#smokes
ggplot(data2,  aes(x = clusters,  fill = smokes),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait de fumer par cluster")

#drugs
ggplot(data2,  aes(x = clusters,  fill = drugs),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait de prendre des drogues par cluster")

#sign
ggplot(data2,  aes(x = clusters,  fill = sign),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack", width = 0.8) +
  scale_fill_manual(values = ochre_palettes$emu_woman_paired )+ 
  ggtitle("Répartition des signes astrologiques par cluster")

ggplot(data2,  aes(x = clusters,  fill = sign),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack", width = 0.8) +
  scale_fill_manual(values = c("antiquewhite", "aliceblue","darkgoldenrod","black","cadetblue","darkolivegreen3","aquamarine4","chocolate4","antiquewhite4","darkslategrey","darkgoldenrod1","darkseagreen1","cyan4"))+ 
  ggtitle("Répartition des signes astrologiques par cluster")

#cat
ggplot(data2,  aes(x = clusters,  fill = cat),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition de l'appréciation des chats par cluster")

#dog
ggplot(data2,  aes(x = clusters,  fill = dog),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition de l'appréciation des chiens par cluster")

#kids_friendly
ggplot(data2,  aes(x = clusters,  fill = kids_friendly),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait d'avoir ou vouloir des enfants par cluster")

#white
ggplot(data2,  aes(x = clusters,  fill = white),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait d'avoir déclaré être blanc par cluster")

#cat_diet
ggplot(data2,  aes(x = clusters,  fill = cat_diet),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du régime alimentaire par cluster")

#religious
ggplot(data2,  aes(x = clusters,  fill = religious),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition de la religion par cluster")

#graduated
ggplot(data2,  aes(x = clusters,  fill = graduate),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait d'être diplomé par cluster")

#loves_music
ggplot(data2,  aes(x = clusters,  fill = loves_music),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait d'aimer la musique par cluster")

#loves_books
ggplot(data2,  aes(x = clusters,  fill = loves_books),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait d'aimer les livres par cluster")

#loves_food
ggplot(data2,  aes(x = clusters,  fill = loves_food),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait d'aimer la nourriture par cluster")

#loves_movies
ggplot(data2,  aes(x = clusters,  fill = loves_movies),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait d'aimer les films par cluster")

#friends
ggplot(data2,  aes(x = clusters,  fill = friends),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait que les amis soient importants par cluster")

#family
ggplot(data2,  aes(x = clusters,  fill = family),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du fait que la famille soit importante par cluster")

#Nombre langues parlées
data2$number_languages <- as.factor(data2$number_languages)
ggplot(data2,  aes(x = clusters,  fill = number_languages),color = wes_palettes$Darjeeling2) + 
  geom_bar(position = "stack") +
  scale_fill_manual(values = wes_palettes$Darjeeling2 )+ 
  ggtitle("Répartition du nombre de langues parlées par cluster")

#### Graphiques des variables continues ####

#age
ggplot(data2, 
       aes(x = age, 
           fill = clusters)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution des ages par cluster")

#description_sentiment
ggplot(data2, 
       aes(x = description_sentiment, 
           fill = clusters)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution des sentiments par cluster")

#height
ggplot(data2, 
       aes(x = height, 
           fill = clusters)) +
  geom_density(alpha = 0.4) +
  labs(title = "Distribution des taille par cluster")






