setwd("/Users/ajkoma/Desktop/U of R/Linguistic/Project 4")
data <- read.csv('noun_distributional_information.csv')
ncol(data)
colname <- colnames(data)
library(FactoMineR)
library(randomForest)
library(party)
library(stringr)
library(Rling)
library(tidyverse)
library(factoextra)
a <- str_extract_all(string = colname,pattern='.*Percentage|.*Avg$',simplify = TRUE)
b <- which(a == '')
a <- a[-b]
colpercent <- subset(data,select = a)
col.pca <- PCA(colpercent)
plot(col.pca, choix = "var", cex = 0.5)


colnames(colpercent) 
newtag <- paste(data$countable,data$uncountable)
colpercent$countable <- factor(newtag)
singletable <- paste(data$singularia,data$pluralia)
singletable
colpercent$singulara <- factor(singletable)

colpercent <- colpercent[-c(55:104)]
col.pca <- subset(colpercent,select = -c(countable,singulara))
col.pca <- PCA(col.pca)

 
plot(col.pca, choix = "var", cex = 0.4)

colnames(colpercent)
set.seed(35)
colpercent_ <-colpercent[c(1:54)]
colpercent_$tag <- paste(colpercent$countable,colpercent$singulara)
colpercent_$tag <- factor(colpercent_$tag)
countable.model <- subset(colpercent,select=-c(singulara))
single.model <- subset(colpercent,select=-c(countable))
countablemodel <- randomForest(countable ~ ., data=countable.model,importance = TRUE, proximity = FALSE, ntree = 1000)
singularamodel <- randomForest(singulara ~ ., data=single.model,importance = TRUE, proximity = FALSE, ntree = 1000)
mixedmodel <- randomForest(tag ~ ., data=colpercent_,importance = TRUE, proximity = FALSE, ntree = 1000)
print(countablemodel) 
plot(countablemodel)
plot(mixedmodel)
importance(countablemodel,type=1)
importance(singularamodel,type=1)
importance(mixedmodel,type =1)
varImpPlot(countablemodel)
varImpPlot(singularamodel)
varImpPlot(mixedmodel)

summary(countablemodel)
library("party")
x <- randomForest(countable ~ Prepositional.Phrase.Percentage + Bare.Singular.Noun.Percentage +
             Determiner.Percentage + Compound.Percentage + Prepositional.Subject.Percentage +
             Linked.Noun.Percentage + Indefinite.Article.Percentage + Definite.Article.Percentage + Prepositional.Object.Percentage+
             Conjoined.Percentage+Adjective.Percentage+Plural.Noun.Percentage+Prepositional.Object.Percentage+
               Adjective.Percentage+Verb.Construction.Percentage+Adjtype..Miscellaneous.Avg+
               Verb.Subject.Percentage+NN.Percentage+Verb.Object.Percentage+Possesive.Percentage+
               Adjtype..Mind.Avg+Possesed.Percentage+Singular.Noun.Percentage+NNS.Percentage+VB.Percentage+
               Adjtype..Social.Avg+Adjtype..Quantity.Avg+Adjtype..Perception.Avg+Bare.Plural.Noun.Percentage+
               Adjtype..Feeling.Avg+Adjtype..Behavior.Avg+ Adjtype..Spatial.Avg , data=colpercent,importance=TRUE,proximity = FALSE, ntree = 500)
plot(x)
library("ggplot2")
library("ggRandomForests")
############









causcountable.split <- split(colpercent, colpercent$countable)
causcountable.split <- as.matrix(as.data.frame(lapply(causcountable.split , as.numeric)))


colpercent <- as.matrix(as.data.frame(lapply(colpercent, as.numeric)))
fit <- kmeans(colpercent,5)
d <- dist(colpercent,method = "euclidean")




















causcountable.split <- split(colpercent, colpercent$countable)
#causcountable.split < lapply(causcountable.split,function(x) x = x[,-1])

str(causcountable.split)

#causcountable.bp1 <- do.call(rbind,causcountable.bp)


caus.dict <- dist(causcountable.split)
max(caus.dict)
min(caus.dict)
caus.hc<- hclust(caus.dict,method="complete")
plot(caus.hc,hang = -1)

dim(colpercent)
install.packages(c("cluster","pvclust","vcd","Rling"))
library("cluster")
library("pvclust")
library("vcd")
caus.dict <- dist(colpercent,method = "canberra")
max(caus.dict)
min(caus.dict)
caus.hc<- hclust(caus.dict,method="complete")
plot(caus.hc,hang = -1)
##########
colnamepercentage <- colnames(colpercent)
a <- str_extract_all(string = colnamepercentage,pattern='VB.*', simplify = TRUE)
b <- which(a == '')
c <- a[-b]
c
colpercentVB <- subset(colpercent,select = c)
#colpercentVB <- cbind(colpercentVB,data[c("countable","uncountable","singularia","pluralia","collect")])


library("cluster")
library("pvclust")
library("vcd")
causVB.dict <- dist(as.matrix(colpercentVB))
max(caus.dict)
min(caus.dict)
caus.hc<- hclust(caus.dict,method="ward.D2")
plot(caus.hc,hang = -1)

test.clust <- cutree(caus.hc,k=15)
test.clust
##########
a <- str_extract_all(string = colnamepercentage,pattern='Adj.*|.*Avg',simplify = TRUE)
b <- which(a == '')

a <- a[-b]
colpercentAdj <- subset(data,select = a)
colpercentAdj <- cbind(colpercentVB,data[c("countable","uncountable","singularia","pluralia","collect")])

library("cluster")
library("pvclust")
library("vcd")
caus.dict <- dist(colpercentAdj,method = "canberra")
max(caus.dict)
min(caus.dict)
caus.hc<- hclust(caus.dict,method="complete")
plot(caus.hc,hang = -1)
