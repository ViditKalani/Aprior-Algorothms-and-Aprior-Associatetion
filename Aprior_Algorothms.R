library(tidyverse)
library(caret)
data(Titanic)
data("Titanic") 
str(Titanic)

test <- read.csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 5610/Assignment_3/test.csv", header=FALSE)
train <- read.csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 5610/Assignment_3/train.csv", header=FALSE)
train <- as.tibble(train)


titanic <- as.data.frame(Titanic)

titanic.df <- NULL # novo objeto para o dataframe pre-processado

for (i in 1:4) { # faz o bind de todas as 4 primeiras colunas do dataframe titanic
  titanic.df <- cbind(titanic.df, rep(as.character(titanic[,i]), titanic$Freq))
}

titanic.df <- as.data.frame(titanic.df) # converte em dataframe
names(titanic.df) <- names(titanic)[1:4]

library(ggplot2)
library(gridExtra)

summary(titanic.df)

plot1 <- suppressWarnings(ggplot(titanic.df, aes(Class, fill=Class)) + geom_histogram(stat="count") +
                            labs(title="Proporção de pessoas \npor classe", x="Classes", y="Quantidade de pessoas"))

plot2 <- suppressWarnings(ggplot(titanic.df, aes(Sex, fill=Sex)) + geom_histogram(stat="count") +
                            labs(title="Proporção de pessoas \npor sexo", x="Sexo", y="Quantidade de pessoas"))

plot3 <- suppressWarnings(ggplot(titanic.df, aes(Age, fill=Age)) + geom_histogram(stat="count") +
                            labs(title="Proporção de pessoas \npor classificação etária", x="Classificação", y="Quantidade de pessoas", size=8))

plot4 <- suppressWarnings(ggplot(titanic.df, aes(Survived, fill=Survived)) + geom_histogram(stat="count") +
                            labs(title="Proporção de pessoas \n(sobreviveram ou não)", x="Sobreviveu", y="Quantidade de pessoas"))

grid.arrange(plot1, plot2, plot3, plot4, nrow=2,ncol=2)


library(lattice)
histogram(~Survived | Class, data=titanic.df, main="Sobreviventes por classe", ylab="Porcentagem do total")


h1 <- histogram(~Survived | Sex, data=titanic.df, col="green")
h2 <- histogram(~Survived | Age, data=titanic.df, col="blue")
grid.arrange(h1,h2,nrow=1, ncol=2)

histogram(~Survived | Age + Sex, data=titanic.df)

library("arules")
rules <- apriori(titanic.df, 
                  parameter = list(minlen=3, supp=0.002, conf=0.2),
                  appearance = list(rhs=c("Survived=Yes", "Survived=No"),
                                    lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                          "Age=Child", "Age=Adult"),
                                    default="none"), 
                  control = list(verbose=F))
quality(rules) <- round(quality(rules),4)
rules.sorted <- sort(rules, by="confidence")
inspect(rules.sorted)

#install.packages("arulesViz")
library(arulesViz)
rules <- apriori(titanic.df, 
                  parameter = list(minlen=3, supp=0.002, conf=0.5),
                  appearance = list(rhs=c("Survived=Yes", "Survived=No"),
                                    lhs=c("Class=1st", "Class=2nd", "Class=3rd",
                                          "Age=Child", "Age=Adult"),
                                    default="none"), 
                  control = list(verbose=F))
quality(rules) <- round(quality(rules),4)
plot(rules, method="graph")

inspect(rules)
str(titanic)
titanic <- sapply(titanic,as.factor)

df <- data.frame(y = sample(0:1, 26, rep=T), x1=runif(26), x2=letters, stringsAsFactors=F)

df$y <- as.factor(df$y)

library(randomForest) 
#randomForest(y ~ x1 + x2, data=df)
#fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age_Bucket + Embarked
#                    + Age_Bucket + Fare_Bucket + F_Name + Title + FamilySize + FamilyID, 
#                   data=train, importance=TRUE, ntree=5000)

#trans <- as(titanic,"Survived")

#rules <- apriori(titanic)
#trans = read.transactions("Titanic.df", format = "single", sep = ",", cols = c("transactionID", "productID"))


rules <- apriori(titanic.df,
                  parameter = list(minlen=2, supp=0.005, conf=0.8),
                  appearance = list(rhs=c("Survived = No", "Survived = Yes"),
                  default="lhs"),
                  control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")

inspect(rules.sorted)

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

# remove redundant rules
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)

library(arulesViz)
plot(rules)

plot(rules, method="graph", control=list(type="items"))

plot(rules, method="paracoord", control=list(reorder=TRUE))















