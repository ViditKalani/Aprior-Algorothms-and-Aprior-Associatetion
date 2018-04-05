library(tidyverse)
library(caret)
library(datasets)
# str(Titanic)
# df <- as.data.frame(Titanic)
# head(df)

test <- read.csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 5610/Assignment_3/test.csv", header=FALSE)
train <- read.csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 5610/Assignment_3/train.csv", header=FALSE)
#titanic <- read.csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 5610/Assignment_3/titanic.csv", header=FALSE)
names(train) <- c("PassengerId",
                  "Survived",
                  "Pclass",
                  "Name",
                  "Sex",
                  "Age",
                  "SibSp",
                  "Parch",
                  "Ticket",
                  "Fare",
                  "Cabin",
                  "Embarked")     
train[-c(1),]
train <- train[-c(1),]

names(test) <- c("PassengerId",
                "Pclass",
                "Name",
                "Sex",
                "Age",
                "SibSp",
                "Parch",
                "Ticket",
                "Fare",
                "Cabin",
                "Embarked")
test[-c(1),]
test <- test[-c(1),]

# titanic.raw <- NULL
# for(i in 1:4) {
#   titanic.raw <- cbind(titanic.raw, rep(as.character(df[,i]), df$Freq))
# }
# titanic.raw <- as.data.frame(titanic.raw)
# names(titanic.raw) <- names(df)[1:4]
# dim(titanic.raw)
# str(titanic.raw)
# head(titanic.raw)
# summary(titanic.raw)

library(arules)
library(arulesViz)

#detach("package:arules", unload=TRUE)
library("arules", lib.loc="[wherever your R libraries go]")



# find association rules with default settings
rules.all <- apriori(test)

rules.all
inspect(rules)



# find association rules with default settings
rules.all <- apriori(train)

rules.all
inspect(rules.all)


# rules with rhs containing "Survived" only
rules <- apriori(train, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=0", "Survived=1"),
                                   default="lhs"))

# keep three decimal places
quality(rules) <- round(quality(rules), digits=3)


# order rules by lift
rules.sorted <- sort(rules, by="lift")
rules.sorted <- sort(rules, by="confidence")

inspect(rules.sorted)


# rules with rhs containing "Survived" only
rules <- apriori(test, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Pclass=1", "Pclass=2", "Pclass=3"),
                                   default="lhs"))
quality(rules) <- round(quality(rules), digits=3)
rules.sorted <- sort(rules, by="lift")
rules.sorted <- sort(rules, by="confidence")
rules.all <- apriori(test)

inspect(rules.sorted[1:2])

rules_lift <- sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift)) 
rules_conf <- sort(rules, by = "confidence", decreasing = TRUE)
inspect(head(rules_conf))

# find redundant rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)
which(redundant)
rules.pruned <- rules.sorted[!redundant]
inspect(rules.pruned)
inspect(rules.pruned[1])


# rules <- apriori(test, control = list(verbose=F),
#                  parameter = list(minlen=3, supp=0.002, conf=0.2),
#                  appearance = list(default="none", rhs=c("Survived=Yes"),
#                                    lhs=c("Pclass=1", "Pclass=2", "Pclass=3",
#                                          )))
# rules.sorted <- sort(rules, by="confidence")
# inspect(rules.sorted)
# 
# rules <- apriori(train, control = list(verbose=F),
#                  parameter = list(minlen=3, supp=0.002, conf=0.2),
#                  appearance = list(default="none", rhs=c("Survived=Yes"),
#                                    lhs=c("Survived=0", "Survived=1",
#                                          default="lhs")))
# rules.sorted <- sort(rules, by="confidence")
# inspect(rules.sorted)

library(arulesViz)
plot(rules.all)
plot(rules.all, method="grouped")
plot(rules.all, method="graph")
plot(rules.all, method="graph", control=list(type="items"))
plot(rules.all, method="paracoord", control=list(reorder=TRUE))







