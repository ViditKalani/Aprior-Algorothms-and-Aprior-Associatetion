library(tidyverse)
library(caret)

titanic <- read.csv("/Users/vidit/Desktop/Masters/Sem_3/MSCS 5610/Assignment_3/titanic.csv", header=FALSE)
names(titanic) <- c("Survived",
                    "Pclass",
                    "Name",
                    "Sex",
                    "Age",
                    "Siblings/Spouses Aboard",
                    "Parents/Children Aboard",
                    "Fare")
titanic[-c(1),]
titanic <- titanic[-c(1),]

library(arules)
library(arulesViz)
library("arules", lib.loc="[wherever your R libraries go]")


# find association rules with default settings
rules.all <- apriori(titanic)

rules.all
inspect(rules.all)

# rules with rhs containing "Survived" only
rules <- apriori(titanic, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=0", "Survived=1"),
                                   default="lhs"))

# keep three decimal places
quality(rules) <- round(quality(rules), digits=3)


# order rules by lift
rules.sorted0 <- sort(rules, by="lift")
rules.sorted1 <- sort(rules, by="confidence")
inspect(rules.sorted1[1:5])

inspect(rules.sorted1)

rules <- apriori(titanic, control = list(verbose=F),
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Pclass=1", "Pclass=2", "Pclass=3"),
                                   default="lhs"))

quality(rules) <- round(quality(rules), digits=3)

rules.sorted2 <- sort(rules, by="lift")
rules.sorted3 <- sort(rules, by="confidence")

rules.all <- apriori(titanic)


inspect(rules.sorted3[1:2])

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
View(rules.pruned)

plot(rules.all)
plot(rules.all, method="grouped")
plot(rules.all, method="graph")
plot(rules.all, method="graph", control=list(type="items"))
plot(rules.all, method="paracoord", control=list(reorder=TRUE))


library(arulesViz)
plot(rules.all)
plot(rules_conf, method="grouped")
plot(rules.all, method="graph")
plot(rules.all, method="graph", control=list(type="items"))
plot(rules.all, method="paracoord", control=list(reorder=TRUE))

















