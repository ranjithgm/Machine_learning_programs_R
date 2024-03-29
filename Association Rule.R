#Association Rule

library(arules)
library(arulesViz)
data("Groceries")
summary(Groceries)

apriori(Groceries,parameter = list(support=0.002, confidence=0.5)) -> rule1

inspect(head(rule1))
inspect(head(sort(rule1,by="lift"),5))

plot(rule1)
plot(rule1, method="grouped")

apriori(Groceries,parameter = list(support=0.002, confidence=0.5,minlen=5))->rule2
inspect(head(rule2,4))
plot(rule2, method="grouped")

apriori(Groceries,parameter = list(support=0.007, confidence=0.6))->rule3
inspect(head(rule3,4))
plot(rule3, method="grouped")

########################################