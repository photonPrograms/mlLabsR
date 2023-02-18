install.packages("ISLR")
install.packages("rpart")
library(ISLR)
library(rpart)

# loading the data
data(Carseats)
hist(Carseats$Sales)

# converting quantitative variable Sales
# to a binary target variable
high = ifelse(Carseats$Sales <= 8, "No", "Yes")
high
carseats = data.frame(Carseats, high)
carseats

# fitting a decision tree
tree.carseats = rpart(high ~ .-Sales, data = carseats)
summary(tree.carseats)
plot(tree.carseats, uniform = TRUE)
text(tree.carseats, pretty = 1)

# examine the complexity plot
printcp(tree.carseats)
plotcp(tree.carseats)

# prepruning
prepruneTree.carseats = rpart(
  high ~ .-Sales, data = carseats,
  method = "class",
  control = rpart.control(
    cp = 0, maxdepth = 4, minsplit = 100
  )
)
plot(prepruneTree.carseats, uniform = TRUE)
text(prepruneTree.carseats, pretty = 1)

# postpruning
postpruneTree.carseats = prune(tree.carseats, cp = 0.06)
plot(postpruneTree.carseats, uniform = TRUE)
text(postpruneTree.carseats, pretty = 1)