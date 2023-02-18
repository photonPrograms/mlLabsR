# library imports
library(e1071)
library(caret)

# load the iris dataset
data(iris)
summary(iris)
head(iris)

# apply pca on iris dataset and choose 2 comps
pc <- prcomp(iris[, -5], center = T, scale. = T)
pc1 <- data.frame(pc$x[, 1])
pc2 <- data.frame(pc$x[, 2])

# partition the data into training and testing
ind <- sample(
  2, nrow(pc1), replace = TRUE,
  prob = c(0.8, 0.2)
)
traindata1 <- pc1[ind == 1, ]
traindata2 <- pc2[ind == 1, ]
trainclass <- data.frame(iris[ind == 1, 5])
testdata1 <- pc1[ind == 2, ]
testdata2 <- pc2[ind == 2, ]
testclass <- data.frame(iris[ind == 2, 5])

# rebuild the data in list form
data <- data.frame(
  class = unlist(trainclass),
  x = unlist(traindata1),
  y = unlist(traindata2)
)
data

# rbf kernel
model1 <- svm(
  class ~ x + y, data = data,
  kernel = "radial"
)
summary(model1)
plot(model1, data)

# polynomial kernel
model2 <- svm(
  class ~ x + y, data = data,
  kernel = "polynomial"
)
summary(model2)
plot(model2, data)

# sigmoid kernel
model3 <- svm(
  class ~ x + y, data = data,
  kernel = "sigmoid"
)
summary(model3)
plot(model3, data)

# predictions
datatest <- data.frame(
  x = unlist(testdata1), y = unlist(testdata2)
)
pred1 = predict(model1, datatest)
pred1
confusionMatrix(pred1, testclass[, 1])
pred2 = predict(model2, datatest)
confusionMatrix(pred2, testclass[, 1])
pred3 = predict(model3, datatest)
confusionMatrix(pred3, testclass[, 1])