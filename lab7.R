# library imports
library(ggplot2)
library(MASS)
library(mvtnorm)
library(e1071)

# variance covariance matrix for
# random bivariate gaussian sample
varCovar = matrix(
  data = c(1.5, 0.3, 0.3, 1.5),
  nrow = 2
)

# random bivariate gaussian samples
# for class = +1 and class = -1
xPlus1 = rmvnorm(
  400, mean = c(6, 6), sigma = varCovar
)
xMinus1 = rmvnorm(
  600, mean = c(2, 2), sigma = varCovar
)

# samples for the dependent variable
ySamples = c(rep(1, 400), rep(-1, 600))

# combining the independent and dependent variables
# into a dataframe
dataset = as.data.frame(
  cbind(rbind(xPlus1, xMinus1), ySamples)
)
colnames(dataset) = c("x1", "x2", "y")
dataset$y = as.character(dataset$y)
dataset

# plot the sample and color by class labels
ggplot(data = dataset) + 
geom_point(aes(x1, x2, color = y))

# train lda model
ldaModel <- lda(y ~ x1 + x2, data = dataset)
ldaModel

# predicting class for each sample
yPred = predict(ldaModel, newdata = dataset)$class

# adding the predictions to dataframe
dataset$lda_pred = as.character(yPred)

# plot the above samples and
# color by actual and predicted class labels
dataset$y_actual_pred = paste(
  dataset$y, dataset$lda_pred, sep = ","
)
ggplot(data = dataset) + 
geom_point(aes(x1, x2, color = y_actual_pred))

# quadratic discriminant analysis
qdaModel = qda(y ~ x1 + x2, data = dataset)
qdaModel

# predicting class for each sample
yPred = predict(qdaModel, newdata = dataset)$class

# adding the predictions to dataframe
dataset$qda_pred = as.character(yPred)

# plot the above samples and
# color by actual and predicted class labels
dataset$y_actual_pred = paste(
  dataset$y, dataset$qda_pred, sep = ","
)
ggplot(data = dataset) + 
  geom_point(aes(x1, x2, color = y_actual_pred))