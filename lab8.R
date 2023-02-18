# OR logic
data = data.frame(
  x1 = c(0, 0, 1, 1),
  x2 = c(0, 1, 0, 1),
  y = c(0, 1, 1, 1)
)
print(data)

# plotting the data
plot(
  data$x1, data$x2, type = "n",
  main = "Dataset OR", xlab = "x1", ylab = "x2"
)
text(data$x1, data$x2, labels = data$y)
grid(
  nx = length(data$x1) + 1,
  ny = length(data$x1),
  col = "black"
)

# initial solution: x1 = 0.5
weights = c(-0.5, 1, 0)

# random initialization of weights within a short range
weights = rnorm(
  mean = 0,
  sd = 0.1,
  n = ncol(data)
)
print(weights)

# heavyside activation function
activationFunction = function (net) {
  if (net > 0.5)
    return(1)
  return(0)
}

# training with only the first example
data = as.matrix(data)
net = c(data[1, 1:2], 1) %*% weights
yHat = activationFunction(net)
error = yHat - data[1, 3]
eta = 0.1 # learning rate
weights = weights - eta * error * c(data[1, 1:2], 1)
print(weights)

# perceptron
perceptron = function(dataset, eta = 0.1, threshold = 1e-5) {
  data = as.matrix(dataset)
  num.features = ncol(data) - 1
  target = ncol(data)
  
  weights = rnorm(
    mean = 0, sd = 0.1, n = ncol(data)
  )
  mse = threshold * 2
  while (mse > threshold) {
    mse = 0
    for (i in 1: nrow(data)) {
      net = c(data[i, 1:num.features], 1) %*% weights
      yHat = activationFunction(net)
      error = yHat - data[i, target]
      mse = mse + error ^ 2
      cat(paste("Mean square error = ", mse, "\n"))
      weights = weights - eta * error * c(
        data[i, 1: num.features], 1
      )
    }
  }
  return(weights)
}

# charting the hyperplane
shattering.plane = function(weights) {
  X = seq(0, 1, length = 10)
  data = outer(X, X, function(X, Y) {
    cbind(X, Y, 1) %*% weights
  })
  id = which(data > 0.5)
  data[id] = 1
  data[-id] = 0
  filled.contour(data)
}

# using the perceptron
weights = perceptron(data, eta = 0.1, threshold = 1e-5)
print(weights)
shattering.plane(weights)

########################################
##  BINARY CLASSIFICATION WITH IRIS  ##
#######################################

# load the iris data set
data(iris)

# subset of iris - extract only species versicolor and setosa
irisSubDF = iris[1: 100, c(1, 3, 5)]
names(irisSubDF) = c("sepal", "petal", "species")
head(irisSubDF)

# plot the data
library(ggplot2)
ggplot(
  irisSubDF,
  aes(x = sepal, y = petal)
) +
  geom_point(
    aes(color = species, shape = species),
    size = 3
  ) +
  xlab("sepal length") +
  ylab("petal length") +
  ggtitle("species vs sepal and petal lengths")

# add binary labels corresponding to species
# initialize all values to; add setosa label of -1
irisSubDF[, 4] = 1
irisSubDF[irisSubDF[, 3] == "setosa", 4] = -1

# create two dataframes for attributes and labels respectively
x = irisSubDF[, c(1, 2)]
y = irisSubDF[, 4]
head(x)
head(y)

# new perceptron
perceptron = function(x, y, eta, niter) {
  # initialize the weight vector
  weight = rep(0, dim(x)[2] + 1)
  errors = rep(0, niter)
  
  # loop over the number of epoch niter
  for (iter in 1: niter) {
    # loop through the training data set
    for (inst in 1: length(y)) {
      # predict the binary label with heavyside activation
      z = sum(
        weight[2: length(weight)] * as.numeric(x[inst,])
          + weight[1]
      )
      yPred = 1
      if (z < 0)
        yPred = -1
      
      # change weight
      weightDiff = eta * (y[inst] - yPred) *
        c(1, as.numeric(x[inst,]))
      weight = weight + weightDiff
      
      # update the error function
      if (y[inst] - yPred != 0)
        errors[iter] = errors[iter] + 1
    }
  }
  
  # weight to decide between two species
  print(weight)
  return(errors)
}

# plotting the training errors
error = perceptron(x, y, 0.01, 20)
plot(
  1: 20, error,
  type = "l", lwd = 2, col = "red",
  xlab = "epoch #", ylab = "errors"
)
title("Errors vs epoch - learning rate eta = 1")