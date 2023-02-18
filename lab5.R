# library imports
install.packages("e1071")
library(e1071)
install.packages("caret", dependencies = TRUE)
library(caret)

# create the data
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
x
y <- c(rep(-1, 10), rep(1, 10))
y
x[y == 1, ] = x[y == 1, ] + 1
x
plot(x, col = (3 - y))

# classification with svm
dat <- data.frame(x = x, y = as.factor(y))
dat
svm.fit = svm(
  y ~ ., data = dat,
  kernel = "linear", cost = 10000, scale = FALSE
)
plot(svm.fit, dat)
summary(svm.fit)

# tuning the cost
set.seed(1)
tune.out <- tune(
  svm, y ~ ., data = dat, kernel = "linear",
  ranges = list(cost = c(
      0.001, 0.01, 0.1, 1, 5, 10, 100
    )
  )
)
summary(tune.out)

# test phase
xtest = matrix(rnorm(20 * 2), ncol = 2)
ytest = sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] = xtest[ytest == 1, ] + 1
testdata = data.frame(x = xtest, y = as.factor(ytest))
yhat = predict(tune.out$best.model, testdata)
yhat
confusionMatrix(yhat, testdata$y)

# nonlinear svm
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1: 100, ] = x[1: 100, ] + 2
x[101: 150] = x[101: 150] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y)

# nonlinear svm - radial kernel
train <- sample(200, 100)
svm.fit <- svm(
  y ~ ., data = dat,
  kernel= "radial", gamma = 1, cost = 1
)
plot(svm.fit, dat[train, ])

summary(svm.fit)
yhat <- predict(svm.fit, dat[-train,])
confusionMatrix(yhat, dat[-train, "y"])

# increasing the cost reduces training error
# but increases the risk of overfitting
svm.fit <- svm(
  y ~ ., dat[train, ],
  kernel = "radial", gamma = 1,
  cost = 1e5
)
plot(svm.fit, dat[train, ])

# tuning cost and gamma
set.seed(1)
tune.out <- tune(
  svm, y ~ ., data = dat[train, ],
  kernel = "radial",
  ranges = list(
    cost = c(0.1, 1, 10, 100, 1000),
    gamma = c(0.5, 1, 2, 3, 4)
  )
)
summary(tune.out)
yhat <- predict(tune.out$best.model, dat[-train, ])
confusionMatrix(yhat, dat[-train, "y"])

# roc curves
library(ROCR)
rocplot <- function(pred, truth, ...) {
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
svm.opt <- svm(
  y ~ ., data = dat[train, ],
  kernel = "radial",
  gamma = 2, cost = 1, decision.values = T
)
fitted <- attributes(
  predict(svm.opt, dat[train, ], decision.values = T)
)$decision.values
rocplot(fitted, dat[train, "y"],
        main = "training data")