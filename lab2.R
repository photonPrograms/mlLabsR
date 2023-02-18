# generating data
x = seq(0, 1, length = 11)
y = sin(2 * pi * x)
noise = y + rnorm(11, sd = 0.3)
plot(x, noise, pch = 19)

# fit a third degree polynomial
lm3 <- lm(y ~ poly(x, 3))
pred <- predict(lm3)
ix <- sort(x, index.return = T)$ix
lines(x[ix], pred[ix], col = "red", lwd = 2)

# plotting the training and testing errors
lms <- list()
for (i in 1: 10)
  lms[[i]] <- lm(y ~ poly(x, i))

train = test = vector(length = 10)
# training error or mse
for (i in 1: 10)
  train[i] = var(lms[i][[1]]$residuals)
# testing error
newy = sin(2 * pi * x) + rnorm(11, sd = 0.3)
for (j in 1: 10)
  test[j] = 1 / 10 * sum(
    (newy - predict(
      lms[j][[1]]
    )) ^ 2
  )
plot(1: 10, train)
lines(1: 10, train,
      ylim = c(0, 0.5), col = "black", lwd = 3)
lines(1: 10, test,
      pch = "X", col = "red", lwd = 3
)
cbind(train, test)

# model summary
summary(lm3)
# coefficient
c1 <- coef(lm3)
c1

# regularization
install.packages("glmnet")
library(glmnet)
lambdas <- 10 ^ seq(3, -2, by = -0.1)
lambdas
fit <- glmnet(x, y, alpha = 0, lambda = lambdas)
fit