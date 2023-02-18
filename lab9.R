# loading the Boston dataset
set.seed(500)
library(MASS)
data = Boston

# check for missing datapoints
apply(data, 2, function(x) sum(is.na(x)))

# randomly split the data
index = sample(
  1: nrow(data), round(0.75 * nrow(data))
)
train = data[index, ]
test = data[-index, ]

# fit a linear regression model
lm.fit = glm(medv ~ ., data = train)
summary(lm.fit)
pr.lm = predict(lm.fit, test)
mse.lm = sum((pr.lm - test$medv) ^ 2) / nrow(test)

# normalize the data before training the NN
maxs = apply(data, 2, max)
mins = apply(data, 2, min)
scaled = as.data.frame(
  scale(
    data,
    center = mins,
    scale = maxs - mins
  )
)
train = scaled[index, ]
test = scaled[-index, ]

# the neural network
# install.packages("neuralnet")
library(neuralnet)
n = names(train)
f = as.formula(
  paste(
    "medv ~",
    paste(
      n[!n %in% "medv"],
      collapse = " + "
    )
  )
)
nn = neuralnet(
  f, data = train,
  hidden = c(5, 3),
  linear.output = T
)

# plot the nn
plot(nn)

# predict the values for the test set
pr.nn = compute(nn, test[, 1: 13])
pr.nn = pr.nn$net.result * (
  max(data$medv) - min(data$medv)
) + min(data$medv)
test.r = (test$medv) * (
  max(data$medv) - min(data$medv)
) + min(data$medv)
mse.nn = sum((test.r - pr.nn) ^ 2) / nrow(test)

# performance of the nn vs the linear model on test set
print(paste(mse.lm, mse.nn))
plot(
  test$medv, pr.nn,
  col = "red", main = "Real vs Predicted NN",
  pch = 18, cex = 0.7
)
abline(0, 1, lwd = 2)
legend (
  "bottomright", legend = "NN",
  pch = 18, col = "red", bty = "n"
)
plot(
  test$medv, pr.lm,
  col = "blue", main = "Real vs Predicted LM",
  pch = 18, cex = 0.7
)
abline(0, 1, lwd = 2)
legend(
  "bottomright", legend = "LM",
  pch = 18, col = "blue", bty = "n", cex = 0.95
)