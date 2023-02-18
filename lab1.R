# variable types
x <- "hello world"
x <- TRUE
x <- 11
x <- 1.1e10
x

# vectors
x <- c(0, 1.2, 8/5)
x <- 1:10
x <- rep(10, times = 3)
x <- sample(c(2, 5, 3), size = 4, replace = TRUE)
x

# vector ops
# are performed element wise retaining the dimensions
x <- 1:4
y <- 5:8
print(x + y)
print(x - y)
print(x * y)
print(x / y)

# wrap around
z <- 9:11
print(x + z)

# slicing
x[1: 3] <- z
x[1: 2]

# matrices (column major) with cycling
m <- matrix(1: 10, nrow = 3, ncol = 4)
m

# adding new rows and columns
m <- rbind(m, c(1, 2, 3, 4))
m <- cbind(m, c(5, 6, 7))
m

# matrix ops
x <- matrix(1: 4, nrow = 2, ncol = 2)
y <- matrix(5: 8, nrow = 2, ncol = 2)
print(x * y) # element wise multiplication
print(x %*% y) # matrix multiplication
print(t(x)) # transpose

# matrix indexing
# the 0th row and col are serial numbers
m
m[0, ]
m[, 0]
m[1: 2, ]

m[1: 2, , drop = FALSE]
m[1: 2, ]

# flow control
for (i in 1:10) {
  print(i)
}
x <- c(10, 12, 14, 44, 43, 21, 21)
for (i in x)
  print(i)

# else follows the } of if
if (i == 21) {
  print(i + i)
} else
  print(i - i)

# loading a dataset
data("mtcars")
summary(mtcars)

# histogram
hist(mtcars$carb)

# breaks is a suggestion - and not a fixed value - for hist()
# to use the closest number of pretty plot bins
hist(
  mtcars$mpg, breaks = 20,
  xlab = "miles per gallon",
  main = "histogram of miles per gallon",
  xlim = range(10: 35)
)

# box and whisker plot
plot(
  mtcars$mpg ~ as.factor(mtcars$am),
  mtcars,
  xlab = "transmission type",
  ylab = "miles per gallon",
  main = "histogram of mpg by transmission type"
)