# loading the data
data(mtcars)
head(mtcars)

# drop the categorical features
d1 <- mtcars[, c(1: 7, 10, 11)]
head(d1)

# applying the pca
d1.pca <- princomp(d1, cor = TRUE, score = TRUE)
summary(d1.pca)
plot(d1.pca)

# scree plot
plot(d1.pca, type = "l")

d1.pca$loadings

# transformed dataset
d2 <- d1.pca$scores
head(d2)

# for svd
A = as.matrix(data.frame(c(3, 1, 1), c(-1, 3, 1)))
A
A.svd <- svd(A)
A.svd

# svd step by step
AtA <- t(A) %*% A
AtA
AtA.e <- eigen(AtA)
V.mat <- AtA.e$vectors
V.mat
V.mat[, 1: 2] <- V.mat[, 1: 2] * -1
V.mat

AAt <- A %*% t(A)
AAt
AAt.e <- eigen(AAt)
U.mat <- AAt.e$vectors
U.mat
U.mat <- U.mat[, 1: 2]
U.mat

r <- sqrt(AtA.e$values)
r
r <- r * diag(length(r))[, 1: 2]
r

svd.matrix <- U.mat %*% r %*% t(V.mat)
svd.matrix