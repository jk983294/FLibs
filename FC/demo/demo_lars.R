set.seed(42) # Set seed for reproducibility

n <- 100000L  # Number of observations
p <- 1000L  # Number of predictors included in model
real_p <- 100L  # Number of true predictors

## Generate the data
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
# y <- apply(x[,1:real_p], 1, sum) + rnorm(n)
y <- apply(x[,1:real_p], 1, sum)

for (c in 1L:ncol(x)) {
    if (c %% 2 == 0) {
        x[, c] = x[, c] * 100.
    }
}

y <- apply(x[,1:real_p], 1, sum)
x_mean_ = colMeans(x)
x_sd_ = apply(x, 2, sd)

gram = t(x) %*% x
Xy = t(x) %*% y

xx_sd <- matrix(sqrt(diag(gram)), ncol = 1) %*% matrix(sqrt(diag(gram)), nrow = 1)
xy_sd <- sqrt(diag(gram))
gram <- gram / xx_sd
Xy <- Xy / xy_sd

model <- FC::lars_gram_xy(gram, Xy, 100)
step <- 101L
beta <- model$beta[step, ]
beta <- beta / sqrt(diag(xx_sd)) # scale by sd to get real coef
beta <- beta[1L:100L]  # first 100 coef
length(beta[abs(beta) > 1e-6])
