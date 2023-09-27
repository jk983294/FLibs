library(ggplot2)
library(data.table)

# positive skewed
x <- rnbinom(10000L, 10, .5)
x <- rbeta(10000L, 3, 5) * 10
range(x)

(q <- FM::fm_quantile(x))
(q <- FM::fm_quantile(x ^ (1/3))) # cube root transformation
(q <- FM::fm_quantile(x ^ (1/2))) # square root transformation
(q <- FM::fm_quantile(log(x))) # logarithm transformation
ggplot(q, aes(x = qv)) + geom_line(aes(y = density_x))
plot(density(x))


# negative skewed
x <- rbeta(10000L, 5, 3) * 10
range(x)
(q <- FM::fm_quantile(x))
(q <- FM::fm_quantile(x ^ 2)) # square transformation
ggplot(q, aes(x = qv)) + geom_line(aes(y = density_x))
plot(density(1 / (1 + x)))
plot(density(x))
