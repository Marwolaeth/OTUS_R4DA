library(RcppEigen)
library(microbenchmark)

(n <- 20000)
(generator <- 'rnorm')
(factor_effect <- 'large')
(binary_effect <- 'small')
(noise_effect <- 'moderate')
(generator <- match.fun(generator))
(factor_levels <- LETTERS[1:3])
(binary_levels <- c(FALSE, TRUE))

set.seed(1111)
x <- generator(n)
d <- factor(sample(factor_levels, n, replace = TRUE))
b <- sample(binary_levels, n, replace = TRUE)

(b1 <- 10.5)
(factor_magn <- switch(
  factor_effect, large = 50, moderate = 20, small = 5, none = 0)
)
(binary_magn <- switch(
  binary_effect, large = 50, moderate = 20, small = 5, none = 0)
)
(noise_magn <- switch(
  noise_effect, large = 50, moderate = 20, small = 10)
)
(b2 <- runif(1, .9, 1.11) * sample(c(-1, 1), 1) * factor_magn)
(b3 <- runif(1, .9, 1.11) * sample(c(-1, 1), 1) * factor_magn)
(b4 <- runif(1, .9, 1.11) * sample(c(-1, 1), 1) * binary_magn)
y <- 100 + b1*x + b2*(d == 'B') + b3*(d == 'C') + b4*b + rnorm(n, noise_magn, noise_magn/10)

plot(x, y)

df <- data.frame(x, d, b, y)

fastLm(y ~ x + d + b, data = df)
(X <- model.matrix(y ~ x + d + b, data = df))

bm_lm <- microbenchmark(
  stats = lm(y ~ x + d + b, data = df),
  eigen = fastLm(X, y),
  times = 1e3,
  control = list(warmup = 100L)
)
bm_lm
