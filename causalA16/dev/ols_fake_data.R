n <- 10000
b0 <- 2
b1 <- -0.5
b2 <- 7
test_ols <- function()
{
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- b0 + b1 * x1 + b2 * x2 + rnorm(n, 0, 1)
  coef(lm(y ~ x1 + x2))
}
test_ols()
simulations <- t(replicate(1000, test_ols()))
hist(simulations[, 1], breaks = 100)
abline(v = b0, col = "red")
