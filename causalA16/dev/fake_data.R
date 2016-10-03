library(causalA16)
library(data.table)
library(ggplot2)
library(texreg)
library(Matching)

dat <- load_dataset("fearon_laitin_03")
setDT(dat)
setkey(dat, onset)
dat <- dat[onset < 4]
dat <- dat[, c("onset", "lpop", "gdpen", "Oil"), with = FALSE]
dat_subset <- dat[complete.cases(dat), ]
prop_1s <- sum(dat$onset)/nrow(dat)
prop_0s <- 1 - prop_1s

make_fake_fearon_data <- function(seed = sample.int(.Machine$integer.max, 1))
{
  n <- nrow(dat)
  set.seed(seed)
  prob_y1 <- prop_1s
  prob_y0 <- prop_0s
  DATA <- data.table(
  y0 = 1 * (runif(n) < prob_y0),
  y1 = 1 * (runif(n) < prob_y1))
  attr(DATA, "seed") <- seed
  DATA
}
test_fearon_data <- make_fake_fearon_data()
true_diff_means <- mean(test_fearon_data$y1 - test_fearon_data$y0)
make_fearon_obs_study <- function(seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)
  n <- 6609
  b0 <- 5
  b1 <- 3
  b2 <- -0.8
  x1 <- rnorm(n, mean = 9.602, sd = 1.45)
  x2 <- exp(rnorm(n, mean = .75, sd = 1))
  DATA <- make_fake_fearon_data()
  DATA[, prob_d_equals_1 := plogis(b0 + b1 * x1 + b2 * x2)]
  #mean(DATA$prob_d_equals_1)
  DATA[, d := 1 * (runif(n) < prob_d_equals_1)]
  DATA[, y := d * y1 + (1 - d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL, prob_d_equals_1 = NULL)]
  DATA[, ':=' (x1 = x1, x2 = x2)]
  attr(DATA, "seed") <- seed
  DATA
}
#prob_y_1 <- .02
test_data <- make_fearon_obs_study(seed = 66)
table(test_data$y)
table(dat$onset)
summary(dat$lpop)
#sd(dat$lpop, na.rm = TRUE)
#sd(dat$gdpen, na.rm = TRUE)


#table(test_data$d)

generate_matching_estimates <- function()
{
  DATA <- make_fearon_obs_study()
  propens_score <- glm(d ~ x1, data = DATA, family = binomial)
  ps <- predict(propens_score, type = "response")
  match_est <- Match(Y = DATA$y, Tr = DATA$d, X = ps)
  match_est$est
}
generate_matching_estimates()
replicates <- replicate(100, generate_matching_estimates())


prop_model <- glm(Oil ~ lpop + gdpen, data = dat_subset, family = binomial)
propscore <- predict(prop_model, type = "response")
naive_match <- Match(dat_subset$onset, Tr = dat_subset$Oil, X = propscore)
naive_match$est
hist(replicates, xlim = c(-1, 0))
hist(replicates)
abline(v = naive_match$est, col = "red")

hist(dat$lpop)
hist(dat$gdpen)
hist(exp(log(dat$gdpen)))
hist(exp(rnorm(n, mean = .75, sd = 1)))
