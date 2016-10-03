library(causalA16)
library(data.table)
library(ggplot2)
library(texreg)

dat <- load_dataset("fearon_laitin_03")
setDT(dat)
setkey(dat, onset)
dat <- dat[onset < 4]
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

make_fearon_obs_study <- function()
{

}
