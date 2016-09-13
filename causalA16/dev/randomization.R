library(causalA16)
set.seed(1)
D <- simulate_potential_outcomes(1000)
# now just write down all the possible treatment assignments
# sample from the randomization distribution
randomize <- function(DATA)
{
  treated <- sample.int(1000, 500)
  control <- setdiff(1:1000, treated)
  DATA[treated, y := y1]
  DATA[control, y := y0]
  DATA[treated, mean(y)] - DATA[control, mean(y)]
}
permutation_distribution <- replicate(10000, randomize(D))
mean(permutation_distribution)
sd(permutation_distribution)
hist(permutation_distribution, breaks = 100)

set.seed(1)
# needs documentation
simulate_paired_experiment <- function(n,
  seed = sample.int(.Machine$integer.max, 1))
{
  stopifnot(n %% 2 == 0) # stop if n isn't even, %% is remainder
  set.seed(seed)
  DATA <- simulate_potential_outcomes(n)
  treated <- sample.int(n, n / 2)
  control <- setdiff(1:n, treated)
  DATA[treated, d := 1]
  DATA[control, d := 0]
  DATA[, y := d * y1 + (1 - d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL)]
  attr(DATA, "seed") <- seed
  DATA
}
experiment <- simulate_paired_experiment(1000)
#experiment[, .N, .(d, y)]
experiment[, y1 := y]
experiment[, y0 := y]
tau0 <- experiment[d == 1, mean(y)] - experiment[d == 0, mean(y)]
randomize(experiment) # distribution of differences in means estimator under sharp null
permutation_distribution <- replicate(1000, randomize(experiment))
hist(permutation_distribution, breaks = 100, xlim = c(-.5, .5))
abline(v = tau0, col = "red")
