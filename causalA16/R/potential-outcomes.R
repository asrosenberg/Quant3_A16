# ' Simulate a binary valued potential outcomes model
#'
#' Makes binary causal states and binary valued potential outcomes.
#' @param n number of observations
#' @param seed RNG seed
#' @return data.table with potential outcomes
#' @import data.table
#' @export
simulate_potential_outcomes <- function(n,
  seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)
  prob_y1 <- .7
  prob_y0 <- .3
  DATA <- data.table(
    y0 = 1 * (runif(n) < prob_y0),
    y1 = 1 * (runif(n) < prob_y1))
  attr(DATA, "seed") <- seed
  DATA
}

# ' Simulate a Simple Experiment
#'
#' Simulates potential outcomes and adds a treatment variable,
#' assumes SUTVA, defines the observed outcomes, deletes potential outcomes, and
#' defines a result.
#' @param n number of observations
#' @param seed RNG seed
#' @param prob_treatment probability of treatment
#' @return data.table with experiment data
#' @import data.table
#' @export
simulate_simple_experiment <- function(n, prob_treatment = 0.5,
  seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)
  DATA <- simulate_potential_outcomes(n)
  DATA[, prob_treatment := prob_treatment]
  DATA[, d := 1 * (runif(n) < prob_treatment)]
  DATA[, y := d * y1 + (1 - d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL)]
  attr(DATA, "seed") <- seed
  DATA
}

# ' Simulate an observational study
#'
#' Simulates potential outcomes and adds a treatment variable,
#' assumes SUTVA, defines the observed outcomes, deletes potential outcomes, and
#' defines a result.
#' @param n number of observations
#' @param seed RNG seed
#' @return data.table with simulated observational study
#' @import data.table
#' @export
simulate_observational_study <- function(n, prob_treatment = 0.5,
  seed = sample.int(.Machine$integer.max, 1))
{
  set.seed(seed)
  DATA <- simulate_potential_outcomes(n)
  DATA[, prob_d_equals_1 := plogis(-2 + 4 * (y1 - y0))]
  DATA[, d := 1 * (runif(n) < prob_d_equals_1)]
  DATA[, y := d * y1 + (1 - d) * y0]
  DATA[, `:=`(y1 = NULL, y0 = NULL, prob_d_equals_1 = NULL)]
  attr(DATA, "seed") <- seed
  DATA
}

# ' Calculate naive difference in means
#'
#' Calculates the naive difference in means
#' @param DATA a dataset
#' @return number
#' @import data.table
#' @export
calculate_naive_difference_in_means <- function(DATA)
{
  DATA[, mean(y[d == 1]) - mean(y[d == 0])]
}
