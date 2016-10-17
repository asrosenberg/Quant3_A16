library(causalA16)
library(ipw)
library(ggplot2)

make_ip_weights <- function(tr, vars, dat, stabilized = FALSE)
{
  if(stabilized == FALSE)
  {
    fmla <- as.formula(paste(tr, " ~ ", paste(vars, collapse= "+")))
    mod <- glm(formula = fmla, data = dat, family = "binomial")
    dat$prop_score <- predict(mod, type = "response")
    weights <- ifelse(dat[, c(tr)] == 1, 1/dat$prop_score, 
      1/(1 - dat$prop_score))
    return(weights)
  }
  if(stabilized == TRUE)
  {
    prob1 <- (sum(dat[, c(tr)] == 1)) / (nrow(dat))
    prob0 <- (sum(dat[, c(tr)] == 0)) / (nrow(dat))
    fmla <- as.formula(paste(tr, " ~ ", paste(vars, collapse= "+")))
    mod <- glm(formula = fmla, data = dat, family = "binomial")
    dat$prop_score <- predict(mod, type = "response")
    weights <- ifelse(dat[, c(tr)] == 1, prob1/dat$prop_score, 
      prob0/(1 - dat$prop_score))
    weights
  }
}

data(haartdat)
vars <- c("sex", "age", "cd4.sqrt")
tr <- "haartind"
dat <- haartdat

weight_test <- make_ip_weights(tr = "haartind", 
  vars = c("sex", "age", "cd4.sqrt"), dat = haartdat, 
  stabilized = FALSE)
dat$weights <- weight_test
ggplot(dat, aes(weights)) + geom_histogram(bins = 100) + 
  theme_bw() +
  facet_wrap(~ haartind, ncol = 1, scales = "free_y")
range(dat$weights)

stabilized_weight_test <- make_ip_weights(tr = "haartind", 
  vars = c("sex", "age", "cd4.sqrt"), dat = haartdat, stabilized = TRUE)
range(dat$stab_weights)
dat$stab_weights <- stabilized_weight_test
ggplot(dat, aes(stab_weights)) + geom_histogram(bins = 100) + theme_bw() +
  facet_wrap(~ haartind, ncol = 1, scales = "free_y")

# Compare with ipw
# This function sucks, don't use it
temp <- ipwtm(
  exposure = haartind,
  family = "binomial",
  link = "logit",
  #numerator = ~ sex + age,
  denominator = ~ sex + age + cd4.sqrt,
  id = patient,
  tstart = tstart,
  timevar = fuptime,
  type = "all",
  data = haartdat)
