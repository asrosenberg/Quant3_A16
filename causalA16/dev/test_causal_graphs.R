# Figure 4.10
library(causalA16)
library(ggplot2)

data_for_figure_4_1 <- make_data_for_figure_4_1(1000)
calc_average_causal_effect_estimand(data_for_figure_4_1)
calc_naive_difference_in_means(data_for_figure_4_1)
calc_conditional_difference_in_means(data_for_figure_4_1, "s")
calc_conditional_difference_in_means(data_for_figure_4_1, "x")

set.seed(884962560)
simulations <- replicate(1000, {
  data_for_figure_4_1 <- make_data_for_figure_4_1(1000)
  c(calc_average_causal_effect_estimand(data_for_figure_4_1),
    calc_naive_difference_in_means(data_for_figure_4_1),
    calc_conditional_difference_in_means(data_for_figure_4_1, "s"),
    calc_conditional_difference_in_means(data_for_figure_4_1, "x"))
})

sim_data <- data.table(t(simulations))
setnames(sim_data, c("ACE_estimand",
  "naive_diff_in_means",
  "diff_in_means_cond_s",
  "diff_in_means_cond_x"))
sim_data[, `:=`(
  MAE_diff_in_means = abs(ACE_estimand - naive_diff_in_means),
  MAE_diff_in_means_cond_s = abs(ACE_estimand - diff_in_means_cond_s),
  MAE_diff_in_means_cond_x = abs(ACE_estimand - diff_in_means_cond_x)
)]
melt_sim_data <- melt(sim_data[, .(MAE_diff_in_means, MAE_diff_in_means_cond_s,
  MAE_diff_in_means_cond_x)])

ggplot(melt_sim_data, aes(y = value, x = variable)) +
  geom_boxplot()
