library(stargazer)
library(tidyverse)

source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Setup
dbl_theta <- 0.6
int_digits <- 3
dbl_scale <- 2
vec_N <- c(50, 100, 400, 1600)
int_N_unique <- length(vec_N)
vec_cases <- c("Only Cross-Fitting", "Only Orthogonal", "DML")

# Sparse ------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_sparse_non_orth.RData")
load("Results/Data/Final MCS Data/mcs_sparse_non_cf.RData")
load("Results/Data/Final MCS Data/mcs_sparse_dml.RData")

# Non-Orthogonal Score ----------------------------------------------------

df_nuis_sparse_non_orth <- desc_nuis(mcs_sparse_non_orth$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_sparse_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Sparse/non_orth_eta.tex")

# Look at Theta parameter directly
list_rate_sparse_non_orth <- mcs_sparse_non_orth %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sparse_non_orth <- list_rate_sparse_non_orth$rate %>% 
  select(-parameter_names)

df_rate_sparse_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sparse/non_orth_theta.tex")

df_rate_desc_sparse_non_orth <- list_rate_sparse_non_orth$rate_desc

df_rate_desc_sparse_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sparse/non_orth_rate_theta.tex")

# Non Cross-Fitting -------------------------------------------------------

df_nuis_sparse_non_cf <- desc_nuis(mcs_sparse_non_cf$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_sparse_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Sparse/non_cf_eta.tex")

# Look at Theta parameter directly
list_rate_sparse_non_cf <- mcs_sparse_non_cf %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sparse_non_cf <- list_rate_sparse_non_cf$rate %>% 
  select(-parameter_names)

df_rate_sparse_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sparse/non_cf_theta.tex")

df_rate_desc_sparse_non_cf <- list_rate_sparse_non_cf$rate_desc

df_rate_desc_sparse_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sparse/non_cf_rate_theta.tex")


# Double Machine Learning -------------------------------------------------

df_nuis_sparse_dml <- desc_nuis(mcs_sparse_dml$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_sparse_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Sparse/dml_eta.tex")

# Look at Theta parameter directly
list_rate_sparse_dml <- mcs_sparse_dml %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sparse_dml <- list_rate_sparse_dml$rate %>% 
  select(-parameter_names)

df_rate_sparse_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sparse/dml_theta.tex")

df_rate_desc_sparse_dml <- list_rate_sparse_dml$rate_desc

df_rate_desc_sparse_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sparse/dml_rate_theta.tex")


# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_sparse <- rbind.data.frame(
  df_nuis_sparse_non_orth,
  df_nuis_sparse_non_cf,
  df_nuis_sparse_dml
)

df_nuis_sparse$Case = rep(vec_cases, each = 2 * int_N_unique)

df_nuis_sparse %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE, shape = Case), size = 1.5) + 
  geom_line(aes(y = MSE), size = 1) +
  facet_grid(Fun ~ ., scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() + 
  scale_shape_manual(values = c(3, 4, 15, 16))

ggsave("Results/Plots/Motivation/Sparse/nuis_mse.png", width = 30, height = 15, units = "cm")

# Table showing MSRs, MSE and Bias for l_0 and m_0
df_nuis_sparse_dml %>% 
  select(N, Fun, MSE) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Estimated MSE of $ \\hat{\\eta}_0 $ (Sparse)",
            label = "tab_mot_sparse_nuis_mse",
            out = "Results/Tables/Motivation/Sparse/nuis_mse.tex",
            rownames = TRUE)

df_nuis_sparse_dml %>% 
  select(N, Fun, Bias) %>% 
  pivot_wider(values_from = Bias, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Estimated Bias of $ \\hat{\\eta}_0 $ (Sparse)",
            label = "tab_mot_sparse_nuis_bias",
            out = "Results/Tables/Motivation/Sparse/nuis_bias.tex",
            rownames = TRUE)

# df_nuis_sparse %>% 
#   filter(Case != "Only Orthogonal") %>% 
#   select(Case, N, Fun, Difference) %>% 
#   pivot_wider(values_from = Difference, names_from = N) %>% 
#   rename("Nuisance" = "Fun") %>% 
#   transform_scientific(int_digits) %>% 
#   stargazer(summary = FALSE, table.placement = "H",
#             title = "Difference of Validation and Test Error Measure (Sparse)",
#             label = "tab_mot_sparse_nuis_diff_msrs",
#             out = "Results/Tables/Motivation/Sparse/nuis_diff_msrs.tex",
#             rownames = FALSE)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_sparse <- rbind(
  df_rate_sparse_non_orth,
  df_rate_sparse_non_cf,
  df_rate_sparse_dml
)

df_rate_sparse$Case <- factor(rep(vec_cases, each = int_N_unique), levels = vec_cases)

df_rate_sparse %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), 
               values_to = "value", 
               names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = type, shape = type)) + 
  geom_point() + 
  geom_line() +
  facet_grid( ~ Case, scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() +
  scale_shape_manual(values = c(3, 4, 16))

ggsave("Results/Plots/Motivation/Sparse/mse_decomp.png", width = 30, height = 15, units = "cm")

df_rate_sparse %>% 
  ggplot(aes(x = N, y = Rate, col = Case, shape = Case)) +
  geom_point() + 
  geom_line() + 
  labs(col = "", shape = "") +
  theme_bw()

ggsave("Results/Plots/Motivation/Sparse/rate_mse.png", width = 30, height = 15, units = "cm")

# Table with Rates
df_rate_sparse %>% 
  select(N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Estimated Rate (Sparse)",
            label = "tab_mot_sparse_rate",
            out = "Results/Tables/Motivation/Sparse/rates.tex", 
            rownames = TRUE)

# Table with MSE by N for differing cases as columns
df_rate_sparse %>% 
  select(N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "MSE of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_mot_sparse_mse",
            out = "Results/Tables/Motivation/Sparse/mse.tex", 
            rownames = TRUE)

# Table with Squared Bias
df_rate_sparse %>% 
  select(N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_mot_sparse_sq_bias",
            out = "Results/Tables/Motivation/Sparse/sq_bias.tex", 
            rownames = TRUE)

# Table with  Bias
df_rate_sparse %>% 
  select(N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_mot_sparse_bias",
            out = "Results/Tables/Motivation/Sparse/bias.tex", 
            rownames = TRUE)

# Table with Variance
df_rate_sparse %>% 
  select(N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Variance of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_mot_sparse_variance",
            out = "Results/Tables/Motivation/Sparse/variance.tex", 
            rownames = TRUE)

# Distribution Plot facetted by case
list_dist_sparse_non_orth <- distribution(mcs_sparse_non_orth$Estimates, plot = FALSE)
list_dist_sparse_non_cf <- distribution(mcs_sparse_non_cf$Estimates, plot = FALSE)
list_dist_sparse_dml <- distribution(mcs_sparse_dml$Estimates, plot = FALSE)

df_parameter_sparse <- rbind(
  list_dist_sparse_non_orth$data,
  list_dist_sparse_non_cf$data,
  list_dist_sparse_dml$data
)

df_parameter_sparse$Case <- factor(
  x = rep(vec_cases, each = nrow(df_parameter_sparse) / length(vec_cases)),
  levels = vec_cases
)

df_normal_sparse <- rbind(
  list_dist_sparse_non_orth$normality,
  list_dist_sparse_non_cf$normality,
  list_dist_sparse_dml$normality
)

df_normal_sparse$Case <- factor(
  x = rep(vec_cases, each = nrow(df_normal_sparse) / length(vec_cases)),
  levels = vec_cases
)

ggplot(df_parameter_sparse) + 
  geom_histogram(aes(x = parameter_est, y = ..density..)) + 
  geom_line(aes(x = x, y = pdf), data = df_normal_sparse) + 
  geom_vline(xintercept = dbl_theta, col = "red") +
  facet_wrap(Case ~ N, scales = "free", strip.position = "right",
             labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density") +
  theme_bw()

ggsave("Results/Plots/Motivation/Sparse/dist.png", width = 30, height = 15, units = "cm")

# Coverage Probabilities
list_cov_prob_sparse_non_orth <- cov_prob(mcs_sparse_non_orth$Estimates, plot = FALSE)
list_cov_prob_sparse_non_cf <- cov_prob(mcs_sparse_non_cf$Estimates, plot = FALSE)
list_cov_prob_sparse_dml <- cov_prob(mcs_sparse_dml$Estimates, plot = FALSE)

df_cov_prob_sparse <- rbind(
  list_cov_prob_sparse_non_orth$data,
  list_cov_prob_sparse_non_cf$data,
  list_cov_prob_sparse_dml$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_sparse$Case <- factor(
  x = rep(vec_cases, each = nrow(df_cov_prob_sparse) / length(vec_cases)),
  levels = vec_cases
)

df_cov_prob_sparse %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  facet_grid(Case ~ ., labeller = label_wrap_gen(width = 10), scales = "free")

ggsave("Results/Plots/Motivation/Sparse/cov_prob.png", width = 30, height = 15, units = "cm")

# Sine --------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_sine_non_cf.RData")
load("Results/Data/Final MCS Data/mcs_sine_non_orth.RData")
load("Results/Data/Final MCS Data/mcs_sine_dml.RData")

# Non-Orthogonal Score ----------------------------------------------------

df_nuis_sine_non_orth <- desc_nuis(mcs_sine_non_orth$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_sine_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Sine/non_orth_eta.tex")

# Look at Theta parameter directly
list_rate_sine_non_orth <- mcs_sine_non_orth %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sine_non_orth <- list_rate_sine_non_orth$rate %>% 
  select(-parameter_names)

df_rate_sine_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sine/non_orth_theta.tex")

df_rate_desc_sine_non_orth <- list_rate_sine_non_orth$rate_desc

df_rate_desc_sine_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sine/non_orth_rate_theta.tex")

# Non Cross-Fitting -------------------------------------------------------

df_nuis_sine_non_cf <- desc_nuis(mcs_sine_non_cf$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_sine_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Sine/non_cf_eta.tex")

# Look at Theta parameter directly
list_rate_sine_non_cf <- mcs_sine_non_cf %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sine_non_cf <- list_rate_sine_non_cf$rate %>% 
  select(-parameter_names)

df_rate_sine_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sine/non_cf_theta.tex")

df_rate_desc_sine_non_cf <- list_rate_sine_non_cf$rate_desc

df_rate_desc_sine_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sine/non_cf_rate_theta.tex")


# Double Machine Learning -------------------------------------------------

df_nuis_sine_dml <- desc_nuis(mcs_sine_dml$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_sine_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Sine/dml_eta.tex")

# Look at Theta parameter directly
list_rate_sine_dml <- mcs_sine_dml %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sine_dml <- list_rate_sine_dml$rate %>% 
  select(-parameter_names)

df_rate_sine_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sine/dml_theta.tex")

df_rate_desc_sine_dml <- list_rate_sine_dml$rate_desc

df_rate_desc_sine_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Sine/dml_rate_theta.tex")


# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_sine <- rbind.data.frame(
  df_nuis_sine_non_orth,
  df_nuis_sine_non_cf,
  df_nuis_sine_dml
)

df_nuis_sine$Case = rep(vec_cases, each = 2 * int_N_unique)

df_nuis_sine %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE, shape = Case), size = 1.5) + 
  geom_line(aes(y = MSE), size = 1) +
  facet_grid(Fun ~ ., scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() + 
  scale_shape_manual(values = c(3, 4, 15, 16))

ggsave("Results/Plots/Motivation/Sine/nuis_mse.png", width = 30, height = 15, units = "cm")

# Table showing MSRs, MSE and Bias for l_0 and m_0
df_nuis_sine_dml %>% 
  select(N, Fun, MSE) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "!htb",
            title = "Estimated MSE of $ \\hat{\\eta}_0 $ (Sine)",
            label = "tab_mot_sine_nuis_mse",
            out = "Results/Tables/Motivation/Sine/nuis_mse.tex",
            rownames = TRUE)

df_nuis_sine_dml %>% 
  select(N, Fun, Bias) %>% 
  pivot_wider(values_from = Bias, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "!htb",
            title = "Estimated Bias of $ \\hat{\\eta}_0 $ (Sine)",
            label = "tab_mot_sine_nuis_bias",
            out = "Results/Tables/Motivation/Sine/nuis_bias.tex",
            rownames = TRUE)

# df_nuis_sine %>% 
#   mutate(Difference = `MSR. Validation` - `MSR. Test`) %>% 
#   select(Case, N, Fun, Difference) %>% 
#   pivot_wider(values_from = Difference, names_from = N) %>% 
#   rename("Nuisance" = "Fun") %>% 
#   transform_scientific(int_digits) %>% 
#   stargazer(summary = FALSE, table.placement = "H",
#             title = "Difference of Validation and Test Error Measure (Sine)",
#             label = "tab_mot_sine_nuis_diff_msrs",
#             out = "Results/Tables/Motivation/Sine/nuis_diff_msrs.tex",
#             rownames = FALSE)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_sine <- rbind(
  df_rate_sine_non_orth,
  df_rate_sine_non_cf,
  df_rate_sine_dml
)

df_rate_sine$Case <- factor(rep(vec_cases, each = int_N_unique), levels = vec_cases)

df_rate_sine %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), 
               values_to = "value", 
               names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = type, shape = type)) + 
  geom_point() + 
  geom_line() +
  facet_grid( ~ Case, scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() +
  scale_shape_manual(values = c(3, 4, 16))

ggsave("Results/Plots/Motivation/Sine/mse_decomp.png", width = 30, height = 15, units = "cm")

df_rate_sine %>% 
  ggplot(aes(x = N, y = Rate, col = Case, shape = Case)) +
  geom_point() + 
  geom_line() + 
  labs(col = "", shape = "") +
  theme_bw()

ggsave("Results/Plots/Motivation/Sine/rate_mse.png", width = 30, height = 15, units = "cm")

# Table with Rates
df_rate_sine %>% 
  select(N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Estimated Rate (Sine)",
            label = "tab_mot_sine_rate",
            out = "Results/Tables/Motivation/Sine/rates.tex", 
            rownames = TRUE)

# Table with MSE by N for differing cases as columns
df_rate_sine %>% 
  select(N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "MSE of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_mot_sine_mse",
            out = "Results/Tables/Motivation/Sine/mse.tex", 
            rownames = TRUE)

# Table with Squared Bias
df_rate_sine %>% 
  select(N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_mot_sine_sq_bias",
            out = "Results/Tables/Motivation/Sine/sq_bias.tex", 
            rownames = TRUE)

# Table with  Bias
df_rate_sine %>% 
  select(N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_mot_sine_bias",
            out = "Results/Tables/Motivation/Sine/bias.tex", 
            rownames = TRUE)

# Table with Variance
df_rate_sine %>% 
  select(N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Variance of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_mot_sine_variance",
            out = "Results/Tables/Motivation/Sine/variance.tex", 
            rownames = TRUE)

# Distribution Plot facetted by case
list_dist_sine_non_orth <- distribution(mcs_sine_non_orth$Estimates, plot = FALSE)
list_dist_sine_non_cf <- distribution(mcs_sine_non_cf$Estimates, plot = FALSE)
list_dist_sine_dml <- distribution(mcs_sine_dml$Estimates, plot = FALSE)

df_parameter_sine <- rbind(
  list_dist_sine_non_orth$data,
  list_dist_sine_non_cf$data,
  list_dist_sine_dml$data
)

df_parameter_sine$Case <- factor(
  x = rep(vec_cases, each = nrow(df_parameter_sine) / length(vec_cases)),
  levels = vec_cases
)

df_normal_sine <- rbind(
  list_dist_sine_non_orth$normality,
  list_dist_sine_non_cf$normality,
  list_dist_sine_dml$normality
)

df_normal_sine$Case <- factor(
  x = rep(vec_cases, each = nrow(df_normal_sine) / length(vec_cases)),
  levels = vec_cases
)

ggplot(df_parameter_sine) + 
  geom_histogram(aes(x = parameter_est, y = ..density..)) + 
  geom_line(aes(x = x, y = pdf), data = df_normal_sine) + 
  geom_vline(xintercept = dbl_theta, col = "red") +
  facet_wrap(Case ~ N, scales = "free", strip.position = "right",
             labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density") +
  theme_bw()

ggsave("Results/Plots/Motivation/Sine/dist.png", width = 30, height = 15, units = "cm")

# Coverage Probabilities
list_cov_prob_sine_non_orth <- cov_prob(mcs_sine_non_orth$Estimates, plot = FALSE)
list_cov_prob_sine_non_cf <- cov_prob(mcs_sine_non_cf$Estimates, plot = FALSE)
list_cov_prob_sine_dml <- cov_prob(mcs_sine_dml$Estimates, plot = FALSE)

df_cov_prob_sine <- rbind(
  list_cov_prob_sine_non_orth$data,
  list_cov_prob_sine_non_cf$data,
  list_cov_prob_sine_dml$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_sine$Case <- factor(
  x = rep(vec_cases, each = nrow(df_cov_prob_sine) / length(vec_cases)),
  levels = vec_cases
)

df_cov_prob_sine %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  facet_grid(Case ~ ., labeller = label_wrap_gen(width = 10), scales = "free")

ggsave("Results/Plots/Motivation/Sine/cov_prob.png", width = 30, height = 15, units = "cm")

# Inter --------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_inter_non_cf.RData")
load("Results/Data/Final MCS Data/mcs_inter_non_orth.RData")
load("Results/Data/Final MCS Data/mcs_inter_dml.RData")

# Non-Orthogonal Score ----------------------------------------------------

df_nuis_inter_non_orth <- desc_nuis(mcs_inter_non_orth$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_inter_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Inter/non_orth_eta.tex")

# Look at Theta parameter directly
list_rate_inter_non_orth <- mcs_inter_non_orth %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_inter_non_orth <- list_rate_inter_non_orth$rate %>% 
  select(-parameter_names)

df_rate_inter_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Inter/non_orth_theta.tex")

df_rate_desc_inter_non_orth <- list_rate_inter_non_orth$rate_desc

df_rate_desc_inter_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Inter/non_orth_rate_theta.tex")

# Non Cross-Fitting -------------------------------------------------------

df_nuis_inter_non_cf <- desc_nuis(mcs_inter_non_cf$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_inter_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Inter/non_cf_eta.tex")

# Look at Theta parameter directly
list_rate_inter_non_cf <- mcs_inter_non_cf %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_inter_non_cf <- list_rate_inter_non_cf$rate %>% 
  select(-parameter_names)

df_rate_inter_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Inter/non_cf_theta.tex")

df_rate_desc_inter_non_cf <- list_rate_inter_non_cf$rate_desc

df_rate_desc_inter_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Inter/non_cf_rate_theta.tex")


# Double Machine Learning -------------------------------------------------

df_nuis_inter_dml <- desc_nuis(mcs_inter_dml$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_inter_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Inter/dml_eta.tex")

# Look at Theta parameter directly
list_rate_inter_dml <- mcs_inter_dml %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_inter_dml <- list_rate_inter_dml$rate %>% 
  select(-parameter_names)

df_rate_inter_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Inter/dml_theta.tex")

df_rate_desc_inter_dml <- list_rate_inter_dml$rate_desc

df_rate_desc_inter_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Inter/dml_rate_theta.tex")


# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_inter <- rbind.data.frame(
  df_nuis_inter_non_orth,
  df_nuis_inter_non_cf,
  df_nuis_inter_dml
)

df_nuis_inter$Case = rep(vec_cases, each = 2 * int_N_unique)

df_nuis_inter %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE, shape = Case), size = 1.5) + 
  geom_line(aes(y = MSE), size = 1) +
  facet_grid(Fun ~ ., scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() + 
  scale_shape_manual(values = c(3, 4, 15, 16))

ggsave("Results/Plots/Motivation/Inter/nuis_mse.png", width = 30, height = 15, units = "cm")

# Table showing MSRs, MSE and Bias for l_0 and m_0
df_nuis_inter_dml %>% 
  select(N, Fun, MSE) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "!htb",
            title = "Estimated MSE of $ \\hat{\\eta}_0 $ (Inter)",
            label = "tab_mot_inter_nuis_mse",
            out = "Results/Tables/Motivation/Inter/nuis_mse.tex",
            rownames = TRUE)

df_nuis_inter_dml %>% 
  select(N, Fun, Bias) %>% 
  pivot_wider(values_from = Bias, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "!htb",
            title = "Estimated Bias of $ \\hat{\\eta}_0 $ (Inter)",
            label = "tab_mot_inter_nuis_bias",
            out = "Results/Tables/Motivation/Inter/nuis_bias.tex",
            rownames = TRUE)

# df_nuis_inter %>% 
#   filter(Case != "Only Orthogonal") %>% 
#   mutate(Difference = `MSR. Validation` - `MSR. Test`) %>% 
#   select(Case, N, Fun, Difference) %>% 
#   pivot_wider(values_from = Difference, names_from = N) %>% 
#   rename("Nuisance" = "Fun") %>% 
#   transform_scientific(int_digits) %>% 
#   stargazer(summary = FALSE, table.placement = "H",
#             title = "Difference of Validation and Test Error Measure (Inter)",
#             label = "tab_mot_inter_nuis_diff_msrs",
#             out = "Results/Tables/Motivation/Inter/nuis_diff_msrs.tex",
#             rownames = FALSE)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_inter <- rbind(
  df_rate_inter_non_orth,
  df_rate_inter_non_cf,
  df_rate_inter_dml
)

df_rate_inter$Case <- factor(rep(vec_cases, each = int_N_unique), levels = vec_cases)

df_rate_inter %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), 
               values_to = "value", 
               names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = type, shape = type)) + 
  geom_point() + 
  geom_line() +
  facet_grid( ~ Case, scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() +
  scale_shape_manual(values = c(3, 4, 16))

ggsave("Results/Plots/Motivation/Inter/mse_decomp.png", width = 30, height = 15, units = "cm")

df_rate_inter %>% 
  ggplot(aes(x = N, y = Rate, col = Case, shape = Case)) +
  geom_point() + 
  geom_line() + 
  labs(col = "", shape = "") +
  theme_bw()

ggsave("Results/Plots/Motivation/Inter/rate_mse.png", width = 30, height = 15, units = "cm")

# Table with Rates
df_rate_inter %>% 
  select(N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Estimated Rate (Interaction)",
            label = "tab_mot_inter_rate",
            out = "Results/Tables/Motivation/Inter/rates.tex", 
            rownames = TRUE)

# Table with MSE by N for differing cases as columns
df_rate_inter %>% 
  select(N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "MSE of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_mot_inter_mse",
            out = "Results/Tables/Motivation/Inter/mse.tex", 
            rownames = TRUE)

# Table with Squared Bias
df_rate_inter %>% 
  select(N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_mot_inter_sq_bias",
            out = "Results/Tables/Motivation/Inter/sq_bias.tex", 
            rownames = TRUE)

# Table with  Bias
df_rate_inter %>% 
  select(N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_mot_inter_bias",
            out = "Results/Tables/Motivation/Inter/bias.tex", 
            rownames = TRUE)

# Table with Variance
df_rate_inter %>% 
  select(N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Variance of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_mot_inter_variance",
            out = "Results/Tables/Motivation/Inter/variance.tex", 
            rownames = TRUE)

# Distribution Plot facetted by case
list_dist_inter_non_orth <- distribution(mcs_inter_non_orth$Estimates, plot = FALSE)
list_dist_inter_non_cf <- distribution(mcs_inter_non_cf$Estimates, plot = FALSE)
list_dist_inter_dml <- distribution(mcs_inter_dml$Estimates, plot = FALSE)

df_parameter_inter <- rbind(
  list_dist_inter_non_orth$data,
  list_dist_inter_non_cf$data,
  list_dist_inter_dml$data
)

df_parameter_inter$Case <- factor(
  x = rep(vec_cases, each = nrow(df_parameter_inter) / length(vec_cases)),
  levels = vec_cases
)

df_normal_inter <- rbind(
  list_dist_inter_non_orth$normality,
  list_dist_inter_non_cf$normality,
  list_dist_inter_dml$normality
)

df_normal_inter$Case <- factor(
  x = rep(vec_cases, each = nrow(df_normal_inter) / length(vec_cases)),
  levels = vec_cases
)

ggplot(df_parameter_inter) + 
  geom_histogram(aes(x = parameter_est, y = ..density..)) + 
  geom_line(aes(x = x, y = pdf), data = df_normal_inter) + 
  geom_vline(xintercept = dbl_theta, col = "red") +
  facet_wrap(Case ~ N, scales = "free", strip.position = "right",
             labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density") +
  theme_bw()

ggsave("Results/Plots/Motivation/Inter/dist.png", width = 30, height = 15, units = "cm")

# Coverage Probabilities
list_cov_prob_inter_non_orth <- cov_prob(mcs_inter_non_orth$Estimates, plot = FALSE)
list_cov_prob_inter_non_cf <- cov_prob(mcs_inter_non_cf$Estimates, plot = FALSE)
list_cov_prob_inter_dml <- cov_prob(mcs_inter_dml$Estimates, plot = FALSE)

df_cov_prob_inter <- rbind(
  list_cov_prob_inter_non_orth$data,
  list_cov_prob_inter_non_cf$data,
  list_cov_prob_inter_dml$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_inter$Case <- factor(
  x = rep(vec_cases, each = nrow(df_cov_prob_inter) / length(vec_cases)),
  levels = vec_cases
)

df_cov_prob_inter %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  facet_grid(Case ~ ., labeller = label_wrap_gen(width = 10), scales = "free")

ggsave("Results/Plots/Motivation/Inter/cov_prob.png", width = 30, height = 15, units = "cm")

# Neural --------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_neural_non_cf.RData")
load("Results/Data/Final MCS Data/mcs_neural_non_orth.RData")
load("Results/Data/Final MCS Data/mcs_neural_dml.RData")

# Non-Orthogonal Score ----------------------------------------------------

df_nuis_neural_non_orth <- desc_nuis(mcs_neural_non_orth$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_neural_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Neural/non_orth_eta.tex")

# Look at Theta parameter directly
list_rate_neural_non_orth <- mcs_neural_non_orth %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_neural_non_orth <- list_rate_neural_non_orth$rate %>% 
  select(-parameter_names)

df_rate_neural_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Neural/non_orth_theta.tex")

df_rate_desc_neural_non_orth <- list_rate_neural_non_orth$rate_desc

df_rate_desc_neural_non_orth %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Neural/non_orth_rate_theta.tex")

# Non Cross-Fitting -------------------------------------------------------

df_nuis_neural_non_cf <- desc_nuis(mcs_neural_non_cf$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_neural_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Neural/non_cf_eta.tex")

# Look at Theta parameter directly
list_rate_neural_non_cf <- mcs_neural_non_cf %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_neural_non_cf <- list_rate_neural_non_cf$rate %>% 
  select(-parameter_names)

df_rate_neural_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Neural/non_cf_theta.tex")

df_rate_desc_neural_non_cf <- list_rate_neural_non_cf$rate_desc

df_rate_desc_neural_non_cf %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Neural/non_cf_rate_theta.tex")


# Double Machine Learning -------------------------------------------------

df_nuis_neural_dml <- desc_nuis(mcs_neural_dml$Measures) %>% 
  arrange(Fun) %>% 
  select(-Mle, -Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    )
  )


# Eta estimator doesn't overfit and is unbiased
df_nuis_neural_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb", 
            out = "Results/Tables/Motivation/Neural/dml_eta.tex")

# Look at Theta parameter directly
list_rate_neural_dml <- mcs_neural_dml %>% 
  pluck("Estimates") %>% 
  mse() %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_neural_dml <- list_rate_neural_dml$rate %>% 
  select(-parameter_names)

df_rate_neural_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Neural/dml_theta.tex")

df_rate_desc_neural_dml <- list_rate_neural_dml$rate_desc

df_rate_desc_neural_dml %>% 
  transform_scientific(int_digits) %>% 
  stargazer(type = "latex", summary = FALSE, table.placement = "!htb",
            out = "Results/Tables/Motivation/Neural/dml_rate_theta.tex")


# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_neural <- rbind.data.frame(
  df_nuis_neural_non_orth,
  df_nuis_neural_non_cf,
  df_nuis_neural_dml
)

df_nuis_neural$Case = rep(vec_cases, each = 2 * int_N_unique)

df_nuis_neural %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE, shape = Case), size = 1.5) + 
  geom_line(aes(y = MSE), size = 1) +
  facet_grid(Fun ~ ., scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() + 
  scale_shape_manual(values = c(3, 4, 15, 16))

ggsave("Results/Plots/Motivation/Neural/nuis_mse.png", width = 30, height = 15, units = "cm")

# Table showing MSRs, MSE and Bias for l_0 and m_0
df_nuis_neural_dml %>% 
  select(N, Fun, MSE) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "!htb",
            title = "Estimated MSE of $ \\hat{\\eta}_0 $ (Neural)",
            label = "tab_mot_neural_nuis_mse",
            out = "Results/Tables/Motivation/Neural/nuis_mse.tex",
            rownames = TRUE)

df_nuis_neural_dml %>% 
  select(N, Fun, Bias) %>% 
  pivot_wider(values_from = Bias, names_from = N) %>% 
  column_to_rownames("Fun") %>% 
  set_rownames(c("$ l_0(X) $", "$ m_0(X) $")) %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "!htb",
            title = "Estimated Bias of $ \\hat{\\eta}_0 $ (Neural)",
            label = "tab_mot_neural_nuis_bias",
            out = "Results/Tables/Motivation/Neural/nuis_bias.tex",
            rownames = TRUE)

# df_nuis_neural_dml %>% 
#   mutate(Difference = `MSR. Validation` - `MSR. Test`) %>% 
#   select(Case, N, Fun, Difference) %>% 
#   pivot_wider(values_from = Difference, names_from = N) %>% 
#   rename("Nuisance" = "Fun") %>% 
#   transform_scientific(int_digits) %>% 
#   stargazer(summary = FALSE, table.placement = "H",
#             title = "Difference of Validation and Test Error Measure (Neural)",
#             label = "tab_mot_neural_nuis_diff_msrs",
#             out = "Results/Tables/Motivation/Neural/nuis_diff_msrs.tex",
#             rownames = FALSE)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_neural <- rbind(
  df_rate_neural_non_orth,
  df_rate_neural_non_cf,
  df_rate_neural_dml
)

df_rate_neural$Case <- factor(rep(vec_cases, each = int_N_unique), levels = vec_cases)

df_rate_neural %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), 
               values_to = "value", 
               names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = type, shape = type)) + 
  geom_point() + 
  geom_line() +
  facet_grid( ~ Case, scales = "free") + 
  labs(y = "", col = "", shape = "") + 
  theme_bw() +
  scale_shape_manual(values = c(3, 4, 16))

ggsave("Results/Plots/Motivation/Neural/mse_decomp.png", width = 30, height = 15, units = "cm")

df_rate_neural %>% 
  ggplot(aes(x = N, y = Rate, col = Case, shape = Case)) +
  geom_point() + 
  geom_line() + 
  labs(col = "", shape = "") +
  theme_bw()

ggsave("Results/Plots/Motivation/Neural/rate_mse.png", width = 30, height = 15, units = "cm")

# Table with Rates
df_rate_neural %>% 
  select(N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Estimated Rate (Neural Network)",
            label = "tab_mot_neural_rate",
            out = "Results/Tables/Motivation/Neural/rates.tex", 
            rownames = TRUE)

# Table with MSE by N for differing cases as columns
df_rate_neural %>% 
  select(N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "MSE of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_mot_neural_mse",
            out = "Results/Tables/Motivation/Neural/mse.tex", 
            rownames = TRUE)

# Table with Squared Bias
df_rate_neural %>% 
  select(N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_mot_neural_sq_bias",
            out = "Results/Tables/Motivation/Neural/sq_bias.tex", 
            rownames = TRUE)

# Table with  Bias
df_rate_neural %>% 
  select(N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_mot_neural_bias",
            out = "Results/Tables/Motivation/Neural/bias.tex", 
            rownames = TRUE)

# Table with Variance
df_rate_neural %>% 
  select(N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  column_to_rownames("Case") %>% 
  transform_scientific(int_digits) %>% 
  stargazer(summary = FALSE, table.placement = "H", 
            title = "Variance of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_mot_neural_variance",
            out = "Results/Tables/Motivation/Neural/variance.tex", 
            rownames = TRUE)

# Distribution Plot facetted by case
list_dist_neural_non_orth <- distribution(mcs_neural_non_orth$Estimates, plot = FALSE)
list_dist_neural_non_cf <- distribution(mcs_neural_non_cf$Estimates, plot = FALSE)
list_dist_neural_dml <- distribution(mcs_neural_dml$Estimates, plot = FALSE)

df_parameter_neural <- rbind(
  list_dist_neural_non_orth$data,
  list_dist_neural_non_cf$data,
  list_dist_neural_dml$data
)

df_parameter_neural$Case <- factor(
  x = rep(vec_cases, each = nrow(df_parameter_neural) / length(vec_cases)),
  levels = vec_cases
)

df_normal_neural <- rbind(
  list_dist_neural_non_orth$normality,
  list_dist_neural_non_cf$normality,
  list_dist_neural_dml$normality
)

df_normal_neural$Case <- factor(
  x = rep(vec_cases, each = nrow(df_normal_neural) / length(vec_cases)),
  levels = vec_cases
)

ggplot(df_parameter_neural) + 
  geom_histogram(aes(x = parameter_est, y = ..density..)) + 
  geom_line(aes(x = x, y = pdf), data = df_normal_neural) + 
  geom_vline(xintercept = dbl_theta, col = "red") +
  facet_wrap(Case ~ N, scales = "free", strip.position = "right",
             labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density") +
  theme_bw()

ggsave("Results/Plots/Motivation/Neural/dist.png", width = 30, height = 15, units = "cm")

# Coverage Probabilities
list_cov_prob_neural_non_orth <- cov_prob(mcs_neural_non_orth$Estimates, plot = FALSE)
list_cov_prob_neural_non_cf <- cov_prob(mcs_neural_non_cf$Estimates, plot = FALSE)
list_cov_prob_neural_dml <- cov_prob(mcs_neural_dml$Estimates, plot = FALSE)

df_cov_prob_neural <- rbind(
  list_cov_prob_neural_non_orth$data,
  list_cov_prob_neural_non_cf$data,
  list_cov_prob_neural_dml$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_neural$Case <- factor(
  x = rep(vec_cases, each = nrow(df_cov_prob_neural) / length(vec_cases)),
  levels = vec_cases
)

df_cov_prob_neural %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  facet_grid(Case ~ ., labeller = label_wrap_gen(width = 10), scales = "free")

ggsave("Results/Plots/Motivation/Neural/cov_prob.png", width = 30, height = 15, units = "cm")


# Joint Analysis of All Functions -----------------------------------------

# Table of Estiamted Rates
df_rate_dml <- rbind(
  df_rate_sparse_dml,
  df_rate_sine_dml,
  df_rate_inter_dml,
  df_rate_neural_dml
) %>% 
  select(N, Rate) %>% 
  drop_na()

df_rate_dml$`Nuisance Function` <- rep(c("Sparse", "Sine", "Interaction", "Neural Net"), each = 3)

df_rate_dml <- df_rate_dml %>% 
  pivot_wider(names_from = N, values_from = Rate)

df_rate_dml$`Mean Rate` <- rowMeans(df_rate_dml[, -1])

df_rate_dml %>% 
  transform_scientific(int_digits) %>% 
  column_to_rownames("Nuisance Function") %>% 
  stargazer(title = "Estimated Rates by $ N $", 
            label = "tab_mot_rate", 
            summary = FALSE, 
            table.placement = "!htb", 
            rownames = TRUE,
            out = "Results/Tables/Motivation/rate_conv.tex")

