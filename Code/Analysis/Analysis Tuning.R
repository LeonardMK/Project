library(latex2exp)
library(stargazer)
library(tidyverse)

source("Code/Monte Carlo Methods.R")
source("Code/Utils.R")

# Setup
dbl_theta <- 0.6
int_digits <- 3
dbl_scale <- 2
vec_N <- c(50, 100, 400)
int_N_unique <- length(vec_N)
vec_cases <- c("Untuned", "Tuned")
vec_mle <- c("E-Net", "K-KNN", "N-Net", "RF", "CART", "SGBM")
vec_mle_fun_unique <- c("glmnet", "kknn", "nnet", "ranger", "rpart", "xgboost")
vec_mle_fun_unique_title <- str_to_title(vec_mle_fun_unique)
names(vec_mle) <- vec_mle_fun_unique
int_mle_unique <- length(vec_mle)
vec_mle_filter <- c("Best Measure", 
                    "E-Net",
                    "K-KNN",
                    "N-Net",
                    "SGBM")

# Sparse ------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_sparse_not.RData")
load("Results/Data/Final MCS Data/mcs_sparse_rcv.RData")

# Untuned -------------------------------------------------------------------

df_nuis_sparse_not <- desc_nuis(mcs_sparse_not$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_sparse_not <- mcs_sparse_not %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sparse_not <- list_rate_sparse_not$rate %>% 
  select(-parameter_names)

df_rate_desc_sparse_not <- list_rate_sparse_not$rate_desc

# Tuned ----------------------------------------------------

df_nuis_sparse_rcv <- desc_nuis(mcs_sparse_rcv$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_sparse_rcv <- mcs_sparse_rcv %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sparse_rcv <- list_rate_sparse_rcv$rate %>% 
  select(-parameter_names)

df_rate_desc_sparse_rcv <- list_rate_sparse_rcv$rate_desc

# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_sparse <- rbind.data.frame(
  df_nuis_sparse_not,
  df_nuis_sparse_rcv
)

df_nuis_sparse$Case <- rep(vec_cases, each = 2 * int_N_unique * int_mle_unique)

df_nuis_sparse$MLE <- str_replace(df_nuis_sparse$MLE, vec_mle_fun_unique_title, vec_mle)

# Exporting Table of MSE and Bias
df_nuis_sparse_l_0 <- df_nuis_sparse %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^l"))

df_nuis_sparse_m_0 <- df_nuis_sparse %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^m"))
  
df_nuis_sparse_l_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ l_0(X) $ by $ N $ (Sparse)",
            label = "tab_tune_sparse_mse_nuis_l_0",
            out = "Results/Tables/Tuning/Sparse/mse_nuis_l_0.tex",
            rownames = FALSE
            )

df_nuis_sparse_l_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ l_0(X) $ by $ N $ (Sparse)",
            label = "tab_tune_sparse_bias_nuis_l_0",
            out = "Results/Tables/Tuning/Sparse/bias_nuis_l_0.tex",
            rownames = FALSE
            )

df_nuis_sparse_m_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ m_0(X) $ by $ N $ (Sparse)",
            label = "tab_tune_sparse_mse_nuis_m_0",
            out = "Results/Tables/Tuning/Sparse/mse_nuis_m_0.tex",
            rownames = FALSE
            )

df_nuis_sparse_m_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ m_0(X) $ by $ N $ (Sparse)",
            label = "tab_tune_sparse_bias_nuis_m_0",
            out = "Results/Tables/Tuning/Sparse/bias_nuis_m_0.tex",
            rownames = FALSE
            )

df_nuis_sparse$Case <- factor(df_nuis_sparse$Case, levels = vec_cases)
df_nuis_sparse$Fun <- factor(df_nuis_sparse$Fun, labels = c(TeX("$ l_0(X) $"), TeX("$ m_0(X) $")))

df_nuis_sparse %>% 
  mutate(
    Upper = MSE + qnorm(0.975) * Sd_mse,
    Lower = MSE + qnorm(0.025) * Sd_mse
  ) %>% 
  mutate(Lower = if_else(Lower < 0, 0, Lower)) %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE), size = 1.5) + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, alpha = 0.5) +
  geom_line(aes(y = MSE), size = 1) +
  facet_wrap(Fun ~ MLE, 
             scales = "free", 
             labeller = labeller(Fun =label_parsed), 
             nrow = 2) + 
  labs(y = "", col = "") + 
  theme_bw()

ggsave("Results/Plots/Tuning/Sparse/nuis_mse.png", scale = dbl_scale)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_sparse_not$Case <- "Untuned"
df_rate_sparse_rcv$Case <- "Tuned"

df_rate_sparse <- rbind(
  df_rate_sparse_not,
  df_rate_sparse_rcv
)

df_rate_sparse$Case <- factor(df_rate_sparse$Case, levels = vec_cases)

df_rate_sparse %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), values_to = "value", names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = Case, lty = type)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 1) +
  facet_wrap( ~ Algorithms, scales = "free", nrow = 1) + 
  labs(y = "", col = "", lty = "") + 
  theme_bw() + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

ggsave("Results/Plots/Tuning/Sparse/mse_decomp.png", scale = dbl_scale)

df_rate_sparse %>% 
  ggplot(aes(x = N, y = Rate, col = Case)) +
  geom_point(size = 1.5) + 
  geom_line(size = 1) + 
  labs(col = "") +
  theme_bw() + 
  facet_grid(~ Algorithms)

ggsave("Results/Plots/Tuning/Sparse/rate_mse.png", scale = dbl_scale)

# Table with Rates
df_rate_sparse %>% 
  select(Algorithms, N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Estimated Rates (Sparse)",
            label = "tab_tune_sparse_rate",
            out = "Results/Tables/Tuning/Sparse/rates.tex",
            rownames = FALSE
            )

# Table with MSE by N for differing cases as columns
df_rate_sparse %>% 
  select(Algorithms, N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_tune_sparse_mse",
            out = "Results/Tables/Tuning/Sparse/mse.tex",
            rownames = FALSE
            )

# Table with Squared Bias
df_rate_sparse %>% 
  select(Algorithms, N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_tune_sparse_sq_bias",
            out = "Results/Tables/Tuning/Sparse/sq_bias.tex",
            rownames = FALSE
            )

# Table with Squared Bias
df_rate_sparse %>% 
  select(Algorithms, N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_tune_sparse_bias",
            out = "Results/Tables/Tuning/Sparse/bias.tex",
            rownames = FALSE
            )

# Table with Variance
df_rate_sparse %>% 
  select(Algorithms, N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "!htb",
            title = "Variance of $ \\hat{\\theta}_0 $ (Sparse)",
            label = "tab_tune_sparse_variance",
            out = "Results/Tables/Tuning/Sparse/variance.tex",
            rownames = FALSE
            )

# Distribution Plot facetted by case
list_dist_sparse_not <- distribution(mcs_sparse_not$Estimates, plot = FALSE, by = "Algorithms")
list_dist_sparse_rcv <- distribution(mcs_sparse_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_dist_sparse_not$data$Case <- "Untuned"
list_dist_sparse_rcv$data$Case <- "Tuned"

list_dist_sparse_not$normality$Case <- "Untuned"
list_dist_sparse_rcv$normality$Case <- "Tuned"

df_parameter_sparse <- rbind(
  list_dist_sparse_not$data,
  list_dist_sparse_rcv$data
)

df_parameter_sparse$Case <- factor(df_parameter_sparse$Case, levels = vec_cases)

df_normal_sparse <- rbind(
  list_dist_sparse_not$normality,
  list_dist_sparse_rcv$normality
)

df_normal_sparse$Case <- factor(df_normal_sparse$Case, levels = vec_cases)

# Filter only Best, GLMNET, Neural Networks, KKNN and SGBM
df_parameter_sparse <- df_parameter_sparse %>% 
  filter(Algorithms %in% vec_mle_filter)

df_normal_sparse <- df_normal_sparse %>% 
  filter(Algorithms %in% vec_mle_filter)

ggplot(df_parameter_sparse) + 
  geom_histogram(aes(x = parameter_est, y = ..density.., col = Case, fill = Case), 
                 alpha = 0.25, position = "identity") + 
  geom_line(aes(x = x, y = pdf, col = Case), data = df_normal_sparse, size = 1) + 
  geom_vline(xintercept = dbl_theta) +
  facet_wrap(Algorithms ~ N, scales = "free_y", nrow = length(vec_mle_filter), 
             strip.position = "right", labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density", col = "", fill = "") +
  theme_bw()

ggsave("Results/Plots/Tuning/Sparse/dist.png", scale = dbl_scale)

# Coverage Probabilities
list_cov_prob_sparse_not <- cov_prob(mcs_sparse_not$Estimates, plot = FALSE, by = "Algorithms")
list_cov_prob_sparse_rcv <- cov_prob(mcs_sparse_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_cov_prob_sparse_not$data$Case <- "Untuned"
list_cov_prob_sparse_rcv$data$Case <- "Tuned"

df_cov_prob_sparse <- rbind(
  list_cov_prob_sparse_not$data,
  list_cov_prob_sparse_rcv$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_sparse$Case <- factor(df_cov_prob_sparse$Case, levels = vec_cases)

df_cov_prob_sparse %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(aes(shape = Case), size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`, lty = Case)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "Point", lty = "CI") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  scale_shape_manual(values = c(3, 4)) +
  scale_linetype_manual(values = c("solid", "twodash")) +
  facet_grid(~ Algorithms, scales = "free")

ggsave("Results/Plots/Tuning/Sparse/cov_prob.png", scale = dbl_scale)

# Sine --------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_sine_not.RData")
load("Results/Data/Final MCS Data/mcs_sine_rcv.RData")

# Untuned -------------------------------------------------------------------

df_nuis_sine_not <- desc_nuis(mcs_sine_not$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_sine_not <- mcs_sine_not %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sine_not <- list_rate_sine_not$rate %>% 
  select(-parameter_names)

df_rate_desc_sine_not <- list_rate_sine_not$rate_desc

# Tuned ----------------------------------------------------

df_nuis_sine_rcv <- desc_nuis(mcs_sine_rcv$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_sine_rcv <- mcs_sine_rcv %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_sine_rcv <- list_rate_sine_rcv$rate %>% 
  select(-parameter_names)

df_rate_desc_sine_rcv <- list_rate_sine_rcv$rate_desc

# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_sine <- rbind.data.frame(
  df_nuis_sine_not,
  df_nuis_sine_rcv
)

df_nuis_sine$Case <- rep(vec_cases, each = 2 * int_N_unique * int_mle_unique)

df_nuis_sine$MLE <- str_replace(df_nuis_sine$MLE, vec_mle_fun_unique_title, vec_mle)

# Exporting Table of MSE and Bias
df_nuis_sine_l_0 <- df_nuis_sine %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^l"))

df_nuis_sine_m_0 <- df_nuis_sine %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^m"))

df_nuis_sine_l_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ l_0(X) $ by $ N $ (Sine)",
            label = "tab_tune_sine_mse_nuis_l_0",
            out = "Results/Tables/Tuning/Sine/mse_nuis_l_0.tex",
            rownames = FALSE
  )

df_nuis_sine_l_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ l_0(X) $ by $ N $ (Sine)",
            label = "tab_tune_sine_bias_nuis_l_0",
            out = "Results/Tables/Tuning/Sine/bias_nuis_l_0.tex",
            rownames = FALSE
  )

df_nuis_sine_m_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ m_0(X) $ by $ N $ (Sine)",
            label = "tab_tune_sine_mse_nuis_m_0",
            out = "Results/Tables/Tuning/Sine/mse_nuis_m_0.tex",
            rownames = FALSE
  )

df_nuis_sine_m_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ m_0(X) $ by $ N $ (Sine)",
            label = "tab_tune_sine_bias_nuis_m_0",
            out = "Results/Tables/Tuning/Sine/bias_nuis_m_0.tex",
            rownames = FALSE
  )

df_nuis_sine$Case <- factor(df_nuis_sine$Case, levels = vec_cases)
df_nuis_sine$Fun <- factor(df_nuis_sine$Fun, labels = c(TeX("$ l_0(X) $"), TeX("$ m_0(X) $")))

df_nuis_sine %>% 
  mutate(
    Upper = MSE + qnorm(0.975) * Sd_mse,
    Lower = MSE + qnorm(0.025) * Sd_mse
  ) %>% 
  mutate(Lower = if_else(Lower < 0, 0, Lower)) %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE), size = 1.5) + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, alpha = 0.5) +
  geom_line(aes(y = MSE), size = 1) +
  facet_wrap(Fun ~ MLE, 
             scales = "free", 
             labeller = labeller(Fun =label_parsed),
             nrow = 2) + 
  labs(y = "", col = "") + 
  theme_bw()

ggsave("Results/Plots/Tuning/Sine/nuis_mse.png", scale = dbl_scale)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_sine_not$Case <- "Untuned"
df_rate_sine_rcv$Case <- "Tuned"

df_rate_sine <- rbind(
  df_rate_sine_not,
  df_rate_sine_rcv
)

df_rate_sine$Case <- factor(df_rate_sine$Case, levels = vec_cases)

df_rate_sine %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), values_to = "value", names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = Case, lty = type)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 1) +
  facet_wrap( ~ Algorithms, scales = "free", nrow = 1) + 
  labs(y = "", col = "", lty = "") + 
  theme_bw() + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

ggsave("Results/Plots/Tuning/Sine/mse_decomp.png", scale = dbl_scale)

df_rate_sine %>% 
  ggplot(aes(x = N, y = Rate, col = Case)) +
  geom_point(size = 1.5) + 
  geom_line(size = 1) + 
  labs(col = "") +
  theme_bw() + 
  facet_grid(~ Algorithms)

ggsave("Results/Plots/Tuning/Sine/rate_mse.png", scale = dbl_scale)

# Table with Rates
df_rate_sine %>% 
  select(Algorithms, N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Estimated Rates (Sine)",
            label = "tab_tune_sine_rate",
            out = "Results/Tables/Tuning/Sine/rates.tex",
            rownames = FALSE
            )

# Table with MSE by N for differing cases as columns
df_rate_sine %>% 
  select(Algorithms, N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_tune_sine_mse",
            out = "Results/Tables/Tuning/Sine/mse.tex",
            rownames = FALSE
            )

# Table with Squared Bias
df_rate_sine %>% 
  select(Algorithms, N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_tune_sine_sq_bias",
            out = "Results/Tables/Tuning/Sine/sq_bias.tex",
            rownames = FALSE
            )

# Table with Squared Bias
df_rate_sine %>% 
  select(Algorithms, N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_tune_sine_bias",
            out = "Results/Tables/Tuning/Sine/bias.tex",
            rownames = FALSE
            )

# Table with Variance
df_rate_sine %>% 
  select(Algorithms, N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Variance of $ \\hat{\\theta}_0 $ (Sine)",
            label = "tab_tune_sine_variance",
            out = "Results/Tables/Tuning/Sine/variance.tex",
            rownames = FALSE
            )

# Distribution Plot facetted by case
list_dist_sine_not <- distribution(mcs_sine_not$Estimates, plot = FALSE, by = "Algorithms")
list_dist_sine_rcv <- distribution(mcs_sine_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_dist_sine_not$data$Case <- "Untuned"
list_dist_sine_rcv$data$Case <- "Tuned"

list_dist_sine_not$normality$Case <- "Untuned"
list_dist_sine_rcv$normality$Case <- "Tuned"

df_parameter_sine <- rbind(
  list_dist_sine_not$data,
  list_dist_sine_rcv$data
)

df_parameter_sine$Case <- factor(df_parameter_sine$Case, levels = vec_cases)

df_normal_sine <- rbind(
  list_dist_sine_not$normality,
  list_dist_sine_rcv$normality
)

df_normal_sine$Case <- factor(df_normal_sine$Case, levels = vec_cases)

# Filter only Best, GLMNET, Neural Networks, KKNN and SGBM
df_parameter_sine <- df_parameter_sine %>% 
  filter(Algorithms %in% vec_mle_filter)

df_normal_sine <- df_normal_sine %>% 
  filter(Algorithms %in% vec_mle_filter)

ggplot(df_parameter_sine) + 
  geom_histogram(aes(x = parameter_est, y = ..density.., col = Case, fill = Case), 
                 alpha = 0.25, position = "identity") + 
  geom_line(aes(x = x, y = pdf, col = Case), data = df_normal_sine, size = 1) + 
  geom_vline(xintercept = dbl_theta) +
  facet_wrap(Algorithms ~ N, scales = "free_y", nrow = length(vec_mle_filter), 
             strip.position = "right", labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density", col = "", fill = "") +
  theme_bw()

ggsave("Results/Plots/Tuning/Sine/dist.png", scale = dbl_scale)

# Coverage Probabilities
list_cov_prob_sine_not <- cov_prob(mcs_sine_not$Estimates, plot = FALSE, by = "Algorithms")
list_cov_prob_sine_rcv <- cov_prob(mcs_sine_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_cov_prob_sine_not$data$Case <- "Untuned"
list_cov_prob_sine_rcv$data$Case <- "Tuned"

df_cov_prob_sine <- rbind(
  list_cov_prob_sine_not$data,
  list_cov_prob_sine_rcv$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_sine$Case <- factor(df_cov_prob_sine$Case, levels = vec_cases)

df_cov_prob_sine %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(aes(shape = Case), size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`, lty = Case)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "Point", lty = "CI") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  scale_shape_manual(values = c(3, 4)) +
  scale_linetype_manual(values = c("solid", "twodash")) +
  facet_grid(~ Algorithms, scales = "free")

ggsave("Results/Plots/Tuning/Sine/cov_prob.png", scale = dbl_scale)

# Inter --------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_inter_not.RData")
load("Results/Data/Final MCS Data/mcs_inter_rcv.RData")

# Untuned -------------------------------------------------------------------

df_nuis_inter_not <- desc_nuis(mcs_inter_not$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_inter_not <- mcs_inter_not %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_inter_not <- list_rate_inter_not$rate %>% 
  select(-parameter_names)

df_rate_desc_inter_not <- list_rate_inter_not$rate_desc

# Tuned ----------------------------------------------------

df_nuis_inter_rcv <- desc_nuis(mcs_inter_rcv$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_inter_rcv <- mcs_inter_rcv %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_inter_rcv <- list_rate_inter_rcv$rate %>% 
  select(-parameter_names)

df_rate_desc_inter_rcv <- list_rate_inter_rcv$rate_desc

# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_inter <- rbind.data.frame(
  df_nuis_inter_not,
  df_nuis_inter_rcv
)

df_nuis_inter$Case <- rep(vec_cases, each = 2 * int_N_unique * int_mle_unique)

df_nuis_inter$MLE <- str_replace(df_nuis_inter$MLE, vec_mle_fun_unique_title, vec_mle)

# Exporting Table of MSE and Bias
df_nuis_inter_l_0 <- df_nuis_inter %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^l"))

df_nuis_inter_m_0 <- df_nuis_inter %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^m"))

df_nuis_inter_l_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ l_0(X) $ by $ N $ (Interaction)",
            label = "tab_tune_inter_mse_nuis_l_0",
            out = "Results/Tables/Tuning/Inter/mse_nuis_l_0.tex",
            rownames = FALSE
  )

df_nuis_inter_l_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ l_0(X) $ by $ N $ (Interaction)",
            label = "tab_tune_inter_bias_nuis_l_0",
            out = "Results/Tables/Tuning/Inter/bias_nuis_l_0.tex",
            rownames = FALSE
  )

df_nuis_inter_m_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ m_0(X) $ by $ N $ (Interaction)",
            label = "tab_tune_inter_mse_nuis_m_0",
            out = "Results/Tables/Tuning/Inter/mse_nuis_m_0.tex",
            rownames = FALSE
  )

df_nuis_inter_m_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ m_0(X) $ by $ N $ (Interaction)",
            label = "tab_tune_inter_bias_nuis_m_0",
            out = "Results/Tables/Tuning/Inter/bias_nuis_m_0.tex",
            rownames = FALSE
  )

df_nuis_inter$Case <- factor(df_nuis_inter$Case, levels = vec_cases)
df_nuis_inter$Fun <- factor(df_nuis_inter$Fun, labels = c(TeX("$ l_0(X) $"), TeX("$ m_0(X) $")))

df_nuis_inter %>% 
  mutate(
    Upper = MSE + qnorm(0.975) * Sd_mse,
    Lower = MSE + qnorm(0.025) * Sd_mse
  ) %>%
  mutate(Lower = if_else(Lower < 0, 0, Lower)) %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE), size = 1.5) + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, alpha = 0.5) +
  geom_line(aes(y = MSE), size = 1) +
  facet_wrap(Fun ~ MLE, 
             scales = "free", 
             labeller = labeller(Fun =label_parsed),
             nrow = 2) + 
  labs(y = "", col = "") + 
  theme_bw()

ggsave("Results/Plots/Tuning/Inter/nuis_mse.png", scale = dbl_scale)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_inter_not$Case <- "Untuned"
df_rate_inter_rcv$Case <- "Tuned"

df_rate_inter <- rbind(
  df_rate_inter_not,
  df_rate_inter_rcv
)

df_rate_inter$Case <- factor(df_rate_inter$Case, levels = vec_cases)

df_rate_inter %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), values_to = "value", names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = Case, lty = type)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 1) +
  facet_wrap( ~ Algorithms, scales = "free", nrow = 1) + 
  labs(y = "", col = "", lty = "") + 
  theme_bw() + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

ggsave("Results/Plots/Tuning/Inter/mse_decomp.png", scale = dbl_scale)

df_rate_inter %>% 
  ggplot(aes(x = N, y = Rate, col = Case)) +
  geom_point(size = 1.5) + 
  geom_line(size = 1) + 
  labs(col = "") +
  theme_bw() + 
  facet_grid(~ Algorithms)

ggsave("Results/Plots/Tuning/Inter/rate_mse.png", scale = dbl_scale)

# Table with Rates
df_rate_inter %>% 
  select(Algorithms, N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Estimated Rates (Interactioin)",
            label = "tab_tune_inter_rate",
            out = "Results/Tables/Tuning/Inter/rates.tex",
            rownames = FALSE
            )

# Table with MSE by N for differing cases as columns
df_rate_inter %>% 
  select(Algorithms, N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_tune_inter_mse",
            out = "Results/Tables/Tuning/Inter/mse.tex",
            rownames = FALSE
            )

# Table with Squared Bias
df_rate_inter %>% 
  select(Algorithms, N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_tune_inter_sq_bias",
            out = "Results/Tables/Tuning/Inter/sq_bias.tex",
            rownames = FALSE
            )

# Table with Squared Bias
df_rate_inter %>% 
  select(Algorithms, N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_tune_inter_bias",
            out = "Results/Tables/Tuning/Inter/bias.tex",
            rownames = FALSE
            )

# Table with Variance
df_rate_inter %>% 
  select(Algorithms, N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Variance of $ \\hat{\\theta}_0 $ (Interaction)",
            label = "tab_tune_inter_variance",
            out = "Results/Tables/Tuning/Inter/variance.tex",
            rownames = FALSE
            )

# Distribution Plot facetted by case
list_dist_inter_not <- distribution(mcs_inter_not$Estimates, plot = FALSE, by = "Algorithms")
list_dist_inter_rcv <- distribution(mcs_inter_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_dist_inter_not$data$Case <- "Untuned"
list_dist_inter_rcv$data$Case <- "Tuned"

list_dist_inter_not$normality$Case <- "Untuned"
list_dist_inter_rcv$normality$Case <- "Tuned"

df_parameter_inter <- rbind(
  list_dist_inter_not$data,
  list_dist_inter_rcv$data
)

df_parameter_inter$Case <- factor(df_parameter_inter$Case, levels = vec_cases)

df_normal_inter <- rbind(
  list_dist_inter_not$normality,
  list_dist_inter_rcv$normality
)

df_normal_inter$Case <- factor(df_normal_inter$Case, levels = vec_cases)

# Filter only Best, GLMNET, Neural Networks, KKNN and SGBM
df_parameter_inter <- df_parameter_inter %>% 
  filter(Algorithms %in% vec_mle_filter)

df_normal_inter <- df_normal_inter %>% 
  filter(Algorithms %in% vec_mle_filter)

ggplot(df_parameter_inter) + 
  geom_histogram(aes(x = parameter_est, y = ..density.., col = Case, fill = Case), 
                 alpha = 0.25, position = "identity") + 
  geom_line(aes(x = x, y = pdf, col = Case), data = df_normal_inter, size = 1) + 
  geom_vline(xintercept = dbl_theta) +
  facet_wrap(Algorithms ~ N, scales = "free_y", nrow = length(vec_mle_filter), 
             strip.position = "right", labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density", col = "", fill = "") +
  theme_bw()

ggsave("Results/Plots/Tuning/Inter/dist.png", scale = dbl_scale)

# Coverage Probabilities
list_cov_prob_inter_not <- cov_prob(mcs_inter_not$Estimates, plot = FALSE, by = "Algorithms")
list_cov_prob_inter_rcv <- cov_prob(mcs_inter_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_cov_prob_inter_not$data$Case <- "Untuned"
list_cov_prob_inter_rcv$data$Case <- "Tuned"

df_cov_prob_inter <- rbind(
  list_cov_prob_inter_not$data,
  list_cov_prob_inter_rcv$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_inter$Case <- factor(df_cov_prob_inter$Case, levels = vec_cases)

df_cov_prob_inter %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(aes(shape = Case), size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`, lty = Case)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "Point", lty = "CI") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  scale_shape_manual(values = c(3, 4)) +
  scale_linetype_manual(values = c("solid", "twodash")) +
  facet_grid(~ Algorithms, scales = "free")

ggsave("Results/Plots/Tuning/Inter/cov_prob.png", scale = dbl_scale)

# Neural --------------------------------------------------------------------

load("Results/Data/Final MCS Data/mcs_neural_not.RData")
load("Results/Data/Final MCS Data/mcs_neural_rcv.RData")

# Untuned -------------------------------------------------------------------

df_nuis_neural_not <- desc_nuis(mcs_neural_not$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_neural_not <- mcs_neural_not %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_neural_not <- list_rate_neural_not$rate %>% 
  select(-parameter_names)

df_rate_desc_neural_not <- list_rate_neural_not$rate_desc

# Tuned ----------------------------------------------------

df_nuis_neural_rcv <- desc_nuis(mcs_neural_rcv$Measures) %>% 
  arrange(Fun) %>% 
  select(-Variance) %>% 
  rename("MSR. Test" = "Mean_msr_in", "MSR. Validation" = Mean_msr_val) %>% 
  mutate(
    Fun = case_when(
      Fun == "ml_g" ~ "l(X)",
      Fun == "ml_m" ~ "m(X)",
      TRUE ~ NA_character_
    ),
    Mle = str_remove(Mle, "^.*\\.") %>% str_to_title()
  ) %>% 
  rename(MLE = Mle)

# Look at Theta parameter directly
list_rate_neural_rcv <- mcs_neural_rcv %>% 
  pluck("Estimates") %>% 
  mse(by = "Algorithms") %>% 
  estimate_rate(FALSE, FALSE, TRUE)

df_rate_neural_rcv <- list_rate_neural_rcv$rate %>% 
  select(-parameter_names)

df_rate_desc_neural_rcv <- list_rate_neural_rcv$rate_desc

# Joint Tables and Plots --------------------------------------------------

# Plot for Nuisance MSE between cases.
df_nuis_neural <- rbind.data.frame(
  df_nuis_neural_not,
  df_nuis_neural_rcv
)

df_nuis_neural$Case <- rep(vec_cases, each = 2 * int_N_unique * int_mle_unique)

df_nuis_neural$MLE <- str_replace(df_nuis_neural$MLE, vec_mle_fun_unique_title, vec_mle)

# Exporting Table of MSE and Bias
df_nuis_neural_l_0 <- df_nuis_neural %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^l"))

df_nuis_neural_m_0 <- df_nuis_neural %>% 
  ungroup() %>% 
  filter(str_detect(Fun, "^m"))

df_nuis_neural_l_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ l_0(X) $ by $ N $ (Neural Network)",
            label = "tab_tune_neural_mse_nuis_l_0",
            out = "Results/Tables/Tuning/Neural/mse_nuis_l_0.tex",
            rownames = FALSE
  )

df_nuis_neural_l_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ l_0(X) $ by $ N $ (Neural Network)",
            label = "tab_tune_neural_bias_nuis_l_0",
            out = "Results/Tables/Tuning/Neural/bias_nuis_l_0.tex",
            rownames = FALSE
  )

df_nuis_neural_m_0 %>% 
  select(N, MLE, MSE, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = MSE) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ m_0(X) $ by $ N $ (Neural Network)",
            label = "tab_tune_neural_mse_nuis_m_0",
            out = "Results/Tables/Tuning/Neural/mse_nuis_m_0.tex",
            rownames = FALSE
  )

df_nuis_neural_m_0 %>% 
  select(N, MLE, Bias, Case) %>% 
  transform_scientific(int_digits) %>% 
  pivot_wider(names_from = N, values_from = Bias) %>% 
  arrange(MLE, Case) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Bias of $ m_0(X) $ by $ N $ (Neural Network)",
            label = "tab_tune_neural_bias_nuis_m_0",
            out = "Results/Tables/Tuning/Neural/bias_nuis_m_0.tex",
            rownames = FALSE
  )

df_nuis_neural$Case <- factor(df_nuis_neural$Case, levels = vec_cases)
df_nuis_neural$Fun <- factor(df_nuis_neural$Fun, labels = c(TeX("$ l_0(X) $"), TeX("$ m_0(X) $")))

df_nuis_neural %>% 
  mutate(
    Upper = MSE + qnorm(0.975) * Sd_mse,
    Lower = MSE + qnorm(0.025) * Sd_mse
  ) %>%
  mutate(Lower = if_else(Lower < 0, 0, Lower)) %>% 
  ggplot(aes(x = N, col = Case)) + 
  geom_point(aes(y = MSE), size = 1.5) + 
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, alpha = 0.5) +
  geom_line(aes(y = MSE), size = 1) +
  facet_wrap(Fun ~ MLE, 
             scales = "free", 
             labeller = labeller(Fun =label_parsed),
             nrow = 2) + 
  labs(y = "", col = "") + 
  theme_bw()

ggsave("Results/Plots/Tuning/Neural/nuis_mse.png", scale = dbl_scale)

# Plot showing MSE, Squared Bias and Variance for all cases
df_rate_neural_not$Case <- "Untuned"
df_rate_neural_rcv$Case <- "Tuned"

df_rate_neural <- rbind(
  df_rate_neural_not,
  df_rate_neural_rcv
)

df_rate_neural$Case <- factor(df_rate_neural$Case, levels = vec_cases)

df_rate_neural %>% 
  pivot_longer(cols = c(MSE, `Squared Bias`, Variance), values_to = "value", names_to = "type") %>% 
  ggplot(aes(x = N, y = value, col = Case, lty = type)) + 
  geom_point(size = 1.5) + 
  geom_line(size = 1) +
  facet_wrap( ~ Algorithms, scales = "free", nrow = 1) + 
  labs(y = "", col = "", lty = "") + 
  theme_bw() + 
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))

ggsave("Results/Plots/Tuning/Neural/mse_decomp.png", scale = dbl_scale)

df_rate_neural %>% 
  ggplot(aes(x = N, y = Rate, col = Case)) +
  geom_point(size = 1.5) + 
  geom_line(size = 1) + 
  labs(col = "") +
  theme_bw() + 
  facet_grid(~ Algorithms)

ggsave("Results/Plots/Tuning/Neural/rate_mse.png", scale = dbl_scale)

# Table with Rates
df_rate_neural %>% 
  select(Algorithms, N, Rate, Case) %>% 
  filter(!is.na(Rate)) %>% 
  pivot_wider(values_from = Rate, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Estimated Rates (Neuralactioin)",
            label = "tab_tune_neural_rate",
            out = "Results/Tables/Tuning/Neural/rates.tex",
            rownames = FALSE
            )

# Table with MSE by N for differing cases as columns
df_rate_neural %>% 
  select(Algorithms, N, MSE, Case) %>% 
  filter(!is.na(MSE)) %>% 
  pivot_wider(values_from = MSE, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "MSE of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_tune_neural_mse",
            out = "Results/Tables/Tuning/Neural/mse.tex",
            rownames = FALSE)

# Table with Squared Bias
df_rate_neural %>% 
  select(Algorithms, N, `Squared Bias`, Case) %>% 
  filter(!is.na(`Squared Bias`)) %>% 
  pivot_wider(values_from = `Squared Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Squared Bias of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_tune_neural_sq_bias",
            out = "Results/Tables/Tuning/Neural/sq_bias.tex",
            rownames = FALSE
            )

# Table with Squared Bias
df_rate_neural %>% 
  select(Algorithms, N, `Mean Bias`, Case) %>% 
  filter(!is.na(`Mean Bias`)) %>% 
  pivot_wider(values_from = `Mean Bias`, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Mean Bias of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_tune_neural_bias",
            out = "Results/Tables/Tuning/Neural/bias.tex",
            rownames = FALSE
            )

# Table with Variance
df_rate_neural %>% 
  select(Algorithms, N, Variance, Case) %>% 
  filter(!is.na(Variance)) %>% 
  pivot_wider(values_from = Variance, names_from = N) %>% 
  arrange(Algorithms, Case) %>% 
  transform_scientific(int_digits) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, table.placement = "H",
            title = "Variance of $ \\hat{\\theta}_0 $ (Neural Network)",
            label = "tab_tune_neural_variance",
            out = "Results/Tables/Tuning/Neural/variance.tex",
            rownames = FALSE
            )

# Distribution Plot facetted by case
list_dist_neural_not <- distribution(mcs_neural_not$Estimates, plot = FALSE, by = "Algorithms")
list_dist_neural_rcv <- distribution(mcs_neural_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_dist_neural_not$data$Case <- "Untuned"
list_dist_neural_rcv$data$Case <- "Tuned"

list_dist_neural_not$normality$Case <- "Untuned"
list_dist_neural_rcv$normality$Case <- "Tuned"

df_parameter_neural <- rbind(
  list_dist_neural_not$data,
  list_dist_neural_rcv$data
)

df_parameter_neural$Case <- factor(df_parameter_neural$Case, levels = vec_cases)

df_normal_neural <- rbind(
  list_dist_neural_not$normality,
  list_dist_neural_rcv$normality
)

df_normal_neural$Case <- factor(df_normal_neural$Case, levels = vec_cases)

# Filter only Best, GLMNET, Neural Networks, KKNN and SGBM
df_parameter_neural <- df_parameter_neural %>% 
  filter(Algorithms %in% vec_mle_filter)

df_normal_neural <- df_normal_neural %>% 
  filter(Algorithms %in% vec_mle_filter)

ggplot(df_parameter_neural) + 
  geom_histogram(aes(x = parameter_est, y = ..density.., col = Case, fill = Case), 
                 alpha = 0.25, position = "identity") + 
  geom_line(aes(x = x, y = pdf, col = Case), data = df_normal_neural, size = 1) + 
  geom_vline(xintercept = dbl_theta) +
  facet_wrap(Algorithms ~ N, scales = "free_y", nrow = length(vec_mle_filter), 
             strip.position = "right", labeller = labeller(N = label_both)) +
  labs(x = TeX("$\\hat{\\theta}_0$"), y = "Density", col = "", fill = "") +
  theme_bw()

ggsave("Results/Plots/Tuning/Neural/dist.png", scale = dbl_scale)

# Coverage Probabilities
list_cov_prob_neural_not <- cov_prob(mcs_neural_not$Estimates, plot = FALSE, by = "Algorithms")
list_cov_prob_neural_rcv <- cov_prob(mcs_neural_rcv$Estimates, plot = FALSE, by = "Algorithms")

list_cov_prob_neural_not$data$Case <- "Untuned"
list_cov_prob_neural_rcv$data$Case <- "Tuned"

df_cov_prob_neural <- rbind(
  list_cov_prob_neural_not$data,
  list_cov_prob_neural_rcv$data
) %>% 
  filter(`Type of CI` == "95% CI")

df_cov_prob_neural$Case <- factor(df_cov_prob_neural$Case, levels = vec_cases)

df_cov_prob_neural %>% 
  ggplot(aes(x = N, y = `Cov. Prob.`, col = `Width of CI`)) + 
  geom_point(aes(shape = Case), size = 2) +
  geom_errorbar(aes(ymin = `Lower 95%`, ymax = `Upper 95%`, lty = Case)) + 
  geom_hline(aes(yintercept = as.numeric(str_remove(`Type of CI`, "% CI$"))), linetype = "dashed") + 
  labs(y = "Coverage Probability", x = "Sample Size", shape = "Point", lty = "CI") +
  theme_bw() + 
  scale_color_continuous(type = "viridis") + 
  scale_shape_manual(values = c(3, 4)) +
  scale_linetype_manual(values = c("solid", "twodash")) +
  facet_grid(~ Algorithms, scales = "free")

ggsave("Results/Plots/Tuning/Neural/cov_prob.png", scale = dbl_scale)

# Rates of Convergence ----------------------------------------------------

df_rate_sparse_n_net <- df_rate_sparse %>% 
  filter(Algorithms == "N-Net") %>% 
  group_by(Case) %>% 
  select(N, Rate, Case) %>% 
  drop_na() %>% 
  pivot_wider(names_from = c(N), values_from = Rate)

df_rate_sine_n_net <- df_rate_sine %>% 
  filter(Algorithms == "N-Net") %>% 
  group_by(Case) %>% 
  select(N, Rate, Case) %>% 
  drop_na() %>% 
  pivot_wider(names_from = c(N), values_from = Rate)

df_rate_inter_n_net <- df_rate_inter %>% 
  filter(Algorithms == "N-Net") %>% 
  group_by(Case) %>% 
  select(N, Rate, Case) %>% 
  drop_na() %>% 
  pivot_wider(names_from = c(N), values_from = Rate)

df_rate_neural_n_net <- df_rate_neural %>% 
  filter(Algorithms == "N-Net") %>% 
  group_by(Case) %>% 
  select(N, Rate, Case) %>% 
  drop_na() %>% 
  pivot_wider(names_from = c(N), values_from = Rate)

df_rate_n_net <- rbind(
  df_rate_sparse_n_net,
  df_rate_sine_n_net,
  df_rate_inter_n_net,
  df_rate_neural_n_net
)

df_rate_n_net$`Mean Rate` <- rowMeans(df_rate_n_net[, -1])
df_rate_n_net$Nuisance <- rep(
  c("Sparse", "Sine", "Interaction", "Neural Network"),
  each = length(vec_cases)
  )

df_rate_n_net %>% 
  transform_scientific(int_digits) %>% 
  select(Nuisance, Case, `100`, `400`, `Mean Rate`) %>% 
  mutate(Case = as.character(Case)) %>% 
  stargazer(summary = FALSE, 
            table.placement = "H",
            out = "Results/Tables/Tuning/rates_n_net.tex",
            title = "Estimated $ N ^ {-r} $ for MLE N-Net",
            label = "tab_tune_rate_n_net",
            rownames = FALSE
            )
