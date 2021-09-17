library(DoubleML)
library(magrittr)
library(mlr3verse)
library(plotly)
library(tidyverse)

source("Code/DGP class.R")
source("Code/DGP functions.R")
source("Code/Utils.R")

# Create DGP objects
set.seed(4)
int_dim <- 2
int_N <- 10000
int_samples <- 1000
vec_beta <- runif(int_dim, -1, 1)
vec_beta_sparse <- vec_beta
vec_gamma <- runif(int_dim, -1, 1)
vec_gamma_sparse <- vec_gamma
vec_mu <- rep(0, int_dim)
mat_A <- matrix(runif(int_dim ^ 2, -1, 1), ncol = int_dim)
mat_Sigma <- cov2cor(t(mat_A) %*% mat_A)
solve(mat_Sigma) # Is invertible

# Investigating Signal to Noise Ratio
df_X <- data.frame(MASS::mvrnorm(int_N, vec_mu, mat_Sigma))
cov(df_X)

# Set SD of E equal 1
sd_E <- 1
var_E <- 1
vec_E <- rnorm(int_N, 0, sd_E)

# Signal to noise ratio is supposed to be 4
g_sparse <- dgp_sparse(df_X, vec_E, vec_beta_sparse, "regression", 2.1) - vec_E
var(g_sparse) / var_E
fac_g_sparse <- 2.1

g_sine <- dgp_sine(df_X, vec_E, vec_beta, "regression", 3.1) - vec_E
var(g_sine) / var_E
fac_g_sine <- 3.1

g_poly <- dgp_poly(df_X, vec_E, 2, type = "regression", factor_signal = 0.55) - 
  vec_E
var(g_poly) / var_E
fac_g_poly <- 0.55

g_nnet <- dgp_nnet(X = df_X, E = vec_E, hidden_units = 5, type = "regression", 
                   factor_signal = 1.55) - vec_E
var(g_nnet) / var_E
fac_g_nnet <- 1.55

# Repeat for classification task
# Also want a 50% share of treated
# Shift the mean vector so that the share is nearly 50%
sd_U <- 1
var_U <- 1
vec_U <- rnorm(int_N, sd = sd_U)

# Sparse
m_sparse <- dgp_sparse(df_X, vec_U, vec_gamma_sparse, "classification", 2.9) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_sparse) / var_U

fac_m_sparse <- 2.9
rbinom(int_N, 1, sigmoid(m_sparse + vec_U)) %>% table()

# Sine
m_sine <- dgp_sine(df_X, vec_U, vec_gamma, "classification", 3.6) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_sine) / var_U

fac_m_sine <- 3.6
rbinom(int_N, 1, sigmoid(m_sine + vec_U)) %>% table()

# Polynomial
int_order <- 2
m_poly <- dgp_poly(df_X, vec_U, order = int_order, 
                   type = "classification", factor_signal = 0.55) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_poly) / var_U

fac_m_poly <- 0.55
rbinom(int_N, 1, sigmoid(m_poly + vec_U - 1.1)) %>% table()
scale_m_poly <- -1.1

# Neural Net
int_hidden_units <- 5
dbl_beta_0 <- -3.4

m_nnet <- dgp_nnet(df_X, vec_U, type = "classification", 
                   beta_0 = dbl_beta_0, factor_signal = 1.55) %>% 
  sigmoid(inverse = TRUE) %>% 
  subtract(vec_U)
var(m_nnet) / var_U

fac_m_nnet <- 1.55
rbinom(int_N, 1, sigmoid(m_nnet + vec_U)) %>% table()

# Create Contour plots
df_X_viz <- cbind.data.frame(
  X1 = seq(-2, 2, by = 0.05),
  X2 = seq(-2, 2, by = 0.05)
) %>% 
  expand.grid()

df_X_viz$E <- rnorm(nrow(df_X_viz), sd = sd_E)

df_X_viz$Sparse <- dgp_sparse(df_X_viz[, 1:2], df_X_viz[, 3], 
                              beta = vec_beta, type = "regression", 
                              factor_signal = fac_g_sparse)
df_X_viz$Sine <- dgp_sine(df_X_viz[, 1:2],  df_X_viz[, 3], 
                          beta = vec_beta, type = "regression", 
                          factor_signal = fac_g_sine)
df_X_viz$Polynomial <- dgp_poly(df_X_viz[, 1:2],  df_X_viz[, 3], 
                                type = "regression", 
                                order = int_order, factor_signal = fac_g_poly)
df_X_viz$`Neural Network` <- dgp_nnet(df_X_viz[, 1:2],  df_X_viz[, 3], 
                                      beta_0 = dbl_beta_0, 
                                      type = "regression", 
                                      factor_signal = fac_g_nnet)

# Add also noise
df_X_noise_viz <- df_X_viz
df_X_viz <- df_X_viz %>% 
  mutate(across(-c(1:3), ~ .x - E))

df_dgps_viz <- df_X_viz %>% 
  pivot_longer(cols = -c(1:3), names_to = "DGP", values_to = "Y")

df_dgps_viz %>% 
  filter(DGP == "Sparse") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Sparse.png", scale = 1.5)

df_dgps_viz %>% 
  filter(DGP == "Sine") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Sine.png", scale = 1.5)

df_dgps_viz %>% 
  filter(DGP == "Polynomial") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Polynomial.png", scale = 1.5)

df_dgps_viz %>% 
  filter(DGP == "Neural Network") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Neural Network.png", scale = 1.5)

# Again with noise
df_dgps_viz_noise %>% 
  filter(DGP == "Sparse") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Sparse with Noise.png", scale = 1.5)

df_dgps_viz_noise %>% 
  filter(DGP == "Sine") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Sine with Noise.png", scale = 1.5)

df_dgps_viz_noise %>% 
  filter(DGP == "Polynomial") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Polynomial with Noise.png", scale = 1.5)

df_dgps_viz_noise %>% 
  filter(DGP == "Neural Network") %>% 
  ggplot(aes(X1, X2, z = Y)) + 
  geom_contour() + 
  geom_contour_filled() +
  theme_bw() +
  labs(x = "x", y = "y", fill = "Level")

ggsave("Results/Plots/DGP Neural Network with Noise.png", scale = 1.5)

# Using Plotly
plot_3d_surface <- function(data, x, y, z, xlab = "x", ylab = "y", zlab = "z"){
  
  x <- enquo(x)
  y <- enquo(y)
  z <- enquo(z)
  
  # Get grid size
  int_grid_size <- data %>% nrow() %>% sqrt()
  
  mat_data <- matrix(data %>% pull(!!z), nrow = int_grid_size)
  vec_x <- data %>% 
    pull(!!x) %>% 
    unique()
  vec_y <- data %>% 
    pull(!!y) %>% 
    unique()
  
  # Set x and y axis labels manually
  int_n_ticks_x <- vec_x %>% length() %>% divide_by(10) %>% floor()
  int_n_ticks_y <- vec_y %>% length() %>% divide_by(10) %>% floor()
  vec_tickvals_x <- seq(0, 10 * int_n_ticks_x, 10)
  vec_tickvals_y <- seq(0, 10 * int_n_ticks_y, 10)
  vec_ticktext_x <- vec_x[vec_tickvals_x + 1]
  vec_ticktext_y <- vec_y[vec_tickvals_y + 1]
  
  plot_ly(z = ~ mat_data) %>% 
    add_surface(showscale = FALSE) %>% 
    layout(
      legend = list(title = "Y"),
      scene = list(
        xaxis = list(
          title = "X.1", 
          tickvals = vec_tickvals_x, 
          ticktext = vec_ticktext_x
          ),
        yaxis = list(
          title = "X.2", 
          tickvals = vec_tickvals_y,
          ticktext = vec_ticktext_y
          ),
        zaxis = list(title = "Y")
      )
    )
  
}

plot_3d_surface(df_X_viz, X1, X2, Sparse)
plot_3d_surface(df_X_viz, X1, X2, Sine)
plot_3d_surface(df_X_viz, X1, X2, Polynomial)
plot_3d_surface(df_X_viz, X1, X2, `Neural Network`)
plot_3d_surface(df_X_noise_viz, X1, X2, Sparse)
plot_3d_surface(df_X_noise_viz, X1, X2, Sine)
plot_3d_surface(df_X_noise_viz, X1, X2, Polynomial)
plot_3d_surface(df_X_noise_viz, X1, X2, `Neural Network`)
