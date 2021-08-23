vec_N <- c(100, 125, 150, 175, 200, 400)
int_Rep <- 10000
df_mse_x <- expand.grid(N = vec_N, R = 1:int_Rep) %>% 
  pmap(function(N, R){
    x <- rnorm(N)
    c(mse_mean = mean(x) ^ 2, mse_median = median(x) ^ 2, N = N, R = R)
  }) %>% 
  map_df(~ .x) %>% 
  group_by(N) %>% 
  summarise(
    mse_mean = mean(mse_mean),
    mse_median = median(mse_median)
    )
df_mse_x$mse_mean[6] / df_mse_x$mse_mean[-6]
df_mse_x$mse_median[6] / df_mse_x$mse_median[-6]

log(vec_ratios) / log(vec_N[])