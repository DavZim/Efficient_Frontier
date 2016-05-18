# load the functions, libraries etc
source("R/functions.R")

set.seed(12345)
df <- data.table(x = rnorm(10000, mean = 0.07, sd = 0.05))

y_mean <- 0.03
y_sd <- 0.02

z_mean <- 0.04
z_sd <- 0.03

df[, y := rmultvar(x, r = 0, y_mean, y_sd)]
df[, z := rmultvar(x, r = 0, z_mean, z_sd)]

write.csv(df, file = "../data/mult_assets.csv")
