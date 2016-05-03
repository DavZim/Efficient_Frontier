# load the functions, libraries etc
source("R/functions.R")

set.seed(12345)
df <- data.table(x = rnorm(10000, mean = 0.05, sd = 0.04))
y_mean <- 0.03
y_sd <- 0.02
df[, y1 := rmultvar(x, r = 0.9999, y_mean, y_sd)]
df[, y2 := rmultvar(x, r = 0.5, y_mean, y_sd)]
df[, y3 := rmultvar(x, r = 0, y_mean, y_sd)]
df[, y4 := rmultvar(x, r = -0.5, y_mean, y_sd)]
df[, y5 := rmultvar(x, r = -0.75, y_mean, y_sd)]
df[, y6 := rmultvar(x, r = -0.9999, y_mean, y_sd)]


dfl <- melt(df)

dfx <- data.table(date = 1:nrow(df),
                  ticker = dfl$variable,
                  ret = dfl$value)

p1 <- plotCombinations(dfx, tickers = c("x", "y1"))
p2 <- plotCombinations(dfx, tickers = c("x", "y2"))
p3 <- plotCombinations(dfx, tickers = c("x", "y3"))
p4 <- plotCombinations(dfx, tickers = c("x", "y4"))
p5 <- plotCombinations(dfx, tickers = c("x", "y5"))
p6 <- plotCombinations(dfx, tickers = c("x", "y6"))

p_all <- grid.arrange(p1, p2, p3, p4, p5, p6)

ggsave(filename =  "../correlation_example.png", p_all, scale = 1.5)
