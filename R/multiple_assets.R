library(data.table)
library(ggplot2)

# load the data
link <- "https://raw.githubusercontent.com/DavZim/Efficient_Frontier/master/data/mult_assets.csv"
df <- data.table(read.csv(link))

df_table <- melt(df)[, .(mean = mean(value), sd = sd(value)), by = variable]

er_x <- mean(df$x)
er_y <- mean(df$y)
er_z <- mean(df$z)
sd_x <- sd(df$x)
sd_y <- sd(df$y)
sd_z <- sd(df$z)
cov_xy <- cov(df$x, df$y)
cov_xz <- cov(df$x, df$z)
cov_yz <- cov(df$y, df$z)

# two assets
two_assets_seq <- seq(from = 0, to = 1, length.out = 1000)

two <- data.table(wx = two_assets_seq,
                     wy = 1 - two_assets_seq)

two[, ':=' (er_p = wx * er_x + wy * er_y,
            sd_p = sqrt(wx^2 * sd_x^2 +
                           wy^2 * sd_y^2 +
                           2 * wx * (1 - wx) * cov_xy))]

plot_two <- ggplot() +
  geom_point(data = two, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = df_table[variable != "z"],
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  theme_bw() + ggtitle("Possible portfolios with two risky assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)

ggsave(plot_two, file = "two_assets.png", scale = 1, dpi = 600)

# three assets
three_assets_seq <- seq(from = 0, to = 1, length.out = 500)

three <- data.table(wx = rep(three_assets_seq, each = length(three_assets_seq)),
                      wy = rep(three_assets_seq, length(three_assets_seq)))

three[, wz := 1 - wx - wy]

three[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                sd_p = sqrt(wx^2 * sd_x^2 +
                              wy^2 * sd_y^2 +
                              wz^2 * sd_z^2 +
                              2 * wx * wy * cov_xy +
                              2 * wx * wz * cov_xz +
                              2 * wy * wz * cov_yz))]

three <- three[wx >= 0 & wy >= 0 & wz >= 0]

plot_three <- ggplot() +
  geom_point(data = three, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = df_table, aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  theme_bw() + ggtitle("Possible portfolios with three risky assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("red", "blue", "yellow"),
                        name = expression(omega[x] - omega[z]), labels = percent)

ggsave(plot_three, file = "three_assets.png", scale = 1, dpi = 600)


#
# # with efficient frontier
# calcABCDList <- function(rets) {
#   library(magrittr)
#   retbar <- colMeans(rets, na.rm = T)
#   covs <- var(rets, na.rm = T) # calculates the covariance of the returns
#   invS <- solve(covs)
#
#   i <- matrix(1, nrow = length(retbar))
#
#   A <- t(i) %*% invS %*% i %>% as.numeric
#   B <- t(i) %*% invS %*% retbar %>% as.numeric
#   C <- t(retbar) %*% invS %*% retbar %>% as.numeric
#   D <- A * C - B * B
#   return(list(A = A, B = B, C = C, D = D))
# }
# calcEffPoints <- function(x, ABCD, upper = T) {
#
#   if (upper) {
#     ABCD$B / ABCD$A + sqrt((ABCD$B / ABCD$A) ^ 2 - (ABCD$C - ABCD$D * x ^ 2) / (ABCD$A))
#   } else {
#     ABCD$B / ABCD$A - sqrt((ABCD$B / ABCD$A) ^ 2 - (ABCD$C - ABCD$D * x ^ 2) / (ABCD$A))
#   }
# }
#
# abcds <- calcABCDList(df)
#
# df_table <- melt(df)[, .(er = mean(value),
#                          sd = sd(value)), by = variable]
#
#
# plot_ef <- ggplot(df_table, aes(x = sd, y = er)) +
#   # add the possible portfoliols
#   geom_point(data = three, aes(x = sd_p, y = er_p, color = wx - wz)) +
#   # add the stocks
#   geom_point(size = 4, color = "red", shape = 18) +
#   # add the upper efficient frontier
#   stat_function(fun = calcEffPoints, args = list(ABCD = abcds, upper = T), n = 10000,
#                 color = "red") +
#   # add the lower "efficient" frontier
#   stat_function(fun = calcEffPoints, args = list(ABCD = abcds, upper = F), n = 10000,
#                 color = "blue") +
#   # miscellaneous, design functions
#   theme_bw() + ggtitle("Efficient frontier with short-selling") +
#   xlab("Volatility") + ylab("Expected Returns") +
#   scale_y_continuous(label = percent, limits = c(0, max(df_table$er) * 1.2)) +
#   scale_x_continuous(label = percent, limits = c(0, max(df_table$sd) * 1.2)) +
#   scale_color_gradient(name = expression(omega[x] - omega[z]), labels = percent,
#                        low = "#FFFF00", high = "#0000FF")
#
# ggsave(plot_ef, file = "plot_efficient_frontier.png", scale = 1, dpi = 600)
