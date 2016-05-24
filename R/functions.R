library(data.table)
library(ggplot2)
library(gridExtra)
library(magrittr)
library(scales)

#' Downloads Yahoo Stock Data
#' The function downloads adjusted stock prices for given stocks
#'
#' @param tickers a vector of tickers (yahoo-syntax)
#' @param long a boolean if the data should be returned in a wide or long format
#'
#' @return a data.table containing the date, ticker and prices (long) or date and one column with prices per ticker (wide)
#' @export
#'
#' @examples
#'
#' getData(tickers = c("IBM", "MSFT"), long = T)
#'
getData <- function(tickers, long = T) {
  # iterate through the tickers and get the last adjusted price in a data.table
  res <- lapply(tickers, function(x) {

    dat <- getSymbols(x, from = "2000-01-01", auto.assign = F)

    dt <- data.table(date = as.Date(index(dat)),
                     ticker = x,
                     price = as.numeric(Ad(dat)))
    return(dt)
  })

  # combine the list to one data.table
  res <- rbindlist(res)

  # cast the data if the user wants to get the data in a wide format
  if (!long) {
    res <- dcast(res, date ~ ticker)
  }

  return(res)
}


#' Calculate correlated Random Values
#'
#' The function calculates random values that are correlated to an input
#' @param x a vector of values
#' @param r the target correlation
#' @param y_mean the mean of the random values
#' @param y_sd the standard deviation of the random values
#'
#' @return a vector with the same length as x that contains random values
#' @export
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(1000)
#' y <- rmultvar(x, r = 0.7, y_mean = 10.5, y_sd = 1)
#'
#' # test
#' cor(x, y) # 0.727
#' mean(y) # 10.5
#' sd(y) # 1.047
#'
rmultvar = function(x, r, y_mean, y_sd){
  # inspired by gung
  # http://stats.stackexchange.com/questions/38856/how-to-generate-correlated-random-numbers-given-means-variances-and-degree-of

  x2 <- (x -  mean(x)) / sd(x)
  r2 <- r ^ 2
  ve <- 1 - r2
  SD <- sqrt(ve)
  e <- rnorm(length(x2), mean = 0, sd = SD)
  y <- r*x2 + e

  y <- (y - mean(y)) * y_sd + y_mean
  return(y)
}


#' Calculate the abcd-list for the Efficient Frontier
#' The function calculates A, B, C, and Delta for a given set of returns
#' @param x a data.table with the columsn date, ticker, and returns
#'
#' @return a list of alpha, beta, gamma, and delta that can be used to compute the efficient frontier
#' @export
#'
#' @examples
#'
calcEFParamsLong <- function(x) {
  x <- x[is.finite(ret), .(date, ticker, ret)]

  rets <- dcast(x, formula = date ~ ticker, value.var = "ret")[, date := NULL]
  
  retbar <- colMeans(rets, na.rm = T)
  covs <- var(rets, na.rm = T) # calculates the covariance of the returns
  invS <- solve(covs)
  i <- matrix(1, nrow = length(retbar))
  
  alpha <- t(i) %*% invS %*% i
  beta <- t(i) %*% invS %*% retbar
  gamma <- t(retbar) %*% invS %*% retbar
  delta <- alpha * gamma - beta * beta
  
  retlist <- list(alpha = as.numeric(alpha),
                  beta = as.numeric(beta),
                  gamma = as.numeric(gamma),
                  delta = as.numeric(delta))
  
  return(retlist)
}


#' Calculate the Efficient Frontier Values
#' The function calculates the y-values for an efficient frontier for given x-values
#' @param xvals a vector of x-values 
#' @param abcd a list of the values for the efficient frontier as outputted by calcEFParamsLong
#' @param upper a boolean value if the upper (efficient) or lower (inefficient) frontier should be returned
#'
#' @return a vector of y-values for the efficient frontier
#' @export
#'
#' @examples
calcEFValues <- function(x, abcd, upper = T) {
  alpha <- abcd$alpha
  beta <- abcd$beta
  gamma <- abcd$gamma
  delta <- abcd$delta
  
  if (upper) {
    retval <- beta / alpha + sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  } else {
    retval <- beta / alpha - sqrt((beta / alpha) ^ 2 - (gamma - delta * x ^ 2) / (alpha))
  }
  
  return(retval)
}


#' Creates a plot of different efficient frontiers given the tickers
#'
#' @param dat a data.table containing at least two columns ticker and ret (returns)
#' @param tickers a vector of tickers that are compared (length 2)
#'
#' @return a ggplot of the two points
#' @export
#'
#' @examples
plotCombinations <- function(dat, tickers) {
  dat <- dat[ticker %in% tickers]

  list1 <- calcABCDs(dat)
  tabs <- dat[, .(mean = mean(ret), sd = sd(ret)), by = "ticker"]

  dfUpper <- data.table(x = seq(from = 0, to = max(tabs$sd), length.out = 10000))
  dfLower <- data.table(x = seq(from = 0, to = min(tabs$sd), length.out = 10000))

  dfUpper[, y := calcEffPoints(x, list1, upper = T)]
  dfLower[, y := calcEffPoints(x, list1, upper = F)]

  # trim values below the lower point
  y_min <- dat[, mean(ret), by = ticker][, min(V1)]

  dfUpper <- dfUpper[y >= y_min]
  dfLower <- dfLower[y >= y_min]

  correl <- cor(dat[ticker == tickers[1], ret], dat[ticker == tickers[2], ret]) %>% round(2)

  ggplot() +
    geom_line(data = dfUpper, aes(x = x, y = y), linetype = "dashed") +
    geom_line(data = dfLower, aes(x = x, y = y), linetype = "dashed") +
    geom_point(data = tabs, aes(x = sd, y = mean), color = "red", shape = 16) +
    theme_bw() + geom_hline(yintercept = 0, color = "darkgrey") +
    geom_vline(xintercept = 0, color = "darkgrey") +
    ggtitle(paste0("Correlation: ", correl)) +
    xlab("Volatility") + ylab("Expected Returns") +
    scale_y_continuous(label = percent, limits = c(0, max(tabs$mean) * 1.2)) +
    scale_x_continuous(label = percent, limits = c(0, max(tabs$sd) * 1.2))

}
