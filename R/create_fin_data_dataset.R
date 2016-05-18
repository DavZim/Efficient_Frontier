library(quantmod)
library(data.table)

lag <- function(x) c(NA, x[1:(length(x) - 1)])

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

ticker_sel <- c("IBM", "GOOG", "JPM")
dt <- getData(tickers = ticker_sel)
dt[, week_id := paste(month(date), year(date), sep = "-")]

dt[, ':=' (mt_price = mean(price),
           mt_date = min(date)), by = c("ticker", "week_id")]

dt <- dt[, .(date = mt_date, ticker, price = mt_price)]
dt <- unique(dt)

write.csv(dt, file = "data/fin_data.csv", row.names = F)
