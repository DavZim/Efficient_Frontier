# load the functions, libraries etc
source("R/functions.R")

ticker_sel <- c("IBM", "GOOG", "JPM")
dt <- getData(tickers = ticker_sel)
dt[, week_id := paste(month(date), year(date), sep = "-")]

dt[, ':=' (mt_price = mean(price),
           mt_date = min(date)), by = c("ticker", "week_id")]

dt <- dt[, .(date = mt_date, ticker, price = mt_price)]
dt <- unique(dt)

write.csv(dt, file = "../data_eff_front.csv", row.names = F)
