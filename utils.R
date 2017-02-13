
# tibble wrapper for getSymbols -------------------------------------------
 
get_stock_prices <- function(ticker, return_format="tibble", ...) {
  require(tidyverse)
  require(lubridate)
  stock_prices_xts <- getSymbols(Symbols=ticker, auto.assign=FALSE, ...)
  names(stock_prices_xts) <- c("Open","High","Low","Close","Volume","Adjusted")
  
  # return xts format if not specified
  if (return_format == "tibble") {
    stock_prices <- stock_prices_xts %>%
      as_tibble() %>%
      rownames_to_column(var="Date") %>%
      mutate(Date=ymd(Date))
  } else {
    stock_prices <- stock_prices_xts
  }
  cat(ticker, " pulled in ", nrow(stock_prices), " rows\n", sep="")
  return(stock_prices)
}


# tibble wrapper for periodReturn -----------------------------------------

get_log_returns <- function(x, return_format="tibble", period="daily", ...) {
  require(tidyverse)
  require(lubridate)
  if (!is.xts(x)) {
    x <- xts(x[,-1], order.by=x$Date)
  }

  log_returns_xts <- periodReturn(x=x$Adjusted, type="log", period=period, ...)
  names(log_returns_xts) <- "Log.Returns"
  
  # return xts format if not specified
  if (return_format == "tibble") {
    log_returns <- log_returns_xts %>%
      as_tibble() %>%
      rownames_to_column(var="Date") %>%
      mutate(Date=ymd(Date))
  } else {
    log_returns <- log_returns_xts
  }
  return(log_returns)
}