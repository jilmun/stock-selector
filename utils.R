
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

get_post_change <- function(stock_returns_input, change_threshold) {
  
  if (change_threshold < 0) {
    threshold_flag <- (stock_returns_input %>% summarise_all(min)) < change_threshold
  } else {
    threshold_flag <- (stock_returns_input %>% summarise_all(max)) > change_threshold
  }
  threshold_flag[1] <- TRUE  # keep Date column
  
  # redefine data with only stocks breaching threshold
  stock_returns <- stock_returns_input %>%
    select(which(threshold_flag))
  
  shifted_returns <- tibble()
  
  for (i in colnames(stock_returns)[-1]) {
    breached_flag <- FALSE
    stock.i <- stock_returns %>% select("Date", i) %>% na.omit()
    names(stock.i) <- c("Date", "Returns")
    
    # pull only rows on or after reaching threshold
    # only considers first case of breaching threshold
    # entire table is returned if nothing reaches threshold
    if (change_threshold < 0) {
      stock.i.shifted <- stock.i %>% 
        slice(if(any(Returns < change_threshold)) 
          which.max(Returns < change_threshold):nrow(stock.i) else row_number())
      if (stock.i.shifted[1,2] < change_threshold) {
        breached_flag <- TRUE
      }
    } else {
      stock.i.shifted <- stock.i %>% 
        slice(if(any(Returns > change_threshold)) 
          which.max(Returns > change_threshold):nrow(stock.i) else row_number())
      if (stock.i.shifted[1,2] > change_threshold) {
        breached_flag <- TRUE
      }
    }
    
    # adjust dates to days after breaching threshold, breached day is 0
    stock.i.shifted <- stock.i.shifted %>%
      mutate(Day = 1:nrow(stock.i.shifted) - 1) %>%
      select(Day, Returns)
    
    if (breached_flag == TRUE) {
      names(stock.i.shifted)[2] <- i
      if (nrow(shifted_returns) == 0) {
        shifted_returns <- stock.i.shifted
      } else {
        suppressMessages(
          shifted_returns <- full_join(shifted_returns, stock.i.shifted))
      }
    } 
  }
  return(shifted_returns)
}