require(tidyverse)
require(quantmod)
require(forecast)

save.image("stock-selector.RData")
load("stock-selector.RData")


# scrape stock symbols ----------------------------------------------------

require(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
sp500_wiki <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table()
sp500_symbols <- sp500_wiki[[1]][1] %>% unlist() %>% unname()


# pull stock history ------------------------------------------------------

sp500 <- new.env()

for(i in sp500_symbols) {
  cat("Downloading time series for symbol '", i, "' ...\n", sep="")
  status <- tryCatch(
    getSymbols(i, env=sp500, src="yahoo", from="2000-01-01", auto.assign=TRUE),
    error=identity)
  if(inherits(status, "error"))
    cat("Symbol '", i, "' FAILED!\n", sep="")
}

browseEnv(sp500)
rm(i)
rm(status)


# master xts with monthly adjusted price ----------------------------------

sp500.xts <- xts(order.by=index(apply.monthly(sp500[["AAPL"]], FUN=function(x) { mean(Ad(x)) })) )
for(i in ls(sp500)) {
  # sp500[[i]] <- adjustOHLC(sp500[[i]], use.Adjusted=TRUE)  # splits and dividends 
  sp500.xts <- merge(sp500.xts, apply.monthly(sp500[[i]], FUN=function(x) { mean(Ad(x)) } ))
  cat("Finished ", i, "...\n")
}
colnames(sp500.xts) <- ls(sp500) %>% paste0(.,".Adjusted")
sp500.xts <- sp500.xts[-nrow(sp500.xts),]  # drop last row, incomplete month
periodicity(sp500.xts)
rm(i)


