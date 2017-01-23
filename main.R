require(magrittr)
require(quantmod)
require(forecast)

save.image("stock-selector.RData")


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

