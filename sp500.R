source("utils.R")

require(rvest)
require(tidyverse)
require(forcats)
require(stringr)
require(quantmod)


# scrape sp500 stock info from wikipedia ----------------------------------------

sp_500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
  html_node("table.wikitable") %>%
  html_table() %>%
  select(`Ticker symbol`, Security, `GICS Sector`, `GICS Sub Industry`) %>%
  as_tibble()
names(sp_500) <- names(sp_500) %>% str_to_lower %>% make.names

sp_500 %>%
  lapply(function(x) x %>% unique %>% length) %>% unlist

sp_500 %>% 
  group_by(security) %>%
  summarise(count = n()) %>%
  filter(count > 1)
sp_500 %>% filter(security == "Under Armour")
sp_500 <- sp_500 %>% filter(ticker.symbol != "UAA")
sp_500[grep("\\.", sp_500$ticker.symbol),]
sp_500 <- sp_500 %>% filter(ticker.symbol != "BF.B")  # errors out on yahoo

# bar chart: stock count by sector
sp_500 %>%
  group_by(gics.sector) %>%
  summarise(count = n()) %>%
  ggplot(aes(x=gics.sector %>% fct_reorder(count), y=count)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=count), size=3, nudge_y=4, nudge_x=0.1) +
    scale_y_continuous(limits = c(0,100)) + 
    ggtitle(label = "Sector Frequency in SP500") +
    xlab(label = "GICS Sector") +
    theme(plot.title = element_text(size=16)) +
    coord_flip()

"AAPL" %>% get_stock_prices(return_format = "tibble") %>%
  get_log_returns(return_format = "tibble")


# download quantmod daily data --------------------------------------------

sp_500 <- sp_500 %>%
  # takes a few minutes to run
  mutate(
    stock.prices = map(ticker.symbol,
                       function(.x) get_stock_prices(.x, return_format="tibble",
                                                     src = "yahoo", 
                                                     from = "2000-01-01")
                     ),
    log.returns = map(stock.prices,
                      function(.x) get_log_returns(.x, return_format="tibble")),
    mean.log.returns = map_dbl(log.returns, ~ mean(.$Log.Returns)),
    sd.log.returns = map_dbl(log.returns, ~ sd(.$Log.Returns)),
    n.trade.days = map_dbl(stock.prices, nrow)
  )

sp_500 %>% select(ticker.symbol, stock.prices:log.returns)
sp_500$stock.prices[[1]]
sp_500$log.returns[[1]]
colnames(sp_500)
sp_500 %>% select(ticker.symbol, mean.log.returns:n.trade.days)


# top stocks by mean and sd -----------------------------------------------

sp_500 %>%
  filter(mean.log.returns > 0.0008,
         sd.log.returns < 0.025,
         n.trade.days > 1000) %>%
  select(ticker.symbol, security, mean.log.returns:n.trade.days) %>%
  arrange(mean.log.returns %>% desc)


# analyze correlations ----------------------------------------------------

limit <- 30
sp_500_hp <- sp_500 %>%
  filter(n.trade.days > 1000,
         sd.log.returns < 0.04) %>%
  mutate(rank = mean.log.returns %>% desc %>% min_rank) %>%
  filter(rank <= limit) %>%
  select(ticker.symbol, rank, mean.log.returns, sd.log.returns, log.returns)
sp_500_hp

# flatten tibble
sp_500_hp_unnest <- sp_500_hp %>%
  select(ticker.symbol, log.returns) %>%
  unnest()
sp_500_hp_unnest

# make into wide data table
sp_500_hp_spread <- sp_500_hp_unnest %>%
  spread(key = ticker.symbol, value = Log.Returns) %>%
  na.omit()
sp_500_hp_spread

sp_500_hp_cor <- sp_500_hp_spread %>%
  select(-Date) %>%
  cor()
sp_500_hp_cor[1:6, 1:6]

require(corrplot)
sp_500_hp_cor %>%
  corrplot(order = "hclust", addrect = 11)


# log return mean and sd by year ------------------------------------------

sp_500.yr <- tibble(
  ticker = character(0), 
  year = numeric(0), 
  mean.log.returns = numeric(0), 
  sd.log.returns = numeric(0), 
  n.trade.days = numeric(0))

for (i in 1:nrow(sp_500)) {
  for (yr in 2000:2017) {
    tmp <- sp_500$log.returns[[i]] %>% filter(year(Date) == yr)
    sp_500.yr %>% 
      add_row(ticker = sp_500$ticker.symbol[i], 
              year = yr, 
              mean.log.returns = mean(tmp$Log.Returns), 
              sd.log.returns = sd(tmp$Log.Returns),
              n.trade.days = nrow(tmp))
  }
}

