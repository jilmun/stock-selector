require(tidyquant)
options(stringsAsFactors = FALSE)

setwd("C:/Users/pistachio/Projects/stock-selector")
curr_date <- as.Date("2017-04-28")

curr_portfolio <- data.frame(
  symbol = c("XLY", "IVV", "VOE", "ITM", "IJR", "VIEIX", "VTSNX"),
  shares = c( 1.346, 0.335, 0.297, 0.415, 0.064, 0.674,   0.113),
  type   = c("inv", "ret", "inv", "inv", "inv", "ret",   "ret")
)
sapply(curr_portfolio, class)

hist_daily <- curr_portfolio$symbol %>%
  tq_get(get = "stock.prices", from = "2010-01-01", to = curr_date)

write_csv(hist_daily, "hist_daily.csv")

# current portfolio value
curr_portfolio_value <- hist_prices %>% 
  filter(date == curr_date) %>%
  select(symbol, close) %>%
  left_join(curr_portfolio)

sum(curr_portfolio_value$close * curr_portfolio_value$shares)


# manual columns ----------------------------------------------------------

hist_prices <- hist_daily %>%
  group_by(symbol) %>%
  mutate(close.lag1 = lag(close, 1),
         adjusted.lag1 = lag(adjusted, 1)) %>%
  slice(-1) %>%
  ungroup() %>%
  mutate(high.open = (high - open) / open,
         low.open = (low - open) / open,
         close.open = (close - open) / open,
         high.low = low.open + high.open,
         high.close1 = (high - close.lag1) / close.lag1,
         low.close1 = (low - close.lag1) / close.lag1,
         return = adjusted / adjusted.lag1 - 1
  )

hist_prices[1:3, 8:15]
hist_prices[1844:1847, 8:15]  # check IVV

# volatility over time
hist_prices %>%
  filter(symbol=="XLY") %>%
  ggplot(aes(x = month(date), y = return)) +
  geom_point() + facet_wrap(~year(date))
hist_prices %>%
  filter(symbol=="XLY") %>%
  ggplot(aes(x = year(date), y = return, group=year(date))) +
  geom_boxplot()

# exit strategy
xly <- hist_prices %>%
  filter(symbol == "XLY")

probs.lo <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9)
probs.hi <- rev(1-probs.lo)

xly %>%
  mutate(year = year(date),
         yearmo = year(date)*100 + month(date)) %>%
  group_by(year) %>%
  do(data.frame(t(quantile(.$high.close1, probs=probs.hi))))
xly$high.close1 %>% quantile(probs=probs.hi)

xly.lo <- xly %>%
  mutate(year = year(date),
         yearmo = year(date)*100 + month(date)) %>%
  group_by(year) %>%
  do(data.frame(t(quantile(.$low.close1, probs=probs.lo))))
xly.lo
xly$low.close1 %>% quantile(probs=probs.lo)

ggplot(xly.lo, aes(x=1:nrow(xly.lo), y=X5.)) +
  geom_point() +
  geom_smooth(method = lm)

# correlation
adjusted_long <- hist_daily %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted)


# tidyquant functions -----------------------------------------------------

hist_daily_log_returns <- hist_daily %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "daily.returns")

hist_monthly <- hist_daily %>%
  group_by(symbol) %>%
  tq_transmute(select = open:adjusted,
               mutate_fun = to.period,
               period = "months")

# daily log return density charts by symbol
hist_daily_log_returns %>%
  ggplot(aes(x = daily.returns, fill = symbol)) +
  geom_density(alpha = 0.5) +
  labs(title = "Daily Log Returns",
       x = "Daily Returns", y = "Density") +
  theme_tq() + 
  scale_fill_tq() +
  facet_wrap(~symbol, ncol=4)

# daily stock prices - line chart
hist_daily %>%
  filter(symbol %in% c("IVV", "XLY", "VOE", "IJR")) %>%
  group_by(symbol) %>%
  ggplot(aes(x=date, y=adjusted, color=symbol)) +
  geom_line(size = 1) +
  labs(title = "Daily Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~symbol, ncol=2, scales="free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_color_tq()

# monthly stock prices - line chart
hist_monthly %>%
  #filter(symbol %in% c("IVV", "XLY", "VOE", "IJR")) %>%
  group_by(symbol) %>%
  ggplot(aes(x=date, y=adjusted, color=symbol)) +
  geom_line(size = 1) +
  labs(title = "Monthly Stock Prices",
       x = "", y = "Adjusted Prices", color = "") +
  facet_wrap(~symbol, ncol=2, scales="free_y") +
  scale_y_continuous(labels = scales::dollar) +
  theme_tq() +
  scale_color_tq()
