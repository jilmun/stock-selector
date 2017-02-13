require(tidyverse)

sp500_symbols2 <- sp500_symbols[-79]  # drop BF.B
with(sp500, AAPL) %>% head
with(sp500, AAPL) %>% Ad() %>% chart_Series() 

# check normal dist
AAPL_log_returns <- with(sp500, AAPL) %>%
  Ad() %>%
  dailyReturn(type="log")
AAPL_log_returns %>%
  ggplot(aes(x=daily.returns)) +
  geom_histogram(bins=100) +
  geom_density() +
  geom_rug(alpha=0.5)
probs <- c(0.005, 0.025, 0.25, 0.5, 0.75, 0.975, 0.995)
dist_log_returns <- AAPL_log_returns %>%
  quantile(probs=probs, na.rm=TRUE)
dist_log_returns
mean_log_returns <- mean(AAPL_log_returns, na.rm=TRUE)
sd_log_returns <- sd(AAPL_log_returns, na.rm=TRUE)
mean_log_returns
sd_log_returns

# transform back to percent return
mean_log_returns %>% exp() *100 - 100


# monte carlo random walk -------------------------------------------------

N <- 252  # approximate trading days per year
M <- 250  # monte carlo simulations
mu <- mean_log_returns
sigma <- sd_log_returns
day <- 1:N

price_init <- Ad(with(sp500,AAPL)) %>% last
set.seed(123)

monte_carlo_mat <- matrix(nrow=N, ncol=M)
for (j in 1:M) {
  monte_carlo_mat[1,j] <- price_init
  for (i in 2:N) {
    monte_carlo_mat[i,j] <- monte_carlo_mat[i-1,j] * exp(rnorm(1, mu, sigma))
  }
}

price_sim <- cbind(day, monte_carlo_mat) %>% as_tibble()
names(price_sim) <- c("Day", paste0("Sim.", 1:M))
price_sim <- price_sim %>%
  gather(key="Simulation", value="Stock.Price", -(Day))

price_sim %>%
  ggplot(aes(x=Day, y=Stock.Price, Group=Simulation)) +
  geom_line(alpha=0.1) +
  ggtitle(paste0("AAPL: ", M, 
                 " Similated Prices for ", N, 
                 " Trading Days"))

end_stock_prices <- price_sim %>% 
  filter(Day==max(Day))
quantile(end_stock_prices$Stock.Price, probs=probs) %>% round(2)

CAGR_historical <- (price_init / Ad(with(sp500,AAPL)) %>% first) ^ (1 / (nrow(with(sp500,AAPL))/252)) - 1
CAGR_sim <- (median(end_stock_prices$Stock.Price) / price_init) ^ (1 / (N/252)) - 1
CAGR_historical
CAGR_sim
