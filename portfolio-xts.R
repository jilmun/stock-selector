library(quantmod)
library(tseries)
library(PerformanceAnalytics)

spy <- get.hist.quote(instrument="spy", start="2003-12-31", quote="AdjClose", compression="d")
agg <- get.hist.quote(instrument="agg", start="2003-12-31", quote="AdjClose", compression="d")

w <- c(0.8, 0.2)


# combine returns ---------------------------------------------------------

portfolio_prices <- as.xts(merge(spy, agg))
portfolio_returns <- na.omit(ROC(portfolio_prices, 1, "discrete"))
colnames(portfolio_returns) <- c("spy", "agg")


# create portfolio --------------------------------------------------------

# rebalanced portfolio
p_rebal <- Return.portfolio(portfolio_returns,
                            rebalance_on = "years",
                            weights = w,
                            wealth.index = TRUE,
                            verbose = TRUE)

# buy and hold portfolio
p_bh <- Return.portfolio(portfolio_returns,
                         weights = w,
                         wealth.index = TRUE,
                         verbose = TRUE)

# merge portfolio returns into one dataset
p_merged <- cbind(p_rebal$returns, p_bh$returns)
colnames(p_merged) <- c("rebalanced", "buyhold")


# compare strategies ------------------------------------------------------

par(mfrow=c(1,1))
chart.CumReturns(p_merged,
                 wealth.index = TRUE,  # start with $1
                 legend.loc = "bottomright",
                 main = "Growth of $1 investment",
                 ylab = "$")

chart.RollingPerformance(p_merged,
                         legend.loc = "topright",
                         main = "Rolling 1 Year % Returns")

table.AnnualizedReturns(p_merged)

# number of periods returned are trading days
table.Drawdowns(p_bh$returns)
table.Drawdowns(p_rebal$returns)

par(mfrow=c(1,2))
qqnorm(p_bh$returns, main="Buy and Hold Portfolio")
qqline(p_bh$returns, col="red")
qqnorm(p_rebal$returns, main="Rebalanced Portfolio")
qqline(p_rebal$returns, col="red")
