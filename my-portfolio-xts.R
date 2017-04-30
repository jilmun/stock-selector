library(quantmod)
library(tseries)
library(PerformanceAnalytics)

xly <- get.hist.quote(instrument="sly", start="2003-12-31", quote="AdjClose", compression="d")
ivv <- get.hist.quote(instrument="ivv", start="2003-12-31", quote="AdjClose", compression="d")

portfolio_prices <- as.xts(merge(xly,ivv))
portfolio_returns <- na.omit(ROC(portfolio_prices))

p_xly <- Return.portfolio(portfolio_returns,
                          weights = c(1,0),
                          wealth.index = TRUE,
                          verbose = TRUE)
p_ivv <- Return.portfolio(portfolio_returns,
                          weights = c(0,1),
                          wealth.index = TRUE,
                          verbose = TRUE)
p_merged <- cbind(p_xly$returns, p_ivv$returns)
colnames(p_merged) <- c("xly", "ivv")

par(mfrow=c(1,1))
chart.CumReturns(p_merged,
                 wealth.index = TRUE,
                 legend.loc = "bottomright",
                 main = "Growth of $1 investment",
                 ylab = "$")

table.AnnualizedReturns(p_merged)
table.Drawdowns(p_xly$returns)
table.Drawdowns(p_ivv$returns)

