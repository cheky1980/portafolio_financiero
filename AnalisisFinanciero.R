# ================================
# PROYECTO: PORTAFOLIO FINANCIERO
# ================================

library(quantmod)
library(PerformanceAnalytics)

# Parametros
date <- "2019-01-15"

# Activos (PRO)
tickers <- c("AAPL", "MSFT", "NVDA", "AMZN", "JPM")


get_returns <- function(ticker) {
  data <- getSymbols(ticker, src = "yahoo", from = date, auto.assign = FALSE)
  close <- data[,6]
  rets <- na.omit(dailyReturn(close, type = "log"))
  return(rets)
}

returns_list <- lapply(tickers, get_returns)
returns <- do.call(merge, returns_list)
colnames(returns) <- tickers
returns <- na.omit(returns)


charts.PerformanceSummary(returns,
                          main = "Assets Performance")

table.AnnualizedReturns(returns)
table.Drawdowns(returns)


mean_returns <- colMeans(returns)
cov_matrix <- cov(returns)


set.seed(123)

n_portfolios <- 5000
n_assets <- ncol(returns)

results <- data.frame(Return = numeric(n_portfolios),
                      Risk = numeric(n_portfolios))

weights_list <- matrix(0, nrow = n_portfolios, ncol = n_assets)

for (i in 1:n_portfolios) {
  
  weights <- runif(n_assets)
  weights <- weights / sum(weights)
  
  weights_list[i, ] <- weights
  
  port_return <- sum(mean_returns * weights)
  port_risk <- sqrt(t(weights) %*% cov_matrix %*% weights)
  
  results$Return[i] <- port_return
  results$Risk[i] <- port_risk
}



sharpe <- results$Return / results$Risk

colors <- colorRampPalette(c("blue", "yellow", "red"))(n_portfolios)
color_scale <- colors[rank(sharpe)]

plot(results$Risk, results$Return,
     col = color_scale,
     pch = 20,
     xlab = "Riesgo (Volatilidad)",
     ylab = "Retorno Esperado",
     main = "Frontera Eficiente (Sharpe Ratio)",
     cex = 0.7)

# Punto Ã³ptimo
best_index <- which.max(sharpe)

points(results$Risk[best_index],
       results$Return[best_index],
       col = "black",
       pch = 19,
       cex = 2)

text(results$Risk[best_index],
     results$Return[best_index],
     labels = "Ãptimo",
     pos = 4)


weights_best <- weights_list[best_index, ]
colnames(weights_list) <- tickers

print("Pesos Ã³ptimos:")
print(weights_best)


portfolio_returns <- Return.portfolio(returns, weights = weights_best)

charts.PerformanceSummary(portfolio_returns,
                          wealth.index = TRUE,
                          main = "Portfolio Growth ($1 Investment)")


sp500 <- getSymbols("^GSPC", src = "yahoo", from = date, auto.assign = FALSE)
sp500_ret <- na.omit(dailyReturn(sp500[,6], type = "log"))

comparison <- merge(portfolio_returns, sp500_ret)
colnames(comparison) <- c("Portfolio", "SP500")

charts.PerformanceSummary(comparison,
                          wealth.index = TRUE,
                          main = "Portfolio vs S&P 500")
