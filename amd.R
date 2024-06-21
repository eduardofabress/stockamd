library(quantmod)
library(ggplot2)
library(forecast)
library(patchwork)

AMD <- getSymbols("AMD", src = "yahoo", from = "2019-01-01", to = "2024-06-01", auto.assign = FALSE)
AMD <- data.frame(Date = index(AMD), coredata(AMD))
AMD <- xts(AMD[,-1], order.by = as.Date(AMD$Date))


AMD_mm <- subset(AMD, index(AMD) >= "2023-01-01")

AMD_mm$mm10 <- rollmean(AMD_mm[, "AMD.Close"], 10, fill = list(NA, NULL, NA), align = "right")
AMD_mm$mm30 <- rollmean(AMD_mm[, "AMD.Close"], 30, fill = list(NA, NULL, NA), align = "right")

p1 <- ggplot(AMD_mm, aes(x = index(AMD_mm))) + 
  geom_line(aes(y = AMD_mm[, "AMD.Close"], color = "AMD")) + 
  geom_line(aes(y = AMD_mm$mm10, color = "MM10")) +
  geom_line(aes(y = AMD_mm$mm30, color = "MM30")) +
  ggtitle("Stock Prices with Moving Averages") +
  xlab("Date") + ylab("Price ($)") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("AMD"="black", "MM10"="firebrick4", "MM30"="green")) +
  theme_classic()

AMD_ret <- diff(log(Cl(AMD)))
AMD_ret <- AMD_ret[-1]

p2 <- ggplot(AMD_ret, aes(x = index(AMD_ret), y = AMD_ret)) + 
  geom_line(color = "brown") +
  ggtitle("Log Returns") + 
  xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months") +
  theme_classic()

AMD_ts <- ts(Cl(AMD), frequency = 252)
decomposed_amd <- decompose(AMD_ts)

p3 <- autoplot(decomposed_amd) +
  ggtitle("Time Series Decomposition") +
  xlab("Date") + ylab("Price ($)") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal()

fit_arima <- auto.arima(Cl(AMD))
forecast_arima <- forecast(fit_arima, h = 30)
p4 <- autoplot(forecast_arima) +
  ggtitle("Price Forecast (ARIMA)") +
  xlab("Date") + ylab("Price ($)") +
  theme(plot.title = element_text(hjust = 0.5))

fit_ets <- ets(Cl(AMD))
forecast_ets <- forecast(fit_ets, h = 30)
p5 <- autoplot(forecast_ets) +
  ggtitle("Price Forecast (ETS)") +
  xlab("Date") + ylab("Price ($)") +
  theme(plot.title = element_text(hjust = 0.5))

(p1 / p2) | (p3 / p4 / p5) +
  plot_layout(ncol = 2, heights = c(1, 2)) &
  theme(plot.title = element_text(hjust = 0.5))

