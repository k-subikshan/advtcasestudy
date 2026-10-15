f<-read.csv("/home/subi/Downloads/server_monitoring_1000_records.csv")
f

library(ggplot2)

library(forecast)
library(ggplot2)
library(patchwork)

f <- f[order(f$timestamp), ]

x <- as.numeric(f$cpu_usage_percent)
x <- x[is.finite(x)]

y <- ts(x, frequency = 96)

p1 <- ggAcf(y, lag.max = 96) +
  labs(
    title = "Autocorrelation Function (ACF)",
    subtitle = "Correlation with previous CPU observations",
    x = "Lag (15-minute intervals)",
    y = "Autocorrelation"
  ) +
  theme_minimal()

p2 <- ggPacf(y, lag.max = 96) +
  labs(
    title = "Partial Autocorrelation Function (PACF)",
    subtitle = "Direct correlation after adjusting for intermediate lags",
    x = "Lag (15-minute intervals)",
    y = "Partial autocorrelation"
  ) +
  theme_minimal()

p1 / p2 +
  plot_annotation(
    title = "ACF and PACF Diagnostics for Server CPU Utilization",
    subtitle = "Time-Series Dependence Analysis"
  )

