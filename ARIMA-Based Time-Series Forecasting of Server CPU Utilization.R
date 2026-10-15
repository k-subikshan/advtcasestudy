f<-read.csv("/home/subi/Downloads/server_monitoring_1000_records.csv")

library(ggplot2)
library(forecast)


library(ggplot2)
library(forecast)

f$timestamp <- as.POSIXct(
  as.character(f$timestamp),
  tz = "UTC"
)

f$cpu_usage_percent <- as.numeric(
  as.character(f$cpu_usage_percent)
)

f <- f[!is.na(f$timestamp) &
         !is.na(f$cpu_usage_percent), ]

f <- f[order(f$timestamp), ]

x <- f$cpu_usage_percent
y <- ts(x, frequency = 96)

model <- auto.arima(y)

forecast_result <- forecast(
  model,
  h = 96,
  level = 95
)

last_time <- max(f$timestamp, na.rm = TRUE)

future_time <- seq(
  from = last_time + 900,
  by = "15 min",
  length.out = 96
)

forecast_data <- data.frame(
  timestamp = future_time,
  predicted = as.numeric(forecast_result$mean),
  lower = as.numeric(forecast_result$lower[, 1]),
  upper = as.numeric(forecast_result$upper[, 1])
)

p <- ggplot() +
  geom_line(
    data = f,
    aes(x = timestamp, y = cpu_usage_percent),
    color = "steelblue",
    linewidth = 0.6
  ) +
  geom_ribbon(
    data = forecast_data,
    aes(x = timestamp, ymin = lower, ymax = upper),
    fill = "skyblue",
    alpha = 0.3
  ) +
  geom_line(
    data = forecast_data,
    aes(x = timestamp, y = predicted),
    color = "red",
    linewidth = 0.9
  ) +
  geom_vline(
    xintercept = as.numeric(last_time),
    linetype = "dashed"
  ) +
  labs(
    title = "ARIMA-Based Server CPU Utilization Forecast",
    subtitle = "24-Hour Forecast with 95% Prediction Interval",
    x = "Timestamp",
    y = "CPU Utilization (%)"
  ) +
  theme_minimal()

print(p)

