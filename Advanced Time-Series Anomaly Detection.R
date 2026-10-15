f<-read.csv("/home/subi/Downloads/server_monitoring_1000_records.csv")
f
library(ggplot2)

f$timestamp <- as.POSIXct(f$timestamp)
f <- f[order(f$timestamp), ]

y <- ts(f$cpu_usage_percent, frequency = 96)

fit <- stl(y, s.window = "periodic", robust = TRUE)

d <- as.data.frame(fit$time.series)
d$timestamp <- f$timestamp

d$observed <- as.numeric(y)

library(tidyr)
library(ggplot2)

d <- pivot_longer(
  d,
  cols = c(observed, trend, seasonal, remainder),
  names_to = "Component",
  values_to = "Value"
)

ggplot(d, aes(x = timestamp, y = Value)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  facet_grid(Component ~ ., scales = "free_y") +
  labs(
    title = "STL Time-Series Decomposition",
    subtitle = "CPU Usage: Trend, Seasonality and Residuals",
    x = "Timestamp",
    y = "CPU Usage (%)"
  ) +
  theme_minimal()

