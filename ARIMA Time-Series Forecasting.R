f<-read.csv("/home/subi/Downloads/server_monitoring_1000_records.csv")
f

library(ggplot2)

f <- f[order(f$timestamp), ]

x <- as.numeric(f$cpu_usage_percent)

window_size <- 25

rolling_median <- rep(NA_real_, length(x))
rolling_mad <- rep(NA_real_, length(x))

for (i in seq_along(x)) {
  start <- max(1, i - window_size + 1)
  values <- x[start:i]
  
  rolling_median[i] <- median(values, na.rm = TRUE)
  
  rolling_mad[i] <- mad(
    values,
    center = rolling_median[i],
    constant = 1.4826,
    na.rm = TRUE
  )
}

robust_z <- (x - rolling_median) / rolling_mad

anomaly <- is.finite(robust_z) & abs(robust_z) > 3.5

f$rolling_median <- rolling_median
f$anomaly <- anomaly

ggplot(f, aes(x = timestamp, y = cpu_usage_percent)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_line(
    aes(y = rolling_median),
    color = "orange",
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  geom_point(
    data = f[f$anomaly, ],
    aes(x = timestamp, y = cpu_usage_percent),
    color = "red",
    size = 2.5
  ) +
  labs(
    title = "Robust Time-Series Anomaly Detection",
    subtitle = "CPU Utilization Using Rolling Median and MAD",
    x = "Timestamp",
    y = "CPU Utilization (%)"
  ) +
  theme_minimal()


