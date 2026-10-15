f<-read.csv("/home/subi/Downloads/server_monitoring_1000_records.csv")
f

library(ggplot2)
library(ggplot2)
library(changepoint)


library(ggplot2)
library(changepoint)

f <- f[order(f$timestamp), ]

x <- as.numeric(f$cpu_usage_percent)

valid <- is.finite(x)

f <- f[valid, ]
x <- x[valid]

cp_model <- cpt.mean(
  x,
  method = "PELT",
  penalty = "MBIC"
)

cp_indices <- cpts(cp_model)

cp_data <- data.frame(
  timestamp = f$timestamp[cp_indices],
  cpu = x[cp_indices]
)

ggplot(f, aes(x = timestamp, y = cpu_usage_percent)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_vline(
    data = cp_data,
    aes(xintercept = as.numeric(timestamp)),
    color = "red",
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_point(
    data = cp_data,
    aes(x = timestamp, y = cpu),
    color = "red",
    size = 2.5
  ) +
  labs(
    title = "PELT-Based Change-Point Detection",
    subtitle = "Identifying Structural Changes in Server CPU Utilization",
    x = "Timestamp",
    y = "CPU Utilization (%)"
  ) +
  theme_minimal()


