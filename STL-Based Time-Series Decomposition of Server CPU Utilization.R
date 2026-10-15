
library(ggplot2)

f$timestamp <- as.POSIXct(
  f$timestamp,
  format = "%Y-%m-%d %H:%M:%S"
)

f <- f[order(f$timestamp), ]

f$cpu_usage_percent <- as.numeric(f$cpu_usage_percent)

f <- f[!is.na(f$timestamp) & !is.na(f$cpu_usage_percent), ]

y <- ts(f$cpu_usage_percent, frequency = 96)

stl_fit <- stl(y, s.window = "periodic", robust = TRUE)

plot(stl_fit,
     main = "STL Time-Series Decomposition of CPU Usage")
bash
