# Load required libraries
library(ggplot2)
library(minpack.lm)
# Load your data from CSV
data <- read.csv("summary_results.csv", sep = ",")
# Filter post-photoconversion data (time >= 0)
data_post <- subset(data, Time >= 0)
# Fit exponential recovery model for green fluorescence
green_model <- nlsLM(Green_mean ~ A * (1 - exp(-k * Time)), data = data_post,
                     start = list(A = max(data_post$Green_mean), k = 0.1))
# Fit exponential decay model for red fluorescence
red_model <- nlsLM(Red_mean ~ R0 * exp(-k * Time), data = data_post,
                   start = list(R0 = max(data_post$Red_mean), k = 0.1))
# Add fitted values to the data
data_post$Green_fit <- predict(green_model)
data_post$Red_fit <- predict(red_model)
# Export fitted data to CSV
write.csv(data_post, "fitted_fluorescence_data.csv", row.names = FALSE)
# Plot with error bars and fitted curves
ggplot(data_post, aes(x = Time)) +
  geom_errorbar(aes(ymin = Green_mean - Green_sd, ymax = Green_mean + Green_sd), color = "lightgreen", width = 0.3) +
  geom_point(aes(y = Green_mean), color = "green") +
  geom_line(aes(y = Green_fit), color = "darkgreen") +
  
  geom_errorbar(aes(ymin = Red_mean - Red_sd, ymax = Red_mean + Red_sd), color = "pink", width = 0.3) +
  geom_point(aes(y = Red_mean), color = "red") +
  geom_line(aes(y = Red_fit), color = "darkred") +
  
  labs(title = "Fluorescence Dynamics Post-Photoconversion",
       x = "Time (s)", y = "Normalized Fluorescence") +
  theme_minimal()
# Print estimated rates
cat("Estimated rate of green molecule entry (k):", coef(green_model)["k"], "\n")
cat("Estimated rate of red molecule exit (k):", coef(red_model)["k"], "\n")

