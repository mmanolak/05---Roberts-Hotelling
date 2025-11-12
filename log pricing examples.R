# Load ggplot2 for high-quality plotting
# If you don't have it, run: install.packages("ggplot2")
library(ggplot2)
library(patchwork) # For combining plots

# --- 1. Define the Demand Function and Parameters ---

# A flexible demand function that takes epsilon as an argument
d_nonlinear <- function(p, A, epsilon) {
  return(A * p^(-epsilon))
}

# Define a range of prices to plot over
price_range <- seq(1, 20, length.out = 100)
A_param <- 100 # A constant scaling factor

# Define the different elasticities we want to test
epsilons_to_test <- c(0.5, 1.0, 1.5)

# --- 2. Generate Data for Each Elasticity ---

# Create a single data frame holding all the results
plot_data_list <- lapply(epsilons_to_test, function(e) {
  data.frame(
    price = price_range,
    quantity = d_nonlinear(price_range, A = A_param, epsilon = e),
    # Use as.factor so ggplot gives each a different color
    epsilon = as.factor(e)
  )
})
plot_data <- do.call(rbind, plot_data_list)


# --- 3. Create the Visualizations ---

# Plot 1: Standard Linear Scale
p1 <- ggplot(data = plot_data, aes(x = quantity, y = price, color = epsilon)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Demand Curves on a Standard Scale",
    x = "Quantity (q)",
    y = "Price (p)"
  ) +
  theme_minimal()

# Plot 2: Log-Log Scale
# We plot the data normally, but then change the axis scales to be logarithmic.
p2 <- ggplot(data = plot_data, aes(x = quantity, y = price, color = epsilon)) +
  geom_line(linewidth = 1.2) +
  # These two lines transform the axes to a log scale
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Demand Curves on a Log-Log Scale",
    x = "Log(Quantity)",
    y = "Log(Price)"
  ) +
  theme_minimal()

# --- 4. Display the Comparison ---
# Display the two plots side-by-side
p1 + p2