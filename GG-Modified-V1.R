# Part 1: Load Libraries and Generate Data

# Load necessary libraries
library(tidyverse)
library(patchwork)

# Step A: Define Model Parameters
r_base <- 0.05; R0_base <- 1000
pT <- 10; A <- 100; epsilon <- 1.0

# Step B: Define Core Economic Functions
d_nonlinear <- function(p) A * p^(-epsilon)
stock_root_finder <- function(Tf, r, R) {
  quantity_as_a_function_of_time <- function(t) {
    price_at_t <- pT * exp(-r * (Tf - t))
    return(ifelse(price_at_t <= 0, Inf, d_nonlinear(price_at_t)))
  }
  total_extraction <- integrate(quantity_as_a_function_of_time, lower = 0, upper = Tf)$value
  return(R - total_extraction)
}

# Step C: Solve for the Baseline Path
Tf_base <- uniroot(stock_root_finder, c(1, 200), r = r_base, R = R0_base)$root
time_base <- seq(0, Tf_base, length.out = 100); price_base <- pT * exp(-r_base * (Tf_base - time_base))
quantity_base <- d_nonlinear(price_base)

# Define key points for plotting
p0 <- price_base[1]; q0 <- quantity_base[1]; Tf <- Tf_base

# Create a tidy data frame for the equilibrium path
plot_data <- tibble(
  time = time_base,
  price = price_base,
  quantity = quantity_base
)


# Part 2: Create the Four Individual ggplot Objects

# Quadrant 1: Demand Curve (Top-Left)
p_demand_data <- tibble(price = seq(0.1, pT, length.out = 100), quantity = d_nonlinear(price))
p1 <- ggplot(data = p_demand_data, aes(x = quantity, y = price)) +
  geom_line(linewidth = 1.1) + scale_x_reverse(limits = c(q0 * 2, 0)) +
  scale_y_continuous(limits = c(0, pT)) +
  geom_segment(aes(x = q0, y = p0, xend = -Inf, yend = p0), linetype = "dotted") +
  geom_segment(aes(x = q0, y = p0, xend = q0, yend = -Inf), linetype = "dotted") +
  annotate("text", x = q0, y = p0 / 2, label = "q[0]", parse = TRUE, hjust = 1.2) +
  annotate("text", x = q0 / 2, y = p0, label = "p[0]", parse = TRUE, vjust = -0.5) +
  theme_classic() + labs(x = "Quantity (q)", y = "Net Price (Pt)")

# Quadrant 2: Price Path (Top-Right)
p2 <- ggplot(data = plot_data, aes(x = time, y = price)) +
  geom_line(linewidth = 1.1) +
  scale_y_continuous(limits = c(0, pT), position = "right") +
  scale_x_continuous(limits = c(0, Tf)) +
  geom_segment(aes(x = -Inf, y = p0, xend = Tf, yend = p0), linetype = "dotted") +
  geom_segment(aes(x = Tf, y = -Inf, xend = Tf, yend = pT), linetype = "dotted") +
  annotate("text", x = time_base[75], y = price_base[75], label = "p[T]", parse = TRUE, vjust = -0.75) +
  theme_classic() + labs(x = "Time (t)", y = NULL)

# Quadrant 3: Extraction Path (Bottom-Left)
polygon_data <- tibble(x = c(0, plot_data$quantity, 0), y = c(0, plot_data$time, Tf))
p3 <- ggplot(data = plot_data, aes(x = quantity, y = time)) +
  geom_polygon(data = polygon_data, aes(x = x, y = y), fill = "gray80") +
  geom_line(linewidth = 1.1) + scale_x_reverse(limits = c(q0 * 2, 0)) +
  scale_y_reverse(limits = c(Tf, 0)) +
  annotate("text", x = mean(plot_data$quantity), y = mean(plot_data$time), label = "R[0]", parse = TRUE, size = 6, hjust = 1) +
  theme_classic() + labs(x = "Quantity (q)", y = "Time (t)")

# Quadrant 4: 45-degree Line (Bottom-Right)
p4 <- ggplot(data = plot_data, aes(x = time, y = time)) +
  geom_line(linewidth = 1.1) + scale_y_reverse(limits = c(Tf, 0), position = "right") +
  scale_x_continuous(limits = c(0, Tf)) +
  geom_segment(aes(x = -Inf, y = Tf, xend = Tf, yend = Tf), linetype = "dotted") +
  geom_segment(aes(x = Tf, y = -Inf, xend = Tf, yend = Tf), linetype = "dotted") +
  theme_classic() + labs(x = "Time (t)", y = NULL) # Remove redundant y-axis title


# Part 3: Assemble and Display the Final Figure

# A more robust assembly method that removes spacing
final_plot <- (p1 + theme(axis.title.x = element_blank(), plot.margin = margin(r=0))) + 
  (p2 + theme(axis.title.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(l=0))) +
  (p3 + theme(plot.margin = margin(r=0))) + 
  (p4 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(l=0))) +
  plot_layout(ncol = 2)

final_plot