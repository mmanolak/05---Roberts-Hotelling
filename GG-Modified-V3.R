# Part 1: Load Libraries and Generate Data
# Installing the necessary libraries
# install.packages("tidyverse")
# install.packages("patchwork")

library(tidyverse)
library(patchwork)

# Step A: Define Model Parameters
r_base <- 0.05    # Baseline interest rate.
# Good range for visualization: 0.01 - 0.20

R0_base <- 1000   # Initial resource stock.
# Good range for visualization: 100 - 10000

pT <- 10          # Choke price (the maximum price).
# Good range for visualization: 5 - 100

A <- 100          # Demand scaling factor (overall size of demand).
# Good range for visualization: 50 - 2000

epsilon <- 1.0    # Price elasticity of demand (controls curve shape).
# Good range for visualization: 0.2 - 2.0

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


# Part 2: Create the Final, Flexible Plotting Function

# This function now combines the logic from V1 and V2
create_gg_plot <- function(log_linear_demand = FALSE) {
  
  # Quadrant 1: Demand Curve (Top-Left)
  # Use an if/else block to create either the standard or log-linear plot
  if (log_linear_demand) {
    # LOG-LINEAR VERSION (from V2)
    log_p_demand <- log(seq(0.1, pT, length.out = 100))
    log_q_demand <- log(d_nonlinear(exp(log_p_demand)))
    log_p0 <- log(p0); log_q0 <- log(q0)
    log_demand_data <- tibble(log_q = log_q_demand, log_p = log_p_demand)
    q_ticks <- c(10, 20, 50, 100); p_ticks <- c(1, 2, 5, 10)
    
    p1 <- ggplot(data = log_demand_data, aes(x = log_q, y = log_p)) +
      geom_line(linewidth = 1.1) + scale_x_reverse() +
      scale_y_continuous(breaks = log(p_ticks), labels = p_ticks, limits = log(c(min(price_base), pT))) +
      scale_x_continuous(breaks = log(q_ticks), labels = q_ticks) +
      geom_segment(aes(x = log_q0, y = log_p0, xend = -Inf, yend = log_p0), linetype = "dotted") +
      geom_segment(aes(x = log_q0, y = log_p0, xend = log_q0, yend = -Inf), linetype = "dotted") +
      annotate("text", x = log_q0, y = log_p0, label = "q[0]", parse = TRUE, vjust = 0.5, hjust = -0.5) +
      annotate("text", x = log_q0, y = log_p0, label = "p[0]", parse = TRUE, vjust = -0.5, hjust = 2) +
      theme_classic() + labs(x = "Log Quantity (q)", y = "Log Net Price (Pt)")
  } else {
    # STANDARD VERSION (from V1)
    p_demand_data <- tibble(price = seq(0.1, pT, length.out = 100), quantity = d_nonlinear(price))
    p1 <- ggplot(data = p_demand_data, aes(x = quantity, y = price)) +
      geom_line(linewidth = 1.1) + scale_x_reverse(limits = c(q0 * 2, 0)) +
      scale_y_continuous(limits = c(0, pT)) +
      geom_segment(aes(x = q0, y = p0, xend = -Inf, yend = p0), linetype = "dotted") +
      geom_segment(aes(x = q0, y = p0, xend = q0, yend = -Inf), linetype = "dotted") +
      annotate("text", x = q0, y = p0 / 2, label = "q[0]", parse = TRUE, hjust = 1.2) +
      annotate("text", x = q0 / 2, y = p0, label = "p[0]", parse = TRUE, vjust = -0.5) +
      theme_classic() + labs(x = "Quantity (q)", y = "Net Price (Pt)")
  }
  
  # Quadrant 2: Price Path (Top-Right)
  p2 <- ggplot(data = plot_data, aes(x = time, y = price)) +
    geom_line(linewidth = 1.1) +
    scale_y_continuous(limits = c(0, pT), position = "right") +
    scale_x_continuous(limits = c(0, Tf)) +
    geom_segment(aes(x = -Inf, y = p0, xend = Tf, yend = p0), linetype = "dotted") +
    geom_segment(aes(x = Tf, y = -Inf, xend = Tf, yend = pT), linetype = "dotted") +
    annotate("text", x = time_base[75], y = price_base[75], label = "p[T]", parse = TRUE, vjust = -0.75) +
    theme_classic() + labs(x = "Time (t)", y = NULL)
  
  # Quadrant 3: Extraction Path (Bottom-Left) - CORRECTED VERSION
  polygon_data <- tibble(
    time = c(plot_data$time, max(plot_data$time), 0),
    quantity = c(plot_data$quantity, 0, 0)
  )
  
  p3 <- ggplot(data = plot_data, aes(x = time, y = quantity)) +
    geom_polygon(data = polygon_data, aes(x = time, y = quantity), fill = "gray80") +
    geom_line(linewidth = 1.1) +
    # The scales are no longer reversed
    scale_y_continuous(limits = c(0, q0 * 1.1)) + 
    scale_x_continuous(limits = c(0, Tf)) +
    annotate("text", x = mean(plot_data$time), y = mean(plot_data$quantity), label = "R[0]", parse = TRUE, size = 6) +
    theme_classic() +
    labs(x = "Time (t)", y = "Quantity (q)") # Correctly labeled axes
  
  # Quadrant 4: 45-degree Line (Bottom-Right)
  p4 <- ggplot(data = plot_data, aes(x = time, y = time)) +
    geom_line(linewidth = 1.1) + scale_y_reverse(limits = c(Tf, 0), position = "right") +
    scale_x_continuous(limits = c(0, Tf)) +
    geom_segment(aes(x = -Inf, y = Tf, xend = Tf, yend = Tf), linetype = "dotted") +
    geom_segment(aes(x = Tf, y = -Inf, xend = Tf, yend = Tf), linetype = "dotted") +
    theme_classic() + labs(x = "Time (t)", y = NULL)
  
  # Assemble and Display the Final Figure
  final_plot <- (p1 + theme(axis.title.x = element_blank(), plot.margin = margin(r=0))) + 
    (p2 + theme(axis.title.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(l=0))) +
    (p3 + theme(plot.margin = margin(r=0))) + 
    (p4 + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(l=0))) +
    plot_layout(ncol = 2)
  
  # Print the plot to the device
  print(final_plot)
}


# Part 3: Assemble and Display the Final Figure
# Generate the two plots together, in harmony
create_gg_plot(log_linear_demand = FALSE) # First, the standard plot
create_gg_plot(log_linear_demand = TRUE)  # Second, the log-linear plot