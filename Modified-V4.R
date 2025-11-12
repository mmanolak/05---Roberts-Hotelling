# Part 1: Data Generation (Simplified for a single scenario)
# Step A: Define Model Parameters
r_base <- 0.05; R0_base <- 1500
pT <- 3; A <- 50; epsilon <- 0.5

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


# Part 2: The Final, Single-Scenario Plotting Function
create_final_single_plot <- function(time, price, quantity, R0) {
  
  if (dev.cur() != 1) dev.off()
  layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
  par(oma = c(0, 0, 0, 0))
  
  # Define key points
  p0 <- price[1]; q0 <- quantity[1]; Tf <- max(time)
  
  # Plot 1: Demand Curve (Top-Left)
  par(mar = c(0, 5, 5, 0))
  p_demand <- seq(0.1, pT, length.out = 100); q_demand <- d_nonlinear(p_demand)
  plot(q_demand, p_demand, type = "l", lwd = 2,
       xaxs = "i", yaxs = "i", xlim = rev(range(q_demand)), ylim = c(0, pT),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  segments(q0, p0, 0, p0, lty = 3, col = "black")
  segments(q0, p0, q0, 0, lty = 3, col = "black")
  axis(side = 2, las = 1); mtext("Net Price (Pt)", side = 2, line = 3)
  axis(side = 3); box()
  
    # p_0: under the line it represents
  text(q0 / 1.25, p0, labels = expression(p[0]), pos = 1, cex = 1.2)
  # q_0: to the left of the line at its halfway point
  text(q0, p0 / 2, labels = expression(q[0]), pos = 2, cex = 1.2)
  
  # Plot 2: Price Path (Top-Right)
  par(mar = c(0, 0, 5, 5))
  plot(time, price, type = "l", lwd = 2, lty = 1, col = "black",
       xaxs = "i", yaxs = "i", xlim = c(0, Tf), ylim = c(0, pT),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  segments(0, p0, Tf, p0, lty = 3, col = "black")
  segments(Tf, 0, Tf, pT, lty = 3, col = "black")
  axis(side = 3); axis(side = 4, las = 1)
  mtext("Net Price (Pt)", side = 4, line = 3); box()
  
  # p_T: under the curve it is changing
  mid_time_index <- 50
  text(time[mid_time_index], price[mid_time_index] * 0.9, labels = expression(p[T]), cex = 1.2)
  
  # Plot 3: Extraction Path (Bottom-Left)
  par(mar = c(5, 5, 0, 0))
  plot(quantity, time, type = "n",
       xaxs = "i", yaxs = "i", xlim = rev(range(c(0, quantity))), ylim = rev(range(time)),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  polygon_x <- c(0, quantity, 0); polygon_y <- c(0, time, Tf)
  polygon(polygon_x, polygon_y, col = "gray80", border = NA)
  lines(quantity, time, lwd = 2, lty = 1, col = "black")
  axis(side = 1); mtext("Quantity (q)", side = 1, line = 3)
  axis(side = 2, las = 1); mtext("Time (t)", side = 2, line = 3); box()
  
  # R_0: to the left of its line
  text(mean(quantity), mean(time), labels = expression(R[0]), pos = 2, cex = 1.5)
  
  # Plot 4: 45-degree Line (Bottom-Right)
  par(mar = c(5, 0, 0, 5))
  plot(time, time, type = "l", lwd = 2, lty = 1, col = "black",
       xaxs = "i", yaxs = "i", xlim = c(0, Tf), ylim = rev(range(time)),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  segments(0, Tf, Tf, Tf, lty = 3, col = "black")
  segments(Tf, 0, Tf, Tf, lty = 3, col = "black")
  axis(side = 1); mtext("Time (t)", side = 1, line = 3)
  axis(side = 4, las = 1); mtext("Time (t)", side = 4, line = 3); box()
}


# Part 3: Image time, let's go
create_final_single_plot(
  time = time_base,
  price = price_base,
  quantity = quantity_base,
  R0 = R0_base
)