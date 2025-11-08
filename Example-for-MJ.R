# Step A: Define Model Parameters
# Using the user-defined parameters for a clear view of the demand curve
r_base <- 0.05; R0_base <- 1000
r_shock <- 0.10; R0_shock <- 500
pT <- 3; A <- 120; epsilon <- 0.5

# Step B: Define Core Economic Functions (Unchanged)
d_nonlinear <- function(p) A * p^(-epsilon)
stock_root_finder <- function(Tf, r, R) {
  quantity_as_a_function_of_time <- function(t) {
    price_at_t <- pT * exp(-r * (Tf - t))
    return(ifelse(price_at_t <= 0, Inf, d_nonlinear(price_at_t)))
  }
  total_extraction <- integrate(quantity_as_a_function_of_time, lower = 0, upper = Tf)$value
  return(R - total_extraction)
}

# Step C: Solve for all three paths (Unchanged)
# Path 1: Baseline
Tf_base <- uniroot(stock_root_finder, c(1, 200), r = r_base, R = R0_base)$root
time_base <- seq(0, Tf_base, length.out = 100); price_base <- pT * exp(-r_base * (Tf_base - time_base))
quantity_base <- d_nonlinear(price_base)
stock_base <- R0_base - sapply(time_base, function(t) {
  integrate(function(x) d_nonlinear(pT * exp(-r_base * (Tf_base - x))), lower = 0, upper = t)$value
})

# Path 2: Interest Rate Shock
Tf_r_shock <- uniroot(stock_root_finder, c(1, 200), r = r_shock, R = R0_base)$root
time_r_shock <- seq(0, Tf_r_shock, length.out = 100); price_r_shock <- pT * exp(-r_shock * (Tf_r_shock - time_r_shock))
quantity_r_shock <- d_nonlinear(price_r_shock)
stock_r_shock <- R0_base - sapply(time_r_shock, function(t) {
  integrate(function(x) d_nonlinear(pT * exp(-r_shock * (Tf_r_shock - x))), lower = 0, upper = t)$value
})

# Path 3: Resource Stock Shock
Tf_R_shock <- uniroot(stock_root_finder, c(1, 200), r = r_base, R = R0_shock)$root
time_R_shock <- seq(0, Tf_R_shock, length.out = 100); price_R_shock <- pT * exp(-r_base * (Tf_R_shock - time_R_shock))
quantity_R_shock <- d_nonlinear(price_R_shock)
stock_R_shock <- R0_shock - sapply(time_R_shock, function(t) {
  integrate(function(x) d_nonlinear(pT * exp(-r_base * (Tf_R_shock - x))), lower = 0, upper = t)$value
})


# The Beginning of the End (Final Plotting Functions with All Projections)
create_final_plot <- function(time_base, price_base, quantity_base, stock_base,
                              time_r_shock, price_r_shock, quantity_r_shock, stock_r_shock,
                              time_R_shock, price_R_shock, quantity_R_shock, stock_R_shock) {
  
  if (dev.cur() != 1) dev.off()
  layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
  par(oma = c(0, 0, 0, 0))
  
  P0_base <- price_base[1]; T_base <- max(time_base); Q0_base <- quantity_base[1]
  P0_r_shock <- price_r_shock[1]; T_r_shock <- max(time_r_shock); Q0_r_shock <- quantity_r_shock[1]
  P0_R_shock <- price_R_shock[1]; T_R_shock <- max(time_R_shock); Q0_R_shock <- quantity_R_shock[1]
  
  # Plot 1: Demand Curve (Top-Left)
  par(mar = c(0, 5, 5, 0))
  p_demand <- seq(0.1, pT, length.out = 100); q_demand <- d_nonlinear(p_demand)
  plot(q_demand, p_demand, type = "l", lwd = 2,
       xaxs = "i", yaxs = "i", xlim = rev(range(q_demand)), ylim = c(0, max(p_demand)),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  # Horizontal lines from initial prices to the demand curve
  segments(Q0_base, P0_base, 0, P0_base, lty = 3, col = "black")
  segments(Q0_r_shock, P0_r_shock, 0, P0_r_shock, lty = 3, col = "red")
  segments(Q0_R_shock, P0_R_shock, 0, P0_R_shock, lty = 3, col = "blue")
  segments(Q0_base, P0_base, Q0_base, 0, lty = 3, col = "black")
  segments(Q0_r_shock, P0_r_shock, Q0_r_shock, 0, lty = 3, col = "red")
  segments(Q0_R_shock, P0_R_shock, Q0_R_shock, 0, lty = 3, col = "blue")
  axis(side = 2, las = 1); mtext("Net Price (Pt)", side = 2, line = 3)
  axis(side = 3); box()
  
  # Plot 2: Price Path (Top-Right)
  par(mar = c(0, 0, 5, 5))
  plot(time_base, price_base, type = "l", lwd = 2, lty = 1, col = "black",
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_r_shock, time_R_shock))),
       ylim = c(0, max(c(price_base, price_r_shock, price_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(time_r_shock, price_r_shock, lwd = 2, lty = 2, col = "red")
  lines(time_R_shock, price_R_shock, lwd = 2, lty = 2, col = "blue")
  # Vertical lines from terminal times up to the choke price
  segments(T_base, 0, T_base, pT, lty = 3, col = "black")
  segments(T_r_shock, 0, T_r_shock, pT, lty = 3, col = "red")
  segments(T_R_shock, 0, T_R_shock, pT, lty = 3, col = "blue")
  axis(side = 3); axis(side = 4, las = 1)
  mtext("Net Price (Pt)", side = 4, line = 3); box()
  
  # Plot 3: Reserve Path (Bottom-Left)
  par(mar = c(5, 5, 0, 0))
  plot(stock_base, time_base, type = "l", lwd = 2, lty = 1, col = "black",
       xaxs = "i", yaxs = "i",
       xlim = rev(range(c(stock_base, stock_r_shock, stock_R_shock))),
       ylim = rev(range(c(time_base, time_r_shock, time_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(stock_r_shock, time_r_shock, lwd = 2, lty = 2, col = "red")
  lines(stock_R_shock, time_R_shock, lwd = 2, lty = 2, col = "blue")
  axis(side = 1); mtext("Demand (Quantity) / Reserves (R)", side = 1, line = 3)
  axis(side = 2, las = 1); mtext("Time (t)", side = 2, line = 3); box()
  
  # Plot 4: 45-degree Line (Bottom-Right)
  par(mar = c(5, 0, 0, 5))
  plot(time_base, time_base, type = "l", lwd = 2, lty = 1, col = "black",
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_r_shock, time_R_shock))),
       ylim = rev(range(c(time_base, time_r_shock, time_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  # Horizontal lines from terminal times to the y-axis
  segments(0, T_base, T_base, T_base, lty = 3, col = "black")
  segments(0, T_r_shock, T_r_shock, T_r_shock, lty = 3, col = "red")
  segments(0, T_R_shock, T_R_shock, T_R_shock, lty = 3, col = "blue")
  segments(T_r_shock, 0, T_r_shock, T_r_shock, lty = 3, col = "red")
  segments(T_R_shock, 0, T_R_shock, T_R_shock, lty = 3, col = "blue")
  axis(side = 1); mtext("Time (t)", side = 1, line = 3)
  axis(side = 4, las = 1); mtext("Time (t)", side = 4, line = 3); box()
  legend("topright",
         legend = c("Baseline", "Interest Rate Shock", "Stock Shock"),
         lty = c(1, 2, 2),
         col = c("black", "red", "blue"),
         lwd = 2, bty = "n", cex = 0.8)
}

# Image time, let's go
create_final_plot(
  time_base, price_base, quantity_base, stock_base,
  time_r_shock, price_r_shock, quantity_r_shock, stock_r_shock,
  time_R_shock, price_R_shock, quantity_R_shock, stock_R_shock
)