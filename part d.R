# PART 1: DATA GENERATION WITH NON-LINEAR DEMAND

# Step A: Define Model Parameters
# Baseline parameters
r_base <- 0.05
R0_base <- 1000

# Shock parameters
r_shock <- 0.10   # The new, higher interest rate
R0_shock <- 500  # The new, smaller resource stock

# Demand parameters for the NON-LINEAR curve
A <- 200   # Demand scaling parameter
epsilon <- 0.8 # Price elasticity of demand
# We still need a choke price to define the terminal condition, p(T).
# This is now an assumption, not derived from the demand curve's intercept.
pT <- 25

# Step B: Define Core Economic Functions (Non-Linear Version)

# New Non-Linear Demand Function: Q = A * P^(-epsilon)
d_nonlinear <- function(p) {
  return(A * p^(-epsilon))
}

# New Stock-Time Function using Numerical Integration
# This function calculates the *difference* between stock and cumulative extraction,
# which is what uniroot() needs to find the root (where the difference is zero).
stock_root_finder <- function(Tf, r, R) {
  # Define quantity as a function of time, q(t), for the given Tf
  quantity_as_a_function_of_time <- function(t) {
    price_at_t <- pT * exp(-r * (Tf - t))
    return(d_nonlinear(price_at_t))
  }
  # Calculate the total extraction over the entire period [0, Tf]
  total_extraction <- integrate(quantity_as_a_function_of_time, lower = 0, upper = Tf)$value
  # Return the difference, which uniroot will drive to zero.
  return(R - total_extraction)
}

# Step C: Solve for all three paths using the new numerical method

# Path 1: Baseline (Solid Line)
Tf_base <- uniroot(stock_root_finder, c(1, 200), r = r_base, R = R0_base)$root
time_base <- seq(0, Tf_base, length.out = 100)
price_base <- pT * exp(-r_base * (Tf_base - time_base))
quantity_base <- d_nonlinear(price_base)
# We need to calculate the stock path by numerically integrating at each time step
stock_base <- R0_base - sapply(time_base, function(t) {
  integrate(function(x) d_nonlinear(pT * exp(-r_base * (Tf_base - x))), lower = 0, upper = t)$value
})

# Path 2: Interest Rate Shock (Dotted Line)
Tf_r_shock <- uniroot(stock_root_finder, c(1, 200), r = r_shock, R = R0_base)$root
time_r_shock <- seq(0, Tf_r_shock, length.out = 100)
price_r_shock <- pT * exp(-r_shock * (Tf_r_shock - time_r_shock))
quantity_r_shock <- d_nonlinear(price_r_shock)
stock_r_shock <- R0_base - sapply(time_r_shock, function(t) {
  integrate(function(x) d_nonlinear(pT * exp(-r_shock * (Tf_r_shock - x))), lower = 0, upper = t)$value
})

# Path 3: Resource Stock Shock (Dashed Line)
Tf_R_shock <- uniroot(stock_root_finder, c(1, 200), r = r_base, R = R0_shock)$root
time_R_shock <- seq(0, Tf_R_shock, length.out = 100)
price_R_shock <- pT * exp(-r_base * (Tf_R_shock - time_R_shock))
quantity_R_shock <- d_nonlinear(price_R_shock)
stock_R_shock <- R0_shock - sapply(time_R_shock, function(t) {
  integrate(function(x) d_nonlinear(pT * exp(-r_base * (Tf_R_shock - x))), lower = 0, upper = t)$value
})


# PART 2: THE MULTI-SHOCK PLOTTING FUNCTION (Unchanged, but now plots curved data)

create_multi_shock_plot <- function(time_base, price_base, stock_base,
                                    time_r_shock, price_r_shock, stock_r_shock,
                                    time_R_shock, price_R_shock, stock_R_shock) {
  
  if (dev.cur() != 1) dev.off()
  layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
  par(oma = c(0, 0, 0, 0))
  
  # Plot 1: Demand Curve (Top-Left) NOW CURVED
  par(mar = c(0, 5, 5, 0))
  p_demand <- seq(min(price_base, price_r_shock, price_R_shock), pT, length.out = 100)
  q_demand <- d_nonlinear(p_demand)
  plot(q_demand, p_demand, type = "l", lwd = 2,
       xaxs = "i", yaxs = "i", xlim = rev(range(q_demand)), ylim = c(0, max(p_demand)),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  axis(side = 2, las = 1); mtext("Net Price (Pt)", side = 2, line = 3)
  axis(side = 3); box()
  
  # (The rest of the plotting function is identical to the previous version)
  
  # Plot 2: Price Path (Top-Right)
  par(mar = c(0, 0, 5, 5))
  plot(time_base, price_base, type = "l", lwd = 2, lty = 1,
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_r_shock, time_R_shock))),
       ylim = c(0, max(c(price_base, price_r_shock, price_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(time_r_shock, price_r_shock, lwd = 2, lty = 3)
  lines(time_R_shock, price_R_shock, lwd = 2, lty = 2)
  axis(side = 3); axis(side = 4, las = 1)
  mtext("Net Price (Pt)", side = 4, line = 3); box()
  
  # Plot 3: Reserve Path (Bottom-Left)
  par(mar = c(5, 5, 0, 0))
  plot(stock_base, time_base, type = "l", lwd = 2, lty = 1,
       xaxs = "i", yaxs = "i",
       xlim = rev(range(c(stock_base, stock_r_shock, stock_R_shock))),
       ylim = rev(range(c(time_base, time_r_shock, time_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(stock_r_shock, time_r_shock, lwd = 2, lty = 3)
  lines(stock_R_shock, time_R_shock, lwd = 2, lty = 2)
  axis(side = 1); mtext("Demand (Quantity) / Reserves (R)", side = 1, line = 3)
  axis(side = 2, las = 1); mtext("Time (t)", side = 2, line = 3); box()
  
  # Plot 4: 45-degree Line (Bottom-Right) & Legend
  par(mar = c(5, 0, 0, 5))
  plot(time_base, time_base, type = "l", lwd = 2, lty = 1,
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_r_shock, time_R_shock))),
       ylim = rev(range(c(time_base, time_r_shock, time_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(time_r_shock, time_r_shock, lwd = 2, lty = 3)
  lines(time_R_shock, time_R_shock, lwd = 2, lty = 2)
  axis(side = 1); mtext("Time (t)", side = 1, line = 3)
  axis(side = 4, las = 1); mtext("Time (t)", side = 4, line = 3); box()
  legend("topright", legend = c("Baseline", "Interest Rate Shock", "Stock Shock"),
         lty = c(1, 3, 2), lwd = 2, bty = "n", cex = 0.8)
}


# PART 3: EXECUTION

create_multi_shock_plot(
  time_base, price_base, stock_base,
  time_r_shock, price_r_shock, stock_r_shock,
  time_R_shock, price_R_shock, stock_R_shock
)