# PART 1: DATA GENERATION FOR ALL THREE SCENARIOS

# Step A: Define Model Parameters
# Baseline parameters
r_base <- 0.05
R0_base <- 1000

# Shock parameters
r_shock <- 0.10   # The new, higher interest rate
R0_shock <- 500  # The new, smaller resource stock

# Static parameters
pT <- 10; a <- 100; b <- a/pT

# Step B: Define Core Economic Functions (Unchanged)
d <- function(p) a - b*p
stock.time <- function(T, r, R) R - a*T + (a/r)*(1 - exp(-r*T))

# Step C: Solve for all three paths

# Path 1: Baseline (Solid Line)
Tf_base <- uniroot(stock.time, c(0, 1000), r = r_base, R = R0_base)$root
time_base <- seq(0, Tf_base, length.out = 100)
price_base <- pT * exp(-r_base * (Tf_base - time_base))
quantity_base <- d(price_base)
stock_base <- stock.time(time_base, r = r_base, R = R0_base)

# Path 2: Interest Rate Shock (Dotted Line)
Tf_r_shock <- uniroot(stock.time, c(0, 1000), r = r_shock, R = R0_base)$root
time_r_shock <- seq(0, Tf_r_shock, length.out = 100)
price_r_shock <- pT * exp(-r_shock * (Tf_r_shock - time_r_shock))
quantity_r_shock <- d(price_r_shock)
stock_r_shock <- stock.time(time_r_shock, r = r_shock, R = R0_base)

# Path 3: Resource Stock Shock (Dashed Line)
Tf_R_shock <- uniroot(stock.time, c(0, 1000), r = r_base, R = R0_shock)$root
time_R_shock <- seq(0, Tf_R_shock, length.out = 100)
price_R_shock <- pT * exp(-r_base * (Tf_R_shock - time_R_shock))
quantity_R_shock <- d(price_R_shock)
stock_R_shock <- stock.time(time_R_shock, r = r_base, R = R0_shock)


# PART 2: THE UPGRADED MULTI-SHOCK PLOTTING FUNCTION

create_multi_shock_plot <- function(time_base, price_base, stock_base,
                                    time_r_shock, price_r_shock, stock_r_shock,
                                    time_R_shock, price_R_shock, stock_R_shock) {
  
  if (dev.cur() != 1) dev.off()
  layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
  par(oma = c(0, 0, 0, 0))
  
  # Plot 1: Demand Curve (Top-Left)
  par(mar = c(0, 5, 5, 0))
  p_demand <- seq(0, pT, length.out = 100); q_demand <- d(p_demand)
  plot(q_demand, p_demand, type = "l", lwd = 2,
       xaxs = "i", yaxs = "i", xlim = rev(range(q_demand)), ylim = c(0, max(p_demand)),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  axis(side = 2, las = 1); mtext("Net Price (Pt)", side = 2, line = 3)
  axis(side = 3); box()
  
  # Plot 2: Price Path (Top-Right)
  par(mar = c(0, 0, 5, 5))
  # Set limits to include all three datasets
  plot(time_base, price_base, type = "l", lwd = 2, lty = 1, # solid
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_r_shock, time_R_shock))),
       ylim = c(0, max(c(price_base, price_r_shock, price_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  # Overlay the two shocks
  lines(time_r_shock, price_r_shock, lwd = 2, lty = 3) # dotted
  lines(time_R_shock, price_R_shock, lwd = 2, lty = 2) # dashed
  axis(side = 3); axis(side = 4, las = 1)
  mtext("Net Price (Pt)", side = 4, line = 3); box()
  
  # Plot 3: Reserve Path (Bottom-Left)
  par(mar = c(5, 5, 0, 0))
  plot(stock_base, time_base, type = "l", lwd = 2, lty = 1, # solid
       xaxs = "i", yaxs = "i",
       xlim = rev(range(c(stock_base, stock_r_shock, stock_R_shock))),
       ylim = rev(range(c(time_base, time_r_shock, time_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(stock_r_shock, time_r_shock, lwd = 2, lty = 3) # dotted
  lines(stock_R_shock, time_R_shock, lwd = 2, lty = 2) # dashed
  axis(side = 1); mtext("Demand (Quantity) / Reserves (R)", side = 1, line = 3)
  axis(side = 2, las = 1); mtext("Time (t)", side = 2, line = 3); box()
  
  # Plot 4: 45-degree Line (Bottom-Right) & Legend
  par(mar = c(5, 0, 0, 5))
  plot(time_base, time_base, type = "l", lwd = 2, lty = 1, # solid
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_r_shock, time_R_shock))),
       ylim = rev(range(c(time_base, time_r_shock, time_R_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(time_r_shock, time_r_shock, lwd = 2, lty = 3) # dotted
  lines(time_R_shock, time_R_shock, lwd = 2, lty = 2) # dashed
  axis(side = 1); mtext("Time (t)", side = 1, line = 3)
  axis(side = 4, las = 1); mtext("Time (t)", side = 4, line = 3); box()
  # Add a legend to explain the line types
  legend("topright",
         legend = c("Baseline", "Interest Rate Shock", "Stock Shock"),
         lty = c(1, 3, 2), # 1=solid, 3=dotted, 2=dashed
         lwd = 2,
         bty = "n", # No box around the legend
         cex = 0.8) # Slightly smaller text
}


# PART 3: EXECUTION

create_multi_shock_plot(
  time_base, price_base, stock_base,
  time_r_shock, price_r_shock, stock_r_shock,
  time_R_shock, price_R_shock, stock_R_shock
)