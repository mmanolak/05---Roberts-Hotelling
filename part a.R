# 1. Data Generation: Model a shock to the resource stock

# Step A: Define Model Parameters
# The SHOCK is now the initial stock. The interest rate is held constant.
R0_base <- 1000  # The original, baseline resource stock (R0)
R0_shock <- 500   # The new, smaller resource stock (R'_0)

r <- 0.05         # The interest rate is now constant for both scenarios

pT <- 10          # Choke price
a <- 100          # Demand intercept
b <- a/pT         # Demand slope

# Step B: Define Core Economic Functions (Unchanged)
d <- function(p) a - b*p
stock.time <- function(T, r, R) R - a*T + (a/r)*(1 - exp(-r*T)) # Note: R is now a required argument

# Step C: Solve for the Baseline Path (Solid Lines, using R0_base)
Tf_base <- uniroot(stock.time, c(0, 1000), r = r, R = R0_base)$root
time_base <- seq(0, Tf_base, length.out = 100)
price_base <- pT * exp(-r * (Tf_base - time_base))
quantity_base <- d(price_base)
stock_base <- stock.time(time_base, r = r, R = R0_base)

# Step D: Solve for the "Shocked" Path (Dotted Lines, using R0_shock)
Tf_shock <- uniroot(stock.time, c(0, 1000), r = r, R = R0_shock)$root
time_shock <- seq(0, Tf_shock, length.out = 100)
price_shock <- pT * exp(-r * (Tf_shock - time_shock))
quantity_shock <- d(price_shock)
stock_shock <- stock.time(time_shock, r = r, R = R0_shock)


# 2. The Comparative Plotting Function (This function is unchanged)

create_comparative_plot <- function(time_base, price_base, quantity_base, stock_base,
                                    time_shock, price_shock, quantity_shock, stock_shock) {
  
  if (dev.cur() != 1) dev.off()
  layout(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
  par(oma = c(0, 0, 0, 0))
  
  # Plot 1: Demand Curve (Top-Left)
  par(mar = c(0, 5, 5, 0))
  p_demand <- seq(0, pT, length.out = 100)
  q_demand <- d(p_demand)
  plot(q_demand, p_demand, type = "l", lwd = 2,
       xaxs = "i", yaxs = "i", xlim = rev(range(q_demand)), ylim = c(0, max(p_demand)),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  axis(side = 2, las = 1); mtext("Net Price (Pt)", side = 2, line = 3)
  axis(side = 3); box()
  
  # Plot 2: Price Path (Top-Right)
  par(mar = c(0, 0, 5, 5))
  plot(time_base, price_base, type = "l", lwd = 2,
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_shock))),
       ylim = c(0, max(c(price_base, price_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(time_shock, price_shock, lwd = 2, lty = "dotted")
  axis(side = 3); axis(side = 4, las = 1)
  mtext("Net Price (Pt)", side = 4, line = 3); box()
  
  # Plot 3: Reserve Path (Bottom-Left)
  par(mar = c(5, 5, 0, 0))
  plot(stock_base, time_base, type = "l", lwd = 2,
       xaxs = "i", yaxs = "i",
       xlim = rev(range(c(stock_base, stock_shock))),
       ylim = rev(range(c(time_base, time_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(stock_shock, time_shock, lwd = 2, lty = "dotted")
  axis(side = 1); mtext("Demand (Quantity) / Reserves (R)", side = 1, line = 3)
  axis(side = 2, las = 1); mtext("Time (t)", side = 2, line = 3); box()
  
  # Plot 4: 45-degree Line (Bottom-Right)
  par(mar = c(5, 0, 0, 5))
  plot(time_base, time_base, type = "l", lwd = 2, lty = "solid",
       xaxs = "i", yaxs = "i",
       xlim = c(0, max(c(time_base, time_shock))),
       ylim = rev(range(c(time_base, time_shock))),
       xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  lines(time_shock, time_shock, lwd = 2, lty = "dotted")
  axis(side = 1); mtext("Time (t)", side = 1, line = 3)
  axis(side = 4, las = 1); mtext("Time (t)", side = 4, line = 3); box()
  
  par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1), oma = c(0,0,0,0))
}


# 3. Execution
# Call the function with the new data comparing the two resource stock levels
create_comparative_plot(
  time_base, price_base, quantity_base, stock_base,
  time_shock, price_shock, quantity_shock, stock_shock
)