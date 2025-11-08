# Simple Hotelling model

R0 <- 1000  # resource stock
pT <- 10    # choke price (where Q=0)
a <- 100    # consumption at p=0
b <- a/pT   # slope of demand
cb <- 6		  # backstop technology price

# Demand
d   <- function(p) a - b*p # save in case you want to plot quantities

# time to exhaustion: solve implicit functin using uniroot()
stock.time <- function(T, r, R=R0) R - a*T + (a/r)*(1 - exp(-r*T))
Tf <- uniroot(stock.time, c(0, 1000), r=0.05)$root

# time series for plot
t.s <- seq(0,Tf, length=100)
p <- pT*exp(-0.05*(Tf-t.s))

par(bty="l")
plot( t.s, p, type = "l", lwd=3,
      xlab = "Time period (t)",
      ylab = "Price (p)",
      ylim = c(0,10),
      xlim = c(0,50),
      main = "How r affects time path of price"
)
text(t.s[30], p[30]+1, "r=0.05") 

# change r to 0.10
Tf <- uniroot(stock.time, c(0, 1000), r=0.10)$root
t.s <- seq(0,Tf, length=100)
p <- pT*exp(-0.10*(Tf-t.s))
lines( t.s, p, col="red", lwd=3,
       xlab = "Time period (t)",
       ylab = "Price (p)"   
)
text(t.s[90]-2, p[90]+1, "r=0.10", col="red")

# change r to 0.02
Tf <- uniroot(stock.time, c(0, 1000), r=0.01)$root
t.s <- seq(0,Tf, length=100)
p <- pT*exp(-0.01*(Tf-t.s))
lines( t.s, p, col="blue", lwd=3,
       xlab = "Time period (t)",
       ylab = "Price (p)"   
)
text(t.s[70], p[70]+0.5, "r=0.02", col="blue")


# Suppose r unexpectedly increases by 300 in period 12 (say, 
# an unexpected discovery of a new deposit)

# First assume path along anticipated path, r=0.05 the whole time
Tf <- uniroot(stock.time, c(0, 1000), r=0.05)$root
t.s <- seq(0,Tf, length=100)
p <- pT*exp(-0.05*(Tf-t.s))

# Now calculate stock remaining at period 12
R1   <- stock.time(12, r=0.05)
NewR <- R1 + 300           # add 300 for the new discovery

# Resolve new problem going forward
Tf2  <- 12 + uniroot(stock.time, c(0, 1000), r=0.05, R=NewR)$root
len.s2 <- 100 - length( t.s[t.s <12] )
t.s2 <- seq(12, Tf2, length = len.s2 )
t.s  <- c( t.s[t.s <12], t.s2 )
p    <- c( p[t.s <12], pT*exp(-0.05*(Tf2-t.s2)) )

plot( t.s, p, type = "l", lwd=3, xlim=c(0,70),
      xlab = "Time period (t)",
      ylab = "Price (p)",
      main = "Resource stock unexpectedly increases at t=12"
)

# Suppose r unexpectedly increases from 0.05 to 0.10 in period 12
Tf <- uniroot(stock.time, c(0, 1000), r=0.05)$root
t.s <- seq(0,Tf, length=100)
p <- pT*exp(-0.05*(Tf-t.s))

# Calculate stock remaining at period 12
R1   <- stock.time(12, r=0.05)
# Find new time to exhaustion
Tf2  <- 12 + uniroot(stock.time, c(0, 1000), r=0.10, R=R1)$root
# Create sequences for plotting
t.12 <- seq(12, Tf2, length=50)
t.s2  <- c( t.s[t.s <12], t.12 )
p2    <- c( p[t.s <12], pT*exp(-0.10*(Tf2-t.12)) )

plot( t.s, p, bty="l",
      type = "l", lwd=2, xlim=c(0,30), ylim=c(0,10),
      xlab = "Time period (t)",
      ylab = "Price (p)",
      main = "r unexpectedly and permanently changes at t=12"
)
lines( t.s2, p2, col="blue", lwd=2)
text(t.s[65], p[65]+1, "r= 0.05")
text(t.s2[75], p2[75]+1.1, "r= 0.10", col="blue")


# Consider backstop price, cb
# time to exhaustion is different now
stock.time.backstop <- function(T, r, R=R0) R - a*T + (b*cb/r)*(1 - exp(-r*T))
Tf.b <- uniroot(stock.time.backstop, c(0, 1000), r=0.05)$root

# plot with and without backstop

# no backstop
Tf <- uniroot(stock.time, c(0, 1000), r=0.05)$root

# time series for plot
t.s <- seq(0,Tf, length=100)
p <- pT*exp(-0.05*(Tf-t.s))

par(bty="n")
plot( t.s, p, type = "l", lwd=3,
      xlab = "Time period (t)",
      ylab = "Price (p)",
      ylim = c(0,10),
      xlim = c(0,50),
      main = "Time path of price with and without backstop"
)
text(t.s[90], p[90]+1, "No backstop, r=0.05", pos=2)

#backstop
t.s <- seq(0,Tf.b, length=100)
p <- cb*exp(-0.05*(Tf.b-t.s))

# add a flat section at cb price
t.s <- c(t.s, t.s[100]+10)
p <- c(p, p[100])

lines( t.s, p, type = "l", lwd=3,
       col="blue"
)
text(t.s[101], p[101]-0.2, "backstop cost=6, r=0.05", pos=4, col="blue")


# Tax of $2 per unit: tax is like a per-unit tax on production.  
# It may be best to consider this an extra cost of production.
# Now the resource rent is (price - tax), not price, and this ought 
# to rise at the rate of interest.

# time to exhaustion: 
# p_T =  a/b  choke price
# (p_T - tax) = (p_0 - tax)*exp(rT)
# (a/b - tax) = (p_0 - tax)*exp(rT)  -->  p_0 = (a/b - tax) exp(-rT) + tax
# p_t = (a/b - tax) exp(r(t-T)) + tax
# q_t = a - bp_t = a - b ( (a/b - tax) exp(r(t-T)) + tax )
#                = a - b ( (a/b) exp(r(t-T)) - tax exp(r(t-T)) + tax )
#                = a - a exp(r(t-T))  + b*tax*exp(r(t-T)) - b*tax
#     = a - b*tax - (a - b*tax) exp(r(t-T)) 
# R = int_0^T q(t) dt = (a - b*tax)*t - (a - b*tax)/r exp(r(t-T)) |_0^T
#   =  (a - b*tax)*T - (a - b*tax)/r + (a - b*tax)/r exp(-rT)

stock.time.tax <- function(T, r, tax=2, R=R0) R - (a - b*tax)*T + (a - b*tax)/r*(1 - exp(-r*T))
Tf.tax <- uniroot(stock.time.tax, c(0, 1000), r=0.05)$root
tax=2
r=0.05
t = seq(0,Tf.tax, length=100)
p.tax = (a/b - tax)*exp(r*(t-Tf.tax)) + tax

#pdf("~/Google Drive/Teaching/Econ-638/Fall2019/ProblemSets/PS1/figures/hotellingTax.pdf")
par(bty="l")
plot( t.s, p, type = "l", lwd=3,
      xlab = "Time period (t)",
      ylab = "Price (p)",
      ylim = c(0,10),
      xlim = c(0,30),
      main = "Time path of price with and without tax"
)
text(t.s[93], p[90]+1, "No tax, r=0.05", pos=2)
lines(t, p.tax, col="blue", lwd=3)
text(t[96], p.tax[93]-2, "Tax=2, r=0.05", col="blue",pos=2)
dev.off()


### Extraction Cost function:  c(q, Q, t) = (2q + Q/250) e^{-0.03t} 
# mc_t = (2 + 1/250)*exp(-0.03*t)  = 2.004*exp(-0.03*t)
#   DEF: m0 = 2.004, m1 = 0.03
# (p_T - mc_T) = (p_0 - mc_0)*exp(r*T)
# (a/b - mc_T) = (p_0 - mc_0)*exp(r*T) --> p_0 = (a/b - mc_T)*exp(-rT) + mc_0
# p_t = (a/b - mc_T) exp(r(t-T)) + mc_t
#     = (a/b - m0*exp(-m1*T))*exp(r*(t-T))  + m0*exp(-m1*t)

# q_t = a - b*p_t = a - b( (a/b - m0*exp(-m1*T))*exp(r*(t-T)) + m0*exp(-m1*t) )
#                 = a - exp(r*(t-T))*(a - b*m0*exp(-m1*T))  - b*m0*exp(-m1*t) 
# R = int_0^T q(t) dt = a*t - exp(r*(t-T))*(a - b*m0*exp(-m1*T))/r + 
#                         b*m0*exp(-m1*t) / m1 |_0^T
#                     = a*T - (a - b*m0*exp(-m1*T))/r + b*m0*exp(-m1*T)/m1 +
#                          exp(-rT)*(a - b*m0*exp(-m1*T))/r  - b*m0/m1 

stock.time.cost <- function(T, r, m0=2.004, m1=0.10, R=R0){
  R - a*T + (a - b*m0*exp(-m1*T))/r - b*m0*exp(-m1*T)/m1 -
    exp(-r*T)*(a - b*m0*exp(-m1*T))/r  + b*m0/m1 
}

r=0.05
m0 = 2.004; m1 = 0.03
Tf.cost <- uniroot( stock.time.cost, c(0,1000), r=r, m0=m0, m1=m1 )$root
t = seq(0, Tf.cost, length=100)
p.cost = (a/b - m0*exp(-m1*Tf.cost))*exp(r*(t-Tf.cost)) + m0*exp(-m1*t)
plot(t, p.cost, type="l", bty="l", 
     xlim=c(0, 45), ylim=c(0, 11),
     xlab = "Years",
     ylab = "Price")

r=0.05
m0 = 0.1; m1 = 0.01
Tf.cost <- uniroot( stock.time.cost, c(0,1000), r=r, m0=m0, m1=m1 )$root
t = seq(0, Tf.cost, length=100)
p.cost = (a/b - m0*exp(-m1*Tf.cost))*exp(r*(t-Tf.cost)) + m0*exp(-m1*t)
lines(t , p.cost, col="blue")

r=0.05
m0 = 5; m1 = 0.10
Tf.cost <- uniroot( stock.time.cost, c(0,1000), r=r, m0=m0, m1=m1 )$root
t = seq(0, Tf.cost, length=100)
p.cost = (a/b - m0*exp(-m1*Tf.cost))*exp(r*(t-Tf.cost)) + m0*exp(-m1*t)
lines(t , p.cost, col="green")

r=0.05
m0 = 6; m1 = 0.20
Tf.cost <- uniroot( stock.time.cost, c(0,1000), r=r, m0=m0, m1=m1 )$root
t = seq(0, Tf.cost, length=100)
p.cost = (a/b - m0*exp(-m1*Tf.cost))*exp(r*(t-Tf.cost)) + m0*exp(-m1*t)
lines(t , p.cost, col="magenta")

r=0.05
m0 = 4; m1 = 0.15
Tf.cost <- uniroot( stock.time.cost, c(0,1000), r=r, m0=m0, m1=m1 )$root
t = seq(0, Tf.cost, length=100)
p.cost = (a/b - m0*exp(-m1*Tf.cost))*exp(r*(t-Tf.cost)) + m0*exp(-m1*t)
lines(t , p.cost, col="red")

r=0.05
m0 = 7; m1 = 0.10
Tf.cost <- uniroot( stock.time.cost, c(0,1000), r=r, m0=m0, m1=m1 )$root
t = seq(0, Tf.cost, length=100)
p.cost = (a/b - m0*exp(-m1*Tf.cost))*exp(r*(t-Tf.cost)) + m0*exp(-m1*t)
lines(t , p.cost, col="orange")

legend(30, 4, c("m0=2.004, m1=0.03",
                "m0=0.1, m1=0.01",
                "m0=5, m1=0.10",
                "m0=6, m1=0.2",
                "m0=4, m1=0.15",
                "m0=7, m1=0.10"),
       col=c("black","blue","green","magenta","red","orange"),
       lty=1, cex=0.8, bty="n"
)


