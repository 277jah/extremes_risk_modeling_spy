library(quantmod)


getSymbols("SPY")
chartSeries(SPY)
spy_r <- dailyReturn(SPY; type = "log")
chart.CumReturns(spy_r, wealth.index = TRUE, geometric = TRUE)


u <- rev(sort(spy_r))

n <- length(spy_r)
e <- (cumsum(u)-(1:n)*u)/(1:n)
plot(u, e, type = "l", xlab = "u", ylab = "e(u)")

mxfPlot(u, u = quantile(u, 0.05), doplot = TRUE, labels = TRUE)  
mxfPlot(u, u = quantile(u, 0.01), doplot = TRUE, labels = TRUE)
mxfPlot(u, u = quantile(u, 0.001), doplot = TRUE, labels = TRUE)

mrlPlot(u, ci = 0.95, umin = mean(u), umax = max(u), nint = 100, doplot = TRUE, 
        plottype = c("autoscale", ""), labels = TRUE)  
msratioPlot(u, p = 1:4, doplot = TRUE, labels = TRUE) 
set.seed(4330)
nt <- 10000
x3 <- rt(nt,df=3)
#mrlPlot(x3, ci = 0.95, umin = mean(x3), umax = max(x3), nint = 100, doplot = TRUE, 
       # plottype = c("autoscale", ""), labels = TRUE)
msratioPlot(x3, p = 1:4, doplot = TRUE, labels = TRUE)


VaR(u)
CVaR(u)

#bloc <- blockTheta(u, block = 22, quantiles = seq(0.95, 0.995, length = 10), title = NULL, description = NULL)

exindexesPlot(u, block = 22, quantiles = seq(0.950, 0.995, length = 10), 
              doplot = TRUE, labels = TRUE)


fit <- gpdFit(u, u = quantile(u, 0.95), type = c("mle", "pwm"), information =
         c("observed", "expected"), title = NULL, description = NULL)
  par(mfrow = c(2, 2), cex = 0.7)

summary(fit)


tailPlot(fit)

#tailSlider(fit)
tailRisk(fit)

VaR(u, alpha = 0.01)
CVaR(u, alpha = 0.01)
last(SPY)
drop <- -.01715589*509.83
drop

chart.Histogram(spy_r,methods = c("add.density", "add.rug"))

tsp_r <- as.timeSeries(spy_r)

drawdownPlot(tsp_r, wealth.index = TRUE, geometric = TRUE, labels = TRUE)
        
fit <- assetsFit(tsp_r, method = "st", type = "mle", information = "observed", 
                 title = NULL, description = NULL)
print(fit)
summary(fit)
tailPlot(fit)


library(ggplot2)

# # Assuming 'xi' and 'beta' are your estimated parameters
xi <- 0.288337803
 beta <- 0.008038788
# 
# # Generate a sequence of values for the x-axis (adjust the range according to your data)
# x_seq <- seq(from = min(tsp_r), to = max(tsp_r), length.out = 1000)
# 
# # Calculate the density of the GPD for each x value (replace 'dgp' with the correct function or formula)
# # This is a placeholder; you'll need to replace it with the actual computation for GPD density
# gpd_density <- dgpd(tsp_r, shape = xi, scale = beta)
# # Assuming 'xi' (shape) and 'beta' (scale) are your estimated parameters
# xi <- 0.288337803
# beta <- 0.008038788

# Load the evd package
library(evd)

# Generate a sequence of values for the x-axis (adjust according to your data)
x_seq <- seq(from = min(tsp_r), to = max(tsp_r), length.out = 4330)

# Calculate the density of the GPD for each x value
gpd_density <- dgpd(x_seq, loc = 0, scale = beta, shape = xi) # Assuming loc = 0 if not estimated

# Now you can plot this density using your preferred method

# Create a data frame for plotting
plot_data <- data.frame(tsp_r, density = gpd_density)

# Plot the GPD fit
ggplot(plot_data, aes(x = x, y = density)) +
  geom_line() +
  ggtitle("Estimated GPD Density") +
  xlab("Value") +
  ylab("Density")

# Assuming 'xi' and 'beta' are your estimated parameters
