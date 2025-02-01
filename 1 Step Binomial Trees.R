# Function to price single-step European options and plot the tree
price_option_single_step <- function(S0, K, r, T, option_type, up_price, down_price) {
  dt <- T  # Single step
  p <- (exp(r * dt) - (down_price / S0)) / ((up_price / S0) - (down_price / S0))  # Risk-neutral probability
  
  # Payoff calculation
  if (option_type == "call") {
    Payoff_u <- max(up_price - K, 0)
    Payoff_d <- max(down_price - K, 0)
  } else if (option_type == "put") {
    Payoff_u <- max(K - up_price, 0)
    Payoff_d <- max(K - down_price, 0)
  } else {
    stop("Invalid option type. Use 'call' or 'put'.")
  }
  
  # Option price at t = 0
  C_0 <- exp(-r * dt) * (p * Payoff_u + (1 - p) * Payoff_d)
  
  # Delta
  delta <- (Payoff_u - Payoff_d) / (up_price - down_price)
  
  # Plot the one-step binomial tree
  plot(0, 0, type="n", xlab="", ylab="", xlim=c(-0.5, 1.5), ylim=c(-1, 1), axes=FALSE,
       main=paste("Single-Step Binomial Tree (", option_type, ")", sep = ""))
  segments(0, 0, 1, 0.5)  # Root to up
  segments(0, 0, 1, -0.5) # Root to down
  text(0, 0.2, paste("S =", S0), col="blue", cex=1.2)
  text(1, 0.7, paste("S =", up_price), col="blue", cex=1.2)
  text(1, -0.3, paste("S =", down_price), col="blue", cex=1.2)
  text(0, -0.2, paste("C =", round(C_0, 4)), col="green", cex=1.2)
  text(1, 0.3, paste("C =", round(Payoff_u, 4)), col="green", cex=1.2)
  text(1, -0.7, paste("C =", round(Payoff_d, 4)), col="green", cex=1.2)
  
  return(list(price = C_0, delta = delta))
}

# Example usage
result <- price_option_single_step(
  S0 = 52,
  K = 47,
  r = 0.02,
  T = 0.25,
  option_type = "put",
  up_price = 56,
  down_price = 45
)
cat("Single-Step European Option Price:", result$price, "\n")
cat("Delta at the root:", result$delta, "\n")
