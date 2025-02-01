### Author: Ryan Loveless ###
### 2-Step Binomial Trees ###

# Function to price two-step European options and plot the tree
price_option_two_step <- function(S0, K, r, sigma, T, option_type) {
  steps <- 2
  dt <- T / steps
  u <- exp(sigma * sqrt(dt))  # Up factor
  d <- 1 / u                  # Down factor
  p <- (exp(r * dt) - d) / (u - d)  # Risk-neutral probability
  
  # Stock price tree
  stock_tree <- matrix(0, nrow = steps + 1, ncol = steps + 1)
  for (i in 0:steps) {
    for (j in 0:i) {
      stock_tree[j + 1, i + 1] <- S0 * (u^j) * (d^(i - j))
    }
  }
  
  # Option value tree
  option_tree <- matrix(0, nrow = steps + 1, ncol = steps + 1)
  
  # Payoffs at maturity
  for (j in 0:steps) {
    if (option_type == "call") {
      option_tree[j + 1, steps + 1] <- max(stock_tree[j + 1, steps + 1] - K, 0)
    } else if (option_type == "put") {
      option_tree[j + 1, steps + 1] <- max(K - stock_tree[j + 1, steps + 1], 0)
    }
  }
  
  # Backward induction
  for (i in (steps - 1):0) {
    for (j in 0:i) {
      option_tree[j + 1, i + 1] <- exp(-r * dt) * (p * option_tree[j + 2, i + 2] + (1 - p) * option_tree[j + 1, i + 2])
    }
  }
  
  # Option price at t = 0
  C_0 <- option_tree[1, 1]
  
  # Delta
  delta <- (option_tree[2, 2] - option_tree[1, 2]) / (stock_tree[2, 2] - stock_tree[1, 2])
  
  # Plot the two-step binomial tree
  plot(0, 0, type="n", xlab="", ylab="", xlim=c(-1, 3), ylim=c(-2, 2), axes=FALSE,
       main=paste("Two-Step Binomial Tree (", option_type, ")", sep = ""))
  segments(0, 0, 1, 1)   # Root to up
  segments(0, 0, 1, -1)  # Root to down
  segments(1, 1, 2, 1.5) # Up to upper
  segments(1, 1, 2, 0.5) # Up to middle
  segments(1, -1, 2, -0.5) # Down to middle
  segments(1, -1, 2, -1.5) # Down to lower
  text(0, 0.2, paste("S =", S0), col="blue", cex=1.2)
  text(1, 1.2, paste("S =", round(stock_tree[2, 2], 4)), col="blue", cex=1.2)
  text(1, -0.8, paste("S =", round(stock_tree[1, 2], 4)), col="blue", cex=1.2)
  text(2, 1.7, paste("S =", round(stock_tree[3, 3], 4)), col="blue", cex=1.2)
  text(2, 0.7, paste("S =", round(stock_tree[2, 3], 4)), col="blue", cex=1.2)
  text(2, -1.3, paste("S =", round(stock_tree[1, 3], 4)), col="blue", cex=1.2)
  text(0, -0.2, paste("C =", round(C_0, 4)), col="green", cex=1.2)
  text(1, 0.8, paste("C =", round(option_tree[2, 2], 4)), col="green", cex=1.2)
  text(1, -1.2, paste("C =", round(option_tree[1, 2], 4)), col="green", cex=1.2)
  text(2, 1.3, paste("C =", round(option_tree[3, 3], 4)), col="green", cex=1.2)
  text(2, 0.3, paste("C =", round(option_tree[2, 3], 4)), col="green", cex=1.2)
  text(2, -1.7, paste("C =", round(option_tree[1, 3], 4)), col="green", cex=1.2)
  
  return(list(price = C_0, delta = delta))
}

# Example usage
result <- price_option_two_step(
  S0 = 98,
  K = 97,
  r = 0.06,
  sigma = 0.15,
  T = 0.5,
  option_type = "put"
)
cat("Two-Step European Option Price:", result$price, "\n")
cat("Delta at the root:", result$delta, "\n")
