## ----------------------      ##
## Derivatives Portfolio Pnl   ##
## Name: Ryan Loveless         ##                 
## ----------------------      ##


# Load required libraries
library(ggplot2)
library(e1071) # For skewness and kurtosis
library(dplyr) # For data manipulation and output formatting

# Define inputs
market_data <- list(
  spot = 100,                 # Initial spot price
  volatility = 0.2,           # Annualized volatility
  risk_free_rate = 0.05,      # Risk-free interest rate
  expected_drift = 0.05       # Drift of the asset price
)

portfolio_data <- list(
  maturity = 0.02,            # (years)
  cash_balance = 0            # Cash balance in the portfolio
)

simulation_data <- list(
  num_simulations = 100000,   # Number of simulations
  low_payout = 90,            # Lower bound for spot price range
  high_payout = 110           # Upper bound for spot price range
)

# Portfolio positions
portfolio <- data.frame(
  Type = c("Call", "Forward"),        # Instrument types
  Number = c(0, 100),                 # Number of contracts
  Strike = c(95.4815, 0),             # Strike prices
  Value = c(4.66, 100)                # Initial value 
)

# Black-Scholes Pricing Function
black_scholes <- function(S, K, T, r, sigma, type) {
  d1 <- (log(S / K) + (r + (sigma^2) / 2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  if (type == "Call") {
    return(S * pnorm(d1) - K * exp(-r * T) * pnorm(d2))
  } else if (type == "Put") {
    return(K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1))
  } else {
    stop("Invalid option type. Use 'Call' or 'Put'.")
  }
}

# Simulate Spot Prices
simulate_spot <- function(S0, mu, sigma, T, n) {
  z <- rnorm(n)
  return(S0 * exp((mu - sigma^2 / 2) * T + sigma * sqrt(T) * z))
}

# Portfolio Valuation Function
value_portfolio <- function(spot_prices, portfolio, T, r, cash_balance) {
  portfolio_value <- numeric(length(spot_prices))
  
  for (i in seq_along(spot_prices)) {
    spot <- spot_prices[i]
    total_value <- 0
    
    # Calculate payoffs for each instrument
    for (j in 1:nrow(portfolio)) {
      position <- portfolio[j, ]
      if (position$Type == "Call") {
        payoff <- pmax(spot - position$Strike, 0)
      } else if (position$Type == "Put") {
        payoff <- pmax(position$Strike - spot, 0)
      } else if (position$Type == "Forward") {
        payoff <- spot - position$Strike
      } else {
        payoff <- 0
      }
      # Aggregate payoffs, accounting for the number of contracts
      total_value <- total_value + position$Number * payoff
    }
    
    # Add the value of cash at maturity
    cash_at_maturity <- cash_balance * exp(r * T)
    total_value <- total_value + cash_at_maturity
    
    # Store the portfolio value for this spot price
    portfolio_value[i] <- total_value
  }
  
  return(portfolio_value)
}

# Main Simulation
set.seed(123)
S0 <- market_data$spot
mu <- market_data$expected_drift
sigma <- market_data$volatility
T <- portfolio_data$maturity
r <- market_data$risk_free_rate
n <- simulation_data$num_simulations

# Simulate spot prices
spot_prices <- simulate_spot(S0, mu, sigma, T, n)

# Initial Portfolio Value 
initial_value <- value_portfolio(c(S0), portfolio, T, r, cash_balance = 0)[1]  

# Portfolio values at maturity (include cash growth at maturity)
portfolio_values <- value_portfolio(spot_prices, portfolio, T, r, cash_balance = portfolio_data$cash_balance)

# Future Value of Cash at Maturity
cash_future_value <- portfolio_data$cash_balance * exp(r * T)

# Statistical Analysis
pnl_mean <- mean(portfolio_values)
pnl_sd <- sd(portfolio_values)
pnl_skewness <- skewness(portfolio_values)
pnl_kurtosis <- kurtosis(portfolio_values)

# Correct Average Future Portfolio Value Calculation
average_future_value <- mean(portfolio_values)  # Mean of portfolio_values includes instruments + cash growth

# Portfolio Outputs
portfolio_output <- data.frame(
  Metric = c(
    "Portfolio Value", 
    "Average Future Portfolio Value", 
    "Standard Deviation", 
    "Skew", 
    "Kurtosis"
  ),
  Value = c(
    round(initial_value, 2), 
    round(average_future_value, 2),
    round(pnl_sd, 2), 
    round(pnl_skewness, 4), 
    round(pnl_kurtosis, 4)
  )
)

# Print Portfolio Outputs
cat("Portfolio Outputs:\n")
print(portfolio_output)


# Define spot price range for the payout diagram
spot_range <- seq(simulation_data$low_payout, simulation_data$high_payout, by = 1)

# Calculate payouts for the defined spot range
payouts <- value_portfolio(spot_range, portfolio, T, r, cash_balance = portfolio_data$cash_balance)

# Payout Diagram
payout_df <- data.frame(Spot = spot_range, Payout = payouts)
ggplot(payout_df, aes(x = Spot, y = Payout)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal(base_size = 14) +
  labs(title = "Payout Diagram", x = "Asset Level", y = "Payout") +
  scale_y_continuous(labels = scales::comma)

# PnL Histogram
ggplot(data.frame(PnL = portfolio_values), aes(x = PnL)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Portfolio PnL Distribution", x = "PnL", y = "Frequency of Payout")
