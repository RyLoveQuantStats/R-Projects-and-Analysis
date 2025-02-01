# Load necessary library
library(ggplot2)

# Function to calculate the payoff of an individual financial instrument
financial_payoff <- function(stock_price, strike_price, type, position) {
  # type: "call", "put", "forward", "bond", or "share"
  # position: 1 for long, -1 for short
  if (type == "call") {
    payoff <- pmax(stock_price - strike_price, 0) * position
  } else if (type == "put") {
    payoff <- pmax(strike_price - stock_price, 0) * position
  } else if (type == "forward") {
    payoff <- (stock_price - strike_price) * position
  } else if (type == "bond") {
    payoff <- strike_price * position # For bonds, strike_price represents the payment
  } else if (type == "share") {
    # Ignore strike_price for shares
    payoff <- stock_price * position
  } else {
    stop("Invalid type. Use 'call', 'put', 'forward', 'bond', or 'share'.")
  }
  return(payoff)
}

# Function to calculate total payout for a portfolio of instruments
portfolio_payout <- function(stock_prices, portfolio) {
  # portfolio: a data frame with columns: "strike_price", "type", "position"
  total <- rep(0, length(stock_prices))
  for (i in 1:nrow(portfolio)) {
    total <- total + financial_payoff(stock_prices, portfolio$strike_price[i], 
                                      portfolio$type[i], portfolio$position[i])
  }
  return(total)
}

# Define a range of stock prices
stock_prices <- seq(0, 50, by = 1)

##################### Define a portfolio ####################
portfolio <- data.frame(
  strike_price = c(35,10,25,15,40,20),  # Strike prices; NA for shares
  type = c("call", "call","call", "call","call", "call"),  # Type of instrument
  position = c(1, 1, 1,-1,-1,-1)  # Position: 1 for long, -1 for short
)

# Calculate the total payout for the portfolio
payouts <- portfolio_payout(stock_prices, portfolio)

# Create a data frame for plotting
plot_data <- data.frame(stock_price = stock_prices, payout = payouts)

# Create the summary label dynamically
summary_text <- "Portfolio Summary:\n"
for (i in 1:nrow(portfolio)) {
  strike_text <- ifelse(is.na(portfolio$strike_price[i]), "N/A", portfolio$strike_price[i])
  instrument_text <- paste(
    "Instrument", i, ":", portfolio$type[i], 
    ", Quantity", portfolio$position[i], 
    ", Strike", strike_text
  )
  summary_text <- paste(summary_text, instrument_text, sep = "\n")
}

# Plot the payout diagram
ggplot(plot_data, aes(x = stock_price, y = payout)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = na.omit(portfolio$strike_price), linetype = "dotted", color = "red") +
  scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 5)) +
  scale_y_continuous(limits = c(-50, 50), breaks = seq(-50, 50, by = 10)) +
  labs(title = "Payout Diagram", x = "Stock Price", y = "Payout Value") +
  theme_minimal() +
  annotate(
    "text", x = 0, y = -20, label = summary_text, size = 4, hjust = 0, vjust = 1,
    color = "black", fontface = "bold", lineheight = 1.2
  )
