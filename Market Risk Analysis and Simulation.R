## ---------------------- ##
## Midterm 1 Script       ##
## Name:                  
## ---------------------- ##

## Use comments to convey your logic.  Logic can earn partial credit
## When you download this, save the filename as "LastName_midterm1.R" (worth 5 points)

## Question 1 ##
#### GrossRet_YTD is stored as a character. 
#### To fix this Jeremiah needs to use the as.numeric function:
#### tsla$GrossRet_YTD <- as.numeric(tsla$GrossRet_YTD) hist(tsla$GrossRet_YTD) 


## Question 2 ##

mean_return <- 0.12
sd_return <- 0.08
VaR_10 <- mean_return + sd_return * qnorm(0.1)
VaR_10



## Question 3 ##

#### 25
#### The probability of selecting any of the numbers would be 1/4; so 25%

## Question 4 ##

set.seed(111)

# Number of friends and weeks
friendcount <- 30
weeksinyear <- 52

# Number of simulations
a_bunch <- 50000

# Store the results
party_weeks <- rep(NA, a_bunch)

# Simulate 50,000 times
for (i in 1:a_bunch) {
  birthdays <- sample(1:weeksinyear, size=friendcount, replace=TRUE) # Simulate random birthdays across 52 weeks
  party_weeks[i] <- length(unique(birthdays)) # Count the number of unique weeks with at least one birthday
}

# Calculate the mean number of weeks Clara is invited to parties
mean_party_weeks <- mean(party_weeks)
mean_party_weeks

####### Problem 4C ##########

# Setting the seed
set.seed(111)

# Store results of birthday conflicts
birthday_conflicts <- c()

# Simulate 50,000 times
for (i in 1:a_bunch) {
  birthdays <- sample(1:weeksinyear, size=friendcount, replace=TRUE) # Simulate random birthdays across 52 weeks
  birthday_count <- table(birthdays) # Create a frequency table of birthdays per week
  conflict_check <- max(birthday_count) >= 3 # Check if any week has 3 or more birthdays
  birthday_conflicts <- c(birthday_conflicts, ifelse(conflict_check, 1, 0)) # Append the result (1 for conflict, 0 for no conflict) to the vector
}

# Calculate the probability of a birthday conflict
conflict_probability <- mean(birthday_conflicts, na.rm = TRUE)
conflict_probability



## Question 5 ##

# Install the necessary package if you haven't already (you only need to do this one time)
install.packages("quantmod")

# Load the package (you need to do this for every session to make quantmod's functions available)
library(quantmod)

# Get SPY, MRNA, and UAL data from Yahoo Finance for the last 5 years
getSymbols("SPY", src = "yahoo", from = as.Date("2020-01-01"), to = Sys.Date())
getSymbols("MRNA", src = "yahoo", from = as.Date("2020-01-01"), to = Sys.Date())
getSymbols("UAL", src = "yahoo", from = as.Date("2020-01-01"), to = Sys.Date())

# Inspecting the first few rows of each dataset to verify data
head(SPY)
head(MRNA)
head(UAL)

# Calculate simple daily returns for each ticker
spy_daily_returns <- Delt(SPY[, "SPY.Adjusted"], k = 1)
mRNA_daily_returns <- Delt(MRNA[, "MRNA.Adjusted"], k = 1)
ual_daily_returns <- Delt(UAL[, "UAL.Adjusted"], k = 1)

# Rename the returns columns
names(spy_daily_returns) <- "spy_daily_ret"
names(mRNA_daily_returns) <- "mRNA_daily_ret"
names(ual_daily_returns) <- "ual_daily_ret"

# Plot histograms with the given specifications
# SPY
hist(spy_daily_returns, breaks = 50, probability = TRUE, 
     main = "Histogram of SPY Daily Returns", 
     xlab = "Return for SPY", col = "lightgoldenrodyellow",
     las = 2)
abline(v = mean(spy_daily_returns, na.rm = TRUE), col = "blue", lty = 2)
# The histogram is mostly symmetric with a sharp peak (leptokurtic) and heavier tails, indicating more extreme values than a normal distribution.
# Symmetric with a sharp peak, less extreme tails compared to others, indicating a near-normal distribution with some leptokurtosis.


# MRNA
hist(mRNA_daily_returns, breaks = 50, probability = TRUE, 
     main = "Histogram of MRNA Daily Returns", 
     xlab = "Return for MRNA", col = "lightgoldenrodyellow",
     las = 2)
abline(v = mean(mRNA_daily_returns, na.rm = TRUE), col = "blue", lty = 2)
# The histogram is mostly symmetric with a sharp peak (leptokurtic) and heavier tails, indicating more extreme values than a normal distribution.
# Symmetric but with wider tails than SPY, suggesting higher volatility and more extreme returns, typical for a volatile stock.

# UAL
hist(ual_daily_returns, breaks = 50, probability = TRUE, 
     main = "Histogram of UAL Daily Returns", 
     xlab = "Return for UAL", col = "lightgoldenrodyellow",
     las = 2)
abline(v = mean(ual_daily_returns, na.rm = TRUE), col = "blue", lty = 2)
# The histogram is mostly symmetric with a sharp peak (leptokurtic) and heavier tails, indicating more extreme values than a normal distribution.
# Highly leptokurtic and wide tails, indicating the most extreme returns of the three, reflecting significant market volatility.

###########Part C##########
# Convert daily data to monthly data using OHLC format
SPY_monthly <- to.monthly(SPY, indexAt = "lastof", OHLC = TRUE)
MRNA_monthly <- to.monthly(MRNA, indexAt = "lastof", OHLC = TRUE)
UAL_monthly <- to.monthly(UAL, indexAt = "lastof", OHLC = TRUE)

# Extract only the Adjusted Close prices at the end of each month
SPY_monthly_adj <- to.monthly(SPY[, "SPY.Adjusted"], indexAt = "lastof", OHLC = FALSE)
MRNA_monthly_adj <- to.monthly(MRNA[, "MRNA.Adjusted"], indexAt = "lastof", OHLC = FALSE)
UAL_monthly_adj <- to.monthly(UAL[, "UAL.Adjusted"], indexAt = "lastof", OHLC = FALSE)

# Calculate simple monthly returns using the Delt function
spy_returns <- Delt(SPY_monthly_adj, k = 1)
mRNA_returns <- Delt(MRNA_monthly_adj, k = 1)
ual_returns <- Delt(UAL_monthly_adj, k = 1)

# View the first few rows of returns to verify calculations
head(spy_returns)
head(mRNA_returns)
head(ual_returns)

# Rename returns columns for clarity
names(spy_returns) <- "spy_ret"
names(mRNA_returns) <- "mRNA_ret"
names(ual_returns) <- "ual_ret"

# Compute the mean and standard deviation for each ticker's monthly returns
mean_spy <- mean(spy_returns, na.rm = TRUE)
sd_spy <- sd(spy_returns, na.rm = TRUE)

mean_mRNA <- mean(mRNA_returns, na.rm = TRUE)
sd_mRNA <- sd(mRNA_returns, na.rm = TRUE)

mean_ual <- mean(ual_returns, na.rm = TRUE)
sd_ual <- sd(ual_returns, na.rm = TRUE)

# Print the mean and standard deviation for each ticker
cat("SPY - Mean Monthly Return:", mean_spy, " | SD:", sd_spy, "\n")
cat("MRNA - Mean Monthly Return:", mean_mRNA, " | SD:", sd_mRNA, "\n")
cat("UAL - Mean Monthly Return:", mean_ual, " | SD:", sd_ual, "\n")

# Calculate VaR(0.01) under a normal model using qnorm
VaR_01_spy <- qnorm(0.01, mean = mean_spy, sd = sd_spy)
VaR_01_mRNA <- qnorm(0.01, mean = mean_mRNA, sd = sd_mRNA)
VaR_01_ual <- qnorm(0.01, mean = mean_ual, sd = sd_ual)

# Print the VaR(0.01) for each ticker
cat("SPY VaR(0.01):", VaR_01_spy, "\n")
cat("MRNA VaR(0.01):", VaR_01_mRNA, "\n")
cat("UAL VaR(0.01):", VaR_01_ual, "\n")

############Part D####################

# Convert the data to data frames 
SPY_df <- as.data.frame(SPY)
MRNA_df <- as.data.frame(MRNA)
UAL_df <- as.data.frame(UAL)

# Extract the adjusted close prices
spy_first_price <- SPY_df$SPY.Adjusted[1]
mRNA_first_price <- MRNA_df$MRNA.Adjusted[1]
ual_first_price <- UAL_df$UAL.Adjusted[1]

# Calculate GrossRet for each stock (adjusted close on day t / adjusted close on the first day)
spy_gross_ret <- SPY_df$SPY.Adjusted / spy_first_price
mRNA_gross_ret <- MRNA_df$MRNA.Adjusted / mRNA_first_price
ual_gross_ret <- UAL_df$UAL.Adjusted / ual_first_price

# Extract the dates for plotting
dates <- as.Date(row.names(SPY_df))  # Convert the row names (dates) to Date format

# Ensure dates length matches GrossRet values
dates <- dates[1:length(spy_gross_ret)]

# Create the time series plot for all three stocks on the same axis
plot(dates, spy_gross_ret, type = "l", col = "black", lwd = 2, 
     main = "Gross Returns since Jan 1, 2020", 
     xlab = "Date", ylab = "Gross Returns (since Jan 1, 2020)", 
     las = 2, ylim = c(0, 27)) 

# Add MRNA and UAL to the plot with specified colors
lines(dates, mRNA_gross_ret, col = "firebrick", lwd = 2)
lines(dates, ual_gross_ret, col = "blue", lwd = 2)

# Add a horizontal line at GrossRet = 1
abline(h = 1, col = "darkgray", lty = 2)






