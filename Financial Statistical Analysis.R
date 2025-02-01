## Name: Ryan Loveless               ##
## ----------------------------------##

############# Question 1
# Compute test_stat based on Jason's p-value
test_stat <- qt(0.06, 74)

# Compute Sandra's p-value for a two-sided test
p_value <- 2 * pt(test_stat, 74)

# Round the p-value to two decimal places
p_value_rounded <- round(p_value, 2)

# Print the p-value
print(p_value_rounded)


############### Question 2
beta <- 0.2  # Probability of making a Type II error

# Calculate power
power <- 1 - beta

# Print the power rounded to three decimal places
power_rounded <- round(power, 3)
cat("The power of the test is:", power_rounded)


############# Question 3 
# Define S as a 5-sided die (1 to 5)
S <- 1:5

# Simulate rolling the 5-sided die 500 times
rolls <- sample(S, size = 500, replace = TRUE)
print(rolls)


################## Question 4
confidence_level <- 0.98
df <- 65 - 1  

# Calculate the t-value (critical value)
t_value <- qt(1 - (1 - confidence_level) / 2, df)

# Round the t-value to 3 digits
t_value_rounded <- round(t_value, 3)

# Output the result to the console
cat("The critical t-value for a 98% confidence interval is:", t_value_rounded, "\n")

################## Question 5
num_tests <- 60
alpha <- 0.10  # 10% significance level

# Step 2: Calculate the expected number of rejections
expected_rejections <- num_tests * alpha

# Step 3: Round to three decimal places
expected_rejections_rounded <- round(expected_rejections, 3)

# Step 4: Output the result to the console
cat("The expected number of rejections is:", expected_rejections_rounded, "\n")

################# Question 6

original_se <- 6.5

# Step 2: Compute the new standard error (sample size increases by 4x)
new_se <- original_se / 2

# Step 3: Round the result to 2 decimal places
new_se_rounded <- round(new_se, 2)

# Step 4: Output the result to the console
cat("The new standard error is:", new_se_rounded, "\n")




## R scripting portion begins here ##

###################### Question 7.  


# Part A: Probability of at least one matching birthday on Mars
set.seed(1)

# Function that counts if there is at least one matching birthday in the class
num_bday_matches_mars <- function(class_size) {
  class_birthdays <- sample(1:687, size = class_size, replace = TRUE)  # Martian calendar has 687 days
  num_matches <- sum(table(class_birthdays) > 1)  # Check for any duplicates (returns TRUE if any match)
  return(num_matches > 0)  # Return TRUE if there is at least one match
}

# Simulate a bunch of randomly drawn classes of size N = 50 for Part A
a_bunch <- 10000  # Number of simulations
simz <- rep(NA, a_bunch)  # Initialize a vector to store the results

# Monte Carlo simulation
for (i in 1:a_bunch) {
  simz[i] <- num_bday_matches_mars(50)  # Simulate for a class size of 50 Martians
}

# Calculate the probability of at least one shared birthday
prob_shared_mars <- mean(simz)

# Output the result for Part A to 4 decimal places
cat("Part A: Probability of at least one matching Martian birthday:", round(prob_shared_mars, 4), "\n")


# Part B: Probability of at least three matching birthdays on Mars
set.seed(2)  # For reproducibility as instructed

# Function that counts if there are 3 or more shared birthdays in the class
num_three_shared_bday_mars <- function(class_size) {
  class_birthdays <- sample(1:687, size = class_size, replace = TRUE)  # Martian calendar has 687 days
  num_counts <- table(class_birthdays)  # Count the occurrences of each birthday
  return(any(num_counts >= 3))  # Return TRUE if any birthday is shared by 3 or more
}

# Simulate a bunch of randomly drawn classes of size N = 50 for Part B
simz_three_shared <- rep(NA, a_bunch)  # Initialize a vector to store the results

# Monte Carlo simulation
for (i in 1:a_bunch) {
  simz_three_shared[i] <- num_three_shared_bday_mars(50)  # Simulate for a class size of 50 Martians
}

# Calculate the probability of at least three shared birthdays
prob_three_shared_mars <- mean(simz_three_shared)

# Output the result for Part B to 4 decimal places
cat("Part B: Probability of at least three matching Martian birthdays:", round(prob_three_shared_mars, 4), "\n")

# For Earth, there are fewer days (365 days), so the probability of shared birthdays is higher.
# Therefore, the Martian probability (with 687 days) is expected to be lower than the Earthling probability.
# This is because, with more days in a year, there is less chance for students to share the same birthday.



############## Question 8.
library(quantmod)

# Daily data for DJI, S&P500, and TSLA from January 2, 2020 to today
getSymbols(c("^DJI", "^GSPC", "TSLA"), src = "yahoo", from = as.Date("2020-01-02"), to = Sys.Date())

# Convert to xts objects retaining only the "Adjusted Close" column
DJI_adj <- DJI[, "DJI.Adjusted"]
GSPC_adj <- GSPC[, "GSPC.Adjusted"]
TSLA_adj <- TSLA[, "TSLA.Adjusted"]

# Convert to monthly prices
DJI_monthly <- to.monthly(DJI_adj, indexAt = "lastof", OHLC = FALSE)
GSPC_monthly <- to.monthly(GSPC_adj, indexAt = "lastof", OHLC = FALSE)
TSLA_monthly <- to.monthly(TSLA_adj, indexAt = "lastof", OHLC = FALSE)

# Compute simple daily and monthly returns using Delt function
DJI_daily_ret <- Delt(DJI_adj, k = 1)
GSPC_daily_ret <- Delt(GSPC_adj, k = 1)
TSLA_daily_ret <- Delt(TSLA_adj, k = 1)

DJI_monthly_ret <- Delt(DJI_monthly, k = 1)
GSPC_monthly_ret <- Delt(GSPC_monthly, k = 1)
TSLA_monthly_ret <- Delt(TSLA_monthly, k = 1)

# Convert to data frames for further calculations
DJI_df <- as.data.frame(DJI_adj)
GSPC_df <- as.data.frame(GSPC_adj)
TSLA_df <- as.data.frame(TSLA_adj)

# Create GrossReturns for each data frame (starting with $1 on January 1, 2020)
DJI_df$GrossReturns <- DJI_df$DJI.Adjusted / DJI_df$DJI.Adjusted[1]
GSPC_df$GrossReturns <- GSPC_df$GSPC.Adjusted / GSPC_df$GSPC.Adjusted[1]
TSLA_df$GrossReturns <- TSLA_df$TSLA.Adjusted / TSLA_df$TSLA.Adjusted[1]

# Add date columns to the data frames
DJI_df$Date <- as.Date(row.names(DJI_df))
GSPC_df$Date <- as.Date(row.names(GSPC_df))
TSLA_df$Date <- as.Date(row.names(TSLA_df))

# Convert return series to numeric vectors
DJI_daily_ret_vec <- as.numeric(DJI_daily_ret$Delt.1.arithmetic)
GSPC_daily_ret_vec <- as.numeric(GSPC_daily_ret$Delt.1.arithmetic)
TSLA_daily_ret_vec <- as.numeric(TSLA_daily_ret$Delt.1.arithmetic)

# Histogram for DJI daily returns
hist(DJI_daily_ret_vec, breaks = 50, main = "Histogram of Daily DJI Returns", xlab = "DJI Daily Return", col = "firebrick", las = 1)

# Histogram for S&P500 daily returns
hist(GSPC_daily_ret_vec, breaks = 50, main = "Histogram of Daily S&P500 Returns", xlab = "S&P500 Daily Return", col = "firebrick", las = 1)

# Histogram for TSLA daily returns
hist(TSLA_daily_ret_vec, breaks = 50, main = "Histogram of Daily TSLA Returns", xlab = "TSLA Daily Return", col = "firebrick", las = 1)


# Null Hypothesis (H0): The mean of the S&P500 daily returns is zero (H0: μ = 0)
# Alternative Hypothesis (H1): The mean of the S&P500 daily returns is not zero (H1: μ ≠ 0)

## Hypothesis Testing 
t_test_sp500 <- t.test(GSPC_daily_ret_vec, mu = 0)
cat("Test Statistic for S&P500:", t_test_sp500$statistic, "\n")
cat("p-value for S&P500:", t_test_sp500$p.value, "\n")

# 10% significance level
if (t_test_sp500$p.value < 0.10) {
  cat("Reject the null hypothesis at the 10% significance level.\n")
} else {
  cat("Do not reject the null hypothesis at the 10% significance level.\n")
}

# Null Hypothesis (H0): The mean daily returns of S&P500 and DJIA are equal (H0: μ_S&P500 = μ_DJIA)
# Alternative Hypothesis (H1): The mean daily returns of S&P500 and DJIA are not equal (H1: μ_S&P500 ≠ μ_DJIA)

# Hypothesis Testing
t_test_sp500_vs_dji <- t.test(GSPC_daily_ret_vec, DJI_daily_ret)
cat("Test Statistic for S&P500 vs DJI:", t_test_sp500_vs_dji$statistic, "\n")
cat("p-value for S&P500 vs DJI:", t_test_sp500_vs_dji$p.value, "\n")

# 10% significance level
if (t_test_sp500_vs_dji$p.value < 0.10) {
  cat("Reject the null hypothesis at the 10% significance level (S&P500 vs DJI).\n")
} else {
  cat("Do not reject the null hypothesis at the 10% significance level (S&P500 vs DJI).\n")
}

# Plot GrossReturns for S&P500, DJI, and TSLA
plot(GSPC_df$Date, GSPC_df$GrossReturns, type = "l", col = "black", lwd = 2, 
     ylim = range(c(GSPC_df$GrossReturns, DJI_df$GrossReturns, TSLA_df$GrossReturns)),
     xlab = "Date", ylab = "Gross Returns", main = "Gross Returns for S&P500, DJI, and TSLA")
lines(DJI_df$Date, DJI_df$GrossReturns, col = "red", lty = 2, lwd = 2)  # DJI (Red, Dashed)
lines(TSLA_df$Date, TSLA_df$GrossReturns, col = "brown", lty = 3, lwd = 2)  # TSLA (Brown, Dotted)

# Add vertical lines for Feb 26, 2020, and Mar 27, 2020
abline(v = as.Date("2020-02-26"), col = "red", lty = 2)
abline(v = as.Date("2020-03-27"), col = "red", lty = 2)


