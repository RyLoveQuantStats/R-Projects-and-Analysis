# Author: Ryan Loveless
# ----------------------------- #

load("indexes_and_spiders.RData")

library(quantmod)
library(ggplot2)

####### Question 1

# Single regressions
mylm <- lm(ret_spy ~ ret_sp500, data = dat)
summary(mylm)

mylm2 <- lm(ret_voo ~ ret_sp500, data = dat)
summary(mylm2)

mylm3 <- lm(ret_spy ~ ret_voo, data = dat)
summary(mylm3)

# Multiple regression
mylm4 <- lm(ret_spy ~ ret_sp500 + ret_voo, data = dat)
summary(mylm4)

# Scatter plots
ggplot(dat, aes(x = ret_sp500, y = ret_spy)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("SPY vs S&P500 Returns") +
  theme_minimal()

ggplot(dat, aes(x = ret_sp500, y = ret_voo)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("VOO vs S&P500 Returns") +
  theme_minimal()

ggplot(dat, aes(x = ret_voo, y = ret_spy)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "brown") +
  ggtitle("SPY vs VOO Returns") +
  theme_minimal()

cat("Tracking error analysis:\n")
cat("SPY regression R-squared:", summary(mylm)$r.squared, "\n")
cat("VOO regression R-squared:", summary(mylm2)$r.squared, "\n")

####### Question 2

# Pull last 5 years of data
start_date <- Sys.Date() - 5 * 365
symbols <- c("SPY", "VOO", "^GSPC")
getSymbols(symbols, from = start_date)

# Calculate daily returns
spy_returns <- dailyReturn(Cl(SPY), type = "log")
voo_returns <- dailyReturn(Cl(VOO), type = "log")
sp500_returns <- dailyReturn(Cl(GSPC), type = "log")

# Recreate quant_data ensuring proper column alignment
quant_data <- data.frame(
  date = as.Date(index(spy_returns)),
  ret_spy = as.numeric(spy_returns),
  ret_voo = as.numeric(voo_returns),
  ret_sp500 = as.numeric(sp500_returns)
)

# Single regressions
quant_mylm <- lm(ret_spy ~ ret_sp500, data = quant_data)
summary(quant_mylm)

quant_mylm2 <- lm(ret_voo ~ ret_sp500, data = quant_data)
summary(quant_mylm2)

quant_mylm3 <- lm(ret_spy ~ ret_voo, data = quant_data)
summary(quant_mylm3)

# Multiple regression
quant_mylm4 <- lm(ret_spy ~ ret_sp500 + ret_voo, data = quant_data)
summary(quant_mylm4)

# Scatter plots
ggplot(quant_data, aes(x = ret_sp500, y = ret_spy)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("SPY vs S&P500 Returns (Quantmod)") +
  theme_minimal()

ggplot(quant_data, aes(x = ret_sp500, y = ret_voo)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "orange") +
  ggtitle("VOO vs S&P500 Returns (Quantmod)") +
  theme_minimal()

ggplot(quant_data, aes(x = ret_voo, y = ret_spy)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "brown") +
  ggtitle("SPY vs VOO Returns (Quantmod)") +
  theme_minimal()

# Monthly returns
spy_monthly <- monthlyReturn(Cl(SPY))
voo_monthly <- monthlyReturn(Cl(VOO))
sp500_monthly <- monthlyReturn(Cl(GSPC))

# Combine into monthly data frame
monthly_data <- data.frame(
  date = index(spy_monthly),
  ret_spy = as.numeric(spy_monthly),
  ret_voo = as.numeric(voo_monthly),
  ret_sp500 = as.numeric(sp500_monthly)
)

# Perform regressions
monthly_mylm <- lm(ret_spy ~ ret_sp500, data = monthly_data)
summary(monthly_mylm)

cat("Goodness of fit (R-squared) for daily regression of SPY on S&P500:", summary(quant_mylm)$r.squared, "\n")
cat("Goodness of fit (R-squared) for monthly regression of SPY on S&P500:", summary(monthly_mylm)$r.squared, "\n")

####### Question 3

# Function for the birthday simulation
birthday_simulation <- function(n_students, days_in_year, n_trials = 10000) {
  mean(replicate(n_trials, {
    birthdays <- sample(1:days_in_year, n_students, replace = TRUE)
    any(duplicated(birthdays))
  }))
}

# Probability for Mars (687 days)
mars_prob <- birthday_simulation(50, 687)
cat("Probability of at least one shared birthday on Mars:", mars_prob, "\n")

# Probability for Venus (225 days)
venus_prob <- birthday_simulation(50, 225)
cat("Probability of at least one shared birthday on Venus:", venus_prob, "\n")


######## Question 4 
# Get PRNHX, VMGIX, and oil data
getSymbols("PRNHX", from = start_date)
getSymbols("VMGIX", from = start_date)
oil_prices <- getSymbols("DCOILWTICO", src = "FRED", auto.assign = FALSE)

# Calculate returns
prnhx_returns <- monthlyReturn(Cl(PRNHX))
vmgix_returns <- monthlyReturn(Cl(VMGIX))
oil_returns <- monthlyReturn(oil_prices)

# Merge data into a single xts object, aligning by date
merged_xts <- merge(prnhx_returns, vmgix_returns, oil_returns, all = FALSE)

# Convert to a data frame
two_factor_data <- data.frame(
  date = index(merged_xts),
  ret_prnhx = as.numeric(merged_xts[, 1]),
  ret_vmgix = as.numeric(merged_xts[, 2]),
  ret_oil = as.numeric(merged_xts[, 3])
)

# Two-factor regression
two_factor_model <- lm(ret_prnhx ~ ret_vmgix + ret_oil, data = two_factor_data)
summary(two_factor_model)

cat("Two-factor regression interpretation:\n")
cat("Significant predictor: ret_vmgix with coefficient", summary(two_factor_model)$coefficients["ret_vmgix", "Estimate"], "\n")


