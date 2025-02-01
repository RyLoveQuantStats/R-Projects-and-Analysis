## Name: Ryan Loveless         ##                 
## ----------------------      ##

# Load necessary package
library(quantmod)

# Manually set the start date 61 months prior to October 2024
start_date <- as.Date("2019-07-01")  # 61 months back from October 2024

# Download data for SPY, TSLA, and GE from Yahoo Finance with monthly frequency
getSymbols(c("SPY", "TSLA", "GE"), src = "yahoo", from = start_date, periodicity = "monthly")

# Convert each to data frames and calculate returns
spy <- data.frame(Date = index(SPY), SPY[,6])
tsla <- data.frame(Date = index(TSLA), TSLA[,6])
ge <- data.frame(Date = index(GE), GE[,6])

# Calculate monthly returns
spy$sp500_ret <- c(NA, diff(log(spy$SPY.Adjusted)))
tsla$tsla_ret <- c(NA, diff(log(tsla$TSLA.Adjusted)))
ge$ge_ret <- c(NA, diff(log(ge$GE.Adjusted)))

# Remove the first row with NA to keep rows consistent in all data frames
spy <- spy[-1, ]
tsla <- tsla[-1, ]
ge <- ge[-1, ]

# Proceed with renaming columns and merging data
names(spy) <- c("Date", "sp500_price", "sp500_ret")
names(tsla) <- c("Date", "tsla_price", "tsla_ret")
names(ge) <- c("Date", "ge_price", "ge_ret")

# Merge data by Date
merged_dat <- merge(merge(spy, tsla, by = "Date"), ge, by = "Date")



############# Part 2: Data Analysis #############

## A: Summary Statistics and Histograms

# Calculate mean and standard deviation for each returns column
summary_stats <- data.frame(
  Mean = sapply(merged_dat[,c("sp500_ret", "tsla_ret", "ge_ret")], mean, na.rm = TRUE),
  SD = sapply(merged_dat[,c("sp500_ret", "tsla_ret", "ge_ret")], sd, na.rm = TRUE)
)
print(summary_stats)

# Plot histograms for each returns column
hist(merged_dat$sp500_ret, main="SP500 Returns", xlab="Returns", breaks=15)
hist(merged_dat$tsla_ret, main="Tesla Returns", xlab="Returns", breaks=15)
hist(merged_dat$ge_ret, main="GE Returns", xlab="Returns", breaks=15)


## B: Scatter Plots and Correlations

# Plot TSLA vs SP500 returns
plot(merged_dat$sp500_ret, merged_dat$tsla_ret, 
     xlab="SP500 Returns", ylab="Tesla Returns", main="SP500 vs TSLA Returns")

# Plot GE vs SP500 returns
plot(merged_dat$sp500_ret, merged_dat$ge_ret, 
     xlab="SP500 Returns", ylab="GE Returns", main="SP500 vs GE Returns")

# Calculate correlations
cor_spy_tsla <- cor(merged_dat$sp500_ret, merged_dat$tsla_ret, use="complete.obs")
cor_spy_ge <- cor(merged_dat$sp500_ret, merged_dat$ge_ret, use="complete.obs")

# Print correlations
cat("Correlation between SP500 and TSLA:", cor_spy_tsla, "\n")
cat("Correlation between SP500 and GE:", cor_spy_ge, "\n")


## C: By-hand Regression Calculations

# Calculate slope (b) and intercept (a) manually for TSLA and GE regressions
b_tsla <- cov(merged_dat$sp500_ret, merged_dat$tsla_ret) / var(merged_dat$sp500_ret)
a_tsla <- mean(merged_dat$tsla_ret) - b_tsla * mean(merged_dat$sp500_ret)

b_ge <- cov(merged_dat$sp500_ret, merged_dat$ge_ret) / var(merged_dat$sp500_ret)
a_ge <- mean(merged_dat$ge_ret) - b_ge * mean(merged_dat$sp500_ret)

# Print the calculated slope and intercept
cat("TSLA Slope:", b_tsla, "Intercept:", a_tsla, "\n")
cat("GE Slope:", b_ge, "Intercept:", a_ge, "\n")


## D: Verify with lm function

# Run regression using lm for TSLA and GE
tsla_lm <- lm(tsla_ret ~ sp500_ret, data = merged_dat)
ge_lm <- lm(ge_ret ~ sp500_ret, data = merged_dat)

# Display regression summaries
summary(tsla_lm)
summary(ge_lm)


## E: Overlay Regression Line on Scatter Plot

# Add regression line to TSLA scatter plot
abline(tsla_lm, col="blue", lty=2)

# Add regression line to GE scatter plot
abline(ge_lm, col="blue", lty=2)


## F: Plot Mean Lines and Interpret

# Add mean lines on each plot
abline(h=mean(merged_dat$tsla_ret), col="red", lty=2)
abline(h=mean(merged_dat$ge_ret), col="red", lty=2)
abline(v=mean(merged_dat$sp500_ret), col="green", lty=2)


## G: R-squared and Residuals Analysis

# Calculate and interpret R-squared for each regression
r_squared_tsla <- summary(tsla_lm)$r.squared
r_squared_ge <- summary(ge_lm)$r.squared
cat("R-squared for TSLA:", r_squared_tsla, "\n")
cat("R-squared for GE:", r_squared_ge, "\n")

# Extract residuals for both TSLA and GE regressions
merged_dat$resids_tsla <- residuals(tsla_lm)
merged_dat$resids_ge <- residuals(ge_lm)

# Average of residuals
cat("Average of TSLA residuals:", mean(merged_dat$resids_tsla), "\n")
cat("Average of GE residuals:", mean(merged_dat$resids_ge), "\n")

# Correlation of residuals with SP500 returns
cor_resids_tsla <- cor(merged_dat$resids_tsla, merged_dat$sp500_ret)
cor_resids_ge <- cor(merged_dat$resids_ge, merged_dat$sp500_ret)
cat("Correlation of TSLA residuals with SP500 returns:", cor_resids_tsla, "\n")
cat("Correlation of GE residuals with SP500 returns:", cor_resids_ge, "\n")


############# Part 3: 36-Month Analysis #############

# Subset the data to the last 36 months
merged_dat_36 <- tail(merged_dat, 36)

## A: Summary Statistics and Histograms for 36 Months

# Calculate mean and standard deviation for each returns column
summary_stats_36 <- data.frame(
  Mean = sapply(merged_dat_36[,c("sp500_ret", "tsla_ret", "ge_ret")], mean, na.rm = TRUE),
  SD = sapply(merged_dat_36[,c("sp500_ret", "tsla_ret", "ge_ret")], sd, na.rm = TRUE)
)
print(summary_stats_36)

# Plot histograms for each returns column
hist(merged_dat_36$sp500_ret, main="SP500 Returns (36 Months)", xlab="Returns", breaks=15)
hist(merged_dat_36$tsla_ret, main="Tesla Returns (36 Months)", xlab="Returns", breaks=15)
hist(merged_dat_36$ge_ret, main="GE Returns (36 Months)", xlab="Returns", breaks=15)


## B: Scatter Plots and Correlations for 36 Months

# Plot TSLA vs SP500 returns
plot(merged_dat_36$sp500_ret, merged_dat_36$tsla_ret, 
     xlab="SP500 Returns", ylab="Tesla Returns", main="SP500 vs TSLA Returns (36 Months)")

# Plot GE vs SP500 returns
plot(merged_dat_36$sp500_ret, merged_dat_36$ge_ret, 
     xlab="SP500 Returns", ylab="GE Returns", main="SP500 vs GE Returns (36 Months)")

# Calculate correlations
cor_spy_tsla_36 <- cor(merged_dat_36$sp500_ret, merged_dat_36$tsla_ret, use="complete.obs")
cor_spy_ge_36 <- cor(merged_dat_36$sp500_ret, merged_dat_36$ge_ret, use="complete.obs")

# Print correlations
cat("Correlation between SP500 and TSLA (36 Months):", cor_spy_tsla_36, "\n")
cat("Correlation between SP500 and GE (36 Months):", cor_spy_ge_36, "\n")


## C: By-hand Regression Calculations for 36 Months

# Calculate slope (b) and intercept (a) manually for TSLA and GE regressions
b_tsla_36 <- cov(merged_dat_36$sp500_ret, merged_dat_36$tsla_ret) / var(merged_dat_36$sp500_ret)
a_tsla_36 <- mean(merged_dat_36$tsla_ret) - b_tsla_36 * mean(merged_dat_36$sp500_ret)

b_ge_36 <- cov(merged_dat_36$sp500_ret, merged_dat_36$ge_ret) / var(merged_dat_36$sp500_ret)
a_ge_36 <- mean(merged_dat_36$ge_ret) - b_ge_36 * mean(merged_dat_36$sp500_ret)

# Print the calculated slope and intercept
cat("TSLA Slope (36 Months):", b_tsla_36, "Intercept:", a_tsla_36, "\n")
cat("GE Slope (36 Months):", b_ge_36, "Intercept:", a_ge_36, "\n")


## D: Verify with lm function for 36 Months

# Run regression using lm for TSLA and GE
tsla_lm_36 <- lm(tsla_ret ~ sp500_ret, data = merged_dat_36)
ge_lm_36 <- lm(ge_ret ~ sp500_ret, data = merged_dat_36)

# Display regression summaries
summary(tsla_lm_36)
summary(ge_lm_36)


## E: Overlay Regression Line on Scatter Plot for 36 Months

# Add regression line to TSLA scatter plot
abline(tsla_lm_36, col="blue", lty=2)

# Add regression line to GE scatter plot
abline(ge_lm_36, col="blue", lty=2)


## F: Plot Mean Lines and Interpret for 36 Months

# Add mean lines on each plot
abline(h=mean(merged_dat_36$tsla_ret), col="red", lty=2)
abline(h=mean(merged_dat_36$ge_ret), col="red", lty=2)
abline(v=mean(merged_dat_36$sp500_ret), col="green", lty=2)


## G: R-squared and Residuals Analysis for 36 Months

# Calculate and interpret R-squared for each regression
r_squared_tsla_36 <- summary(tsla_lm_36)$r.squared
r_squared_ge_36 <- summary(ge_lm_36)$r.squared
cat("R-squared for TSLA (36 Months):", r_squared_tsla_36, "\n")
cat("R-squared for GE (36 Months):", r_squared_ge_36, "\n")

# Extract residuals for both TSLA and GE regressions
merged_dat_36$resids_tsla <- residuals(tsla_lm_36)
merged_dat_36$resids_ge <- residuals(ge_lm_36)

# Average of residuals
cat("Average of TSLA residuals (36 Months):", mean(merged_dat_36$resids_tsla), "\n")
cat("Average of GE residuals (36 Months):", mean(merged_dat_36$resids_ge), "\n")

# Correlation of residuals with SP500 returns
cor_resids_tsla_36 <- cor(merged_dat_36$resids_tsla, merged_dat_36$sp500_ret)
cor_resids_ge_36 <- cor(merged_dat_36$resids_ge, merged_dat_36$sp500_ret)
cat("Correlation of TSLA residuals with SP500 returns (36 Months):", cor_resids_tsla_36, "\n")
cat("Correlation of GE residuals with SP500 returns (36 Months):", cor_resids_ge_36, "\n")


# Comparison of Calculated Betas to Yahoo Finance Reported Betas
# --------------------------------------------------------------
# TSLA:
# - Yahoo Finance Beta (5-Year Monthly): 2.30
# - Calculated Beta (61 Months): 2.05 (close to Yahoo's long-term value)
# - Calculated Beta (36 Months): 1.55 (lower than Yahoo’s, indicating recent reduced volatility)
# Interpretation: TSLA’s recent beta (1.55) suggests it has been less volatile relative to the S&P 500
# compared to its long-term beta (2.30), possibly due to stabilization or company-specific factors.

# GE:
# - Yahoo Finance Beta (5-Year Monthly): 1.18
# - Calculated Beta (61 Months): 1.24 (similar to Yahoo's long-term value)
# - Calculated Beta (36 Months): 1.49 (higher, suggesting increased sensitivity to the market recently)
# Interpretation: GE’s recent beta increase (1.49) implies heightened market sensitivity, likely due
# to macroeconomic factors or company-specific changes that align it more closely with broader market trends.

# R-Squared Insights:
# - TSLA: R-squared decreased from 0.29 (61 months) to 0.20 (36 months), suggesting weaker recent 
# correlation with the market.
# - GE: R-squared increased from 0.33 to 0.55, indicating stronger recent correlation with the market.



