## Name: Ryan Loveless         ##                 
## ----------------------      ##

# Load necessary package
library(quantmod)

# Set date range for analysis
start_date <- as.Date("2019-06-30")
end_date <- as.Date("2022-10-31")

# Download daily data for PRNHX and VMGIX from Yahoo Finance
getSymbols(c("PRNHX", "VMGIX"), src = "yahoo", from = start_date, to = end_date, periodicity = "daily")

# Convert each to data frames and calculate daily returns
prnhx <- data.frame(Date = index(PRNHX), PRNHX[,6])
vmgix <- data.frame(Date = index(VMGIX), VMGIX[,6])

# Calculate daily returns using log differences
prnhx$ret_a <- c(NA, diff(log(prnhx$PRNHX.Adjusted)))
vmgix$ret_p <- c(NA, diff(log(vmgix$VMGIX.Adjusted)))

# Remove the first row with NA to keep rows consistent
prnhx <- prnhx[-1, ]
vmgix <- vmgix[-1, ]

# Rename columns and merge data by Date
names(prnhx) <- c("Date", "prnhx_price", "ret_a")
names(vmgix) <- c("Date", "vmgix_price", "ret_p")
dat <- merge(prnhx, vmgix, by = "Date")

############# Part I: Summaries and Plots #############

## A: Summary Statistics and Histograms

# Calculate mean and standard deviation for returns
summary_stats <- data.frame(
  Mean = sapply(dat[,c("ret_a", "ret_p")], mean, na.rm = TRUE),
  SD = sapply(dat[,c("ret_a", "ret_p")], sd, na.rm = TRUE)
)
print(summary_stats)  # Print to interpret average performance and volatility

# Provide numerical summaries using the summary() command
summary(dat$ret_a)
summary(dat$ret_p)

# Plot histograms to visualize returns distributions
hist(dat$ret_a, main="Histogram of PRNHX Returns", xlab="Returns", breaks=15, col="blue")
hist(dat$ret_p, main="Histogram of VMGIX Returns", xlab="Returns", breaks=15, col="green")

## B: Scatter Plot and Correlation

# Scatter plot of PRNHX vs VMGIX returns
plot(dat$ret_p, dat$ret_a, xlab="VMGIX Returns", ylab="PRNHX Returns", main="PRNHX vs VMGIX Returns", pch=20, col="blue")

# Add a regression line to the plot
abline(lm(ret_a ~ ret_p, data=dat), col="red", lwd=2)

# Describe the relationship in a comment
# The scatter plot shows a strong positive linear relationship between PRNHX returns and VMGIX returns.

# Calculate and interpret correlation
cor_ret <- cor(dat$ret_a, dat$ret_p, use="complete.obs")
cat("Correlation between PRNHX and VMGIX:", cor_ret, "\n")

# Interpretation of correlation
# The correlation coefficient is close to 0.95, indicating a strong positive linear relationship.
# This means that as VMGIX returns increase, PRNHX returns tend to increase as well.

############# Part II: Regression Analysis and Inference #############

## C: Regression Analysis with lm()

# Run linear regression and display summary output
char_lm <- lm(ret_a ~ ret_p, data = dat)
summary(char_lm)  # Use this output to interpret coefficients, R-squared, etc.

# Extract R-squared, slope, and intercept for interpretation
r_squared <- summary(char_lm)$r.squared
slope <- summary(char_lm)$coefficients[2, 1]
intercept <- summary(char_lm)$coefficients[1, 1]

cat("R-squared:", r_squared, "\n")
cat("Slope (Beta):", slope, "\n")
cat("Intercept (Alpha):", intercept, "\n")

# Interpretation of R-squared
# The R-squared value of approximately 0.91 indicates that about 91% of the variability in PRNHX returns
# is explained by VMGIX returns.

# Relation to correlation
# R-squared is the square of the correlation coefficient. Since the correlation is about 0.95, R-squared is about 0.95^2 ≈ 0.9025.

## D: Interpretation of Intercept

# The estimated intercept represents the expected return of PRNHX when the return of VMGIX is zero.
# In this context, it can be interpreted as the 'alpha' or the excess return of the active fund over the benchmark when the benchmark return is zero.

# Since the intercept is approximately zero (-1.66e-05), it suggests that when VMGIX returns are zero, PRNHX returns are also approximately zero.

# Does this make contextual sense?
# In finance, the intercept (alpha) represents the fund's performance independent of the benchmark.
# A zero alpha suggests that the fund does not generate excess returns over the benchmark when the benchmark's return is zero.

## E: Interpretation of Slope

# The estimated slope (beta) represents the sensitivity of PRNHX returns to changes in VMGIX returns.
# A beta of approximately 0.98 indicates that for every 1% change in VMGIX returns, PRNHX returns change by approximately 0.98%.

# In the context of the problem, this suggests that PRNHX closely tracks the benchmark VMGIX, but with slightly less sensitivity.

## F: Standard Errors for Inference

# Extract standard errors for slope and intercept
intercept_se <- summary(char_lm)$coefficients[1, 2]
slope_se <- summary(char_lm)$coefficients[2, 2]

# Print standard errors for interpretation
cat("Standard Error for Intercept:", intercept_se, "\n")
cat("Standard Error for Slope:", slope_se, "\n")

# Interpretation:
# The standard error of the intercept estimates the variability of the intercept estimate across different samples.
# A small standard error indicates that the estimate of the intercept is precise.

# The standard error of the slope measures the variability of the slope estimate across different samples.
# A small standard error indicates that the estimate of the slope is precise.

# These standard errors can be used to conduct hypothesis tests and construct confidence intervals for the coefficients.

## G: Hypothesis Testing for Coefficients

# For the intercept:
# Null Hypothesis (H0): The true intercept (alpha) is equal to zero.
# Alternative Hypothesis (H1): The true intercept is not equal to zero.

# The t-test checks whether the intercept is significantly different from zero.

# Does it make contextual sense to test this?
# In finance, testing whether alpha is zero is meaningful because a non-zero alpha indicates that the fund generates excess returns independent of the benchmark.

# From the regression output, the p-value for the intercept is approximately 0.928.
# Since the p-value is much greater than 0.05, we fail to reject the null hypothesis.
# Conclusion: There is no statistical evidence that the intercept (alpha) is different from zero.

# For the slope:
# Null Hypothesis (H0): The true slope (beta) is equal to zero.
# Alternative Hypothesis (H1): The true slope is not equal to zero.

# The t-test checks whether the slope is significantly different from zero.

# In this context, testing whether beta is zero helps us understand if there is any relationship between PRNHX returns and VMGIX returns.

# From the regression output, the p-value for the slope is less than 2e-16, which is essentially zero.
# Since the p-value is less than 0.05, we reject the null hypothesis.
# Conclusion: There is strong statistical evidence that the slope (beta) is different from zero.

# Contextual interpretation:
# The returns of PRNHX are significantly related to the returns of VMGIX.

## H: Hypothesis Testing: Is the true slope equal to 1?

# Null Hypothesis (H0): The true slope (beta) is equal to 1.
# Alternative Hypothesis (H1): The true slope is not equal to 1.

# We have:
# Estimated slope (beta_hat) = slope
# Standard error of slope (se_beta) = slope_se

# Calculate t-statistic
t_stat <- (slope - 1) / slope_se
cat("t-statistic for testing beta = 1:", t_stat, "\n")

# Degrees of freedom
df <- nrow(dat) - 2

# Calculate two-sided p-value
p_value <- 2 * pt(-abs(t_stat), df = df)
cat("p-value for testing beta = 1:", p_value, "\n")

# Conclusion:
# With a p-value of approximately 0.062, which is greater than 0.05, we fail to reject the null hypothesis at the 5% significance level.
# Therefore, there is not enough statistical evidence to conclude that the true beta is different from 1.

# Interpretation:
# The beta of PRNHX is not significantly different from 1, suggesting that its sensitivity to the benchmark is similar to the benchmark itself.

############# Part III: Rescaling #############

## I: Rescaled Regression with scale()

# Run regression with scaled predictor and analyze
mylm2 <- lm(ret_a ~ scale(ret_p), data = dat)
summary(mylm2)  # Compare this output with the unscaled regression

# Interpretation:

# How does the estimate of the intercept change?
# In this regression, the intercept represents the expected return of PRNHX when the standardized return of VMGIX is zero.
# Since the standardized return has a mean of zero, the intercept here represents the mean of ret_a.

cat("Intercept (mylm2):", summary(mylm2)$coefficients[1,1], "\n")
cat("Mean of ret_a:", mean(dat$ret_a, na.rm=TRUE), "\n")

# The intercept is equal to the mean of ret_a, as expected.

# How does the estimate of the slope change?
# The slope now represents the change in ret_a for a one standard deviation change in ret_p.

# Do you necessarily want to apply the scale() function in this case?
# Scaling the predictor can be useful for interpretation when variables are on different scales, but in this case, both variables are returns and are on the same scale.

# Compare test statistics, p-values, R-squared
# The R-squared and residual standard error remain the same.
# The t-statistics and p-values for the slope are the same, since scaling the predictor does not affect the significance.

# What changes?
# The slope and intercept estimates change due to scaling.
# What does not change?
# The R-squared, residual standard error, and overall model fit remain the same.

# Why?
# Scaling the predictor variable does not change the relationship between the variables, only the units in which the slope is expressed.

## J: Rescaled Regression using 100% Returns

# Run regression by scaling returns to percentages
mylm3 <- lm(I(100 * ret_a) ~ I(100 * ret_p), data = dat)
summary(mylm3)  # Compare interpretation of coefficients to prior results

# Interpretation:

# How does the estimate of the intercept change?
# The intercept and slope are scaled by a factor of 100, reflecting the change from decimal returns to percentage returns.

# Intercept comparison
cat("Intercept (mylm3):", summary(mylm3)$coefficients[1,1], "\n")
cat("Intercept (char_lm) * 100:", intercept * 100, "\n")

# Slope comparison
cat("Slope (mylm3):", summary(mylm3)$coefficients[2,1], "\n")
cat("Slope (char_lm):", slope, "\n")

# The intercept is scaled by 100, the slope remains the same.

# Do you necessarily want to apply the scale() function in this case?
# Scaling by 100 changes the units but does not affect the relationship between the variables.

# Compare test statistics, p-values, R-squared
# The test statistics and p-values remain the same, as they are not affected by scaling of variables.

# What changes?
# The estimates of coefficients change due to scaling.

# What does not change?
# The statistical significance, R-squared, residual standard error.

# Why?
# Because scaling the variables by a constant factor does not affect the statistical properties of the regression.

############# Part IV: Adding Midcap ETF (XMMO) for Comparison #############

# Download XMMO data and calculate returns
getSymbols("XMMO", src = "yahoo", from = start_date, to = end_date, periodicity = "daily")
xmmo <- data.frame(Date = index(XMMO), XMMO[,6])
xmmo$ret_x <- c(NA, diff(log(xmmo$XMMO.Adjusted)))
xmmo <- xmmo[-1, ]

# Rename columns and merge with the main dataset
names(xmmo) <- c("Date", "xmmo_price", "ret_x")
dat <- merge(dat, xmmo, by = "Date")

## K: Regression Analysis for XMMO vs VMGIX

# Run linear regression for XMMO returns against VMGIX
xmmo_lm <- lm(ret_x ~ ret_p, data = dat)
summary(xmmo_lm)  # Use this to interpret alpha, beta, and R-squared for the ETF

# Extract coefficients and standard errors
xmmo_intercept <- summary(xmmo_lm)$coefficients[1,1]
xmmo_intercept_se <- summary(xmmo_lm)$coefficients[1,2]
xmmo_slope <- summary(xmmo_lm)$coefficients[2,1]
xmmo_slope_se <- summary(xmmo_lm)$coefficients[2,2]
xmmo_r_squared <- summary(xmmo_lm)$r.squared

# Hypothesis testing for intercept (alpha)

# Null Hypothesis (H0): Alpha = 0
# Alternative Hypothesis (H1): Alpha ≠ 0

# t-statistic for intercept
t_alpha <- xmmo_intercept / xmmo_intercept_se
p_alpha <- 2 * pt(-abs(t_alpha), df=nrow(dat)-2)
cat("XMMO Alpha t-statistic:", t_alpha, "\n")
cat("XMMO Alpha p-value:", p_alpha, "\n")

# Conclusion: Since p-value > 0.05, we fail to reject the null hypothesis. Alpha is not significantly different from zero.

# Hypothesis testing for slope (beta)

# Null Hypothesis (H0): Beta = 1
# Alternative Hypothesis (H1): Beta ≠ 1

# t-statistic for beta
t_beta <- (xmmo_slope - 1) / xmmo_slope_se
p_beta <- 2 * pt(-abs(t_beta), df=nrow(dat)-2)
cat("XMMO Beta t-statistic:", t_beta, "\n")
cat("XMMO Beta p-value:", p_beta, "\n")

# Conclusion: Since p-value is less than 0.05, we reject the null hypothesis. Beta is significantly different from 1.

# Compare with PRNHX

# For PRNHX, we failed to reject the null hypothesis that beta = 1.

# Interpretation:

# XMMO has an estimated beta of approximately 0.916, which is significantly different from 1.
# This suggests that XMMO is less sensitive to the benchmark than PRNHX.

# The alpha (intercept) for XMMO is not significantly different from zero, similar to PRNHX.

# R-squared for XMMO is lower (approximately 0.828) compared to PRNHX (approximately 0.911), indicating that the benchmark explains less of the variability in XMMO's returns compared to PRNHX.

# Overall, PRNHX tracks the benchmark more closely than XMMO.

############# Part V: Analysis Summary #############

# Summarize results for case study conclusions
cat("\n--- Case Study Summary ---\n")
cat("PRNHX Alpha:", intercept, "\n")
cat("PRNHX Beta:", slope, "\n")
cat("PRNHX R-squared:", r_squared, "\n")
cat("XMMO Alpha:", xmmo_intercept, "\n")
cat("XMMO Beta:", xmmo_slope, "\n")
cat("XMMO R-squared:", xmmo_r_squared, "\n")

cat("\nInterpretation:\n")
cat("1. PRNHX has a high correlation with VMGIX (benchmark), with beta near 1, showing close tracking.\n")
cat("2. The beta of PRNHX is not significantly different from 1.\n")
cat("3. XMMO has a lower beta than PRNHX, and its beta is significantly different from 1.\n")
cat("4. Neither PRNHX nor XMMO has statistically significant alpha (excess return).\n")
cat("5. R-squared for PRNHX is higher than for XMMO, indicating that PRNHX's returns are more closely explained by the benchmark.\n")
cat("6. Overall, PRNHX tracks the benchmark more closely than XMMO.\n")
