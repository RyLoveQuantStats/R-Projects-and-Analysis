# Author: Ryan Loveless  #
# ---------------------- #

# Load required libraries
library(quantmod)

# ---------------------- #
# Part 1: Data Preparation #
# ---------------------- #

# Step 1: Retrieve Data for SPY, SLB, and BICSX
tickers <- c("SPY", "SLB", "BICSX")
getSymbols(tickers, src = "yahoo", from = "2018-10-01", to = "2023-10-31")

# Step 2: Convert data to monthly format and calculate log returns
SPY_monthly <- monthlyReturn(Ad(SPY), type = "log")
SLB_monthly <- monthlyReturn(Ad(SLB), type = "log")
BICSX_monthly <- monthlyReturn(Ad(BICSX), type = "log")

# Step 3: Create data frames for each return series
SPY_df <- data.frame(Date = index(SPY_monthly), SPY_Return = coredata(SPY_monthly))
SLB_df <- data.frame(Date = index(SLB_monthly), SLB_Return = coredata(SLB_monthly))
BICSX_df <- data.frame(Date = index(BICSX_monthly), BICSX_Return = coredata(BICSX_monthly))

# Ensure Date format is consistent
SPY_df$Date <- as.Date(SPY_df$Date)
SLB_df$Date <- as.Date(SLB_df$Date)
BICSX_df$Date <- as.Date(BICSX_df$Date)

# Merge all return series into a single data frame
merged_data <- merge(SPY_df, SLB_df, by = "Date", all = TRUE)
merged_data <- merge(merged_data, BICSX_df, by = "Date", all = TRUE)

# Step 4: Import Crude Oil Data
oil_data <- read.csv("WTI Crude Oil Percent Change Monthly End of Period.csv")

# Rename columns for clarity and format Date
colnames(oil_data) <- c("Date", "Oil_Return")
oil_data$Date <- as.Date(oil_data$Date, format = "%m/%d/%Y")

# Align oil_data dates with end-of-month convention
oil_data$Date <- as.Date(format(oil_data$Date, "%Y-%m-01")) + 31
oil_data$Date <- as.Date(format(oil_data$Date, "%Y-%m-%d")) - as.numeric(format(oil_data$Date, "%d"))

# Merge oil data with financial returns data
merged_data <- merge(merged_data, oil_data, by = "Date", all = FALSE)

# Rename columns for clarity
colnames(merged_data) <- c("Date", "SPY_Return", "SLB_Return", "BICSX_Return", "Oil_Return")

# Ensure all return columns are numeric
merged_data$SPY_Return <- as.numeric(merged_data$SPY_Return)
merged_data$SLB_Return <- as.numeric(merged_data$SLB_Return)
merged_data$BICSX_Return <- as.numeric(merged_data$BICSX_Return)
merged_data$Oil_Return <- as.numeric(merged_data$Oil_Return)

# Remove rows with missing values
merged_data <- na.omit(merged_data)

# Verify final structure
cat("Final structure of merged_data:\n")
print(str(merged_data))

# ---------------------- #
# Part 2: Data Analysis  #
# ---------------------- #

# A. Summary Statistics
means <- sapply(merged_data[, -1], mean, na.rm = TRUE)  # Exclude Date column
sds <- sapply(merged_data[, -1], sd, na.rm = TRUE)
summary_stats <- data.frame(Mean = means, SD = sds)

cat("Summary statistics for returns:\n")
print(summary_stats)

# B. Histograms for each return series
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
hist(merged_data$SPY_Return, breaks = 20, main = "Histogram of SPY Returns", xlab = "SPY Returns", col = "blue")
hist(merged_data$SLB_Return, breaks = 20, main = "Histogram of SLB Returns", xlab = "SLB Returns", col = "green")
hist(merged_data$BICSX_Return, breaks = 20, main = "Histogram of BICSX Returns", xlab = "BICSX Returns", col = "red")
hist(merged_data$Oil_Return, breaks = 20, main = "Histogram of Oil Returns", xlab = "Oil Returns", col = "purple")

# C. Scatterplots and Single Linear Regressions
scatterplot_and_regression <- function(x, y, xlab, ylab, main, col) {
  plot(x, y, main = main, xlab = xlab, ylab = ylab, col = col, pch = 19)
  model <- lm(y ~ x)
  abline(model, col = "red", lwd = 2)
  return(model)
}

cat("\nSingle Linear Regression: SLB vs SPY\n")
model_slb_spy <- scatterplot_and_regression(merged_data$SPY_Return, merged_data$SLB_Return, 
                                            "SPY Returns", "SLB Returns", 
                                            "SLB vs SPY Returns", "green")

cat("\nSingle Linear Regression: SLB vs Oil\n")
model_slb_oil <- scatterplot_and_regression(merged_data$Oil_Return, merged_data$SLB_Return, 
                                            "Oil Returns", "SLB Returns", 
                                            "SLB vs Oil Returns", "blue")

cat("\nSingle Linear Regression: BICSX vs SPY\n")
model_bicsx_spy <- scatterplot_and_regression(merged_data$SPY_Return, merged_data$BICSX_Return, 
                                              "SPY Returns", "BICSX Returns", 
                                              "BICSX vs SPY Returns", "red")

cat("\nSingle Linear Regression: BICSX vs Oil\n")
model_bicsx_oil <- scatterplot_and_regression(merged_data$Oil_Return, merged_data$BICSX_Return, 
                                              "Oil Returns", "BICSX Returns", 
                                              "BICSX vs Oil Returns", "purple")

# D. Multiple Linear Regressions
cat("\nMultiple Regression: SLB ~ SPY + Oil\n")
model_slb_multiple <- lm(SLB_Return ~ SPY_Return + Oil_Return, data = merged_data)
print(summary(model_slb_multiple))

cat("\nMultiple Regression: BICSX ~ SPY + Oil\n")
model_bicsx_multiple <- lm(BICSX_Return ~ SPY_Return + Oil_Return, data = merged_data)
print(summary(model_bicsx_multiple))

# E. Residual Analysis (Binned Scatterplot)
binscatter <- function(x, y, bins = 10, main = "Binned Scatterplot") {
  bin <- cut(x, breaks = quantile(x, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE), include.lowest = TRUE)
  bin_means <- aggregate(cbind(x, y), by = list(bin), FUN = mean)
  plot(bin_means$x, bin_means$y, pch = 19, xlab = "Binned X", ylab = "Mean Y", main = main, col = "blue")
  abline(lm(bin_means$y ~ bin_means$x), col = "red", lwd = 2)
}

# Residual scatterplot for SLB vs Oil
slb_residuals <- resid(lm(SLB_Return ~ SPY_Return, data = merged_data))
oil_residuals <- resid(lm(Oil_Return ~ SPY_Return, data = merged_data))
binscatter(oil_residuals, slb_residuals, bins = 10, main = "Residual SLB Returns vs Residual Oil Returns")

# Residual scatterplot for BICSX vs Oil
bicsx_residuals <- resid(lm(BICSX_Return ~ SPY_Return, data = merged_data))
binscatter(oil_residuals, bicsx_residuals, bins = 10, main = "Residual BICSX Returns vs Residual Oil Returns")
