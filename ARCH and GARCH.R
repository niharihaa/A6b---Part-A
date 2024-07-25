# Install necessary packages if not already installed
if (!require(quantmod)) install.packages("quantmod")
if (!require(tseries)) install.packages("tseries")
if (!require(FinTS)) install.packages("FinTS")
if (!require(rugarch)) install.packages("rugarch")

# Load the libraries
library(quantmod)
library(tseries)
library(FinTS)
library(rugarch)

# Define the ticker symbol and the date range
ticker <- "HUDCO.NS"
start_date <- as.Date("2019-04-01")
end_date <- as.Date("2024-03-31")

# Download the data
getSymbols(ticker, src = "yahoo", from = start_date, to = end_date)
hudco_data <- get(ticker)

# Display the first few rows of the data
head(hudco_data)

# Check for missing values
missing_values <- sum(is.na(hudco_data))
print(paste("Total number of missing values:", missing_values))

# Plot the adjusted closing price for the entire period
plot(hudco_data$HUDCO.NS.Adjusted, main = "HUDCO Adjusted Closing Prices (2019-2024)", 
     ylab = "Adjusted Price", xlab = "Date", col = "blue", lwd = 2)

# Step 4: Calculate Returns (Using Adjusted Prices)
hudco_returns <- dailyReturn(hudco_data$HUDCO.NS.Adjusted, type = "log") * 100
names(hudco_returns) <- "Returns"

# Plot the returns
plot(hudco_returns, main = "HUDCO Daily Returns (Adjusted Prices)", 
     ylab = "Returns (%)", xlab = "Date", col = "green", lwd = 2)

# Step 5: Check for ARCH Effects
# Perform ARCH test
arch_test <- ArchTest(hudco_returns, lags = 12)
print(arch_test)

# Step 6: Fit an ARCH Model and Plot Conditional Volatility
spec_arch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)), mean.model = list(armaOrder = c(0, 0)))
fit_arch <- ugarchfit(spec = spec_arch, data = hudco_returns)
print(fit_arch)

# Plot the conditional volatility from the ARCH model
plot(fit_arch, which = 3, main = "Conditional Volatility (ARCH Model)", 
     col = "purple", lwd = 2)

# Forecast 3-month (approximately 60 trading days) volatility
forecast_arch <- ugarchforecast(fit_arch, n.ahead = 60)
sigma_forecast_arch <- sigma(forecast_arch)

# Plot the forecasted volatility from the ARCH model
plot(sigma_forecast_arch, type = "l", main = "Forecasted Volatility for 3 Months (ARCH)", 
     ylab = "Volatility", xlab = "Days", col = "purple", lwd = 2)

# Step 7: Fit a GARCH Model and Plot Conditional Volatility
spec_garch <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
fit_garch <- ugarchfit(spec = spec_garch, data = hudco_returns)
print(fit_garch)

# Plot the conditional volatility from the GARCH model
plot(fit_garch, which = 3, main = "Conditional Volatility (GARCH Model)", 
     col = "red", lwd = 2)

# Forecast 3-month (approximately 60 trading days) volatility
forecast_garch <- ugarchforecast(fit_garch, n.ahead = 60)
sigma_forecast_garch <- sigma(forecast_garch)

# Plot the forecasted volatility from the GARCH model
plot(sigma_forecast_garch, type = "l", main = "Forecasted Volatility for 3 Months (GARCH)", 
     ylab = "Volatility", xlab = "Days", col = "red", lwd = 2)

# Additional Visualizations: ACF and PACF plots of the returns
par(mfrow = c(2, 1))
acf(hudco_returns, main = "ACF of HUDCO Returns (Adjusted Prices)", col = "darkblue", lwd = 2)
pacf(hudco_returns, main = "PACF of HUDCO Returns (Adjusted Prices)", col = "darkred", lwd = 2)
par(mfrow = c(1, 1))
