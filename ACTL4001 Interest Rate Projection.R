##########################################INTEREST RATE----
# Load necessary libraries
library(forecast)
library(readxl)
library(tseries)
library(tsoutliers)

# Read the data from CSV file
data <- read_excel("srcsc-2024-lumaria-economic-data.xlsx")

#Convert to time series object
ts_data <- ts(data$`1-yr Risk Free Annual Spot Rate`, start = min(data$Year), 
              end = max(data$Year), frequency = 1)

# Detect outliers
outliers <- tso(ts_data, types = c("AO", "LS", "TC"), maxit.iloop = 10)
ts_data_adj <- ts_data
ts_data_adj[outliers$outliers$ind] <- ts_data_adj[outliers$outliers$ind] - 
  outliers$outliers$coefhat

# Visualize the data
plot(ts_data_adj, main = "Spot Rate Over Time", 
     xlab = "Year", 
     ylab = "Spot Rate")

# Fit ARIMA model
arima_model <- auto.arima(ts_data_adj, d = d, D = 0, max.p = 3, max.q = 3, 
                          seasonal = FALSE)

# Summary of the ARIMA model
summary(arima_model)

# Forecast interest rates for the next 60 years
forecast_values <- forecast(arima_model, h = 60)

print(forecast_values)




