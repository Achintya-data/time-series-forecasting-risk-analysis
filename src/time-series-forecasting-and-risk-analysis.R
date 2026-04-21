
# Load required libraries
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)
library(zoo)
library(scales)

# Baltic Dry Index dataset
bdi_data <- read.csv(
  "data/raw/baltic-dry-index-historical-data.csv",
  stringsAsFactors = FALSE
)

# Ice Cream dataset
ice_cream_data <- read.csv(
  "data/raw/ice-cream-frozen-dessert-seasonal-dataset.csv",
  stringsAsFactors = FALSE
)



# Baltic Dry Index Preprocessing

# Convert Date to Date type properly
bdi_data$Date <- as.Date(bdi_data$Date, format = "%m/%d/%Y")

# Sort by ascending Date
bdi_data <- bdi_data[order(bdi_data$Date), ]

# Force Price column to be numeric (removes commas, spaces if any)
bdi_data$Price <- as.numeric(gsub(",", "", bdi_data$Price))

# Drop rows where Price is NA
bdi_data_clean <- na.omit(bdi_data[, c("Date", "Price")])

# Create MONTHLY Time Series
start_year <- year(min(bdi_data_clean$Date))
start_month <- month(min(bdi_data_clean$Date))

bdi_ts <- ts(bdi_data_clean$Price, start = c(start_year, start_month), frequency = 12)


# Ice Cream Data Preprocessing

# Rename the columns
colnames(ice_cream_data) <- c("Date", "Production_Index")

# Date format parsing
ice_cream_data$Date <- as.Date(ice_cream_data$Date)

# Sort the dataset by Date in ascending order
ice_cream_data <- ice_cream_data[order(ice_cream_data$Date), ]

# Drop rows where Production_Index is NA
ice_data_clean <- na.omit(ice_cream_data)

# Create a Monthly Time Series Object
start_year <- year(min(ice_data_clean$Date))
start_month <- month(min(ice_data_clean$Date))

ice_cream_ts <- ts(
  ice_data_clean$Production_Index,start = c(start_year, start_month),
  frequency = 12)


# Plotting Baltic Dry Index as Monthly Time Series

options(repr.plot.width = 12, repr.plot.height = 6)
autoplot(bdi_ts) +
  labs(
    title = "Baltic Dry Index - Monthly Time Series",
    x = "Year",
    y = "BDI Price"
  ) +
  theme_minimal()

# Baltic Dry Index Monthly Plot

ggplot(data = bdi_data_clean, aes(x = Date, y = Price)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_point(color = "black", size = 1) +
  labs(
    title = "Baltic Dry Index (BDI) Over Time",
    subtitle = "Monthly Data from January 2002 to January 2024",
    x = "Year",
    y = "BDI Price (Index Value)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))


# Add a smooth trend line (optional but recommended)
ggplot(data = bdi_data_clean, aes(x = Date, y = Price)) +
  geom_line(color = "darkblue", size = 1.2) +
  geom_point(color = "black", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linetype = "dashed", size = 1) +  # SMOOTH RED LINE
  labs(
    title = "Baltic Dry Index (BDI) Over Time",
    subtitle = "Monthly Data from January 2002 to January 2024",
    x = "Year",
    y = "BDI Price (Index Value)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05)))


# Plotting the Ice Cream Production Series
autoplot(ice_cream_ts) +
  labs(
    title = "Industrial Production: Ice Cream and Frozen Desserts (Monthly)",
    subtitle = paste0("From ", start_year, " to ", year(max(ice_cream_data$Date))),
    x = "Year",
    y = "Production Index (2017=100)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Seasonal Plot for Ice Cream Production

library(scales)  # for better date formatting on X-axis

autoplot(ice_cream_ts) +
  geom_line(color = "darkred", size = 1.2) +
  labs(
    title = "Seasonal Trend: Industrial Production of Ice Cream and Frozen Desserts",
    subtitle = paste0("Monthly Data from ", start_year, " to ", year(max(ice_cream_data$Date))),
    x = "Year",
    y = "Production Index (Base Year 2017 = 100)"
  ) +
  scale_x_continuous(
    breaks = seq(start_year, year(max(ice_cream_data$Date)), by = 2),
    labels = function(x) format(as.Date(paste0(x, "-01-01")), "%Y")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  ) +
  scale_y_continuous(labels = comma)  # Adds commas to y-axis numbers if large

# Decomposing the Ice Cream Production

# Additive decomposition (because index values are relatively stable)
ice_cream_decomp <- decompose(ice_cream_ts, type = "additive")

# Plot the decomposed components
autoplot(ice_cream_decomp) +
  labs(
    title = "Decomposition of Ice Cream Production Time Series",
    x = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# ADF Test for Baltic Dry Index (BDI)
adf.test(bdi_ts)

# ADF Test for Ice Cream Production Time Series
adf.test(ice_cream_ts)



# First Differencing for Baltic Dry Index (BDI) ---
bdi_diff <- diff(bdi_ts, differences = 1)

# Plot differenced BDI series
autoplot(bdi_diff) +
  labs(
    title = "Differenced Baltic Dry Index (1st Difference)",
    x = "Year",
    y = "Differenced BDI Price"
  ) +
  theme_minimal()

# ADF Test on Differenced BDI
adf.test(bdi_diff)


# First Order Differencing
ice_cream_diff1 <- diff(ice_cream_ts, differences = 1)

# Seasonal Differencing (Lag-12)
ice_cream_diff_seasonal <- diff(ice_cream_diff1, lag = 12)

# Plot the final differenced series
autoplot(ice_cream_diff_seasonal) +
  labs(
    title = "First and Seasonal Differenced Ice Cream Production Time Series",
    x = "Year",
    y = "Differenced Production Index"
  ) +
  theme_minimal()

# ADF Test on the Final Differenced Series
adf.test(ice_cream_diff_seasonal)


# ACF and PACF for Differenced BDI

# First difference for BDI
bdi_diff <- diff(bdi_ts, differences = 1)

# Plot ACF
acf(bdi_diff, main = "ACF of Differenced Baltic Dry Index")

# Plot PACF
pacf(bdi_diff, main = "PACF of Differenced Baltic Dry Index")


# ACF and PACF for Differenced Ice Cream Production

# First differencing
ice_cream_diff1 <- diff(ice_cream_ts, differences = 1)

# Seasonal differencing
ice_cream_diff_seasonal <- diff(ice_cream_diff1, lag = 12)

# Plot ACF
acf(ice_cream_diff_seasonal, main = "ACF of Differenced Ice Cream Production")

# Plot PACF
pacf(ice_cream_diff_seasonal, main = "PACF of Differenced Ice Cream Production")

# ACF Plot for BDI
acf(bdi_ts, lag.max = 36, main = "ACF of Baltic Dry Index (Up to 36 Lags)")

# PACF Plot for BDI
pacf(bdi_ts, lag.max = 36, main = "PACF of Baltic Dry Index (Up to 36 Lags)")

# ACF Plot for Ice Cream Production
acf(ice_cream_ts, lag.max = 48, main = "ACF of Ice Cream Production (Up to 48 Lags)")

# PACF Plot for Ice Cream Production
pacf(ice_cream_ts, lag.max = 48, main = "PACF of Ice Cream Production (Up to 48 Lags)")


# Fitting ARIMA to BDI
bdi_model <- Arima(bdi_ts, order = c(1,1,1))

# Summary of the model
summary(bdi_model)

Box.test(residuals(bdi_model), lag=20, type="Ljung-Box")

# Residual Diagnostics for BDI Model
checkresiduals(bdi_model)


# Fit SARIMA to Ice Cream Data
ice_cream_model <- Arima(
  ice_cream_ts,
  order = c(1,1,0),
  seasonal = list(order = c(0,1,1), period = 12)
)

# Summary of the model
summary(ice_cream_model)

Box.test(residuals(ice_cream_model), lag=24, type="Ljung-Box")


# Residual Diagnostics for Ice Cream Model
checkresiduals(ice_cream_model)


# Find the best ARIMA model for BDI
bdi_auto_model <- auto.arima(bdi_ts, stepwise = FALSE, approximation = FALSE, trace = TRUE)

# Summary of best BDI model
summary(bdi_auto_model)

# Residual Diagnostics
checkresiduals(bdi_auto_model)

# Find the best SARIMA model for Ice Cream Production
ice_cream_auto_model <- auto.arima(
  ice_cream_ts,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)


# Summary of best Ice Cream model
summary(ice_cream_auto_model)

# Residual Diagnostics
checkresiduals(ice_cream_auto_model)

# Histogram of Residuals
bdi_residuals <- residuals(bdi_auto_model)  # Replace with your BDI model variable name

hist(bdi_residuals, breaks = 20, col = "skyblue", main = "Histogram of BDI Model Residuals",
     xlab = "Residuals", border = "white")

# Normal Q-Q Plot
qqnorm(bdi_residuals, main = "Normal Q-Q Plot of BDI Model Residuals")
qqline(bdi_residuals, col = "red", lwd = 2)
# Residual ACF for BDI Model
acf(bdi_residuals, lag.max = 36, main = "ACF of Residuals - BDI Model")

# Histogram of Residuals
ice_cream_residuals <- residuals(ice_cream_model)  # Replace with your Ice Cream model variable name

hist(ice_cream_residuals, breaks = 20, col = "pink", main = "Histogram of Ice Cream Model Residuals",
     xlab = "Residuals", border = "white")

# Normal Q-Q Plot
qqnorm(ice_cream_residuals, main = "Normal Q-Q Plot of Ice Cream Model Residuals")
qqline(ice_cream_residuals, col = "red", lwd = 2)

# Residual ACF for Ice Cream Model
acf(ice_cream_residuals, lag.max = 48, main = "ACF of Residuals - Ice Cream Model")



#Forecast BDI using Auto-Selected ARIMA(0,1,5)

# Forecast next 12 months
bdi_forecast <- forecast(bdi_auto_model, h = 12)

# Plot the forecast
autoplot(bdi_forecast) +
  labs(
    title = "BDI - 12 Month Forecast",
    x = "Year",
    y = "BDI Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# View Forecast Table
bdi_forecast_df <- data.frame(
  Date = time(bdi_forecast$mean),
  `Point Forecast` = as.numeric(bdi_forecast$mean),
  `Lower 80%` = as.numeric(bdi_forecast$lower[,1]),
  `Upper 80%` = as.numeric(bdi_forecast$upper[,1]),
  `Lower 95%` = as.numeric(bdi_forecast$lower[,2]),
  `Upper 95%` = as.numeric(bdi_forecast$upper[,2])
)

# Display the forecast table
print("Point Forecast with Confidence Intervals")
print(bdi_forecast_df)

# Forecast Ice Cream using Manually Selected SARIMA(1,1,0)(0,1,1)[12]

# Forecast next 12 months
ice_cream_forecast <- forecast(ice_cream_model, h = 12)

# Plot the forecast
autoplot(ice_cream_forecast) +
  labs(
    title = "Ice Cream Production - 12 Month Forecast",
    x = "Year",
    y = "Production Index (Base 2017 = 100)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )


# View Forecast Table
ice_cream_forecast_df <- data.frame(
  Date = time(ice_cream_forecast$mean),
  `Point Forecast` = as.numeric(ice_cream_forecast$mean),
  `Lower 80%` = as.numeric(ice_cream_forecast$lower[,1]),
  `Upper 80%` = as.numeric(ice_cream_forecast$upper[,1]),
  `Lower 95%` = as.numeric(ice_cream_forecast$lower[,2]),
  `Upper 95%` = as.numeric(ice_cream_forecast$upper[,2])
)

# Display the forecast table
print("Point Forecast with Confidence Intervals")
print(ice_cream_forecast_df)

# BDI Forecast Plot with shading
bdi_forecast_df <- data.frame(
  Date = as.numeric(time(bdi_forecast$mean)),
  Mean = as.numeric(bdi_forecast$mean),
  Lower80 = as.numeric(bdi_forecast$lower[,1]),
  Upper80 = as.numeric(bdi_forecast$upper[,1]),
  Lower95 = as.numeric(bdi_forecast$lower[,2]),
  Upper95 = as.numeric(bdi_forecast$upper[,2])
)

#  Manual Forecast Plot for BDI
ggplot(bdi_forecast_df, aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95), fill = "lightblue", alpha = 0.3) +  # 95% CI
  geom_ribbon(aes(ymin = Lower80, ymax = Upper80), fill = "skyblue", alpha = 0.5) +    # 80% CI
  geom_line(aes(y = Mean), color = "black", size = 1.2) +  # Forecast Line
  labs(
    title = "Forecast of Baltic Dry Index (Next 12 Months)",
    subtitle = "80% and 95% Confidence Intervals",
    x = "Year",
    y = "BDI Price Index"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Ice Cream Forecast Plot with shading
ice_cream_forecast_df <- data.frame(
  Date = as.numeric(time(ice_cream_forecast$mean)),
  Mean = as.numeric(ice_cream_forecast$mean),
  Lower80 = as.numeric(ice_cream_forecast$lower[,1]),
  Upper80 = as.numeric(ice_cream_forecast$upper[,1]),
  Lower95 = as.numeric(ice_cream_forecast$lower[,2]),
  Upper95 = as.numeric(ice_cream_forecast$upper[,2])
)

#  Manual Forecast Plot for Ice Cream Production
ggplot(ice_cream_forecast_df, aes(x = Date)) +
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95), fill = "mistyrose", alpha = 0.3) +  # 95% CI
  geom_ribbon(aes(ymin = Lower80, ymax = Upper80), fill = "pink", alpha = 0.5) +       # 80% CI
  geom_line(aes(y = Mean), color = "black", size = 1.2) +  # Forecast Line
  labs(
    title = "Forecast of Ice Cream Production (Next 12 Months)",
    subtitle = "80% and 95% Confidence Intervals",
    x = "Year",
    y = "Production Index (Base 2017 = 100)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Simple dataframe for Forecast Mean
bdi_forecast_line_df <- data.frame(
  Date = as.numeric(time(bdi_forecast$mean)),
  Value = as.numeric(bdi_forecast$mean)
)

#  Observed Data
bdi_observed_df <- data.frame(
  Date = as.numeric(time(bdi_ts)),
  Value = as.numeric(bdi_ts)
)

ice_cream_observed_df <- data.frame(
  Date = as.numeric(time(ice_cream_ts)),
  Value = as.numeric(ice_cream_ts)
)

# Plot for Baltic Dry Index Forecast

ggplot() +
  geom_line(data = bdi_observed_df, aes(x = Date, y = Value), color = "black", size = 1.2) +  # Past actual data
  geom_line(data = bdi_forecast_line_df, aes(x = Date, y = Value), color = "blue", size = 1.2, linetype = "dashed") +  # Forecasted mean
  geom_ribbon(data = bdi_forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95), fill = "lightblue", alpha = 0.3) +  # 95% CI
  geom_ribbon(data = bdi_forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80), fill = "skyblue", alpha = 0.5) +   # 80% CI
  geom_line(data = bdi_forecast_df, aes(x = Date, y = Lower95), linetype = "dashed", color = "red", size = 0.8) +  # 95% Lower
  geom_line(data = bdi_forecast_df, aes(x = Date, y = Upper95), linetype = "dashed", color = "red", size = 0.8) +  # 95% Upper
  geom_line(data = bdi_forecast_df, aes(x = Date, y = Lower80), linetype = "dotted", color = "darkred", size = 0.8) +  # 80% Lower
  geom_line(data = bdi_forecast_df, aes(x = Date, y = Upper80), linetype = "dotted", color = "darkred", size = 0.8) +  # 80% Upper
  labs(
    title = "Baltic Dry Index: Observed and Forecasted",
    subtitle = "Dashed and dotted lines show 80% and 95% confidence intervals",
    x = "Year",
    y = "BDI Price Index"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

# Plot for Ice Cream Production Forecast

# Ice Cream Production Forecast: Dashed Boundary Lines

ice_cream_forecast_line_df <- data.frame(
  Date = as.numeric(time(ice_cream_forecast$mean)),
  Value = as.numeric(ice_cream_forecast$mean)
)

ggplot() +
  geom_line(data = ice_cream_observed_df, aes(x = Date, y = Value), color = "black", size = 1.2) +  # Past data
  geom_line(data = ice_cream_forecast_line_df, aes(x = Date, y = Value), color = "blue", size = 1.2, linetype = "dashed") +  # Forecasted mean
  geom_ribbon(data = ice_cream_forecast_df, aes(x = Date, ymin = Lower95, ymax = Upper95), fill = "mistyrose", alpha = 0.3) +  # 95% CI
  geom_ribbon(data = ice_cream_forecast_df, aes(x = Date, ymin = Lower80, ymax = Upper80), fill = "pink", alpha = 0.5) +       # 80% CI
  geom_line(data = ice_cream_forecast_df, aes(x = Date, y = Lower95), linetype = "dashed", color = "red", size = 0.8) +  # 95% Lower
  geom_line(data = ice_cream_forecast_df, aes(x = Date, y = Upper95), linetype = "dashed", color = "red", size = 0.8) +  # 95% Upper
  geom_line(data = ice_cream_forecast_df, aes(x = Date, y = Lower80), linetype = "dotted", color = "darkred", size = 0.8) +  # 80% Lower
  geom_line(data = ice_cream_forecast_df, aes(x = Date, y = Upper80), linetype = "dotted", color = "darkred", size = 0.8) +  # 80% Upper
  labs(
    title = "Ice Cream Production: Observed and Forecasted",
    subtitle = "Dashed lines show 80% and 95% confidence intervals",
    x = "Year",
    y = "Production Index (Base 2017 = 100)"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

