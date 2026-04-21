# Time Series Forecasting and Risk Analysis

This project analyzes two monthly time series with different statistical behavior:

- **Baltic Dry Index (BDI):** a non-seasonal financial and shipping market indicator
- **Ice Cream and Frozen Dessert Production Index:** a seasonal industrial production series

The goal was to compare modeling strategies for non-seasonal and seasonal data, test stationarity, fit appropriate ARIMA-family models, diagnose residual behavior, and produce 12-month forecasts.

> **Start here:** [Final Report PDF](reports/project-report.pdf)

## Project Overview

The analysis follows a classical time series workflow:

1. Clean and preprocess the source datasets
2. Convert both series into monthly time series objects
3. Explore trend, seasonality, and decomposition behavior
4. Test stationarity using the Augmented Dickey-Fuller test
5. Apply differencing where needed
6. Inspect ACF and PACF patterns
7. Fit and compare ARIMA and SARIMA models
8. Run residual diagnostics and Ljung-Box tests
9. Generate 12-month forecasts with confidence intervals

## Datasets

### Baltic Dry Index

- Type: non-seasonal monthly economic/market series
- File: `data/raw/baltic-dry-index-historical-data.csv`
- Use in project: ARIMA-based forecasting and residual diagnostics

### Ice Cream and Frozen Dessert Production

- Type: seasonal monthly industrial production index
- File: `data/raw/ice-cream-frozen-dessert-seasonal-dataset.csv`
- Use in project: SARIMA-based forecasting, seasonal differencing, and decomposition

## Methods and Tools

- **Language:** R
- **Notebook format:** Jupyter Notebook
- **Core libraries:** `forecast`, `tseries`, `ggplot2`, `lubridate`, `zoo`, `scales`
- **Modeling techniques:** ARIMA, SARIMA, stationarity testing, decomposition, ACF/PACF analysis, residual diagnostics

## Working Files

- `src/time-series-forecasting-and-risk-analysis.R` is the cleaned, repo-ready version of the analysis script and uses relative data paths.
- `notebooks/time-series-forecasting-and-risk-analysis.ipynb` preserves the original notebook-based workflow from the project submission.

## Repository Structure

```text
time-series-forecasting-risk-analysis/
  README.md
  .gitignore
  data/
    README.md
    raw/
      baltic-dry-index-historical-data.csv
      ice-cream-frozen-dessert-seasonal-dataset.csv
  notebooks/
    time-series-forecasting-and-risk-analysis.ipynb
  reports/
    project-report.pdf
  src/
    time-series-forecasting-and-risk-analysis.R
```

## Key Analysis Areas

- Time series preprocessing and monthly indexing
- Stationarity testing with ADF
- First-order and seasonal differencing
- Model identification with ACF/PACF
- Manual and automatic model selection
- Residual checking and diagnostic evaluation
- Forecast generation with interval estimates

## Main Takeaways

- The Baltic Dry Index behaves like a non-seasonal time series and is modeled with ARIMA techniques after differencing.
- The ice cream production series shows clear seasonal structure and is better handled with seasonal modeling.
- Comparing both datasets in one project makes the difference between ARIMA and SARIMA modeling easy to explain.

## Notes

- This staged GitHub version keeps the strongest source files, report material, and datasets while leaving out scratch files such as `.RData`, `.Rhistory`, notebook checkpoints, and duplicate draft artifacts.
- Some original files in the source folder were exploratory or intermediate submissions and are intentionally not included in the cleaned repository.
