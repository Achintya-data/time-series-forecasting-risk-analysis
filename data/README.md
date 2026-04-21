# Data Notes

This project uses two monthly datasets:

1. `baltic-dry-index-historical-data.csv`
   - Non-seasonal economic and shipping market indicator
   - Used for ARIMA modeling and 12-month forecasting

2. `ice-cream-frozen-dessert-seasonal-dataset.csv`
   - Seasonal industrial production index
   - Used for decomposition, seasonal differencing, SARIMA modeling, and forecasting

The staged repository includes the working CSV files used in the final submission so the analysis can be reproduced without depending on hidden local paths.
