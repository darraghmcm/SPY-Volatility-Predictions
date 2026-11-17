# SPY-Volatility-Predictions
Predicting Stock Market Volatility using Logistic Regression and XGBoost in R.
This project investigates whether machine learning algorithms can predict periods of high and low stock market volatility over the following five trading days.

The analysis focuses on the SPDR S&P 500 ETF Trust (SPY), an Exchange-Traded Fund (ETF) designed to track the performance of the S&P 500 index, which represents approximately 500 leading publicly traded US companies.

Two classification algorithms are compared:

- Logistic Regression
- Extreme Gradient Boosting (XGBoost)

The objective is to evaluate whether historical market information can provide useful predictions of future volatility and to investigate how these predictions could assist investors in managing financial risk.
The project was developed as part of the R for Statistical Data Science module (MS6071) at the University of Limerick.

# Dataset

Historical SPY price data was downloaded from Yahoo Finance using the quantmod package in R.

- Asset: SPY (SPDR S&P 500 ETF Trust)
- Data period: 2010–2025
- Data frequency: Daily trading observations
- Price variable: Adjusted closing price

Adjusted closing prices were used to account for corporate actions such as dividends and stock splits.

The historical price data was used to calculate technical indicators and construct the target variable for the classification models.
