# SPY-Volatility-Predictions
Predicting Stock Market Volatility using Logistic Regression and XGBoost in R.
This project investigates whether machine learning algorithms can predict periods of high and low stock market volatility over the following five trading days.

The analysis focuses on the SPDR S&P 500 ETF Trust (SPY), an Exchange-Traded Fund (ETF) designed to track the performance of the S&P 500 index, which represents approximately 500 leading publicly traded US companies.

Two classification algorithms are compared:

- Logistic Regression
- Extreme Gradient Boosting (XGBoost)

The objective is to evaluate whether historical market information can provide useful predictions of future volatility and to investigate how these predictions could assist investors in managing financial risk.
The project was developed as part of the R for Statistical Data Science module (MS6071) at the University of Limerick.

**Dataset**

Historical SPY price data was downloaded from Yahoo Finance using the quantmod package in R.

- Asset: SPY (SPDR S&P 500 ETF Trust)
- Data period: 2010–2025
- Data frequency: Daily trading observations
- Price variable: Adjusted closing price

Adjusted closing prices were used to account for corporate actions such as dividends and stock splits.

The historical price data was used to calculate technical indicators and construct the target variable for the classification models.

**Predictor Variables**

- Daily Return: Percentage change in SPY's adjusted closing price from the previous trading day
- 20-day Volatility: Standard deviation of daily returns over the previous 20 trading days
- 14-day RSI: Relative Strength Index measuring the strength of recent price increases relative to decreases

**Target Variable**

The models predict whether volatility over the next five trading days will be unusually high. Future volatility is calculated using the standard deviation of daily returns over the following five trading days. A rolling benchmark is constructed using the median of historical five-day volatility over approximately the previous trading year (252 trading days).

The target is defined as:

- High Volatility (1): Future five-day volatility exceeds the rolling benchmark.
- Normal/Low Volatility (0): Future five-day volatility is below or equal to the rolling benchmark.

This approach allows the classification threshold to adapt to changing historical market conditions.

**Machine Learning Models**

- Logistic Regression
- XGBoost

**Model Validation and Testing**

Because financial observations are ordered over time, expanding-window walk-forward validation was used instead of random cross-validation. The models were trained using historical observations and evaluated on subsequent years. Validation was performed across six years, from 2018 to 2023, with the training window expanding after each validation period. This approach helped evaluate the models under different historical market conditions while preserving the chronological order of observations. The final models were trained on data before 2024 and evaluated using a separate, unseen test period covering 2024–2025.

**Evaluation Metrics**

- Accuracy
- AUC (Area under the ROC Curve)
- Brier Score

**Results**


### Final Test Performance (2024–2025)

| Metric | Logistic Regression | XGBoost |
|---|---:|---:|
| Accuracy | **0.685** | 0.669 |
| AUC | 0.705 | **0.728** |
| Brier Score | **0.217** | 0.220 |

**Interactive Shiny Dashboard**

An interactive R Shiny application was developed to allow users to explore the model predictions.

The dashboard allows user to pick a date and compare the high-volatility probabilities from both logistic regression and xgboost. They can then compare the predictions with the actual observed outcome over the next five trading days.

Link to Shiny Dashboard:
https://darraghmcm.shinyapps.io/Downloads/

**R Packages Used**

- quantmod - Financial Market Data Retrieval
- dplyr - Data Manipulation
- TTR - Technical Indicators
- ggplot2 - Data Visualisation
- pROC - ROC Curves and AUC Calculation
- xgboost - Gradient Boosting Classification
- car - Multicollinearity Diagnostics
- shiny - Interactive Dashboard Development
- rsconnect - Deployment of ShinyApp
