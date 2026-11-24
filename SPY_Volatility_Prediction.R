## Packages
library(quantmod)  # download stock market data
library(dplyr)  # data manipulation
library(TTR)  # technical indicators
library(ggplot2)  # plotting
library(pROC)  # AUC calculation
library(car)  # VIF calculation
library(xgboost)  # XGBoost model

set.seed(123)

## Download SPY data
getSymbols(
  "SPY",
  src = "yahoo",  # data source
  from = "2010-01-01",
  to = "2025-12-31"
)

## Create data frame
spy <- data.frame(
  Date = index(SPY),  # trading date
  Price = as.numeric(Ad(SPY))  # adjusted closing price
)

## Plot of SPY price history
ggplot(spy,
  aes(x = Date, y = Price)
) +
  geom_line(linewidth = 0.8, colour = "steelblue") +
  labs(
    title = "S&P 500 Market Performance: 2010–2025",
    subtitle = "SPY is an ETF designed to track the S&P 500",
    x = NULL,
    y = "Adjusted SPY Price ($)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 16))

ggsave("spy_price_history.png",
  width = 10,
  height = 6,
  dpi = 300)

## Calculate daily return

spy$Return <- c( NA,
  diff(spy$Price) / head(spy$Price, -1))  # percentage change from previous day

## Create technical indicators
spy <- spy %>%
  mutate(
    Volatility20 = runSD(
      Return,
      n = 20),  # volatility over previous 20 days
    
    RSI14 = RSI(
      Price,
      n = 14)  # 14-day Relative Strength Index
  )

## Create future volatility target
spy_vol <- spy %>%
  mutate(
    PastVol5 = runSD(
      Return,
      n = 5),  # volatility over previous 5 days
    
    FutureVol5 = lead(
      runSD(Return, n = 5),
      5),  # volatility over next 5 trading days
    
    VolThreshold = lag(
      runMedian(PastVol5, n = 252),
      1),  # median 5-day volatility over roughly previous year
    
    HighVol = ifelse(
      FutureVol5 > VolThreshold,
      1,  # unusually high future volatility
      0  # normal or lower future volatility
    )
  )

## Create modelling dataset
vol_model_data <- spy_vol %>%
  select(Date, Return, Volatility20, RSI14, PastVol5, FutureVol5, VolThreshold, HighVol) %>%
  na.omit()  # remove rows with missing values

summary(vol_model_data)

## Define model predictors

predictors <- c( "Return", "Volatility20", "RSI14")

## Development data
development_vol <- vol_model_data %>%
  filter(Date < as.Date("2024-01-01"))  # keep 2024-2025 untouched for final testing


## Correlation matrix
cor_data <- development_vol %>%
  select(Return, Volatility20,RSI14)

cor_matrix <- cor(cor_data)

round( cor_matrix, 2)

# VIF check
vif_model <- glm(
  HighVol ~ Return + Volatility20 + RSI14,
  data = development_vol,
  family = binomial
)

vif(vif_model)

## Walk-forward validation
validation_years <- 2018:2023

walk_forward_results <- data.frame()

## Run expanding-window validation
for (year in validation_years) {
  
  train_fold <- development_vol %>%
    filter(Date < as.Date(paste0(year, "-01-01")))
  
  validation_fold <- development_vol %>%
    filter(Date >= as.Date(paste0(year, "-01-01")), Date < as.Date(paste0(year + 1, "-01-01")))
  
  #Remove rows at split boundary
  train_fold <- train_fold[1:(nrow(train_fold) - 5),]
  
  # Logistic Regression
  logistic_fold <- glm(HighVol ~ Return + Volatility20 + RSI14, data = train_fold, family = binomial)
  
  # Logistic predictions
  prob_logistic <- predict(logistic_fold, newdata = validation_fold, type = "response")
  
  pred_logistic <- ifelse(prob_logistic > 0.5,1,0)
  
  #Logistic metrics
  accuracy_logistic <- mean( pred_logistic == validation_fold$HighVol)
  
  auc_logistic <- as.numeric(
    pROC::auc(validation_fold$HighVol, prob_logistic)
  )
  
  brier_logistic <- mean((prob_logistic - validation_fold$HighVol)^2)
  
  # Prepare XGBoost data
  x_train_fold <- as.matrix(train_fold[, predictors])
  
  x_validation_fold <- as.matrix(validation_fold[, predictors])
  
  y_train_fold <- factor(train_fold$HighVol,levels = c(0, 1))
  
  # Fit XGBoost
  xgb_fold <- xgboost( x = x_train_fold, y = y_train_fold,
    nrounds = 100,
    max_depth = 3,
    learning_rate = 0.1,
    verbose = 0)  #tell r not to display progress messages while running
  
  ## XGBoost predictions
  prob_XGB <- predict(xgb_fold, x_validation_fold, type = "response")
  
  pred_XGB <- ifelse(prob_XGB > 0.5,1, 0)
  
  ## XGBoost metrics
  accuracy_XGB <- mean(pred_XGB == validation_fold$HighVol)
  
  auc_XGB <- as.numeric(
    pROC::auc(
      validation_fold$HighVol,
      prob_XGB
    )
  )
  
  brier_XGB <- mean((prob_XGB - validation_fold$HighVol )^2)
  
  # Store fold results
  fold_results <- data.frame(
    Year = year,
    Logistic_Accuracy = accuracy_logistic,
    Logistic_AUC = auc_logistic,
    Logistic_Brier = brier_logistic,
    XGB_Accuracy = accuracy_XGB,
    XGB_AUC = auc_XGB,
    XGB_Brier = brier_XGB
  )
  
  walk_forward_results <- rbind( walk_forward_results, fold_results )
}

## View walk-forward results
walk_forward_results[, 2:7] <- round( walk_forward_results[, 2:7], 3)

walk_forward_results

## Yearly class balance and naive baseline
yearly_baseline <- vol_model_data %>%
  filter(
    Date >= as.Date("2018-01-01"),
    Date < as.Date("2024-01-01")
  ) %>%
  mutate(Year = as.integer(format(Date, "%Y"))
  ) %>%
  group_by(Year) %>%
  summarise(
    Prop_0 = mean(HighVol == 0),
    Prop_1 = mean(HighVol == 1),
    Naive_Accuracy = max(Prop_0, Prop_1)
  )

## Round yearly baseline results
yearly_baseline[, 2:4] <- round( yearly_baseline[, 2:4], 3)

yearly_baseline

# Plot walk-forward AUC
auc_plot_data <- walk_forward_results %>%
  select(Year, Logistic_AUC, XGB_AUC) %>%
  tidyr::pivot_longer(
    cols = c(
      Logistic_AUC,
      XGB_AUC
    ),
    names_to = "Model",
    values_to = "AUC"
  ) %>%
  mutate(
    Model = dplyr::recode(
      Model,
      Logistic_AUC = "Logistic Regression",
      XGB_AUC = "XGBoost"
    )
  )

ggplot(
  auc_plot_data,
  aes(
    x = Year,
    y = AUC,
    colour = Model,
    group = Model
  )
) +
  geom_hline(
    yintercept = 0.5,
    linetype = "dashed",
    linewidth = 0.8
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  geom_point(
    size = 3
  ) +
  scale_colour_manual(
    values = c(
      "Logistic Regression" = "steelblue",
      "XGBoost" = "firebrick")
  ) +
  scale_x_continuous(
    breaks = 2018:2023
  ) +
  scale_y_continuous(
    limits = c(0.45, 0.9),
    breaks = seq(0.5,0.9, 0.1)
  ) +
  labs(
    title = "Model Performance Across Different Years",
    subtitle = "AUC above 0.5 indicates better-than-random discrimination",
    x = NULL,
    y = "AUC",
    colour = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16)
  )

ggsave(
  "walk_forward_auc.png",
  width = 10,
  height = 6,
  dpi = 300
)

# Average walk-forward performance
average_walk_forward <- data.frame( Model = c( "Logistic Regression", "XGBoost" ),
  
  Accuracy = c(
    mean(walk_forward_results$Logistic_Accuracy),
    mean(walk_forward_results$XGB_Accuracy)
  ),
  
  AUC = c(
    mean(walk_forward_results$Logistic_AUC),
    mean(walk_forward_results$XGB_AUC)
  ),
  
  Brier = c(
    mean(walk_forward_results$Logistic_Brier),
    mean(walk_forward_results$XGB_Brier)
  )
)

average_walk_forward[, 2:4] <- round(
  average_walk_forward[, 2:4],
  3)

average_walk_forward

# XGBoost tuning grid
tuning_grid <- expand.grid(
  max_depth = c(2, 3, 4),  # tree depth
  learning_rate = c(0.01, 0.05, 0.1),  # learning step size
  nrounds = c(50, 100, 200)  # number of boosting rounds
)

tuning_results <- data.frame()  # store tuning results

## Test each parameter combination
for (i in 1:nrow(tuning_grid)) {
  
  depth <- tuning_grid$max_depth[i]  # current depth
  rate <- tuning_grid$learning_rate[i]  # current learning rate
  rounds <- tuning_grid$nrounds[i]  # current number of rounds
  
  fold_AUC <- c()  # store yearly AUC values
  fold_Brier <- c()  # store yearly Brier scores
  
  ##Walk-forward validation for current combination
  
  for (year in validation_years) {
    
    train_fold <- development_vol %>%
      filter(Date < as.Date(paste0(year, "-01-01")))
    
    validation_fold <- development_vol %>%
      filter(
        Date >= as.Date(paste0(year, "-01-01")),
        Date < as.Date(paste0(year + 1, "-01-01"))
      )
    
    
    # Remove final training rows
    train_fold <- train_fold[
      1:(nrow(train_fold) - 5),  # prevent target crossing validation boundary
    ]
    
    # Prepare XGBoost data
    x_train_fold <- as.matrix(train_fold[, predictors])
    
    x_validation_fold <- as.matrix(validation_fold[, predictors])
    
    y_train_fold <- factor(train_fold$HighVol,
      levels = c(0, 1))  # binary classes
    
    ## Fit XGBoost
    xgb_tuned_fold <- xgboost(
      x = x_train_fold,
      y = y_train_fold,
      nrounds = rounds,  # current boosting rounds
      max_depth = depth,  # current tree depth
      learning_rate = rate,  # current learning rate
      verbose = 0  # hide progress messages
    )
    
    # Validation probabilities
    prob_tuned <- predict(
      xgb_tuned_fold,
      x_validation_fold,
      type = "response"
    )
    
    ## AUC
    auc_tuned <- as.numeric(
      pROC::auc(
        validation_fold$HighVol,
        prob_tuned)
    )
    
    #Brier score
    brier_tuned <- mean((prob_tuned - validation_fold$HighVol)^2)
    
    ## Store results for this year
    fold_AUC <- c(fold_AUC,auc_tuned)
    
    fold_Brier <- c(fold_Brier,brier_tuned)
  }
  
  # Average across validation years
  combination_results <- data.frame(
    max_depth = depth,
    learning_rate = rate,
    nrounds = rounds,
    Mean_AUC = mean(fold_AUC),
    Mean_Brier = mean(fold_Brier))
  
  ##Add to tuning table
  tuning_results <- rbind(
    tuning_results,
    combination_results)
}

# Round tuning results
tuning_results$Mean_AUC <- round(
  tuning_results$Mean_AUC, 3)

tuning_results$Mean_Brier <- round(
  tuning_results$Mean_Brier, 3)

# Rank parameter combinations
tuning_results <- tuning_results %>%
  arrange(
    desc(Mean_AUC))  # highest AUC first

tuning_results

# Final test data
final_train <- vol_model_data %>%
  filter(Date < as.Date("2024-01-01"))

final_test <- vol_model_data %>%
  filter(Date >= as.Date("2024-01-01"))

## Remove final training rows
final_train <- final_train[
  1:(nrow(final_train) - 5),
]

# Check final test class balance
table(final_test$HighVol)

prop.table(table(final_test$HighVol))

## Final naive baseline
final_naive_accuracy <- max(
  prop.table(table(final_test$HighVol))
)

final_naive_accuracy

#Final Logistic Regression
final_logistic <- glm(
  HighVol ~ Return + Volatility20 + RSI14,
  data = final_train,
  family = binomial
)

# Logistic test probabilities
final_test$Prob_Logistic <- predict(
  final_logistic,
  newdata = final_test,
  type = "response"
)

## Logistic test classes
final_test$Pred_Logistic <- ifelse(final_test$Prob_Logistic > 0.5, 1, 0)

## Logistic confusion matrix

table(
  Actual = final_test$HighVol,
  Predicted = final_test$Pred_Logistic
)

# Logistic final accuracy
final_accuracy_logistic <- mean(final_test$Pred_Logistic == final_test$HighVol)

final_accuracy_logistic

## Logistic final AUC
final_auc_logistic <- as.numeric(
  pROC::auc(
    final_test$HighVol,
    final_test$Prob_Logistic
  )
)

final_auc_logistic

# Logistic final Brier score
final_brier_logistic <- mean(
  (final_test$Prob_Logistic - final_test$HighVol)^2)

final_brier_logistic

## Prepare final XGBoost data
x_final_train <- as.matrix(
  final_train[, predictors])

x_final_test <- as.matrix(
  final_test[, predictors])

y_final_train <- factor(
  final_train$HighVol,
  levels = c(0, 1))


## Check final training and test data

dim(x_final_train)
dim(x_final_test)

table(y_final_train)
table(final_test$HighVol)

colSums(x_final_train)
colSums(x_final_test)

## Final tuned XGBoost model
set.seed(123)  # ensures the final XGBoost fit is reproducible

final_xgb <- xgboost(
  x = x_final_train,
  y = y_final_train,
  nrounds = 200,  # selected through tuning
  max_depth = 3,  # selected through tuning
  learning_rate = 0.05,  # selected through tuning
  objective = "binary:logistic",  # predict probabilities for a binary outcome
  seed = 123,  # fix XGBoost's own random seed
  nthread = 1,  # use one thread for more reproducible results
  verbose = 0 #dont print progress messages
)
# XGBoost test probabilities
final_test$Prob_XGB <- predict(
  final_xgb,
  x_final_test,
  type = "response")

## XGBoost test classes
final_test$Pred_XGB <- ifelse(final_test$Prob_XGB > 0.5, 1, 0)

## XGBoost confusion matrix
table(
  Actual = final_test$HighVol,
  Predicted = final_test$Pred_XGB)

## XGBoost final accuracy
final_accuracy_XGB <- mean( final_test$Pred_XGB == final_test$HighVol)

final_accuracy_XGB

## XGBoost final AUC
final_auc_XGB <- as.numeric(
  pROC::auc(
    final_test$HighVol,
    final_test$Prob_XGB)
)

final_auc_XGB

# XGBoost final Brier score
final_brier_XGB <- mean((final_test$Prob_XGB - final_test$HighVol)^2)

final_brier_XGB

## Final model comparison
final_results <- data.frame(
  Model = c("Logistic Regression", "Tuned XGBoost", "Naive Baseline"),
  
  Accuracy = c(
    final_accuracy_logistic,
    final_accuracy_XGB,
    final_naive_accuracy
  ),
  
  AUC = c(
    final_auc_logistic,
    final_auc_XGB,
    NA),
  
  Brier = c(
    final_brier_logistic,
    final_brier_XGB,
    NA)
)

final_results[, 2:4] <- round(
  final_results[, 2:4], 3)

final_results

## Final test ROC curve
final_roc_logistic <- pROC::roc(
  final_test$HighVol,
  final_test$Prob_Logistic
)

final_roc_XGB <- pROC::roc(
  final_test$HighVol,
  final_test$Prob_XGB
)

png(
  "final_test_roc.png",
  width = 2400,
  height = 2100,
  res = 300
)

plot(
  final_roc_logistic,
  col = "blue",
  lwd = 2,
  main = "ROC Curves for Final Test Period")

plot(
  final_roc_XGB,
  col = "red",
  lwd = 2,
  add = TRUE)

legend(
  "bottomright",
  legend = c(
    paste(
      "Logistic AUC =",
      round(final_auc_logistic, 3)
    ),
    paste(
      "XGBoost AUC =",
      round(final_auc_XGB, 3)
    )
  ),
  col = c("blue", "red"),
  lty = 1,
  lwd = 2)

dev.off()

#tables
walk_forward_results
yearly_baseline
average_walk_forward
tuning_results
final_results

## Save results for Shiny dashboard
saveRDS(
  final_test,
  "final_test.rds"
)  # final 2024-2025 observations and predictions