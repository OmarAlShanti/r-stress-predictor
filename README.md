# Stress Level Predictor (R)

Logistic regression model that predicts high vs. low stress from self-reported
survey features — built for my Predictive Analytics course (BUAN 6337, UT Dallas).
**R end to end**: EDA, correlation analysis, feature scaling, binary classification,
and model interpretation.

## Why this project

R is the language my graduate analytics curriculum was built on, and this project
shows the full analytical workflow: understand the data, engineer the target,
fit the model, and read the coefficients back into business language. The same
discipline powers the dashboards and retention models I built at Floori.

## Data

`data/StressLevelDataset.csv` — public stress-level survey dataset (psychological,
physiological, and lifestyle features; target = stress level 0-5, binarized at the mean).

## Approach

1. **Clean**: dedupe, drop nulls, convert categoricals, scale numeric features
2. **Explore**: correlation matrix — which features move with stress?
3. **Model**: binary logistic regression (high/low stress)
4. **Interpret**: coefficients → which factors push someone into the high-stress bucket

## Run it

```r
# In RStudio / R:
setwd("r-stress-predictor")
source("scripts/exploration.R")          # EDA + correlations
source("scripts/stress_logistic_regression.R")  # model + interpretation
```

## Files

```
├── data/
│   └── StressLevelDataset.csv
├── scripts/
│   ├── exploration.R                   # EDA, scaling, correlations
│   └── stress_logistic_regression.R    # target binarization + logistic model
└── README.md
```

## Skills demonstrated

R (tidyverse, dplyr, ggplot2) · EDA · correlation analysis · logistic regression ·
feature engineering · translating model output into business insight
