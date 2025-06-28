# 🗺️ Spatial vs. Machine Learning for Unemployment Analysis in Java

![Python](https://img.shields.io/badge/Python-3776AB?style=for-the-badge&logo=python&logoColor=white)
![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![Scikit-learn](https://img.shields.io/badge/scikit--learn-%23F7931E.svg?style=for-the-badge&logo=scikit-learn&logoColor=white)
![Spatial Analysis](https://img.shields.io/badge/Spatial-Analysis-8A2BE2)

## 📖 Overview
The Open Unemployment Rate (TPT) is a critical socio-economic issue in Indonesia, with significant regional disparities, especially across Java Island. Traditional regression models often fail to capture the complex, non-linear, and spatial nature of unemployment determinants. This project addresses this gap by providing a comprehensive comparison between traditional spatial econometric models and modern machine learning algorithms to determine the most effective approach for predicting TPT and identifying its key drivers.

## 📊 Dataset
* **Region:** 118 regencies and cities across Java Island, Indonesia.
* **Year:** 2023.
* **Source:** Official data from **BPS (Statistics Indonesia)**.
* **Dependent Variable:** `TPT` (Open Unemployment Rate, %).
* **Independent Variables:** `TPAK` (Labor Force Participation Rate), `UHH` (Life Expectancy at Birth), `HLS` (Expected Years of Schooling), `PKD` (Adjusted Per Capita Expenditure), and `PPM` (Poverty Percentage).

## ⚙️ Methodology: A Tale of Two Approaches
This study directly compares the performance of two distinct modeling paradigms to tackle the same problem.

#### Approach 1: Spatial Econometric Modeling (in R)
1.  **Baseline OLS Model:** An initial OLS model was built. Diagnostic tests confirmed the presence of significant spatial autocorrelation in the residuals (**Moran's I = 0.2542, p < 0.05**), validating the need for spatial models.
2.  **Model Specification:** Based on Lagrange Multiplier tests, three spatial regression models were developed:
    * Spatial Autoregressive Model (SAR)
    * Spatial Error Model (SEM)
    * Spatial Autoregressive Moving Average (SARMA)

#### Approach 2: Machine Learning Modeling (in Python)
1.  **Model Suite:** Four different ML algorithms were implemented: Decision Tree, Support Vector Regression (SVR), Gradient Boosting Machine (GBM), and **Random Forest**.
2.  **Spatial Feature Engineering:** To make the models spatially aware, key spatial features were added as predictors: the **geographic coordinates** (latitude & longitude) and a **spatially lagged TPT variable**.
3.  **Optimization:** The models were optimized using **hyperparameter tuning** and validated with **5-fold cross-validation**.

## 🏆 Model Performance & Comparison
After a comprehensive evaluation, the machine learning approach, specifically the tuned Random Forest model, demonstrated vastly superior predictive performance.

| Model | R² (Coefficient of Determination) | RMSE | MAE | AIC |
|---|---|---|---|---|
| OLS (Baseline) | 0.521 | 1.403 | 1.115 | 425.24 |
| SARMA (Best Spatial Model) | 0.658 | 1.094 | 0.931 | 401.10 |
| **Random Forest (CV-Tuned)** | **0.926** | **0.551** | **0.423** | N/A |

> **Key Result:** The optimized Random Forest model **(R² = 0.926)** significantly outperformed the best spatial regression model, SARMA (R² = 0.658), proving its effectiveness in capturing the complex, non-linear, and spatial relationships in the data.

## 🔑 Key Findings & Variable Importance
The variable importance analysis from the winning Random Forest model revealed the most crucial predictors for unemployment in Java:

1.  **`lag_TPT` (Spatial Lag of Unemployment):** This was the **most important predictor**, confirming that unemployment in one region is strongly influenced by the rates in its neighboring regions (spatial spillover effect).
2.  **`TPAK` (Labor Force Participation Rate):** The second most important factor, indicating a strong relationship between labor market engagement and unemployment.
3.  **`lon` (Longitude):** The inclusion of a spatial coordinate as a key predictor highlights an East-West economic disparity across the island that influences employment outcomes.

![image](https://github.com/user-attachments/assets/c265c70d-c0ca-427d-b803-da9443217cf7)
