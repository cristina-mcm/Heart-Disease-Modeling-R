# Heart Disease Modeling in R

## Project Overview

This project examines relationships between patient health indicators and the recorded presence of heart disease using logistic regression and random forest models in R. Two logistic regression models were developed to estimate the probability of heart disease. A classification random forest was used to predict heart disease status, while a regression random forest was used to predict maximum heart rate achieved. The project demonstrates statistical modeling, model interpretation, classification evaluation, and regression evaluation in R.

## Analysis

The project includes four models:

1. A logistic regression model using age, resting blood pressure, exercise-induced angina, and maximum heart rate.

2. A logistic regression model incorporating chest pain categories, a quadratic age term, and an interaction between age and maximum heart rate.

3. A classification random forest predicting heart disease status.

4. A regression random forest predicting maximum heart rate achieved.

The models were evaluated using:

- Wald tests
- Hosmer–Lemeshow goodness-of-fit tests
- Confusion matrices
- ROC curves and AUC
- Root mean squared error

## Key Findings

### Logistic Regression Model 1

- Accuracy: 73.60%
- Precision: 73.22%
- Recall: 81.21%
- AUC: 0.8007

### Logistic Regression Model 2

- Accuracy: 76.24%
- Precision: 78.18%
- Recall: 78.18%
- AUC: 0.8478

Model 2 performed better on the existing dataset. However, both logistic regression models were evaluated using the same observations used to build them, so these results may overestimate performance on new data.

### Classification Random Forest

- Training accuracy: 99.22%
- Validation accuracy: 67.39%
- Validation precision: 74.07%
- Validation recall: 71.43%

The difference between training and validation performance suggests that the model did not generalize as well as its training results implied.

### Regression Random Forest

- Training RMSE: 11.78 beats per minute
- Validation RMSE: 18.73 beats per minute

The higher validation error indicates weaker performance on unseen observations and possible overfitting.

The logistic regression and random forest classification results should not be compared directly because they were evaluated using different methods.

## Repository Files
- [`heart_disease_code.r`](heart_disease_analysis.r) - R code used for data preparation, visualization, regression modeling, diagnostics, predictions, and model comparison
- [`heart_disease_report.pdf`](heart_disease_report.pdf) - Technical summary report containing the analysis, results, visualizations, interpretations, and limitations
- [`heart_disease_notebook.html`](heart_disease_notebook.html) = [`heart_disease_notebook.pdf`](heart_disease_notebook.pdf)- HTML notebook and its associated PDF preserving the complete code and original outputs

## Tools

- R
- Logistic regression
- Classification random forests
- Regression random forests
- Quadratic terms and interaction effects
- Wald hypothesis testing
- Confusion-matrix evaluation
- ROC curves and AUC
- RMSE

## Acknowledgment
This project was originally completed as part of an academic course. The R code, analysis, interpretations, and portfolio revisions are my own.
