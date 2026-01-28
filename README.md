# Statistical Computing - WiSe '25

Semester - I - MSc Data Science  
Berliner Hochschule für Technik  
Course taught by: [Prof. Dr. Steffan Wagner](https://prof.bht-berlin.de/wagner)

## Project Overview

This project performs a comprehensive descriptive statistical analysis of the **IBM HR Employee Attrition Dataset**, examining workforce demographics, financial structure, and employee sentiment to identify key factors associated with turnover.

### Dataset

- **Source**: IBM HR Employee Attrition Dataset
- **Observations**: 1,470 employees
- **Selected Variables**: 7 key attributes including:
  - Categorical: Attrition, OverTime, JobLevel, JobSatisfaction
  - Numeric: Age, MonthlyIncome, YearsAtCompany
- **URL**: https://www.kaggle.com/datasets/pavansubhasht/ibm-hr-analytics-attrition-dataset/data

### Analysis Summary

**Univariate Analysis** characterizes individual variables through:

- Frequency distributions for categorical variables (Attrition, JobSatisfaction, JobLevel, OverTime)
- Descriptive statistics (mean, median, variance, IQR) for continuous variables (Age, MonthlyIncome, YearsAtCompany)
- Distributional visualizations including histograms, boxplots, and ECDFs

**Bivariate Analysis** investigates relationships between variables:

- Contingency tables examining Job Satisfaction vs. Attrition
- Group-wise comparisons of Monthly Income, Age, and Tenure by Attrition status
- Correlation analysis between Years at Company and Monthly Income

**Linear Regression** models the relationship between tenure and compensation, estimating the financial premium associated with years of service ($R^2 = 0.26$, $p < 0.001$).

### Key Findings

- Attrition exhibits significant class imbalance (16.12% turnover rate)
- Lower job satisfaction, reduced income, younger age, and shorter tenure are associated with higher attrition
- Income and tenure show right-skewed distributions with substantial outliers
- Tenure explains approximately 26% of income variance, with an estimated increase of \$395 per year
