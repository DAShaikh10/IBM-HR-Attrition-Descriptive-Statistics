# ============================================================
# Statistical Computing Project
# Dataset: IBM HR Employee Attrition
# OVER-COMPLETE VERSION (prune later)
# ============================================================

# ---- 1. Load data ----
hr <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv",
               stringsAsFactors = FALSE)

# ---- 2. Select variables ----
hr_sub <- hr[, c(
  "Attrition",
  "OverTime",
  "JobLevel",
  "Age",
  "YearsAtCompany",
  "MonthlyIncome"
)]

# ---- 3. Variable types ----
hr_sub$Attrition <- factor(hr_sub$Attrition)
hr_sub$OverTime  <- factor(hr_sub$OverTime)

hr_sub$JobLevel <- factor(hr_sub$JobLevel,
                          levels = 1:5,
                          ordered = TRUE)

hr_sub$Age <- as.numeric(hr_sub$Age)
hr_sub$YearsAtCompany <- as.numeric(hr_sub$YearsAtCompany)
hr_sub$MonthlyIncome <- as.numeric(hr_sub$MonthlyIncome)

# ============================================================
# 4. UNIVARIATE ANALYSIS
# ============================================================

# ---- Frequency tables (categorical / ordinal) ----
table(hr_sub$Attrition)
prop.table(table(hr_sub$Attrition))

table(hr_sub$OverTime)
prop.table(table(hr_sub$OverTime))

joblevel_freq <- table(hr_sub$JobLevel)
joblevel_freq
prop.table(joblevel_freq)

# ---- Cumulative absolute & relative frequency (ordinal only) ----
cumsum(joblevel_freq)
cumsum(prop.table(joblevel_freq))

# ---- Histogram (numeric) ----
hist(hr_sub$Age,
     col = "lightgray",
     main = "Histogram of Age",
     xlab = "Age")

hist(hr_sub$YearsAtCompany,
     col = "lightgray",
     main = "Histogram of Years at Company",
     xlab = "Years at Company")

hist(hr_sub$MonthlyIncome,
     col = "lightgray",
     main = "Histogram of Monthly Income",
     xlab = "Monthly Income")

# ---- Mean & Median (numeric) ----
mean(hr_sub$Age)
median(hr_sub$Age)

mean(hr_sub$YearsAtCompany)
median(hr_sub$YearsAtCompany)

mean(hr_sub$MonthlyIncome)
median(hr_sub$MonthlyIncome)

# ---- Mode (discrete / ordinal) ----
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

get_mode(hr_sub$JobLevel)
get_mode(hr_sub$Attrition)
get_mode(hr_sub$OverTime)

# ---- Quantiles / Quartiles / Deciles / Percentiles ----
quantile(hr_sub$Age)
quantile(hr_sub$Age, probs = seq(0,1,0.1))

quantile(hr_sub$YearsAtCompany)
quantile(hr_sub$YearsAtCompany, probs = c(0.1, 0.9))

quantile(hr_sub$MonthlyIncome)
quantile(hr_sub$MonthlyIncome, probs = seq(0,1,0.1))

# ---- Boxplots ----
boxplot(hr_sub$Age,
        col = "lightgray",
        main = "Boxplot of Age")

boxplot(hr_sub$YearsAtCompany,
        col = "lightgray",
        main = "Boxplot of Years at Company")

boxplot(hr_sub$MonthlyIncome,
        col = "lightgray",
        main = "Boxplot of Monthly Income")

# ---- ECDF ----
plot(ecdf(hr_sub$Age),
     main = "ECDF of Age",
     xlab = "Age",
     ylab = "ECDF")

plot(ecdf(hr_sub$YearsAtCompany),
     main = "ECDF of Years at Company",
     xlab = "Years at Company",
     ylab = "ECDF")

plot(ecdf(hr_sub$MonthlyIncome),
     main = "ECDF of Monthly Income",
     xlab = "Monthly Income",
     ylab = "ECDF")

# ---- Dispersion measures ----
var(hr_sub$Age)
sd(hr_sub$Age)
sd(hr_sub$Age) / mean(hr_sub$Age)
range(hr_sub$Age)
IQR(hr_sub$Age)

var(hr_sub$YearsAtCompany)
sd(hr_sub$YearsAtCompany)
sd(hr_sub$YearsAtCompany) / mean(hr_sub$YearsAtCompany)
range(hr_sub$YearsAtCompany)
IQR(hr_sub$YearsAtCompany)

var(hr_sub$MonthlyIncome)
sd(hr_sub$MonthlyIncome)
sd(hr_sub$MonthlyIncome) / mean(hr_sub$MonthlyIncome)
range(hr_sub$MonthlyIncome)
IQR(hr_sub$MonthlyIncome)

# ============================================================
# 5. BIVARIATE ANALYSIS
# ============================================================

# ---- Categorical × Categorical ----
attr_ot_tab <- table(hr_sub$Attrition, hr_sub$OverTime)
attr_ot_tab

# Marginal frequencies
margin.table(attr_ot_tab, 1)
margin.table(attr_ot_tab, 2)

# Row-wise & column-wise relative frequencies
prop.table(attr_ot_tab, margin = 1)
prop.table(attr_ot_tab, margin = 2)

# Grouped bar chart
barplot(attr_ot_tab,
        beside = TRUE,
        legend = TRUE,
        main = "Attrition by OverTime",
        xlab = "Attrition",
        ylab = "Frequency")

# ---- Categorical × Numeric ----
boxplot(Age ~ Attrition,
        data = hr_sub,
        main = "Age by Attrition")

boxplot(MonthlyIncome ~ Attrition,
        data = hr_sub,
        main = "Monthly Income by Attrition")

# ---- Numeric × Numeric ----
plot(hr_sub$Age, hr_sub$MonthlyIncome,
     main = "Monthly Income vs Age",
     xlab = "Age",
     ylab = "Monthly Income")

plot(hr_sub$YearsAtCompany, hr_sub$MonthlyIncome,
     main = "Monthly Income vs Years at Company",
     xlab = "Years at Company",
     ylab = "Monthly Income")

# ---- Covariance ----
cov(hr_sub$Age, hr_sub$MonthlyIncome)
cov(hr_sub$YearsAtCompany, hr_sub$MonthlyIncome)

# ---- Correlation ----
cor(hr_sub$Age, hr_sub$MonthlyIncome)               # Pearson
cor(hr_sub$Age, hr_sub$MonthlyIncome, method="spearman")

cor(hr_sub$YearsAtCompany, hr_sub$MonthlyIncome)
cor(hr_sub$YearsAtCompany, hr_sub$MonthlyIncome, method="spearman")

# ============================================================
# 6. LINEAR REGRESSION (SIMPLE, DESCRIPTIVE)
# ============================================================

lm_income <- lm(MonthlyIncome ~ YearsAtCompany,
                data = hr_sub)

summary(lm_income)

# Regression plot
plot(hr_sub$YearsAtCompany, hr_sub$MonthlyIncome,
     main = "Linear Regression: Income vs Years at Company",
     xlab = "Years at Company",
     ylab = "Monthly Income")

abline(lm_income, col = "red", lwd = 2)

# ---- Prediction ----
predict(lm_income,
        newdata = data.frame(YearsAtCompany = c(2, 5, 10)))

# ---- Goodness of fit ----
summary(lm_income)$r.squared

