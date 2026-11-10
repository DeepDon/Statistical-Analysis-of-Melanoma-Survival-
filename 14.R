# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(car)
library(stats)

# Load the dataset
data <- read.csv("C://Users//Deeps//Desktop//melanoma.csv")

# Check structure of the data
str(data)

# ----------------------
# 1. Summary Statistics
# ----------------------
# Summary statistics for all variables
summary_stats <- data %>%
  summarise(
    time_mean = mean(time, na.rm = TRUE),
    time_sd = sd(time, na.rm = TRUE),
    thickness_mean = mean(thickness, na.rm = TRUE),
    thickness_sd = sd(thickness, na.rm = TRUE),
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    ulcer_present = sum(ulcer == 1),
    ulcer_absent = sum(ulcer == 0),
    male_count = sum(sex == 1),
    female_count = sum(sex == 0)
  )

print(summary_stats)


# ----------------------
# 2. Graphical Summaries
# ----------------------
# Histograms for numeric variables
hist_time <- ggplot(data, aes(x = time)) +
  geom_histogram(binwidth = 200, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Survival Time", x = "Survival Time (days)", y = "Frequency")

hist_thickness <- ggplot(data, aes(x = thickness)) +
  geom_histogram(binwidth = 0.5, fill = "darkgreen", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Tumor Thickness", x = "Tumor Thickness (mm)", y = "Frequency")

hist_age <- ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "darkred", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Age", x = "Age (years)", y = "Frequency")

# Boxplots by gender
boxplot_time_gender <- ggplot(data, aes(x = factor(sex, labels = c("Female", "Male")), y = time)) +
  geom_boxplot(fill = c("pink", "lightblue")) +
  theme_minimal() +
  labs(title = "Survival Time by Gender", x = "Gender", y = "Survival Time (days)")

boxplot_thickness_gender <- ggplot(data, aes(x = factor(sex, labels = c("Female", "Male")), y = thickness)) +
  geom_boxplot(fill = c("pink", "lightblue")) +
  theme_minimal() +
  labs(title = "Tumor Thickness by Gender", x = "Gender", y = "Tumor Thickness (mm)")

# ----------------------
# 3. Regression Analysis
# ----------------------
# Linear models
lm_time_thickness <- lm(time ~ thickness, data = data)
lm_time_age <- lm(time ~ age, data = data)
lm_thickness_age <- lm(thickness ~ age, data = data)

# Summary of models
summary(lm_time_thickness)
summary(lm_time_age)
summary(lm_thickness_age)

# Correlation coefficients
cor_time_thickness <- cor(data$time, data$thickness, use = "complete.obs")
cor_time_age <- cor(data$time, data$age, use = "complete.obs")
cor_thickness_age <- cor(data$thickness, data$age, use = "complete.obs")

print(cor_time_thickness)
print(cor_time_age)
print(cor_thickness_age)

# ----------------------
# 4. Significance Tests
# ----------------------
# t-tests by gender
time_gender_ttest <- t.test(time ~ sex, data = data)
thickness_gender_ttest <- t.test(thickness ~ sex, data = data)
age_gender_ttest <- t.test(age ~ sex, data = data)

print(time_gender_ttest)
print(thickness_gender_ttest)
print(age_gender_ttest)

# ----------------------
# 5. QQ-Plots
# ----------------------
qq_time <- qqPlot(data$time, main = "QQ-Plot of Survival Time")
qq_thickness <- qqPlot(data$thickness, main = "QQ-Plot of Tumor Thickness")
qq_age <- qqPlot(data$age, main = "QQ-Plot of Age")

# ----------------------
# Save plots to file
# ----------------------
ggsave("hist_time.png", plot = hist_time, width = 6, height = 4)
ggsave("hist_thickness.png", plot = hist_thickness, width = 6, height = 4)
ggsave("hist_age.png", plot = hist_age, width = 6, height = 4)
ggsave("boxplot_time_gender.png", plot = boxplot_time_gender, width = 6, height = 4)
ggsave("boxplot_thickness_gender.png", plot = boxplot_thickness_gender, width = 6, height = 4)
