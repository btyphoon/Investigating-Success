# Investigating-Success
project 2

---
title: "Project 2: Investigating Student Success"
author: "Basay Tayfun, Ethan Terrill, Isaac Kim"
date: "`r Sys.Date()`"
output:
  html_document:
    code_folding: hide
    number_sections: true
    toc: yes
    toc_depth: 3
    toc_float: yes
---

```{r init, include=FALSE}
library(ezids)
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(warning = FALSE, message = FALSE, fig.width = 8, fig.height = 5)
options(scientific = TRUE, digits = 3)
```

# Introduction

Our project examines how student behavioral and environmental factors relate to academic performance. Using the "Student Performance Dataset" from Kaggle (6,607 observations), we analyze Final Exam Score as a continuous dependent variable and a derived Pass/Fail outcome as a binary dependent variable. Each observation represents a student, with 10 independent variables spanning study habits, demographics, and resource access.

SMART Questions:

1. Which combination of behavioral and environmental factors most strongly predicts Final Exam Score, and how much variance do they explain?
2. After categorizing students as passing or failing, can we accurately identify at-risk students, and which factors are the strongest predictors of failure?

# Data Loading and Inspection

```{r load_data}
students <- read.csv("student_dataset.csv", header = TRUE)
str(students)
```

The dataset has `r nrow(students)` observations and `r ncol(students)` variables.

```{r preview, results='asis'}
xkabledplyhead(students, title = "First rows of the Student Performance dataset")
```

## Variable Types and Cleaning

```{r check_missing}
# Check for missing values
missing_counts <- colSums(is.na(students))
missing_counts[missing_counts > 0]
cat("Total missing values:", sum(is.na(students)), "\n")
```

```{r clean}
# Convert categorical variables to factors
students$Parental_Involvement <- factor(students$Parental_Involvement,
                                         levels = c("Low", "Medium", "High"))
students$Access_to_Resources  <- factor(students$Access_to_Resources,
                                         levels = c("Low", "Medium", "High"))
students$Motivation_Level     <- factor(students$Motivation_Level,
                                         levels = c("Low", "Medium", "High"))
students$Internet_Access           <- factor(students$Internet_Access)
students$Extracurricular_Activities <- factor(students$Extracurricular_Activities)

str(students)
```

We ordered the three-level factors (Parental Involvement, Access to Resources, Motivation Level) as Low < Medium < High so that tables and plots display in a logical order. Internet Access and Extracurricular Activities are binary (Yes/No).

## Summary Statistics

```{r summary_stats, results='asis'}
numeric_vars <- students[, c("Hours_Studied", "Attendance", "Sleep_Hours",
                              "Previous_Scores", "Tutoring_Sessions", "Final_Exam_Score")]
xkablesummary(numeric_vars, title = "Summary statistics for numeric variables")
```

```{r cat_summaries}
cat("Parental Involvement:\n")
table(students$Parental_Involvement)
cat("\nAccess to Resources:\n")
table(students$Access_to_Resources)
cat("\nMotivation Level:\n")
table(students$Motivation_Level)
cat("\nInternet Access:\n")
table(students$Internet_Access)
cat("\nExtracurricular Activities:\n")
table(students$Extracurricular_Activities)
```

# Exploratory Data Analysis

## Distribution of Final Exam Score (Dependent Variable)

```{r dv_hist}
library(ggplot2)

ggplot(students, aes(x = Final_Exam_Score)) +
  geom_histogram(binwidth = 2, fill = "steelblue", color = "white", alpha = 0.7) +
  labs(title = "Distribution of Final Exam Score",
       x = "Final Exam Score", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r dv_boxplot}
ggplot(students, aes(y = Final_Exam_Score)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
  labs(title = "Boxplot of Final Exam Score", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r dv_stats}
cat("Mean:", round(mean(students$Final_Exam_Score), 2), "\n")
cat("Median:", round(median(students$Final_Exam_Score), 2), "\n")
cat("SD:", round(sd(students$Final_Exam_Score), 2), "\n")
cat("Min:", min(students$Final_Exam_Score), "\n")
cat("Max:", max(students$Final_Exam_Score), "\n")
```

## Distributions of Numeric Independent Variables

```{r numeric_hists, fig.width=10, fig.height=8}
library(tidyr)

numeric_long <- pivot_longer(students[, c("Hours_Studied", "Attendance", "Sleep_Hours",
                                           "Previous_Scores", "Tutoring_Sessions")],
                              cols = everything(),
                              names_to = "variable", values_to = "value")

ggplot(numeric_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Distributions of Numeric Independent Variables",
       x = "Value", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r numeric_boxplots, fig.width=10, fig.height=8}
ggplot(numeric_long, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 8) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Boxplots of Numeric Independent Variables",
       x = "", y = "Value") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none",
        axis.text.x = element_blank())
```

## Outlier Assessment

```{r outlier_hours}
outlierKD2(students, Hours_Studied, rm = FALSE, boxplt = TRUE, qqplt = FALSE)
```

```{r outlier_attendance}
outlierKD2(students, Attendance, rm = FALSE, boxplt = TRUE, qqplt = FALSE)
```

```{r outlier_sleep}
outlierKD2(students, Sleep_Hours, rm = FALSE, boxplt = TRUE, qqplt = FALSE)
```

```{r outlier_previous}
outlierKD2(students, Previous_Scores, rm = FALSE, boxplt = TRUE, qqplt = FALSE)
```

```{r outlier_tutoring}
outlierKD2(students, Tutoring_Sessions, rm = FALSE, boxplt = TRUE, qqplt = FALSE)
```

```{r outlier_final}
outlierKD2(students, Final_Exam_Score, rm = FALSE, boxplt = TRUE, qqplt = FALSE)
```

## Distributions of Categorical Variables

```{r cat_barplots, fig.width=10, fig.height=8}
library(dplyr)

p1 <- ggplot(students, aes(x = Parental_Involvement, fill = Parental_Involvement)) +
  geom_bar(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Parental Involvement", x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

p2 <- ggplot(students, aes(x = Access_to_Resources, fill = Access_to_Resources)) +
  geom_bar(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Access to Resources", x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

p3 <- ggplot(students, aes(x = Motivation_Level, fill = Motivation_Level)) +
  geom_bar(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Motivation Level", x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

p4 <- ggplot(students, aes(x = Internet_Access, fill = Internet_Access)) +
  geom_bar(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Internet Access", x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

p5 <- ggplot(students, aes(x = Extracurricular_Activities, fill = Extracurricular_Activities)) +
  geom_bar(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Extracurricular Activities", x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

loadPkg("gridExtra")
grid.arrange(p1, p2, p3, p4, p5, ncol = 3)
```

## Scatterplots: Numeric IVs vs. Final Exam Score

```{r scatter_hours}
ggplot(students, aes(x = Hours_Studied, y = Final_Exam_Score)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Hours Studied vs. Final Exam Score",
       x = "Hours Studied", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r scatter_attendance}
ggplot(students, aes(x = Attendance, y = Final_Exam_Score)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Attendance vs. Final Exam Score",
       x = "Attendance (%)", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r scatter_sleep}
ggplot(students, aes(x = Sleep_Hours, y = Final_Exam_Score)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Sleep Hours vs. Final Exam Score",
       x = "Sleep Hours", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r scatter_previous}
ggplot(students, aes(x = Previous_Scores, y = Final_Exam_Score)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Previous Scores vs. Final Exam Score",
       x = "Previous Scores", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r scatter_tutoring}
ggplot(students, aes(x = Tutoring_Sessions, y = Final_Exam_Score)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Tutoring Sessions vs. Final Exam Score",
       x = "Tutoring Sessions", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

## Boxplots: Final Exam Score by Categorical Variables

```{r box_parental, fig.width=10, fig.height=5}
ggplot(students, aes(x = Parental_Involvement, y = Final_Exam_Score, fill = Parental_Involvement)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Final Exam Score by Parental Involvement",
       x = "Parental Involvement", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

```{r box_resources}
ggplot(students, aes(x = Access_to_Resources, y = Final_Exam_Score, fill = Access_to_Resources)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Final Exam Score by Access to Resources",
       x = "Access to Resources", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

```{r box_motivation}
ggplot(students, aes(x = Motivation_Level, y = Final_Exam_Score, fill = Motivation_Level)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Final Exam Score by Motivation Level",
       x = "Motivation Level", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

```{r box_internet}
ggplot(students, aes(x = Internet_Access, y = Final_Exam_Score, fill = Internet_Access)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Final Exam Score by Internet Access",
       x = "Internet Access", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

```{r box_extracurricular}
ggplot(students, aes(x = Extracurricular_Activities, y = Final_Exam_Score,
                      fill = Extracurricular_Activities)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 8) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Final Exam Score by Extracurricular Activities",
       x = "Extracurricular Activities", y = "Final Exam Score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

## Correlation Matrix (Numeric Variables)

```{r correlation, fig.width=8, fig.height=6}
loadPkg("corrplot")

cor_vars <- c("Hours_Studied", "Attendance", "Sleep_Hours", "Previous_Scores",
              "Tutoring_Sessions", "Final_Exam_Score")
cor_matrix <- cor(students[, cor_vars], use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "lower",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.7,
         title = "Correlation Matrix: Numeric Variables",
         mar = c(0, 0, 2, 0))
```

# Statistical Testing

## T-Tests: Binary Categorical Variables vs. Final Exam Score

We use two-sample t-tests to compare mean Final Exam Score between the two groups for each binary variable. H0: the means are equal; Ha: the means differ.

### Internet Access

```{r ttest_internet}
internet_yes <- subset(students, Internet_Access == "Yes")$Final_Exam_Score
internet_no  <- subset(students, Internet_Access == "No")$Final_Exam_Score

cat("Mean score (Internet Yes):", round(mean(internet_yes), 2), "\n")
cat("Mean score (Internet No):", round(mean(internet_no), 2), "\n")
cat("N (Yes):", length(internet_yes), "  N (No):", length(internet_no), "\n\n")

t.test(internet_yes, internet_no)
```

### Extracurricular Activities

```{r ttest_extra}
extra_yes <- subset(students, Extracurricular_Activities == "Yes")$Final_Exam_Score
extra_no  <- subset(students, Extracurricular_Activities == "No")$Final_Exam_Score

cat("Mean score (Extracurricular Yes):", round(mean(extra_yes), 2), "\n")
cat("Mean score (Extracurricular No):", round(mean(extra_no), 2), "\n")
cat("N (Yes):", length(extra_yes), "  N (No):", length(extra_no), "\n\n")

t.test(extra_yes, extra_no)
```

## ANOVA: Three-Level Categorical Variables vs. Final Exam Score

For categorical variables with 3 levels (Low, Medium, High), we use one-way ANOVA. H0: all group means are equal; Ha: at least one group mean differs. If significant, Tukey HSD identifies which pairs differ.

### Parental Involvement

```{r anova_parental}
anova_parental <- aov(Final_Exam_Score ~ Parental_Involvement, data = students)
summary(anova_parental)
```

```{r tukey_parental}
TukeyHSD(anova_parental)
```

### Access to Resources

```{r anova_resources}
anova_resources <- aov(Final_Exam_Score ~ Access_to_Resources, data = students)
summary(anova_resources)
```

```{r tukey_resources}
TukeyHSD(anova_resources)
```

### Motivation Level

```{r anova_motivation}
anova_motivation <- aov(Final_Exam_Score ~ Motivation_Level, data = students)
summary(anova_motivation)
```

```{r tukey_motivation}
TukeyHSD(anova_motivation)
```

## Chi-Square Tests: Associations Between Categorical Variables

We test whether pairs of categorical variables are independent. H0: the variables are independent; Ha: they are associated.

```{r chisq_internet_extra}
table_ie <- table(students$Internet_Access, students$Extracurricular_Activities)
table_ie
chisq.test(table_ie)
```

```{r chisq_parental_motivation}
table_pm <- table(students$Parental_Involvement, students$Motivation_Level)
table_pm
chisq.test(table_pm)
```

```{r chisq_resources_internet}
table_ri <- table(students$Access_to_Resources, students$Internet_Access)
table_ri
chisq.test(table_ri)
```

## Correlation Tests: Numeric IVs vs. Final Exam Score

We test whether each numeric IV is significantly correlated with Final Exam Score (Pearson correlation). H0: rho = 0; Ha: rho != 0.

```{r cor_tests}
cor_test_hours    <- cor.test(students$Hours_Studied, students$Final_Exam_Score)
cor_test_attend   <- cor.test(students$Attendance, students$Final_Exam_Score)
cor_test_sleep    <- cor.test(students$Sleep_Hours, students$Final_Exam_Score)
cor_test_previous <- cor.test(students$Previous_Scores, students$Final_Exam_Score)
cor_test_tutor    <- cor.test(students$Tutoring_Sessions, students$Final_Exam_Score)

cor_summary <- data.frame(
  Variable = c("Hours_Studied", "Attendance", "Sleep_Hours",
                "Previous_Scores", "Tutoring_Sessions"),
  Correlation = round(c(cor_test_hours$estimate, cor_test_attend$estimate,
                         cor_test_sleep$estimate, cor_test_previous$estimate,
                         cor_test_tutor$estimate), 4),
  P_Value = c(cor_test_hours$p.value, cor_test_attend$p.value,
               cor_test_sleep$p.value, cor_test_previous$p.value,
               cor_test_tutor$p.value),
  stringsAsFactors = FALSE
)

cor_summary
```

```{r cor_table, results='asis'}
xkabledply(cor_summary, title = "Pearson Correlations with Final Exam Score")
```

# Pass/Fail Definition and EDA

Here, we explore different pass/fail definitions. We create three versions:

1. Cutoff at 50 (absolute score threshold)
2. Cutoff at the median (relative to this cohort)
3. Cutoff at the 25th percentile (identifying the lowest-performing quartile)

```{r create_passfail}
median_score <- median(students$Final_Exam_Score)
q25_score    <- quantile(students$Final_Exam_Score, 0.25)

cat("Median Final Exam Score:", round(median_score, 2), "\n")
cat("25th Percentile Final Exam Score:", round(q25_score, 2), "\n")

students$Pass_50     <- factor(ifelse(students$Final_Exam_Score >= 50, "Pass", "Fail"))
students$Pass_Median <- factor(ifelse(students$Final_Exam_Score >= median_score, "Pass", "Fail"))
students$Pass_Q25    <- factor(ifelse(students$Final_Exam_Score >= q25_score, "Pass", "Fail"))
```

## Pass/Fail Distributions Under Each Cutoff

```{r passfail_bars, fig.width=10, fig.height=4}
pf1 <- ggplot(students, aes(x = Pass_50, fill = Pass_50)) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = paste0("Cutoff = 50\n(Fail: ", sum(students$Pass_50 == "Fail"),
                       ", Pass: ", sum(students$Pass_50 == "Pass"), ")"),
       x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

pf2 <- ggplot(students, aes(x = Pass_Median, fill = Pass_Median)) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = paste0("Cutoff = Median (", round(median_score, 1), ")\n(Fail: ",
                       sum(students$Pass_Median == "Fail"), ", Pass: ",
                       sum(students$Pass_Median == "Pass"), ")"),
       x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

pf3 <- ggplot(students, aes(x = Pass_Q25, fill = Pass_Q25)) +
  geom_bar(alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = paste0("Cutoff = 25th %ile (", round(q25_score, 1), ")\n(Fail: ",
                       sum(students$Pass_Q25 == "Fail"), ", Pass: ",
                       sum(students$Pass_Q25 == "Pass"), ")"),
       x = "", y = "Count") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")

grid.arrange(pf1, pf2, pf3, ncol = 3)
```

## EDA of Pass/Fail by Key Variables

Using the median cutoff as our primary definition (a natural 50/50 split), we explore how pass/fail relates to key variables.

```{r passfail_hours, fig.width=10, fig.height=5}
ggplot(students, aes(x = Pass_Median, y = Hours_Studied, fill = Pass_Median)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = "Hours Studied by Pass/Fail (Median Cutoff)",
       x = "Pass/Fail", y = "Hours Studied") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

```{r passfail_attendance}
ggplot(students, aes(x = Pass_Median, y = Attendance, fill = Pass_Median)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = "Attendance by Pass/Fail (Median Cutoff)",
       x = "Pass/Fail", y = "Attendance (%)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

```{r passfail_previous}
ggplot(students, aes(x = Pass_Median, y = Previous_Scores, fill = Pass_Median)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = "Previous Scores by Pass/Fail (Median Cutoff)",
       x = "Pass/Fail", y = "Previous Scores") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), legend.position = "none")
```

```{r passfail_motivation}
ggplot(students, aes(x = Motivation_Level, fill = Pass_Median)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = "Pass Rate by Motivation Level (Median Cutoff)",
       x = "Motivation Level", y = "Proportion", fill = "Result") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r passfail_parental}
ggplot(students, aes(x = Parental_Involvement, fill = Pass_Median)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = "Pass Rate by Parental Involvement (Median Cutoff)",
       x = "Parental Involvement", y = "Proportion", fill = "Result") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

```{r passfail_internet}
ggplot(students, aes(x = Internet_Access, fill = Pass_Median)) +
  geom_bar(position = "fill", alpha = 0.7) +
  scale_fill_manual(values = c("Fail" = "coral", "Pass" = "steelblue")) +
  labs(title = "Pass Rate by Internet Access (Median Cutoff)",
       x = "Internet Access", y = "Proportion", fill = "Result") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
```

# Regression Analysis

## Multiple Linear Regression (SMART Question 1)

We fit a multiple linear regression with Final Exam Score as the dependent variable and all 10 independent variables as predictors.

$$\text{Final\_Exam\_Score}_i = \beta_0 + \beta_1 \text{Hours\_Studied}_i + \beta_2 \text{Attendance}_i + \cdots + \varepsilon_i$$

```{r lm_full}
model_lm <- lm(Final_Exam_Score ~ Hours_Studied + Attendance + Sleep_Hours +
                  Previous_Scores + Tutoring_Sessions + Parental_Involvement +
                  Access_to_Resources + Motivation_Level + Internet_Access +
                  Extracurricular_Activities,
                data = students)
summary(model_lm)
```

```{r lm_table, results='asis'}
xkabledply(model_lm, title = "Multiple Linear Regression: Final Exam Score ~ All Predictors")
```

```{r lm_rsquared}
lm_summary <- summary(model_lm)
cat("R-squared:", round(lm_summary$r.squared, 4), "\n")
cat("Adjusted R-squared:", round(lm_summary$adj.r.squared, 4), "\n")
cat("This means the model explains approximately",
    round(lm_summary$r.squared * 100, 1), "% of the variance in Final Exam Score.\n")
```

### Residual Diagnostics

```{r lm_diagnostics, fig.width=10, fig.height=8}
par(mfrow = c(2, 2))
plot(model_lm)
par(mfrow = c(1, 1))
```

```{r lm_qq}
qqnorm(residuals(model_lm), main = "QQ Plot of Residuals (Full Linear Model)")
qqline(residuals(model_lm), col = "red")
```

### VIF (Multicollinearity Check)

```{r vif_check}
loadPkg("car")
vif(model_lm)
```

VIF values above 5-10 would suggest problematic multicollinearity among the predictors.

## Logistic Regression (SMART Question 2)

Using the median cutoff for pass/fail, we fit a logistic regression to identify predictors of failure.

```{r logit_model}
model_logit <- glm(Pass_Median ~ Hours_Studied + Attendance + Sleep_Hours +
                     Previous_Scores + Tutoring_Sessions + Parental_Involvement +
                     Access_to_Resources + Motivation_Level + Internet_Access +
                     Extracurricular_Activities,
                   data = students, family = "binomial")
summary(model_logit)
```

```{r logit_table, results='asis'}
xkabledply(model_logit, title = "Logistic Regression: Pass/Fail (Median Cutoff) ~ All Predictors")
```

### Odds Ratios

```{r odds_ratios}
exp(coef(model_logit))
```

Odds ratios greater than 1 indicate higher odds of passing; less than 1 indicate higher odds of failing.

### Confusion Matrix

```{r confusion_matrix}
predicted_probs <- predict(model_logit, type = "response")
predicted_class <- ifelse(predicted_probs >= 0.5, "Pass", "Fail")
predicted_class <- factor(predicted_class, levels = c("Fail", "Pass"))

conf_matrix <- table(Predicted = predicted_class, Actual = students$Pass_Median)
conf_matrix

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nAccuracy:", round(accuracy * 100, 2), "%\n")
```

## Comparing Pass/Fail Cutoffs in Logistic Regression

We briefly compare model performance across the three cutoff definitions.

```{r logit_50}
model_logit_50 <- glm(Pass_50 ~ Hours_Studied + Attendance + Sleep_Hours +
                        Previous_Scores + Tutoring_Sessions + Parental_Involvement +
                        Access_to_Resources + Motivation_Level + Internet_Access +
                        Extracurricular_Activities,
                      data = students, family = "binomial")

pred_50 <- ifelse(predict(model_logit_50, type = "response") >= 0.5, "Pass", "Fail")
pred_50 <- factor(pred_50, levels = c("Fail", "Pass"))
acc_50 <- sum(diag(table(pred_50, students$Pass_50))) / nrow(students)

cat("Cutoff = 50:  Accuracy =", round(acc_50 * 100, 2), "%\n")
```

```{r logit_q25}
model_logit_q25 <- glm(Pass_Q25 ~ Hours_Studied + Attendance + Sleep_Hours +
                         Previous_Scores + Tutoring_Sessions + Parental_Involvement +
                         Access_to_Resources + Motivation_Level + Internet_Access +
                         Extracurricular_Activities,
                       data = students, family = "binomial")

pred_q25 <- ifelse(predict(model_logit_q25, type = "response") >= 0.5, "Pass", "Fail")
pred_q25 <- factor(pred_q25, levels = c("Fail", "Pass"))
acc_q25 <- sum(diag(table(pred_q25, students$Pass_Q25))) / nrow(students)

cat("Cutoff = 25th percentile:  Accuracy =", round(acc_q25 * 100, 2), "%\n")
cat("Cutoff = Median:  Accuracy =", round(accuracy * 100, 2), "%\n")
