 "Fiber Intake and AR Prevalence - Trend Testing"

# 加载所需包
library(tidyverse)
library(broom)
library(ggplot2)
library(knitr)
library(kableExtra)

# 假设纤维变量名为 fiber_g，AR变量名为 AR（0/1或因子）
# 请根据实际变量名修改
df <- nhanes_cleaned_data %>%
  mutate(
    fiber_group = case_when(
      fiber_g > 25 ~ "High",
      fiber_g >= 15 & fiber_g <= 25 ~ "Medium",
      fiber_g < 15 ~ "Low"
    ),
    # 将分组转换为有序因子，顺序为 Low < Medium < High
    fiber_group = factor(fiber_group, levels = c("Low", "Medium", "High")),
    # 确保 AR 为因子（如果尚未）
    AR = factor(AR, levels = c(0, 1), labels = c("No", "Yes"))
  ) %>%
  # 删除缺失值
  filter(!is.na(fiber_group) & !is.na(AR))

# 查看各组合计数
table(df$fiber_group)

prev_table <- df %>%
  group_by(fiber_group) %>%
  summarise(
    N = n(),
    AR_Yes = sum(AR == "Yes"),
    Prevalence = round(AR_Yes / N * 100, 2)
  ) %>%
  mutate(Prevalence_CI = NA)  # 可自行添加置信区间

kable(prev_table, caption = "AR Prevalence by Fiber Group",
      col.names = c("Fiber Group", "N", "AR Cases", "Prevalence (%)")) %>%
  kable_styling(full_width = FALSE)

chisq_test <- chisq.test(table(df$fiber_group, df$AR))
cat("Chi-square test for association:\n")
print(chisq_test)

# 将 High 设为参考水平
df$fiber_group_ref <- relevel(df$fiber_group, ref = "High")

# 单变量逻辑回归
model_unadj <- glm(AR ~ fiber_group_ref, data = df, family = binomial())
summary(model_unadj)

# 提取 OR 和 95% CI
or_table <- tidy(model_unadj, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = gsub("fiber_group_ref", "", term),
    OR = round(estimate, 2),
    CI = paste0(round(conf.low, 2), "–", round(conf.high, 2)),
    P = round(p.value, 4)
  ) %>%
  select(Term = term, OR, CI, P)

kable(or_table, caption = "Odds Ratios for AR (vs. High Fiber)") %>%
  kable_styling(full_width = FALSE)

# 为分组赋值：Low=1, Medium=2, High=3
df <- df %>%
  mutate(
    fiber_score = case_when(
      fiber_group == "Low" ~ 1,
      fiber_group == "Medium" ~ 2,
      fiber_group == "High" ~ 3
    )
  )

# 将 fiber_score 作为连续变量纳入逻辑回归
model_trend <- glm(AR ~ fiber_score, data = df, family = binomial())
summary(model_trend)

# 提取趋势检验的 p 值
trend_p <- coef(summary(model_trend))["fiber_score", "Pr(>|z|)"]
cat("Linear trend p-value (from logistic regression):", trend_p, "\n")

cat("=== Summary of Results ===\n")
cat("Prevalence:\n")
print(prev_table)
cat("\nChi-square p-value:", round(chisq_test$p.value, 4))
cat("\nCochran-Armitage trend p-value:", round(prop_trend$p.value, 4))
cat("\nLogistic linear trend p-value:", round(trend_p, 4))
