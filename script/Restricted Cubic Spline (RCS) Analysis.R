 "Restricted Cubic Spline (RCS) Analysis: Fiber Intake and AR"

# 加载核心包
library(tidyverse)
library(rms)          # 用于 RCS 回归（lrm, datadist, Predict）
library(ggplot2)
library(knitr)
library(kableExtra)

# 筛选所需变量，删除缺失值
df <- df_raw %>%
  dplyr::select(AR, fiber_g, age, gender) %>%   # 若有其他协变量可添加
  filter(!is.na(AR) & !is.na(fiber_g)) %>%
  # 确保 AR 为数值型 0/1（rms 要求）
  mutate(
    AR = as.numeric(AR) - 1,          # 若原为 1/2 或因子，请调整此处
    gender = as.factor(gender)        # 若有性别变量，转为因子
  )

# 剔除纤维摄入极端异常值（可选，建议保留 1%-99% 百分位数）
# df <- df %>% filter(fiber_g > quantile(fiber_g, 0.01) & fiber_g < quantile(fiber_g, 0.99))

# 查看数据结构
str(df)
summary(df$fiber_g)
table(df$AR)

# 定义数据分布环境，rms 包依赖于 datadist
dd <- datadist(df)
options(datadist = "dd")

# lrm() 用于拟合逻辑回归，rcs() 定义限制性立方样条
model_crude <- lrm(AR ~ rcs(fiber_g, knots), data = df, x = TRUE, y = TRUE)

# 查看模型概要
print(model_crude)

# 输出方差分析（检验线性趋势与非线性趋势）
anova_crude <- anova(model_crude)
print(anova_crude)

# 若有协变量，例如年龄（连续）和性别（分类）
# 若没有相关变量，可跳过此步
if ("age" %in% colnames(df) & "gender" %in% colnames(df)) {
  model_adj <- lrm(AR ~ rcs(fiber_g, knots) + age + gender, 
                   data = df, x = TRUE, y = TRUE)
  print(model_adj)
  anova_adj <- anova(model_adj)
  print(anova_adj)
} else {
  cat("未检测到 age 或 gender 变量，跳过调整模型。\n")
  model_adj <- NULL
}

# 设定参照值：根据研究设计，以 High Fiber 切点 25 g/day 为参照 (OR=1)
ref_value <- 25

# 生成预测数据范围（从第 5 百分位数到第 95 百分位数，避免尾部噪声）
fiber_range <- seq(quantile(df$fiber_g, 0.05, na.rm = TRUE), 
                   quantile(df$fiber_g, 0.95, na.rm = TRUE), 
                   length.out = 100)

# 使用 Predict 函数进行预测（未调整模型）
p_crude <- Predict(model_crude, fiber_g = fiber_range, ref.zero = TRUE, fun = exp)

# 将预测结果转为 data.frame 方便绘图
pred_df_crude <- as.data.frame(p_crude)
colnames(pred_df_crude) <- c("fiber_g", "OR", "lower", "upper")

# 如果有调整模型，同样预测
if (!is.null(model_adj)) {
  p_adj <- Predict(model_adj, fiber_g = fiber_range, ref.zero = TRUE, fun = exp)
  pred_df_adj <- as.data.frame(p_adj)
  colnames(pred_df_adj) <- c("fiber_g", "OR", "lower", "upper")
} else {
  pred_df_adj <- NULL
}

# 查看预测数据前几行
head(pred_df_crude)

# 基础绘图（未调整模型）
p_plot <- ggplot(pred_df_crude, aes(x = fiber_g, y = OR)) +
  geom_line(size = 1.2, color = "#2c3e50") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#3498db") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", size = 0.8) +
  # 在参照值 25 g/day 处添加竖线标记
  geom_vline(xintercept = ref_value, linetype = "dotted", color = "grey40", size = 0.8) +
  annotate("text", x = ref_value + 1, y = max(pred_df_crude$upper) * 0.9, 
           label = paste0("Ref: ", ref_value, " g/day"), hjust = 0, size = 4) +
  # 添加直方图或地毯图以显示数据分布（可选）
  geom_rug(data = df, aes(x = fiber_g), sides = "b", alpha = 0.1) +
  labs(
    title = "Restricted Cubic Spline: Fiber Intake and AR Odds Ratio",
    subtitle = "Adjusted for covariates (if applicable) | 4 knots",
    x = "Dietary Fiber Intake (g/day)",
    y = "Odds Ratio for AR (95% CI)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(p_plot)

# 如果有调整模型，可以将两条曲线叠加比较（可选）
if (!is.null(pred_df_adj)) {
  pred_df_crude$Model <- "Crude"
  pred_df_adj$Model <- "Adjusted"
  combined <- rbind(pred_df_crude, pred_df_adj)
  
  p_combined <- ggplot(combined, aes(x = fiber_g, y = OR, color = Model, fill = Model)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.15, linetype = 0) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.5) +
    geom_vline(xintercept = ref_value, linetype = "dotted", color = "grey50") +
    scale_color_manual(values = c("Crude" = "#e74c3c", "Adjusted" = "#2980b9")) +
    scale_fill_manual(values = c("Crude" = "#e74c3c", "Adjusted" = "#2980b9")) +
    labs(
      title = "RCS Comparison: Crude vs Adjusted",
      x = "Fiber Intake (g/day)", y = "Odds Ratio for AR (95% CI)"
    ) +
    theme_minimal(base_size = 14)
  
  print(p_combined)
}

# 未调整模型非线性检验 P 值
p_nonlinear_crude <- anova_crude["fiber_g", "Nonlinear"]  # 注意行名可能略有不同，请查看 anova_crude 实际输出

# 若存在调整模型
if (!is.null(model_adj)) {
  p_nonlinear_adj <- anova_adj["fiber_g", "Nonlinear"]
} else {
  p_nonlinear_adj <- NA
}

# 构建结果汇总表
result_table <- data.frame(
  Model = c("Crude", "Adjusted"),
  `Nonlinearity P-value` = c(round(p_nonlinear_crude, 4), round(p_nonlinear_adj, 4))
)

kable(result_table, caption = "Tests for Non-linearity in RCS Models") %>%
  kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)

# 为了呼应前文的分类分析，在图中添加低、中、高纤维的分界线
# Low < 15, Medium 15-25, High > 25
group_breaks <- c(0, 15, 25, max(df$fiber_g, na.rm = TRUE))
group_labels <- c("Low Fiber", "Medium Fiber", "High Fiber")

p_plot +
  # 添加分组背景色块
  annotate("rect", xmin = 0, xmax = 15, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "#f1c40f") +
  annotate("rect", xmin = 15, xmax = 25, ymin = -Inf, ymax = Inf, 
           alpha = 0.1, fill = "#2ecc71") +
  annotate("rect", xmin = 25, xmax = max(pred_df_crude$fiber_g), 
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "#3498db") +
  annotate("text", x = 7.5, y = 0.2, label = "Low", size = 3, color = "grey30") +
  annotate("text", x = 20, y = 0.2, label = "Medium", size = 3, color = "grey30") +
  annotate("text", x = 35, y = 0.2, label = "High", size = 3, color = "grey30") +
  labs(subtitle = "Overlay with traditional fiber intake categories")


