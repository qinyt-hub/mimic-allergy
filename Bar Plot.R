---
title: "Bar Plot"
```{r setup, message=FALSE, warning=FALSE}
library(tidyverse)
library(ggpubr)
```

## 输入数据

```{r}
df <- data.frame(
  Ctrl=c(26.70,24.93,27.52,25.06,24.29,26.74),
  OVA=c(33.65,34.73,29.47,30.24,36.43,31.35),
  Butyrate=c(25.98,26.38,29.37,20.31,24.72,25.33),
  OVA_Butyrate=c(23.95,25.72,24.56,23.85,23.21,20.84)
)

df
```

## 转成长格式

```{r}
data_long <- df %>%
  pivot_longer(
    cols = everything(),
    names_to = "Group",
    values_to = "Value"
  )

data_long
```

## 计算均值和标准差

```{r}
summary_df <- data_long %>%
  group_by(Group) %>%
  summarise(
    Mean = mean(Value),
    SD = sd(Value)
  )

summary_df
```

## 绘图

```{r fig.width=6, fig.height=5}

p <- ggplot(summary_df,
            aes(Group, Mean, fill=Group))+

geom_col(width=0.65,
         color="black",
         linewidth=0.8)+

geom_errorbar(aes(ymin=Mean-SD,
                  ymax=Mean+SD),
              width=0.18,
              linewidth=0.8)+

geom_jitter(data=data_long,
            aes(Group, Value),
            width=0.10,
            size=2.8,
            shape=21,
            fill="white",
            color="black",
            inherit.aes=FALSE)+

scale_fill_manual(values=c(
  "#999999",
  "#E64B35",
  "#4DBBD5",
  "#00A087"
))+

labs(
  x="",
  y="Value"
)+

theme_classic(base_size = 16)+

theme(
  legend.position="none",
  axis.text=element_text(color="black"),
  axis.title=element_text(face="bold"),
  axis.line=element_line(linewidth=0.8),
  axis.ticks=element_line(linewidth=0.8)
)

p
```

## 保存图片

```{r}
ggsave(
  filename="Barplot.pdf",
  plot=p,
  width=6,
  height=5
)

ggsave(
  filename="Barplot.tiff",
  plot=p,
  dpi=600,
  compression="lzw",
  width=6,
  height=5
)
```
