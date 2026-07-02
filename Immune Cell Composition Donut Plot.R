Immune Cell Composition Donut Plot

``{r setup, message=FALSE, warning=FALSE}
library(tidyverse)
library(scales)
library(ggplot2)
```

# 1. 输入数据

```{r}
df <- data.frame(
  CellType = c(
    "B cell",
    "T cell",
    "Dendritic cell",
    "NK",
    "Macrophage",
    "Monocytes",
    "Others"
  ),
  AR = c(
    0.0216,
    0.2103,
    0.0455,
    0.2866,
    0.1186,
    0.0378,
    0.2796
  ),
  NDF = c(
    0.2795,
    0.0112,
    0.1175,
    0.2829,
    0.1974,
    0.0045,
    0.1070
  )
)

df
```

# 2. 数据整理

```{r}
df_long <- df %>%
  pivot_longer(
    cols = c(AR, NDF),
    names_to = "Group",
    values_to = "Proportion"
  ) %>%
  group_by(Group) %>%
  arrange(Group, desc(Proportion)) %>%
  mutate(
    Fraction = Proportion / sum(Proportion),
    ymax = cumsum(Fraction),
    ymin = lag(ymax, default = 0),
    label_pos = (ymax + ymin) / 2,
    Label = percent(Fraction, accuracy = 0.1)
  )

df_long
```

# 3. 配色

```{r}
cell_colors <- c(
  "B cell"="#4DBBD5",
  "T cell"="#E64B35",
  "Dendritic cell"="#00A087",
  "NK"="#3C5488",
  "Macrophage"="#F39B7F",
  "Monocytes"="#91D1C2",
  "Others"="#8491B4"
)
```

# 4. 绘制环形饼图

```{r fig.width=10, fig.height=5}

p <- ggplot(df_long,
            aes(
              ymax = ymax,
              ymin = ymin,
              xmax = 4,
              xmin = 2,
              fill = CellType
            )) +

  geom_rect(color="white", linewidth=0.8) +

  coord_polar(theta="y") +

  xlim(0,4) +

  facet_wrap(~Group) +

  geom_text(
    aes(
      x=3,
      y=label_pos,
      label=Label
    ),
    size=4
  ) +

  scale_fill_manual(values=cell_colors) +

  labs(
    title="Immune Cell Composition",
    fill="Cell Type"
  ) +

  theme_void(base_size=14) +

  theme(
    plot.title=element_text(
      hjust=0.5,
      face="bold",
      size=18
    ),
    strip.text=element_text(
      face="bold",
      size=14
    ),
    legend.position="right"
  )

p
```

# 5. 保存图片

```{r}

ggsave(
  "DonutPlot.pdf",
  p,
  width=10,
  height=5
)

ggsave(
  "DonutPlot.tiff",
  p,
  width=10,
  height=5,
  dpi=600,
  compression="lzw"
)

```
