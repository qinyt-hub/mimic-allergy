title: "Immune Cell Composition Barplot"   

```{r setup, message=FALSE, warning=FALSE}
library(tidyverse)
library(ggplot2)
library(scales)
```

# 1. 输入数据

```{r}
df <- data.frame(
  CellType = c(
    "Basophil",
    "Eosinophil",
    "mast_cell",
    "M1_macrophage",
    "M2_macrophage",
    "DC.Immature",
    "Th1.Cells",
    "Th2.Cells",
    "Treg",
    "B1_cell"
  ),

  AR = c(
    0.0898,
    0.0599,
    0.1295,
    0.0070,
    0.1116,
    0.0001,
    0.0659,
    0.0105,
    0.0010,
    0.0098
  ),

  NDF = c(
    0.0023,
    0.0189,
    0.0522,
    0.0524,
    0.1450,
    0.0162,
    0.2014,
    0.0001,
    0.0197,
    0.0630
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
    values_to = "Fraction"
  )

df_long
```

# 3. 设置绘图顺序

```{r}
df_long$CellType <- factor(
  df_long$CellType,
  levels = rev(df$CellType)
)
```

# 4. Nature配色

```{r}
group_colors <- c(
  "AR" = "#E64B35",
  "NDF" = "#4DBBD5"
)
```

# 5. 绘制柱状图

```{r fig.width=9, fig.height=6}

p <- ggplot(
  df_long,
  aes(
    x = CellType,
    y = Fraction,
    fill = Group
  )
) +

geom_col(
  position = position_dodge(width = 0.75),
  width = 0.65,
  colour = "black",
  linewidth = 0.4
) +

coord_flip() +

scale_fill_manual(values = group_colors) +

scale_y_continuous(
  labels = percent_format(accuracy = 1)
) +

labs(
  title = "Immune Cell Composition",
  x = "",
  y = "Cell proportion",
  fill = ""
) +

theme_classic(base_size = 15) +

theme(
  plot.title = element_text(
    hjust = 0.5,
    face = "bold",
    size = 18
  ),
  axis.text.y = element_text(
    colour = "black",
    face = "bold"
  ),
  axis.text.x = element_text(
    colour = "black"
  ),
  axis.title.y = element_blank(),
  legend.position = "top",
  legend.title = element_blank()
)

p
```

# 6. 保存图片

```{r}

ggsave(
  filename = "ImmuneCell_Barplot.pdf",
  plot = p,
  width = 9,
  height = 6
)

ggsave(
  filename = "ImmuneCell_Barplot.tiff",
  plot = p,
  width = 9,
  height = 6,
  dpi = 600,
  compression = "lzw"
)

```  

                             
