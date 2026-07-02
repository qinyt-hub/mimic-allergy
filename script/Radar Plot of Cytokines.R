
title: "Radar Plot of Cytokines"
```{r setup, message=FALSE, warning=FALSE}
library(tidyverse)
library(fmsb)
```

# 输入数据

```{r}
cytokine <- data.frame(

Marker=c(
"IL-1 beta",
"TNF-alpha",
"IL-13",
"IL-6",
"IL-4",
"IL-1 alpha",
"IL-10",
"IL-17A",
"IL-12p70"
),

G0_1=c(2.05,18.97,15.15,7.25,0.97,13.58,21,1.15,27.06),
G0_2=c(2.05,21.08,19.06,6.15,0.74,13.16,17.01,1.38,20.53),
G0_3=c(1.47,14.97,11.46,6.34,0.74,17.32,9.50,0.93,20.53),

G1_1=c(6.05,47.55,59.17,12.91,1.45,25.29,46.28,2.12,41.08),
G1_2=c(3.98,30.07,36.14,11.39,2.74,18.75,33.47,1.38,35.70),
G1_3=c(3.32,21.08,36.14,16.15,2.97,16.08,33.47,1.99,23.75),

G2_1=c(2.99,27.74,36.14,22.60,6.44,14.84,25.09,1.62,22.93),
G2_2=c(2.05,14.97,27.37,9.31,1.98,15.46,21.00,0.93,18.97),
G2_3=c(2.67,27.74,49.82,18.43,4.29,19.76,29.26,1.62,27.06),

G3_1=c(2.05,23.24,27.37,14.37,1.71,17.94,17.01,1.15,27.06),
G3_2=c(2.67,37.33,36.14,7.81,1.45,18.14,21.00,1.38,33.94),
G3_3=c(5.35,23.24,36.14,11.77,1.71,18.96,29.26,1.62,28.75),

G4_1=c(1.47,18.97,23.15,11.02,1.71,12.94,13.15,0.73,22.13),
G4_2=c(1.47,23.24,15.15,4.72,0.74,12.31,17.01,1.15,22.13),
G4_3=c(2.67,21.08,40.64,8.00,0.94,11.03,21.00,1.38,27.06)

)

cytokine
```

# 计算均值

```{r}

mean_df <- data.frame(

Group=c("G0","G1","G2","G3","G4"),

t(sapply(1:nrow(cytokine),function(i){

c(

mean(as.numeric(cytokine[i,2:4])),

mean(as.numeric(cytokine[i,5:7])),

mean(as.numeric(cytokine[i,8:10])),

mean(as.numeric(cytokine[i,11:13])),

mean(as.numeric(cytokine[i,14:16]))

)

}))

)

colnames(mean_df)[-1] <- cytokine$Marker

mean_df
```

# 雷达图数据

```{r}

radar <- mean_df

rownames(radar) <- radar$Group

radar <- radar[,-1]

radar <- as.data.frame(radar)

radar <- rbind(

apply(radar,2,max)*1.1,

rep(0,ncol(radar)),

radar

)

rownames(radar)[1:2] <- c("Max","Min")
```

# 绘图

```{r fig.width=8,fig.height=8}

cols <- c(
"#4DBBD5",
"#E64B35",
"#00A087",
"#3C5488",
"#F39B7F"
)

par(
mar=c(2,2,2,2)
)

radarchart(

radar,

axistype=1,

seg=5,

pcol=cols,

pfcol=adjustcolor(cols,alpha.f=0.20),

plwd=3,

plty=1,

cglcol="grey80",

cglty=1,

cglwd=0.8,

axislabcol="grey30",

vlcex=1.2,

title="Cytokine Radar Plot"

)

legend(

"topright",

legend=c("G0","G1","G2","G3","G4"),

col=cols,

lwd=3,

bty="n"

)
```

# 保存图片

```{r}

pdf(
"RadarPlot.pdf",
width=8,
height=8
)

par(mar=c(2,2,2,2))

radarchart(
radar,
axistype=1,
seg=5,
pcol=cols,
pfcol=adjustcolor(cols,0.2),
plwd=3,
cglcol="grey80",
cglty=1,
cglwd=0.8,
vlcex=1.2
)

legend(
"topright",
legend=c("G0","G1","G2","G3","G4"),
col=cols,
lwd=3,
bty="n"
)

dev.off()

tiff(
"RadarPlot.tiff",
width=8,
height=8,
units="in",
res=600,
compression="lzw"
)

par(mar=c(2,2,2,2))

radarchart(
radar,
axistype=1,
seg=5,
pcol=cols,
pfcol=adjustcolor(cols,0.2),
plwd=3,
cglcol="grey80",
cglty=1,
cglwd=0.8,
vlcex=1.2
)

legend(
"topright",
legend=c("G0","G1","G2","G3","G4"),
col=cols,
lwd=3,
bty="n"
)

dev.off()

```
