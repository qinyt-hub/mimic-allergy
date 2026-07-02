 "Multivariable Logistic Regression"

```{r setup,message=FALSE,warning=FALSE}

library(tidyverse)
library(broom)
library(gtsummary)
library(gt)
library(finalfit)
library(flextable)
library(officer)
library(ggplot2)

theme_set(theme_bw())

```

```{r}

df <- read.csv("data/nhanes_cleaned_data.csv")

dim(df)

head(df)

summary(df)

```{r}

df <- df %>%

mutate(

AR = factor(
allergy_rhinitis,
levels=c(0,1),
labels=c("No","Yes")
),

fiber_group=factor(
fiber_group,
levels=c(
"High Fiber",
"Medium Fiber",
"Low Fiber"
)
),

RIAGENDR=factor(RIAGENDR),

RIDRETH3=factor(RIDRETH3),

DMDEDUC2=factor(DMDEDUC2),

SMQ020=factor(SMQ020)

)

```

levels(df$fiber_group)

table(df$fiber_group)

```{r}

analysis_df <-

df %>%

select(

AR,

fiber_group,

RIDAGEYR,

RIAGENDR,

RIDRETH3,

BMXBMI,

DMDEDUC2,

INDHHIN2,

SMQ020,

DR1TKCAL

) %>%

drop_na()

dim(analysis_df)

```

```{r}

model1 <-

glm(

AR~

fiber_group,

family=binomial,

data=analysis_df

)

summary(model1)

```

```{r}

model2 <-

glm(

AR~

fiber_group+

RIDAGEYR+

RIAGENDR,

family=binomial,

data=analysis_df

)

summary(model2)

```

```{r}

model3 <-

glm(

AR~

fiber_group+

RIDAGEYR+

RIAGENDR+

RIDRETH3+

BMXBMI+

DMDEDUC2+

INDHHIN2+

SMQ020+

DR1TKCAL,

family=binomial,

data=analysis_df

)

summary(model3)


title: "Continuous Exposure Modeling"

```{r setup,message=FALSE,warning=FALSE}

library(tidyverse)
library(broom)
library(gtsummary)
library(gt)
library(finalfit)
library(flextable)
library(officer)
library(ggplot2)

theme_set(theme_bw())


df <- read.csv("data/nhanes_cleaned_data.csv")

head(df)

```{r}

analysis_df <-

df %>%

mutate(

AR=factor(
allergy_rhinitis,
levels=c(0,1),
labels=c("No","Yes")
),

Sex=factor(RIAGENDR),

Race=factor(RIDRETH3),

Education=factor(DMDEDUC2),

Smoking=factor(SMQ020)

) %>%

select(

AR,

DR1TFIBE,

RIDAGEYR,

Sex,

Race,

BMXBMI,

Education,

INDHHIN2,

Smoking,

DR1TKCAL

) %>%

drop_na()

dim(analysis_df)

```{r}

summary(analysis_df$DR1TFIBE)

hist(

analysis_df$DR1TFIBE,

breaks=30,

col="#4DBBD5",

main="Dietary Fiber Intake",

xlab="Fiber (g/day)"

)

```{r}

model1 <- glm(

AR~

DR1TFIBE,

family=binomial,

data=analysis_df

)

summary(model1)

```{r}

model2 <- glm(

AR~

DR1TFIBE+

RIDAGEYR+

Sex,

family=binomial,

data=analysis_df

)

summary(model2)

```{r}

model3 <- glm(

AR~

DR1TFIBE+

RIDAGEYR+

Sex+

Race+

BMXBMI+

Education+

INDHHIN2+

Smoking+

DR1TKCAL,

family=binomial,

data=analysis_df

)

summary(model3)

result1 <- tidy(

model1,

conf.int=TRUE,

exponentiate=TRUE

)

result2 <- tidy(

model2,

conf.int=TRUE,

exponentiate=TRUE

)

result3 <- tidy(

model3,

conf.int=TRUE,

exponentiate=TRUE

)

fiber1 <-

result1 %>%

filter(term=="DR1TFIBE") %>%

mutate(Model="Model1")

fiber2 <-

result2 %>%

filter(term=="DR1TFIBE") %>%

mutate(Model="Model2")

fiber3 <-

result3 %>%

filter(term=="DR1TFIBE") %>%

mutate(Model="Model3")

continuous_result <-

bind_rows(

fiber1,

fiber2,

fiber3

)

continuous_result

continuous_result <-

continuous_result %>%

transmute(

Model,

OR=round(estimate,3),

Lower95CI=round(conf.low,3),

Upper95CI=round(conf.high,3),

P=round(p.value,4)

)

continuous_result

write.csv(

continuous_result,

"Results/Continuous_Logistic.csv",

row.names=FALSE

)

```
```{r fig.width=6,fig.height=4}

ggplot(

continuous_result,

aes(

x=Model,

y=OR

)

)+

geom_point(

size=4,

colour="#E64B35"

)+

geom_errorbar(

aes(

ymin=Lower95CI,

ymax=Upper95CI

),

width=.15,

linewidth=.8

)+

geom_hline(

yintercept=1,

linetype=2

)+

theme_classic(base_size=15)+

ylab("Odds Ratio per 1 g/day Fiber")

```
```{r}

p <- last_plot()

ggsave(

"Results/Continuous_Logistic.pdf",

p,

width=6,

height=4

)

ggsave(

"Results/Continuous_Logistic.tiff",

p,

dpi=600,

compression="lzw",

width=6,

height=4

)

sessionInfo()
