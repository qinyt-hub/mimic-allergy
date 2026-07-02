Univariable Logistic Regression for Dietary Fiber and Allergic Rhinitis

# Packages

```{r setup,message=FALSE,warning=FALSE}

library(tidyverse)
library(gtsummary)
library(broom)
library(gt)
library(flextable)
library(officer)
library(finalfit)

theme_set(theme_bw())

```

# Read data

```{r}

df <- read.csv("data/nhanes_cleaned_data.csv")

dim(df)

head(df)

```

# Data preprocessing

```{r}

df <- df %>%

mutate(

AR=factor(
allergy_rhinitis,
levels=c(0,1),
labels=c("No","Yes")
),

fiber_group=factor(
fiber_group,
levels=c("High Fiber","Medium Fiber","Low Fiber")
)

)

```

# Check variables

```{r}

str(df)

summary(df)

```

# Table 1

```{r}

table1 <-

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

tbl_summary(

by=AR,

missing="no"

) %>%

add_p()

table1

```

# Export Table 1

```{r}

table1 %>%

as_flex_table() %>%

save_as_docx(path="Results/Table1.docx")

```

# Univariable Logistic Regression

Outcome

```{r}

dependent="AR"

```

Predictors

```{r}

independent <- c(

"fiber_group",

"RIDAGEYR",

"RIAGENDR",

"RIDRETH3",

"BMXBMI",

"DMDEDUC2",

"INDHHIN2",

"SMQ020",

"DR1TKCAL"

)

```

Run models

```{r}

uni_result <-

lapply(

independent,

function(x){

formula <- as.formula(

paste(

"AR~",

x

)

)

glm(

formula,

data=df,

family=binomial

)

}

)

```

# Extract OR

```{r}

uni_table <-

map2_df(

uni_result,

independent,

function(model,var){

tidy(

model,

conf.int=TRUE,

exponentiate=TRUE

) %>%

mutate(

Variable=var

)

}

)

```

# Rearrange

```{r}

uni_table <-

uni_table %>%

select(

Variable,

term,

estimate,

conf.low,

conf.high,

p.value

)

colnames(uni_table) <- c(

"Variable",

"Level",

"OR",

"Lower95CI",

"Upper95CI",

"P"

)

uni_table

```

# Round

```{r}

uni_table <-

uni_table %>%

mutate(

OR=round(OR,3),

Lower95CI=round(Lower95CI,3),

Upper95CI=round(Upper95CI,3),

P=round(P,4)

)

```

# Export csv

```{r}

write.csv(

uni_table,

"Results/Univariable_Logistic.csv",

row.names=FALSE

)

```

# Pretty Table

```{r}

gt(uni_table)

```

# Forest Plot

```{r}

forest <-

uni_table %>%

filter(

Level!="(Intercept)"

)

ggplot(

forest,

aes(

y=reorder(Level,OR),

x=OR

)

)+

geom_point(

size=3,

colour="#E64B35"

)+

geom_errorbarh(

aes(

xmin=Lower95CI,

xmax=Upper95CI

),

height=.25

)+

geom_vline(

xintercept=1,

linetype=2

)+

labs(

x="Odds Ratio",

y=""

)+

theme_classic()

forest

```

# Save Figure

```{r}

ggsave(

"Results/Forest_Univariable.pdf",

forest,

width=7,

height=6

)

ggsave(

"Results/Forest_Univariable.tiff",

forest,

dpi=600,

compression="lzw",

width=7,

height=6

)

```

# Session information

```{r}

sessionInfo()

```
