# clear workspace
rm(list = ls())

pacman::p_load(
  dplyr,
  ggplot2,
  paths
)


setwd("~/Documents/GitHub/casual-statistical-fairness-analysis")
income.df <- read.csv("data/ACSIncome_NY_2018.csv")

# filter out a select number of features
income.df <- select(income.df, c("AGEP", "COW", "SCHL", "MAR", "WKHP", "SEX", "TARGET"))

# rename target column
income.df <- rename(income.df, INCOME = TARGET)

income.df <- income.df %>% mutate(SEX = recode(SEX, `1`=0, `2`=1)) # male is 0, female is 1


m1 <- c("SCHL")
m2 <- c("MAR")
mediators <- list(m1, m2)

# NOTE: adding squared interaction term to continous features
formula_m0 <- INCOME ~ SEX + AGEP + I(AGEP^2)
formula_m1 <- update(formula_m0,    ~ . + SCHL) # adding education mediator
formula_m2 <- update(formula_m1,    ~ . + MAR) # adding marital mediator
#formula_ps <- SEX ~ AGEP + I(AGEP^2) + SCHL + MAR

glm_m0 <- glm(formula_m0, family = binomial("logit"), data = income.df)
glm_m1 <- glm(formula_m1, family = binomial("logit"), data = income.df)
glm_m2 <- glm(formula_m2, family = binomial("logit"), data = income.df)
glm_ymodels <- list(glm_m0, glm_m1, glm_m2)

# propensity score model
#glm_ps <- glm(formula_ps, family = binomial("logit"), data = income.df)

paths_glm <- paths(a = "SEX", y = "INCOME", m = mediators,
                   glm_ymodels, data = income.df, nboot = 250)

# summarize results
summary(paths_glm)

plot(paths_glm, mediator_names = c('education', 'marital status'),
     estimator = 'pure',
     horizontal = FALSE)
