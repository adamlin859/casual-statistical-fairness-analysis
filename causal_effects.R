# clear workspace
rm(list = ls())

pacman::p_load(
  dplyr,
  ggplot2,
  paths
)


setwd("~/Documents/GitHub/casual-statistical-fairness-analysis")
income.df <- read.csv("data/ACSIncome_2018_prepared.csv")

income.df <- income.df %>% mutate(SEX = recode(SEX, `1`=1, `2`=0)) # male is 1, female is 0


m1 <- c("SCHL")
m2 <- c("MAR")
mediators <- list(m1, m2)

# NOTE: adding squared interaction term to continuous features
formula_m0 <- INCOME ~ SEX + AGEP + I(AGEP^2)
formula_m1 <- update(formula_m0,    ~ . + SCHL) # adding education mediator
formula_m2 <- update(formula_m1,    ~ . + MAR) # adding marital mediator


glm_m0 <- glm(formula_m0, family = binomial("logit"), data = income.df)
glm_m1 <- glm(formula_m1, family = binomial("logit"), data = income.df)
glm_m2 <- glm(formula_m2, family = binomial("logit"), data = income.df)
glm_ymodels <- list(glm_m0, glm_m1, glm_m2)

paths_glm <- paths(a = "SEX", y = "INCOME", m = mediators,
                   glm_ymodels, data = income.df, nboot = 250)

# summarize results
summary(paths_glm)

plot(paths_glm, mediator_names = c('education', 'marital status'),
     estimator = 'pure',
     horizontal = FALSE)
