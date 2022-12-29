# clear workspace
rm(list = ls())

pacman::p_load(
  dplyr,
  ggplot2,
  paths
)


setwd("/Users/adam/github/causal-fairness-experiments")
income.df <- read.csv("data/processed/asc_pre.csv")


m1 <- c("Education")
m2 <- c("marital")
mediators <- list(m1, m2)

# NOTE: adding squared interaction term to continuous features
formula_m0 <- INCOME ~ sex + Age + workclass
formula_m1 <- update(formula_m0,    ~ . + Education) # adding education mediator
formula_m2 <- update(formula_m1,    ~ . + marital) # adding marital mediator


glm_m0 <- glm(formula_m0, family = binomial("logit"), data = income.df)
glm_m1 <- glm(formula_m1, family = binomial("logit"), data = income.df)
glm_m2 <- glm(formula_m2, family = binomial("logit"), data = income.df)
glm_ymodels <- list(glm_m0, glm_m1, glm_m2)

paths_glm <- paths(a = "sex", y = "INCOME", m = mediators,
                   glm_ymodels, data = income.df, nboot = 250)

# summarize results
summary(paths_glm)

plot(paths_glm, mediator_names = c('education', 'marital status'),
     estimator = 'pure',
     horizontal = FALSE)