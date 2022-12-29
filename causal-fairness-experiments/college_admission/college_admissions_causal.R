# clear workspace
rm(list = ls())

pacman::p_load(
  dplyr,
  ggplot2,
  paths
)

setwd("/Users/adam/github/causal-fairness-experiments")

admission.df <- read.csv("data/processed/admissions_v2.csv")
m <- c("Major")
mediators <- list(m)

formula_m0 <- Admission ~ Gender + Year
formula_m1 <- update(formula_m0,    ~ . + Major)

glm_m0 <- glm(formula_m0, family = binomial("logit"), data = admission.df)
glm_m1 <- glm(formula_m1, family = binomial("logit"), data = admission.df)
glm_ymodels <- list(glm_m0, glm_m1)

paths_glm <- paths(a = "Gender", y = "Admission", m = mediators, glm_ymodels, data=admission.df, nboot=250)
plot(paths_glm, mediator_names = c('Department'),
     estimator = 'pure',
     horizontal = FALSE)


