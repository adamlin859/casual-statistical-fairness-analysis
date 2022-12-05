install.packages("paths")

library(paths)

library(dplyr)
library(gbm)

setwd("~/Github/casual-statistical-fairness-analysis/")
income <- read.csv("data/ACSIncome_NY_2018.csv")

income <- income %>% mutate(SEX=recode(SEX, 
                         `1`=0,
                         `2`=1))

m1 <- c("COW", "SCHL", "OCCP")
m2 <- c("POBP", "RELP", "WKHP")
mediators <- list(m1, m2)


formula_m0 <- TARGET ~ MAR + AGEP +  SEX 
formula_m1 <- update(formula_m0,    ~ . + COW + SCHL + OCCP)
formula_m2 <- update(formula_m1,    ~ . + POBP + RELP + WKHP)

gbm_m0 <- gbm(formula_m0, data = income, distribution = "bernoulli",
              interaction.depth = 3)
gbm_m1 <- gbm(formula_m1, data = income, distribution = "bernoulli",
              interaction.depth = 3)
gbm_m2 <- gbm(formula_m2, data = income, distribution = "bernoulli",
              interaction.depth = 3)

gbm_ymodels <- list(gbm_m0, gbm_m1, gbm_m2)

income_paths <- paths(a = "SEX", y = "TARGET", m = mediators,
                      gbm_ymodels, data = income, nboot = 250)

# summarize results
summary(income_paths)

#####################################################
# Example 1: Issue Framing Effects
#####################################################
# variable names
x <- c("gender1", "educ1", "polint1", "ideo1", "know1", "value1")
a <- "ttt"
m1 <- c("W1", "W2")
m2 <- c("M1","M2","M3","M4","M5")
y <- "Y"
m <- list(m1, m2)


form_m0 <- as.formula(paste0(y, "~", a))
form_m1 <- as.formula(paste0(y, "~", paste0(c(x, a, m1), collapse = "+")))
form_m2 <- as.formula(paste0(y, "~", paste0(c(x, a, m1, m2), collapse = "+")))


lm_m0 <- lm(form_m0, data = welfare)
# GBM outcome models
gbm_m1 <- gbm(form_m1, data = welfare, distribution = "gaussian",
              interaction.depth = 3)
gbm_m2 <- gbm(form_m2, data = welfare, distribution = "gaussian",
              interaction.depth = 3)

gbm_ymodels <- list(lm_m0, gbm_m1, gbm_m2)
# causal paths analysis
welfare_paths <- paths(a, y, m, models = gbm_ymodels,
                       data = welfare, nboot = 250)
# summarize results
summary(welfare_paths)


# Example 2: The Legacy of Political Violence
#####################################################
# K=3 causally ordered mediators
m1 <- c("trust_g1", "victim_g1", "fear_g1")
m2 <- c("trust_g2", "victim_g2", "fear_g2")
m3 <- c("trust_g3", "victim_g3", "fear_g3")
mediators <- list(m1, m2, m3)

# outcome model formulas
formula_m0 <- annex ~ kulak + prosoviet_pre + religiosity_pre + land_pre +
  orchard_pre + animals_pre + carriage_pre + otherprop_pre + violence
formula_m1 <- update(formula_m0, ~ . + trust_g1 + victim_g1 + fear_g1)
formula_m2 <- update(formula_m1, ~ . + trust_g2 + victim_g2 + fear_g2)
formula_m3 <- update(formula_m2, ~ . + trust_g3 + victim_g3 + fear_g3)

# outcome models
gbm_m0 <- gbm(formula_m0, data = tatar, distribution = "bernoulli",
              interaction.depth = 3)
gbm_m1 <- gbm(formula_m1, data = tatar, distribution = "bernoulli",
              interaction.depth = 3)
gbm_m2 <- gbm(formula_m2, data = tatar, distribution = "bernoulli",
              interaction.depth = 3)
gbm_m3 <- gbm(formula_m3, data = tatar, distribution = "bernoulli",
              interaction.depth = 3)

gbm_ymodels <- list(gbm_m0, gbm_m1, gbm_m2, gbm_m3)
# causal paths analysis using gbm
tatar_paths <- paths(a = "violence", y = "annex", m = mediators,
                     gbm_ymodels, data = tatar, nboot = 250)


# propensity score model via gbm
formula_ps <- violence ~ kulak + prosoviet_pre + religiosity_pre + land_pre +
  orchard_pre + animals_pre + carriage_pre + otherprop_pre
gbm_ps <- gbm(formula_ps, data = tatar, distribution = "bernoulli",
              interaction.depth = 3)
# causal paths analysis using both the pure imputation estimator and
# the imputation-based weighting estimator
tatar_paths2 <- paths(a = "violence", y = "annex", m = mediators,
                      ps_model = gbm_ps, gbm_ymodels, data = tatar, nboot = 250)
# plotting PSEs
plot(tatar_paths2, mediator_names = c("G1 identity", "G2 identity", "G3 identity"),
     estimator = "both")
# sensitivity analysis for the path-specific effect via M1
sens_paths <- sens(tatar_paths, confounded = "M1", estimand = "via M1",
                   gamma_values = - seq(0, 0.5, 0.002), eta_values = seq(-0.5, 0.5, 0.002))
plot(sens_paths)
# summarize results
summary(tatar_paths)
