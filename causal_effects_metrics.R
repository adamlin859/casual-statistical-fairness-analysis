# clear workspace
rm(list = ls())

pacman::p_load(
  dplyr,
  ggplot2,
  forcats,
  lavaan
)

setwd("~/Documents/GitHub/casual-statistical-fairness-analysis")
income.df <- read.csv("data/ACSIncome_NY_2018.csv")

# filter out a select number of features
income.df <- select(income.df, c("AGEP", "COW", "SCHL", "MAR", "WKHP", "SEX", "TARGET"))

# rename target column
income.df <- rename(income.df, INCOME = TARGET)

income.df <- income.df %>% mutate(SEX = recode(SEX, `1`=0, `2`=1)) # male is 0, female is 1

# model specifications
model <- '
# equation where marital status is predicted by age and sex
MAR ~ AGEP + a*SEX
# equation where education is predicted by sex
SCHL ~ c*SEX
# equation where income is predicted by marital, sex, and education
INCOME ~ b*MAR + e*SEX + d*SCHL
# estimating variance of exogenous variables
AGEP ~~ AGEP
SEX ~~ SEX
# estimating residual variances of endogenous variables
MAR ~~ MAR
SCHL ~~ SCHL
INCOME ~~ INCOME
# calculating specific indirect effect 1: sex (a) --> marital --> (b) income
SIE1 := a*b
# calculating specific indirect effect 2: sex --> (c) education --> (d) income
SIE2 := c*d
# calculating total indirect effect via mediators
TIE := SIE1 + SIE2
# calculating total direct effect
DE := e 
# calculating total effect of sex on income (plus direct effect path (e))
TE := TIE + e'

# fit the model
fit <- lavaan(model, data=income.df, se='bootstrap')

summary(fit, fit.measures= TRUE, standardized=TRUE, rsquare=TRUE)

# obtain confidence intervals
parameterEstimates(fit)

# had to copy the results from the parameterEstimates
x <- c("TE", "DE", "TIE", "IE via Marital", "IE via Education")
y <- c(-0.112, -0.132, 0.019, -0.005, 0.024)
lower.ci <- c(-0.118, -0.137, 0.017, -0.006, 0.022)
upper.ci <- c(-0.107, -0.126, 0.022, -0.004, 0.026)

metrics <- data.frame(x, y, lower.ci, upper.ci)
metrics$x <- as.factor(metrics$x)
metrics <- metrics %>% mutate(x = fct_relevel(x, "TE", "DE", "TIE", "IE via Marital", "IE via Education"))

ggplot(metrics, aes(x, y)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower.ci, ymax = upper.ci), width=0.2) + 
  geom_hline(yintercept = 0, linetype = "dotted") + 
  labs(x = "Causal-Fairness Measures",
       y = "Causal Effect Estimation") +
  theme_minimal()






