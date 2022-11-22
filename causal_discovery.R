pacman::p_load(
  tidyverse,
  data.table,
  pcalg
)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("RBGL", "Rgraphviz"))


data_path <- sprintf('data/ACSIncome_NY_2018.csv')
income <- fread(data_path)

# select only features
income <- select(income, -c("TARGET"))

# compute sufficient statistics
suff.stat <- list(C = cor(income), n = nrow(income))

# get causal graph using PC algorithm
pc.income<- pc(suff.stat, indepTest = gaussCItest, labels = colnames(income),
                alpha = 0.01, skel.method = "stable.fast")
plot(pc.income, main="")

# get causal graph using FCI algorithm
fci.income <- fci(suff.stat, indepTest=gaussCItest,
               alpha = 0.01, labels = colnames(income))
plot(fci.income, main = "")

# get causal graph using GES algorithm
score <- new("GaussL0penObsScore", data = income,
             lambda = 0.5*log(nrow(data)), intercept = FALSE, use.cpp = TRUE)
ges.income <- ges(score)
plot(ges.income$essgraph, main="")



