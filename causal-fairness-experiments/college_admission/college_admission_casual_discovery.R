pacman::p_load(
  tidyverse,
  data.table,
  pcalg
)
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("RBGL", "Rgraphviz"))


data_path <- sprintf('data/processed/admissions.csv')
admission <- fread(data_path)

# select only features
admission <- select(admission, -c("Admission"))

# compute sufficient statistics
suff.stat <- list(C = cor(admission), n = nrow(admission))

# get causal graph using PC algorithm
pc.admission<- pc(suff.stat, indepTest = gaussCItest, labels = colnames(admission),
                alpha = 0.01, skel.method = "stable.fast")
plot(pc.admission, main="")