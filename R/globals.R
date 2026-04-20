# Questo file serve solo a eliminare i NOTE del R CMD check
# causati dall'uso di colonne nei pacchetti Tidyverse o Bioconductor

utils::globalVariables(c(
  "patient_id",
  "cluster_id",
  "p_adj",
  "group",
  "value"
))
