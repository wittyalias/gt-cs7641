
packages <- c("ggplot2", 
              "dplyr", 
              "tidyr",  
              "grid", 
              "gridExtra", 
              "stringr", 
              "C50", 
              "caret", 
              "knitr", 
              "gbm", 
              "FNN",
              "e1071")

sapply(packages, install.packages, dependencies = TRUE)
