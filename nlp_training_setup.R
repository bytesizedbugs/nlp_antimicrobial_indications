# Load the required libraries for the analysis

required_packages <- c("readxl", "tm", "plyr", "dplyr", "stringr", "tidytext",
                       "caret", "xgboost", "rpart.plot", "tictoc", "NLP", "stringdist", "pROC", "DiagrammeR", "ggplot2")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(readxl)
library(tm)
library(plyr)
library(dplyr)
library(stringr)
library(tidytext)
library(caret)
library(xgboost)
library(rpart.plot)
library(tictoc)
library(NLP)
library(stringdist)
library(pROC)
library(DiagrammeR)
library(ggplot2)
