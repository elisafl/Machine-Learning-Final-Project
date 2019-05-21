# Load packages
library(glmnet)
library(data.table)
library(ggplot2)
library(knitr)
library(ROCR)
library(gridExtra)
library(rstudioapi)

# Prepare
rm(list = ls())
set.seed(132435)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### 1. Load the data and summarize.

data.full <- read.csv("train.csv") 
data.full <- data.full[sample(nrow(data.full)), -c(1, 3)] # drop irrelevant variables for analysis, randomize data
missing_prop <- sapply(data.full, function(x) {mean(is.na(x))}) 
data.full <- data.full[missing_prop <= 0] # drop varaiables with missing data
data.train <- data.full[1:floor(nrow(data.full) * 0.8), ] # training set
data.test <- data.full[ceiling(nrow(data.full) * 0.8):nrow(data.full), ] # test set

# Descriptive statistics
library(broom)
linear_model <- lm(depressed ~., data = data.train)
sum_linear_model <- summary(linear_model)
d <- data.frame(sum_linear_model$coefficients)
colnames(d) <- c("Estimate", "Std. Error", "t-Value", "p-Value")
d$Significance <- ifelse(d$`p-Value` < 0.01, "***", ifelse(d$`p-Value` < 0.05, "**", ifelse(d$`p-Value` < 0.1, "*","")))
write.csv(d, "output/linear_model.csv")
