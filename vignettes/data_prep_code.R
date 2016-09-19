# Prepping the data

# To begin, load in the code for the following programs:
library(readxl)
library(nnet)
library(MASS)
library(lm.beta)
library(AICcmodavg)
library(glmulti)
library(rJava)
setwd("/Users/megevansen/Desktop")
# read_excel("working_copy.xlsx", na = "NA")

dat <- read_excel("working_copy.xlsx")
dim(dat)
dat <- dat[c(1:366), ]

# This is to convert variables to numeric
to_num <- c(5,13,18:37,41:47,50,52,54,57:60)
for (i in to_num) {
  dat[[i]] <-as.numeric(dat[[i]])
}

# This is to convert variables to factor
to_fac <- c(4,6,15:17,38:40,48,49,51,53,56)
for (i in to_fac) {
  dat[[i]] <-as.factor(dat[[i]])
}

# To save the file
save(dat, file="working_data.RData")

# To load the data back in
#load("working_data.RData")

