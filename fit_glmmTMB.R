# ---------------------------------------------------------------------------- #
# load necessary packages, set working directory, import and view data
# ---------------------------------------------------------------------------- #
rm(list = ls()); gc()
library("glmmTMB")
library("lmerTest")
library("Matrix")
library("tidyverse")
library("fastDummies")

options(scipen = 99)
set.seed(5543)

setwd("/Users/Desktop/")
dlong <- read.csv("cleaned_data.csv") 
head(dlong, 10)
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 0: Naive Approach Ignoring Changes in Group Membership (Pure 3-level Clustering)
# ---------------------------------------------------------------------------- #
# fit model
m0 <- glmmTMB(math ~ time + female + ses + public + suspension + platform + 
                (1 | school) + 
                (1 + time | school:student),
              data = dlong, REML = TRUE)

# view parameter estimates
summary(m0)

# view random effects
print(VarCorr(m0), comp = "Variance")
VarCorr(m0)$cond$school
VarCorr(m0)$cond$`school:student`
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 1: Acute Effects CCREM
# ---------------------------------------------------------------------------- #
# fit model
m1 <- glmmTMB(math ~ time + female + ses + public + suspension + platform + 
                (1 | school) + 
                (1 + time | student),
              data = dlong, REML = TRUE)

# view parameter estimates
summary(m1)

# view random effects
print(VarCorr(m1), comp = "Variance")
VarCorr(m1)$cond$school
VarCorr(m1)$cond$student
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 2: Cumulative Effects CCREM
# ---------------------------------------------------------------------------- #
# to construct school Z matrix of cumulative effects, first create dummy 
# indicators for each school
dummy <- fastDummies::dummy_cols(dlong$school)
dummy <- dummy[,-1]
colnames(dummy) <- paste0("d", 1:length(unique(dlong$school)))

# view dummy matrix
head(dummy)

# make weight indicators for each school
weight <- ifelse(dummy == 1, 1, NA)
colnames(weight) <- paste0("w", 1:length(unique(dlong$school)))

# view weight matrix
head(weight)

# add dummy and weight indicators as new columns to original data
dlong <- cbind(dlong, dummy, weight)

# for cumulative weights, fill downward consecutively 
dlong <- dlong %>%
  dplyr::group_by(student) %>%
  tidyr::fill(w1:paste0("w", length(unique(dlong$school))), .direction="down") %>% 
  ungroup()
dlong[,grep("^w", colnames(dlong))] <- ifelse(is.na(dlong[,grep("^w", colnames(dlong))]) == TRUE, 0, 1)
dlong <- data.frame(dlong)

# remove unnecessary items, and view data with new variables
rm(dummy,weight)
head(dlong)

# the following is the formula used for group random effects
ranef_formula <- paste0(paste0("w",1:length(unique(dlong$school))), collapse = "+")
ranef_formula

# need to define dummy (factor) variable == 1
dlong$DummyOf1s <- factor(1)

# fit model
m2 <- glmmTMB(as.formula(paste0("math ~ time + female + ses + public + suspension + platform + 
                                homdiag(",ranef_formula," | DummyOf1s) + 
                                (1 + time | student)")), 
              data = dlong, REML = TRUE)

# view parameter estimates
summary(m2)

# view random effects
print(VarCorr(m2), comp="Variance")
VarCorr(m2)$cond$DummyOf1s
VarCorr(m2)$cond$student
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 3: Cumulative Effects CCREM with Standardized Weights
# ---------------------------------------------------------------------------- #
# keep same weights from previous Model 2, but ensuring weights are proportional 
# such that weights sum to 1 for each row of the weight matrix

# first, count the number of weights == 1 in a given row
dlong$tot_w <- rowSums(dlong[,grep("^w", colnames(dlong))])

# then, each weight gets divided by the row total
dlong[,grep("^w",colnames(dlong))] <- lapply(
  dlong[,grep("^w",colnames(dlong))], 
  function(x) x / dlong[,"tot_w"]
)

# view data
tail(dlong, 10)

# need to define dummy (factor) variable == 1
dlong$DummyOf1s <- factor(1)

# fit model
m3 <- glmmTMB(as.formula(paste0("math ~ time + female + ses + public + suspension + platform + 
                                homdiag(",ranef_formula," | DummyOf1s) + 
                                (1 + time | student)")), 
              data = dlong, REML = TRUE)

# view parameter estimates
summary(m3)

# view random effects
print(VarCorr(m3), comp="Variance")
VarCorr(m3)$cond$DummyOf1s
VarCorr(m3)$cond$student
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 4: Acute Effects CCREM with Autoregressive Group Effects
# ---------------------------------------------------------------------------- #
# fit model
m4 <- glmmTMB(math ~ time + female + ses + public + suspension + platform + 
                ar1(0 + factor(time) | school) + 
                (1 + time | student),
              data = dlong, REML = TRUE)

# view parameter estimates
summary(m4)

# view random effects
print(VarCorr(m4), comp="Variance")
VarCorr(m4)$cond$school
VarCorr(m4)$cond$student
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 5: Acute Effects CCREM with Compound Symmetric Group Effects
# ---------------------------------------------------------------------------- #
# fit model
m5 <- glmmTMB(math ~ time + female + ses + public + suspension + platform + 
                cs(0 + factor(time) | school) + 
                (1 + time | student),
              data = dlong, REML = TRUE, 
              map = list(theta = factor(c(1,1,1,1,4,5,6,7)))) 

# view parameter estimates
summary(m5)

# view random effects
print(VarCorr(m5), comp="Variance")
VarCorr(m5)$cond$school
VarCorr(m5)$cond$student
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 6: Acute Effects CCREM with Unstructured Group Effects
# ---------------------------------------------------------------------------- #
# fit model
m6 <- glmmTMB(math ~ time + female + ses + public + suspension + platform + 
                (0 + as.factor(time) | school) + 
                (1 + time | student),
              data = dlong, REML = TRUE)

# view parameter estimates
summary(m6)

# view random effects
print(VarCorr(m6), comp="Variance")
VarCorr(m6)$cond$school
VarCorr(m6)$cond$student
# ---------------------------------------------------------------------------- #
