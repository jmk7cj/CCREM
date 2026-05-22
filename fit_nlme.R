# ---------------------------------------------------------------------------- #
# load necessary packages, set working directory, import and view data
# ---------------------------------------------------------------------------- #
rm(list = ls()); gc()
library("nlme")
library("lmerTest")
library("Matrix")
library("tidyverse")
library("fastDummies")

options(scipen = 99)
set.seed(5543)

# define function to extract random effects covariance matrices
get_re_covariance_matrix <- function(model) {
  
  # extract SDs
  school_sd <- unique(attr(corMatrix(model$modelStruct[[1]])[[1]], which = "stdDev"))
  student_sd <- unique(attr(corMatrix(model$modelStruct[[1]])[[2]], which = "stdDev"))
  
  # multiply by residual
  school_sd <- school_sd * model$sigma
  student_sd <- student_sd * model$sigma
  
  # square to calculate variance
  school_var <- school_sd^2
  student_var <- student_sd^2
  
  # calculate covariance b/w intercept and slope for student
  student_correlation <- corMatrix(model$modelStruct[[1]])[[2]][2,1]
  student_covar <- (student_sd[1] * student_sd[2]) * student_correlation
  
  school_matrix <- matrix(school_var)
  student_matrix <- matrix(c(student_var[1],
                             student_covar,
                             student_covar,
                             student_var[2]),
                           nrow = 2, ncol = 2, byrow = FALSE)
  
  
  return(list("school_covariance_matrix" = school_matrix,
              "student_covariance_matrix" = student_matrix,
              "residual_variance" = (model$sigma)^2))
}

setwd("/Users/Desktop/")
dlong <- read.csv("cleaned_data.csv") 
head(dlong, 10)
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 0: Naive Approach Ignoring Changes in Group Membership (Pure 3-level Clustering)
# ---------------------------------------------------------------------------- #
# fit model
m0 <- lme(math ~ time + female + ses + public + suspension + platform,
    random = list(
      school =~ 1,
      student =~ 1 + time
    ),
    data = dlong, method = "REML")


# view parameter estimates
summary(m0)

# view random effects
print(VarCorr(m0), comp = "Variance")
get_re_covariance_matrix(m0)
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 1: Acute Effects CCREM
# ---------------------------------------------------------------------------- #
# first define dummy (factor) variable == 1
dlong$DummyOf1s <- factor(1)

# fit model
m1 <- lme(math ~ time + female + ses + public + suspension + platform,
          random = list(
            DummyOf1s = pdIdent(~ 0 + factor(school)),
            student = pdSymm((~ 1 + time))
          ),
          data = dlong, method = "REML")


# view parameter estimates
summary(m1)

# view random effects
print(VarCorr(m1), comp = "Variance")
get_re_covariance_matrix(m1)
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
reformulate(ranef_formula)

# need to define dummy (factor) variable == 1
dlong$DummyOf1s <- factor(1)

# fit model
m2 <- lme(math ~ time + female + ses + public + suspension + platform,
          random = list(
            DummyOf1s = pdIdent(reformulate(ranef_formula)),
            student = pdSymm(~ 1 + time)
          ),
          data = dlong, method = "REML")

# view parameter estimates
summary(m2)

# view random effects
get_re_covariance_matrix(m2)
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
m3 <- lme(math ~ time + female + ses + public + suspension + platform,
          random = list(
            DummyOf1s = pdIdent(reformulate(ranef_formula)),
            student = pdSymm(~ 1 + time)
          ),
          data = dlong, method = "REML")

# view parameter estimates
summary(m3)

# view random effects
get_re_covariance_matrix(m3)
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 4: Acute Effects CCREM with Autoregressive Group Effects
# ---------------------------------------------------------------------------- #

# not able to implement

# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 5: Acute Effects CCREM with Compound Symmetric Group Effects
# ---------------------------------------------------------------------------- #

# not able to implement

# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 6: Acute Effects CCREM with Unstructured Group Effects
# ---------------------------------------------------------------------------- #

# not able to implement

# ---------------------------------------------------------------------------- #
