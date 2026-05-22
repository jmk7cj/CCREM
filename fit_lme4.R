# ---------------------------------------------------------------------------- #
# load necessary packages, set working directory, import and view data
# ---------------------------------------------------------------------------- #
rm(list = ls()); gc()
library("lme4")
library("lmerTest")

options(scipen = 99)
set.seed(5543)

setwd("/Users/jokush/Desktop/jmk/IXL/Research/ccrem/")
dlong <- read.csv("cleaned_data.csv") 
head(dlong, 10)
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 0: Naive Approach Ignoring Changes in Group Membership (Pure 3-level Clustering)
# ---------------------------------------------------------------------------- #
# fit model
m0 <- lmer(math ~ time + female + ses + public + suspension + platform + 
             (1 | school) + 
             (1 + time | school:student),
           data = dlong, REML = TRUE)

# view parameter estimates
summary(m0)

# view random effects
print(VarCorr(m0), comp = "Variance")

# covariance matrix of student random effects
as.matrix(bdiag(VarCorr(m0)[1]))

# covariance matrix of school random effects
as.matrix(bdiag(VarCorr(m0)[2]))
# ---------------------------------------------------------------------------- #

library("tidyr")
library("dplyr")
wide <- pivot_wider(
  data = dlong,
  id_cols = student,
  names_from = time,
  values_from = c(school)
)
colnames(wide) <- c("student","first_school","b","c","d")
wide <- wide[,1:2]
head(wide)
dlong <- inner_join(
  x = dlong,
  y = wide,
  by = join_by(student)
)
head(dlong)


gg <- lmer(math ~ time + female + ses + public + suspension + platform + 
             #(1 | first_school) + 
             #(1 + time | first_school:student),
             (1 | first_school) + 
             (1 + time | student),
           data = dlong, REML = TRUE)
# view parameter estimates
summary(m0)
summary(gg)

# view random effects
print(VarCorr(m0), comp = "Variance")
print(VarCorr(gg), comp = "Variance")

# covariance matrix of student random effects
as.matrix(bdiag(VarCorr(m0)[1]))
as.matrix(bdiag(VarCorr(gg)[1]))

# covariance matrix of school random effects
as.matrix(bdiag(VarCorr(m0)[2]))
as.matrix(bdiag(VarCorr(gg)[2]))


zzz <- getME(m0, "Z")
zzz
zzz@Dim

ztab <- as.matrix(table(dlong$student, dlong$school))
length(unique(dlong$student))
length(unique(dlong$school))


table(ztab)
437 + 217 + 26 + 225

# ---------------------------------------------------------------------------- #
# Model 1: Acute Effects CCREM
# ---------------------------------------------------------------------------- #
# fit model
m1 <- lmer(math ~ time + female + ses + public + suspension + platform + 
             (1 | school) + 
             (1 + time | student),
           data = dlong, REML = TRUE)

# view parameter estimates
summary(m1)

# view random effects
print(VarCorr(m1), comp = "Variance")

# covariance matrix of student random effects
as.matrix(bdiag(VarCorr(m1)[1]))

# covariance matrix of school random effects
as.matrix(bdiag(VarCorr(m1)[2]))
# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 2: Cumulative Effects CCREM
# ---------------------------------------------------------------------------- #

# not able to implement

# ---------------------------------------------------------------------------- #



# ---------------------------------------------------------------------------- #
# Model 3: Cumulative Effects CCREM with Standardized Weights
# ---------------------------------------------------------------------------- #

# not able to implement

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
# fit model
m6 <- lmer(math ~ time + female + ses + public + suspension + platform + 
             (0 + as.factor(time) | school) + 
             (1 + time | student),
           data = dlong, REML = TRUE)

# view parameter estimates
summary(m6)

# view random effects
print(VarCorr(m6), comp = "Variance")

# covariance matrix of student random effects
as.matrix(bdiag(VarCorr(m6)[1]))

# covariance matrix of school random effects
as.matrix(bdiag(VarCorr(m6)[2]))
# ---------------------------------------------------------------------------- #