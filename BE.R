# install packages if required
requiredPackages <- c("readxl, nlme")
for (package in requiredPackages) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}


# Cmax estimation

# upload packges
library(readxl)
library(nlme)
# temporarily turn off warnings
options(warn=0)

# Import and data preprocessing (set working directory)
setwd('/Users/valer/Desktop/R_project/')
path_ <- getwd()
path_ <- "C:/Users/valer/Desktop/R_project/project 4/total.xlsx"

# loading data containing test and reference datasheets 
loading_data <- function(path_, sheet_) {
  # reading file
  data <- read_excel(path = path_, sheet = sheet_)
  return (data)
}

# dataset for drug product (dataset for Cmax)
data_test <- loading_data(path_, "Cmax_")

# select columns
cols <- c('Subject', 'Sequence', 'Period', 'Treatment')
data_test[,cols] <- data.frame(apply(data_test[cols], 2, as.factor))


# R formula for the model
Result = lme(log(Cmax) ~ Sequence + Period + Treatment, random=~1|Subject,
             data = data_test)
summary(Result)
# variances from the standard deviation
VarCorr(Result)
# 90% of log scale difference
ci = intervals(Result, 0.9)

# 90% CI of GMR
exp(ci$fixed["TreatmentT", ])


# AUCt estimation

# dataset for the drug product dataset for AUC(0-t)
data_test <- loading_data(path_, "AUCt_")

# select columns
cols <- c('Subject', 'Sequence', 'Period', 'Treatment')
data_test[,cols] <- data.frame(apply(data_test[cols], 2, as.factor))


# R formula for the model
Result = lme(log(AUCt) ~ Sequence + Period + Treatment, random=~1|Subject,
             data = data_test)
summary(Result)
# variances from the standard deviation
VarCorr(Result)
# 90% of log scale difference
ci = intervals(Result, 0.9)


# 90% CI of GMR
exp(ci$fixed["TreatmentT", ])


# AUCinf estimation

# dataset for the drug product dataset for AUCinf
data_test <- loading_data(path_, "AUCinf_")

# select columns
cols <- c('Subject', 'Sequence', 'Period', 'Treatment')
data_test[,cols] <- data.frame(apply(data_test[cols], 2, as.factor))


# R formula for the model
Result = lme(log(AUCinf) ~ Sequence + Period + Treatment, random=~1|Subject,
             data = data_test)
summary(Result)
# variances from the standard deviation
VarCorr(Result)
# 90% of log scale difference
ci = intervals(Result, 0.9)


# 90% CI of GMR
exp(ci$fixed["TreatmentT", ])