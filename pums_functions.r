# Functions for std_error, population estimates (for categorical variables
# and weighted means for continuous variables.

# Works for any estimate, given all the weighted estimates.
std_error_from_replicate_estimates <- function(estimate, replicate_estimates) {
    pwgtp_diffs <- lapply(estimates, `-`, estimate)
    squared_diffs <- pwgtp_diffs * pwgtp_diffs
    sum.squared.diffs <- sum(squared_diffs) # now just 1 value.
    stand.err <- sqrt(sum.squared.diffs * (4.0 / 80.0))
    return(stand.err)
}

# can call in a dplyr chain with
# do(mean_estimate_with_std_error(.))
pop_estimate_with_std_error <- function(df) {
    weights = select(df, starts_with("PWGTP"))
    # Does weighted_mean for variable_col for each replicate weight.
    estimates = lapply(weights, sum)
    estimates = t(data.frame(estimates))
    estimate = estimates["PWGTP",]
    std_err = std_error_from_replicate_estimates(estimate, estimates)
    return(data.frame("pop_estimate"=estimate, std_err))
}

# Can call for any continuous variable in a dplyr chain like:
# do(mean_estimate_with_std_error(.,variable = ~ PAP))
# do(mean_estimate_with_std_error(.,variable = ~ JWMNP))
mean_estimate_with_std_error <- function(df, variable) {
    variable_col = f_eval(~ uq(variable), data = df)
    weights = select(df, starts_with("PWGTP"))
    # Does weighted_mean for variable_col for each replicate weight.
    estimates = lapply(weights, weighted.mean, x = variable_col, na.rm=TRUE)
    estimates = t(data.frame(estimates))
    estimate = estimates["PWGTP",]
    std_err = std_error_from_replicate_estimates(estimate, estimates)
    return(data.frame(estimate, std_err))
}

library(dplyr)
library(tidyr)
library(lazyeval)

#Set URL for PUMS data
# URL.PUMS.PTX <- "https://www2.census.gov/programs-surveys/acs/data/pums/2015/5-Year/csv_ptx.zip"
#
# #Set download destination
# destfile.PUMS.PTX <- "csv_ptx.zip"
#
# #Download PUMS to destination (in working directory)
# download.file(URL.PUMS.PTX, destfile.PUMS.PTX)
# print("PUMS data downloaded")
#
# #Unzip file
# unzip(destfile.PUMS.PTX)
#
# #List to identify .csv name
# unzip(destfile.PUMS.PTX, list = TRUE)
#
# ## Read csv into dataframe, load libraries
#
# PUMS.TX15 <- read.csv(file = "ss15ptx.csv", header = TRUE)

setwd("/Users/howison/Documents/UTexas/Advising/Sam/r-pums")
load("PUMS.TX15.Rdata") # saved version

test_pumas <- c("5201", "5202")
test_puma <- PUMS.TX15 %>%
            filter(PUMA10 %in% test_pumas | PUMA00 %in% test_pumas)

test_puma %>%
    group_by(SEX) %>%
    do(mean_estimate_with_std_error(.))

PUMS.TX15 %>%
do(pop_estimate_with_std_error(.))
    #       pop_estimate std_err
    # PWGTP     26538614       2

PUMS.TX15 %>%
      group_by(SEX) %>%
      do(pop_estimate_with_std_error(.))
    #       pop_estimate std_err
    # PWGTP     26538614       2

PUMS.TX15 %>%
    group_by(SEX, JWTR) %>%
    do(mean_estimate_with_std_error(.,variable = ~ JWMNP))
    # Groups: SEX, JWTR [26]
    #
    #      SEX  JWTR estimate    std_err
    #    <int> <int>    <dbl>      <dbl>
    # 1      1     1 27.06995 0.05287559
    # 2      1     2 46.88773 0.73915944
    # 3      1     3 37.85052 3.71141899
    # 4      1     4 45.07175 1.72451863
    # 5      1     5 55.30206 2.16574762
    # 6      1     6 43.92386 8.61636318
    # 7      1     7 22.95459 2.13280384
    # 8      1     8 22.26644 0.64316608
    # 9      1     9 19.10404 0.57188585
    # 10     1    10 11.18764 0.22358418
    # # ... with 16 more rows
