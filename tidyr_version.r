# This file shows using dplyr to calculate the estimates.

# In the end it all takes too long!

# Uses tidr.gather to move from wide to long

# ...,PWGTP,PWGTP1,...PWGTP80
# ...,1,.8,...,3

# to

# ...,replicate,weight
# ...,PWGTP,1
# ...,PWGTP1,.8
# ...,PWGTP80,3

# This lets you use group_by(replicate) and summarize to calculate
# the replicate estimates for different weight sets.

# Also: generating the standard errors is easier using this function
# Works with any replicate estimates (sum, weighted_mean, etc)
# estimate is single value (derived from PWGTP). replicate_estimates
# are one for each of PWGTP1 through PWGTP80. replicate_estimates
# could include the estimate from PWGTP itself as that will always be zero
# after subtraction.
std_error_from_replicate_estimates <- function(estimate, replicate_estimates) {
    pwgtp_diffs <- replicate_estimates$estimate - estimate

    squared_diffs <- pwgtp_diffs * pwgtp_diffs # or pwgtp_diffs ^ 2

    sum.squared.diffs <- sum(squared_diffs) # now just 1 value.

    stand.err <- sqrt(sum.squared.diffs * (4.0 / 80.0))

    return(stand.err)
}

pop_std_estimate_tidyr <- function(df) {
    df_gathered <- df %>% gather(replicate, weight, PWGTP:PWGTP80)
    replicate_estimates <- df_gathered %>%
                        group_by(replicate) %>%
                        summarize(estimate = sum(weight))

    estimate = filter(replicate_estimates, replicate == "PWGTP")$estimate

    stand.err <- std_error_from_replicate_estimates(estimate, replicate_estimates)

    return(data.frame("pop_estimate"=estimate, stand.err))
}

# These next two don't work, due to lazyeval difficulties.
#
# mean_std_estimate_tidyr <- function(df, variable = ~ JWMNP) {
#     extra_args <- list(~ weight, na.rm = TRUE)
#     df_gathered <- df %>% gather(replicate, weight, PWGTP:PWGTP80)
#     replicate_estimates <- df_gathered %>%
#                         group_by(replicate) %>%
#                         summarize_(estimate = f_interp(~ weighted.mean(uq(variable), uqs(extra_args))))
#
#     estimate = filter(replicate_estimates, replicate == "PWGTP")$estimate
#
#     stand.err <- std_error_from_replicate_estimates(estimate, replicate_estimates)
#
#     return(data.frame(estimate, stand.err))
# }



# Hoped this would work for any function.
# std_estimate_tidyr <- function(df, FUN = ~ weighted.mean(JWMNP, weight, na.rm = TRUE)) {
#     extra_args <- list(~ weight, na.rm = TRUE)
#     df_gathered <- df %>% gather(replicate, weight, PWGTP:PWGTP80)
#     replicate_estimates <- df_gathered %>%
#                         group_by(replicate) %>%
#                         summarize(estimate = uq(FUN))))
#
#     estimate = filter(replicate_estimates, replicate == "PWGTP")$estimate
#
#     stand.err <- std_error_from_replicate_estimates(estimate, replicate_estimates)
#
#     return(data.frame(estimate, stand.err))
# }

library(dplyr)
library(tidyr)
library(lazyeval)

setwd("/Users/howison/Documents/UTexas/Advising/Sam/r-pums")
load("PUMS.TX15.Rdata")

test_pumas <- c("5201", "5202")

test_puma <- PUMS.TX15 %>%
            filter(PUMA10 %in% test_pumas | PUMA00 %in% test_pumas)

test_puma %>%
    select(-starts_with("PWGTP"),starts_with("PWGTP")) %>%
    group_by(SEX) %>%
    do(pop_std_estimate_tidyr(.))

PUMS.TX15 %>%
select(-starts_with("PWGTP"),starts_with("PWGTP")) %>%
group_by(SEX) %>%
do(pop_std_estimate_tidyr(.))
