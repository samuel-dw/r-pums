## This is an example script for joining a PUMS data dictionary onto PUMS data for clarity and ease of use.

## This script makes a number of assumptions:
## (1) You have already run James Howison's python script to create the PUMS data dictionary in csv format;
## (2) This data dictionary is in your working directory; and
## (3) You have already loaded your state person-record PUMS file (in this example, PUMS.TX15)

## Note: user-defined functions are included at the end of this script (see, End Note: UD FUNCTIONS)


############## Step 1: Load libraries ##############

library(dplyr)

############## Step 2: Load data      ##############

test_data <- PUMS.TX15 %>%
  group_by(SEX) %>%
  do(estimate_with_std_err(.)) 

## See End note for UD function.
## SEX is an arbitary variable, useful because the Census provides verifications of estimates.

glimpse(test_data) 

## We will glimpse the data after each step to ensure the code is working correctly.
## From this glimpse, we see that the SEX variable is coded as a integer with two values: 1 and 2.

data_dictionary <- read.csv(file = "code_dictionary.csv", header = TRUE, blank.lines.skip = TRUE)

glimpse(data_dictionary)

## Notice that all variables are coded as factors. 
## This has to be changed so that the data_dictionary columns can be matched to the test_data columns.
## We will do this in a later step.

############## Step 3: Filter data    ############## 

## This is done for ease of review.

filtered_dd <- data_dictionary %>%
  filter(title == 'SEX') 

glimpse(filtered_dd)

## Notice that the id column corresponds to the SEX column from the test_data. 
## The id column from the data dictionary cannot be joined to the SEX column from the test_data,
## because they are incompatible types (factor/integer). Hence, Step 4:

############## Step 4: Transform data ##############

filtered_dd$id <- as.numeric(as.character(filtered_dd$id))

glimpse(filtered_dd) 

## The id column is now numberic, and can be joined.

## Note: Hadley suggests a different method of doing this through the lapply function.
## This method would look *something* like this:
## filtered_dd[] <- lapply(filtered_dd, as.character)
## (But, there would be one more step to the above call (changing character to numeric).)

############### Step 5: Join data    ##############

joined_f_dd <- left_join(test_data, filtered_dd, by = c("SEX" = "id"), copy = TRUE)

glimpse(joined_f_dd)

## Now, the variable name corresponds to the SEX id.

############### Next steps / other considerations    ##############

## The above can also be achieved through the following:

test_data$SEX <- filtered_dd$variable_name[match(test_data$SEX, filtered_dd$id)]

glimpse(test_data)

## This changes SEX in test_data to the corresponding variable names in the data_dictionary.
## Though this appears optimal, it is not clear if it could be expanded.

## Next steps:
## (1) Review James Howison's work on NSE to be able to call groups by variable name.
## (2) Edit above functions to call groups by variable name 
## (3) Remove title, title_note from join.
## (4) Replace grouped-variable values with variable_names



############### End note: UD Functions    ##############

stand_err_from_replicate_est <- function(data, estimate){
  # data has PWGTP1 through PWGTP80
  # estimate is a single value (e.g., from sum(PWGTP))
  just_pwgtp <- data %>% select(PWGTP1:PWGTP80)
  
  # sum up each of the 80 columns, now have just 80 values.
  # whatever method used to create estimate done to each of the 80 colums.
  # here estimate done with sum(PWGTP) so we can use colSums.
  # if we'd justed weighted_mean would have to use that on each.
  pwgtp_sums <- colSums(just_pwgtp)
  
  # subtract the estimate from each, still have 80 values.
  pwgtp_diffs <- pwgtp_sums - estimate
  
  # square those, still have 80 values
  squared_diffs <- pwgtp_diffs * pwgtp_diffs # or pwgtp_diffs ^ 2
  
  sum_squared_diffs <- sum(squared_diffs) # now just 1 value.
  
  stand_err <- sqrt(sum_squared_diffs * (4.0 / 80.0)) # just 1 value.
  
  return(stand_err)
}

estimate_with_std_err <- function(data){ 
  estimate = sum(data$PWGTP)
  std_err = stand_err_from_replicate_est(data, estimate)
  return(data.frame(estimate, std_err))
}

