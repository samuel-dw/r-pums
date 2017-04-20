### NOTE: presumes all data and packages from previous script already loaded
### Some functions are included at the end the this script, for reference purposes.

# From ?do documentation: 
# "Groups are preserved for a single unnamed input. 
# This is different to summarise because do generally does not reduce the complexity of the data, it just expresses it in a special way."

# So, when summarise is called on data, it reduces that data to summaries.
# This means it can be difficult for summarized data to be used to perform other calculations (such as SE!)

# Step 1: derive estimate that preserves complexity of data

campo.males.est <- campo_pums %>% 
  filter(SEX == 1) %>%
  do(., summarise(., population = sum(PWGTP))) # Note: have to call "." twice becuase, normally, first "." is assumed with %>% operator

# The above appears to work, but we want a more robust function. 

campo.modes.by.sex.est <- campo_pums %>%
  group_by(SEX) %>%
  group_by(JWTR, add = TRUE) %>%
  do(., summarise(., population = sum(PWGTP))) 

# The above does not exactly work. Instead of being grouped by SEX and then JWTR, it instead just summarizes JWTR.  Maybe ad "do" to the second group?

campo.modes.by.sex.est <- campo_pums %>%
  group_by(SEX) %>%
  do(., group_by(JWTR)) %>%
  do(., summarise(., population = sum(PWGTP))) # Does not work. Error: no applicable method for 'group_by_' applied to an object of class "c('integer', 'numeric')"

# From the above error, it sounds like group_by must be applied to an object; therefore, would need to split the above functions. (Let's save this for later)

###

# Let's switch gears and attempt to incorporate "do" into past functions

campo.modes.by.sex.est.as.values <- campo.modes.by.sex.est[,2] # To transform df in to values necessary for SE function. Note: still preseved as df, here :(

# "Data" needs to be in similar format for function

campo.modes.by.sex.data <- campo_pums %>%
  group_by(SEX) %>%
  group_by(JWTR) 

# Now, have data and estimate necessarty for SE function.
# Question is, do we need to call "do" in function?

# First, let's try it without do and see what happens.

stand.err.from.replicate.est(campo.modes.by.sex.data, campo.modes.by.sex.est.as.values) # Does not work. "Adding missing grouping variables: `JWTR`"

# Necessary to call with "do"?

do(campo.modes.by.sex.data, stand.err.from.replicate.est(campo.modes.by.sex.data, campo.modes.by.sex.est.as.values)) # Errors: adds missing varibales, plus df not at positions...

# Maybe change function?

race.ethnicity.sex.by_mode <- function(race.select, ethnicity.select, sex.select){ 
  mres <-   campo_pums %>%
    filter(RAC1P == race.select) %>% # White = 1, Black = 2, Other = 8, Two or more = 9
    filter(HISP == ethnicity.select) %>% # 01 = Not Hispanic, everything else is
    filter(SEX == sex.select) %>% # 1 is male, 2 is female
    group_by(JWTR) %>%
    do(., summarise(., population = sum(PWGTP)))
       return(data.frame(mres))
}

race.ethnicity.sex.by_mode(1,01,1) # Works.



mode.by_race.by_ethnicity.by_sex <- function(mode.select){ 
  mres2 <-   campo_pums %>%
    filter(JWTR == mode.select) %>% # White = 1, Black = 2, Other = 8, Two or more = 9
    group_by(RAC1P) %>%
    group_by(HISP, add = TRUE) %>% # The add = true call turns out to be important. It is necessary to prevent overriding existing groups.
    group_by(SEX, add = TRUE) %>%
    do(., summarise(., population = sum(PWGTP)))
  return(data.frame(mres2)) # It is not clear how data changed by calling as a data.frame (this is explored in the tutorial, below)
}

car.characteristics <- mode.by_race.by_ethnicity.by_sex(1) # Works. (if inelegant)



# The following five sections are past attempts to work with "do"

campo.male.modes.est <- campo_pums %>% # Estimate, part 1
  filter(SEX == 1) %>%
  group_by(JWTR) %>%
  do(., summarise(., population = sum(PWGTP))) # Error in summarise_impl(.data, dots) : expecting result of length one, got : 17361

campo_pums %>%
  group_by(JWTR) %>%
  do(., stand.err.from.replicate.est(., campo.males.est)) # Error: Results are not data frames at positions: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13

campo_pums %>%
  group_by(JWTR) %>%
  summarize(population.est = sum(PWGTP)) %>%
  do(., stand.err.from.replicate.est(campo_pums,.)) # Error: Result must be a data frame

campo.male.modes.data <- campo.males %>%
  group_by(JWTR) %>%
  do(., stand.err.from.replicate.est(campo.males, campo.male.modes.est.values)) # Error: Results are not data frames at positions: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13

campo.male.modes.est2 <- campo.males %>%
  group_by(JWTR) %>%
  do(., summarise_each(., pop.est = sum(PWGTP))) # Error: argument "funs" misssing




### The following are just practice with "do" from the ?help module, with additional annotations:

by_cyl <- group_by(mtcars, cyl)
do(by_cyl, head(., 2))

models <- by_cyl %>% do(mod = lm(mpg ~ disp, data = .)) # Note: "do" is used in every expression after data has been grouped.
models # Note: mod results are lists "<S3: lm>"

summarise(models, rsq = summary(mod)$r.squared) # I think this takes those lists from above and summarizes them.

models %>% do(data.frame(coef = coef(.$mod))) # This displayes the model coefficients as a df. Note: lists may be expanded here

# What is purpose/need of calling data.frame()?

models %>% do(data.frame(
  var = names(coef(.$mod)),
  coef(summary(.$mod)))
) # This is an interestinf result! Lists are again expanded. Note continued use of "do" after the inital grouping.

models <- by_cyl %>% do(
  mod_linear = lm(mpg ~ disp, data = .),
  mod_quad = lm(mpg ~ poly(disp, 2), data = .)
) # Creates lists for each...

models

compare <- models %>% do(aov = anova(.$mod_linear, .$mod_quad))

# compare %>% summarise(p.value = aov$`Pr(>F)`) # Example was commented out in original; does not work here.

# The below example requires a package which we install and load now:
install.packages('nycflights13')
library(nycflights13)

if (require("nycflights13")) {
  # You can use it to do any arbitrary computation, like fitting a linear
  # model. Let's explore how carrier departure delays vary over the time
  carriers <- group_by(flights, carrier)
  group_size(carriers)
  
  mods <- do(carriers, mod = lm(arr_delay ~ dep_time, data = .)) # This makes a tbl of 16 carriers, each with a corresponding lm list.
  mods %>% do(as.data.frame(coef(.$mod)))
  mods %>% summarise(rsq = summary(mod)$r.squared)
}

mods



## The following takes the workflow of the help module and plugs it into the campo_pums data

by_mode <- group_by(campo_pums, JWTR)
do(by_mode, head(.,2))

group_size(by_mode)

mode_sum <- by_mode %>% 
  do(pop.est = summarize(., sum(PWGTP))) # This generates pop.est as a list, but the list is simply a 1x1 tbl. The value does not look right, according to the calls below.
mode_sum
summary(mode_sum)




### The following are functions/data referenced in past work, necessary to test functions, above.

# Standard error function

stand.err.from.replicate.est <- function(data, estimate){
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
  
  sum.squared.diffs <- sum(squared_diffs) # now just 1 value.
  
  stand.err <- sqrt(sum.squared.diffs * (4.0 / 80.0)) # just 1 value.
  
  return(stand.err)
}

male.modes <- campo.males %>%
  group_by(JWTR) %>%
  do(., stand.err.from.replicate.est(campo.males, campo.males.est.value)) # Error: Results are not data frames at positions: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13

campo.males <- campo_pums %>% # Data
  filter(SEX == 1)

campo.males.est <- campo_pums %>% # Estimate, part 1
  filter(SEX == 1) %>%
  summarise(population = sum(PWGTP))

campo.males.est.value <- campo.males.est[,1] # Estimate, part 2

campo_pums %>%
  filter(JWMNP > 0) %>%
  filter(CIT == 5) %>%
  filter(ENG >= 3) %>%
  group_by(JWTR) %>%
  summarize(commuters = sum(PWGTP),
            median_travel_time = weightedMedian(JWMNP,PWGTP))


