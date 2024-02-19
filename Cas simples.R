# Load required libraries

library(dplyr)

library(ggplot2)

library(broom) # for tidy function



# Set seed for reproducibility

set.seed(123)



# Number of individuals per group

n_per_group <- 20



# Number of time periods

n_periods <- 2



# Time variable

time <- 1:n_periods



# Generate synthetic data

data <- data.frame(
  
  Group = rep(c("Group 1", "Group 2",”Group3”), each = n_per_group * n_periods),
  
  Individual = rep(rep(1:n_per_group, each = n_periods), times = 3),
  
  Time = rep(time, times = 3 * n_per_group),
  
  stringsAsFactors = FALSE
  
)



data = data %>% group_by(Individual) %>% mutate(Treatment = ifelse(Group == "Group1" & Time == 2, 1,
                                                                   
                                                                   ifelse(Group == "Group2" & Time == 2, 0.5, 0),
                                                                   
                                                                   Outcome = A COMPLETER
                                                                   
                                                                   %>% distinct()
                                                                   
                                                                   