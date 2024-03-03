# Load required libraries
library(dplyr)
library(ggplot2)
library(broom) # for tidy function

# Set seed for reproducibility
set.seed(123)

# Number of individuals per group
n_per_group <- 10

# Number of time periods
n_periods <- 100

# Time variable
time <- 1:n_periods

# Generate synthetic data
data2 <- data.frame(
  Group = rep(c("Group 1", "Group 2", "Group 3"), each = n_per_group * n_periods),
  Individual = rep(rep(1:n_per_group, each = n_periods), times = 3),
  Time = rep(time, times = 3 * n_per_group),
  stringsAsFactors = FALSE
)

# data = data %>% group_by(Individual) %>% mutate(Treatment = ifelse(Time > 50 & Group == "Group 1", 1, 0),
#                                                 Outcome = rnorm(2 * n_periods, mean = ifelse(Treatment == 1, 2+0.5*Time+5, -5+0.5*Time))) %>% distinct()

data2 = data2 %>% group_by(Individual) %>% mutate(Treatment = ifelse((Time > 50 & Group == "Group 1") | (Time > 75 & Group == "Group 2"), 1, 0),
                                                Outcome = rnorm(3 * n_periods, mean = case_when(
                                                  Treatment == 1 & Group== "Group 1" ~ -5+0.5*Time+10+0.3*Time,
                                                  Treatment == 0 & Group== "Group 1" ~ -5+0.5*Time,
                                                  Treatment == 1 & Group== "Group 2" ~ -15+0.5*Time+10+0.3*Time,
                                                  Treatment == 0 & Group== "Group 2" ~ -15+0.5*Time,
                                                  TRUE ~ 0.5*Time))) %>% distinct()

# Plot the simulated data
ggplot(data2, aes(x = Time, y = Outcome, color = Group, linetype = factor(Treatment))) +
  geom_point() +
  labs(x = "Time", y = "Outcome", color = "Group", linetype = "Treatment") +
  theme_minimal()

# Estimate DiD parameters using linear regression
diD_model <- lm(Outcome ~ Treatment + Time + Treatment:Time + Group, data2 = data2)

# Display DiD model summary
summary(diD_model)

#Mettre data2 sous la forme des bases de données du départ

#On change le nom des colonnes
data3 <- data2 %>%
  rename(G = Group,
         id = Individual,
         period = Time,
         treat = Treatment,
         Y = Outcome)

unique(data3$G) #on a Group1, Group2, Group3
#on remplace par les chiffres
data3$G <- gsub("Group 1", "1", data3$G)
data3$G <- gsub("Group 2", "2", data3$G)
data3$G <- gsub("Group 3", "3", data3$G)