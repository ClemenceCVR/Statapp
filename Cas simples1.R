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
  G = rep(c(1, 2, 3), each = n_per_group * n_periods),
  id = rep(rep(1:n_per_group, each = n_periods), times = 3),
  period = rep(time, times = 3 * n_per_group),
  stringsAsFactors = FALSE
)


data2 = data2 %>% group_by(id) %>% mutate(treat = ifelse((period > 50 & G == 1) | (period > 75 & G == 2), 1, 0),
                                          Y = rnorm(3 * n_periods, mean = case_when(
                                            treat == 1 & G== 1 ~ -5+0.5*period+10+0.3*period,
                                            treat == 0 & G== 1 ~ -5+0.5*period,
                                            treat == 1 & G== 2 ~ -15+0.5*period+10+0.3*period,
                                            treat == 0 & G== 2 ~ -15+0.5*period,
                                            TRUE ~ 0.5*period))) %>% distinct()

# Plot the simulated data
ggplot(data2, aes(x = period, y = Y, color = G, linetype = factor(treat))) +
  geom_point() +
  labs(x = "period", y = "Y", color = "G", linetype = "treat") +
  theme_minimal()

# Estimate DiD parameters using linear regression
diD_model <- lm(Y ~ treat + period + treat:period + G, data = data2)

# Display DiD model summary
summary(diD_model)


#On ajoute les colonnes X et cluster nulles
data2$X <- 0
data2$cluster <- 0


