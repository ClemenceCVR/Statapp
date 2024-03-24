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
  G = rep(c(50, 75, 0), each = n_per_group * n_periods),
  id = rep(rep(1:n_per_group, each = n_periods), times = 3),
  period = rep(time, times = 3 * n_per_group),
  stringsAsFactors = FALSE
)


data2 = data2 %>% group_by(id) %>% mutate(treat = ifelse((period > 50 & G == 50) | (period > 75 & G == 75), 1, 0),
                                          Y = rnorm(3 * n_periods, mean = case_when(
                                            treat == 1 & G== 50 ~ -5+0.5*period+10+0.3*period,
                                            treat == 0 & G== 50 ~ -5+0.5*period,
                                            treat == 1 & G== 75 ~ -15+0.5*period+10+0.3*period,
                                            treat == 0 & G== 75 ~ -15+0.5*period,
                                            TRUE ~ 0.5*period))) %>% distinct()


# on crée 3 groupes qui suivent la même tendance pre et post traitement mais le groupe 1 est traité en période 50 et le groupe 2 en période 75


data2 <- data2 %>%
  mutate(TreatmentPeriod = case_when(
    G == 1 ~ 50,
    G == 2 ~ 75,
    G == 3 ~ 0,
    TRUE ~ NA_real_  # pour gérer les cas non spécifiés, optionnel
  ))

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



# Event study approach

# generate leads and lags of the treatment
t0 = 50# Number of periods before the event
t1 = 25 # Number of periods after the event
Dtl <- sapply(-t0:t1, function(l) {1*((data2$period == data2$TreatmentPeriod + l) & (data2$TreatmentPeriod > 0))})
Dtl <- as.data.frame(Dtl)
cnames1 <- paste0("Dtmin", t0:1)
colnames(Dtl) <- c(cnames1, paste0("Dt", 0:t1))
data2 <- cbind.data.frame(data2, Dtl)
row.names(data2) <- NULL

# panel regression
pdata = pdata.frame(data2, index = c("id", "period", "G"))
# table(index(pdata))

es <- plm(as.formula(paste("Y ~", paste(colnames(Dtl), collapse="+"))), data = pdata, model = "within", effect = "twoways")

summary(es)

# dans es on retrouve tous nos coefficients de Dt0, Dt1 etc et ensuite on fait la regression linéaire des coefficients des Dt sur le temps.
#Normalement nous devrions retrouver 0.5 (c.f manière dont nos données sont simulées) mais ici on trouve 0.3

valeurs_reg_Dt <- as.numeric(coef(es))


# Afficher les coefficients numériques
print(valeurs_reg_Dt)

valeurs_reg_Dt_aprestraitement <- valeurs_reg_Dt[(t0+2):(t1+t0+1)]

df1 = data.frame(y = valeurs_reg_Dt_aprestraitement, x = c(1:t1))


maregression = lm(y~x, data = df1)

summary(maregression)

