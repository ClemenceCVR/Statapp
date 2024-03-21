# Load required libraries
library(dplyr)
library(ggplot2)
library(broom) # for tidy function
library(plm)

# Set seed for reproducibility
set.seed(1234)

# Number of individuals per group
n_per_group <- 100

# Number of time periods
n_periods <- 10

# Time variable
time <- 1:n_periods

# Treatment period
tp=n_periods/2

# Generate synthetic data
data <- data.frame(
  Group = rep(c("Group 1", "Group 2"), each = n_per_group * n_periods),
  Individual = rep(rep(1:n_per_group, each = n_periods), times = 2),
  Time = rep(time, times = 2 * n_per_group),
  stringsAsFactors = FALSE
)

# data = data %>% group_by(Individual) %>% mutate(Treatment = ifelse(Time > 50 & Group == "Group 1", 1, 0),
#                                                 Outcome = rnorm(2 * n_periods, mean = ifelse(Treatment == 1, 2+0.5*Time+5, -5+0.5*Time))) %>% distinct()

data = data %>% group_by(Individual) %>% mutate(Treatment = ifelse(Time > tp & Group == "Group 1", 1, 0),
                                                Outcome = rnorm(2 * n_periods, mean = case_when(
                                                  Treatment == 1 & Group== "Group 1" ~ -5+0.5*Time+10+22*Time,
                                                  Treatment == 0 & Group== "Group 1" ~ -5+0.5*Time,
                                                  TRUE ~ 0.5*Time))) %>% distinct() %>% 
  group_by(Individual, Group) %>% mutate(TreatmentPeriod = if(is.na(which(Treatment>0)[1])){0}else{which(Treatment>0)[1]})

# Rename individuals to be unique
data$Individual = rep(1:(n_per_group*2), each = n_periods)

# Plot the simulated data
ggplot(data, aes(x = Time, y = Outcome, color = Group, linetype = factor(Treatment))) +
  geom_point() +
  labs(x = "Time", y = "Outcome", color = "Group", linetype = "Treatment") +
  theme_minimal()

# Estimate DiD parameters using linear regression
diD_model <- lm(Outcome ~ Treatment + Time + Treatment:Time + Group, data = data)

# Display DiD model summary
summary(diD_model)

# Event study approach

# generate leads and lags of the treatment
t0 = 3 # Number of periods before the event
t1 = 5 # Number of periods after the event
Dtl <- sapply(-t0:t1, function(l) {1*((data$Time == data$TreatmentPeriod + l) & (data$TreatmentPeriod > 0))})
Dtl <- as.data.frame(Dtl)
cnames1 <- paste0("Dtmin", t0:1)
colnames(Dtl) <- c(cnames1, paste0("Dt", 0:t1))
data <- cbind.data.frame(data, Dtl)
row.names(data) <- NULL

# panel regression
pdata = pdata.frame(data, index = c("Individual", "Time", "Group"))
# table(index(pdata))

es <- plm(as.formula(paste("Outcome ~", paste(colnames(Dtl), collapse="+"))), data = pdata, model = "within", effect = "twoways")

summary(es)

# Plot
coefs1 <- coef(es)
ses1 <- sqrt(diag(summary(es)$vcov))
idx.pre <- 1:t0
idx.post <- (t1-1):length(coefs1)
coefs <- c(coefs1[idx.pre], 0, coefs1[idx.post])
ses <- c(ses1[idx.pre], 0, ses1[idx.post])
exposure <- -t0:t1

cmat <- data.frame(coefs=coefs, ses=ses, exposure=exposure)

ggplot(data = cmat, mapping = aes(y = coefs, x = exposure)) +
  geom_line(linetype = "dashed") +
  geom_point() + 
  geom_errorbar(aes(ymin = (coefs-1.96*ses), ymax = (coefs+1.96*ses)), width = 0.2) +
  theme_bw()

valeurs_reg_Dt <- as.numeric(coef(es))


# Afficher les coefficients numériques
print(valeurs_reg_Dt)

valeurs_reg_Dt_aprestraitement <- valeurs_reg_Dt[(t0+2):(t1+t0+1)]

df1 = data.frame(y = valeurs_reg_Dt_aprestraitement, x = c(1:t1))


maregression = lm(y~x, data = df1)

summary(maregression)

#On retrouve (en faisant la régression de outcome sur les Dt0,1,2 (les positifs)) bien le coeff de 0.3 qui nous avait servi à simuler les données post traitement
# régression linéaire : Treatment:Time  0.27985    0.03295   8.492   <2e-16 ***
# event study :            x            0.32795    0.03786   8.662  0.00324 ** 
# les deux approches semblent trouver le bon résultat
