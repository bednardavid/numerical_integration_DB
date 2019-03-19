# Numerical integration for DB
# by Dávid Kubanda

# Get libraries
library(tidyverse)
library(ggplot2)
library(cmna)

# Get mass attuneation coefficient (as a function of energy(included)) from file
data_Al = read_delim("mu_Al.txt", delim = " ")
x = 1
rho = 2.7
data_Al <- data_Al %>% mutate(mu = rho*mass_att_coef, energy_prev = c(0, energy)[1:length(energy-1)])
# Plot mass attenuation coefficient as a function energy

ggplot(data_Al, aes(x = Energy, y = mass_att_coef)) + 
  geom_line() + 
  geom_point() +
  scale_x_log10()+
  scale_y_log10()

# Interpolate function
linear_params_Al <- data.frame(pwiselinterp(data_Al$energy, data_Al$mass_att_coef))
linear_params_Al <- rbind(data.frame(m = 0, b = 0), linear_params_Al)

data_and_params_Al <- cbind(data_Al, linear_params_Al)

result_Al <- data_and_params_Al %>% mutate((x*exp(-b)/m)*(exp(-m)))
