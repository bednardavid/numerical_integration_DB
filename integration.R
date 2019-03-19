# Numerical integration for DB
# by Dávid Kubanda

# Get libraries
library(tidyverse)
library(ggplot2)
library(cmna)

# Get mass attuneation coefficient (as a function of energy(included)) from file
data = read_delim("mu_Fe.txt", delim = " ")
x = 0.299
rho = 7.879
data <- data %>% mutate(mu = rho*mass_att_coef, energy_prev = c(0, energy)[1:length(energy-1)])
# Plot mass attenuation coefficient as a function energy

ggplot(data, aes(x = energy, y = mu)) + 
  geom_line() + 
  geom_point() +
  scale_x_log10()+
  scale_y_log10()

# Interpolate function
linear_params <- data.frame(pwiselinterp(data$energy, data$mu))
linear_params <- rbind(data.frame(m = 0, b = 0), linear_params)

data_and_params <- cbind(data, linear_params)

result <- data_and_params %>% mutate(partial_integral = (1/(m*x)) * (exp(-x*(m*energy_prev + b)) - exp(-x*(m*energy + b))))
result <- result %>% mutate(cum_integral = ifelse(!is.na(partial_integral), cumsum(na.omit(partial_integral)), 0))

integral <- result %>% summarise(sum(na.omit(partial_integral)))

                                    