# Numerical integration for DB
# by Dávid Kubanda

# Get libraries
library(tidyverse)
library(ggplot2)
library(cmna)

# Get mass attuneation coefficient (as a function of energy(included)) from file
data = read_delim("mu_Fe.txt", delim = " ")
a_params = read_tsv("A_parameters.txt") %>% 
  mutate(energy_prev = lag(energy, default = 0)) 

x = 0.598
rho = 7.879
data <- data %>% mutate(mu = rho*mass_att_coef, 
                        mu_prev = lag(mu, default = Inf),
                        energy_prev = lag(energy, default = 0))
endpoint_energy <- tail(data$energy, n = 1)
# Plot mass attenuation coefficient as a function energy

ggplot(data, aes(x = energy, y = mu)) + 
  geom_line() + 
  geom_point() +
  scale_x_log10()+
  scale_y_log10()

# Interpolate function
linear_params <- tbl_df(pwiselinterp(data$energy, data$mu))
linear_params <- rbind(data.frame(m = 0, b = 0), linear_params)

data_and_params <- cbind(data, linear_params)


result <- data_and_params %>% mutate(partial_integral = (1/(m*x)) * (exp(-x*(m*energy_prev + b)) - exp(-x*(m*energy + b))))
result <- result %>% mutate(cum_integral = ifelse(!is.na(partial_integral), 
                                                  cumsum(na.omit(partial_integral)), 0))

integral <- result %>% summarise(sum(na.omit(partial_integral)))
integral <- integral[1,1]


mu_mean <- -log2(integral/endpoint_energy)/x

energy_mean_calc <- data_and_params %>% 
  mutate(in_interval = if_else(mu_mean < mu_prev & mu_mean >= mu, 1, 0)) %>% 
  filter(in_interval == 1) %>% 
  mutate(mean_energy = (mu_mean - b)/m)

energy_mean <- energy_mean_calc$mean_energy

a_linear_params_A1 <- tbl_df(pwiselinterp(a_params$energy, a_params$A1)) 
a_linear_params_Alfa1 <- tbl_df(pwiselinterp(a_params$energy, a_params$Alfa1))
a_linear_params_Alfa2 <- tbl_df(pwiselinterp(a_params$energy, a_params$Alfa2))

a_linear_params_A1 <- rbind(data.frame(m = 0, b = 0), a_linear_params_A1)%>% 
  rename(m_A1 = m,
         b_A1 = b)
a_linear_params_Alfa1 <- rbind(data.frame(m = 0, b = 0), a_linear_params_Alfa1)%>% 
  rename(m_Alfa1 = m,
         b_Alfa1 = b)
a_linear_params_Alfa2 <- rbind(data.frame(m = 0, b = 0), a_linear_params_Alfa2)%>% 
  rename(m_Alfa2 = m,
         b_Alfa2 = b)

a_linear_params <- cbind(a_params, a_linear_params_A1, a_linear_params_Alfa1, a_linear_params_Alfa2) 

a_params_calc <- a_linear_params %>% 
  mutate(in_interval = if_else(energy_mean >= energy_prev & energy_mean < energy, 1, 0)) %>% 
  filter(in_interval == 1) %>% 
  mutate(A1_calc = m_A1*energy_mean + b_A1,
         Alfa1_calc = m_Alfa1*energy_mean + b_Alfa1,
         Alfa2_calc = m_Alfa2*energy_mean + b_Alfa2)
