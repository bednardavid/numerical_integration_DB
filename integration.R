# Numerical integration for DB
# by Dávid Kubanda


# Get libraries

library(ggplot2)

# Get mass attuneation coefficient (as a function of energy(included)) from file

mass_att_coef_Al <- scan("mu_Al.txt")
mass_att_coef_Al <- matrix(mass_att_coef_Al, ncol = 8, byrow = T)
energy <-  mass_att_coef_Al[,1]
data = data.frame(energy = energy, mu_Al = mass_att_coef_Al[,8])

# Plot mass attenuation coefficient as a function energy

ggplot(data, aes(x = energy, y = mu_Al)) + 
  geom_line() + 
  geom_point() +
  scale_x_log10()+
  scale_y_log10()


## Spline interpolation
#
#func = splinefun(x = data$energy, y=data$mu_Al, method="fmm",  ties = mean)
#func(seq(0, 1, 0.001))
#data_interp = data.frame(energy = seq(0, 1, 0.001), mu_Al_interp = func(seq(0, 1, 0.001)))
## Plot mass attenuation coefficient as a function energy
#
#ggplot(data_interp, aes(x = energy, y = mu_Al_interp)) + 
#  geom_line() + 
#  geom_point() +
#  scale_x_log10()+
#  scale_y_log10()