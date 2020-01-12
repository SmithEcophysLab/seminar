# make some figures for big bend 2020 talk

## libraries
library(tidyverse)
library(ggplot2)

## photosynthesis figures
source('optimal_vcmax/calc_optimal_vcmax.R')
sourceDirectory('optimal_vcmax/functions')

### temperature response
t_response = calc_optimal_vcmax(tg_c = seq(10, 40, 1))
t_response$a_net = (t_response$vcmax_prime * ((t_response$ci - t_response$gammastar) / (t_response$ci + t_response$km))) - (0.015 * t_response$vcmax_prime)
(t_response$a_net[26] - t_response$a_net[21]) / t_response$a_net[21] # change from 30-35C

### vpd response
d_response = calc_optimal_vcmax(vpdo = seq(0.5, 5, 0.25))
d_response$a_net = (d_response$vcmax_prime * ((d_response$ci - d_response$gammastar) / (d_response$ci + d_response$km))) - (0.015 * d_response$vcmax_prime)
(d_response$a_net[19] - d_response$a_net[13]) / d_response$a_net[13] # change from 3.5-5 kPa

### co2 response
c_response = calc_optimal_vcmax(cao = seq(400, 1000, 50))
c_response$a_net = (c_response$vcmax_prime * ((c_response$ci - c_response$gammastar) / (c_response$ci + c_response$km))) - (0.015 * c_response$vcmax_prime)
(c_response$a_net[9] - c_response$a_net[1]) / c_response$a_net[1] # change from 400-800 ppm
(c_response$chi[9] - c_response$chi[1]) / c_response$chi[1] # change from 400-800 ppm
(c_response$vcmax_prime[9] - c_response$vcmax_prime[1]) / c_response$vcmax_prime[1] # change from 400-800 ppm

### t and d plot
t_plot = ggplot(data = t_response, aes(x = tg_c, y = a_net)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2.6), colour = 'black'),
        axis.title.x=element_text(size=rel(2.6), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_line(size = 3) +
  ylim(c(0,15)) +
  xlab('Temperature (°C)') +
  ylab(expression('Photosynthesis (µmol m' ^ '-2' * ' s' ^ '-1' * ')'))

d_plot = ggplot(data = d_response, aes(x = vpdo, y = a_net)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2.6), colour = 'black'),
        axis.title.x=element_text(size=rel(2.6), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_line(size = 3) +
  ylim(c(0,15)) +
  xlab('VPD (kPa)') +
  ylab(expression('Photosynthesis (µmol m' ^ '-2' * ' s' ^ '-1' * ')'))
  
c_plot = ggplot(data = c_response, aes(x = cao, y = a_net)) +
  theme(legend.position = 'none',
        legend.title = element_blank(),
        axis.title.y=element_text(size=rel(2.6), colour = 'black'),
        axis.title.x=element_text(size=rel(2.6), colour = 'black'),
        axis.text.x=element_text(size=rel(2), colour = 'black'),
        axis.text.y=element_text(size=rel(2), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black")) +
  geom_line(size = 3) +
  ylim(c(0,20)) +
  xlab(expression('CO'[2] * ' (ppm)')) +
  ylab(expression('Photosynthesis (µmol m' ^ '-2' * ' s' ^ '-1' * ')'))

