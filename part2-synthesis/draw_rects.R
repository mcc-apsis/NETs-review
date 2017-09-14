rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)

load("data/all_data.RData")


#############
## Costs
costs <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="cost",
    costsinclude==T
  ) %>%
  group_by(technology) %>% 
  summarise(
    costs_min = quantile(value, .25, na.rm = T),
    costs_max = quantile(value, .75, na.rm = T)
  )

## Pots

pots <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="totalPotential",
    potsinclude==T
  ) %>%
  group_by(technology) %>% 
  summarise(
    pots_min = quantile(value, .25, na.rm = T),
    pots_max = quantile(value, .75, na.rm = T)
  )



all_sums <- left_join(costs, pots)

all_sums$pots_min[is.na(all_sums$pots_min)] <- 0
all_sums$pots_max[is.na(all_sums$pots_max)] <- 100


expert_judgements <- data.frame(
  technology = unique(all_sums$technology),
  pots_min = c(0,0.5,NA,4,2,NA,NA,3),
  pots_max = c(3.6, 5, NA, 40, 4, NA, NA, 6),
  costs_min = c(5,100, NA, 50, 50, NA, NA, 0),
  costs_max = c(50, 200, NA, 250, 200, NA, NA, 100)
)



ggplot() + 
  geom_rect(
    data=all_sums,
    mapping=aes(
      xmin=pots_min,
      xmax=pots_max,
      ymin=costs_min,
      ymax=costs_max,
      color=technology
    ),
    alpha=0.0,
    size=2
    # color="black"
  ) +
  geom_rect(
    data=all_sums,
    mapping=aes(
      xmin=pots_min,
      xmax=pots_max,
      ymin=costs_min,
      ymax=costs_max,
      color=technology
    ),
    alpha=0.0,
    size=0
  ) + theme_bw() +
  labs(x="Potentials in Gt CO2/year",y="Costs in $/tCO2")

ggsave("plots/synthetic/rects.svg",width=16,height=10)


all_sums$pots_max[all_sums$pots_max > 20] <- 20

ggplot() + 
  geom_rect(
    data=all_sums,
    mapping=aes(
      xmin=pots_min,
      xmax=pots_max,
      ymin=costs_min,
      ymax=costs_max,
      color=technology
    ),
    alpha=0.0,
    size=2
    # color="black"
  ) + theme_bw() +
  labs(x="Potentials in Gt CO2/year",y="Costs in $/tCO2") +

ggsave("plots/synthetic/rects_doctored.svg",width=16,height=10)