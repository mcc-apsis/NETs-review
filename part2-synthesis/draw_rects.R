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
    costs_q25 = quantile(value, .25, na.rm = T),
    costs_q75 = quantile(value, .75, na.rm = T),
    costs_min = min(value, na.rm = T),
    costs_max = max(value, na.rm = T)
  )

costsranges <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="cost",
    costsinclude==T
  ) %>%
  group_by(technology) %>% 
  summarise(
    costs_min = min(value, na.rm = T),
    costs_max = max(value, na.rm = T)
  )

write.csv(costs,'tables/allcosts.csv')

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
    pots_q25 = quantile(value, .25, na.rm = T),
    pots_q75 = quantile(value, .75, na.rm = T),
    pots_min = min(value, na.rm = T),
    pots_max = max(value, na.rm = T)
  )


potranges <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="totalPotential",
    potsinclude==T
  ) %>%
  group_by(technology) %>% 
  summarise(
    pots_min = min(value, na.rm = T),
    pots_max = max(value, na.rm = T)
  )

write.csv(pots,'tables/allpotentials.csv')

#################

all_sums <- left_join(costs, pots)

all_sums$pots_min[is.na(all_sums$pots_min)] <- 0
all_sums$pots_max[is.na(all_sums$pots_max)] <- 100


expert_judgements <- data.frame(
  technology = unique(all_sums$technology),
  pots_min = c(0.5, 0.5, 0.3, 0.5,2,NA,NA,3),
  pots_max = c(3.6, 5, 2, 5, 4, NA, NA, 6),
  costs_min = c(5, 100, 0, 100, 50, NA, NA, 0),
  costs_max = c(50, 200, 120, 300, 200, NA, NA, 100)
)

write.csv(expert_judgements,"tables/expert_judgements.csv")



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
  # geom_rect(
  #   data=expert_judgements,
  #   mapping=aes(
  #     xmin=pots_min,
  #     xmax=pots_max,
  #     ymin=costs_min,
  #     ymax=costs_max,
  #     fill=technology
  #   ),
  #   alpha=0.7,
  #   size=0
  # ) + 
  theme_bw() +
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
  ) + 
  # geom_rect(
  #   data=expert_judgements,
  #   mapping=aes(
  #     xmin=pots_min,
  #     xmax=pots_max,
  #     ymin=costs_min,
  #     ymax=costs_max,
  #     fill=technology
  #   ),
  #   alpha=0.7,
  #   size=0
  # ) + 
  theme_bw() +
  labs(x="Potentials in Gt CO2/year",y="Costs in $/tCO2") 

ggsave("plots/synthetic/rects_doctored.svg",width=16,height=10)




ggplot() + geom_rect(
  data=expert_judgements,
  mapping=aes(
    xmin=pots_min,
    xmax=pots_max,
    ymin=costs_min,
    ymax=costs_max,
    colour=technology
  ),
  alpha=0.0,
  size=2
) + theme_bw() +
  labs(x="Potentials in Gt CO2/year",y="Costs in $/tCO2") 

ggsave("plots/synthetic/expertly_judged_rects.svg",width=16,height=10)
