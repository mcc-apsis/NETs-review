rm(list=ls())

library(dplyr)
library(tidyr)
library(ggplot2)

load("data/all_data.RData")


#####################
## By technology ranges

bytech <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    costsinclude==T, potsinclude==T
  ) %>%
  group_by(technology, variable) %>% 
  summarise(
    # min = min(value, na.rm = T),
    # max = max(value, na.rm = T)
    min = quantile(value, .25, na.rm = T),
    max = quantile(value, .75, na.rm = T)
  )

costsums <- filter(bytech, variable=="cost") %>%
  select(technology, costs_min=min,costs_max=max)

potsums <- filter(bytech, variable=="totalPotential") %>%
  select(technology, pots_min=min,pots_max=max)

all_sums <- left_join(costsums, potsums)

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
  labs(x="Potentials in Gt CO2/year",y="Costs in $/tCO2")

ggsave("plots/synthetic/rects.svg",width=16,height=10)