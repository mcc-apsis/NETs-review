#rm(list=ls())

costs <- 0

#==== USER SECTION ==========
#==== STEPS ====
DEBUG       <- FALSE

steps <- list(
  data.frame(
    net      = "AR",
    pot_min  = 1,
    pot_max  = 8,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 200,
    cost_n   = 1000
  ),
  data.frame(
    net      = "BECCS",
    pot_min  = 1,
    pot_max  = 10,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 400,
    cost_n   = 1000
  ),
  data.frame(
    net      = "BC",
    pot_min  = 1,
    pot_max  = 8,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 200,
    cost_n   = 1000
  ),
  data.frame(
    net      = "DAC",
    pot_min  = 1,
    pot_max  = 50,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 1000,
    cost_n   = 1000
  ),
  data.frame(
    net      = "EW",
    pot_min  = 1,
    pot_max  = 100,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 1000,
    cost_n   = 1000
  ),
  data.frame(
    net      = "OA",
    pot_min  = 1,
    pot_max  = 40,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 300,
    cost_n   = 1000
  ),
  data.frame(
    net      = "EW&OA",
    pot_min  = 1,
    pot_max  = 100,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 1000,
    cost_n   = 1000
  ),
  data.frame(
    net      = "OF",
    pot_min  = 1,
    pot_max  = 50,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 100,
    cost_n   = 1000
  ),
  data.frame(
    net      = "SCS",
    pot_min  = 1,
    pot_max  = 15,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 200,
    cost_n   = 1000
  )
)  %>% 
  do.call("rbind", .)
#####
# ==== EJ =====
#Expert judgment data based on Synthesis table in NEt review part 2
data_ej <- list(
  data.frame(
    net      = "AR",
    pot_min  = 0.5,
    pot_max  = 3.6,
    cost_min = 5,
    cost_max = 50
  ),
  data.frame(
    net      = "BECCS",
    pot_min  = 0.5,
    pot_max  = 5,
    cost_min = 100,
    cost_max = 200
  ),
  data.frame(
    net      = "BC",
    pot_min  = 0.3,
    pot_max  = 2.0,
    cost_min = 0,
    cost_max = 120
  ),
  data.frame(
    net      = "DAC",
    pot_min  = 0.5,
    pot_max  = 5,
    cost_min = 150,
    cost_max = 300
  ),
  data.frame(
    net      = "EW",
    pot_min  = 2,
    pot_max  = 4,
    cost_min = 50,
    cost_max = 200
  ),
  data.frame(
    net      = "OA",
    pot_min  = 10,
    pot_max  = 25,
    cost_min = 90,
    cost_max = 110
  ),
  data.frame(
    net      = "EW&OA",
    pot_min  = 2,
    pot_max  = 4,
    cost_min = 50,
    cost_max = 200
  ),
  data.frame(
    net      = "OF",
    pot_min  = NA,
    pot_max  = NA,
    cost_min = NA,
    cost_max = NA
  ),
  data.frame(
    net      = "SCS",
    pot_min  = 3,
    pot_max  = 6,
    cost_min = 0,
    cost_max = 100
  )
) %>% 
  do.call("rbind", .)

net_names <- data.frame(
  longname  = c(sort(unique(all_data$technology)), "Enhanced weathering (terrestrial and ocean) and Ocean alkalinisation"),
  shortname = c("AR", "BECCS", "BC", "Bioenergy", "DAC", "EW", "OA", "OF", "SCS", "Storage", "EW&OA")
)


#==== INITIALISE ==========
# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(plotrix)
library(parallel)
library(scales)
library(gridExtra)

source("heatbars/heatbar_functions.R")
source("part2-synthesis/heatmap_functions.R")
#source("../../bitbucket/beccs/functions/useful_functions.R")

plotdir <- paste0("plots/1.5_Sabine/both") #,u_sheetName)

dir.create(plotdir, recursive = TRUE)


#==== READ IN DATA ==========
#load("../../bitbucket/beccs/data/dataplotAll.RData") # IAM

load("data/all_data.RData") #BOTTOM UP 
all_data$TI[is.na(all_data$TI)] <- all_data$CITATION[is.na(all_data$TI)]

#==== PROCESS DATA ==========
all_data <- all_data %>%
  filter(!technology %in% c("Storage", "Bioenergy")) %>%
  group_by(TI, variable, `Data categorisationresource`, boundaries, year, measurement) %>%
  arrange(value) %>%
  mutate(ind = row_number()) %>%
  filter(ind == 1) %>%
  ungroup()

# TODO: EW and OA are additive!! => Add up potentials (and costs?)
#all_data$technology[which(all_data$technology == "Enhanced weathering (terrestrial and ocean)")] <- "Enhanced weathering (terrestrial and ocean) and Ocean alkalinisation"
#all_data$technology[which(all_data$technology == "Ocean alkalinisation")]                        <- "Enhanced weathering (terrestrial and ocean) and Ocean alkalinisation"

all_data <- all_data %>%
  mutate()

#==== PROCESS DATA ==========
data_steps <- list()
data_bu    <- list()
for (k_net in unique(all_data$technology)) {
  
  cat(paste0("Processing ", k_net, "...\n"))
  
  net_sn <- paste(net_names$shortname[which(net_names$longname == k_net)])
  
  if (!net_sn %in% c("DAC")) {
    
    data_steps[[k_net]] <- data.frame(
      "pot" = seq(steps$pot_min[which(steps$net == net_sn)], 
                  steps$pot_max[which(steps$net == net_sn)], 
                  (steps$pot_max[which(steps$net == net_sn)]-steps$pot_min[which(steps$net == net_sn)])/steps$pot_n[which(steps$net == net_sn)]),
      "cost" = seq(steps$cost_min[which(steps$net == net_sn)], 
                   steps$cost_max[which(steps$net == net_sn)],
                   (steps$cost_max[which(steps$net == net_sn)]-steps$cost_min[which(steps$net == net_sn)])/steps$cost_n[which(steps$net == net_sn)])
    )
    
    data_bu[[k_net]] <- list(
      "pot"  = generate_potentials(all_data %>% filter(technology == k_net) %>%
                                     filter(
                                       !is.na(value) , 
                                       is.finite(value),
                                       potsinclude==T
                                     ), net_sn, data_steps[[k_net]]$pot, measure = "range"),
      "cost" = generate_costs(all_data %>% filter(technology == k_net) %>%
                                filter(
                                  !is.na(value) , 
                                  is.finite(value),
                                  costsinclude==T), net_sn, data_steps[[k_net]]$cost)
    )
  } else {
    data_steps[[k_net]] <- data.frame(
      "pot" = rep(NA, 1001),
      "cost" = seq(steps$cost_min[which(steps$net == net_sn)], 
                   steps$cost_max[which(steps$net == net_sn)],
                   (steps$cost_max[which(steps$net == net_sn)]-steps$cost_min[which(steps$net == net_sn)])/steps$cost_n[which(steps$net == net_sn)])
    )
    
    data_bu[[k_net]] <- list(
      "pot"  = NULL,
      "cost" = generate_costs(all_data %>% filter(technology == k_net) %>%
                                filter(
                                  !is.na(value) , 
                                  is.finite(value),
                                  costsinclude==T#, potsinclude==T
                                ), net_sn, data_steps[[k_net]]$cost)
    )
  }
}

p_cex_axis=1.5



#==== PROCESS DATA FOR RECTS==========
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
    costs_max = quantile(value, .75, na.rm = T), 
    max_cost = max(value, na.rm = T), 
    min_cost = min(value, na.rm = T)
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
    pots_max = quantile(value, .75, na.rm = T),
    max_pots = max(value, na.rm = T), 
    min_pots = min(value, na.rm = T)
  )



all_sums <- left_join(costs, pots)
#==== FLL GAPS AND TRIM EXTREMES ==========
max_potential_axis <- 8

all_sums$pots_min[is.na(all_sums$pots_min)] <- 0
all_sums$pots_max[is.na(all_sums$pots_max)] <- max_potential_axis
all_sums$costs_max[all_sums$technology == "DAC"] <-250
all_sums$pots_max[all_sums$pots_max > max_potential_axis] <- max_potential_axis
#==== PLOT RECTS==========
ggplot() + 
  geom_rect(
    data=all_sums,
    mapping=aes(
      xmin=pots_min,
      xmax=pots_max,
      ymin=costs_min,
      ymax=costs_max,
      color=technology, 
      fill=technology
    ),
    alpha=0.7,
    size=1
  ) +
#removed expert judments from draw_rects
theme_bw() +
  labs(x="Potentials in Gt CO2/year",y="Costs in $/tCO2")+
  theme(legend.position = "bottom")

file = file.path(plotdir, paste0("panelA_shortX.pdf"))
ggsave(file)

#==== PANEL B ====
#==== PROCESS DATA ==========
all_data <- all_data %>%
  filter(!technology %in% c("Storage", "Bioenergy")) %>% 
  group_by(TI, variable, `Data categorisationresource`, boundaries, year, measurement) %>%
  arrange(value) %>%
  mutate(ind = row_number()) %>%
  filter(ind==1) %>%
  ungroup()

my_pots <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="totalPotential",
    potsinclude==T
  )

my_costs <-all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    variable=="cost",
    costsinclude==T
  )

# ggplot(filter(my_pots, measurement != "min"), aes(x = value))+
#   geom_density(kernel = "gaussian")+
#   facet_grid(.~technology, scales = "free_x")
# 
# ggplot(filter(my_costs, measurement != "min"), aes(x = value))+
#   geom_density(kernel = "gaussian")+
#   facet_grid(.~technology, scales = "free_x")

#get data_bu from draw_heatmap_all
#==== SUBPLOTS ==========
plotdir <- paste0("plots/1.5_Sabine/both") #,u_sheetName) 

#==== adjust theme ====
adj <- theme(plot.title = element_text(hjust = 0.5), 
      axis.title.x = element_text(size = 15),
      plot.background = element_rect(fill = "#f2f2f2ff"))

pot_color <- "darkgoldenrod3"
cost_color <- "aquamarine4"
papers <- data.frame(technology = unique(all_data$technology), N_costs = 0, N_pots = 0)

rm(k_net)
for (k_net in unique(all_data$technology)){
#k_net = "Afforestation and Reforestation" #return to loop after fine-tuning

tempSCS <- rbind(data_bu[[k_net]]$pot, data_bu[[k_net]]$cost)
# tempSCS <- tempSCS %>% mutate(
#   value = as.integer(value)
# )

## Potentials

pot_NoP <-  max(tempSCS$maxvalue[tempSCS$resource == "totalPotential"])
ylab = paste0("No. of estimates", pot_NoP) #
papers$N_pots[papers$technology == k_net] <- pot_NoP
#==== Special SCS & Biochar Potential =====
# combined <- c("Biochar" , "Soil Carbon Sequestration")
# 
# if (k_net %in% combined) {
#  pot <- ggplot(data = filter(tempSCS, resource == "totalPotential"),
#                       aes(x = v, y = value))+
#       geom_point(size = 0.7, color = "grey60")+
#       geom_smooth(se = F, color = pot_color)+
#       geom_line(color = "grey20")+
#       labs(x = "", y = ylab, title = "")+ #expression("Gt CO"[2]*"/yr by 2050")
#       theme_bw()+
#       adj+
#       scale_y_continuous(breaks = pretty_breaks())
# pot  
# 
# file = file.path(plotdir, paste0("count_special", k_net,".png"))
# ggsave(file, width = 5.2, height = 1.8, units = "in")
# } else {

#==== Other techs ====

if (pot_NoP > 4){

pot <- ggplot(data = filter(tempSCS, resource == "totalPotential"),
       aes(x = v, y = value))+
  geom_point(size = 0.7, color = "grey60")+
  geom_smooth(se = F, color = pot_color)+
  geom_line(color = "grey20")+
  labs(x = expression("Gt CO"[2]*"yr"^{-1}), y = "Articles", title = "")+ #expression("Gt CO"[2]*"/yr by 2050")
  theme_bw()+
  adj+
  scale_y_continuous(breaks = pretty_breaks())
#+
  # facet_grid(resource~., scales = "free_y")
pot
} else {
  pot <- ggplot(data = filter(tempSCS, resource == "totalPotential" ),
                aes(x = v, y = value))+
    geom_point(size = 0.7, color = "grey60")+
    #geom_smooth(se = F, color = pot_color)+
    geom_line(color = "grey20")+
    labs(x = expression("Gt CO"[2]*"yr"^{-1}), y = "Articles", title = "")+ #expression("Gt CO"[2]*"/yr by 2050") title = k_net
    theme_bw()+
    adj+
    scale_y_continuous(breaks = pretty_breaks())
  #+
  # facet_grid(resource~., scales = "free_y")
  pot
}


file = file.path(plotdir, paste0("count_pots_range2", k_net, ".png"))
ggsave(file)


#==== Costs ====

cost_NoP <- max(tempSCS$maxvalue[tempSCS$resource == "cost"])
ylab = paste0("No. of papers = ", cost_NoP)

papers$N_costs[papers$technology == k_net] <- cost_NoP

if (cost_NoP > 4){

cost <- ggplot(data = filter(tempSCS, resource == "cost" ),
                      aes(x = v, y = value))+
  geom_point(size = 0.7, color = "grey60")+
  geom_smooth(se = F, color = cost_color)+
  geom_line(color = "grey20")+
  # geom_ribbon()
  labs(x = expression("US$ tCO"[2]^{-1}), y = "Articles", title = "")+ #expression("$/ton CO"[2])
  theme_bw()+
  adj+
  scale_y_continuous(breaks = pretty_breaks())
cost
} else {
  cost <- ggplot(data = filter(tempSCS, resource == "cost" ),
                 aes(x = v, y = value))+
    geom_point(size = 0.7, color = "grey60")+
    #geom_smooth(color = "red", se = F)+
    geom_line(color = "grey20")+
    labs(x = expression("US$ tCO"[2]^{-1}), y = "Articles", title = "")+ #expression("$/ton CO"[2])
    theme_bw()+
    adj+
    scale_y_continuous(breaks = pretty_breaks())
  cost
  
}
file = file.path(plotdir, paste0("count_cost", k_net, ".png"))
ggsave(file)

p <- grid.arrange(pot, cost, ncol = 1)
file = file.path(plotdir, paste0("count_both_N_range_2", k_net, ".png"))
p
ggsave(file, p, width = 5.2, height = 3.6, units = "in")

 }
#}






#==== Facets ====
# mylist <- list()
# 
# for (k_net in unique(all_data$technology)){
#   #k_net = "Afforestation and Reforestation" #return to loop after fine-tuning
#   
#   data_bu[[k_net]]$pot %>% mutate(
#     tech = k_net
#   )
#   data_bu[[k_net]]$cost %>% mutate(
#     tech = k_net
#   )
#   
#   tempSCS <- rbind(data_bu[[k_net]]$pot, data_bu[[k_net]]$cost)
#   