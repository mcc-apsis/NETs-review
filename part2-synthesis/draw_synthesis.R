rm(list=ls())
#==== USER SECTION ==========
DEBUG       <- FALSE

steps <- list(
  data.frame(
    net      = "AR",
    pot_min  = 1,
    pot_max  = 10,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 300,
    cost_n   = 1000
  ),
  data.frame(
    net      = "BECCS",
    pot_min  = 1,
    pot_max  = 20,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 400,
    cost_n   = 1000
  ),
  data.frame(
    net      = "BC",
    pot_min  = 1,
    pot_max  = 10,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 1000,
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
    pot_max  = 100,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 1000,
    cost_n   = 1000
  ),
  data.frame(
    net      = "OF",
    pot_min  = 1,
    pot_max  = 100,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 1000,
    cost_n   = 1000
  ),
  data.frame(
    net      = "SCS",
    pot_min  = 1,
    pot_max  = 10,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 1000,
    cost_n   = 1000
  )
)  %>% 
  do.call("rbind", .)

# Expert judgment data based on Synthesis table in NEt review part 2
data_ej <- list(
  data.frame(
    net      = "AR",
    pot_min  = 1,
    pot_max  = 10,
    cost_min = 1,
    cost_max = 6
  ),
  data.frame(
    net      = "BECCS",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  ),
  data.frame(
    net      = "BC",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  ),
  data.frame(
    net      = "DAC",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  ),
  data.frame(
    net      = "EW",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  ),
  data.frame(
    net      = "OA",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  ),
  data.frame(
    net      = "OF",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  ),
  data.frame(
    net      = "SCS",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  )
) %>% 
  do.call("rbind", .)

net_names <- data.frame(
  longname  = sort(unique(all_data$technology)),
  shortname = c("AR", "BECCS", "BC", "DAC", "EW", "OA", "OF", "SCS")
)


#==== INITIALISE ==========
# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(plotrix)

source("heatbars/heatbar_functions.R")
source("part2-synthesis/heatmap_functions.R")
source("../../bitbucket/beccs/functions/useful_functions.R")

plotdir <- paste0("plots/synthesis-part2/",u_sheetName)

dir.create(plotdir)


#==== READ IN DATA ==========
load("data/all_data.RData") #BOTTOM UP 
load("../../bitbucket/beccs/data/dataplotAll.RData") # IAM


#==== PROCESS DATA ==========
data_steps <- list()
data_bu    <- list()
for (k_net in unique(all_data$technology)) {
  
  cat(paste0("Processing ", k_net, "...\n"))
  
  net_sn <- paste(net_names$shortname[which(net_names$longname == k_net)])
  
  if (net_sn != "EW") {
    
    data_steps[[k_net]] <- data.frame(
      "pot" = seq(steps$pot_min[which(steps$net == net_sn)], 
                  steps$pot_max[which(steps$net == net_sn)], 
                  (steps$pot_max[which(steps$net == net_sn)]-steps$pot_min[which(steps$net == net_sn)])/steps$pot_n[which(steps$net == net_sn)]),
      "cost" = seq(steps$cost_min[which(steps$net == net_sn)], 
                   steps$cost_max[which(steps$net == net_sn)],
                   (steps$cost_max[which(steps$net == net_sn)]-steps$cost_min[which(steps$net == net_sn)])/steps$cost_n[which(steps$net == net_sn)])
    )
    
    data_bu[[k_net]] <- list(
      "pot"  = generate_potentials(all_data, net_sn, data_steps[[k_net]]$pot),
      "cost" = generate_costs(all_data,      net_sn, data_steps[[k_net]]$cost)
    )
  }
}


#==== PLOT DATA =============
mydata <- all_data %>%
  filter(
    !is.na(value) , 
    is.finite(value),
    costsinclude==T, potsinclude==T
  )

#####################
## By technology ranges

bytech <- mydata %>%
  group_by(technology, variable) %>% 
  summarise(
    min = quantile(value, .25, na.rm = T),
    max = quantile(value, .75, na.rm = T)
  )

costsums <- filter(bytech, variable=="cost") %>%
  select(technology, costs_min=min, costs_max=max)

potsums <- filter(bytech, variable=="totalPotential") %>%
  select(technology, pots_min=min, pots_max=max)

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

counts <- mydata %>%
  filter(variable %in% c("cost", "totalPotential")) %>% 
  group_by(technology, variable) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
  spread(variable, count)

data_density <- lapply(unique(mydata$technology),
                       function(x) {
                         
                         net_sn <- paste(net_names$shortname[which(net_names$longname == x)])
                         
                         if (net_sn != "EW") {
                           
                           data_pot  <- data_bu[[x]]$pot
                           data_cost <- data_bu[[x]]$cost
                           
                           pot_d   <- density(unlist(sapply(1:nrow(data_pot), function(y) rep(data_pot$v[y], data_pot$value[y]))))
                           pot_d$y <- pot_d$y/max(pot_d$y) #*counts$totalPotential[which(counts$technology == x)]/sum(counts$totalPotential)
                           
                           cost_d   <- density(unlist(sapply(1:nrow(data_cost), function(y) rep(data_cost$v[y], data_cost$value[y]))))
                           cost_d$y <- cost_d$y/max(cost_d$y) #*counts$cost[which(counts$technology == x)]/sum(counts$cost)
                           
                           return(
                             list(
                               "pot"  = data.frame(x=pot_d$x,  y=pot_d$y),
                               "cost" = data.frame(x=cost_d$y, y=cost_d$x))
                           )
                         }
                       })
names(data_density) <- unique(mydata$technology)

plt_pos <- list(
  "main"  = c(0.35, 0.95, 0.35, 0.95),
  "dpot"  = c(0.35, 0.95, 0.15, 0.35),
  "dcost" = c(0.15, 0.35, 0.35, 0.95)
)

xmin <- 0 
xmax <- 20
ymin <- 0 
ymax <- 200

net_cols <- c(
  "AR"    = "#1c9e77ff",
  "BECCS" = "#e72989ff",
  "BC"    = "#d95f02ff",
  "DAC"   = "#000000ff",
  "EW"    = "#d95f02ff",
  "OA"    = "#377eb8ff",
  "OF"    = "#e6ab02ff",
  "SCS"   = "#a6761dff"
)

par(mar = c(0,0,0,0),
    las = 1,
    plt = plt_pos$main)

plot(0,0,
     type="n",
     axes=FALSE,
     xlim=c(xmin-(xmax-xmin)*0.05,xmax+(xmax-xmin)*0.05), ylim=c(ymin,ymax+(ymax-ymin)*0.05),
     xlab="", ylab="",
     xaxs="i",yaxs="i")

# Grid
for (kx in seq(5,15,5)) lines(c(kx, kx),     c(ymin, ymax+(ymax-ymin)*0.05), col="#eeeeee")
for (ky in seq(50, 150, 50)) lines(c(xmin-(xmax-xmin)*0.05, xmax+(xmax-xmin)*0.05), c(ky, ky),     col="#eeeeee")

for (k in 1:nrow(all_sums)) {
  
  k_net <- all_sums$technology[k]
  
  rect(
    all_sums$pots_min[which(all_sums$technology == k_net)], all_sums$costs_min[which(all_sums$technology == k_net)],  
    all_sums$pots_max[which(all_sums$technology == k_net)], all_sums$costs_max[which(all_sums$technology == k_net)], 
    col=NA, border=paste(net_cols[net_names$shortname[which(net_names$longname == k_net)]]),
    lwd=2
  )
}

#axis(1, at=seq(0, 20,   5))
#axis(2, at=seq(0, 200, 50))

box()

plot_density2_stacked(
  data_density, xmin-(xmax-xmin)*0.05, xmax+(xmax-xmin)*0.05, seq(0, 20, 5),
  ax_lab = "Potential [Gt(CO2)]",
  plt = plt_pos$dpot,
  switch_axes = FALSE
)

plot_density2_stacked(
  data_density, ymin, ymax+(ymax-ymin)*0.05, seq(0, 200, 50),
  ax_lab = "Costs [$US/t(CO2)]",
  plt = plt_pos$dcost,
  switch_axes = TRUE
)

