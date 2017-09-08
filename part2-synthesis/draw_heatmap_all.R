rm(list=ls())
#==== USER SECTION ==========
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
    pot_max  = 40,
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
    pot_min  = 3,
    pot_max  = 4,
    cost_min = 70,
    cost_max = 100
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
    pot_max  = 8,
    cost_min = 30,
    cost_max = 100
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

dir.create(plotdir, recursive = TRUE)


#==== READ IN DATA ==========
load("data/all_data.RData") #BOTTOM UP 
load("../../bitbucket/beccs/data/dataplotAll.RData") # IAM


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
                                       costsinclude==T, potsinclude==T
                                     ), net_sn, data_steps[[k_net]]$pot),
      "cost" = generate_costs(all_data %>% filter(technology == k_net) %>%
                                filter(
                                  !is.na(value) , 
                                  is.finite(value),
                                  costsinclude==T, potsinclude==T
                                ), net_sn, data_steps[[k_net]]$cost)
    )
  }
}


#==== PLOT DATA =============
for (k_net in unique(all_data$technology)) {
  
  net_sn <- paste(net_names$shortname[which(net_names$longname == k_net)])
  
  if (net_sn != "DAC") {
   
    do_rev <- TRUE
    data_rev <- all_data %>% 
      filter(technology == k_net) %>% 
      filter(variable %in% c("totalPotential", "cost")) %>% 
      unite(var.mes, variable, measurement, sep=".") %>% 
      select(-category) %>% 
      filter(`Data categorisationresource` == "Review")
    
    if (nrow(data_rev) > 0) {
      data_rev <- data_rev %>% spread(var.mes, value)
    } else {
      data_rev <- NULL
      do_rev <- FALSE
    }
    
    plot_synthesis_part2(
      data_bu[[k_net]]$pot,  # Bottom-up data potential
      data_bu[[k_net]]$cost, # Bottom-up data costs
      v_data_tempTargets_world_plot %>% 
        filter(variable %in% c("Price|Carbon", "Emissions|CO2|Carbon Capture and Storage|Biomass")), # IAM data
      data_rev, # Previous review data 
      data_ej %>% 
        filter(net == net_sn),  # Expert judgment data
      #--- Data options ---
      0.50,  # Threshold for density plots (same for both)
      2050,  # IAM Period for boxplots
      #--- Plot options ---
      do_bu  = TRUE,
      do_iam = ifelse(k_net == "BECCS", TRUE, FALSE),
      do_rev = do_rev,
      alpha  = "33",
      #--- Other options ---
      xlab  = "Potential [Gt(CO2)]",
      ylab  = "Costs [$US/t(CO2)]",
      title = "[NET name]",
      file  = NULL, #file.path(plotdir, paste0("heatmap_", net_sn, ".svg")),
      DEBUG = DEBUG
    )
  }
}