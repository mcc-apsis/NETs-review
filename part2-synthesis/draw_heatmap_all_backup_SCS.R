rm(list=ls())
library(dplyr)
load("data/all_data.RData") #BOTTOM UP 

costs <- 0

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
    cost_min = -50,
    cost_max = 200,
    cost_n   = 1000
  )
)  %>% 
  do.call("rbind", .)

# Expert judgment data based on Synthesis table in NEt review part 2
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
    cost_min = 30,
    cost_max = 120
  ),
  data.frame(
    net      = "DAC",
    pot_min  = 0.5,
    pot_max  = 5,
    cost_min = 100,
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

source("heatbars/heatbar_functions.R")
source("part2-synthesis/heatmap_functions.R")
source("../../bitbucket/beccs/functions/useful_functions.R")

plotdir <- paste0("plots/synthesis-part2/test") #,u_sheetName)

dir.create(plotdir, recursive = TRUE)


#==== READ IN DATA ==========
#load("data/all_data.RData") #BOTTOM UP 
load("../../bitbucket/beccs/data/dataplotAll.RData") # IAM


#==== PROCESS DATA ==========
all_data$TI[is.na(all_data$TI)] <- all_data$CITATION[is.na(all_data$TI)] #Fix TI/CITATION


all_data <- all_data %>%
  filter(!technology %in% c("Storage", "Bioenergy")) %>% 
  group_by(TI, variable, `Data categorisationresource`, boundaries, year, measurement) %>%
  arrange(value) %>%
  mutate(ind = row_number()) %>%
  filter(ind==1) %>%
  ungroup()

# TODO: EW and OA are additive!! => Add up potentials (and costs?)
#all_data$technology[which(all_data$technology == "Enhanced weathering (terrestrial and ocean)")] <- "Enhanced weathering (terrestrial and ocean) and Ocean alkalinisation"
#all_data$technology[which(all_data$technology == "Ocean alkalinisation")]                        <- "Enhanced weathering (terrestrial and ocean) and Ocean alkalinisation"

# all_data <- all_data %>%
#   mutate()

#Bug fix SCS min cost value set to -50 (McKinsey 2007)
all_data$value[which(grepl("Smith", all_data$AU) & all_data$PY == 2008 & all_data$variable == "cost" & all_data$measurement == "min" & all_data$technology == "Soil Carbon Sequestration" & all_data$boundaries == "global")] <- -45

#==== PROCESS DATA ==========
data_steps <- list()
data_bu    <- list()
for (k_net in c("Soil Carbon Sequestration")) { #unique(all_data$technology)) {
  
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
                                     ), net_sn, data_steps[[k_net]]$pot, "max"),
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

#==== PLOT DATA =============
for (k_net in c("Soil Carbon Sequestration")){ #unique(all_data$technology)) {
  
  net_sn <- paste(net_names$shortname[which(net_names$longname == k_net)])
  print(net_sn)
  if (net_sn != "DAC") {
    
    do_rev <- TRUE
    data_rev <- all_data %>% 
      filter(technology == k_net) %>% 
      filter(variable %in% c("totalPotential", "cost")) %>% 
      unite(var.mes, variable, measurement, sep=".") %>% 
      select(-category)%>% 
      filter(grepl("Review", `Data categorisationresource`))
    
    if (nrow(data_rev) > 0) {
      data_rev <- data_rev %>% spread(var.mes, value)
    } else {
      data_rev <- NULL
      do_rev <- FALSE
    }
    
    plot_synthesis_part2_SCS(
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
      do_iam = FALSE, #ifelse(k_net == "BECCS", TRUE, FALSE),
      do_rev = FALSE,
      alpha  = "33",
      #--- Other options ---
      xlab  = "Potential [Gt(CO2)]",
      ylab  = "Costs [$US/t(CO2)]",
      title = "[NET name]",
      file  = file.path(plotdir, paste0("heatmap_", net_sn, "new.png")),
      DEBUG = DEBUG
    )
  } else {
    do_rev <- TRUE
    data_rev <- all_data %>% 
      filter(technology == k_net) %>% 
      filter(variable %in% c("totalPotential", "cost")) %>% 
      unite(var.mes, variable, measurement, sep=".") %>% 
      select(-category) %>% 
      filter(grepl("Review", `Data categorisationresource`))
    
    if (nrow(data_rev) > 0) {
      data_rev <- data_rev %>% spread(var.mes, value)
    } else {
      data_rev <- NULL
      do_rev <- FALSE
    }
    
    plot_synthesis_part2_DAC(
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
      do_iam = FALSE, #ifelse(k_net == "BECCS", TRUE, FALSE),
      do_rev = do_rev,
      alpha  = "33",
      #--- Other options ---
      xlab  = "Potential [Gt(CO2)]",
      ylab  = "Costs [$US/t(CO2)]",
      title = "[NET name]",
      file  = file.path(plotdir, paste0("heatmap_", net_sn, ".png")),
      DEBUG = DEBUG
    )
  }
}