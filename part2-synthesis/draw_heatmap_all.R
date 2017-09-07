rm(list=ls())
#==== USER SECTION ==========
u_sheetName <- "BECCS"
#u_sheetName <- "Afforestation and Reforestation"
#u_sheetName <- "DAC"
#u_sheetName <- "BECCS (Bioenergy)"
DEBUG       <- TRUE

#==== INITIALISE ==========
# Load libraries
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(plotrix)


source("heatbars/heatbar_functions.R")
source("../../bitbucket/beccs/functions/useful_functions.R")

dir.create(paste0("plots/heatbars/",u_sheetName))

plotdir = paste0("plots/synthesis-part2/",u_sheetName)

dir.create(plotdir)



steps <- list(
  data.frame(
    net      = "BECCS",
    pot_min  = 1,
    pot_max  = 20,
    pot_n    = 1000,
    cost_min = 1,
    cost_max = 400,
    cost_n   = 1000
  )
)  %>% 
  do.call("rbind", .)

# Expert judgment data based on Synthesis table in NEt review part 2
data_ej <- list(
  data.frame(
    net      = "BECCS",
    pot_min  = 2,
    pot_max  = 10,
    cost_min = 80,
    cost_max = 250
  ),
  data.frame(
    net      = "Afforestation and reforestation",
    pot_min  = 1,
    pot_max  = 10,
    cost_min = 1,
    cost_max = 6
  )
) %>% 
  do.call("rbind", .)

net_names <- data.frame(
  longname  = sort(unique(all_data$technology)),
  shortname = c("AR", "BECCS", "BC", "DAC", "EW", "OA", "OF", "SCS")
)


#==== READ IN DATA ==========
load("data/all_data.RData") #BOTTOM UP 
load("../../bitbucket/beccs/data/dataplotAll.RData") # IAM


#==== PLOT DATA =============

for (k_net in unique(all_data$technology)) {
  
  if (net_names$shortname[which(net_names$longname == k_net)] != "EW") {
    
    pot_steps <- seq(steps$pot_min[which(steps$net == k_net)], 
                     steps$pot_max[which(steps$net == k_net)], 
                     (steps$pot_max[which(steps$net == k_net)]-steps$pot_min[which(steps$net == k_net)])/steps$pot_n[which(steps$net == k_net)])
    cost_steps <- seq(steps$cost_min[which(steps$net == k_net)], 
                      steps$cost_max[which(steps$net == k_net)],
                      (steps$cost_max[which(steps$net == k_net)]-steps$cost_min[which(steps$net == k_net)])/steps$cost_n[which(steps$net == k_net)])
    
    data_pot  <- generate_potentials(all_data, k_net, pot_steps)
    data_cost <- generate_costs(all_data,      k_net, cost_steps)
    
    data_rev <- all_data %>% 
      filter(technology == k_net) %>% 
      filter(variable %in% c("totalPotential", "cost")) %>% 
      unite(var.mes, variable, measurement, sep=".") %>% 
      select(-category) %>% 
      filter(`Data categorisationresource` == "Review") %>% 
      spread(var.mes, value)
    
    plot_synthesis_part2(
      data_pot,  # Bottom-up data potential
      data_cost, # Bottom-up data costs
      v_data_tempTargets_world_plot %>% 
        filter(variable %in% c("Price|Carbon", "Emissions|CO2|Carbon Capture and Storage|Biomass")), # IAM data
      data_rev, # Previous review data 
      data_ej %>% 
        filter(net == k_net),  # Expert judgment data
      #--- Data options ---
      0.50,  # Threshold for density plots (same for both)
      2050,  # IAM Period for boxplots
      #--- Plot options ---
      do_bu  = TRUE,
      do_iam = FALSE,
      do_rev = TRUE,
      alpha  = "99",
      #--- Other options ---
      xlab  = "Potential [Gt(CO2)]",
      ylab  = "Costs [$US/t(CO2)]",
      title = "[NET name]",
      file  = file.path(plotdir, paste0("heatmap_", net_names$shortname[which(net_names$longname == k_net)], ".svg")),
      DEBUG = TRUE
    )
  }
}