rm(list=ls())
#==== USER SECTION ==========
u_sheetName <- "Bioenergy"
#u_sheetName <- "Afforestation and Reforestation"
#u_sheetName <- "DAC"
#u_sheetName <- "BECCS (Bioenergy)"
DEBUG       <- TRUE

#==== INITIALISE ==========
# Load libraries
library(googlesheets)
# library(dplyr)
# library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(parallel)
library(countrycode)


source("heatbars/heatbar_functions.R")

dir.create(paste0("plots/heatbars/",u_sheetName))

costsdir = paste0("plots/heatbars/",u_sheetName,"/costs")
potsdir = paste0("plots/heatbars/",u_sheetName,"/potentials")

dir.create(costsdir)
dir.create(potsdir)
# Authorise googlesheets to access your Google Sheets account
gs_auth()


#==== READ IN SPREADSHEET ==========
gs  <- gs_title("NETs Review")
ss  <- gs_read(gs, ws = u_sheetName, verbose=DEBUG)

ss <- [2:dim(ss),1:dim(ss)]

data <- get_data(ss,2)
names(data) <- make.names(names(data))


################################################
## Generate a new df of ranges

# Adjust the maximum here to change the scale
ranges <- seq(1,675)
df <- data.frame(v=ranges)

data_copy <- data %>%
  filter(variable=="totalPotential") %>%
  mutate(variable = Data.categorisationresource)

# Get a list of resources, or define it yourself
# resources <- unique(
#   data_copy[data_copy$measurement=="max" & data_copy$variable!="cost",]$variable
# )
# resources <- resources[!is.na(resources)]
resources<- list("Forestry", "Total", "Bioenergy Crops", "Residues")



# Count the studies with a maximum under each range for each resource
# Add any additional "Dimension" filters too
# 2050, Global estimates by resource category
res2050 <- countranges(
  df, 
  filter(
    data_copy, Data.categorisationyear == 2050 & 
      Data.categorisationsystem.boundaries == "Global"
    ), 
  resources, "max"
  )
heatbar(filter(res2050,!is.na(pcnt)),"pcnt") + 
  labs(x="Variable",y="Resources")
ggsave("plots/BECCS/bioenergyResource2050.png",width=8,height=5)


res2050 <- countranges(df, 
                       filter(data, Data.categorisationyear == 2050 & Data.categorisationsystem.boundaries == "Global"), 
                       TotalEstimates, "max")

heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Estimate") +
  ylim(c(0,60))
ggsave("plots/BECCS/bioenergypotential.png",width=8,height=5)


#ggsave("heatbar_example.png",width=8,height=5)

