rm(list=ls())
#==== USER SECTION ==========
u_sheetName <- "Storage"
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

source("heatbars/heatbar_functions.R")

# Authorise googlesheets to access your Google Sheets account
gs_auth()


#==== READ IN SPREADSHEET ==========
gs  <- gs_title("NETs Review")
ss  <- gs_read(gs, ws = u_sheetName, verbose=DEBUG)

data <- get_data(ss,3)
names(data) <- make.names(names(data))


################################################
## Generate a new df of ranges

# Adjust the maximum here to change the scale
ranges <- seq(1,50)
df <- data.frame(v=ranges)

data_copy <- data %>%
  filter(variable=="totalPotential") %>%
  mutate(variable = Data.categorisationresource)

# Get a list of resources, or define it yourself
# resources <- unique(
#   data_copy[data_copy$measurement=="max" & data_copy$variable!="cost",]$variable
# )
# resources <- resources[!is.na(resources)

resources <- list("Aquifers", "Coal beds", "DNG", "DOF", "DOG", "Total")


# Count the studies with a maximum under each range for each resource
# Add any additional "Dimension" filters too
res <- countranges(
  df, 
  filter(
    data_copy, Data.categorisationsystem.boundaries == "Global" | 
      Data.categorisationsystem.boundaries == "Global, Review"
    ),
  resources, "max"
  )
heatbar(res,"pcnt") + 
  labs(x="Variable",y="Location")
ggsave("plots/BECCS/Storage_Global.png",width=8,height=5)

res2050 <- countranges(df, filter(data), costs, "min")
heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Cost") +
  ylim(c(0,200))
ggsave("plots/afforestation/min.png",width=8,height=5)


res2050 <- countranges(df, filter(data, PY > 2004), costs, "max")
heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Cost")
ggsave("plots/afforestation/max_gt_2004.png",width=8,height=5)

res2050 <- countranges(df, filter(data, PY > 2004), costs, "min")
heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Cost") +
  ylim(c(0,200))

ggsave("plots/afforestation/min_gt_2004.png",width=8,height=5)





#ggsave("heatbar_example.png",width=8,height=5)



