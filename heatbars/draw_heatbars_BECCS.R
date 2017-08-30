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

source("heatbars/heatbar_functions.R")

# Authorise googlesheets to access your Google Sheets account
gs_auth()


#==== READ IN SPREADSHEET ==========
gs  <- gs_title("NETs Review")
ss  <- gs_read(gs, ws = u_sheetName, verbose=DEBUG)

data <- get_data(ss,2)
names(data) <- make.names(names(data))

################################################
## Generate a new df of ranges

# Adjust the maximum here to change the scale
ranges <- seq(1,20)
df <- data.frame(v=ranges)


# Get a list of resources, or define it yourself
resources <- unique(
  data[data$measurement=="max" & data$variable!="cost",]$variable
)
resources <- resources[!is.na(resources)]


#   unique(
#   data[data$measurement == "max" &
#          data&variable == "totalPotential",
#        ]$variable
# )


# Count the studies with a maximum under each range for each resource
# Add any additional "Dimension" filters too


#potentials <----
res2050 <- countranges(df, 
                       filter(data, Data.categorisationyear == 2050 & Data.categorisationsystem.boundaries == "Global"),
                       resources,
                       "max"
                       )
heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Estimate") +
  ylim(c(0,20))
ggsave("plots/BECCS/potential.png",width=8,height=5)

#### Costs <----
data_copy <- data %>%
  filter(variable=="cost") %>%
  mutate(variable = Data.categorisationresource)

# costs <- unique(
#   data_copy[data_copy$measurement=="max",
#        ]$variable
# )

costs <- list()

res2050 <- countranges(df, filter(data_copy), costs, "max")
heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Cost")
ggsave("plots/BECCS/max.png",width=8,height=5)

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

