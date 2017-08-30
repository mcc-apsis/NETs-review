rm(list=ls())
#==== USER SECTION ==========
u_sheetName <- "BECCS (so far only bioenergy potential)"
u_sheetName <- "Afforestation and Reforestation"
#u_sheetName <- "DAC"
#u_sheetName <- "BECCS (Bioenergy)"
DEBUG       <- TRUE

dir.create(paste0("plots/heatbars/",u_sheetName))

costsdir = paste0("plots/heatbars/",u_sheetName,"/costs")
potsdir = paste0("plots/heatbars/",u_sheetName,"/potentials")

dir.create(costsdir)
dir.create(potsdir)

#==== INITIALISE ==========
# Load libraries
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)

source("heatbars/heatbar_functions.R")

# Authorise googlesheets to access your Google Sheets account
gs_auth()


#==== READ IN SPREADSHEET ==========
gs  <- gs_title("NETs Review")
ss  <- gs_read(gs, ws = u_sheetName, verbose=DEBUG)

data <- get_data(ss)

################################################
## Generate a new df of ranges

# Adjust the maximum here to change the scale
ranges <- seq(0,300)
df <- data.frame(v=ranges)


# Get a list of resources, or define it yourself
resources <- unique(
  data[data$measurement=="max" & data$variable!="cost",]$variable
)
resources <- resources[!is.na(resources)]


costs <- unique(
  data[data$measurement=="max" & 
         data$variable=="cost",
       ]$variable
)

costs <- c("cost")


### Range
res2050 <- countranges(df, filter(data, PY > 2004), costs, "range")

heatbar(res2050,"pcnt") + 
  labs(x="",y="Costs in $US(2011)/tCO2") 

ggsave(paste0(costsdir,"/range_gt_2004.png"),width=8,height=5)


res2050 <- countranges(df, filter(data), costs, "range")

heatbar(res2050,"pcnt") + 
  labs(x="",y="Costs in $US(2011)/tCO2") 

ggsave(paste0(costsdir,"/range.png"),width=8,height=5)

######################################################
## Plot year bars (coloured)

heatbar_years(data, res2050, "pcnt")

ggsave(paste0(costsdir,"/range_years.png"),width=8,height=5)

heatbar_years(data, res2050, "pcnt", "region")

ggsave(paste0(costsdir,"/range_years_region.png"),width=8,height=5)

#########################################################################
## Now do potentials

ranges <- seq(0,10,by=0.1)

df <- data.frame(v=ranges)

pots2050 <- countranges(
  df, 
  mutate(
    filter(
      data,`Data categorisationsystem boundaries`=="Global", `Data categorisationyear`==2050
      ),
    value=as.numeric(value)/1000
  ),
  c("totalPotential"), 
  "max"
  )

heatbar(pots2050,"pcnt",step=0.1) + 
  labs(x="",y="Potentials (2050) Gt CO2/year")

ggsave(paste0(potsdir,"/max_2050.png"),width=8,height=5)


dataf <- data %>%
  filter(
    variable=="totalPotential",
    `Data categorisationsystem boundaries`=="Global",
    `Data categorisationyear`==2100,
    measurement=="max"
    )
