

rm(list=ls())
#==== USER SECTION ==========
u_sheetName <- "Enhanced weathering (terrestrial and ocean)"
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
ranges <- seq(1,1000)
df <- data.frame(v=ranges)


# Get a list of resources, or define it yourself

costs <- c("cost")

### Range
res2050 <- countranges(
  df, 
  filter(
    data,
    `Data categorisationsystem conditions`!="MIL-101" | 
      is.na(`Data categorisationsystem conditions`)
  ), 
  costs, "range")

heatbar(res2050,"pcnt") + 
  labs(x="",y="Costs in $US(2011)/tCO2") 

ggsave(paste0(costsdir,"/range.png"),width=8,height=5)

######################################################
## Plot year bars (coloured)

heatbar_years(data, res2050, "pcnt")

ggsave(paste0(costsdir,"/range_years.png"),width=8,height=5)
