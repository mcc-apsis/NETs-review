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

data <- get_data(ss,2)

################################################
## Generate a new df of ranges

##### POTENTIALS
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


# max
res2050 <- countranges(df, 
                       filter(data, `Data categorisationyear` == 2050 & `Data categorisationsystem boundaries` == "Global"),
                       resources,
                       "max"
                       )
heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Estimate") +
  ylim(c(0,20))
ggsave(paste0(potsdir,"/max_2050.png"),width=8,height=5)

# range
res2050 <- countranges(df, 
                       filter(data, `Data categorisationyear` == 2050 & `Data categorisationsystem boundaries` == "Global"),
                       resources,
                       "range"
)
heatbar(res2050,"pcnt") + 
  labs(x="Variable",y="Estimate") +
  ylim(c(0,20))

ggsave(paste0(potsdir,"/range_2050.png"),width=8,height=5)


###### Costs 

ranges <- seq(1,400)
df <- data.frame(v=ranges)
costs <- c("cost")

costs <- unique(
  data[data$measurement=="max" & 
         data$variable=="cost",
       ]$variable
)


res2050 <- countranges(df, 
                       filter(data),
                       costs,
                       "range"
)

heatbar_years(data, res2050, "pcnt")

ggsave(paste0(costsdir,"/range_years.png"),width=8,height=5)

heatbar_years(data, res2050, "pcnt", "`Data categorisationresource`")

ggsave(paste0(costsdir,"/range_years_region.png"),width=8,height=5)





#ggsave("heatbar_example.png",width=8,height=5)

