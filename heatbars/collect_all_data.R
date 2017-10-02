rm(list=ls())

#==== INITIALISE ==========
# Load libraries
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)
library(ggrepel)
library(parallel)

source("heatbars/heatbar_functions.R")

############################################################
## Get pics

image.file <- dir("icons", pattern=".png", full.names=TRUE)

image.file <- image.file[order(as.integer(sub("_.*","",sub("icons/","",image.file))))]
npoints <- length(image.file)
pics  <- vector(mode="list", length=npoints)
for(i in 1:npoints) {
  pics[[i]] <- EBImage::readImage(image.file[i])
}
names(pics) <- sub(".png","",sub("icons/","",image.file))



# Authorise googlesheets to access your Google Sheets account
gs_auth()


#==== READ IN SPREADSHEET ==========
gs  <- gs_title("NETs Review")


#######################################
## Go through all sheets, and merge all the data

sheets <- gs$ws$ws_title

sheets <- sheets[!(sheets %in% c("Template"))]

all_data <- data.frame()

datarows <- data.frame()

for (u_sheetName in sheets) {
  print(u_sheetName)
  Sys.sleep(6)
  ss  <- gs_read(gs, ws = u_sheetName, verbose=FALSE)
  data <- get_data(ss)
  data$technology <- u_sheetName
  data$PY <- as.numeric(data$PY)
  all_data <- bind_rows(all_data, data)
  datarows <- bind_rows(datarows,ss)
}

length(unique(datarows$X3))

#######################################
## Fix one or two problems in the old data

all_data <- all_data %>%
  mutate(
    year = `Data categorisationyear`,
    nyear = as.numeric(year),
    value = as.numeric(value),
    pcf = `Potentials in tCO2/yrconversion factor to common unit`,
    ccf = `Costs in $US(2011)/tCO2conversion factor to common unit`,
    boundaries = tolower(`Data categorisationsystem boundaries`)
  ) 

######################################
## convert bioenergy data to seq pots

all_data$value[
  all_data$technology=="Bioenergy" & all_data$variable=="totalPotential"
  ] <- all_data$value[
    all_data$technology=="Bioenergy" & all_data$variable=="totalPotential"
    ] * 0.04982206 - 1.432129

all_data$value[
  all_data$technology=="Bioenergy" & all_data$variable=="totalPotential" &
    all_data$value < 1
  ] <- 1

###############################
## Standardise units from enhanced weathering/ocean stuff

ews <- c(
  "Enhanced weathering (terrestrial and ocean)",
  "Ocean alkalinisation",
  "Ocean fertilization"
)

all_data$pcf <- as.numeric(lapply(all_data$pcf, evcf))
all_data$ccf <- as.numeric(lapply(all_data$ccf, evcf))

all_data$value[
  all_data$technology %in% ews & 
    all_data$variable=="totalPotential"
  ] <- all_data$value[all_data$technology %in% ews & 
                        all_data$variable=="totalPotential"
                      ] * all_data$pcf[
                        all_data$technology %in% ews & 
                          all_data$variable=="totalPotential"
                        ]

all_data$value[
  all_data$technology %in% ews & 
    all_data$variable=="cost"
  ] <- all_data$value[all_data$technology %in% ews & 
                        all_data$variable=="cost"
                      ] * all_data$ccf[
                        all_data$technology %in% ews & 
                          all_data$variable=="cost"
                        ]


#########################################
## We should define some criteria for each tech - it's always different

# First record the old technologies before merge
all_data$technology_detail <- all_data$technology

# Do this for costs and potentials
all_data$costsinclude=F
all_data$potsinclude=F

all_data$boundaries[is.na(all_data$boundaries)] <- ""

#DAC - remove mil-101, as MG-2 has a bigger range
#pots
all_data$costsinclude[
  all_data$technology=="DAC" &
    (is.na(all_data$`Data categorisationsystem conditions`) |
       all_data$`Data categorisationsystem conditions`=="MG-2")
  ] <- T

#Bioenergy (BECCS POTS)
all_data$potsinclude[
  all_data$technology=="Bioenergy" & grepl("global",all_data$boundaries) &
    all_data$nyear==2050
  ] <- T

all_data$technology[
  all_data$technology=="Bioenergy" & all_data$variable=="totalPotential"
] <- "BECCS"


############
## merge ocean alk with ew

# all_data$technology[
#   all_data$technology=="Ocean alkalinisation"
#   ] <- "Enhanced weathering (terrestrial and ocean)"

#BECCS - include everything that is not cumulative
all_data$costsinclude[
  all_data$technology=="BECCS" &
    !(all_data$boundaries %in% c("cumulative","exclude"))
  ] <- T


all_data$`Data categorisationmethod`[
  is.na(all_data$`Data categorisationmethod`)] <- ""
#Storage need to have a look at this
all_data$potsinclude[
  all_data$technology=="Storage" & grepl("global",all_data$boundaries) &
    all_data$`Data categorisationmethod` !="theoretical"
  ] <- T
# 
# all_data$technology[
#   all_data$technology=="Bioenergy" & all_data$variable=="totalPotential"
#   ] <- "BECCS"


# 
# all_data$potsinclude[
#   all_data$technology=="BECCS" &
#     !(all_data$boundaries %in% c("cumulative","exclude"))
#   ] <- T



#EW include everything global
all_data$costsinclude[
  all_data$technology=="Enhanced weathering (terrestrial and ocean)" 
  ] <- T

all_data$potsinclude[
  all_data$technology=="Enhanced weathering (terrestrial and ocean)" &
    grepl("global",all_data$boundaries)
  ] <- T

# Ocean fertilisation - include everything global
all_data$costsinclude[
  all_data$technology=="Ocean fertilization" 
  ] <- T

all_data$potsinclude[
  all_data$technology=="Ocean fertilization" &
    grepl("global",all_data$boundaries)
  ] <- T

# Ocean alk - include everything global
all_data$costsinclude[
  all_data$technology=="Ocean alkalinisation" 
  ] <- T

all_data$potsinclude[
  all_data$technology=="Ocean alkalinisation" &
    grepl("global",all_data$boundaries)
  ] <- T

# Biochar - include everything global
all_data$costsinclude[
  all_data$technology=="Biochar" 
  ] <- T

all_data$potsinclude[
  all_data$technology=="Biochar" &
    all_data$boundaries=="global"
  ] <- T

# Soil Carbon Sequestration
all_data$potsinclude[
  all_data$technology=="Soil Carbon Sequestration" &
    grepl("global",all_data$boundaries)
  ] <- T

all_data$costsinclude[
  all_data$technology=="Soil Carbon Sequestration"
  ] <- T

# AR - include everything global, in 2050 and flux measurement
all_data$potsinclude[
  all_data$technology=="Afforestation and Reforestation" &
    grepl("global",all_data$boundaries) &
    all_data$nyear==2050 &
    all_data$`Potentials in Mt CO2/yearEstimate type`=="Flux"
  ] <- T

all_data$costsinclude[
  all_data$technology=="Afforestation and Reforestation"
  ] <- T

all_data$AU[is.na(all_data$AU)] <- all_data$UT[is.na(all_data$AU)]

bcomma <- function(x) {
  return(strsplit(x,",")[[1]][[1]])
}

all_data$label <- paste0(as.character(lapply(all_data$AU, bcomma)),", ",all_data$PY)


# t -> gt or mt -> gt
all_data$value[
  all_data$technology=="Biochar" &all_data$variable=="totalPotential"
  ] <- all_data$value[all_data$technology=="Biochar" & all_data$variable=="totalPotential"]/1000000000

all_data$value[
  all_data$technology=="Afforestation and Reforestation" & all_data$variable=="totalPotential"
  ] <- all_data$value[
    all_data$technology=="Afforestation and Reforestation" & all_data$variable=="totalPotential"
    ]/1000

all_data$value[
  all_data$technology %in% ews & all_data$variable=="totalPotential"
  ] <- all_data$value[
    all_data$technology %in% ews & all_data$variable=="totalPotential"
    ]/1000000000


save(all_data,file="data/all_data.RData")
