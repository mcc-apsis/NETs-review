rm(list=ls())

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


#######################################
## Go through all sheets, and merge all the data

sheets <- gs$ws$ws_title

sheets <- sheets[!(sheets %in% c("Bioenergy","Storage","Template"))]

all_data <- data.frame()

for (u_sheetName in sheets) {
  print(u_sheetName)
  Sys.sleep(3)
  ss  <- gs_read(gs, ws = u_sheetName, verbose=FALSE)
  data <- get_data(ss)
  data$technology <- u_sheetName
  data$PY <- as.numeric(data$PY)
  all_data <- bind_rows(all_data, data)
}

###################################
## Plot costs for all estimates and all technologies


costs <- all_data %>%
  filter(variable=="cost") %>%
  mutate(variable=technology)

techs <- unique(all_data$technology)


ranges <- seq(0,1000)
df <- data.frame(v=ranges)

costs2050 <- countranges(
  df, 
  mutate(
    filter(
      costs
    ),
    value=as.numeric(gsub("[^0-9\\.]", "", value))
  ), 
  techs, "range")


heatbar(costs2050,"pcnt") +
  theme(axis.text.x = element_text(angle=60, hjust=1,vjust=1))

heatbar(costs2050,"pcnt",text=T) +
  theme(axis.text.x = element_text(angle=60, hjust=1,vjust=1))
