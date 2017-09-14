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
library(parallel)
library(countrycode)


source("heatbars/heatbar_functions.R")

#dir.create(paste0("plots/heatbars/",u_sheetName))

costsdir = paste0("plots/heatbars/",u_sheetName,"/costs")
potsdir = paste0("plots/heatbars/",u_sheetName,"/potentials")

dir.create(costsdir)
dir.create(potsdir)


# Authorise googlesheets to access your Google Sheets account
gs_auth()


#==== READ IN SPREADSHEET ==========
gs  <- gs_title("NETs Review")
ss  <- gs_read(gs, ws = u_sheetName, verbose=DEBUG)

data <- get_data(ss,3)

#data <- filter(data, `Data categorisationresource` == "DOG")
################################################
## Generate a new df of ranges

# Adjust the maximum here to change the scale
ranges <- seq(1,5050)
df <- data.frame(v=ranges)

data_copy <- data %>%
  filter(variable=="totalPotential") %>%
  mutate(variable = `Data categorisationresource`)

# Get a list of resources, or define it yourself
# resources <- unique(
#   data_copy[data_copy$measurement=="max" & data_copy$variable!="cost",]$variable
# )
# resources <- resources[!is.na(resources)

resources <- list("Aquifers", "Coal beds", "DNG", "DOF", "DOG", "Total")
costs <- c("cost")

# extracting theoretical potentials to put in caption, 
# Count the studies with a maximum under each range for each resource
# Add any additional "Dimension" filters too
store <- countranges(
  df, 
  filter(
    data_copy, `Data categorisationsystem boundaries` == "Global" | 
      `Data categorisationsystem boundaries` == "Global, Review"
    ),
  resources, "max"
  )

#save(store, file = "heatbars/storage5000.RData")

heatbar(store,"pcnt") + 
  labs(x="Variable",y="Location")
#ggsave("plots/BECCS/Storage_Global.png",width=8,height=5)

store <- countranges(
  df, 
  filter(
    data_copy, `Data categorisationsystem boundaries` == "Global" | 
      `Data categorisationsystem boundaries` == "Global, Review"
  ),
  resources, "range"
)
heatbar(store,"pcnt") + 
  labs(x="Variable",y="Location")

#Bars with resources with the lines with colors for type of estimate
#might have to do it per bar 
data_copy <- data %>% 
  filter(`Data categorisationsystem boundaries` == "Global" | 
           `Data categorisationsystem boundaries` == "Global, Review" 
           ) %>% 
  filter(variable == "totalPotential") %>% 
  mutate(resource =`Data categorisationresource`,
         value = ifelse(value > 5000, 5000,value), 
         PY = as.numeric(PY), 
         value = as.numeric(value))

h1 <- heatbar_years(data_copy, 
                    store,
                    "pcnt",
                    "`Data categorisationmethod`",
                    graph = TRUE,
                    var = "totalPotential", 
                    y = 2000,
                    w = 20
                    )

h1[[1]] + facet_grid(.~resource)+
  geom_jitter(data = data_copy, aes(x = PY, y = value))
ggsave("plots/BECCS/Storage_jitter.png",width=8,height=5)

#ggsave("heatbar_example.png",width=8,height=5)



