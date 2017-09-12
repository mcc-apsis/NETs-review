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
library(ggrepel)


source("heatbars/heatbar_functions.R")

#dir.create(paste0("plots/heatbars/",u_sheetName))

costsdir = paste0("plots/heatbars/",u_sheetName,"/costs")
potsdir = paste0("plots/heatbars/",u_sheetName,"/potentials")

# dir.create(costsdir)
# dir.create(potsdir)

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

ranges <- seq(1,450)
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

heatbar(res2050, "pcnt")

heatbar_years(data, res2050, "pcnt", graph = TRUE)

ggsave(paste0(costsdir,"/range_years.png"),width=8,height=5)

heatbar_years(data, res2050, "pcnt", "`Data categorisationresource`")

ggsave(paste0(costsdir,"/range_years_region.png"),width=8,height=5)

# with labels
heatbar_years(data, res2050, "pcnt", "`Data categorisationresource`") +
  geom_text_repel(data = data_copy, aes(label = UT), check_overlap = TRUE)

h1<- heatbar_years(data, res2050, "pcnt", graph = TRUE, y = 2010, w = 15)
h1[[1]] +  geom_text_repel(data = mutate(h1[[2]],UT = paste(substr(UT, 1,10),PY)), 
                           aes(x = PYJ, y = max, label = UT, angle = 90) 
)
ggsave(paste0(costsdir,"/range_years_labels2.png"),width=16,height=5)


h2<- heatbar_years(data, res2050, "pcnt","`Data categorisationresource`", graph = TRUE, y = 2010, w = 15)
h2[[1]] +  geom_text_repel(data = mutate(h2[[2]],UT = paste(substr(UT, 1,10),PY)), 
                           aes(x = PYJ, y = max, label = UT, angle = 90) 
)
ggsave(paste0(costsdir,"/range_years_labels3.png"),width=16,height=5)

##### costs separated by resource
# copy resource type to appropriate variable location

data_copy <- data %>%
  filter(variable=="cost") %>%
  mutate(variable = `Data categorisationresource`,
         value = as.numeric(value), 
         UT = paste(substr(UT, 1,10),PY)
  )


# list of resources
resources <- unique(
  data_copy[data_copy$measurement=="max" & data_copy$variable!="cost",]$variable
)
resources <- resources[!is.na(resources)]

res2050 <- countranges(
  df, 
  filter(data_copy),
  resources,
  "range"
)

heatbar(res2050, "pcnt")


ggsave(paste0(costsdir,"/costs_tech.png"),width=8,height=5)


#ggsave("heatbar_example.png",width=8,height=5)

