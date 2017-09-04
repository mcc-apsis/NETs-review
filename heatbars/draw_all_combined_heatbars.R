rm(list=ls())

#==== INITIALISE ==========
# Load libraries
library(googlesheets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)

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

rlabs <- costs2050 %>%
  group_by(resource) %>%
  summarise(
    resourcelab=paste0(gsub(" (terrestrial and ocean)","",first(resource),fixed=T),'\n[',max(maxvalue),' studies]')
  )


names(pics) <- rlabs$resourcelab


heatbar(costs2050,"pcnt") +
  theme(axis.text.x = element_text(angle=60, hjust=1,vjust=1)) + 
  labs(x="Technology",y="Costs in $US(2011)/tCO2") 

ggsave("plots/heatbars/all_costs.png")



heatbar(costs2050,"pcnt",text=T) +
  theme(axis.text.x = element_text(angle=60, hjust=1,vjust=1)) + 
  labs(x="",y="Costs in $US(2011)/tCO2") +
  theme(axis.text.x  = my_axis(pics))

ggsave("plots/heatbars/all_costs_labelled.png", width=16,height=10)



dataf <- filter(
  suppressWarnings(mutate(all_data,value=as.numeric(value))),
  measurement %in% c("min","max","estimate"),
  variable=="cost",
  !is.na(value)
) %>% spread(
  measurement, value
) %>%
  group_by(PY) %>%
  mutate(
    gtot = n(),
    pn = row_number()
  ) %>%
  ungroup() %>%
  mutate(
    PY = as.numeric(PY),
    jitter= (1/gtot)*(pn-1),
    PYJ = PY + (1/gtot)*(pn-1),
    country= substr(`Data categorisationsystem boundaries`,1,15),
    region = countrycode(`Data categorisationsystem boundaries`,"country.name","ar5")
  )

ggplot() +
  geom_crossbar(
    data=dataf,
    aes(x=PYJ, ymin=min, ymax=max,y=max, fill=technology),
    size=0.001,
    width=0.2,
    alpha=0.5
  ) +
  geom_point(
    data=dataf,
    aes(x=PYJ,y=estimate, colour=technology, shape=technology),
    size=2   
  ) +
  theme_bw() +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  labs(x="Study Year",y="Costs in $US(2011)/tCO2")

ggsave("plots/heatbars/all_costs_years.png")


ggplot() +
  geom_crossbar(
    data=dataf,
    aes(x=PYJ, ymin=min, ymax=max,y=max, fill=technology),
    size=0.001,
    width=0.2,
    alpha=0.5
  ) +
  geom_point(
    data=dataf,
    aes(x=PYJ,y=estimate, colour=technology, shape=technology),
    size=2   
  ) +
  theme_bw() +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  labs(x="Study Year",y="Costs in $US(2011)/tCO2") +
  facet_grid(technology~.)

ggsave("plots/heatbars/all_costs_years_faceted.png")


