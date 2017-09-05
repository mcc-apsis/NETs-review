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
  Sys.sleep(6)
  ss  <- gs_read(gs, ws = u_sheetName, verbose=FALSE)
  data <- get_data(ss)
  data$technology <- u_sheetName
  data$PY <- as.numeric(data$PY)
  all_data <- bind_rows(all_data, data)
}

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

all_data$include=F

#DAC - remove mil-101, as MG-2 has a bigger range
all_data$include[
  all_data$technology=="DAC" &
    (is.na(all_data$`Data categorisationsystem conditions`) |
       all_data$`Data categorisationsystem conditions`=="MG-2")
  ] <- T

#BECCS - include everything that is not cumulative
all_data$include[
  all_data$technology=="BECCS" &
    all_data$boundaries!="cumulative"
  ] <- T

#EW include everything global
all_data$include[
  all_data$technology=="Enhanced weathering (terrestrial and ocean)" &
    grepl("global",all_data$boundaries)
  ] <- T

# Ocean fertilisation - include everything global
all_data$include[
  all_data$technology=="Ocean fertilization" &
    grepl("global",all_data$boundaries)
  ] <- T

# Ocean alk - include everything global
all_data$include[
  all_data$technology=="Ocean alkalinisation" &
    grepl("global",all_data$boundaries)
  ] <- T

# Biochar - include everything global
all_data$include[
  all_data$technology=="Biochar" &
    all_data$boundaries=="global"
  ] <- T

# Soil Carbon Sequestration
all_data$include[
  all_data$technology=="Soil Carbon Sequestration" &
    all_data$boundaries=="global"
  ] <- T

# AR - include everything global, in 2050 and flux measurement
all_data$include[
  all_data$technology=="Afforestation and Reforestation" &
    grepl("global",all_data$boundaries) &
    all_data$nyear==2050 &
    all_data$`Potentials in Mt CO2/yearEstimate type`=="Flux"
  ] <- T

all_data$AU[is.na(all_data$AU)] <- all_data$UT[is.na(all_data$AU)]




###################################
## Plot costs for all estimates and all technologies


costs <- all_data %>%
  filter(variable=="cost" & include==T) %>%
  mutate(variable=technology)

techs <- unique(costs$technology)


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
  costs,
  measurement %in% c("min","max","estimate"),
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



###################################
## Plot potentials for all estimates and all technologies

pots <- all_data %>%
  filter(variable=="totalPotential" & include==T) %>%
  mutate(
    variable=technology
    ) 

# Transform units
# Biochar t->gigatons
pots$value[pots$technology=="Biochar"] <- pots$value[pots$technology=="Biochar"]/1000000000
pots$value[pots$technology=="Afforestation and Reforestation"] <- pots$value[pots$technology=="Afforestation and Reforestation"]/1000
pots$value[pots$technology %in% ews] <- pots$value[pots$technology %in% ews]/1000000000

pots$measurement <- gsub(" (Gt CO2/yr)","",pots$measurement,fixed=T)

ggplot(pots) +
  geom_jitter(
    aes(year,value,colour=technology,shape=measurement)
  ) + theme_bw() 

ggsave("plots/heatbars/potentials.png")

bcomma <- function(x) {
  return(strsplit(x,",")[[1]][[1]])
}

pots$label <- as.character(lapply(pots$AU, bcomma))

ggplot() +
  geom_jitter(
    data=pots,
    aes(technology,value,colour=technology,shape=measurement)
  ) + theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1,vjust=1)) +
  ggrepel::geom_label_repel(
    data=filter(pots,value> 30 | value < 0.5),
    aes(technology, value, label=label)
  )
  

ggsave("plots/heatbars/all_potentials.png")

ggplot() +
  geom_jitter(
    data=pots,
    aes(PY,value,colour=technology,shape=measurement)
  ) + theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1,vjust=1)) +
  ggrepel::geom_label_repel(
    data=filter(pots,value> 30 | value < 0.5),
    aes(PY, value, label=label)
  ) +
  facet_grid(technology~.)


ggsave("plots/heatbars/all_potentials_faceted.png",width=16,height=32)



ggplot(pots) +
  geom_point(
    aes(nyear,value,colour=technology,shape=measurement)
  ) + theme_bw()

ggsave("plots/heatbars/potentials_numeric_year.png")



techs <- unique(pots$technology)


ranges <- seq(0,100)
df <- data.frame(v=ranges)

potsranges <- countranges(
  df, 
  mutate(
    filter(
      pots
    ),
    value=as.numeric(value)
  ),
  techs, "max")

rlabs <- potsranges %>%
  group_by(resource) %>%
  summarise(
    resourcelab=paste0(gsub(" (terrestrial and ocean)","",first(resource),fixed=T),'\n[',max(maxvalue),' studies]')
  )


names(pics) <- rlabs$resourcelab


heatbar(potsranges,"pcnt") +
  theme(axis.text.x = element_text(angle=60, hjust=1,vjust=1)) + 
  labs(x="",y="Potentials in Gt CO2/year") +
  theme(axis.text.x  = my_axis(pics))

ggsave("plots/heatbars/all_potentials_labelled.png", width=16,height=10)

