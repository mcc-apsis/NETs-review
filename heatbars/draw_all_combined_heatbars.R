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

#BECCS - include everything that is not cumulative
all_data$costsinclude[
  all_data$technology=="BECCS" &
    !(all_data$boundaries %in% c("cumulative","exclude"))
  ] <- T

all_data$potsinclude[
  all_data$technology=="BECCS" &
    !(all_data$boundaries %in% c("cumulative","exclude"))
  ] <- T


#EW include everything global
all_data$costsinclude[
  all_data$technology=="Enhanced weathering (terrestrial and ocean)"
  ] <- T

all_data$potsinclude[
  all_data$technology=="Enhanced weathering (terrestrial and ocean)"
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
    all_data$boundaries=="global"
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


#### Add in max + min 0.5 around estimate
all_data$TI[is.na(all_data$TI)] <- all_data$CITATION[is.na(all_data$TI)]

onames <- names(all_data)
dataf <- suppressWarnings(mutate(all_data,value=as.numeric(value))) %>%
  group_by(TI, variable) %>%
  filter(!is.na(measurement),!is.na(TI)) %>%
  spread(measurement, value )

dataf$max[is.na(dataf$max)] <- dataf$estimate[is.na(dataf$max)] + 0.5
dataf$min[is.na(dataf$min)] <- dataf$estimate[is.na(dataf$min)] - 0.5
newnames <- names(dataf)[!(names(dataf) %in% onames)]
newnames <- newnames[nchar(newnames)>0]
all_data <- dataf %>%
  gather_("measurement","value",newnames) 


all_data <- all_data[,names(all_data[names(all_data)!=""])]


###################################
## Plot costs for all estimates and all technologies


costs <- as.data.frame(all_data) %>%
  filter(variable=="cost" & costsinclude==T) %>%
  mutate(variable=technology)





## Get a dataframe with values to 1000 and the number of studies in that range
## for each tech

techs <- unique(costs$technology)
ranges <- seq(0,1000)
df <- data.frame(v=ranges)


system.time(
costranges <- countranges(
  df, 
  mutate(
    costs,
    value=as.numeric(gsub("[^0-9\\.]", "", value))
  ), 
  techs, "range"))

## Make some nicer labels

rlabs <- costranges %>%
  group_by(resource) %>%
  summarise(
    resourcelab=paste0(gsub(" (terrestrial and ocean)","",first(resource),fixed=T),'\n[',max(maxvalue),' studies]')
  )

names(pics) <- rlabs$resourcelab

## Plot a heatbar with that text label, and pretty pictures of the technologies

heatbar(costranges,"pcnt",text=T) +
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



for (t in techs) {
  tranges <- costranges[costranges$resource==t,]
  tcosts <- costs[costs$technology==t,]
  y1 <- min(tcosts$PY,na.rm=T)
  y2 <- max(tcosts$PY,na.rm=T)
  diff <- y2-y1
  mid <- y1+diff/2
  h1<- heatbar_years(tcosts, tranges, "pcnt", graph = TRUE, y = mid, w = diff,var=t)
  h1[[1]] +  geom_text_repel(data = h1[[2]], 
                             aes(x = PYJ, y = max, label = label, angle = 90) 
  ) + ggtitle(t)
  ggsave(paste0("plots/heatbars/",t,"/costs/range_year_studies.png"))
}

##########################
## All costs with jittered ranges
costsjitter <- costs %>%
  filter(measurement=="max", !is.na(value)) %>%
  group_by(variable) %>%
  mutate(
    nstudies = n(),
    resourcelab = paste0(variable,'\n[',nstudies,' studies]')
  )

costrange <- costs %>%
  filter(measurement %in% c("max","min"), !is.na(value)) %>%
  left_join(select(costsjitter,label, TI, resourcelab,`Data categorisationyear`,`Data categorisationsystem conditions`)) %>%
  spread(measurement, value)

costrange$resourcelabn <- as.numeric(factor(costrange$resourcelab)) #+ runif(length(costrange$resourcelab),-0.4,0.4)

costrange <- costrange %>%
  group_by(technology) %>%
  arrange(min) %>% 
  mutate(
    gtot = n(),
    pn = row_number(),
    jitter = (0.8/gtot)*(pn-1),
    resourcelabn = resourcelabn- 0.4 + jitter
  )



costrange$TIs <- lapply(costrange$TI, splitwords, n=8)

costrange$conditions <- lapply(costrange$`Data categorisationsystem conditions`, splitwords, n=8)

costrange$AUs <- lapply(costrange$AU, fixauthors)

costrange <- costrange %>%
  mutate(
    ttip=paste0(
      AUs," (",PY,") ",
      "<br>",
      TIs,
      "<br><br>",
      "Cost range: ",round(min,2),"-",round(max,2),
      " Gt CO2/year","<br>",
      "<b>System boundaries:</b> ", boundaries, "<br>",
      "<b>System conditions:</b> ", conditions
    ) 
  )



costrange <- costrange %>%
  mutate(
    ttip=paste0(
      AU," (",PY,") ",
      "<br>",
      TI,
      "<br>",

      "$/tCO2","<br>",
      "System boundaries: ", boundaries, "<br>",
      "System conditions: ", `Data categorisationsystem conditions`
    ) 
  )



gg <- heatbar(costranges,"pcnt", numeric=T) +
  geom_errorbar(
    data=costrange,
    aes(resourcelabn ,ymin=min, ymax=max),
    width=0.05,
    alpha=0.3
  ) +
  scale_x_continuous(breaks=seq(1,8)) +
  theme_bw()+
  theme(axis.text.x  = my_axis(pics)) +
  labs(x="",y="Costs in $US(2011)/tCO2") +
  coord_cartesian(expand=F) +
  theme(
    axis.line.x=element_blank(),
    axis.line.y= element_line(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 

print(gg)

ggsave("plots/heatbars/all_costs_years_ranges.png")



gg <- heatbar(costranges,"pcnt", numeric=T) +
  geom_linerange(
    data=costrange,
    aes(resourcelabn ,ymin=min, ymax=max, text=ttip),
    width=1,
    alpha=1
  ) +
  #scale_x_continuous(breaks=seq(1,8)) +
  theme_bw()+
  labs(x="",y="Costs in $US(2011)/tCO2") +
  coord_cartesian(expand=F) +
  theme(
    axis.line.x=element_blank(),
    axis.line.y= element_line(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,8),labels=names(rlabs$resourcelab))

print(gg)

m <- list(
  l = 100,
  r = 50,
  b = 200,
  t = 100,
  pad = 10
)

ggplotly(gg, tooltip="text") %>%
  layout(margin = m)






###################################
## Plot potentials for all estimates and all technologies

pots <- all_data %>%
  ungroup() %>%
  filter(variable=="totalPotential" & potsinclude==T) %>%
  mutate(
    variable=technology
    ) 

techs <- unique(pots$technology)

ranges <- seq(0,100,by=0.1)
df <- data.frame(v=ranges)

pots$measurement <- gsub(" (Gt CO2/yr)","",pots$measurement,fixed=T)

potsranges <- countranges(
  df, 
  mutate(
    pots,
    value=as.numeric(value)
  ),
  techs, "max")




ggplot(pots) +
  geom_jitter(
    aes(year,value,colour=technology,shape=measurement)
  ) + theme_bw() 

ggsave("plots/heatbars/potentials.png")

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


###########################
## Start doing some heatbars for the pots




rlabs <- potsranges %>%
  group_by(resource) %>%
  summarise(
    resourcelab=paste0(gsub(" (terrestrial and ocean)","",first(resource),fixed=T),'\n[',max(maxvalue),' studies]')
  )




image.file <- dir("icons", pattern=".png", full.names=TRUE)

image.file <- image.file[!grepl("DAC",image.file)]

image.file <- image.file[order(as.integer(sub("_.*","",sub("icons/","",image.file))))]
npoints <- length(image.file)
pics  <- vector(mode="list", length=npoints)
for(i in 1:npoints) {
  pics[[i]] <- EBImage::readImage(image.file[i])
}
names(pics) <- sub(".png","",sub("icons/","",image.file))

names(pics) <- rlabs$resourcelab





ggsave("plots/heatbars/all_potentials_labelled.png", width=16,height=10)



for (t in techs) {
  tranges <- potsranges[potsranges$resource==t,]
  tpots <- pots[pots$technology==t,]
  y1 <- min(tpots$PY,na.rm=T)
  y2 <- max(tpots$PY,na.rm=T)
  diff <- y2-y1
  mid <- y1+diff/2
  h1<- heatbar_years(tpots, tranges, "pcnt", graph = TRUE, y = mid, w = diff, var=t, measurement="max",step=0.1)
  h1[[1]] +  geom_text_repel(data = h1[[2]], 
                             aes(x = PYJ, y = max, label = label, angle = 90) 
  ) + ggtitle(t) +
    labs(y="Potentials in Gt CO2/year")
  
  ggsave(paste0("plots/heatbars/",t,"/potentials/range_year_studies.png"))
}



potsjitter <- pots %>%
  filter(measurement=="max", !is.na(value)) %>%
  group_by(variable) %>%
  mutate(
    nstudies = n(),
    resourcelab = paste0(variable,'\n[',nstudies,' studies]')
  ) 

potsjitter$resourcelabn <- as.numeric(factor(potsjitter$resourcelab))

potsjitter <- potsjitter %>%
  group_by(technology) %>%
  arrange(value) %>% 
  mutate(
    gtot = n(),
    pn = row_number(),
    jitter = (0.76/gtot)*(pn-1),
    resourcelabn = resourcelabn- 0.38 + jitter
  )




gg <- heatbar(potsranges,"pcnt", step=0.1, numeric=T) +
  geom_point(
    data=potsjitter,
    aes(resourcelabn ,y=value),
    size=1,
    alpha=0.3
  ) +
  scale_x_continuous(breaks=seq(1,7)) +
  theme_bw()+
  theme(axis.text.x  = my_axis(pics)) +
  labs(x="",y="Potential GtCO2/year Sequestered") +
  coord_cartesian(expand=F) +
  theme(
    axis.line.x=element_blank(),
    axis.line.y= element_line(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 

print(gg)

### Can't do it automatically !!! htmlwidgets::saveWidget(ggp, "plots\\heatbars\\costs\\index.html")

ggsave("plots/heatbars/all_potentials_points.png", width=16,height=10)



potsjitter$TIs <- lapply(potsjitter$TI, splitwords, n=8)

potsjitter$conditions <- lapply(potsjitter$`Data categorisationsystem conditions`, splitwords, n=8)

potsjitter$AUs <- lapply(potsjitter$AU, fixauthors)

potsjitter <- potsjitter %>%
  mutate(
    ttip=paste0(
      AUs," (",PY,") ",
      "<br>",
      TIs,
      "<br><br>",
      "<b>Potential:</b> ",round(value,1),
      " Gt CO2/year","<br>",
      "<b>System boundaries:</b> ", boundaries, "<br>",
      "<b>System conditions:</b> ", conditions
    ) 
  )


gg <- heatbar(potsranges,"pcnt", step=0.1, numeric=T) +
  geom_point(
    data=potsjitter,
    aes(resourcelabn ,y=value, text=ttip),
    size=1,
    alpha=0.3
  ) +
  theme_bw()+
  labs(x="",y="Potential GtCO2/year Sequestered") +
  coord_cartesian(expand=F) +
  theme(
    axis.line.x=element_blank(),
    axis.line.y= element_line(),
    axis.ticks.x = element_blank(),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_x_continuous(breaks=seq(1,7),labels=names(pics))

print(gg)

m <- list(
  l = 100,
  r = 50,
  b = 200,
  t = 100,
  pad = 10
)

ggplotly(gg, tooltip="text") %>%
  layout(margin = m)


######





########################
## 1 point per study

bystudy <- all_data %>%
  filter(variable %in% c("cost","totalPotential"),
         !is.na(value), costsinclude==T, potsinclude==T) %>%
  select(TI, technology, variable, measurement, value, boundaries, year, `Data categorisationsystem conditions`) %>%
  spread(variable, value)

ggplot(bystudy) +
  geom_point(
    aes(totalPotential, cost, colour=technology)
  ) +theme_bw()

