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
library(grid)
library(gridExtra)
library(plotly)

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


load("data/all_data.RData")

all_data$technology[all_data$technology=="Enhanced weathering (terrestrial and ocean)"] <- "Enhanced weathering"

# Curtail very large ew value to 1000

all_data[all_data$value>1000 & !is.na(all_data$value) & all_data$variable=="cost" & all_data$technology=="Enhanced weathering",]$value = 1000

#### Add in max + min 0.5 around estimate
all_data$TI[is.na(all_data$TI)] <- all_data$CITATION[is.na(all_data$TI)]


dataf <- all_data %>%
  group_by(TI, variable, `Data categorisationresource`, boundaries, year, measurement) %>%
  arrange(value) %>%
  mutate(ind = row_number()) %>%
  filter(ind==1) %>%
  ungroup()

onames <- names(dataf)

dataf <- dataf %>%
  filter(!is.na(measurement) & !is.na(value) & measurement!="",!is.na(TI)) %>%
 #mutate(group_i=row_number()) %>%
  spread(measurement, value ) %>%
  select(-ind)

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
  filter(variable=="cost" & costsinclude==T & !is.na(value)) %>%
  mutate(variable=technology)

costs <- costs[!duplicated(costs[,c("TI","measurement","value")]),]

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

plabs <- potsranges %>%
  group_by(resource) %>%
  summarise(
    resourcelab=paste0(gsub(" (terrestrial and ocean)","",first(resource),fixed=T),'\n[',max(maxvalue),' studies]')
  )

names(pics) <- rlabs$resourcelab

###############################
## Do the same for potentials



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

plabs <- potsranges %>%
  filter(resource!="Storage") %>%
  group_by(resource) %>%
  summarise(
    resourcelab=paste0(gsub(" (terrestrial and ocean)","",first(resource),fixed=T),'\n[',max(maxvalue),' studies]')
  )

#####################################
## Potspics

image.file <- dir("icons", pattern=".png", full.names=TRUE)

image.file <- image.file[!grepl("DACCS",image.file)]

image.file <- image.file[order(as.integer(sub("_.*","",sub("icons/","",image.file))))]
npoints <- length(image.file)
potspics  <- vector(mode="list", length=npoints)
for(i in 1:npoints) {
  potspics[[i]] <- EBImage::readImage(image.file[i])
}
names(potspics) <- sub(".png","",sub("icons/","",image.file))

names(potspics) <- plabs$resourcelab



######################################
## Ranges for BECCS and Storage


## special one for BECCS pots

beccs <- filter(pots, technology=="BECCS") %>%
  mutate(variable=`Data categorisationresource`) %>%
  filter(variable!="?",variable!="Misc") %>%
  mutate(value = value / 0.05506451)



beccs$variable[beccs$variable=="Bioenergy Crops"] <- " Bioenergy \nCrops "
beccs$variable[beccs$variable=="Forestry"] <- " Forestry "
beccs$variable[beccs$variable=="Residues"] <- " Residues "
beccs$variable[beccs$variable=="Waste"] <- " Waste "


resources <- unique(beccs$variable)

ranges <- seq(0,2000,by=2)
df <- data.frame(v=ranges)

beccsranges <- countranges(
  df,
  beccs,
  resources, "max")


beccsjitter <- beccs %>%
  filter(measurement=="max", !is.na(value)) %>%
  group_by(variable) %>%
  mutate(
    nstudies = n(),
    resourcelab = paste0(variable,'\n[',nstudies,' studies]')
  ) 

beccsjitter$resourcelabn <- as.numeric(factor(beccsjitter$resourcelab))

beccsjitter <- beccsjitter %>%
  group_by(variable) %>%
  arrange(value) %>% 
  mutate(
    gtot = n(),
    pn = row_number(),
    jitter = (0.76/gtot)*(pn-1),
    resourcelabn = resourcelabn- 0.38 + jitter
  )

beccslabels <- beccsjitter %>%
  group_by(variable) %>%
  arrange(value) %>%
  mutate(lowest=row_number()) %>%
  arrange(-value) %>%
  mutate(highest=row_number()) %>%
  filter(highest < 2 | lowest < 2)


### Storage 

storage <- filter(pots, technology=="Storage") %>%
  mutate(variable=`Data categorisationresource`) %>%
  filter(variable!="?",variable!="Misc")

resources <- unique(storage$variable)

ranges <- seq(0,10000,by=100)
bigdf <- data.frame(v=ranges)
resources


storageranges <- countranges(
  bigdf,
  storage,
  resources, "max")

storagejitter <- storage %>%
  filter(measurement=="max", !is.na(value)) %>%
  group_by(variable) %>%
  mutate(
    nstudies = n(),
    resourcelab = paste0(variable,'\n[',nstudies,' studies]')
  ) 

storagejitter$resourcelabn <- as.numeric(factor(storagejitter$resourcelab))

storagejitter <- storagejitter %>%
  group_by(variable) %>%
  arrange(value) %>% 
  mutate(
    gtot = n(),
    pn = row_number(),
    jitter = (0.76/gtot)*(pn-1),
    resourcelabn = resourcelabn- 0.38 + jitter
  )

storagelabels <- storagejitter %>%
  group_by(variable) %>%
  arrange(value) %>%
  mutate(lowest=row_number()) %>%
  arrange(-value) %>%
  mutate(highest=row_number()) %>%
  filter(highest < 4 | lowest < 4)



########################################
## Plot sizing parameters


fsize <- 15

lsize <- 4.5

dsize <- 2


##################################################################
########################################
## By technology graphs
top_n <- 4


tech_graphs <- list()
techs <- unique(costs$technology)

for (t in techs) {
  if (t %in% c("Enhanced weathering","Ocean alkalinisation")) {
    top_n <- 3
  } else {
    top_n <- 3
  }
  tranges <- costranges[costranges$resource==t,]
  tcosts <- costs[costs$technology==t,]
  y1 <- min(tcosts$PY,na.rm=T)
  y2 <- max(tcosts$PY,na.rm=T)
  diff <- y2-y1+2
  mid <- y1+diff/2-1
  h1<- heatbar_years(tcosts, tranges, "pcnt", graph = TRUE, y = mid, w = diff,var=t)
  int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 
  labels <- h1[[2]] %>%
    group_by(variable) %>%
    arrange(max) %>%
    mutate(lowest=row_number()) %>%
    arrange(-max) %>%
    mutate(highest=row_number()) %>%
    filter(highest < top_n | lowest < top_n)
  p <- h1[[1]] +  geom_text_repel(data = labels, 
                                  aes(x = PYJ, y = max, label = label, angle = 90) ,
                                  size=lsize,
                                  min.segment.length = unit(0,"lines"),
                                  point.padding = unit(0.1,"lines"),
                                  box.padding = unit(0.5,"lines"),
                                  segment.color = "grey",
                                  segment.alpha = 0.6,
                                  nudge_x = 0.2,
                                  segment.size=1
  ) + ggtitle(paste0(t," - Costs")) +
    scale_x_continuous(breaks= int_breaks) +
    ylab("Cost [US$(2011)/tCO2]") +
    theme(text = element_text(size=fsize))
  print(p)
  if (t %in% c("Ocean alkalinisation","Enhanced weathering")) {
    p <- p + ylim(0,1100)
  }
  if (t=="Ocean alkalinisation") {
    tech_graphs[["Enhanced weathering"]][[4]] <- p
    tech_graphs[["Enhanced weathering"]][[6]] <- t
  } 
  tech_graphs[[t]][[1]] <- p
  tech_graphs[[t]][[3]] <- t
  
  ggsave(paste0("plots/heatbars/",t,"/costs/range_year_studies.png"))
}





techs <- unique(pots$technology)

for (t in techs) {
  tranges <- potsranges[potsranges$resource==t,]
  tpots <- pots[pots$technology==t,]
  
  y1 <- min(tpots$PY,na.rm=T)
  y2 <- max(tpots$PY,na.rm=T)
  diff <- y2-y1+2
  mid <- y1+diff/2-1
  int_breaks <- function(x, n = 5) pretty(x, n)[pretty(x, n) %% 1 == 0] 
  
  h1<- heatbar_years(tpots, tranges, "pcnt", graph = TRUE, y = mid, w = diff, var=t, measurement="max",step=0.1,dsize=dsize)
  
  labels <- h1[[2]] %>%
    group_by(variable) %>%
    arrange(max) %>%
    mutate(lowest=row_number()) %>%
    arrange(-max) %>%
    mutate(highest=row_number()) %>%
    filter(highest < top_n | lowest < top_n)
  p <- h1[[1]] +  geom_text_repel(data = labels, 
                                  aes(x = PYJ, y = max, label = label, angle = 90) ,
                                  size=lsize,
                                  min.segment.length = unit(0,"lines"),
                                  point.padding = unit(0.1,"lines"),
                                  box.padding = unit(0.5,"lines"),
                                  segment.color = "grey",
                                  segment.alpha = 0.6,
                                  nudge_x = 0.2,
                                  segment.size=1
  ) + ggtitle(paste0(t," - Potentials")) +
    labs(y="Sequestration Potential [Gt CO2/year]") +
    scale_x_continuous(breaks= int_breaks) +
    theme(text = element_text(size=fsize))
  
  print(p)
  
  if (t %in% c("Ocean alkalinisation","Enhanced weathering")) {
    p <- p + ylim(0,100)
  }
  
  if (t=="Ocean alkalinisation") {
    tech_graphs[["Enhanced weathering"]][[5]] <- p
  }
  tech_graphs[[t]][[2]] <- p
  tech_graphs[[t]][[3]] <- t
  
  ggsave(paste0("plots/heatbars/",t,"/potentials/range_year_studies.png"))
}

## BECCS graph

p <- heatbar(beccsranges,"pcnt",step=2) +
  geom_point(
    data=filter(beccsjitter,technology!="Storage"),
    aes(resourcelabn ,y=value),
    size=dsize,
    alpha=0.3
  ) +
  labs(x="Resource",y="Bioenergy Potential [EJ/year]") +
  geom_text_repel(data = beccslabels, 
                  aes(x = resourcelabn, y = value, 
                      label = label, angle = 90) ,
                  size=lsize#,
                  #min.segment.length = unit(0,"lines"),
                  #point.padding = unit(0.1,"lines"),
                  #box.padding = unit(0.5,"lines")#,
                  #segment.color = "steelblue",
                  #segment.alpha = 0.7#,
                  #nudge_x = 0.2,
                  #segment.size=1
  ) + ggtitle("Bioenergy Potential") +
  theme(text = element_text(size=fsize))

print(p)

ggsave("plots/heatbars/BECCS/potentials/bioenergy_range_year_studies.png")

tech_graphs[["BECCS"]][[4]] <- p

## special one for Storage pots

storagelabelsx <- storagelabels %>%
  filter(highest < 2 | lowest < 2)

p <- heatbar(storageranges,"pcnt",step=100) +
  geom_point(
    data=storagejitter,
    aes(resourcelabn ,y=value),
    size=dsize,
    alpha=0.3
  ) +
  labs(x="Sink",y="Total Storage Potential [Gt CO2]") +
  geom_text_repel(data = storagelabelsx, 
                  aes(x = resourcelabn, y = value, 
                      label = label, angle = 90) ,
                  size=lsize#,
                  # min.segment.length = unit(0,"lines"),
                  # point.padding = unit(0.1,"lines"),
                  # box.padding = unit(0.5,"lines"),
                  # segment.color = "grey",
                  # segment.alpha = 0.6,
                  # nudge_x = 0.2,
                  # segment.size=1
  ) + ggtitle("CO2 Storage Potential") +
  theme(text = element_text(size=fsize))

print(p)

tech_graphs[["Storage"]][[2]] <- p

tech_graphs[["BECCS"]][[5]] <- p



tech_graphs[["BECCS"]][[1]]


#################### 
## Tech panels
extline = data.frame(
  x=c(2016,2016),
  y=c(1000,1075)
)

p <- tech_graphs["Enhanced weathering"][[1]][[1]] +
  geom_line(
    data=extline,
    aes(x,y),
    linetype=5,
    arrow = arrow(angle = 45, ends = "last", type = "open", length=unit(0.2,"inches"))
  ) +
  geom_text(
    aes(2016,1100,label="3460")
  ) +
  ylim(0,1100)

tech_graphs["Enhanced weathering"][[1]][[1]] <- p

for (t in tech_graphs[!is.null(tech_graphs)]) {
  if (!is.null(t)) {
    print(t[[3]])
    for (ftype in c(".svg","png","pdf")) {
      if (ftype=="pdf") {
        pdf(paste0("plots/heatbars/",t[[3]],"/panel.pdf"),width=14,height=9)
      }
      else if (ftype==".svg") {
        svg(paste0("plots/heatbars/",t[[3]],"/panel.svg"),width=14,height=9)
      } else {
        png(paste0("plots/heatbars/",t[[3]],"/panel.png"),width=800,height=500)
      }
      if (!is.null(t[[2]]) & !is.null(t[[1]])) {
        if (t[[3]]=="Enhanced weathering") {
          int_breaks <- function(x, n = 3) pretty(x, n)[pretty(x, n) %% 1 == 0] 
          grid_arrange_shared_legend(
            t[[1]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Enhanced\nweathering"),
            t[[4]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Ocean\nalkalinisation") +ylab(""),
            t[[2]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Enhanced\nweathering"),
            t[[5]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Ocean\nalkalinisation") +ylab(""), 
            ncol=4
          )
        } else if (t[[3]]=="BECCS"){
          g <- ggplotGrob(t[[1]] + theme(legend.position="bottom"))$grobs
          legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
          lay <- rbind(c(1,2),c(3,3),c(4,4))
          lwidth <- sum(legend$width)
          grid.arrange(
            t[[1]]+ theme(legend.position="none"),
            t[[4]]+ theme(legend.position="none"),
            t[[5]]+ theme(legend.position="none"),
            legend,
            layout_matrix=lay,
            heights= unit.c(unit(0.45, "npc"),unit(0.45, "npc"), unit(0.1, "npc"))
          )
        } else {
          grid_arrange_shared_legend(t[[1]],t[[2]],ncol=2)
        }
      } else if (!is.null(t[[1]])) {
        #grid.arrange(t[[1]],ncol=1)
        print(t[[1]])
      } else if (!is.null(t[[2]])) {
        print(t[[2]])
      }
      dev.off()
      if (!is.null(t[[3]])) {
        if (t[[3]]=="Enhanced weathering") {
          png(paste0("plots/heatbars/",t[[3]],"/panel_alt.png"),width=800,height=500)
          grid_arrange_shared_legend(t[[1]],t[[2]],t[[4]],t[[5]],ncol=2,nrow=2)
          dev.off()
        }
      }  
    }
  }
}

for (t in tech_graphs[!is.null(tech_graphs)]) {
  if (!is.null(t)) {
    print(t[[3]])
    for (ftype in c(".svg","png","pdf")) {
      if (ftype=="pdf") {
        pdf(paste0("plots/heatbars/panels/",t[[3]],".pdf"),width=14,height=9)
      } else if (ftype==".svg") {
        svg(paste0("plots/heatbars/panels/",t[[3]],".svg"),width=14,height=9)
      } else {
        png(paste0("plots/heatbars/panels/",t[[3]],".png"),width=800,height=500)
      }
      if (!is.null(t[[2]]) & !is.null(t[[1]])) {
        if (t[[3]]=="Enhanced weathering") {
          int_breaks <- function(x, n = 3) pretty(x, n)[pretty(x, n) %% 1 == 0] 
          grid_arrange_shared_legend(
            t[[1]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Enhanced\nweathering"),
            t[[4]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Ocean\nalkalinisation") +ylab(""),
            t[[2]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Enhanced\nweathering"),
            t[[5]] + scale_x_continuous(breaks= int_breaks) + ggtitle("Ocean\nalkalinisation") +ylab(""), 
            ncol=4
          )
        } else if (t[[3]]=="BECCS"){
          g <- ggplotGrob(t[[1]] + theme(legend.position="bottom"))$grobs
          legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
          lay <- rbind(c(1,2),c(3,3),c(4,4))
          lwidth <- sum(legend$width)
          grid.arrange(
            t[[1]]+ theme(legend.position="none"),
            t[[4]]+ theme(legend.position="none"),
            t[[5]]+ theme(legend.position="none"),
            legend,
            layout_matrix=lay,
            heights= unit.c(unit(0.45, "npc"),unit(0.45, "npc"), unit(0.1, "npc"))
          )
        } else {
          grid_arrange_shared_legend(t[[1]],t[[2]],ncol=2)
        }
      } else if (!is.null(t[[1]])) {
        #grid.arrange(t[[1]],ncol=1)
        print(t[[1]])
      } else if (!is.null(t[[2]])) {
        print(t[[2]])
      }
      dev.off()
      if (!is.null(t[[3]])) {
        if (t[[3]]=="Enhanced weathering") {
          png(paste0("plots/heatbars/",t[[3]],"/panel_alt.png"),width=800,height=500)
          grid_arrange_shared_legend(t[[1]],t[[2]],t[[4]],t[[5]],ncol=2,nrow=2)
          dev.off()
        }
      }  
    }
  }
}


vars = c("costs","potentials")

for (cp in seq(1,2)) {
  gs <- nth_el(tech_graphs,cp)
  gs <- gs[!unlist(lapply(gs, is.null))]
  png(paste0("plots/heatbars/",vars[cp],"/panel.png"),width=1200,height=1600)
  do.call("grid_arrange_shared_legend", c(gs, ncol=2,nrow=4))
  dev.off()
}

tech_graphs[3]

##########################
## Other graphs

#########################
## Interactive!

## Data prep

ranges <- seq(0,100,by=0.1)
df <- data.frame(v=ranges)

pots$measurement <- gsub(" (Gt CO2/yr)","",pots$measurement,fixed=T)

pots <- pots %>% group_by(
  TI,technology,measurement
) %>% filter(
  value == max(value)
) %>% 
  ungroup()

potsranges <- countranges(
  df, 
  mutate(
    pots,
    value=as.numeric(value)
  ),
  techs, "max")



costs <- costs %>% group_by(
  TI,technology,measurement
) %>% filter(
  value == max(value)
) %>% 
  ungroup()


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

costrange <- unique(costrange) %>%
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
      trimws(AUs)," (",PY,") ",
      "<br>",
      trimws(TIs),
      "<br><br>",
      "Cost range: ",round(min,2),"-",round(max,2),
      " $/tCO2","<br>",
      "<b>System boundaries:</b> ", trimws(boundaries), "<br>",
      "<b>System conditions:</b> ", trimws(conditions)
    ) 
  )



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



potsjitter$TIs <- trimws(lapply(potsjitter$TI, splitwords, n=8))

potsjitter$conditions <- trimws(lapply(potsjitter$`Data categorisationsystem conditions`, splitwords, n=8))

potsjitter$AUs <- trimws(lapply(potsjitter$AU, fixauthors))

potsjitter <- potsjitter %>%
  mutate(
    ttip=paste0(
      AUs," (",PY,") ",
      "<br>",
      TIs,
      "<br><br>",
      "<b>Potential:</b> ",round(value,1),
      " Gt CO2/year","<br>",
      "<b>System boundaries:</b> ", trimws(boundaries), "<br>",
      "<b>System conditions:</b> ", conditions
    ) 
  )

jittertheme <- theme(
  axis.line.x=element_blank(),
  axis.line.y= element_line(),
  axis.ticks.x = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank()
)

m <- list(
  l = 100,
  r = 100,
  b = 75,
  t = 75,
  pad = 0
)

## Costs

pw <- 1100

rlabs <- rlabs %>%
  separate(resourcelab, into=c("resourcecopy","lab"),sep="\n",remove=F) %>%
  mutate(
    resourcebreak = sub(" ","\n",resource),
    resourcelabbreak = paste0(resourcebreak,"\n",lab)
  )

rlabs$resourcelabbreak <- gsub(
  "Soil\nCarbon Sequestration",
  "Soil Carbon\nSequestration",
  rlabs$resourcelabbreak
)

plabs <- plabs %>%
  separate(resourcelab, into=c("resourcecopy","lab"),sep="\n",remove=F) %>%
  mutate(
    resourcebreak = sub(" ","\n",resource),
    resourcelabbreak = paste0(resourcebreak,"\n",lab)
  )

gg <- heatbar(costranges,"pcnt", numeric=T) +
  geom_linerange(
    data=costrange,
    aes(resourcelabn ,ymin=min, ymax=max, text=ttip),
    width=1,
    alpha=1
  ) +
  theme_bw()+
  labs(x="",y="Costs in $US(2011)/tCO2") +
  coord_cartesian(expand=F) +
  jittertheme +
  scale_x_continuous(breaks=seq(1,8),labels=rlabs$resourcelabbreak)

print(gg)

p <- ggplotly(gg, tooltip="text",autosize = F, width = pw, height = 600) %>%
#p <- ggplotly(gg, tooltip="text") %>%
  layout(margin = m) %>%
  layout(plot_bgcolor='transparent') %>%
  layout(paper_bgcolor='transparent')

p

save(p,file="plots/heatbars/costs/pl.RData")

library(widgetframe)

path <- getwd()

htmlwidgets::saveWidget(frameableWidget(p),paste0(path,'/plots/heatbars/costs/index.html'))

tx  <- readLines("plots/heatbars/costs/index.html")
tx  <- gsub(pattern = '"padding":40', replace = '"padding":0', x = tx, fixed=T)
tx  <- gsub(pattern = '"background-color:white;"', replace = '"background-color:none;"', x = tx, fixed=T)
writeLines(tx, con="plots/heatbars/costs/index.html")



## Potentials

plabs$resourcelabbreak <- gsub(
  "Soil\nCarbon Sequestration",
  "Soil Carbon\nSequestration",
  plabs$resourcelabbreak
  )

gg <- heatbar(filter(potsranges,resource!="Storage"),"pcnt", step=0.1, numeric=T) +
  geom_point(
    data=filter(potsjitter,technology!="Storage"),
    aes(resourcelabn ,y=value, text=ttip),
    size=1,
    alpha=0.3
  ) +
  theme_bw()+
  labs(x="",y="Potential GtCO2/year Sequestered") +
  coord_cartesian(expand=F) +
  jittertheme +
  scale_x_continuous(breaks=seq(1,7),labels=plabs$resourcelabbreak[plabs$resource!="DACCS"])

print(gg)

p <- ggplotly(gg, tooltip="text",autosize = F, width = pw, height = 600) %>%
#p <- ggplotly(gg, tooltip="text") %>%  
  layout(margin = m) %>%
  layout(plot_bgcolor='transparent') %>%
  layout(paper_bgcolor='transparent')

p

of <- all_data %>%
  filter(
    technology=="Ocean fertilization"
  )

save(p,file="plots/heatbars/potentials/pl.RData")

knitr::knit("plots/heatbars/costs/costs.Rmd")

knitr::knit("plots/heatbars/potentials/potentials.Rmd")



### Save this, then do this

tx  <- readLines("plots/heatbars/potentials/index.html")
tx  <- gsub(pattern = '"padding":40', replace = '"padding":0', x = tx, fixed=T)
tx  <- gsub(pattern = '"background-color:white;"', replace = '"background-color:none;"', x = tx, fixed=T)
writeLines(tx, con="plots/heatbars/potentials/index.html")

tx  <- readLines("index_alt.html")
tx  <- gsub(pattern = '"padding":40', replace = '"padding":0', x = tx, fixed=T)
tx  <- gsub(pattern = '"background-color:white;"', replace = '"background-color:none;"', x = tx, fixed=T)
writeLines(tx, con="index_alt.html")


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
  jittertheme
  
  print(gg)

ggsave("plots/heatbars/costs/all_costs_ranges.png")



gg <- heatbar(filter(potsranges,resource!="Storage"),"pcnt", step=0.1, numeric=T) +
  geom_point(
    data=filter(potsjitter,technology!="Storage"),
    aes(resourcelabn ,y=value),
    size=1,
    alpha=0.3
  ) +
  scale_x_continuous(breaks=seq(1,7)) +
  theme_bw()+
  theme(axis.text.x  = my_axis(potspics)) +
  labs(x="",y="Potential GtCO2/year Sequestered") +
  coord_cartesian(expand=F) +
  jittertheme

print(gg)


ggsave("plots/heatbars/potentials/all_potentials_points.png", width=16,height=10)















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






ggsave("plots/heatbars/all_potentials_labelled.png", width=16,height=10)











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

