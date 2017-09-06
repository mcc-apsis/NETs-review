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
library(plotrix)


source("heatbars/heatbar_functions.R")
source("../../bitbucket/beccs/functions/useful_functions.R")

dir.create(paste0("plots/heatbars/",u_sheetName))

plotdir = paste0("plots/synthesis-part2/",u_sheetName)

dir.create(plotdir)

# Authorise googlesheets to access your Google Sheets account
gs_auth()

white2red  <- colorRampPalette(c("white", "red"))
white2blue <- colorRampPalette(c("white", "blue"))

pot_max  <- 20
cost_max <- 400

pot_n  <- 1000
cost_n <- 1000

pot_steps  <- seq(1,20)
cost_steps <- seq(1,400)


#==== READ IN SPREADSHEET ==========
gs  <- gs_title("NETs Review")
ss  <- gs_read(gs, ws = u_sheetName, verbose=DEBUG)

data <- get_data(ss,2)


#==== READ IN IAM data =============
load("../../bitbucket/beccs/data/dataplotAll.RData")

  
################################################
## Generate a new df of ranges

##### POTENTIALS
# Adjust the maximum here to change the scale
df <- data.frame(v=pot_steps)

# Get a list of resources, or define it yourself
resources <- unique(
  data[data$measurement=="max" & data$variable!="cost",]$variable
)
resources <- resources[!is.na(resources)]

# # max
# pot_max <- countranges(df, 
#                        filter(data, `Data categorisationyear` == 2050 & `Data categorisationsystem boundaries` == "Global"),
#                        resources,
#                        "max"
# )

# range
pot_range <- countranges(df, 
                       filter(data, `Data categorisationyear` == 2050 & `Data categorisationsystem boundaries` == "Global"),
                       resources,
                       "range"
)

###### Costs 
df <- data.frame(v=cost_steps)
costs <- c("cost")

costs <- unique(
  data[data$measurement=="max" & 
         data$variable=="cost",
       ]$variable
)


cost_range <- countranges(df, 
                       filter(data),
                       costs,
                       "range"
)


# # Plot 2D heatmap
# xmin <- 0
# xmax <- max(pot_steps)
# ymin <- 0
# ymax <- max(cost_steps)
# 
# alpha <- "99"
# 
# pot_cols   <- paste0(white2red(length(pot_steps)), alpha)
# cost_cols  <- paste0(white2blue(length(cost_steps)), alpha)
# 
# threshold <- 0.50
# 
# # Pre-computations
# # Potential
# pot_scalecol <- 1
# pot_rects <- data.frame(xmin=pot_steps-1, 
#                         xmax=pot_steps, 
#                         ymin=rep(ymin, length(pot_steps)), 
#                         ymax=rep(ymax, length(pot_steps)), 
#                         colid     = pmin(length(pot_steps), pmax(1, round(pot_range$pcnt*(max(pot_steps)*pot_scalecol)/100, digit=0))))
# pot_rects$colidnorm <- round(pot_rects$colid/max(pot_rects$colid)*max(pot_steps), digits=0)
# pot_rects$fill      <- pot_cols[pot_rects$colidnorm]
# # Cost
# cost_scalecol <- 1
# cost_rects <- data.frame(xmin=rep(xmin, length(cost_steps)),
#                          xmax=rep(xmax, length(cost_steps)),
#                          ymin=cost_steps-1,
#                          ymax=cost_steps,
#                          colid=pmin(length(cost_steps), pmax(1, round(cost_range$pcnt*(max(cost_steps)*cost_scalecol)/100, digit=0))))
# cost_rects$colidnorm <- round(cost_rects$colid/max(cost_rects$colid)*max(cost_steps), digits=0)
# cost_rects$fill      <- cost_cols[cost_rects$colidnorm]
# 
# pot_d <- density(unlist(sapply(1:nrow(pot_range), function(x) rep(pot_range$v[x], pot_range$value[x]))))
# pot_d$y <- pot_d$y/max(pot_d$y)
# 
# cost_d <- density(unlist(sapply(1:nrow(cost_range), function(x) rep(cost_range$v[x], cost_range$value[x]))))
# cost_d$y <- cost_d$y/max(cost_d$y)
# 
# par(mar=c(0,0,0,0),
#     las=1)
# 
# par(plt=c(0.35, 0.95, 0.35, 0.95))
# plot(0,0,
#      type="n",
#      axes=FALSE,
#      xlim=c(xmin,xmax), ylim=c(ymin,ymax),
#      xlab="", ylab="",
#      xaxs="i",yaxs="i")
# 
# # Grid
# for (kx in seq(5,15,5))    lines(c(kx, kx),     c(ymin, ymax), col="#eeeeee")
# for (ky in seq(100,300,100)) lines(c(xmin, xmax), c(ky, ky),     col="#eeeeee")
# 
# # Draw rectangles
# for (kr in 1:nrow(pot_rects)) {
#   rect(pot_rects$xmin[kr],pot_rects$ymin[kr],pot_rects$xmax[kr],pot_rects$ymax[kr],col=pot_rects$fill[kr], border=NA)
# }
# for (kr in 1:nrow(cost_rects)) {
#   rect(cost_rects$xmin[kr],cost_rects$ymin[kr],cost_rects$xmax[kr],cost_rects$ymax[kr],col=cost_rects$fill[kr], border=NA)
# }
# 
# # # Draw ellipse
# # draw.ellipse(pot_d$x[which(pot_d$y == max(pot_d$y))], 
# #              cost_d$x[which(cost_d$y == max(cost_d$y))],
# #              abs(pot_d$x[min(which(pot_d$y> 0.9))] - pot_d$x[max(which(pot_d$y> 0.9))])/2,
# #              abs(cost_d$x[min(which(cost_d$y> 0.9))] - cost_d$x[max(which(cost_d$y> 0.9))])/2,
# #              lwd=3, lty=3
# #              )
# # Draw rectangle
# # rect(
# #   pot_d$x[min(which(pot_d$y> 0.5))], cost_d$x[min(which(cost_d$y> 0.5))],
# #   pot_d$x[max(which(pot_d$y> 0.5))], cost_d$x[max(which(cost_d$y> 0.5))],
# #   lwd=1.5, lty=3 
# # )
# 
# rect(
#   pot_d$x[min(which(pot_d$y> threshold))], cost_d$x[min(which(cost_d$y> threshold))],
#   pot_d$x[max(which(pot_d$y> threshold))], cost_d$x[max(which(cost_d$y> threshold))],
#   lwd=3, lty=3 
# )
# 
# # Bottom-up studies
# # data %>% 
# #   filter(variable %in% c("totalPotential", "cost")) %>% 
# #   unite(var.mes, variable, measurement, sep=".") %>% 
# #   select(-category) 
#   
# 
# # Former review
# rev_data <- data %>% 
#   filter(variable %in% c("totalPotential", "cost")) %>% 
#   unite(var.mes, variable, measurement, sep=".") %>% 
#   select(-category) %>% 
#   filter(`Data categorisationresource` == "Review") %>% 
#   spread(var.mes, value)
# 
# rect(
#   rev_data$totalPotential.min, rev_data$cost.min,
#   rev_data$totalPotential.max, rev_data$cost.max,
#   lwd=3, lty=3, border="#33ff33")
# 
# # IAM data
# iam_data <- v_data_tempTargets_world_plot %>% 
#   filter(variable %in% c("Price|Carbon", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
#   select(model,scenario,tempcat,period,variable,value) %>% 
#   spread(variable, value) %>% 
#   rename(price=`Price|Carbon`) %>% 
#   rename(potential=`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
#   mutate(potential=potential/1000)
# 
# points(iam_data$potential[which(iam_data$period == 2050 & iam_data$potential != 0)], 
#        iam_data$price[which(iam_data$period == 2050 & iam_data$potential != 0)], 
#        pch=21, col="#777777ff", bg="#ffffff00")
# points(iam_data$potential[which(iam_data$period == 2100 & iam_data$potential != 0)], 
#        iam_data$price[which(iam_data$period == 2100 & iam_data$potential != 0)], 
#        pch=19, col="#777777ff", cex=0.5)
# 
# box()
# 
# # Density plot potential
# par(plt=c(0.35, 0.95, 0.15, 0.35), new=TRUE)
# 
# plot(0,0,
#      type="n",
#      axes=FALSE,
#      xlim=c(xmin,xmax), ylim=c(0,1),
#      xlab="Potential [Gt(CO2)]", ylab="",
#      xaxs="i",yaxs="i")
# 
# # Grid
# for (kx in seq(5,15,5))    lines(c(kx, kx),     c(0, 1), col="#eeeeee")
# 
# # Density line and threshold
# rect(pot_d$x[min(which(pot_d$y> threshold))], 0,
#      pot_d$x[max(which(pot_d$y> threshold))], 1,
#      col="#cccccc66", border=NA)
# lines(c(xmin,xmax), c(threshold,threshold), col="black", lty=2)
# lines(pot_d$x, pot_d$y, col="red",lwd=3)
# 
# # Axes
# axis(1, at=seq(5,20,5))
# 
# box()
# 
# # Density plot Cost
# par(plt=c(0.15, 0.35, 0.35, 0.95), new=TRUE)
# 
# plot(0,0,
#      type="n",
#      axes=FALSE,
#      xlim=c(0,1), ylim=c(ymin,ymax),
#      xlab="", ylab="Costs [US$/t(CO2)]",
#      xaxs="i",yaxs="i")
# 
# # Grid
# for (ky in seq(100,300,100)) lines(c(0, 1), c(ky, ky),     col="#eeeeee")
# 
# # Density line and threshold
# rect(0, cost_d$x[min(which(cost_d$y> threshold))], 
#      1, cost_d$x[max(which(cost_d$y> threshold))], 
#      col="#cccccc66", border=NA)
# lines(c(threshold,threshold), c(ymin,ymax), col="black", lty=2)
# lines(cost_d$y, cost_d$x, col="blue",lwd=3)
#      
# # Axes
# axis(2, at=seq(0,400,100))
# 
# box()



#================

svglite::svglite(file=file.path(plotdir, "heatmap_beccs.svg"), width=6.5, height=6.5)

# Plot 2D heatmap
xmin <- 0
xmax <- max(pot_steps)
ymin <- 0
ymax <- max(cost_steps)

alpha <- "99"

pot_cols   <- paste0(white2red(length(pot_steps)), alpha)
cost_cols  <- paste0(white2blue(length(cost_steps)), alpha)

plt_pos <- list(
  "main"  = c(0.30, 0.90, 0.30, 0.90),
  "dpot"  = c(0.30, 0.90, 0.15, 0.30),
  "dcost" = c(0.15, 0.30, 0.30, 0.90),
  "bpot"  = c(0.30, 0.90, 0.90, 0.99),
  "bcost" = c(0.90, 0.99, 0.30, 0.90)
)

threshold <- 0.50

# Pre-computations
# Potential
pot_scalecol <- 1
pot_rects <- data.frame(xmin=pot_steps-1, 
                        xmax=pot_steps, 
                        ymin=rep(ymin, length(pot_steps)), 
                        ymax=rep(ymax, length(pot_steps)), 
                        colid     = pmin(length(pot_steps), pmax(1, round(pot_range$pcnt*(max(pot_steps)*pot_scalecol)/100, digit=0))))
pot_rects$colidnorm <- round(pot_rects$colid/max(pot_rects$colid)*max(pot_steps), digits=0)
pot_rects$fill      <- pot_cols[pot_rects$colidnorm]
# Cost
cost_scalecol <- 1
cost_rects <- data.frame(xmin=rep(xmin, length(cost_steps)),
                         xmax=rep(xmax, length(cost_steps)),
                         ymin=cost_steps-1,
                         ymax=cost_steps,
                         colid=pmin(length(cost_steps), pmax(1, round(cost_range$pcnt*(max(cost_steps)*cost_scalecol)/100, digit=0))))
cost_rects$colidnorm <- round(cost_rects$colid/max(cost_rects$colid)*max(cost_steps), digits=0)
cost_rects$fill      <- cost_cols[cost_rects$colidnorm]

pot_d <- density(unlist(sapply(1:nrow(pot_range), function(x) rep(pot_range$v[x], pot_range$value[x]))))
pot_d$y <- pot_d$y/max(pot_d$y)

cost_d <- density(unlist(sapply(1:nrow(cost_range), function(x) rep(cost_range$v[x], cost_range$value[x]))))
cost_d$y <- cost_d$y/max(cost_d$y)

par(mar=c(0,0,0,0),
    las=1)

par(plt=plt_pos$main)
plot(0,0,
     type="n",
     axes=FALSE,
     xlim=c(xmin,xmax), ylim=c(ymin,ymax),
     xlab="", ylab="",
     xaxs="i",yaxs="i")

# Grid
for (kx in seq(5,15,5))    lines(c(kx, kx),       c(ymin, ymax), col="#eeeeee")
for (ky in seq(100,300,100)) lines(c(xmin, xmax), c(ky, ky),     col="#eeeeee")

# Draw rectangles
for (kr in 1:nrow(pot_rects)) {
  rect(pot_rects$xmin[kr],pot_rects$ymin[kr],pot_rects$xmax[kr],pot_rects$ymax[kr],col=pot_rects$fill[kr], border=NA)
}
for (kr in 1:nrow(cost_rects)) {
  rect(cost_rects$xmin[kr],cost_rects$ymin[kr],cost_rects$xmax[kr],cost_rects$ymax[kr],col=cost_rects$fill[kr], border=NA)
}

# # Draw ellipse
# draw.ellipse(pot_d$x[which(pot_d$y == max(pot_d$y))], 
#              cost_d$x[which(cost_d$y == max(cost_d$y))],
#              abs(pot_d$x[min(which(pot_d$y> 0.9))] - pot_d$x[max(which(pot_d$y> 0.9))])/2,
#              abs(cost_d$x[min(which(cost_d$y> 0.9))] - cost_d$x[max(which(cost_d$y> 0.9))])/2,
#              lwd=3, lty=3
#              )
# Draw rectangle
# rect(
#   pot_d$x[min(which(pot_d$y> 0.5))], cost_d$x[min(which(cost_d$y> 0.5))],
#   pot_d$x[max(which(pot_d$y> 0.5))], cost_d$x[max(which(cost_d$y> 0.5))],
#   lwd=1.5, lty=3 
# )

rect(
  pot_d$x[min(which(pot_d$y> threshold))], cost_d$x[min(which(cost_d$y> threshold))],
  pot_d$x[max(which(pot_d$y> threshold))], cost_d$x[max(which(cost_d$y> threshold))],
  lwd=3, lty=3 
)

# Bottom-up studies
# data %>% 
#   filter(variable %in% c("totalPotential", "cost")) %>% 
#   unite(var.mes, variable, measurement, sep=".") %>% 
#   select(-category) 


# Former review
rev_data <- data %>% 
  filter(variable %in% c("totalPotential", "cost")) %>% 
  unite(var.mes, variable, measurement, sep=".") %>% 
  select(-category) %>% 
  filter(`Data categorisationresource` == "Review") %>% 
  spread(var.mes, value)

rect(
  rev_data$totalPotential.min, rev_data$cost.min,
  rev_data$totalPotential.max, rev_data$cost.max,
  lwd=3, lty=3, border="#33ff33")

# IAM data
iam_data <- v_data_tempTargets_world_plot %>% 
  filter(variable %in% c("Price|Carbon", "Emissions|CO2|Carbon Capture and Storage|Biomass")) %>% 
  select(model,scenario,tempcat,period,variable,value) %>% 
  spread(variable, value) %>% 
  rename(price=`Price|Carbon`) %>% 
  rename(potential=`Emissions|CO2|Carbon Capture and Storage|Biomass`) %>% 
  mutate(potential=potential/1000)

points(iam_data$potential[which(iam_data$period == 2050 & iam_data$potential != 0)], 
       iam_data$price[which(iam_data$period == 2050 & iam_data$potential != 0)], 
       pch=21, col="#777777ff", bg="#ffffff00")
points(iam_data$potential[which(iam_data$period == 2100 & iam_data$potential != 0)], 
       iam_data$price[which(iam_data$period == 2100 & iam_data$potential != 0)], 
       pch=19, col="#777777ff", cex=0.5)

legend(
  
)

box()

# Density plot potential
par(plt=plt_pos$dpot, new=TRUE)

plot(0,0,
     type="n",
     axes=FALSE,
     xlim=c(xmin,xmax), ylim=c(0,1),
     xlab="Potential [Gt(CO2)]", ylab="",
     xaxs="i",yaxs="i")

# Grid
for (kx in seq(5,15,5))    lines(c(kx, kx),     c(0, 1), col="#eeeeee")

# Density line and threshold
rect(pot_d$x[min(which(pot_d$y> threshold))], 0,
     pot_d$x[max(which(pot_d$y> threshold))], 1,
     col="#cccccc66", border=NA)
lines(c(xmin,xmax), c(threshold,threshold), col="#777777", lty=2)
lines(pot_d$x, pot_d$y, col="red",lwd=3)

rect(pot_d$x[min(which(pot_d$y> threshold))]-xmax*0.025, 0.03, 
     pot_d$x[min(which(pot_d$y> threshold))]+xmax*0.025, 0.16,
     col="#ffffff99", border="#99999999")
text(pot_d$x[min(which(pot_d$y> threshold))], 0.1, paste(round(pot_d$x[min(which(pot_d$y> threshold))], digits=0)), cex=0.8, col="#777777")

rect(pot_d$x[max(which(pot_d$y> threshold))]-xmax*0.025, 0.03, 
     pot_d$x[max(which(pot_d$y> threshold))]+xmax*0.025, 0.16,
     col="#ffffff99", border="#99999999")
text(pot_d$x[max(which(pot_d$y> threshold))], 0.1, paste(round(pot_d$x[max(which(pot_d$y> threshold))], digits=0)), cex=0.8, col="#777777")

text(xmax - xmax*0.025, threshold+0.05, paste0(threshold*100, "%"), adj = c(1,0), cex=0.8, col="#777777")

# Axes
axis(1, at=seq(5,20,5))

box()

# Density plot Cost
par(plt=plt_pos$dcost, new=TRUE)

plot(0,0,
     type="n",
     axes=FALSE,
     xlim=c(0,1), ylim=c(ymin,ymax),
     xlab="", ylab="Costs [US$/t(CO2)]",
     xaxs="i",yaxs="i")

# Grid
for (ky in seq(100,300,100)) lines(c(0, 1), c(ky, ky),     col="#eeeeee")

# Density line and threshold
rect(0, cost_d$x[min(which(cost_d$y> threshold))], 
     1, cost_d$x[max(which(cost_d$y> threshold))], 
     col="#cccccc66", border=NA)
lines(c(threshold,threshold), c(ymin,ymax), col="#777777", lty=2)
lines(cost_d$y, cost_d$x, col="blue",lwd=3)

rect(0.05, cost_d$x[min(which(cost_d$y> threshold))]-ymax*0.025, 
     0.25, cost_d$x[min(which(cost_d$y> threshold))]+ymax*0.025, 
     col="#ffffff99", border="#99999999")
text(0.15, cost_d$x[min(which(cost_d$y> threshold))], paste(round(cost_d$x[min(which(cost_d$y> threshold))], digits=0)), cex=0.8, col="#777777")

rect(0.05, cost_d$x[max(which(cost_d$y> threshold))]-ymax*0.025, 
     0.25, cost_d$x[max(which(cost_d$y> threshold))]-ymax*0.025, 
     col="#ffffff99", border="#99999999")
text(0.15, cost_d$x[max(which(cost_d$y> threshold))], paste(round(cost_d$x[max(which(cost_d$y> threshold))], digits=0)), cex=0.8, col="#777777")

text(threshold+0.05, ymax - ymax*0.025, paste0(threshold*100, "%"), adj = c(0,1), cex=0.8, col="#777777")

# Axes
axis(2, at=seq(0,400,100))

box()


# IAM boxplots
par(plt=plt_pos$bpot, new=TRUE)

plot(0,0,
     type="n",
     axes=FALSE,
     xlim=c(xmin,xmax), ylim=c(0,1),
     xlab="", ylab="",
     xaxs="i",yaxs="i")

# Grid
#for (kx in seq(5,15,5))    lines(c(kx, kx),     c(0, 0.75), col="#eeeeee")

# Boxplots potential
# 1.5°C
ypos <- 0.25
offset <- 0.1
iam_data <- v_data_tempTargets_world_plot %>% 
  filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
  select(model,scenario,region,tempcat,period,variable,value) %>% 
  mutate(value = value/1000) %>% 
  compute_stats_tempcat("Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
  filter(period == 2050, tempcat == "1.5°C scenario")
lines(c(iam_data$min, iam_data$max), c(ypos,ypos))
rect(iam_data$q15, ypos-offset, 
     iam_data$q85, ypos+offset, 
     col="#074594ff")
lines(c(iam_data$med, iam_data$med), c(ypos-offset, ypos+offset))
points(c(iam_data$mean, ypos), pch=21, col="#000000", bg="#ffffff")
# Likely 2.0°C
ypos <- 0.5
offset <- 0.1
iam_data <- v_data_tempTargets_world_plot %>% 
  filter(variable == "Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
  select(model,scenario,region,tempcat,period,variable,value) %>% 
  mutate(value = value/1000) %>% 
  compute_stats_tempcat("Emissions|CO2|Carbon Capture and Storage|Biomass") %>% 
  filter(period == 2050, tempcat == "Likely 2.0°C scenario")
lines(c(iam_data$min, iam_data$max), c(ypos,ypos))
rect(iam_data$q15, ypos-offset, 
     iam_data$q85, ypos+offset, 
     col="#3f91c5ff")
lines(c(iam_data$med, iam_data$med), c(ypos-offset, ypos+offset))
points(c(iam_data$mean, ypos), pch=21, col="#000000", bg="#ffffff")

#box()

par(plt=plt_pos$bcost, new=TRUE)

plot(0,0,
     type="n",
     axes=FALSE,
     xlim=c(0,1), ylim=c(ymin,ymax),
     xlab="", ylab="",
     xaxs="i",yaxs="i")

# Grid
#for (ky in seq(100,300,100))    lines(c(0, 0.75), c(kx, kx), col="#eeeeee")


# Boxplots costs
# 1.5°C
xpos <- 0.25
offset <- 0.1
iam_data <- v_data_tempTargets_world_plot %>% 
  filter(variable == "Price|Carbon") %>% 
  select(model,scenario,region,tempcat,period,variable,value) %>% 
  compute_stats_tempcat("Price|Carbon") %>% 
  filter(period == 2050, tempcat == "1.5°C scenario")
lines(c(xpos,xpos), c(iam_data$min, iam_data$max))
rect(xpos-offset, iam_data$q15, 
     xpos+offset, iam_data$q85, 
     col="#074594ff")
lines(c(xpos-offset, xpos+offset), c(iam_data$med, iam_data$med))
#points(c(xpos, iam_data$mean), pch=21, col="#000000", bg="#ffffff")
# Likely 2.0°C
xpos <- 0.5
offset <- 0.1
iam_data <- v_data_tempTargets_world_plot %>% 
  filter(variable == "Price|Carbon") %>% 
  select(model,scenario,region,tempcat,period,variable,value) %>% 
  compute_stats_tempcat("Price|Carbon") %>% 
  filter(period == 2050, tempcat == "Likely 2.0°C scenario")
lines(c(xpos,xpos), c(iam_data$min, iam_data$max))
rect(xpos-offset, iam_data$q15, 
     xpos+offset, iam_data$q85, 
     col="#3f91c5ff")
lines(c(xpos-offset, xpos+offset), c(iam_data$med, iam_data$med))
#points(c(xpos, iam_data$mean), pch=21, col="#000000", bg="#ffffff")

#box()

dev.off()

# potcost_tiles <- list()
# for (kr in 1:nrow(pot_rects)) {
#   for (kc in 1:nrow(cost_rects)) {
#     potcost_tiles[[paste0(kr,"-",kc)]] <- data.frame(
#       x=(pot_rects$xmin[kr]  + pot_rects$xmax[kr])/2,
#       y=(cost_rects$ymin[kc] + cost_rects$ymax[kc])/2,
#       metric1 = 0.5*round(pot_range$pcnt[kr] *(max(pot_steps) *pot_scalecol)/100,  digit=0) + 
#                 0.5*round(cost_range$pcnt[kc]*(max(cost_steps)*cost_scalecol)/100, digit=0),
#       metric2 = round(pot_range$pcnt[kr] *(max(pot_steps) *pot_scalecol)/100,  digit=0) * 
#                 round(cost_range$pcnt[kc]*(max(cost_steps)*cost_scalecol)/100, digit=0),
#       stringsAsFactor=FALSE
#     ) 
#   }
# }
# potcost_tiles <- do.call("rbind", potcost_tiles)
#   
# p <- ggplot(potcost_tiles %>% mutate(metric1=metric1*max(metric1)/100))+
#   geom_tile(aes(x=x, y=y, fill=metric1)) +
#   scale_fill_gradient2(low="white", mid="red", high="red", midpoint=90)
# print(p)
# 
# 
# data %>% 
#   filter(variable %in% c("totalPotential", "cost")) %>% 
#   unite(var.mes, variable, measurement, sep=".") %>% 
#   select(-category) %>% 
#   spread(var.mes, value)
#   
