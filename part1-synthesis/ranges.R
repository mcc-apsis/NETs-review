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