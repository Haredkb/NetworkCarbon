###################################
###################################
##Coweeta Carbon Network Analysis##
###################################
###################################
source("R/global.R")
## Read Example Watershed network in 

net <- readRDS("data/eg_watershed.RDS")
calc_net <- solveMB(net)
