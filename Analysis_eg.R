###################################
###################################
##Coweeta Carbon Network Analysis##
###################################
###################################

#source all req packages and scripts
source("R/global.R")

## Read Example Watershed network in 
net <- readRDS("data/eg_watershed.RDS")

#update igraph object to include calculations
calc_net <- solveMB(net)

#Extract vertices dataframe for easy viewing 
v_df <- as.data.frame(get.vertex.attribute(calc_net))

##Note the "lengthup_m" is from the original GIS calculations, so is determined seperately from this code. 
##Therefore, the SLout and lengthup_m should be ~ the same. 
