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




## Read initial network (starting values)
#net <- read in network with starting values

##for loop for each time step
  #read input files (inputs external to the network)
    #water yield, doc yield, litter inputs, temperature
  
  #solve network function (should add list of input files to pass to function)
  net <- solveMB(net)

  #Extract vertices dataframe for easy viewing (if timestep every 10 time steps write a dataframe) 
  v_df <- as.data.frame(get.vertex.attribute(calc_net))


##Note the "lengthup_m" is from the original GIS calculations, so is determined seperately from this code. 
##Therefore, the SLout and lengthup_m should be ~ the same. 
