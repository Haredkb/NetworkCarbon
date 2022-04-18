###################################
###################################
##Coweeta Carbon Network Analysis##
###################################
###################################

#source all req packages and scripts
source("R/global.R")

########################
###### Initialize ######
########################

## Read Example Watershed network in 
net <- readRDS("data/eg_watershed.RDS")

## Read initial network (starting values)
#net <- read in network with starting values

#define time steps and units
timesteps = 14 
ts_units = "day" #hour
s_date = as.POSIXct("10-01-2018 00:00", format = "%m-%d-%Y %H:%M") #define model start date
s_Jday = yday(s_date) #starting Julian Day 
s_month = month(s_date) #starting month

##########################
#### Read Input Data #####
##########################
#read input files (inputs external to the network)
        #Stream Temperature - Created in file MultipleRegressionAnalysis.R
        temp_sin <- readRDS("data/temp_sin.RDS")
        
        #Baseflow Linear Model for Watershed- CWT_NetworkModel_v3.R
        Q_JDate_lm <- readRDS("data/Q_JDate_lm.RDS")  %>%
          dplyr::select(-std.error, -p.value, -statistic)%>%
          pivot_wider(names_from = term, values_from = estimate)%>%
          dplyr::rename(b = "(Intercept)", m = totLength_)
        
        #Baseflow based on nearest landscape point
        Q_JDate_emp <- readRDS("data/Q_JDate_emp.RDS")
        
        #Seep Data by Month
        DOC_seep_table <- readRDS("data/DOC_seep_table.RDS")
        
        #Direct and Lateral C Inputs
        POM_input <- readRDS("data/POM_input.RDS")

##for loop for each time step
#water yield, doc yield, litter inputs, temperature
        dates <- seq(from = as.POSIXct(s_date), to = as.POSIXct(s_date + days(timesteps)), by = "day")
        network <- net2
        
        #set up igraph variables
        # Inflow discharge from local catchment (m3 d-1):
        V(network)$Qlocal <- 0
        # Inflow discharge from upstream reaches (m3 d-1):
        V(network)$Qnet <- 0
        # Outflow discharge from each reach (m3 d-1):
        V(network)$Qout <- NA
        

#day iteration        
rm(network_pre) #remove from global env
net_lst <- lapply(dates, function(ts, env = parent.frame(), inherits = FALSE){
              #for(i in seq_along(dates)){ #working on function for this
                print(ts)
  
                #initalize date details
                #ts <- dates[i]
                day <- yday(ts)
                mon <- month(ts)
                
              ### INITIAL INPUT 
                ### CALCULATE BASE Q
                bq_m3dm <- Q_JDate_lm %>%
                  dplyr::filter(JDate == day) 
                
                ### Add Litter POC (direct and lateral) in
                set.seed(2)
                POM_input_mon <- POM_input %>%
                  dplyr::filter(sample.mo == mon)  
                
                # ### Add Terrestrial Input DOC
                # DOC_seep_table_mon <- DOC_seep_table %>%
                #   filter(mon == month) 
                
                #temperature
                #n$temp <-  n$amp * cos(rad_day(j - n$phase)) + n$ymean
                
                ### CALCULATE GEOMORPHIC PARAMETERS

                network <- netset(network, bq_m3dm)
                   
            #########################################
            ##-----POC STANDING STOCK CALC-------- ##
            #########################################
                #POC IN, sum direct input and lateral - assume direct for full width 
                V(network)$ClocalLit_gd <- abs(rnorm(length(V(network)), mean = POM_input_mon$direct_gm2d_avg, 
                                                          sd = POM_input_mon$direct_gm2d_sd))*V(network)$length_reach * V(network)$width_m+ #direct
                        
                        abs(rnorm(length(V(network)), mean = POM_input_mon$lateral_gmd_avg, 
                                  sd = POM_input_mon$lateral_gmd_sd))*V(network)$length_reach  #lateral  
                
                ### Previous Time Step to determine present standing stock (sStock)
                if(!exists("network_pre")){
                #   #network_pre <- get("network", env.pre())
                  V(network)$POC_sStock_gd <- V(network)$ClocalLit_gd
                  message("No PRE")
                } else({
                  V(network)$POC_sStock_gd <- V(network_pre)$POC_gd + V(network)$ClocalLit_gd
                  message("yes PRE")
                
                
                })

                #POC standing Stock Loss per day
                V(network)$POC_loss_gd <- V(network)$POC_sStock_gd * 0 #placeholder (as positive value)
                
                #remaining standstock end of timestep
                V(network)$POC_gd <- V(network)$POC_sStock_gd - V(network)$POC_loss_gd
                
                ##set up environment for next timestep
                #YES ASSIGNING TO THE GLOBAL IS FROWNED ON _ WILL CHANGE TO DIFFERNT ONE BUT WORKS!!!!! 
                assign("network_pre", network, envir = .GlobalEnv)
                message(Sys.time())
                return(network)
                
        })

#TESTING#
v_df3 <- as.data.frame(get.vertex.attribute(net_lst[[3]]))
v_df15 <- as.data.frame(get.vertex.attribute(net_lst[[15]]))

# ##time step names
# net_lst1 <- lapply(seq_along(dates), function(i) paste(names(net_lst)[[i]], dates[[i]]))
          
          
        #Extract vertices dataframe for easy viewing (if timestep every 10 time steps write a dataframe) 
  v_df <- as.data.frame(get.vertex.attribute(net_lst[[3]]))


##Note the "lengthup_m" is from the original GIS calculations, so is determined seperately from this code. 
##Therefore, the SLout and lengthup_m should be ~ the same. 

# #
# show_env <- solveMB(){
#   list(ran.in = environment(), 
#        parent = parent.env(environment()), 
#        objects = ls.str(environment()))
# }
