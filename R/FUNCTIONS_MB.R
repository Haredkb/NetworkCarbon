## Functions used in the Network-MB analysis script
## Created based on structure created by LE Koenig 2019
## Danielle Hare UConn
## Last Updated Feb 2022

## add in details here 

#########################
## Create Network Direction 
#########################

netset <- function(network, bq_m3dm){
  #Watershed Parameters Wolheim 2006
  a=7.3 #NC, Heton 2011 
  b=0.45 #NC, Helton 2011
  c=0.408 
  d=0.294
  
  for(i in 1:length(V(network))){
      # Calculate mass-balance for each reach moving down the network from headwaters to mouth:
        n_ID <- V(network)$name[i] #node ID being run
        e_ID <- incident(network, n_ID, mode = c("in")) #get edges so the data is shared - this should replace the mutate to nodes. 
        #e_ID$SEGMENT_ID
        length_reach <- if(length(e_ID)>0){
          sum(e_ID$length_m)} else(length_reach = 10)
        #ifelse(length_reach == 0, 10, length_reach) # locations within any edges in are springs and thus have 10 meter contributing
        V(network)$length_reach[i] <- length_reach
        
        # Find neighboring reaches upstream that flow in to the reach:
        up <- igraph::neighbors(network, i,mode=c("in")) #only single up
        up.all.nodes <- head(unlist(ego(network,order=length(V(network)),nodes=V(network)[i],mode=c("in"),mindist=0)),-1)
        
        # Define hydrologic inflows/outflows for each reach (m3 d-1)
        # Discharge inflow from local catchment (m3 d-1):
        #V(network)$Qlocal[i] <- V(network)$runoff_mday[i] * (V(network)$areasqkm[i]*10^6)
        #filter based on basin
        bq <- bq_m3dm %>%
          dplyr::filter(Basin_ID == V(network)$basin_id[i])
        
        V(network)$Qlocal[i] <-  length_reach * bq$m + bq$b #V(network)$baseQ_m3dm[i] #baseflow per meter stream length[i]#V(network)$Q_lat_m3d[i] #V(network)$length_m[i] previous
        V(network)$Qlocal[i] <- V(network)$Qlocal[i]/24 #correct for hours timestep
        
        
        # Discharge inflow from upstream network (m3 d-1):
        if(length(up)>0){
          V(network)$Qnet[i] <- sum(V(network)$Qout[up]) #only one or junction will have two
        }  
        
        #Discharge outflow to downstream reach (m3 d-1):
        V(network)$Qout[i] <- sum(V(network)$Qlocal[i], V(network)$Qnet[i], na.rm = T)
        V(network)$width_m[i] <- a * (V(network)$Qout[i]/86400)^b #already in m3/d, needs to be in m3/s
        V(network)$depth_m[i] <- c * (V(network)$Qout[i]/86400)^d 
        V(network)$Bedarea_m2[i] <- V(network)$width_m[i] * length_reach
        V(network)$CSarea_m2[i] <- V(network)$width_m[i] * V(network)$depth_m[i]
        V(network)$avg_v[i] <- V(network)$Qout[i] / V(network)$CSarea_m2[i]
        #V(network)$runoff_mday[i] = V(network)$Qout[i]/V(network)$CatchmentA[i] #need to check q_ma versus q0001e
}
  return(network)
}

## ---------- Functions used in the mass balance model --------- ##

  # Function to implement the C mass-balance model:
    solveMB <- function(network, POM_input){
      ##debug
      #network <- net#net_BF
      # Define input parameters:
      
      #Stream Length:
      V(network)$SLlocal <- 0
      #Sum of upstream reaches (m):
      V(network)$SLin <- 0
      # Total Contributing stream reach to downgradient (m):
      V(network)$SLout <- NA
      #POC standing stock
      V(network)$POClocal <- 0
      
      
    ################################################
    ################################################
    # Define hydrologic inflows/outflows for each reach (m3 d-1)
    for(i in 1:length(V(network))){
      
      #Watershed Parameters 
      a=7.3 #NC, Heton 2011 
      b=0.45 #NC, Helton 2011
      c=0.408 #Wolheim 2006
      d=0.294 #Wolheim 2006
      
      # Calculate mass-balance for each reach moving down the network from headwaters to mouth:
      for(i in 1:length(V(network))){
        
        #node ids
        n_ID <- V(network)$name[i] #node ID being run
        #attribute edge with node
        e_ID <- incident(network, n_ID, mode = c("in")) #get edges so the data is shared - this should replace the mutate to nodes. 

        #length attrbuted to reach
        length_reach <- sum(e_ID$length_m)
        # #no edges contributing to initial node - should these be 0 or 10? These are only summing incident, so all total length upstream will be accounted for via the total calculation moving downgradient
        length_reach <- ifelse(length_reach == 0, 10, length_reach) # locations within any edges in are springs and thus have 10 meter contributing
        # 
        
        #add reach length to node attribute
        V(network)$length_reach[i] <- length_reach
        
        # Find neighboring reaches upstream that flow in to the reach:
        up <- igraph::neighbors(network, i,mode=c("in")) #only single up
        up.all.nodes <- head(unlist(ego(network,order=length(V(network)),nodes=V(network)[i],mode=c("in"),mindist=0)),-1)#all nodes that are upgradient
        
    # Define hydrologic inflows/outflows for each reach (m3 d-1)
        
        #Determine stream length within reach
        #node ids
      n_ID <- V(network)$name[i] #node ID being run
      #attribute edge with node
      e_ID <- incident(network, n_ID, mode = c("in")) #get edges so the data is shared - this should replace the mutate to nodes. 
      
      #length attrbuted to reach
      length_reach <- sum(e_ID$length_m)
      # #no edges contributing to initial node - should these be 0 or 10? These are only summing incident, so all total length upstream will be accounted for via the total calculation moving downgradient
      length_reach <- ifelse(length_reach == 0, 10, length_reach) # locations within any edges in are springs and thus have 10 meter contributing
      # 
      
      #add reach length to node attrbute
      V(network)$length_reach[i] <- length_reach
      
      # Find neighboring reaches upstream that flow in to the reach:
      up <- igraph::neighbors(network, i,mode=c("in")) #only single up (will be mostly 2)
      #put as a vertice attribute, want a string within a single column
      V(network)$upV[i] <- up
      
      up.all.nodes <- head(unlist(ego(network,order=length(V(network)),nodes=V(network)[i],mode=c("in"),mindist=0)))#all nodes that are upgradient
      #V(network)$upAllV[i] <- c(up.all.nodes)  
      
      #Determine stream length within reach
        V(network)$SLlocal[i] <-  length_reach #* V(network)$baseSL_m3dm[i] #baseflow per meter stream length[i]#V(network)$SL_lat_m3d[i] #V(network)$length_m[i] previous
        
        # # Determine inflow from contributing (up) reaches (m3 d-1):
        # if(length(up)>0){
        #   V(network)$SLin[i] <- sum(V(network)$SLout[up]) #only one or junction will have two
        # }  
        # 
        # # Discharge outflow to downstream reach (m3 d-1):
        # V(network)$SLout[i] <- sum(V(network)$SLlocal[i], V(network)$SLin[i], na.rm = T)
        
      # #Calculate stream reach attributes
        # V(network)$width_m[i] <- a * (V(network)Qout[i]/86400)^b #already in m3/d, needs to be in m3/s
        # V(network)$depth_m[i] <- c * (V(network)$Qout[i]/86400)^d 
        # V(network)$runoff_mday[i] = V(network)$Qout[i]/V(network)$CatchmentA[i] #need to check q_ma versus q0001e
        # 
        # # Calculate reach hydraulic load (m d-1): double check day versus second
        # V(network)$HL[i] <- V(network)$Qout[i]/(V(network)$width_m[i]*length_reach)
      
      }
      # Get list with attributes
      out <- network   #get.vertex.attribute(network)
      
      # Export network:
      return(out)
      
    }
}
    
    ## ---------- Functions for interfacing with NHD network structures --------- ##
    
    # Function to find the X,Y coordinates of the center point of each NHD flowline:
    get_coords_reach_mid <- function(x){
      # Project to albers equal area and get coordinates for the center point along the flowline:
      fline <- x %>% st_transform(5070)
      mid <- suppressWarnings(st_centroid(fline) %>% st_transform(4269))   
      X <- st_coordinates(mid)[,1]
      Y <- st_coordinates(mid)[,2]
      out <- data.frame(X=X,Y=Y)
      return(out)
    }
    
    
    ## ----------- Calcuate radian date from julian day------ ##
    rad_day <- function(x){
      rad_day1 <- 2*pi*x/365
      return(rad_day1)
    }
    
    ## ------------ Calculate time steps from starting day ---## 
    # timesteps_calc <- function(start_date, timesteps, ts_units = "day"){
    #   if(ts_units = "day"){
    #     x <- seq(from = as.POSIXct(start_date), to = as.POSIXct(start_date + days(timesteps)), by = "day")
    #   }else if (ts_units = "hour" ){
    #     x <- seq(from = as.POSIXct(start_date), to = as.POSIXct(start_date + hours(timesteps)), by = "hour")
    #   }else{
    #     x <- print("Choose a valid time step")
    #   }
    #   return(x)
    # }
    
    
    ####-- env time steps
    
    previous_ts <- function(network) {
      p_network <- NULL
      function() {
        p_network <<- network
      }
      p_network
    }

        