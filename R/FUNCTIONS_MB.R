## Functions used in the Network-MB analysis script
## Created based on structure created by LE Koenig 2019
## Danielle Hare UConn
## Last Updated June 2022

## add in details here 

#########################
## Create Network Direction 
#########################




#add up stream node values to a network 
up_nodes <- function(network){
  up_list <- lapply(1:length(V(network)),function(i){
    #all values contributing to point
    up <- igraph::neighbors(network, i,  mode=c("in"))
    e_ID <- incident(network, i, mode = c("in"))
    
    return(up$name)
    
  })
  #
  up_vec <- sapply(up_list, function(s) if (length(s) == 0) NA_character_ else paste(s, collapse = ","))
  V(network)$up <- up_vec
  
  return(network)
  
}




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
        
        #all values contributing to point
        up <- igraph::neighbors(network, i,  mode=c("in")) #only single up (will be mostly 2)
        ##put as a vertice attribute, want a string within a single column
        #V(network)$upV[i] <- c(up)
        #up.all.nodes <- head(unlist(ego(network,order=length(V(network)),nodes=V(network)[i],mode=c("in"),mindist=0)),-1)
        
        #e_ID$SEGMENT_ID
        length_reach <- if(length(e_ID)>0){
          sum(e_ID$length_m)} else(length_reach = 10)
        #ifelse(length_reach == 0, 10, length_reach) # locations within any edges in are springs and thus have 10 meter contributing
        V(network)$length_reach[i] <- length_reach

        # Define hydrologic inflows/outflows for each reach (m3 d-1)
        # Discharge inflow from local catchment (m3 d-1):
        #V(network)$Qlocal[i] <- V(network)$runoff_mday[i] * (V(network)$areasqkm[i]*10^6)
        # #filter based on basin
        V(network)$basin_id[i] <- if_else(V(network)$basin_id[i] == "CoweetaCreek", "ShopeFork", V(network)$basin_id[i]) #right now putting shope projection on the base as that incompasses most of the base
        # 
        bq <- bq_m3hrm %>%
          dplyr::filter(basin_id == V(network)$basin_id[i]) %>% #average Baseflow m3/hr
          mutate(m = estimate)
                 #b = b
        
        V(network)$Qlocal[i] <-  length_reach * bq$m #+ bq$b 
        
        
        # Discharge inflow from upstream network (m3 hr-1):
        if(length(up)>0){
          V(network)$Qup[i] <- sum(V(network)$Qout[up]) #only one or junction will have two
        }  
        
        #Discharge outflow to downstream reach (m3 hr-1):
        V(network)$Qout[i] <- if_else(V(network)$Qlocal[i]> 0, sum(V(network)$Qlocal[i], V(network)$Qup[i], na.rm = T), V(network)$Qup[i])
        V(network)$width_m[i] <- if_else(V(network)$Qout[i]> 0, a * (V(network)$Qout[i]/3600)^b, 0) #already in m3/hr, needs to be in m3/s
        V(network)$depth_m[i] <- if_else(V(network)$Qout[i]> 0, c * (V(network)$Qout[i]/3600)^d, 0)
        V(network)$Bedarea_m2[i] <- V(network)$width_m[i] * length_reach
        V(network)$CSarea_m2[i] <- V(network)$width_m[i] * V(network)$depth_m[i]
        V(network)$avg_v[i] <- if_else(V(network)$Qout[i]> 0, V(network)$Qout[i] / V(network)$CSarea_m2[i], 0)
        #V(network)$runoff_mday[i] = V(network)$Qout[i]/V(network)$CatchmentA[i] #need to check q_ma versus q0001e
}
  return(network)
}

## ---------- Functions used in the mass balance model --------- ##
move_OC <- function(network, network_pre){
  V(network)$FTOC_up <- 0
  V(network)$DOC_up <- 0
  V(network)$FTOC_out <- 0
  V(network)$DOC_out <- 0
  
  #move through nodes - this network is structured so nodes move from headwaters to outlet, thus has to be a loop (order matters)
  for(i in 1:length(V(network))){
    
    # Calculate mass-balance for each reach moving down the network from headwaters to mouth:
    n_ID <- V(network)$name[i] #node ID being run
    e_ID <- incident(network, n_ID, mode = c("in")) #get edges so the data is shared - this should replace the mutate to nodes. 
    #all values contributing to point
    up <- igraph::neighbors(network, i,  mode=c("in"))
    
    # Discharge inflow from upstream network (m3 hr-1):
    ##NOTE FPOC is in ADFM and DOC is in gm3
    if(length(up)>0){
      V(network)$DOC_up[i] <- sum(V(network_pre)$DOC_out[up]) #sum the DOC_out from the previous timestep - amount that has come to the reach
      V(network)$DOC_out[i] <- V(network)$DOC_up[i] + V(network)$DOC_local[i] #add DOC from upstream from previous timestep to the DOC local from the current timestep
      V(network)$FTOC_up[i] <- sum(V(network_pre)$FTOC_out[up])
      V(network)$FTOC_out[i] <- V(network)$FTOC_up[i] + V(network)$FTOC_local[i]
    }  
    
    #origin sites - no contributing streams
    if(length(up) == 0){
      V(network)$DOC_out[i] <- V(network)$DOC_local[i]
      V(network)$FTOC_out[i] <- V(network)$FTOC_local[i]
    }
  }
  
  return(network)
}


### previous time step
previous_ts <- function(network) {
      p_network <- NULL
      function() {
        p_network <<- network
      }
      p_network
    }

## ----------- Calcuate radian date from julian day------ ##
rad_day <- function(x){
  rad_day1 <- 2*pi*x/365
  return(rad_day1)
}