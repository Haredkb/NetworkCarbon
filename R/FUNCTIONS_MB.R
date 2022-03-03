## Functions used in the Network-MB analysis script
## Created based on structure created by LE Koenig 2019
## Danielle Hare UConn
## Last Updated Feb 2022

## add in details here 


## ---------- Functions used in the mass balance model --------- ##

  # Function to implement the C mass-balance model:
    solveMB <- function(network){
      ##debug
      #network <- net#net_BF
      # Define input parameters:
      
      #Stream Length:
      V(network)$SLlocal <- 0
      #Sum of upstream reaches (m):
      V(network)$SLin <- 0
      # Total Contributing stream reach to downgradient (m):
      V(network)$SLout <- NA
      
      
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
        # #no edges contributing to initial node - should these be 0 or 10? 
        length_reach <- ifelse(length_reach == 0, 10, length_reach) # locations within any edges in are springs and thus have 10 meter contributing
        # 
        
        #add reach length to node attrbute
        V(network)$length_reach[i] <- length_reach
        
        # Find neighboring reaches upstream that flow in to the reach:
        up <- igraph::neighbors(network, i,mode=c("in")) #only single up
        up.all.nodes <- head(unlist(ego(network,order=length(V(network)),nodes=V(network)[i],mode=c("in"),mindist=0)),-1)
        
    # Define hydrologic inflows/outflows for each reach (m3 d-1)
        
        #Determine stream length within reach
        V(network)$SLlocal[i] <-  length_reach #* V(network)$baseSL_m3dm[i] #baseflow per meter stream length[i]#V(network)$SL_lat_m3d[i] #V(network)$length_m[i] previous
        
        # Determine inflow from contributing (up) reaches (m3 d-1):
        if(length(up)>0){
          V(network)$SLin[i] <- sum(V(network)$SLout[up]) #only one or junction will have two
        }  
        
        # Discharge outflow to downstream reach (m3 d-1):
        V(network)$SLout[i] <- sum(V(network)$SLlocal[i], V(network)$SLin[i], na.rm = T)
        
      # #Calculate stream reach attributes
        # V(network)$width_m[i] <- a * (V(network)Qout[i]/86400)^b #already in m3/d, needs to be in m3/s
        # V(network)$depth_m[i] <- c * (V(network)$Qout[i]/86400)^d 
        # V(network)$runoff_mday[i] = V(network)$Qout[i]/V(network)$CatchmentA[i] #need to check q_ma versus q0001e
        # 
        # # Calculate reach hydraulic load (m d-1): double check day versus second
        # V(network)$HL[i] <- V(network)$Qout[i]/(V(network)$width_m[i]*length_reach)
      
      }
      # Get list with attributes
      out <- get.vertex.attribute(network)
      
      # Export network:
      return(out)
      
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