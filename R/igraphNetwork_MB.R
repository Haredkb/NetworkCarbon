## Functions used in the Network-MB analysis script
## Created based on structure created by LE Koenig 2019
## Danielle Hare UConn
## Last Updated Feb 2022

## add in details here 


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

## ---------- Functions used in the mass balance model --------- ##

  # Function to implement the C mass-balance model:
    solveMB_C <- function(network){
      ##debug
      network <- net#net_BF
      # Define input parameters:
      
      # Inflow discharge from local catchment (m3 d-1):
      V(network)$Qlocal <- 0
      # Inflow discharge from upstream reaches (m3 d-1):
      V(network)$Qnet <- 0
      # Outflow discharge from each reach (m3 d-1):
      V(network)$Qout <- NA
      # C load from upstream catchment (g d-1):
      V(network)$ClocalGW <- 0
      # C load from upstream reaches (g d-1):
      V(network)$Cnet <- 0
      # C GROSS input load from upstream reaches (g d-1):
      V(network)$Cnet_gross <- 0
      # C GROSS exported load from upstream reaches (g d-1):
      V(network)$Cnet_gross <- 0
      # Exported C load from each reach (g d-1):
      V(network)$Cout <- 0
      # Uptake/decay rate (m day-1) - change vf within analysis file:
      #V(network)$vf <- 0
      # Fraction of C load lost during transport within each reach (unitless):
      V(network)$Clost <- NA
      
      
      #Watershed Parameters Wolheim 2006
      a=7.3 #NC, Heton 2011 
      b=0.45 #NC, Helton 2011
      c=0.408 
      d=0.294
      
      # Calculate mass-balance for each reach moving down the network from headwaters to mouth:
      for(i in 1:length(V(network))){
        
        n_ID <- V(network)$name[i] #node ID being run
        e_ID <- incident(network, n_ID, mode = c("in")) #get edges so the data is shared - this should replace the mutate to nodes. 
        #e_ID$SEGMENT_ID
        #e_ID <- 
        length_reach <- sum(e_ID$length_m)
        length_reach <- ifelse(length_reach == 0, 10, length_reach) # locations within any edges in are springs and thus have 10 meter contributing
        V(network)$length_reach[i] <- length_reach
        
        # Find neighboring reaches upstream that flow in to the reach:
        up <- igraph::neighbors(network, i,mode=c("in")) #only single up
        up.all.nodes <- head(unlist(ego(network,order=length(V(network)),nodes=V(network)[i],mode=c("in"),mindist=0)),-1)
        
        # Define hydrologic inflows/outflows for each reach (m3 d-1)
        # Discharge inflow from local catchment (m3 d-1):
        #V(network)$Qlocal[i] <- V(network)$runoff_mday[i] * (V(network)$areasqkm[i]*10^6)
        V(network)$Qlocal[i] <-  length_reach #* V(network)$baseQ_m3dm[i] #baseflow per meter stream length[i]#V(network)$Q_lat_m3d[i] #V(network)$length_m[i] previous
        
        # Discharge inflow from upstream network (m3 d-1):
        if(length(up)>0){
          V(network)$Qnet[i] <- sum(V(network)$Qout[up]) #only one or junction will have two
        }  
        
        # Discharge outflow to downstream reach (m3 d-1):
        V(network)$Qout[i] <- sum(V(network)$Qlocal[i], V(network)$Qnet[i], na.rm = T)
        V(network)$width_m[i] <- a * (V(network)$Qout[i]/86400)^b #already in m3/d, needs to be in m3/s
        V(network)$depth_m[i] <- c * (V(network)$Qout[i]/86400)^d 
        V(network)$runoff_mday[i] = V(network)$Qout[i]/V(network)$CatchmentA[i] #need to check q_ma versus q0001e
        
        # Calculate reach hydraulic load (m d-1): double check day versus second
        V(network)$HL[i] <- V(network)$Qout[i]/(V(network)$width_m[i]*length_reach)
        
        # Define carbon inflows/outflows for each reach (g d-1)
        # Carbon inflow from local catchment (mg d-1):
        #groundwater
        V(network)$ClocalGW_gd[i] <- (V(network)$Qlocal[i]*V(network)$DOC_gw[i])/1000#m3/D * mg/m3
        #POC, sum direct input and lateral - assume direct for full width 
        V(network)$ClocalLit_gd[i] <- V(network)$C_in_gmd[i]*length_reach + V(network)$C_in_gm2d[i]*length_reach*V(network)$width_m[i]#m3/D * mg/m3
        ###HOW TO THINK ABOUT THIS IN AS FAR AS TRANSFER DOWNSTREAM - WILL LIKELY NOT INCLUDE AND INCLUDE LITTER BREAKDOWN HERE 
        
        
        ## TOTAL LOCAL CARBON LOAD
        V(network)$Clocal[i] <- V(network)$ClocalGW_gd[i] # ADD ALL ADDITIONAL TRANSPORTED LOADS
        
        # Carbon inflow from upstream network (g d-1):
        if(length(up)>0){
          V(network)$Cnet[i] <- sum(V(network)$Cout[up])
          V(network)$Cnet_gross[i] <- sum(V(network)$Cout_gross[up])
        } 
        
        #Gross gain of carbon load to downstream reach (g d-1):
        V(network)$Cout_gross[i] <- V(network)$ClocalGW_gd[i] + V(network)$Cnet_gross[i]
        
        # Exported carbon load to downstream reach (g d-1):
        V(network)$Cout[i] <- V(network)$Clocal[i] + V(network)$Cnet[i] - 
          ((V(network)$Clocal[i] + V(network)$Cnet[i])*(1-exp(-V(network)$vf[i]/V(network)$HL[i])))
        
        # Calculate fraction C lost
        V(network)$Clost[i] <- 1-(V(network)$Cout[i]/(V(network)$Clocal[i] + V(network)$Cnet[i]))
      }
      
      # Get list with attributes
      out <- get.vertex.attribute(network)
      
      # Calculate proportion C removed in the network:r
      out$Prop_C_lost <- 1-(V(network)$Cout[length(V(network))]/sum(V(network)$Clocal))
      
      # Export network:
      return(out)
      
    }
