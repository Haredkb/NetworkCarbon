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
#net <- readRDS("data/eg_watershed.RDS")
net <- readRDS("data/network_intital.RDS")

for(i in 1:length(V(net))){
  up.all <- ego(net,order=length(V(net)),nodes=V(net)[i],mode=c("in"),mindist=0)
  V(net)$up.all[i] <- length(unlist(up.all))
}

# Re-arrange network graph so that model starts with headwaters, and moves down the network to larger river reaches:
net2 <- igraph::graph_from_data_frame(d=igraph::as_data_frame(net,what="edges"),
                                      vertices=igraph::as_data_frame(net,what="vertices") %>%
                                        arrange(up.all))
V(net2)$label <- V(net)$up.all
net <- net2

#add reach length in (incident edges) and nodes contributing (nodes in)
net <- up_nodes(net)

## Read initial network (starting values)
#net <- read in network with starting values


#set up output dataframe
network_ts_all <- list()
network_ts_day <- list()
network_ts_day_id <- list()

###################
##Input Variables## 
###################

############################
#define time steps and units
##############################
timesteps = (24 * 90) -1 #minus one hour to end on correct day
ts_units = "hour" #hour'
# Set up model run times start dates
intial_dates = as.POSIXct(c("11-01-2018 00:00"), format = "%m-%d-%Y %H:%M")
#intial_dates = as.POSIXct(c("11-01-2018 00:00", "02-01-2019 00:00", "05-01-2019 00:00", "08-01-2019 00:00"), format = "%m-%d-%Y", tz = "GMT")
#))#

##########################
#### Read Input Data #####
##########################
###read decomposition raw data
k_df <- readRDS("data/k_df.RDS")
#Modelled Microbial Lambda based on landscape data
mod_lamM <- readRDS("mod_w1_LM.RDS") #kc_lamM ~ q_55 + temp_55   - #k = 0.005 #Feb - April 2018 WS 37
#Modelled Fragmentation Lambda based on landscape data
mod_lamF <- readRDS("mod_w1_LF.RDS") #kc_lamF ~ q_55 + temp_55

#Stream Temperature - Created in file MultipleRegressionAnalysis.R
temp_sin <- readRDS("data/temp_sin.RDS")
#temp_sin$ymean <- temp_sin$ymean  + 2 ### scenario 50 years 
        ### Keep for Temperature Scenarios###
        # #create synthetic scenarios, shallow and deep based on 0.55 and 20 day amp ratio for shallow and 0.4 for deep, 0.9 for atmospheric. For 50 year future values, only changed ymean 
        # temp_sin_sc <- data.frame(scen = c("ATM", "DEEP", "SHAL", "ATM50", "DEEP50", "SHAL50"), amp = c(9, 4,5.5, 9, 4, 5.5), phase = c(200, 200, 220, 200, 200, 220), ymean = c(13, 12, 12, 15, 12.5, 14) )
        # sc = 5
        #From Hare et al. 2021; 0.04 C/year (which is the mean rate of increase for both shall and atm), verus for 0.01 C/year; therefore for 50 years 2 C for atmospheric and shallow, versus 0.5 for deep groundwater 
        #temp_sin <- rbind(temp_scen, temp_sin) 

#CPOM 
cpom_gm2 <- readRDS("data/cpom_gm2.RDS")
#create a vector with the correct cpom initiatlation for each date. 
intial_cpomSS = cpom_gm2 %>% dplyr::filter(Jdate %in% yday(intial_dates))


#Baseflow Linear Model for Watershed- baseQ_predict m3/hr - slope only
    Q_JDate_lm_m3hr <- readRDS("data/Q_JDate_lm_m3hr.RDS")

#### Seep DOC Data by Month ####
    DOC_gw <- readRDS("data/DOC_gw.RDS")
    
####Direct and Lateral POC Inputs####
    ClocalLit_AFDMg <- readRDS("data/ClocalLit_g.RDS")
    
#### Basin data
    V(net)$basin_id <- if_else(V(net)$basin_id == "CoweetaCreek", "ShopeFork", V(net)$basin_id)
    V(net)$basin_id <- if_else(is.na(V(net)$basin_id) == TRUE, "ShopeFork", V(net)$basin_id)

####################################
#SETTING UP INITIAL DATE FOR THE RUN,
####################################
    
#has to be FOR loop as order matters
for(j in seq(1,length(intial_dates))){
  
  
          s_date = as.POSIXct(intial_dates[j], format = "%m-%d-%Y %H:%M")#("10-01-2018 00:00", format = "%m-%d-%Y %H:%M") #define model start date
          s_Jday = yday(s_date) #starting Julian Day 
          s_month = month(s_date) #starting month
          
          ##for loop for each time step
          #water yield, doc yield, litter inputs, temperature
                  dates <- seq(from = as.POSIXct(s_date), to = as.POSIXct(s_date + hours(timesteps)), by = "hour")
                  
                  #create new object to keep original intact
                  network <- net
                  
                  #set up igraph variables
                  # Inflow discharge from local catchment (m3 d-1):
                  V(network)$Qlocal <- 0
                  # Inflow discharge from upstream reaches (m3 d-1):
                  V(network)$Qup <- 0
                  # Outflow discharge from each reach (m3 d-1):
                  V(network)$Qout <- NA
                
          
          #REMOVE OBJECT TO REFRESH ENVIRONMENT        
                if(exists("network_pre")){
                  rm(network_pre)} #remove from global env
                  
          ##HOURLY IME STEPS        
          net_lst <- lapply(dates, function(ts, env = parent.frame(), inherits = FALSE){
                          print(ts)#print current time step
                          ts_pre <- ts - hours(1) #what was the previous time step 
                          
                          #initialize date details
                          #add date to igraph
                          V(network)$date <- as.character(ts)
                          #pull timestep specific values for filtering 
                          day <- yday(ts)
                          mon <- month(ts)
                          season <- quarter(ts, fiscal_start = 1) #season starting with jan
                          
      ### INITIAL INPUT
                ### Test if there is a previous time step, or if its a new day. As this generates a new Q, temp, breakdown based on day
                if(!exists("network_pre") || yday(ts) != yday(ts_pre)){
                      ################
                      ### Temperature###
                      #################
                          
                          ##For homogenous scenarios
                          #tempC <-  temp_sin[sc,]$amp * cos(rad_day(day -  temp_sin[sc,]$phase)) +  temp_sin[sc,]$ymean
                          #V(network)$tempC <- tempC
                          
                          ##For landscape temperature 
                          temp_sin$tempC <-  temp_sin$amp * cos(rad_day(day -  temp_sin$phase)) +  temp_sin$ymean
                          #add the day temperature to the igraph
                          V(network)$tempC <- makeVertexAtt(network, df=temp_sin, vname='tempC', by.df='stream', by.g='n_lscp_name')
                          
                      ###################
                      ## Breakdown #####
                      #################
                          ###### Using nearest upstream Landscape Values #######
                          ### Find nearest k values for each landscape point
                          ### these rates are in /day - converted latter
                          # k_df_ts <- k_df %>%
                          #   mutate(
                          #     Jdate = yday(date_s),
                          #     date_near = abs(Jdate - day))%>%
                          #   dplyr::group_by(stream) %>% 
                          #   dplyr::arrange(desc(Jdate), by.group = TRUE) %>% 
                          #   dplyr::mutate(mean_k_coarse = zoo::na.fill(mean_k_coarse, fill = "extend"),
                          #                 mean_k_shred_new = zoo::na.fill(mean_k_shred_new, fill = "extend"))%>%
                          #   slice(which.min(date_near))# keep the closest k for given timestep date for each landscape location
                          
                          # #from package Rsenal https://rdrr.io/github/brooksandrew/Rsenal/f/README.md
                          # V(network)$lambda_F <- makeVertexAtt(network, df=k_df_ts, vname='mean_k_shred_new', by.df='stream', by.g='n_lscp_name')
                          # V(network)$k_coarse <- makeVertexAtt(network, df=k_df_ts, vname='mean_k_coarse', by.df='stream', by.g='n_lscp_name')
                          # V(network)$lambda_M <- V(network)$k_coarse - V(network)$lambda_F
                      
                          
                      #################
                      ### CPOM ###
                      ################
                          ###find landscape cpom standingstock
                          cpom_ts <- cpom_gm2 %>%
                            dplyr::filter(Jdate == day)
                      
                        ### Add Litter POC (direct and lateral) in
                          set.seed(2)
                          POM_input_mon <- ClocalLit_AFDMg %>%
                            dplyr::filter(month == mon)  
                          
                          # ### Add Terrestrial Input DOC
                          DOC_seep_table_mon <- DOC_gw %>%
                            filter(mon == month)

                      #######################
                      ### Discharge ######
                      ####################
                          
                          ### CALCULATE BASE Q
                          bq_m3hrm <- Q_JDate_lm_m3hr %>%
                            dplyr::filter(Jdate == day)
                            
                          ### CALCULATE GEOMORPHIC PARAMETERS and NETWORK 

                            #network_pre <- get("network", env.pre())
                            ##Set up parameters that do not change. 
                            network <- netset(network, bq_m3hrm) 
                            
                            #add CPOM standing stock based 
                            V(network)$ss_POC_l <- cpom_ts$cpom   #makeVertexAtt(network, df=cpom_ts, vname='cbom')#vname='cbom.afdm.g.m2', by.df='stream', by.g='n_lscp_name')
                            V(network)$ss_POC <- V(network)$Bedarea_m2 * V(network)$ss_POC_l #initial POC standing stock, only used in first time step
                            
                            #POC in
                            V(network)$ClocalLit_AFDMg <- #(POM_input_mon$Cdirect_gm2hr *V(network)$Bedarea_m2) + #direct
                                                        POM_input_mon$Clateral_gmhr * V(network)$length_reach * 2 #lateral 
                            
                            #If the flow is not negative, mutliply DOC seep value by reach Q added to get DOC from GW for the reach
                            V(network)$DOC_local_gC <- if_else(DOC_seep_table_mon$doc_mgL * V(network)$Qlocal < 0, 0, DOC_seep_table_mon$doc_mgL * V(network)$Qlocal) # *1000 / 1000 mg / L <- g/m3
                            
                            ##Litter Breakdown temp and q55 dependence from landscape glm 
                            #create data frame to calculate temperature and stream discharge dependence 
                            mod_df <- data.frame(temp_55 = V(network)$tempC, q_55 = V(network)$Qout) %>%
                              dplyr::mutate(temp_55 = zoo::na.fill(temp_55, fill = "extend"),
                                            q_55 = zoo::na.fill(q_55, fill = "extend"))
                            
                            #need this extra step for fragmentation because of na (due to neg values)
                            k_TQ_lamF <- posterior_predict(mod_lamF,new = mod_df, type = "response")
                            k_TQ_lamF <- apply(k_TQ_lamF,2,median,na.rm = TRUE)
                            V(network)$k_TQ_lamF <- k_TQ_lamF
                            
                            k_TQ_lamM <- posterior_predict(mod_lamM,new = mod_df, type = "response")
                            k_TQ_lamM <- apply(k_TQ_lamM,2,median,na.rm = TRUE)
                            V(network)$k_TQ_lamM <- k_TQ_lamM
                            
                            
                            #############################
                            #Indicates this part of if statment run if there is no pre network, or the network is from the same day - no new Q, T or k values
                            message("No PRE or New day")
                            
                            
                    } else{ #USE THE PREVEIOUS DATA- except STandingstock 
                            network <- network_pre
                            message("Same day")
                            
                    }
                          

                             
                      #########################################
                      ##-----POC STANDING STOCK CALC-------- ##
                      #########################################
                          #POC IN hourly, sum direct input and lateral - assume direct for full width 
                          ### Previous Time Step to determine present standing stock for reach length (sStock)
                          if(!exists("network_pre")){


                            V(network)$POC_sStock_AFDMg <- V(network)$ss_POC + V(network)$ClocalLit_AFDMg 
                          
                            
                            #standing stock
                            message("No PRE")
                            
                            
                          } else({
                            #use previous time step POC standingstock + litter in 
                            V(network)$POC_sStock_AFDMg <- V(network_pre)$POC_AFDMg + V(network)$ClocalLit_AFDMg
                            message("Yes PRE")
                          
                          })
          
                          #POC standing Stock Loss percent per hour
                          # #Loss using Landscape k values
                          # V(network)$POC_loss_AFDMg   <- V(network)$POC_sStock_AFDMg * (V(network)$k_coarse/24) #0.041 october coarse Acer WS37 # 0.005  March Coarse Acer WS37 #placeholder (loss as positive value)
                          # V(network)$POC_loss_AFDMg_F <-  V(network)$POC_sStock_AFDMg * (V(network)$lambda_F/24)
                          # V(network)$POC_loss_AFDMg_M <-  V(network)$POC_sStock_AFDMg * (V(network)$lambda_M/24)
                          
                          #Loss using temperature and q dependence
                          V(network)$POC_loss_AFDMg_F_TQ <-  V(network)$POC_sStock_AFDMg * (V(network)$k_TQ_lamF/24)
                          V(network)$POC_loss_gC_F_TQ <- V(network)$POC_loss_AFDMg_F_TQ *0.484 #convert gC
                          V(network)$FTOC_local <- V(network)$POC_loss_AFDMg_F_TQ # I have two for clarity right now when conisdering transport in move_OC
                          V(network)$POC_loss_AFDMg_M_TQ <-  V(network)$POC_sStock_AFDMg * (V(network)$k_TQ_lamM/24)
                          V(network)$POC_loss_gC_M_TQ <- V(network)$POC_loss_AFDMg_M_TQ *0.484 #convert to gC
                          V(network)$POC_loss_AFDMg_TQ   <- V(network)$POC_loss_AFDMg_F_TQ + V(network)$POC_loss_AFDMg_M_TQ
                          V(network)$POC_loss_gC_TQ   <- V(network)$POC_loss_AFDMg_TQ *0.484
                          #remaining standstock end of timestep, using Temp q55 to create next timestep as this will be the final product
                          V(network)$POC_AFDMg <- V(network)$POC_sStock_AFDMg - V(network)$POC_loss_AFDMg_TQ
                          
                          
                          ################
                          ### Calculate movement within basin
                          ### FPOC and DOC ###
                      if(!exists("network_pre")){
                        #set up transport
                        V(network)$FTOC_up <- 0
                        V(network)$DOC_up <- 0
                        V(network)$FTOC_out <- V(network)$FTOC_local
                        V(network)$DOC_out <- V(network)$DOC_local_gC
                      }else{
                          network <- move_OC(network, network_pre)
                      }
                  
                ##set up environment for next timestep
                          #YES ASSIGNING TO THE GLOBAL IS FROWNED ON _ WILL CHANGE TO DIFFERNT ONE BUT WORKS!!!!! 
                          assign("network_pre", network, envir = .GlobalEnv)
                          message(Sys.time())
                          return(network)
                          
                  })
          
          
          ##################
          ### OUTPUT #######
          ##################
          #Create dataframe#
          network_cols = colnames(as.data.frame(get.vertex.attribute(net_lst[[1]])))
          network_ts <- data.frame(matrix(ncol = length(network_cols), nrow = 0))
          ts_all <- data.frame(matrix(ncol = length(network_cols), nrow = 0))
          
          for(i in seq(2, length(net_lst))){ # start with second one as the first timestep doesnt have transport.
            ts_all = rbind(ts_all, as.data.frame(get.vertex.attribute(net_lst[[i]])))
          }
          
          #summarize breakdown and gw_DOC
          ts_all$date_d <- as.Date(ts_all$date)
          
          ### Sum hourly timesteps to the day
          ts_day <- ts_all %>%
                                        group_by(date_d) %>%
                                        summarise(
                                          C_breakdown_AFDMg_TQ = sum(POC_loss_AFDMg_TQ, na.rm = TRUE),
                                          #C_breakdown_g_ss_tIV = sum(POC_loss_AFDMg_ss_tIV),
                                          #C_breakdown_AFDMg = sum(POC_loss_AFDMg, na.rm = TRUE),
                                          C_gw_gC = sum(DOC_local_gC, na.rm = TRUE)
                                        )
          
          ##Note the "lengthup_m" is from the original GIS calculations, so is determined seperately from this code. 
          ##Therefore, the SLout and lengthup_m should be ~ the same. 
          
          # #
          # show_env <- solveMB(){
          #   list(ran.in = environment(), 
          #        parent = parent.env(environment()), 
          #        objects = ls.str(environment()))
          # }
          
          #dataframe with hourly timesteps
          network_ts_all[[j]] <- as.data.frame(ts_all)
          #loss and doc summed per day
          network_ts_day[[j]] <- ts_day
}


# For saving data
# saveRDS(network_ts_all, "network_ts_all_DEEP50.RDS")
# saveRDS(network_ts_day, "network_ts_day_DEEP50.RDS")
# saveRDS(network_ts_day_id, "network_ts_dayid_DEEP50.RDS")
# 

#Join timestep lists into a signle dataframe
network_ts_all_ss <- do.call(rbind,  network_ts_all)
network_ts_day_df <- do.call(rbind,  network_ts_day)


##### Create summary table by date and gC in and 
network_ts_day_df <- network_ts_day_df %>% #do.call(rbind,  network_ts_day_ss_DEEP50) %>%
  mutate(month_dep = as.factor(month(date_d, label = TRUE))) %>%
  group_by(month_dep) %>%
  summarise(
    #C_internal = mean(C_breakdown_g),
    POCbreakdown_TQ = mean(C_breakdown_AFDMg_TQ)*0.484,
    DOCseep = mean(C_gw_gC))%>%
  pivot_longer(col = 2:3) #%>%#4)%>%
  #mutate(value = if_else(startsWith(name, "C_internal"), value * 0.484, value)) #correct ADFM to gC

network_ts_day_df_2 %<>%
  dplyr::filter(name == "POC_breakdown_TQ")%>%
  mutate(name = "POC_breakdown_TQ_2")

network_ts_day_df_compare <- left_join(network_ts_day_df, network_ts_day_df_2, by = c("month_dep", "name"))%>%
  mutate(diff_2 = value.y - value.x)

## Plot Internal gC from breakdown and gC from dissolved
ggplot(network_ts_day_df) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), value, fill = name), width=.5, position = "dodge")+
  scale_fill_manual(values=c("blue","#56B4E9"))+
  xlab("")+
  ylab("gC per day")

ggplot(network_ts_day_df) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Dec", "Jan", "Feb"), value, fill = name), width=.5, position = "dodge")+
  scale_fill_manual(values=c("blue","#56B4E9"))+
  xlab("")+
  ylab("gC per day")
