###################################
###################################
##Coweeta Carbon Network Analysis##
###################################
###################################

#source all req packages and scripts
source("R/global.R")

#Set start date
intial_dates = as.POSIXct(c("08-01-2018"), format = "%m-%d-%Y")


## Discharge BaseFlow Calculation to provide as read in file
#ask user if they really want to run the baseQ again
var = readline(prompt = "Do you want to reRun baseflow calculations for the network: Y/N")

if(var == "Y"){
            ####net <- readRDS("data/eg_watershed.RDS") #examples  small wateshed WS37
            net <- readRDS("data/network_intital.RDS") #full Coweeta Watershed
            
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
            
            #set up time steps
            timesteps = 364 #days
            ts_units = "day" #hour' #days for Q as its the same for every hour within the day, but still units are per hr. 
            # Set up model run times start dates

            s_date = as.POSIXct(intial_dates[1], format = "%m-%d-%Y")
            #intial_dates = as.POSIXct(c("11-01-2018 00:00", "02-01-2019 00:00", "05-01-2019 00:00", "08-01-2019 00:00"), format = "%m-%d-%Y", tz = "GMT")
            #))#
            
            #Baseflow Linear Model for Watershed- baseQ_predict m3/hr - slope only
            Q_JDate_lm_m3hr <- readRDS("data/Q_JDate_lm_m3hr.RDS")
            
            #### Basin data
            V(net)$basin_id <- if_else(V(net)$basin_id == "CoweetaCreek", "ShopeFork", V(net)$basin_id)
            V(net)$basin_id <- if_else(is.na(V(net)$basin_id) == TRUE, "ShopeFork", V(net)$basin_id)
            
            #create new object to keep original intact
            network <- net
            
            ### CALCULATE BASE Q
            dates_day <- seq(from = as.Date(s_date), to = as.Date(s_date + days(timesteps)), by = "day")
            
            
            net_lstQ <- lapply(dates_day, function(t_s){
                                              message(t_s)
                                              #initialize date details
                                              #add date to igraph
                                              V(network)$date <- as.character(t_s)  #, format = "%Y-%m-%d")
                                              #print(V(network)$date[1])
                                              
                                              #pull timestep specific values for filtering 
                                              day <- yday(t_s)
                                              mon <- month(t_s)
                                              season <- quarter(t_s, fiscal_start = 1)
            
            
                                              #set up igraph variables
                                              # Inflow discharge from local catchment (m3 d-1):
                                              V(network)$Qlocal <- 0
                                              # Inflow discharge from upstream reaches (m3 d-1):
                                              V(network)$Qup <- 0
                                              # Outflow discharge from each reach (m3 d-1):
                                              V(network)$Qout <- NA
                                              
                                              bq_m3hrm <- Q_JDate_lm_m3hr %>%
                                                dplyr::filter(Jdate == day)
            
                                        ### CALCULATE GEOMORPHIC PARAMETERS and NETWORK 
                                        
                                        #network_pre <- get("network", env.pre())
                                        ##Set up parameters that do not change. 
                                        network <- netset(network, bq_m3hrm) 
                                        })#end net_lstQ

            names(net_lstQ) <- as.character(dates_day)#name each igraph as its date

            saveRDS(net_lstQ, "output/data/net_lst_baseQ.RDS")
} else {# read in previously saved version
            net_lstQ <- readRDS("output/data/net_lst_baseQ.RDS")
            }

###########################################################################
## Run multiple  Temperature Scenarios
#Stream Temperature - Created in file MultipleRegressionAnalysis.R

#From Hare et al. 2021; 0.04 C/year (which is the mean rate of increase for both shall and atm), verus for 0.01 C/year; therefore for 50 years 2 C for atmospheric and shallow, versus 0.5 for deep groundwater 
#temp_sin <- rbind(temp_scen, temp_sin) 

temp_sin <- readRDS("data/temp_sin.RDS")
scen_T <- list(base = temp_sin, #base
               base2 = mutate(temp_sin, ymean=  ymean + 2), #base + 2
               deepGW = mutate(temp_sin, amp = 4, phase = 200, ymean = 12), #deep GW
               shalGW = mutate(temp_sin, amp = 5.5, phase = 220, ymean = 12),#shallow GW
               shalGW_40 = mutate(temp_sin, amp = 5.5, phase = 240, ymean = 12),#shallow GW
               low_GW = mutate(temp_sin, amp = 9, phase = 200, ymean = 13), #minimal GW influence 
               low_GW_5 = mutate(temp_sin, amp = 9, phase = 200, ymean = 18)
               )
scen <- names(scen_T) #list the scen

scenarios_temperature <- lapply(scen, function(scen_temp){
  temp_sin <- scen_T[scen_temp][[1]]
  
    #set up output dataframe
    network_ts_all <- list()
    network_ts_day <- list()
    network_ts_day_id <- list()
    
    ###################
    ##Input Variables## 
    ###################
    
    ############################
    #define hour time steps and units
    ##############################
    timesteps = (24 * 365) -1 #minus one hour to end on correct day
    ts_units = "hour" #hour'
    # Set up model run times start dates
    #intial_dates = as.POSIXct(c("08-01-2018 00:00"), format = "%m-%d-%Y %H:%M")
    #intial_dates = as.POSIXct(c("11-01-2018 00:00", "02-01-2019 00:00", "05-01-2019 00:00", "08-01-2019 00:00"), format = "%m-%d-%Y", tz = "GMT")
    #))#
    
    ##########################
    #### Read Input Data #####
    ##########################
    # ###read decomposition raw data
    # k_df <- readRDS("data/k_df.RDS")
    # #Modelled Microbial Lambda based on landscape data
    # mod_lamM <- readRDS("mod_w1_LM.RDS") #kc_lamM ~ q_55 + temp_55   - #k = 0.005 #Feb - April 2018 WS 37
    # #Modelled Fragmentation Lambda based on landscape data
    # mod_lamF <- readRDS("mod_w1_LF.RDS") #kc_lamF ~ q_55 + temp_55
    mod_k <- readRDS("data/Model_CC.RDS")
    
    #CPOM 
    cpom_gm2 <- readRDS("data/cpom_gm2.RDS")
    #create a vector with the correct cpom initiatlation for each date. 
    intial_cpomSS = cpom_gm2 %>% dplyr::filter(Jdate %in% yday(intial_dates))
    
    #### Seep DOC Data by Month ####
        DOC_gw <- readRDS("data/DOC_gw.RDS")
        
    ####Direct and Lateral POC Inputs####
        #ClocalLit_AFDMg <- readRDS("data/ClocalLit_g.RDS")
        ClocalLit_AFDMg <- readRDS("data/POM_In.RDS") #%>%
          #dplyr::select(-2:-3) #remove by day values
        

    
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
              
              #REMOVE OBJECT TO REFRESH ENVIRONMENT        
                    if(exists("network_pre")){
                      rm(network_pre)} #remove from global env
                      
              ##HOURLY IME STEPS        
              net_lst <- lapply(dates, function(t_s, env = parent.frame(), inherits = FALSE){
                              print(t_s)#print current time step
                              ##what was the previous time step , fifelse presevers type and class of inputs this catches daylight savings
                              ts_pre <- dplyr::if_else(is.na(t_s - hours(1)) == TRUE, t_s - hours(2), t_s - hours(1)) 
                              
                              #initialize date details
                              #pull timestep specific values for filtering 
                              date_c <- as.character(t_s)
                              day <- yday(t_s)
                              mon <- month(t_s)
                              season <- quarter(t_s, fiscal_start = 1) #season starting with jan
              
                              
          ### INITIAL INPUT
                    ### Test if there is a previous time step, or if its a new day. As this generates a new Q, temp, breakdown based on day
                    if(!exists("network_pre") || yday(t_s) != yday(ts_pre)){
                      #######################
                      ### Discharge ######
                      ####################
                      #if there is no previous network (eg start of the session), or there is a new day pull the correct date baseQ (standing stock will be still use previous timestep)
                      network <- net_lstQ[[date_c]] #pull the correct network generated from baseQ data
                      
                      #add date to igraph
                      V(network)$date <- as.character(t_s)  #, format = "%Y-%m-%d")
                      
                          ################
                          ### Temperature###
                          #################
                              
                              
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
                              ###find day landscape cpom standingstock g per m2 - interpolated and averaged
                              cpom_ts <- cpom_gm2 %>%
                                dplyr::filter(Jdate == day)
                          
                            ### Add Litter POC (direct and lateral) in
                              set.seed(2)
                              # POM_input_mon <- ClocalLit_AFDMg %>%
                              #   dplyr::filter(month == mon)  
                              POM_input_day <- ClocalLit_AFDMg %>%
                                dplyr::filter(Jdate == day)
                             ### Add Terrestrial Input DOC
                              DOC_seep_table_mon <- DOC_gw %>%
                                filter(mon == month)
    
                                
                                #add CPOM standing stock based 
                                V(network)$ss_POC_l <- cpom_ts$cpom_fit   #makeVertexAtt(network, df=cpom_ts, vname='cbom')#vname='cbom.afdm.g.m2', by.df='stream', by.g='n_lscp_name')
                                V(network)$ss_POC <- V(network)$Bedarea_m2 * V(network)$ss_POC_l #initial POC standing stock, only used in first time step
                                
                                #POC in - only using lateral
                                # V(network)$ClocalLit_AFDMg <- (POM_input_mon$Cdirect_gm2hr *V(network)$Bedarea_m2) + #direct
                                #                             (POM_input_mon$Clateral_gmhr * V(network)$length_reach * 2) #lateral
                                V(network)$ClocalLit_AFDMg <- POM_input_day$Cin_gm2hr_all *V(network)$Bedarea_m2 #added in cbom_organization.R  
                                
                                #If the flow is not negative, mutliply DOC seep value by reach Q added to get DOC from GW for the reach
                                V(network)$DOC_local_gC <- if_else(DOC_seep_table_mon$doc_mgL * V(network)$Qlocal < 0, 0, DOC_seep_table_mon$doc_mgL * V(network)$Qlocal) # *1000 / 1000 mg / L <- g/m3
                                
                                # ##Litter Breakdown temp and q55 dependence from landscape glm 
                                # #create data frame to calculate temperature and stream discharge dependence 
                                # mod_df <- data.frame(temp_55 = V(network)$tempC, q_55 = V(network)$Qout) %>%
                                #   dplyr::mutate(temp_55 = zoo::na.fill(temp_55, fill = "extend"),
                                #                 q_55 = zoo::na.fill(q_55, fill = "extend"))
                                # 
                                # #need this extra step for fragmentation because of na (due to neg values)
                                # k_TQ_lamF <- posterior_predict(mod_lamF,new = mod_df, type = "response")
                                # k_TQ_lamF <- apply(k_TQ_lamF,2,median,na.rm = TRUE)
                                # V(network)$k_TQ_lamF <- k_TQ_lamF
                                # 
                                # k_TQ_lamM <- posterior_predict(mod_lamM,new = mod_df, type = "response")
                                # k_TQ_lamM <- apply(k_TQ_lamM,2,median,na.rm = TRUE)
                                # V(network)$k_TQ_lamM <- k_TQ_lamM
                                
                                
                                #########BREAKDOWN UPDATES##################
                                ##k using carolyn's models##
                                ##updated 2022-08-02## 

                                #add scaled temp and q to the network
                                V(network)$one.k.T.cent <-  (1/((V(network)$tempC + 273.15)* 8.62E-5)) - 40.66639
                                V(network)$mean_flow_st = scale(V(network)$Qout, 93.58932, 205.6402) #scale the Qout absed on CC model, hardcoded but in "setup_data_for_CCmodel.R" #attr(mean_flow_sc, "scaled:center"), attr(mean_flow_sc, "scaled:scale"))
                                
                                #create input dataframe
                                mod_df <- data.frame(one.k.T.cent = V(network)$one.k.T.cent, mean_flow_st = V(network)$mean_flow_st)
                                mod_df$Type = "Microbes"
                                mod_df$rhodo_acer = "A"
                                
                                #"A" for Acer and "M" for Microbes
                                #https://stackoverflow.com/questions/28199140/using-lme4-modeling-to-predict-from-fixed-effects-values
                                mod_AM <- predict(mod_k ,newdata=mod_df,re.form=~0)#treat as population, so no RE
                                
                                
                                #create input dataframe
                                mod_df <- data.frame(one.k.T.cent = V(network)$one.k.T.cent, mean_flow_st = V(network)$mean_flow_st)
                                mod_df$Type = "Shredders"
                                mod_df$rhodo_acer = "A"
                                mod_AF <- predict(mod_k,newdata=mod_df,re.form=~0)#treat as population, so no RE
                                
                                #output into the network 
                                V(network)$k_AM <- exp(mod_AM)
                                V(network)$k_AF <- exp(mod_AF)
                                V(network)$k_At <- V(network)$k_AM + V(network)$k_AF
                                
                                #############################
                                ##Indicates this part of if statment run if there is no pre network, or the network is from the same day - no new Q, T or k values
                                #message("No PRE or New day") #for bebugging
                                
                                
                        } else{ #USE THE PREVEIOUS DATA- except STandingstock 
                                network <- network_pre
                                #message("Same day") #for debugging
                                
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
                              
                              # #Loss using temperature and q dependence
                              # V(network)$POC_loss_AFDMg_F_TQ <-  V(network)$POC_sStock_AFDMg * (V(network)$k_TQ_lamF/24)
                              # V(network)$POC_loss_gC_F_TQ <- V(network)$POC_loss_AFDMg_F_TQ *0.484 #convert gC
                              # V(network)$FTOC_local <- V(network)$POC_loss_AFDMg_F_TQ # I have two for clarity right now when conisdering transport in move_OC
                              # V(network)$POC_loss_AFDMg_M_TQ <-  V(network)$POC_sStock_AFDMg * (V(network)$k_TQ_lamM/24)
                              # V(network)$POC_loss_gC_M_TQ <- V(network)$POC_loss_AFDMg_M_TQ *0.484 #convert to gC
                              # V(network)$POC_loss_AFDMg_TQ   <- V(network)$POC_loss_AFDMg_F_TQ + V(network)$POC_loss_AFDMg_M_TQ
                              # V(network)$POC_loss_gC_TQ   <- V(network)$POC_loss_AFDMg_TQ *0.484
                              
                              V(network)$k_AM
                              
                              #Loss using CREWS (CC) Ladnscape Model
                              V(network)$POC_loss_AFDMg_F <-  V(network)$POC_sStock_AFDMg * (V(network)$k_AF/24)
                              V(network)$POC_loss_gC_F <- V(network)$POC_loss_AFDMg_F *0.484 #convert gC
                              V(network)$FTOC_local <- V(network)$POC_loss_AFDMg_F # I have two for clarity right now when conisdering transport in move_OC
                              V(network)$POC_loss_AFDMg_M <-  V(network)$POC_sStock_AFDMg * (V(network)$k_AM/24)
                              V(network)$POC_loss_gC_M <- V(network)$POC_loss_AFDMg_M *0.484 #convert to gC
                              V(network)$POC_loss_AFDMg   <- V(network)$POC_loss_AFDMg_F + V(network)$POC_loss_AFDMg_M
                              V(network)$POC_loss_gC   <- V(network)$POC_loss_AFDMg *0.484#convert to gC
                              
                              
                              # #remaining standstock end of timestep, using Temp q55 to create next timestep as this will be the final product
                              # V(network)$POC_AFDMg <- V(network)$POC_sStock_AFDMg - V(network)$POC_loss_AFDMg_TQ
                              
                              V(network)$POC_AFDMg <- V(network)$POC_sStock_AFDMg - V(network)$POC_loss_AFDMg
                              print(sum(V(network)$POC_AFDMg))#test
                              ################
                              
                              ### ADD IN FOR SERIAL###
                          #     ### Calculate movement within basin
                          #     ### FPOC and DOC ###
                          # if(!exists("network_pre")){
                          #   #set up transport
                          #   V(network)$FTOC_up <- 0
                          #   V(network)$DOC_up <- 0
                          #   V(network)$FTOC_out <- V(network)$FTOC_local
                          #   V(network)$DOC_out <- V(network)$DOC_local_gC
                          # }else{
                          #     network <- move_OC(network, network_pre)
                          #     message("moveOC")
                          # }
                      
                    ##set up environment for next timestep
                              #YES ASSIGNING TO THE GLOBAL IS FROWNED ON _ WILL CHANGE TO DIFFERNT ONE BUT WORKS!!!!! 
                              assign("network_pre", network, envir = .GlobalEnv)
                              message(Sys.time())
                              return(network)
                              
                      })
              
              
              ##################
              ### OUTPUT #######
              ##################
              # #Create dataframe#
              # network_cols = colnames(as.data.frame(get.vertex.attribute(net_lst[[1]])))
              # network_ts <- data.frame(matrix(ncol = length(network_cols), nrow = 0))
              # ts_all <- data.frame(matrix(ncol = length(network_cols), nrow = 0))
              
              #remoev first list in network list as there is no transport
              net_lst[[1]] <- NULL
              
              #"convert igraphs to dataframe baed on vertices attribtues
              ts_all <-  lapply(net_lst, function(i){ # start with second one as the first timestep doesnt have transport.
                igraph::as_data_frame(i, what = "vertices")
              })
              
              ts_all <-  do.call("rbind", ts_all) %>%
                dplyr::mutate(date_d = as_datetime(date, format = "%Y-%m-%d"))
              
              
              #summarize breakdown and gw_DOC
              ### Sum hourly timesteps to the day
              ts_day <- ts_all %>%
                                            #mutate(date_d = as.Date(date)) %>% #add date (no time) column for grouping)
                                            group_by(date_d) %>%
                                            summarise(
                                              C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
                                              C_breakdown_gC = sum(POC_loss_AFDMg, na.rm = TRUE) * 0.484,
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
              
              
              
              # #### For runs with multiple inital dates 
              # #dataframe with hourly timesteps
              # network_ts_all[[j]] <- ts_all#as.data.frame(ts_all)
              # #loss and doc summed per day
              # network_ts_day[[j]] <- ts_day
    }
    
    
    # ##Join timestep lists into a single dataframe
    # network_ts_all_ss <- do.call(rbind,  network_ts_all)
    # network_ts_day_df <- do.call(rbind,  network_ts_day)
    
    ############Data Output #####################################
    # For saving data
    saveRDS(net_lst, paste0("output/data/net_lst_",scen_temp,"_noserialC.RDS"))
    saveRDS(ts_all, paste0("output/data/network_ts_all_", scen_temp, "_noserialC.RDS"))
    # write.csv(ts_all, "output/data/network_ts_all_lowGW_direct_noserialC.csv")
    # saveRDS(ts_day, "output/data/network_ts_day_lowGW_direct_noserialC.RDS")
    # write.csv(ts_day, "output/data/network_ts_day_lowGW_direct_noserialC.csv")
    
    
    ##############Plotting#############################
    ## Plot Internal gC from breakdown and gC from dissolved
    p <- ts_day %>%
      pivot_longer(., cols = 2:4)%>%
      ggplot(.)+
      geom_smooth(aes(date_d, value, group = name, color = name))+
      geom_line(aes(date_d, value, group = name, color = name))+
      scale_color_manual(values=c("#56B4E9", "blue", "brown"))+
      xlab("")+
      ylab("total gC per day")
    ggsave(plot = p,filename =  paste0("output/figures/", scen_temp, ".png"))
    
    ################ Create summary table by date and gC from breakdown and gC from GW DOC
    network_ts_day_df <- ts_day %>%#network_ts_day_df %>% #do.call(rbind,  network_ts_day_ss_DEEP50) %>%
      mutate(month_dep = as.factor(month(date_d, label = TRUE))) %>%
      group_by(month_dep) %>%
      summarise(
        POCbreakdown = mean(C_breakdown_gC),
        DOCseep = mean(C_gw_gC),
        POCin = mean(C_LitterIn_gC))%>%
      pivot_longer(col = 2:4) #%>%#4)%>%
    
    p <- ggplot(network_ts_day_df) +
      geom_col(aes(fct_relevel(month_dep, "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), value, fill = name), width=.5, position = "dodge")+
      scale_fill_manual(values=c("blue", "#56B4E9", "brown"))+
      xlab("")+
      ylab("average gC per day")+
      ggtitle(paste0("Landscape Temperature Scenarios-", scen_temp))
    ggsave(plot = p, filename = paste0("output/figures/CWT_", scen_temp, "_monthlyavg.png"))
    
    return(ts_all)
}
)#end scenario lapply

saveRDS(scenarios_temperature, "output/data/scenarios_temperature_serialC.RDS")
