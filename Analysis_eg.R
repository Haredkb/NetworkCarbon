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
## Read initial network (starting values)
#net <- read in network with starting values

#set up output dataframe
network_ts_all <- list()
network_ts_day <- list()
network_ts_day_id <- list()


##Input Variables## 
#k = 0.005 #Feb - April 2018 WS 37
#model temperature dependance 
tempM.glm.gamma <- readRDS("tempM_glm_gamma.RDS")

#Stream Temperature - Created in file MultipleRegressionAnalysis.R
temp_sin <- readRDS("data/temp_sin.RDS")

#create sythehic scenarios, shallow and deep based on 0.55 and 20 day amp ratio for shallow and 0.4 for deep, 0.9 for atmospheric. For 50 year future values, only changed ymean 
temp_sin <- data.frame(scen = c("ATM", "DEEP", "SHAL", "ATM50", "DEEP50", "SHAL50"), amp = c(9, 4,5.5, 9, 4, 5.5), phase = c(200, 200, 220, 200, 200, 220), ymean = c(13, 12, 12, 15, 14, 12.5) )
sc = 5
#Fom Hare et al. 2021; 0.04 C/year (which is the mean rate of increase for both shall and atm), verus for 0.01 C/year; therefore for 50 years 2 C for atmospheric and shallow, versus 0.5 for deep groundwater 
#temp_sin <- rbind(temp_scen, temp_sin) 
#WY_jdate <- as.numeric(difftime(as.Date("01-01-2018", format = "%m-%d-%Y"), as.Date("10-01-2017", format = "%m-%d-%Y"), units = c("days")))

cpom <- read.csv("G:/My Drive/CREWS_official/200_Landscape/210_data_landscape/211_organicmatter/Landscape_cbom/landscape_cbom_calculations_master_31Oct2020.csv") %>%
  dplyr::filter(om.category == "LEAF") %>%
  mutate(date = as.Date(sample.date, format = "%m/%d/%Y"),
         Jdate = yday(date),
         date_diff = ifelse(Jdate > 274, Jdate - 274, Jdate + as.numeric(difftime(as.Date("01-01-2018", format = "%m-%d-%Y"), as.Date("10-01-2017", format = "%m-%d-%Y"), units = c("days")))))

cpom.lm <- lm(cbom.afdm.g.m2 ~ date_diff, cpom)

ggplot(cpom, aes(x = date_diff, y = cbom.afdm.g.m2)) + 
  geom_point() + 
  geom_abline(slope = coef(cpom.lm)[["date_diff"]], 
              intercept = coef(cpom.lm)[["(Intercept)"]])
cpom_gm2 <- data.frame(date_diff = seq(1,365))
cpom_gm2$cpom <-  predict.lm(cpom.lm, cpom_gm2)

cpom_gm2$Jdate <- ifelse(cpom_gm2$date_diff <= 92, cpom_gm2$date_diff + 273, cpom_gm2$date_diff - 92)



#define time steps and units
timesteps = (24 * 3) -1
ts_units = "hour" #hour
intial_dates = c("11-01-2018 00:00", "02-01-2019 00:00", "05-01-2019 00:00", "08-01-2019 00:00")

for(j in seq(1,length(intial_dates))){
  
  
          s_date = as.POSIXct(intial_dates[j], format = "%m-%d-%Y %H:%M")#("10-01-2018 00:00", format = "%m-%d-%Y %H:%M") #define model start date
          s_Jday = yday(s_date) #starting Julian Day 
          s_month = month(s_date) #starting month
          
          ##########################
          #### Read Input Data #####
          ##########################
          #read input files (inputs external to the network)
                  #decomposition rate
                  
                      #Baseflow Linear Model for Watershed- CWT_NetworkModel_v3.R
                      Q_JDate_lm <- readRDS("data/Q_JDate_lm_m3hr.RDS")  %>%
                        dplyr::select(-starts_with("std.error"), -starts_with("p.value"), -starts_with("statistic"))%>%
                        pivot_wider(names_from = term, values_from = estimate)%>%
                        dplyr::rename(b = "(Intercept)", m = totLength_) #CHECK UNITS OF INPUT LM
                        #dplyr::mutate(b_m3hr = ((b/1000)* (60*60)), m_m3hr = (m/1000) * (60*60))
                    
                      #Seep Data by Month
                      DOC_seep_table <- readRDS("data/DOC_seep_table.RDS")
                      set.seed(2)
                      DOC_gw <- lapply(seq(1:12), function(x, df = DOC_seep_table, l = length(V(net))){
                      
                        data.frame(doc_mgL = df[x,]$doc_ppm_av,#abs(rnorm(l, mean = df[x,]$doc_ppm_av, sd = df[x,]$doc_ppm_sd)),
                                   month = x
                    )
                  })
                  
                  DOC_gw = do.call(rbind,  DOC_gw)
                  
                  
                  #Direct and Lateral C Inputs
                  POM_input <- readRDS("data/POM_input.RDS") 
                  #make dataframe of values throughout network for consistency
                  set.seed(2)
                  ClocalLit_g <- lapply(seq(1:12), function(x, df = POM_input, l = length(V(net))){
          
                      data.frame(Cdirect_gm2hr = abs(rnorm(l, mean = df[x,]$direct_gm2d_avg, 
                                                       sd = df[x,]$direct_gm2d_sd))/24, #direct hourly
                             
                                 Clateral_gmhr = abs(rnorm(l, mean = df[x,]$lateral_gmd_avg, 
                                  sd = df[x,]$lateral_gmd_sd))/24, #lateral hourly
                                  month = x
                                 )
                    
                    })
                  ClocalLit_g = do.call(rbind,  ClocalLit_g)

                  
                  
                  
          ##for loop for each time step
          #water yield, doc yield, litter inputs, temperature
                  dates <- seq(from = as.POSIXct(s_date), to = as.POSIXct(s_date + hours(timesteps)), by = "hour")
                  
                  
                  network <- net
                  
                  #set up igraph variables
                  # Inflow discharge from local catchment (m3 d-1):
                  V(network)$Qlocal <- 0
                  # Inflow discharge from upstream reaches (m3 d-1):
                  V(network)$Qnet <- 0
                  # Outflow discharge from each reach (m3 d-1):
                  V(network)$Qout <- NA
                  
          
          #day iteration        
          if(exists("network_pre")){
            rm(network_pre)} #remove from global env
                  
                  
          net_lst <- lapply(dates, function(ts, env = parent.frame(), inherits = FALSE){
                        #for(i in seq_along(dates)){ #working on function for this
                          print(ts)
            
                          #initalize date details
                          #ts <- dates[i]
                          V(network)$date <- as.character(ts)
                          day <- yday(ts)
                          mon <- month(ts)
                          season <- quarter(ts, fiscal_start = 1)
                          
                          #decomposition time since leaves entered
                          t = as.numeric(difftime(ts, as.Date("10-01-2018", format = "%m-%d-%Y"), units = c("days")))
                          V(network)$t_LitRT <- t
                          
                          #jdate in terms of october 1
                          Jdate_WY <- ifelse(day > 274, day - 274, day + as.numeric(difftime(as.Date("01-01-2018", format = "%m-%d-%Y"), as.Date("10-01-2017", format = "%m-%d-%Y"), units = c("days"))))
                          
                        ### INITIAL INPUT 
                          ### Temperature
                          tempC <-  temp_sin[sc,]$amp * cos(rad_day(day -  temp_sin[sc,]$phase)) +  temp_sin[sc,]$ymean
                          V(network)$tempC <- tempC
                          #k <- as.numeric(coef(tempM.glm)[1] + coef(tempM.glm)[season + 1] * temp)
                          
                          #create dataframe to calculate temperature dependance 
                          temp_df <- data.frame(temp_Dmed = tempC, season = as.factor(season))
                          
                          #k for a given season and temperature 
                          k <- median(posterior_predict(tempM.glm.gamma, new = temp_df, type = "response"))
                          V(network)$k <- k
                          
                          ### CALCULATE BASE Q
                          bq_m3dm <- Q_JDate_lm %>%
                            dplyr::filter(JDate == day) 
                          
                          ### Add Litter POC (direct and lateral) in
                          set.seed(2)
                          POM_input_mon <- ClocalLit_g %>%
                            dplyr::filter(month == mon)  
                          
                          # ### Add Terrestrial Input DOC
                          DOC_seep_table_mon <- DOC_gw %>%
                            filter(mon == month)
                          
                          #temperature
                          #n$temp <-  n$amp * cos(rad_day(j - n$phase)) + n$ymean
                          
                          ### CALCULATE GEOMORPHIC PARAMETERS
                          network <- netset(network, bq_m3dm)
                             
                      #########################################
                      ##-----POC STANDING STOCK CALC-------- ##
                      #########################################
                          #POC IN hourly, sum direct input and lateral - assume direct for full width 
                          V(network)$ClocalLit_g <- (POM_input_mon$Cdirect_gm2hr *V(network)$Bedarea_m2) + #direct
                                                      POM_input_mon$Clateral_gmhr * V(network)$length_reach * 2 #lateral 
                          
                          V(network)$DOC_local <- DOC_seep_table_mon$doc_mgL * V(network)$Qlocal # *1000 / 1000 mg / L <- g/m3
                          
                          ss <- cpom_gm2%>%
                            dplyr::filter(Jdate == day)%>%
                            dplyr::select(cpom) 
                          
                          ss <- as.numeric(ss[1])
                          
                          V(network)$ss_POC <- V(network)$Bedarea_m2 * ss
                          
                          ### Previous Time Step to determine present standing stock (sStock)
                          if(!exists("network_pre")){
                          #   #network_pre <- get("network", env.pre())
                            V(network)$POC_sStock_g <- V(network)$ss_POC + V(network)$ClocalLit_g #use stocking stock linear fit to initiate the amount of standing stock, but the time steps are a function of the loss - in
                            #standing stock
                            message("No PRE")
                          } else({
                            V(network)$POC_sStock_g <- V(network_pre)$POC_g + V(network)$ClocalLit_g
                            message("Yes PRE")
                          
                          
                          })
          
                          #POC standing Stock Loss percent per hour
                          loss_percent_hr <- k
                          
                          #Based on litter input values
                          V(network)$POC_loss_g <- V(network)$POC_sStock_g * loss_percent_hr #0.041 october coarse Acer WS37 # 0.005  March Coarse Acer WS37 #placeholder (loss as positive value)
                          
                          #remaining standstock end of timestep
                          V(network)$POC_g <- V(network)$POC_sStock_g - V(network)$POC_loss_g
                          
                          
                          #Based on Linear interp of Standing Stock (therefore no mass balance for this value), meant to compare to POC_loss_g which is based on litter input each time step
                          V(network)$POC_loss_g_ss <- V(network)$ss_POC * loss_percent_hr
                          

                          ##set up environment for next timestep
                          #YES ASSIGNING TO THE GLOBAL IS FROWNED ON _ WILL CHANGE TO DIFFERNT ONE BUT WORKS!!!!! 
                          assign("network_pre", network, envir = .GlobalEnv)
                          message(Sys.time())
                          return(network)
                          
                  })
          
          #TESTING#
          network_cols = colnames(as.data.frame(get.vertex.attribute(net_lst[[1]])))
          network_ts <- data.frame(matrix(ncol = length(network_cols), nrow = 0))
          
          
          # for(i in seq(1, 169, 24)){ #
          #   network_ts = rbind(network_ts, as.data.frame(get.vertex.attribute(net_lst[[i]])))
          # }
          
          ts_all <- data.frame(matrix(ncol = length(network_cols), nrow = 0))
          
          for(i in seq(1, length(net_lst)-1)){ # (remove one)
            ts_all = rbind(ts_all, as.data.frame(get.vertex.attribute(net_lst[[i]])))
          }
          
          #summarize breakdown and gw_DOC
          ts_all$date_d <- as.Date(ts_all$date)
          
          ts_day <- ts_all %>%
                                        group_by(date_d) %>%
                                        summarise(
                                          C_breakdown_g_ss = sum(POC_loss_g_ss, na.rm = TRUE),
                                          #C_breakdown_g_ss_tIV = sum(POC_loss_g_ss_tIV),
                                          C_breakdown_g = sum(POC_loss_g, na.rm = TRUE),
                                          C_gw_g = sum(DOC_local, na.rm = TRUE)
                                        )
          
          # Calculate standing stock projected loss 
          ts_day_id <- ts_all %>%
            group_by(date_d, name) %>%
            summarise(
              ss_avg = mean(ss_POC)
            ) %>% 
            ungroup() %>%
            group_by(date_d) %>%
            summarise(
              ss_sum = sum(ss_avg)
            )
          
          
          
          ##Note the "lengthup_m" is from the original GIS calculations, so is determined seperately from this code. 
          ##Therefore, the SLout and lengthup_m should be ~ the same. 
          
          # #
          # show_env <- solveMB(){
          #   list(ran.in = environment(), 
          #        parent = parent.env(environment()), 
          #        objects = ls.str(environment()))
          # }
          
          network_ts_all[[j]] <- as.data.frame(ts_all)
          network_ts_day[[j]] <- ts_day
          network_ts_day_id[[j]] <- ts_day_id
}





#Convert to gC
#48.4


saveRDS(network_ts_all, "network_ts_all_DEEP50.RDS")
saveRDS(network_ts_day, "network_ts_day_DEEP50.RDS")
saveRDS(network_ts_day_id, "network_ts_dayid_DEEP50.RDS")


network_ts_all_ss_DEEP50 <- do.call(rbind,  network_ts_all)

network_ts_day_id_DEEP50 <- do.call(rbind,  network_ts_day_id)

network_ts_day_ss_DEEP50 <- do.call(rbind,  network_ts_day) %>%
  mutate(month_dep = as.factor(month(date_d, label = TRUE))) %>%
  group_by(month_dep) %>%
  summarise(
    C_internal = mean(C_breakdown_g_ss),
    C_external = mean(C_gw_g))%>%
  pivot_longer(col = 2:3)

network_ts_day_ss_DEEP50$scen <- "DEEP50"
network_ts_day_ss_DEEP$scen <- "DEEP"
network_ts_day_ss_ATM$scen <- "ATM"
network_ts_day_ss_ATM50$scen <- "ATM50"

network_C.0 <- rbind(network_ts_day_ss_ATM, network_ts_day_ss_DEEP, network_ts_day_ss_ATM50, network_ts_day_ss_DEEP50)%>%
  mutate(value = if_else(name == "C_internal", value * 0.484, value))
  
network_C.1 <-network_C.0%>%# rbind(network_ts_day_ss_SHAL, network_ts_day_ss_DEEP, network_ts_day_ss_ATM, network_ts_day_ss_ATM2)%>%
  pivot_wider(names_from = scen, values_from = value) %>%
  mutate(diff_deep = DEEP - ATM,
         diff_deep50 = DEEP50 - ATM,
         diff_atm50 = ATM50 - ATM,
         diff_deep50_atm50 = DEEP50 - ATM50)

network_C.2 <- network_C.1 %>%
  dplyr::select( -"DEEP", -"ATM", -"DEEP50", -"ATM50")%>%#(-"SHAL", -"DEEP", -"ATM", -"ATM2")%>%
    dplyr::filter(name == "C_internal")%>%
  pivot_longer(cols = starts_with("diff"), names_to = "diff_fromATM") #, "diff_shal"

network_C.3 <- network_C.1 %>%
  dplyr::select(-3: -5, -starts_with("diff")) %>% #-"DEEP", -"ATM", -"ATM50", -"DEEP50")%>% #
  dplyr::filter(name == "C_external") %>%
  dplyr::rename(value = "DEEP50") %>%
  dplyr::mutate(diff_fromATM = "External") %>%
  rbind(., network_C.2)

ggplot(network_C.0) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), value, fill = name))+
  #scale_fill_manual(values=c("blue","#56B4E9"))+
  xlab("")+
  ylab("gC per day")+
  facet_wrap(~scen)


ggplot(network_C.2) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), value, fill = diff_fromATM), width=.5, position = "dodge")+
  scale_fill_manual(values=c("red", "#56B7E9", "blue", "green"))+
  xlab("")+
  ylab("gC per day")


ggplot(network_C.3) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), abs(value), fill = diff_fromATM), width=.5, position = "dodge")+
  scale_fill_manual(values=c("red", "#56B7E9", "blue", "green"))+
  xlab("")+
  ylab("gC per day")




