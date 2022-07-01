################################
### Create base flow interpretation ####
####################################

###Updated 2022/06/29###

library(sf)
library(sp)
library(tidyverse)
library(timetk)
library(baytrends) #for fillMissing
library(smwrBase)
library(DVstats)

#Read input files
#landscape data
# flow is in liters per second
df_bq <- read.csv("data/landscape_discharge_all_dailymeans09012017_12312019_updated01March2021_long.csv", as.is = TRUE) %>%
  mutate(stream = as.factor(toupper(variable)), #make all 
         variable = NULL,
         date = as.Date(date2, format = "%Y-%m-%d"),
         date2 = NULL,
         flow_ls = value, 
         value = NULL,
         X = NULL)
#read in ws55 data
df_exp <- read.csv("data/ws55_stage_all_7July2020.csv", as.is = TRUE)%>%
  dplyr::mutate(flow_ls = discharge_power_stagecorrected,
                date = as.Date(datetime),
                stream = as.factor("WS55")
                ) %>%
  group_by(stream) %>%
  summarise_by_time(
    .date_var = date,
    .by       = "day", # Setup for daily aggregation
    # Summarization
    flow_ls  = mean(flow_ls)
  )%>%
  dplyr::select(c("date","flow_ls", "stream"))

#Read in TOWR data
df_bq <- read.csv("data/towrstage_all_27April2020.csv", as.is = TRUE)%>%
  dplyr::mutate(flow_ls = discharge_power_stagecorrected,
                date = as.Date(datetime),
                stream = as.factor("TOWR")
  ) %>%
  group_by(stream) %>%
  summarise_by_time(
    .date_var = date,
    .by       = "day", # Setup for daily aggregation
    # Summarization
    flow_ls  = mean(flow_ls)
  )%>%
  dplyr::select(c("date","flow_ls", "stream")) %>%
  rbind(., df_exp) %>%
  rbind(., df_bq)%>%
  drop_na()


### Add Watershed ID and details
sites <- sf::st_read('C:/Users/hared/Dropbox/UConn/Projects/500_NetworkScaleCarbon/540_QGISProjects/CUASHI_DEM/LandscapePoints_network_v2.shp')
SHP_Fork_Basin <- sf::st_read("C:/Users/hared/OneDrive/Documents/CWT_BasinConstruct.shp")
SHP_Fork_Basin <- st_transform(SHP_Fork_Basin, crs = st_crs(sites))
sites_BF <- st_join(SHP_Fork_Basin, sites)
sites_BF$basin_id[sites_BF$layer == "WS_WS09"] <- "ShopeFork" #"CoweetaCreek" #fix lower watershed
sites_BF$basin_id[sites_BF$layer == "WS_WS08"] <- "BallCreek"


### SET UP DATAFRAMES###
#List Site Names to work through
station_list <- unique(df_bq$stream)

#Create data list for outputvalues
datalist = list()

#Output Q df
Q_df <- data.frame(matrix(ncol = 4, nrow = 0))


###############################################
######## Create Baseflow Dataframe############
##############################################

# Make a dataframe with continuous dates

#2017-12-15 : 2018-03-26'Removing Pre April 2018 TO PERFORM BASEFLOW ON ALL DATA THE SAME (DATAGAPS BEFORE)
df_bq <- df_bq %>% filter(date >= "2018-04-01" & date >= "2018-04-01" )

df_bq_l <- lapply(station_list, function(id){
  print(id)
  df <- dplyr::filter(df_bq, stream == id)
  
  hhfile <- try(
    hh<- data.frame(date=seq(as.Date(min(df$date)), 
                             as.Date(max(df$date)), 
                             by="days")))
  
  df <- left_join(hh,df)
  df$flowFill <- fillMissing(df$flow_ls, max.fill = 45) #> 14 only for TOWR  
  # 
  df <- df %>%
    drop_na(flowFill)
  
  #Perform BFI calculation with bfi
  try(
    df_bfi <- with(df, 
                   bfi(flowFill, date, 
                       by="continuous", 
                       STAID= stream))
    
    
  )
  
  df_bfi <- df_bfi %>%
    dplyr::mutate(stream = id)
  
})

##
nodes <- readRDS("nodes_cwt.RDS")


#join all the metrics together 
df_bfi <-  do.call(rbind, df_bq_l) %>%
  mutate(flow_m3hr = Flow * (60 * 60 / 1000),
         baseq_m3hr = BaseQ* (60 * 60 / 1000)) %>% #convert ls to m3hr
drop_na()%>%
  base::merge(., sites_BF, by.x = "stream", by.y = "stream.y", all.x = TRUE)%>%### Join with basin ###
  mutate(WS_ID = ifelse(is.na(WS_ID) == TRUE, ifelse(stream == "TOWR", "BallCreek", "ShopeFork"), WS_ID), #ws55 and towr are out of bounds
         Jdate = yday(Dates),
         basin_id = NULL) %>%
  merge(., nodes, by.x = "stream", by.y = as.factor("stream"))
  
  

####################

#create linear relationship between length and 
# create a dataframe that is grouped by date and basin
by_date <- df_bfi %>%
  group_by(Jdate, basin_id)


## outputs slope, intercept, r2, p 
Q_JDate_lm_m3hr <- do(by_date,
                            tidy( #tidy #glance #augment
                              lm(baseq_m3hr ~ lengthup_m + 0, data =.))) #control intercept at 0


saveRDS(Q_JDate_lm_m3hr, "data/Q_JDate_lm_m3hr.RDS")













