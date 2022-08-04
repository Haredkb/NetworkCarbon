#
library(tidyverse)
library(lubridate)
library(zoo)

llsc <- read.csv("G:/My Drive/CREWS_official/300_wholestream/310_data_wholestream/321_organicmatterbudget/paired.llsc.data_processed.csv") %>%
  mutate(stream = ifelse(stream == "ref", "WS55", "TOWR"),
         date = as.Date(sample.date, "%Y-%m-%d"),
         OM_Type = ifelse(category== "leaves", "LEAF", category),
         tech = "UGA") %>%
  dplyr::select(date, stream, OM_Type, areal.afdm.gm2, sample.mo, tech)

cbom <- read.csv("G:/My Drive/CREWS_official/300_wholestream/310_data_wholestream/321_organicmatterbudget/monthly_core_coarse_OM.csv") %>%
  mutate(areal.afdm.gm2 = (Dry_Mass - Ashed)/0.049,
         stream = Stream,
         date = as.Date(Sample_Date, "%m/%d/%Y"),
         sample.mo = month(date),
         tech = "UA") %>%
  dplyr::select(date, stream, OM_Type, areal.afdm.gm2, sample.mo, tech)

  
cbom_df <- rbind(llsc, cbom)%>%
  dplyr::filter(OM_Type == "LEAF") %>%
  mutate(Jdate = yday(date),
         areal.afdm.gm2 = ifelse(areal.afdm.gm2 < 0, NA, areal.afdm.gm2)) %>%
  drop_na() %>%
  group_by(sample.mo, stream) %>%
  summarise(
    areal.afdm.gm2 = mean(areal.afdm.gm2)
  )


ggplot(cbom_df) +
  geom_boxplot(aes(x = as.factor(sample.mo), y = areal.afdm.gm2))

#predict for everyday of the year
library(mgcv)
#cbom_signal <- gam(areal.afdm.gm2 ~ s(Jdate), data = cbom_df)
library(splines2)
#t <- seq(0, 365, length.out = 800)

cbom_annual_fit <- lm(areal.afdm.gm2 ~ mSpline(x = Jdate, 
                                        df = 4, 
                                        periodic = TRUE, 
                                        Boundary.knots = c(1, 365)), data = cbom_df) #like this better than gam to fit the periodic nature of the signal
cbom_year <- data.frame(Jdate = seq(1:365))
cbom_pred <- data.frame(Jdate = as.numeric(seq(1:365)), cbom_AFDM_gm2 = predict(cbom_annual_fit, cbom_year)) %>%
  mutate(
    cbom_gC_m2 = cbom_AFDM_gm2 * 0.484
  )
plot(cbom_pred[,c(1,3)])

ggplot(cbom_df) +
  geom_point(aes(x = Jdate, y = areal.afdm.gm2))+
  geom_point(data = cbom_pred, aes(x = Jdate, y = cbom_AFDM_gm2))






### add landscape cpom
cbom_lscp <- read.csv("G:/My Drive/CREWS_official/200_Landscape/210_data_landscape/211_organicmatter/Landscape_cbom/landscape_cbom_calculations_master_31Oct2020.csv") %>%
  mutate(date = as.Date(sample.date, format = "%m/%d/%Y"),
         Jdate = yday(date),
         tech = "UGA",
         OM_Type = om.category,
         areal.afdm.gm2 = cbom.afdm.g.m2) %>%
  dplyr::select(Jdate, stream, OM_Type, areal.afdm.gm2, sample.mo, tech)%>%
  dplyr::filter(OM_Type == "LEAF") %>%
  group_by(stream, sample.mo) %>%
  summarise(areal.afdm.gm2 = mean(areal.afdm.gm2)) #,
  #           cbom_gC_m2 = areal.afdm.gm2 * 0.484)%>%
  # ungroup()#%>%
  # dplyr::select(-stream)

#cbom_full <- rbind(cbom_lscp, cbom_pred) #using the prediction to inform new model
cbom_df1  <-  cbom_df %>%
  mutate(Jdate = (sample.mo) * 30 - 15)#,
         #areal.afdm.gm2 = areal.afdm.gm2)#%>%
  # ungroup()%>%
  # dplyr::select(-sample.mo)

cbom_full <- rbind(cbom_lscp, cbom_df)%>%
  mutate(Jdate = (sample.mo) * 30 - 15) # this approach uses the monthly averages as data points


plot(cbom_full[,c(4,3)])
# cbom_df <- rbind(llsc, cbom, cbom_lscp)%>%
#   dplyr::filter(OM_Type == "LEAF") %>%
#   mutate(Jdate = yday(date)) %>%
#   group_by(stream, Jdate) %>%
#   summarise(areal.afdm.gm2 = mean(areal.afdm.gm2))%>%
#   mutate(areal.afdm.gm2 = ifelse(areal.afdm.gm2 < 0, NA, areal.afdm.gm2),
#          cbom_gC_m2 = areal.afdm.gm2 * 0.484)

cbom_annual_fit <- lm(areal.afdm.gm2 ~ mSpline(x = Jdate, 
                                               df = 4, 
                                               periodic = TRUE, 
                                               Boundary.knots = c(1, 365)), data = cbom_full) #like this better than gam to fit the periodic nature of the signal
cbom_year <- data.frame(Jdate = seq(1:365))
cbom_pred <- data.frame(Jdate = as.numeric(seq(1:365)), cbom_AFDM_gm2 = predict(cbom_annual_fit, cbom_year)) %>%
  mutate(
    cbom_gC_m2 = cbom_AFDM_gm2 * 0.484
  )
plot(cbom_pred[,c(1,3)])


ggplot(cbom_full) +
  geom_point(aes(x = Jdate, y = areal.afdm.gm2))+
  geom_point(data = cbom_pred, aes(x = Jdate, y = cbom_AFDM_gm2), color = "red")

##############################################################################3
#### C IN ####
###########################################################
#### Make the cin daily time stpes from monthly
ClocalLit_AFDMg <- readRDS("data/ClocalLit_g.RDS")
PL_day <- readRDS("./data/POM_lateral_day.RDS")
PD_day <- readRDS("./data/POM_direct_day.RDS")

cIn_direct_annual_fit <- lm(direct_gm2d_avg ~ mSpline(x = Jdate, 
                                               df = 4, 
                                               periodic = TRUE, 
                                               Boundary.knots = c(1, 365)), data = PD_day) #like this better than gam to fit the periodic nature of the signal
cbom_year <- data.frame(Jdate = seq(1:365))
cIN_direct_pred <- data.frame(Jdate = as.numeric(seq(1:365)), cbom_AFDM_gm2 = predict(cIn_direct_annual_fit, cbom_year))

plot(cIN_direct_pred[,c(1,2)])

cIn_lateral_annual_fit <- lm(lateral_gmd_avg ~ mSpline(x = Jdate, 
                                                      df = 4, 
                                                      periodic = TRUE, 
                                                      Boundary.knots = c(1, 365)), data = PL_day) #like this better than gam to fit the periodic nature of the signal
cbom_year <- data.frame(Jdate = seq(1:365))
cIN_lateral_pred <- data.frame(Jdate = as.numeric(seq(1:365)), cbom_AFDM_gmd = predict(cIn_lateral_annual_fit, cbom_year))

plot(cIN_lateral_pred[,c(1,2)])

cIn <- cbind(cIN_direct_pred, cIN_lateral_pred) %>%
  dplyr::select(-3)%>% #remove extra jDATE
  mutate(Cdirect_gm2hr = if_else(cbom_AFDM_gm2/24 < 0, 0, cbom_AFDM_gm2/24) ,
         Clateral_gmhr = if_else(cbom_AFDM_gmd/24 < 0, 0, cbom_AFDM_gmd/24),
         Cin_gm2d = cbom_AFDM_gm2 + (cbom_AFDM_gmd *2))


saveRDS(cIn, "data/POM_In_pred.RDS")

dates <- data.frame(Jdate = seq(1,365))

cIn_Obs <- left_join(PD_day, PL_day, by = "Jdate") %>%
  mutate(Cin_gm2d = (lateral_gmd_avg * 2) + direct_gm2d_avg)

cIn_rollmean <- left_join(PD_day, PL_day, by = "Jdate") %>%
  mutate(Cin_gm2d = (lateral_gmd_avg * 2) + direct_gm2d_avg)%>%
  left_join(dates, .) %>%
  mutate( ##rule 2 to extrapolate as a constant value of the nearest extreme
         Cin_gm2d_mean = rollapply(Cin_gm2d, width = 7, FUN= mean, partial = TRUE, na.rm = TRUE ),
         Cin_gm2d_all_int = zoo::na.approx(Cin_gm2d_mean, rule =2),
         Cin_gm2d_all = zoo::rollmean(Cin_gm2d_all_int, k = 7, na.pad = TRUE, align = "left"), #na at the end 
         Cin_gm2d_all = if_else(is.na(Cin_gm2d_all) == TRUE, (3.2 + 1.1)/2 , Cin_gm2d_all), #if na avaerage last december and first jan values
         Cin_gm2hr_all = Cin_gm2d_all/24)

saveRDS(cIn_rollmean, "data/POM_In.RDS")

ggplot()+
  geom_line(data = cIn, aes(x = Jdate, y = Cin_gm2d))+
  geom_point(data = cIn_rollmean, aes(Jdate, Cin_gm2d_mean))+
  geom_point(data = cIn_rollmean, aes(Jdate, Cin_gm2d_all), color = "blue")+
  geom_point(data = cIn_Obs, aes(Jdate, Cin_gm2d), color = "red")

