#####
## Comparing Scenarios
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)

#detach("package:plyr", unload=TRUE)

# Tsc_base <- readRDS("output/data/network_ts_all_intial_noserialC.RDS")
# Tsc_plus2 <- read.csv("output/data/network_ts_all_intial_plus2_noserialC.csv")
# Tsc_shalGW <- readRDS("output/data/network_ts_all_shalGW_noserialC.RDS") 
# Tsc_noGW <- readRDS("output/data/network_ts_all_lowGW_noserialC.RDS")
# Tsc_deepGW <- readRDS("output/data/network_ts_all_DeepGW_noserialC.RDS")

##Summarize by day
Tsc_shalGWt <- scenarios_temperature[[4]] %>%#readRDS("output/data/network_ts_day_shalGW_noserialC.RDS")%>%
  dplyr::select(date_d, name, TYPE, up.all, x, y, 
                stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2)%>%
  group_by(date_d) %>%
  summarise(
    tempC = mean(tempC),
    Q_outlet = max(Qout),
    C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484,
    C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
    C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
    C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
    C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
    C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
    WS_bentic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
  ) %>%
  mutate(scenario = "ShalGW",
         date_d = as.character(date_d))

Tsc_shalGWt40 <- scenarios_temperature[[5]] %>%#readRDS("output/data/network_ts_day_shalGW_noserialC.RDS")%>%
  dplyr::select(date_d, name, TYPE, up.all, x, y, 
                stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2)%>%
  group_by(date_d) %>%
  summarise(
    tempC = mean(tempC),
    Q_outlet = max(Qout),
    C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484,
    C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
    C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
    C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
    C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
    C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
    WS_bentic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
  ) %>%
  mutate(scenario = "ShalGW40",
         date_d = as.character(date_d))

Tsc_noGWt <- scenarios_temperature[[6]] %>%
  dplyr::select(date_d, name, TYPE, up.all, x, y, 
                stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2)%>%
  group_by(date_d) %>%
  summarise(
    tempC = mean(tempC),
    Q_outlet = max(Qout),
    C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484,
    C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
    C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
    C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
    C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
    C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
    WS_bentic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
  ) %>%
  mutate(scenario = "NoGW",
         date_d = as.character(date_d))

Tsc_noGWt5 <- scenarios_temperature[[7]] %>%
  dplyr::select(date_d, name, TYPE, up.all, x, y, 
                stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2)%>%
  group_by(date_d) %>%
  summarise(
    tempC = mean(tempC),
    Q_outlet = max(Qout),
    C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484,
    C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
    C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
    C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
    C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
    C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
    WS_bentic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
  ) %>%
  mutate(scenario = "NoGW5",
         date_d = as.character(date_d))

Tsc_deepGWt <- scenarios_temperature[[3]] %>%
  dplyr::select(date_d, name, TYPE, up.all, x, y, 
                stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2)%>%
  group_by(date_d) %>%
  summarise(
    tempC = mean(tempC),
    Q_outlet = max(Qout),
    C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484,
    C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
    C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
    C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
    C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
    C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
    WS_bentic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
  ) %>%
  mutate(scenario = "DeepGW",
         date_d = as.character(date_d))

Tsc_baset <- scenarios_temperature[[1]]%>%
  dplyr::select(date_d, name, TYPE, up.all, x, y, 
                stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2)%>%
  group_by(date_d) %>%
  summarise(
    tempC = mean(tempC),
    Q_outlet = max(Qout),
    C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484,
    C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
    C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
    C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
    C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
    C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
    WS_bentic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
  ) %>%
  mutate(scenario = "Base",
         date_d = as.character(date_d))

Tsc_plus2t <- scenarios_temperature[[2]] %>%
  dplyr::select(date_d, name, TYPE, up.all, x, y, 
                stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2)%>%
  group_by(date_d) %>%
  summarise(
    tempC = mean(tempC),
    Q_outlet = max(Qout),
    C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484,
    C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
    C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
    C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
    C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
    C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
    WS_bentic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
  ) %>%
  mutate(scenario = "Plus2",
         date_d = as.character(date_d))


#compare_sc <- rbind(Tsc_baset, Tsc_plus2t, Tsc_deepGWt, Tsc_shalGWt, Tsc_noGWt, Tsc_shalGWt40)%>%#, Tsc_noGWt5)%>%
compare_sc <- rbind(Tsc_baset, Tsc_deepGWt, Tsc_shalGWt, Tsc_noGWt, Tsc_shalGWt40)%>%#, Tsc_noGWt5)%>%
  mutate(date_d = as.Date(date_d, format = "%Y-%m-%d"),
         #scenario = fct_relevel(as.factor(scenario), "Base", "Plus2", "NoGW", "ShalGW", "DeepGW", "ShalGW40"),#, "NoGW5"),
         scenario = fct_relevel(as.factor(scenario), "Base", "NoGW", "ShalGW", "DeepGW", "ShalGW40"),#, "NoGW5"),
         Jdate = yday(date_d)
         )

ggplot(compare_sc)+
  geom_line(aes(Jdate, C_breakdownTQ_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios: POC Breakdown gC")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCBreakdowngC.png")

ggplot(compare_sc)+
  geom_line(aes(Jdate, C_breakdownTQ_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios: POC Breakdown gC")+
  theme_bw()+
  xlim(0, 365) + coord_polar()
ggsave("output/figures/TemperatureSc_POCBreakdowngC_radar.png")


ggplot(compare_sc)+
  geom_line(aes(Jdate, tempC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios")+
  theme_bw()
ggsave("output/figures/TemperatureSc_tempC.png")


ggplot(compare_sc)+
  geom_line(aes(Jdate, C_StStock_gC, group = scenario, color = scenario))+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios: Total Watershed POC StandingStock gC")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCStandingStock.png")


ggplot(compare_sc)+
  geom_line(aes(date_d, C_breakdownTQ_gC/C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios: POC Breakdown to POC Availability")+
  labs(y = "POC Breakdown: POC Available (gC)")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCavailability.png")

ggplot(compare_sc)+
  geom_point(aes(tempC, C_breakdownTQ_gC/C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green"))+
  ggtitle("Temperature Scenarios: POC Breakdown to POC Availability vs. Temperature")+
  labs(y = "POC Breakdown: POC Available (gC)")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCavailability_vs_TempC.png")

ggplot(compare_sc)+
  geom_point(aes(Q_outlet, C_breakdownTQ_gC/C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios: POC Breakdown to POC Availability vs. Temperature")+
  labs(y = "POC Breakdown: POC Available (gC)")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCavailability_vs_Qoutlet.png")


# ## Compare previous plot to observed standinf stock (multipled by ws bentic area)
# cpom_gm2 <- readRDS("data/cpom_gm2.RDS") %>%
#   mutate(date_d = as.Date(Jdate, format = "%j", origin = "12-31-2017") - years(4), #cant figure out why years is werid this fixes it now
#          cpom_gC = fit *0.484) #correct cpom measurement to gC per m2

# Add observed standing stock for comparison
compare_sc1 <- compare_sc %>%
  mutate(Jdate = yday(date_d)) %>%
  left_join(., as.data.frame(cbom_pred), by = "Jdate")%>%
  #left_join(., as.data.frame(cbom_df), by = "Jdate")%>%
  mutate(WS_observed_StStock_gC = cbom_gC_m2 * WS_bentic_area_m2)
  #mutate(WS_observed_StStock_gC = areal.afdm.gm2 * WS_bentic_area_m2)
# %>%
# left_join(., as.data.frame(cpom_gm2), by = "Jdate")%>%
#   mutate(WS_observed_StStock_gC_lscp = cpom_gC * WS_bentic_area_m2)

compare_sc1 %>%
  ggplot(.)+
  geom_point(aes(Jdate, cbom_gC_m2))#+
  #geom_point(aes(Jdate, cpom_gC), color = "red")

ggplot(compare_sc1)+
  geom_line(aes(Jdate, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  geom_line(aes(Jdate, WS_observed_StStock_gC), linetype = 2)+
  ggtitle("Temperature Scenarios: Total Watershed POC StandingStock gC")+
  theme_bw()+ xlim(0, 365) + coord_polar()
ggsave("output/figures/TemperatureSc_POCStandingStock_comparedtoObserved_radar.png")

ggplot(compare_sc1)+
  geom_line(aes(Jdate, C_StStock_gC, group = scenario, color = scenario))+
  #geom_line(aes(date_d, C_StStock_gC, group = scenario, color = scenario))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  geom_line(aes(Jdate, WS_observed_StStock_gC), linetype = 2)+
  ggtitle("Temperature Scenarios: Total Watershed POC StandingStock gC")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCStandingStock_comparedtoObserved.png")


Tsc_baset <- Tsc_baset %>%
  mutate(date_d = as.Date(date_d, format = "%Y-%m-%d"))

Tsc_noGWt <- Tsc_noGWt %>%
  mutate(date_d = as.Date(date_d, format = "%Y-%m-%d"))

## substract all from base
compare_2base <- compare_sc %>%
  full_join(., Tsc_baset, by = "date_d") %>%
  mutate(Diff_base_gC = C_breakdownTQ_gC.x - C_breakdownTQ_gC.y)


ggplot(compare_2base)+
  geom_line(aes(date_d, Diff_base_gC, group = scenario.x, color = scenario.x))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios: Compared to Observed Scenario")+
  theme_bw()+
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
ggsave("output/figures/TemperatureSc_POCBreakdowngC_compare2base.png")


ggplot(compare_2base)+
  geom_line(aes(Jdate, Diff_base_gC, group = scenario.x, color = scenario.x))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  ggtitle("Temperature Scenarios: Compared to Observed Scenario")+
  theme_bw()+
  xlim(0, 365) + coord_polar()
ggsave("output/figures/TemperatureSc_POCBreakdowngC_compare2base_radar.png")

## substract all from LowGW
compare_noGW<- compare_sc %>%
  full_join(., Tsc_noGWt, by = "date_d") %>%
  mutate(Diff_lowGW_gC = C_breakdownTQ_gC.x - C_breakdownTQ_gC.y)


ggplot(compare_noGW)+
  geom_line(aes(date_d, Diff_lowGW_gC, group = scenario.x, color = fct_relevel(scenario.x, "Base", "Plus2", "NoGW", "ShalGW", "DeepGW", "ShalGW40", "NoGW5")))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  labs(color = "Temperature Scenarios")+
  ggtitle("Temperature Scenarios: Compared to No GW Scenario")+
  theme_bw()+
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
ggsave("output/figures/TemperatureSc_POCBreakdowngC_compare2noGW.png")


ggplot(compare_noGW)+
  geom_line(aes(date_d, Diff_lowGW_gC, group = scenario.x, color = fct_relevel(scenario.x, "Base", "Plus2", "NoGW", "ShalGW", "DeepGW", "ShalGW40", "NoGW5")))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  geom_line(aes(x = date_d, y =  C_gw_gC.y,linetype = "GW Seep C Input"), color = "darkblue")+#, linetype = 3)+
  labs(color = "Temperature Scenarios", y = "Breakdown magnitude difference from NoGW Scenarios (gC)", linetype = NULL)+
  ggtitle("Temperature Scenarios: Compared to No GW Scenario")+
  theme_bw()+
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
ggsave("output/figures/TemperatureSc_POCBreakdowngC_compare2noGW_line.png")

ggplot(compare_noGW)+
  geom_line(aes(Jdate, Diff_lowGW_gC, group = scenario.x, color = fct_relevel(scenario.x, "Base", "Plus2", "NoGW", "ShalGW", "DeepGW", "ShalGW40", "NoGW5")))+
  scale_color_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
  #geom_line(aes(x = Jdate, y =  C_gw_gC.y,linetype = "GW Seep C Input"), color = "darkblue")+#, linetype = 3)+
  labs(color = "Temperature Scenarios", y = "Breakdown magnitude difference from NoGW Scenarios (gC)", linetype = NULL)+
  ggtitle("Temperature Scenarios: Compared to No GW Scenario")+
  theme_bw()+
  xlim(0, 365) + coord_polar()
ggsave("output/figures/TemperatureSc_POCBreakdowngC_compare2noGW_radar.png")




#### Monthly Comparison

compare_months <- compare_sc %>%#network_ts_day_df %>% #do.call(rbind,  network_ts_day_ss_DEEP50) %>%
  mutate(month_d = fct_relevel(as.factor(month(date_d, label = TRUE)), "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")) %>%
  dplyr::group_by(scenario, month_d) %>%
  dplyr::summarise(
    POCbreakdown_frag = mean(C_breakdownTQ_gC_frag),
    POCbreakdown_micrb = mean(C_breakdownTQ_gC_micrb),
    POCbreakdown = mean(C_breakdownTQ_gC),
    DOCseep = mean(C_gw_gC),
    POCin = mean(C_LitterIn_gC),
    Temp_avg = mean(tempC),
    BQ_avg_outlet = mean(Q_outlet))



ggplot(compare_months) +
  geom_col(aes(month_d, POCbreakdown, fill = scenario), width=.5, position = "dodge")+
              scale_fill_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "green", "purple"))+
             xlab("")+
             ylab("average gC per day for entire Watershed ")+
             ggtitle("Watershed Temperature Scenarios: POC Breakdown")+
              theme_bw()
ggsave("output/figures/TemperatureSc_POCBreakdowngC_month.png")


### Monthly differnce
##create annual dataframe
compare_annual <- compare_noGW %>%#network_ts_day_df %>% #do.call(rbind,  network_ts_day_ss_DEEP50) %>%
  mutate(month_d = fct_relevel(as.factor(month(date_d, label = TRUE)), "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"),
         scenario.x = fct_relevel(as.factor(scenario.x), "Base", "Plus2", "NoGW", "ShalGW", "DeepGW", "ShalGW40", "NoGW5")) %>%
  dplyr::filter(scenario.x != "NoGW") %>%
  dplyr::group_by(scenario.x) %>%
  dplyr::summarise(
    Difference_from_LowGW = mean(Diff_lowGW_gC),
    month_d = "annual")

compare_months_diff <- compare_noGW %>%#network_ts_day_df %>% #do.call(rbind,  network_ts_day_ss_DEEP50) %>%
  mutate(month_d = fct_relevel(as.factor(month(date_d, label = TRUE)), "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"),
         scenario.x = fct_relevel(as.factor(scenario.x), "Base", "Plus2", "NoGW", "ShalGW", "DeepGW" , "ShalGW40", "NoGW5")) %>%
  dplyr::filter(scenario.x != "NoGW") %>%
  dplyr::group_by(scenario.x, month_d) %>%
  summarise(
    Difference_from_LowGW = mean(Diff_lowGW_gC)) %>%
  dplyr::bind_rows(., compare_annual)%>%
  mutate(month_d = fct_relevel(month_d, "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "annual"))

ggplot(compare_months_diff) +
  geom_col(aes(month_d, Difference_from_LowGW , fill = scenario.x), width=.5, position = "dodge")+
  scale_fill_manual(values=c("black", "red", "#dbd624", "#2E8BC0", "green", "purple"))+
  xlab("")+
  ylab("average gC per day")+
  ggtitle("Watershed Temperature Scenarios: POC Breakdown compared to lowGW ")+
  theme_bw() 
ggsave("output/figures/TemperatureSc_POCBreakdowngC_month_comparedlowGW.png")  


ggplot(compare_months) +
  geom_col(aes(month_d, POCbreakdown_micrb, fill = scenario), width=.5, position = "dodge")+
  scale_fill_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "grey", "green"))+
  xlab("")+
  ylab("average gC per day")+
  ggtitle("Watershed Temperature Scenarios: POC Breakdown from Microbial")+
  theme_bw() 
ggsave("output/figures/TemperatureSc_POCBreakdowngC_Micb_month.png")  


ggplot(compare_months) +
  geom_col(aes(month_d, POCbreakdown_frag, fill = scenario), width=.5, position = "dodge")+
  scale_fill_manual(values=c("black", "red", "#FF00FF", "#dbd624", "#2E8BC0", "grey"))+
  xlab("")+
  ylab("average gC per day")+
  ggtitle("Watershed Temperature Scenarios: POC Breakdown from Frag")+
  theme_bw() 
ggsave("output/figures/TemperatureSc_POCBreakdowngC_Fag_month.png")  

compare_months %>%
  pivot_longer(., col= 5:6) %>%
  ggplot(.) +
  geom_col(aes(month_d, value, fill = name), width=.5, position = "dodge")+
  scale_fill_manual(values=c("blue", "#56B4E9"))+
  facet_wrap(~scenario)+
  xlab("")+
  ylab("average gC per day")+
  ggtitle("Watershed Temperature Scenarios: POC Breakdown and DOC from GW")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCandDOC_month.png")


compare_months <- compare_months %>% 
  mutate(month_d = factor(month_d, order = FALSE))
         
compare_noGW %>%#network_ts_day_df %>% #do.call(rbind,  network_ts_day_ss_DEEP50) %>%
  mutate(month_d = factor(month(date_d, label = TRUE), order = FALSE),
         scenario= fct_relevel(as.factor(scenario.x), "Base", "Plus2", "NoGW", "ShalGW", "DeepGW" , "ShalGW40", "NoGW5")) %>%
  dplyr::group_by(scenario, month_d) %>%
  dplyr::summarise(
    Difference_from_LowGW = mean(Diff_lowGW_gC)) %>% 
  ungroup() %>%
  full_join(., compare_months) %>% #by = c(scenario = "scenario.x", "month_d")) %>%
  dplyr::filter(scenario != "NoGW")%>%
  pivot_longer(., col= c(3,7)) %>%
  ggplot(.) +
  geom_col(aes(fct_relevel(month_d, "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep"), value, fill = name), width=.5, position = "dodge")+
  scale_fill_manual(values=c("#56B4E9", "blue"))+
  facet_wrap(~scenario)+
  xlab("")+
  ylab("average gC per day")+
  ggtitle("Watershed Temperature Scenarios: POC Breakdown Difference and DOC from GW")+
  theme_bw()
ggsave("output/figures/TemperatureSc_POCdifffromlowGWandDOC_month.png")


ggplot(compare_months) +
  geom_col(aes(fct_relevel(month_d, value, fill = name), width=.5, position = "dodge"))+
  scale_fill_manual(values=c("blue", "#56B4E9", "brown"))+
  xlab("")+
  ylab("average gC per day")+
  ggtitle("Landscape Observed Temperature - plus2")
