##### plotting testing
library(ggplot2)
library(tidyverse)
theme_set(theme_bw)



network_C.0 <- rbind(network_ts_day_ss_ATM, network_ts_day_ss_DEEP, network_ts_day_ss_ATM50, network_ts_day_ss_DEEP50)%>%
  mutate(value = if_else(name == "C_internal", value * 0.484, value)) #correct ADFM to gC

network_C.1 <-network_C.0%>%# rbind(network_ts_day_ss_SHAL, network_ts_day_ss_DEEP, network_ts_day_ss_ATM, network_ts_day_ss_ATM2)%>%
  pivot_wider(names_from = scen, values_from = value) %>%
  mutate(diff_deep_atm = DEEP - ATM,
         diff_atm50 = ATM50 - ATM,
         diff_deep50_atm50 = DEEP50 - ATM50,
         diff_deep50 = DEEP50 - DEEP)

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

network_C.0 %>%
  dplyr::filter(scen == "ATM" | scen == "DEEP")%>%
  ggplot(.) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), value, fill = name), width=.5, position = "dodge")+
  scale_fill_manual(values=c( "blue", "#56B7E9"))+
  xlab("")+
  ylab("gC per day")+
  facet_wrap(~scen, nrow = 2)

#figure compare 1
network_C.2 %>%
  dplyr::filter(diff_fromATM == "diff_deep_atm")%>%
ggplot(.) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), value, fill = diff_fromATM), width=.5, position = "dodge")+
  scale_fill_manual(values=c("#6c1150", "red", "pink", "#56B7E9", "blue", "green"))+
  xlab("")+
  ylab("gC per day")

#figure compare 2 temporal
network_C.2 %>%
  dplyr::filter(diff_fromATM == "diff_atm50" | diff_fromATM =="diff_deep50")%>%
  ggplot(.) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), value, fill = diff_fromATM), width=.5, position = "dodge")+
  scale_fill_manual(values=c("red", "pink", "#56B7E9", "blue", "green"))+
  xlab("")+
  ylab("gC per day")

#figure compare 3
network_C.2 %>%
  dplyr::filter(diff_fromATM == "diff_deep_atm" | diff_fromATM == "diff_deep50_atm50")%>%
  ggplot(.) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), value, fill = diff_fromATM), width=.5, position = "dodge")+
  scale_fill_manual(values=c("#6c1150", "orange"))+
  xlab("")+
  ylab("gC per day")


#figure 1a external only
network_C.3 %>%
  dplyr::filter( diff_fromATM == "External")%>%
ggplot(.) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), abs(value), fill = diff_fromATM), width=.5, position = "dodge")+
  scale_fill_manual(values=c( "blue"))+
  xlab("")+
  ylab("gC per day")

#figur 1b compare external and temp dependance
network_C.3 %>%
  dplyr::filter(diff_fromATM == "diff_deep_atm" | diff_fromATM == "External")%>%
  ggplot(.) +
  geom_col(aes(fct_relevel(month_dep, "Nov", "Feb", "May", "Aug"), abs(value), fill = diff_fromATM), width=.5, position = "dodge")+
  scale_fill_manual(values=c("#6c1150", "blue"))+
  xlab("")+
  ylab("gC per day")



as.data.frame(network_C.all) %>%
  group_by(scen, date_d)%>%
  summarize(temp = mean(k))%>%
  ggplot(.)+
  geom_point(aes(date_d, temp, color = scen))


as.data.frame(network_C.all) %>%
  group_by(scen, date_d)%>%
  summarize(temp = mean(tempC))%>%
  ggplot(.)+
  #geom_point(aes(date_d, temp, color = scen))+
  geom_smooth(aes(date_d, temp, color = scen),se = FALSE)+
  xlab("Month")+
  ylab("Stream Temperature (C)")



test10 <- data.frame(temp_Dmed = rep(seq(1:20) ,4), season = c(rep(as.factor(1), 20), rep(as.factor(2), 20), rep(as.factor(3), 20), rep(as.factor(4), 20)))
pred_k<- as.data.frame(posterior_predict(tempM.glm.gamma, test10, type = "response", draws = 50))


test10$pred_k <- apply(pred_k, 2, median)

test10$season <- dplyr::recode(test10$season, "1" = 'Winter (JFM)', 
                        "2" = 'Spring (AMJ)',
                        "3" = 'Summer (JAS)',
                        "4" = 'Fall (OND)')

ggplot(test10)+
  geom_smooth(aes(x = temp_Dmed, y = pred_k, group = season, color = season))+
  scale_color_manual(values=c("black", "green", "blue", "yellow"))+
  xlab("Median Temperature (C)")+
  ylab("k (day-1)")



###CPOM plotting
cpom %>%
  dplyr::filter(om.category =="LEAF")%>%
ggplot()+
  geom_point(aes(Jdate, cbom.afdm.g.m2))


  
### Messing around lambda
ggplot(df1, aes(x = temp_Dmed, y = mean_k_coarse, group = as.factor(stream), colour = as.factor(stream)))+
  geom_point()+
  stat_smooth(method = "lm", se = FALSE)+
  facet_wrap(~season)

df2 <- df1 %>%
  mutate(mean_k_c_sc = scale(mean_k_coarse),
         temp_Dmed_sc = scale(temp_Dmed),
         Jdate = yday(date))

## add in standing stock, used the cpom_gm2 from Analysis_eg.R
df2 <- df2 %>%
  left_join(., cpom_gm2)

ggplot(df2)+
  geom_point(aes(x = Jdate, y = mean_k_c_sc))+
  geom_smooth(aes(x = Jdate, y = mean_k_c_sc), col = "red")+
  geom_point((aes(x = Jdate, y = temp_Dmed_sc)))+
  geom_smooth(aes(x = Jdate, y = temp_Dmed_sc), col = "blue")+
  facet_wrap(~as.factor(stream))

ggplot(df2)+
  geom_point(aes(mean_k_coarse, cpom))

###where/when are rates the highest
ggplot(df2)+
  geom_violin(aes(x = as.factor(stream), y = mean_k_coarse))

ggplot(df2)+
  geom_violin(aes(x = as.factor(month_s), y = mean_k_coarse))
## Why are the lowest breakdown rates in the fall? 

ggplot(df2)+
  geom_violin(aes(x = as.factor(month_s), y = mean_k_fine))

