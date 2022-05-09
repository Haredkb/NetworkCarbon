
##############################
### Temperature Dependance ###
##############################

library(rstan)
library(rstanarm)
library(tidyverse)
library(lubridate)
library(zoo)#roling

###read deocmposiition raw data
k_Year1 <- read.csv("data/si_k_year1.csv")
k_Year2 <- read.csv("data/si_k_year2.csv")

k_df <- rbind(k_Year1, k_Year2)
rm(k_Year1, k_Year2) #clean up

k_df <- k_df %>%
  mutate(date_s = as.Date(date_dep, format = "%Y-%m-%d"),
         date_e = as.Date(date_coll, format = "%Y-%m-%d"),
         days_dep = as.numeric(difftime(date_e, date_s, units = "days"))) %>%
  dplyr::filter(rhodo_acer == "A") %>%
  dplyr::select(mean_k_coarse, sd_k_coarse, mean_k_fine, sd_k_fine, date_s, date_e, days_dep, rhodo_acer, stream) #clean up

###read temperature raw data
tempC_1 <- read.csv("G:/My Drive/CREWS_official/200_Landscape/210_data_landscape/212_physical/Landscape_temperature/Temperature_all_Serial_Incubations/Master long files/landscape_dailytemp_masterlong_SIyr1.csv")
tempC_2 <- read.csv("G:/My Drive/CREWS_official/200_Landscape/210_data_landscape/212_physical/Landscape_temperature/Temperature_all_Serial_Incubations/Master long files/landscape_dailytemp_masterlong_SIyr2.csv")
temp_C <- rbind(tempC_1, tempC_2)
rm(tempC_1, tempC_2) #clean up

temp_C$date <- as.Date(temp_C$date, "%m/%d/%y")
temp_C$X <- NULL

### Create one df

df <- left_join(temp_C, k_df, by = c(date = "date_s", "stream" = "stream"))%>%
  mutate(ID = as.factor(stream))

df1 <- df %>%
  dplyr::arrange(desc(ID)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(year = as.factor(year(date)),
                month_s = month(date),
                season = as.factor(quarter(date, fiscal_start = 1)),
                temp_Davg = zoo::rollmean(temp.celsius, k = 51, fill = NA, align = "left"),  #deployment average, wanted to do each with correct deployment days, but 51 is the mimium and the last days have the lowest decomosition control so using 51for all
                temp_Dmed = zoo::rollapply(temp.celsius, width=51, quantile, probs=0.5, fill=NA),
                temp_D75 = zoo::rollapply(temp.celsius, width=51, quantile, probs=0.75, fill=NA),
                temp_D25 = zoo::rollapply(temp.celsius, width=51, quantile, probs=0.25, fill=NA),
                temp_DD = zoo::rollapply(temp.celsius, width=51, sum, fill=NA),
                temp_DD10 = zoo::rollapply(temp.celsius, width=51, sum, fill=NA)/10) %>%

  dplyr::ungroup() %>%
  na.omit #remove all days not associated with decompisition 


library(bayesplot)
tempM.glm.gamma <- stan_glm(mean_k_coarse ~ temp_Dmed:season, data = df1, family = Gamma)
tempM.glm <- stan_glm(mean_k_coarse ~ temp_Dmed:season, data = df1)

loo_compare(loo(tempM.glm), loo(tempM.glm.gamma))
#gamma is better
saveRDS(tempM.glm.gamma, "tempM_glm_gamma.RDS")
test10 <- data.frame(temp_Dmed = 10, season = as.factor(1))
predict(tempM.glm.gamma, new = test10, type = "response")
posterior_predict(tempM.glm.gamma, new = test10, type = "response")

predictive_interval(tempM.glm.gamma)


summary(tempM.glm.gamma)
stan_trace(tempM.glm)
mcmc_areas(tempM.glm.gamma)
coef(tempM.glm)
c = as.numeric(coef(tempM.glm)[2])

color_scheme_set("red")
ppc_dens_overlay(y = tempM.glm.gamma$y,
                 yrep = posterior_predict(tempM.glm.gamma, draws = 50))

ppc_intervals_grouped(
  y = df1$mean_k_coarse,
  yrep = posterior_predict(tempM.glm.gamma),
  x = df1$temp_Dmed,
  group = df1$season,
  prob = 0.5
) +
  labs(
    x = "Median Temperature (C)",
    y = "k coarse (day-1)",
    title = "Temperature Dependancy",
    subtitle = "Coweeta Data"
  ) +
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white")


# #dm/dt = -ktem* exp(c * (Temp - TempR)) * m
# 
# ktem = as.numeric(coef(tempM.glm)[1])
# dm <- -ktem * exp(c * 10)



#read in decomposition
decom_rw <- read.csv("data/CREWS_SI_LB _2Dec2019_coarsedata.csv", skip = 1, skipNul = TRUE) %>%
  dplyr::mutate(Date.deployed = as.Date(Date.deployed, "%m/%d/%Y"),
                Date.Collected = as.Date(Date.Collected, "%m/%d/%Y"),
                month_s = month(Date.deployed)) %>% 
  tidyr::drop_na(Date.Collected, Final.AFDM) %>%
  dplyr::mutate(days_dep = Date.Collected - Date.deployed,
                percent_remain = Final.AFDM/Starting.AFDM,
                percent_remain_ln = log(Final.AFDM/Starting.AFDM))

#https://www.researchgate.net/post/How-to-calculate-litter-decomposition-rates-k-value
decom_rw$k = -log(decom_rw$Final.AFDM/decom_rw$Starting.AFDM)/as.numeric(decom_rw$days_dep)
decom_rw <- decom_rw[is.finite(decom_rw$k) & decom_rw$k > 0,] %>%
  dplyr::select(Rhodo.Acer, Date.deployed, Site, k)%>%
  group_by(Site, Rhodo.Acer, Date.deployed)%>%
  dplyr::summarise(avg_k = mean(k))

ggplot(decom_rw)+
  geom_smooth(aes(Date.deployed, avg_k, color = Site))+
  facet_wrap(~Rhodo.Acer)+
  theme_bw()

df_DD <- decom_rw %>%
  dplyr::filter(Rhodo.Acer == "A")%>%
  dplyr::mutate(Date.deployed = as.Date(Date.deployed, format = "%Y-%m-%d"))%>%
  dplyr::left_join(df, ., by = c("date"= "Date.deployed", "stream" = "Site"))%>% 
  dplyr::select(-Rhodo.Acer) %>%
  dplyr::arrange(desc(ID)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(year = as.factor(year(date)),
                temp_Davg = zoo::rollmean(temp.celsius, k = 51, fill = NA, align = "left"),  #deployment average, wanted to do each with correct deployment days, but 51 is the mimium and the last days have the lowest decomosition control so using 51for all
                temp_Dmed = zoo::rollapply(temp.celsius, width=51, quantile, probs=0.5, fill=NA),
                temp_D75 = zoo::rollapply(temp.celsius, width=51, quantile, probs=0.75, fill=NA),
                temp_D25 = zoo::rollapply(temp.celsius, width=51, quantile, probs=0.25, fill=NA),
                temp_DD = zoo::rollapply(temp.celsius, width=51, sum, fill=NA),
                temp_DD10 = zoo::rollapply(temp.celsius, width=51, sum, fill=NA)/10) %>%
  
  dplyr::ungroup() %>%
  na.omit


tempDD.glm <- stan_glm(percent_remain_ln ~ temp_DD10, data = df_DD)

coef(tempDD.glm)
lam_DD <- as.numeric(coef(tempDD.glm)[2])

ppc_intervals(
  y = df_DD$percent_remain_ln,
  yrep = posterior_predict(tempDD.glm),
  x = df_DD$temp_DD10,
  prob = 0.5
) +
  labs(
    x = "Median Temperature (C)",
    y = "k coarse (day-1)",
    title = "Temperature Dependancy",
    subtitle = "Coweeta Data"
  ) +
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white")



