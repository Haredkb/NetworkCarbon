
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
         Jdate_s = format.Date(date_s, "%y"),
         date_e = as.Date(date_coll, format = "%Y-%m-%d"),
         days_dep = as.numeric(difftime(date_e, date_s, units = "days"))) %>%
  dplyr::filter(rhodo_acer == "A") %>%
  dplyr::select(mean_k_coarse, sd_k_coarse, mean_k_fine, sd_k_fine, mean_k_shred_new, date_s, date_e, days_dep, rhodo_acer, stream) #clean up

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

### Calculate Lambda M and Lambda F Lecerf_2017 
### Pathway Specfic Decay Rates 
df1 <- df1%>%
mutate(
        #lamM_01 = mean_k_fine * exp((mean_k_coarse - mean_k_fine)*1), #day 1
#       lamM_30 = mean_k_fine * exp((mean_k_coarse - mean_k_fine)*30),
#        lamF_01 = mean_k_coarse - mean_k_fine * exp((mean_k_coarse - mean_k_fine)*1),
#        lamF_30 = mean_k_coarse - mean_k_fine * exp((mean_k_coarse - mean_k_fine)*30),
       lamF_avg =  mean_k_coarse - ((mean_k_fine - mean_k_coarse)/(log(mean_k_fine) - log(mean_k_coarse))), #recalculation of mean_Shread_new using the avg of multiple sites
       # kc_lam_01  = lamM_01 + lamF_01,
       # kc_lam_30 = lamM_30 + lamF_30, #day 30 equation 8 from Lecerf_2017
       kc_lamM = mean_k_coarse - mean_k_shred_new,
       kc_lamF = mean_k_shred_new) #lambda Microbial based on avg Fragmentaion over deployment

ggplot(df1)+
  geom_point(aes(x = mean_k_coarse, y = kc_lamM))+
  geom_point(aes(x = mean_k_coarse, y = kc_lamF))

# ## examples 
# test <- data.frame(x = 1:206)
# test$day1_lamM <- (100 * df1$lamM_01) #loss due to microbial
# test$day1_lamF <- (100 * df1$lamF_01) #loss due to fragmentation 
# test$day1_lamTot <- (100 * df1$lamM_01) + (100 * df1$lamF_01)
# test$day1_kc <- 100* df1$mean_k_coarse


                                      

library(bayesplot)
#### GLM for Coarse OM Breakdown
tempM.glm.gamma <- stan_glm(mean_k_coarse ~ temp_Dmed:season, data = df1, family = Gamma)
tempM.glm <- stan_glm(mean_k_coarse ~ temp_Dmed:season, data = df1)
tempM.glmer.gamma <- stan_glmer(mean_k_coarse ~ temp_Dmed:season + (1|ID), data = df1, family = Gamma)
#### GLM for measured fine OM breakdown
tempM_fine_glmer.gamma <- stan_glmer(mean_k_fine ~ temp_Dmed:season + (1|ID), data = df1, family = Gamma)
#### GLM for lambda value for fragmentation 
lamF.glm <- stan_glm(kc_lamF ~ temp_Dmed:season, data = df1)
lamM.glm <- stan_glm(kc_lamM ~ temp_Dmed:season, data = df1)

summary(lamF.glm)
mcmc_areas((lamF.glm))
mcmc_areas((lamM.glm))
coef(lamF.glm)




summary(tempM_fine_glmer.gamma)
stan_trace(tempM_fine_glmer.gamma)
mcmc_areas(tempM_fine_glmer.gamma)
coef(tempM_fine_glmer.gamma)
color_scheme_set("red")
ppc_dens_overlay(y = tempM_fine_glmer.gamma$y,
                 yrep = posterior_predict(tempM_fine_glmer.gamma, draws = 50))

predictive_interval(tempM_fine_glmer.gamma)

ppc_intervals_grouped(
  y = df1$mean_k_fine,
  yrep = posterior_predict(tempM_fine_glmer.gamma),
  x = df1$temp_Dmed,
  group = df1$season,
  prob = 0.9
) +
  labs(
    x = "Median Temperature (C)",
    y = "k coarse (day-1)",
    title = "Temperature Dependancy",
    subtitle = "Coweeta Data"
  ) +
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white")

#################
#Comparing results


loo_compare(loo(tempM.glm), loo(tempM.glm.gamma), loo(tempM.glmer.gamma))
#gamma is better
saveRDS(tempM.glm.gamma, "tempM_glm_gamma.RDS")
test10 <- data.frame(temp_Dmed = 10, season = as.factor(1))
predict(tempM.glm.gamma, new = test10, type = "response")
posterior_predict(tempM.glm.gamma, new = test10, type = "response")

predictive_interval(tempM.glm.gamma)


summary(tempM.glm.gamma)
stan_trace(tempM.glm)
mcmc_areas(tempM.glmer.gamma)
coef(tempM.glm)
c = as.numeric(coef(tempM.glm)[2])

color_scheme_set("red")
ppc_dens_overlay(y = tempM.glmer.gamma$y,
                 yrep = posterior_predict(tempM.glm.gamma, draws = 50))

ppc_intervals_grouped(
  y = df1$mean_k_coarse,
  yrep = posterior_predict(tempM.glm.gamma),
  x = df1$temp_Dmed,
  group = df1$season,
  prob = 0.9
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

########################
### Growing from EEB input 
########################

mod_z2.gamma1 <- gam(avg_k ~ s(temp_55, bs = 'cc'), #+ s(q_55_75, bs = 'cc') + s(POC_55, bs = 'cc') + s(ID, bs = "re"),
                    data = df, method = 'REML', family = Gamma)
mod_z2.gamma2 <- gam(avg_k ~ s(temp_55) + s(q_55_75), #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
                     data = df, method = 'REML', family = Gamma)

mod_z3.gamma2 <- glm(avg_k ~ q_55_75, #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
                     data = df, family = Gamma)

mod_z2.gamma3 <- gam(avg_k ~ s(temp_55, bs = 'cc') + s(q_55_75, bs = 'cc') + s(POC_55, bs = 'cc'), #+ s(ID, bs = "re"),
                     data = df, method = 'REML', family = Gamma)

mod_y <- glm(mean_k_coarse ~ cpom + , #+ s(ID, bs = "re"),
                     data = df2, family = Gamma)

mod_x<- glm(avg_k ~ q_55_75 + temp_Dmed + cpom, #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
            data = df3, family = Gamma)

summary(mod_y)
summary(mod_z2.gamma1)
summary(mod_z2.gamma2)
summary(mod_z3.gamma2)
summary(mod_z2.gamma3)

glm.test <- mod_z2.gamma #update to apply to all the graphs
performance::model_performance(glm.test)
gam.check(glm.test)
plot(glm.test,pages=1,residuals=TRUE)
anova(glm.test)

ggplot()+
  geom_point(aes(x=mod_z2.gamma1$y, y =mod_z2.gamma1$fitted.values), color = "red")+
  stat_smooth(aes(x=mod_z2.gamma1$y, y =mod_z2.gamma1$fitted.values), color = "red")+
  geom_point(aes(x=mod_z2.gamma2$y, y =mod_z2.gamma2$fitted.values), color = "blue")+
  stat_smooth(aes(x=mod_z2.gamma2$y, y =mod_z2.gamma2$fitted.values), color = "blue")+
  geom_point(aes(x=mod_z3.gamma2$y, y =mod_z3.gamma2$fitted.values), color = "yellow")+
  stat_smooth(aes(x=mod_z3.gamma2$y, y =mod_z3.gamma2$fitted.values), color = "yellow")+
  geom_point(aes(x=mod_z2.gamma3$y, y =mod_z2.gamma3$fitted.values), color = "black")+
  stat_smooth(aes(x=mod_z2.gamma3$y, y =mod_z2.gamma3$fitted.values), color = "black")+
  geom_point(aes(x=mod_y$y, y =mod_y$fitted.values), color = "cyan")+
  stat_smooth(aes(x=mod_y$y, y =mod_y$fitted.values), color = "cyan")+
  geom_point(aes(x=mod_x$y, y =mod_x$fitted.values), color = "green")+
  stat_smooth(aes(x=mod_x$y, y =mod_x$fitted.values), color = "green")+
  geom_abline(slope = 1, intercept = 0)
  
  
df3 <-  df %>%
  left_join(., df2, by = c("date", "stream", "Jdate")) %>%
  na.omit()


ggplot()+
geom_point(aes(x=mod_x$y, y =mod_x$fitted.values), color = "green")+
  stat_smooth(aes(x=mod_x$y, y =mod_x$fitted.values), color = "green")+
  geom_abline(slope = 1, intercept = 0)

mod_x<- glm(avg_k ~ q_55_75 + cpom, #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
            data = df3, family = Gamma)

summary(mod_x)


#############
## Addingi CPOM landscape values

library(fuzzyjoin)
##

df_ref <- read_csv("data/input_data.csv")%>%
  select(1:6, "basin_id")
#add cpom_landscape
cpom_lndsp <- read.csv("G:/My Drive/CREWS_official/200_Landscape/210_data_landscape/211_organicmatter/Landscape_cbom/landscape_cbom_calculations_master_31Oct2020.csv")%>%
  mutate(date = as.Date(sample.date, "%m/%d/%Y")) %>%
  dplyr::filter(om.category == "LEAF")%>%
  dplyr::select(date, stream, cbom.afdm.g.m2)%>%
  group_by(date, stream)%>%
  summarize(cbom = mean(cbom.afdm.g.m2))

#add julilan date
df_ref$Jdate <- yday(df_ref$date) 
#add deployment year - water year
df_ref$year <- as.factor(as.character(waterYear(df_ref$date))) #do not want ordered factor
#make location ID a factor
df_ref$ID <- as.factor(df_ref$stream)

#remove duplicates
df_ref <-  df_ref %>%
  distinct(date, stream, .keep_all = TRUE)


# remove streams with strongly unique signals WS01 and WS17 removed as they are pine groves and USHF exhibits a similar characteristic to those streams. 
df_ref <- df_ref %>%
  dplyr::filter(!stream %in% 
                  c('WS01', 'WS17', 'USHF'))

para_names <- c("qls", "temp.celsius")

#I recongize imputation for Q is not great choice considering the variability
for(x in para_names){
  coef_x <- df_ref %>%
    dplyr::select(stream, Jdate, starts_with(x)) %>%
    tidyr::pivot_wider(names_from = stream, values_from = starts_with(x))%>%
    arrange(., Jdate)#order by Jdate so index is Jdate
  
  imp=mice(as.matrix(coef_x), print=F,threshold=.9999)
  imp=mice::complete(imp,'long')
  
  imp= imp %>% group_by(Jdate) %>% 
    summarize_all(mean)%>%
    dplyr::select(-2:-3)%>%#remove extra columns
    pivot_longer(cols = 2:ncol(.), names_to = "stream", values_to = x)
  
  #replace df_Ref column with imputated column
  df_ref <- df_ref %>%
    dplyr::select(-starts_with(x))%>%
    left_join(., imp, by = c("stream", "Jdate"))
}

head(df_ref)

### THis is an awesome function for dates that dont match exactly. See https://community.rstudio.com/t/tidy-way-to-range-join-tables-on-an-interval-of-dates/7881
df_c <- fuzzy_left_join(
  df1, cpom_lndsp,
  by = c(
    "stream" = "stream",
    "date" = "date",
    "date_e" = "date"
  ),
  match_fun = list(`==`, `<=`, `>=`)
)


df <- left_join(df_ref, df_c, by = c("ID", "date" = "date.x", "temp.celsius"))

df_run <- df %>%
  dplyr::arrange(desc(ID)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(month = month(date),
                temp_55 = zoo::rollmean(temp.celsius, k = 55, fill = NA, align = "left"),
                q_55= zoo::rollmean(qls, k = 55, fill = NA, align = "left"),
                q_55_75 = zoo::rollapply(qls, width=55, quantile, probs=0.75, fill=NA),
                POC_55= zoo::rollapply(POC_in_gd, width=55, mean, fill = NA, align = "left"))#%>%  # sum versus average 
  # dplyr::ungroup() %>% #ungroup to scale 
  # dplyr::mutate(temp_55 = scale(temp_55),
  #               q_55= scale(log(q_55)), #ln
  #               q_55_75 = scale(log(q_55_75)), #ln
  #               #POC_55= scale(POC_55),
  #               kc_s = scale(mean_k_coarse),
  #               season = lubridate::quarter(date, fiscal_start = 1), #DJF/MAM/JJA/SON
  #               basin_id = as.factor(basin_id),
  #               cbom_s = scale(cbom))%>% 
  # na.omit


mod_w<- glm(mean_k_coarse ~ q_55_75 + cbom_s + temp_55, #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
            data = df_run, family = Gamma)
summary(mod_w)
summary(mod_w1_LF)

df_run <- df_run %>%
  dplyr::filter(kc_lamF > 0)
mod_w1<- stan_glm(mean_k_coarse ~ q_55 + temp_55, #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
            data = df_run, family = Gamma)
mod_w1_LM<- stan_glm(kc_lamM ~ q_55 + temp_55, #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
             data = df_run, family = Gamma)
mod_w1_LF<- stan_glm(kc_lamF ~ q_55 + temp_55, #+ s(POC_55, bs = 'cc') + s(ID, bs = "re"),
                data = df_run, family = Gamma)
ggplot()+
  geom_point(aes(x=mod_w1$y, y =mod_w1$fitted.values), color = "green")+
  stat_smooth(aes(x=mod_w1$y, y =mod_w1$fitted.values), color = "green")+
  geom_point(aes(x=mod_w1_LM$y, y =mod_w1_LM$fitted.values), color = "blue")+
  stat_smooth(aes(x=mod_w1_LM$y, y =mod_w1_LM$fitted.values), color = "blue")+
  geom_point(aes(x=mod_w1_LF$y, y =mod_w1_LF$fitted.values), color = "red")+
  stat_smooth(aes(x=mod_w1_LF$y, y =mod_w1_LF$fitted.values), color = "red")+
  geom_abline(slope = 1, intercept = 0)

saveRDS(mod_w1_LM, "mod_w1_LM.RDS")
saveRDS(mod_w1_LF, "mod_w1_LF.RDS")


ggplot(df_run) +
  geom_violin(aes(as.factor(month), POC_55))
