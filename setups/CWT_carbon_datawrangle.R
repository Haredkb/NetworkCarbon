###################
## Set up data for Analysis Observed Data

k_Year1 <- read.csv("data/si_k_year1.csv")
k_Year2 <- read.csv("data/si_k_year2.csv")
#combine raw data 
k_df <- rbind(k_Year1, k_Year2)
rm(k_Year1, k_Year2) #clean up
#Replace Inf with NA
k_df <- do.call(data.frame,lapply(k_df, function(x) replace(x, is.infinite(x),NA)))

#clean up decomposition data
k_df <- k_df %>%
  mutate(date_s = as.Date(date_dep, format = "%Y-%m-%d"),
         Jdate_s = format.Date(date_s, "%y"),
         date_e = as.Date(date_coll, format = "%Y-%m-%d"),
         days_dep = as.numeric(difftime(date_e, date_s, units = "days"))) %>%
  dplyr::filter(rhodo_acer == "A") %>%
  dplyr::select(mean_k_coarse, sd_k_coarse, mean_k_fine, sd_k_fine, mean_k_shred_new, date_s, date_e, days_dep, rhodo_acer, stream) #clean up

saveRDS(k_df, "data/k_df.RDS")

############
##CPOM
############

## Read in CPOM data 

cpom <- read.csv("G:/My Drive/CREWS_official/200_Landscape/210_data_landscape/211_organicmatter/Landscape_cbom/landscape_cbom_calculations_master_31Oct2020.csv") %>%
  dplyr::filter(om.category == "LEAF") %>%
  mutate(date = as.Date(sample.date, format = "%m/%d/%Y"),
         Jdate = yday(date),
         #date diff is the time since october 1
         date_diff = ifelse(Jdate > 172, Jdate - 172, Jdate + 193))#as.numeric(difftime(as.Date("01-01-2018", format = "%m-%d-%Y"), as.Date("10-01-2017", format = "%m-%d-%Y"), units = c("days")))))

#create df that average replicate cpom samples 
cpom_lndsp <- cpom %>%
  group_by(stream, date, date_diff, Jdate) %>%
  dplyr::summarise(
    cpom_ss_avg = mean(cbom.afdm.g.m2)
  )

cpom_wsavg <- cpom %>%
  mutate(month_d = month(date))%>%
  group_by(month_d) %>%
  dplyr::summarise(
    cpom_ss_avg = mean(cbom.afdm.g.m2))%>%
  mutate(Jdate = month_d * 30)

#Determine average standing stock for the whole watershed
#https://stats.stackexchange.com/questions/225653/periodic-splines-to-fit-periodic-data
library(splines2)
#t <- seq(0, 365, length.out = 800)

pspline_fit <- lm(cpom_ss_avg ~ mSpline(x = Jdate, 
                              df = 4, 
                              periodic = TRUE, 
                              Boundary.knots = c(1, 365)), data = cpom_wsavg)

cpom_gm2 <- data.frame(Jdate = seq(1,365))
#cpom_test <- cbind(cpom_gm2, as.data.frame(predict(pspline_fit, interval = "prediction")))


cpom_gm2<- cbind(cpom_gm2, as.data.frame(predict(pspline_fit,cpom_gm2, interval="prediction")))
cpom_gm2$cpom_fit <- cpom_gm2$fit
pred_plot <- 
  ggplot(cpom_gm2, aes(x = Jdate, y = fit)) + 
  geom_point(data = cpom_wsavg, aes(Jdate, cpom_ss_avg))+
  #geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) + 
  geom_line(size = 1, colour = "blue") + 
  geom_point() + 
  theme_minimal()

pred_plot
  
pred_plot + xlim(0, 365) + coord_polar()

#cpom.lm <- lm(cpom_ss_avg ~ bs(date_diff), data = cpom_wsavg) #intial standing stock based on whole basin

#create a df that predictes cpom for each date from october 1 (so we can fit a linear relationship)
#cpom_gm2 <- data.frame(date_diff = seq(1,365)) #for linear
#cpom_gm2$cpom <-  predict.lm(cpom.lm, cpom_gm2)

#convert date_diff (days since Octoer 1) to Julian day for use in the model
#cpom_gm2$Jdate <- ifelse(cpom_gm2$date_diff <= 172, cpom_gm2$date_diff + 193, cpom_gm2$date_diff - 172)

plot(cpom_gm2$Jdate, cpom_gm2$cpom_fit)



saveRDS(cpom_gm2, "data/cpom_gm2.RDS")


####POM input
POM_input <- readRDS("data/POM_input.RDS") 
#make dataframe of values throughout network for consistency
set.seed(2)
ClocalLit_g <- lapply(seq(1:12), function(x, df = POM_input, l = length(V(net))){
  
  data.frame(Cdirect_gm2hr = df[x,]$direct_gm2d_avg/24,#hourly   #abs(rnorm(l, mean = df[x,]$direct_gm2d_avg, 
             #sd = df[x,]$direct_gm2d_sd))/24, #direct hourly
             
             Clateral_gmhr = df[x,]$lateral_gmd_avg/24,#hourly #abs(rnorm(l, mean = df[x,]$lateral_gmd_avg, 
             #sd = df[x,]$lateral_gmd_sd))/24, #lateral hourly
             month = x
  )
  
})
ClocalLit_g = do.call(rbind,  ClocalLit_g)

saveRDS(ClocalLit_g, "data/ClocalLit_g.RDS")



### DOC seep data
DOC_seep_table <- readRDS("data/DOC_seep_table.RDS")
# set the random from the here for reproducability
#set.seed(2)
# Only using average currently
DOC_gw <- lapply(seq(1:12), function(x, df = DOC_seep_table, l = length(V(net))){
  
  data.frame(doc_mgL = df[x,]$doc_ppm_av,     #abs(rnorm(l, mean = df[x,]$doc_ppm_av, sd = df[x,]$doc_ppm_sd)),
             month = x
  )
})

DOC_gw = do.call(rbind,  DOC_gw)


saveRDS(DOC_gw, "data/DOC_gw.RDS")

