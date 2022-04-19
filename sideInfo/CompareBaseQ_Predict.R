


#######################
######plot predicted versus observed 
sites_BF <- st_sf(sites_BF) #from baseflow regression 
sites_BF<- st_transform(sites_BF, crs = st_crs(SHP_Fork_Basin))


BF_test <- st_join(sites_BF, left = TRUE, SHP_Fork_Basin["basin_id"]) #left true treats as left join (false is inner)
BF_test$basin_id[is.na(BF_test$basin_id)] <- "CoweetaCreek"

BF_test <- left_join(BF_test, Q_JDate_lm, by = c("basin_id" = "Basin_ID", "Jdate" = "JDate"))


BF_test <- BF_test %>%
  mutate(predict_BF = m * totLength_ + b)


ggplot(BF_test) +
  geom_point(aes(BaseQ, predict_BF, colour = basin_id))+
  geom_abline(slope=1, intercept=0)
