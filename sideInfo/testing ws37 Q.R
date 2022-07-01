df_37 <- dplyr::filter(df_bfi, stream == "WS37")
net_eg <- dplyr::filter(network_ts_all_ss, stream == "WS37") %>% # only landscape ndoe
            mutate(Jdate = yday(as.Date(date, format = "%Y-%m-%d")))
#plots
ggplot(df_37)+
  geom_point(aes(Jdate, baseq_m3hr))+
  geom_point(data = net_eg, aes(Jdate, Qout), color = "red")
