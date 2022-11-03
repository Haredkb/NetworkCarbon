#Functions 

readdf_Scen <- function(ts_all_filepath, scen_name, df_edge, lit_type){
readRDS(ts_all_filepath) %>% #scenarios_temperature[[1]]%>%
    left_join(., df_edge, by = c("name" = "to"))%>%
    ##dplyr::filter(st_order<5)%>%
    dplyr::select(date_d, name, TYPE, up.all, x, y, 
                  stream_nam, length_reach, Qout, tempC, POC_sStock_AFDMg,
                  POC_loss_gC, DOC_local_gC, ClocalLit_AFDMg,POC_loss_gC_F,POC_loss_gC_M, Bedarea_m2, k_At)%>%
    dplyr::filter(date_d > as.Date("2019-07-31", format = "%Y-%m-%d") & date_d < as.Date("2020-07-31", format = "%Y-%m-%d"))%>%
    mutate(hour = rep(rep(1:24, each=length(unique(name))), length(unique(date_d)))
                      ) %>%
    ## SUM FIRST TO GET THE TOTAL WATERSHED NETWORK 
    group_by(date_d, hour) %>%
    summarise(
      tempC = mean(tempC, na.rm = TRUE),
      Q_outlet = max(Qout, na.rm = TRUE),
      kA_avg = mean(k_At, na.rm = TRUE),
      C_StStock_gC = sum(POC_sStock_AFDMg, na.rm = TRUE) * 0.484, ## SUM HERE TO GET THE TOTAL WATERSHED NETWORK 
      C_LitterIn_gC = sum(ClocalLit_AFDMg, na.rm = TRUE) * 0.484,
      C_breakdownTQ_gC = sum(POC_loss_gC, na.rm = TRUE),
      C_breakdownTQ_gC_frag = sum(POC_loss_gC_F, na.rm = TRUE),
      C_breakdownTQ_gC_micrb = sum(POC_loss_gC_M, na.rm = TRUE),
      C_gw_gC = sum(DOC_local_gC, na.rm = TRUE),
      WS_benthic_area_m2 =  sum(Bedarea_m2, na.rm = TRUE)
    ) %>%
    # summarize daily 
    group_by(date_d) %>%
    summarise(
      tempC = mean(tempC, na.rm = TRUE),
      Q_outlet = max(Q_outlet, na.rm = TRUE),
      kA_avg = mean(kA_avg, na.rm = TRUE),
      C_StStock_gC = mean(C_StStock_gC, na.rm = TRUE),
      C_LitterIn_gC = sum(C_LitterIn_gC, na.rm = TRUE),
      C_breakdownTQ_gC = sum(C_breakdownTQ_gC, na.rm = TRUE),
      C_breakdownTQ_gC_frag = sum(C_breakdownTQ_gC_frag, na.rm = TRUE),
      C_breakdownTQ_gC_micrb = sum(C_breakdownTQ_gC_micrb , na.rm = TRUE),
      C_gw_gC = sum(C_gw_gC, na.rm = TRUE),
      WS_benthic_area_m2 =  mean(WS_benthic_area_m2, na.rm = TRUE)
    ) %>%
    mutate(scenario = scen_name,
           date_d = as.character(date_d),
           Jdate= yday(date_d),
           type = lit_type)
  }


