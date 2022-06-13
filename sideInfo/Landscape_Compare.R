
#### 
## Check DOC 
####
# Use DataWrangle DOC stream: DWCoweetaDOCStream.R

library(fuzzyjoin)
DOC_stream

#filter out any nodes not a landscape point
lscp_sites <- network_ts_all_ss %>%
  dplyr::filter(!is.na(stream)) %>%
  mutate(JDate = yday(date_d),
    Jdate_a30 = yday(date_d) + 30,
         Jdate_s30 = yday(date_d) - 30)%>%
  left_join(., DOC_stream, by = c("stream", "JDate"))
  
  fuzzy_inner_join(., DOC_stream,
                  by = c("stream" = "stream",
                         'Jdate_a30' = "JDate",
                         'Jdate_s30' =  "JDate"),
                  match_fun = list(`==`, `>`, `<`))



  lscp_sites$DOC_stream = lscp_sites$doc.mgl * lscp_sites$Qout
  