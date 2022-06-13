##### Explore the age distrbutuion of litter
library(tidyverse)
library(lubridate)

date_seq <- seq.Date(as.Date("11-01-2018", "%m-%d-%Y"), as.Date("10-31-2019", "%m-%d-%Y"), by = 'day')

set.seed(2)
IN_pom <- as.data.frame(date_seq) %>%
  mutate(mon = as.numeric(month(.$date_seq)))%>%
  left_join(., POM_input, by = c("mon" = "sample.mo"))
  
  
  dplyr::filter(month == mon)  
