#### CREWS model

library(lme4)
library(tidyverse)
library(igraph)

## read in data from CC
all_smra_mte_newshred <- read_csv("C:/Users/hared/Dropbox/UConn/Projects/500_NetworkScaleCarbon/520_Data/Carolyn_TQkModels/all_smra_mte_newshred.csv")
date_deploy <- data.frame(date_dep = unique(all_smra_mte_newshred$date_dep), date_coll = unique(all_smra_mte_newshred$date_coll))
all_smra_mte_newshred$one.k.T.cent_test <- all_smra_mte_newshred$one.k.T - (mean(all_smra_mte_newshred$one.k.T))
(mean(all_smra_mte_newshred$one.k.T))
(mean(all_smra_mte_newshred$mean_flow))
mean_flow_sc <- scale(all_smra_mte_newshred$mean_flow) # (mean(all_smra_mte_newshred$mean_flow))
# center 96.2, scaled 210

big_model <- lmer(ln_mean_k ~ one.k.T.cent*rhodo_acer*Type + mean_flow_st*rhodo_acer + mean_flow_st*Type + (1|stream) + (1|date_dep), data=all_smra_mte_newshred)
summary(big_model)

saveRDS(big_model, "data/Model_CC.RDS")

#add scaled temp and q to the network
V(network_pre)$one.k.T.cent <-  (1/((V(network_pre)$tempC + 273.15)* 8.62E-5)) - 40.66639
V(network_pre)$mean_flow_st = scale(V(network_pre)$Qout, attr(mean_flow_sc, "scaled:center"), attr(mean_flow_sc, "scaled:scale"))

#create input dataframe
input_df <- data.frame(one.k.T.cent = V(network_pre)$one.k.T.cent, mean_flow_st = V(network_pre)$mean_flow_st)
input_df$Type = "Microbes"
input_df$rhodo_acer = "A"

#"A" for Acer and "M" for Microbes
#https://stackoverflow.com/questions/28199140/using-lme4-modeling-to-predict-from-fixed-effects-values
mod_pred_AM <- predict(big_model,newdata=input_df,re.form=~0)#treat as population, so no RE


#create input dataframe
input_df <- data.frame(one.k.T.cent = V(network_pre)$one.k.T.cent, mean_flow_st = V(network_pre)$mean_flow_st)
input_df$Type = "Shredders"
input_df$rhodo_acer = "A"
mod_pred_AF <- predict(big_model,newdata=input_df,re.form=~0)#treat as population, so no RE

#output into the network 
V(network_pre)$k_AM <- exp(mod_pred_AM)
V(network_pre)$k_AF <- exp(mod_pred_AF)
V(network_pre)$k_At <- V(network_pre)$k_AM + V(network_pre)$k_AF
#lme4:::predict.merMod(big_model)