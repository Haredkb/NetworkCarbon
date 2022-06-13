
ggplot(cpom_lndsp, aes(x = date, y = cpom_ss_avg)) + 
  geom_point(aes(color = stream)) + 
  geom_line(aes(color = stream)) + 
  geom_abline(slope = coef(cpom.lm)[["date_diff"]], 
              intercept = coef(cpom.lm)[["(Intercept)"]])

#Compare Standing Stock from linear interp to the standingstock stock from standingstock initial then litter in and loss
ggplot(network_ts_all_ss, aes(x = ss_POC, y = POC_sStock_g, colour = yday(date))) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Observed Standing Stock (landscape nearest measure)", y = "Standing stock considering litter in and loss")+
  facet_wrap(~month(date))

colors <- c("Landscape k" = "black", "TempDischarge k" = "red")
ggplot(network_ts_all_ss) + 
  geom_point(aes(x = ClocalLit_g, y = POC_loss_g, colour = "Landscape k")) + 
  geom_point(aes(x = ClocalLit_g, y = POC_loss_g_TQ, colour = "TempDischarge k"))+
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Litter In (gC)", y = "POC loss (gC)", color = "Legend") +
  scale_color_manual(values = colors)

ggplot(network_ts_all_ss, aes(x = date, y = ss_POC - POC_sStock_g, color = n_lscp_name)) + 
  geom_point() + 
  labs(x = "date", y = "Residiuals: Landscape StandingStock - Standing stock considering litter in and loss")


ggplot(network_ts_all_ss, aes(x = ClocalLit_g, y = POC_loss_g, colour = stream)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Litter In (gC)", y = "POC loss (gC)")+
  facet_wrap(~month(date))


ggplot(network_ts_all_ss, aes(x = POC_loss_g_F, y = POC_loss_g_F_TQ, colour = as.factor(n_lscp_name))) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Frag POC loss Landscape Prediction (gC))", y = "Frag POC loss based on Temp and Q Depedence (gC)")


ggplot(network_ts_all_ss, aes(x = POC_loss_g_M, y = POC_loss_g_M_TQ, colour = Qout))+ #yday(date))) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Microbial POC loss Landscape Prediction (gC))", y = "Microbial POC loss based on Temp and Q Depedence (gC)")

ggplot(network_ts_all_ss, aes(x = lambda_F, y = k_TQ_lamF, colour = yday(date))) + 
  geom_point() + 
  #geom_abline(slope = 1, intercept = 0)+
  labs(x = "lambda F landscape (day-1)", y = "lambda F Temp and Q Depedence (day-1)")

ggplot(network_ts_all_ss, aes(x = lambda_M, y = k_TQ_lamM, colour = yday(date))) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "lambda M landscape (day-1)", y = "lambda M on Temp and Q Depedence (day-1)")

#set colours to keep dataframe wide
colors <- c("Fragmentation" = "Blue", "Microbial" = "Red")
#use the label names to specficy colours
ggplot(network_ts_all_ss) + 
  # geom_point(aes(x = date_d, y = lambda_M - k_TQ_lamM, colour = "Microbial")) + 
  # geom_point(aes(x = date_d, y = lambda_F - k_TQ_lamF, colour = "Fragmentation")) + 
    geom_point(aes(x = date_d, y = POC_loss_g_M - POC_loss_g_M_TQ, colour = "Microbial")) + 
    geom_point(aes(x = date_d, y = POC_loss_g_F - POC_loss_g_F_TQ, colour = "Fragmentation")) + 
  geom_hline(yintercept = 0)+
  labs(x = "date", y = "loss landscape - TempQ: Residuals", color = "Legend") +
  scale_color_manual(values = colors)+
  facet_wrap(~n_lscp_name)

#########################333
### igraph plotting
##########################
ggraph(net_lst[[1]], layout = "stress") + 
  geom_edge_link(
    arrow = arrow(), 
    start_cap = circle(5, "mm"),
    end_cap = circle(5, "mm"))

plot(net_lst[[1]]) #, vertex.label="n_lscp_name")
     vertex.color = V(net)$color,
     #getPalette(colourcount),#not right
     vertex.shape='circle',vertex.size=5,edge.width=1,edge.arrow.size=0.4,edge.color="darkgray")
