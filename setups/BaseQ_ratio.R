BQ_ratio <- Q_JDate_lm %>% dplyr::filter(term == "totLength_") %>%
  dplyr::filter(Basin_ID != "CoweetaCreek") %>%
  dplyr::select(-term,-std.error, -p.value, -statistic)%>%
  pivot_wider(names_from = Basin_ID, values_from = estimate)%>%
  dplyr::mutate(ball_shope_ratio = BallCreek/ShopeFork)


ggplot(BQ_ratio)+
  geom_line(aes(x = JDate, y = ball_shope_ratio))


### Would be cool to plot versus precipitation