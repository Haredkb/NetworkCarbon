#make small wateshed

#load data from data file 

ws <- "WS37" #choosen due to sizze

n <- nodes %>%
  dplyr::filter()


rds_files <- list.files("data", pattern = ".RDS") %>%
  file.path("data", .) %>%
  map(readRDS)

nodes <- rds_files[[7]]
edges <- rds_files[[3]]


nodes_37 <- nodes %>%
  dplyr::filter(n_lscp_name == ws)

node_n <- as.numeric(unique(nodes_37$name))#get names of the nodes

edges_37 <- edges %>%
  dplyr::filter(NODE_A %in% node_n)

edges_37 <- edges_37[1:nrow(edges_37)-1,]


net <- graph.data.frame(d=edges_37,vertices=nodes_37,directed = T)

saveRDS(net, "data/eg_watershed.RDS")
