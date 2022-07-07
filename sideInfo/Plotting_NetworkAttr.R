###Plotting Network

#which timestep to plot
iG = 90
p <- net_lst[[iG]]
# Convert igraph network into visNetwork format
visCWT <- toVisNetworkData(p)

# Grab nodes data frame.
nodes <- visCWT$nodes

# Grab edges data frame. 
edges <- visCWT$edges


# Create continuous color palette.
DOCoutPal <- colorRampPalette(c('#E0F4FF','#003049'))

# Plot. 
visNetwork(nodes, edges) %>%
  #visGroups(groupname = "", color = "red") %>%
  #visGroups(groupname = "B", color = "lightblue") %>%
  visLegend()
#visNetwork(nodes, edges)

V(net_lst[[iG]])$color <- DOCoutPal(7)[cut(V(p)$FTOC_out, breaks = 7)]
visIgraph(net_lst[[iG]])


visIgraph(net_lst[[iG]]) %>% 
  visOptions(nodesIdSelection = TRUE, highlightNearest = TRUE)
