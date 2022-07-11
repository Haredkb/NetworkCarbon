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


## Plot. 
# visNetwork(nodes, edges) %>%
#   #visGroups(groupname = "", color = "red") %>%
#   #visGroups(groupname = "B", color = "lightblue") %>%
#   visLegend()
#visNetwork(nodes, edges)

DOC_lvs <- levels(cut(V(p)$DOC_out, breaks = 7))
cbind(lower = as.numeric( sub("\\((.+),.*", "\\1", DOC_lvs) ),
      upper = as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", DOC_lvs) ))


DOCoutPal <- colorRampPalette(c('#E0F4FF','#003049'))
V(net_lst[[iG]])$color <- DOCoutPal(length(seq(0,100,2)))[cut(V(net_lst[[iG]])$DOC_out, breaks = seq(0,100,2))]
visIgraph(net_lst[[iG]])


visIgraph(net_lst[[iG]]) %>% 
  visOptions(nodesIdSelection = TRUE, highlightNearest = TRUE)

### Creating Anmation
### carbon output an image

for(iG in seq(2:length(dates))){
  V(net_lst[[iG]])$color <- DOCoutPal(length(seq(0,200,2)))[cut(V(net_lst[[iG]])$DOC_out, breaks = seq(0,200,2))]
  
  data <- toVisNetworkData(net_lst[[iG]])
  visNetwork(nodes = data$nodes, edges = data$edges)
  
fn <- paste0("animate/CWT_DOC_output", iG, ".png")
png(fn)
#plot(edges_sf["ORDER"])#dont know why the layering doesnt align...

plot_vis <- visIgraph(net_lst[[iG]])
visSave(plot_vis, file = "network.html", background = "black")

}
dev.off()


# plot(p, vertex.label=V(p)$NODE_ID, vertex.color=V(p)$col,  #vertex.color=type_colour,
#      vertex.shape='circle',
#      vertex.size=4,edge.width=1,edge.arrow.size=0.4,edge.color="darkgray",
#      xlab="DOC Input Gross Gains via GW (mg/d)",
#      sub=paste("Julian Day", day),#month.abb[mon]),
#      cex.sub=2, cex.axis=2
# )
# AddGradientLegend(breaks, pal = pal,#GetColors(scheme = "discrete rainbow"),##library(inlmisc)
#                   scientific = FALSE, strip.dim = c(1, 14),
#                   inset = c(0.1, 0.1))

# legend("topright", col=pal(2), pch=19,
#        legend=c(round(range(nodes_bf$baseflow_cfs), 0)))
#dev.copy(png,fn)
dev.off()


## Example from https://wetlandscapes.github.io/blog/blog/making-movie-in-r-from-still-images/
library(magick)
library(animation)
#This tells the animation package where FFmpeg lives on my computer.
#FFmpeg is what is actually doing all the heavy lifting.
animation::ani.options(ffmpeg = shortPathName("C:/Program Files/ImageMagick-7.1.0-Q16-HDRI/ffmpeg.exe"))

CWT.photos <- list.files(path = "./animate/", pattern = "DOC", all.files = TRUE, ignore.case = TRUE)

animation::saveVideo(
  #Loop through all the photos, adding them one at a time. Not sure if this
  # is the most efficient way of doing things, but it works.
  for(i in 1:length(CWT.photos)){
    #Read in an image.
    SLF.image <- magick::image_read(CWT.photos[i])
    #Plot the image.
    plot(CWT.image)
  },
  #Designate the name of the image within the working directory.
  video.name = "AnimationCWTDOC.avi",
  #The extra image quality parameters indicated earlier.
  #other.opts = opts
)

