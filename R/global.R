
library(igraph)        # network analysis; create river network graphs
library(tidygraph)

library(visNetwork) #plotting igraphs
library(nhdplusTools)  # R package for interafacing with NHDPlus
library(dataRetrieval) # R package for interfacing with NWIS data
library(sf)            # spatial analysis
library(rgeos)      #for clip
library(maptools)   #for clip
library(raster)
library(rgdal) #for projection
library(tidyverse)
library(broom)
library(ggnetwork)    #clean up igraph presentation?
library(ggplot2)
theme_set(theme_bw())
library(ggraph) #network graph pretty
library(lubridate)
library(dplyr)       
library(rstan)
library(rstanarm)
library(bayesplot)
# general data manipulation and cleaning
#library(magick) #create animation
#source("R/igraphNetwork_MB.R")
source("R/FUNCTIONS_MB.R")

library('devtools') 
#install_github('brooksandrew/Rsenal') 
library('Rsenal')
library(fuzzyjoin)

