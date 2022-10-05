# Package ID: knb-lter-hbr.50.5 Cataloging System:https://pasta.edirepository.org.
# Data set title: Coarse Litterfall Data at the Hubbard Brook Experimental     Forest, 1996 - present.
# Data set creator:  Timothy J. Fahey -  
# Metadata Provider:    - Hubbard Brook Experimental Forest LTER 
# Contact:    - Information Manager, Hubbard Brook
# LTER   - 
#   hbr-im@lternet.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/50/5/c184ac88dda757017922649aed3a68a5" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep="," 
               , col.names=c(
                 "SITE",     
                 "PLOT",     
                 "YEAR",     
                 "DRY_WT"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$PLOT)!="factor") dt1$PLOT<- as.factor(dt1$PLOT)
if (class(dt1$DRY_WT)=="factor") dt1$DRY_WT <-as.numeric(levels(dt1$DRY_WT))[as.integer(dt1$DRY_WT) ]               
if (class(dt1$DRY_WT)=="character") dt1$DRY_WT <-as.numeric(dt1$DRY_WT)

# Convert Missing Values to NA for non-dates

dt1$DRY_WT <- ifelse((trimws(as.character(dt1$DRY_WT))==trimws("n/s")),NA,dt1$DRY_WT)               
suppressWarnings(dt1$DRY_WT <- ifelse(!is.na(as.numeric("n/s")) & (trimws(as.character(dt1$DRY_WT))==as.character(as.numeric("n/s"))),NA,dt1$DRY_WT))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(SITE)
summary(PLOT)
summary(YEAR)
summary(DRY_WT) 
# Get more details on character variables

summary(as.factor(dt1$SITE)) 
summary(as.factor(dt1$PLOT))
detach(dt1)               





