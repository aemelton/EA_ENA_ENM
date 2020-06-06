### AE Melton
#
# Libraries required for other scripts in my Niche_Things folder
#

#options(java.parameters = "- Xmx16g") # increase memory that can be used; Doesn't seem to work for all new OS or R versions..?

# If there are problems installing the following packages, use the devtools package to install # from GitHub.

#library(devtools)
#install_github('johnbaums/rmaxent')
#install.packages("devtools")
#library(devtools)
#install_github(repo="ecospat/ecospat/ecospat")

### Check installed package - install if still needed

list.of.packages <- c("caret",
"dismo",
"doParallel",
"ecospat",
"ENMeval",
"foreach",
"ggplot2",
"hypervolume",
"parallel",
"raster",
"readr",
"rgdal",
"rgeos",
"rmaxent",
#"rSDM",
"RStoolbox",
"scrubr",
"sp",
"spocc",
"spThin")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Load libraries
lapply(list.of.packages, require, character.only = TRUE)

### Source scripts
setwd("~/Dropbox/Niche_Things/Functions/")
files.sources = list.files()
sapply(files.sources, source)
