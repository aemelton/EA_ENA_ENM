### AE Melton
# Extract info from enmeval r data 

###
options(java.parameters = "- Xmx16g") # increase memory that can be used
#library(devtools)
#install_github('johnbaums/rmaxent')
source("~/Dropbox/Scripts/MacroEcology_and_Phylogenetics/Parse_lambdas.R")
library(rmaxent)

# Load required libraries
library(ENMeval)
#library(rSDM)
library(dismo)
#library(spThin)
#library(readr)
library(raster)
library(rgdal)
library(sp)
#library(rgeos)
#library(RStoolbox) 
#library(ggplot2)
library(ecospat)

# Load all the data
setwd("~/Dropbox/UF_Research/EA_ENA_ENM/ENMEval_Outputs/Aesculus_pavia/")
load(file = "Aesculus_pavia.rda")
n <- which(modeval@results$delta.AICc==0)
pred.rast <- raster("Aesculus_pavia.asc")
cleaned.pts <- read.csv("~/Dropbox/UF_Research/EA_ENA_ENM/Point_Data/clean_Aesculus_paviaPointsSPOCC_EDITS.csv")[,2:3]

# all cleaned points v points used in modeling
nrow(cleaned.pts)
nrow(modeval@occ.pts)

# load the predeictions raster and all cleanded points - plot
plot(pred.rast)
points(cleaned.pts, pch = 19, col = "black")
points(modeval@occ.pts, pch = 19, col = "red")
points(gridSample(cleaned.pts, r = pred.rast, n = 1), col = "blue")

# 
aic.opt <- modeval@models[[which(modeval@results$delta.AICc==0)]]
parse_lambdas(aic.opt) 

# AUC statistics
modeval@results$avg.test.AUC[[n]]
modeval@results$avg.diff.AUC[[n]]

# Continuous Boyce Index
cbi <- ecospat.boyce(fit = p, obs = rare.pts, PEplot = T)
cbi$Spearman.cor

