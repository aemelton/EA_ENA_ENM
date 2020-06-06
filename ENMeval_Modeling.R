## A.E. Melton
# Script to run ENMEval

### Install required packages
#library(devtools)
#install_github("bobmuscarella/ENMeval@master", force = TRUE)
#devtools::install_github("Pakillo/rSDM", force = TRUE)

#options(java.parameters = "- Xmx16g") # increase memory that can be used
#library(devtools)
#install_github('johnbaums/rmaxent')

library(rmaxent)

# Load required libraries
library(ENMeval)
library(rSDM)
library(dismo)
library(spThin)
library(readr)


### Load environmental data and prepare for analyses
# Present climate data
#
setwd("PATH TO RASTER LAYERS")

env.files <- list.files(pattern = ".asc", full.names = TRUE)
envStack <- stack(env.files)
names(envStack) <- c("bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "CEC",
                     "Clay", "Elevation", "ORC", "PH", "Sand", "Silt", "Slope", "Taxonomy", "TRI")

envStack <- setMinMax(envStack)
projection(envStack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
plot(envStack[[1]])

### Use correlation output from layer processing script (caret package)
corr <- layerStats(envStack, stat = "pearson", na.rm = TRUE)
c <- corr$`pearson correlation coefficient`
c.matrix <- data.matrix(c)
c.abs <- abs(c.matrix)
envtCor <- findCorrelation(c.abs, cutoff = 0.8, names = TRUE, exact = TRUE)

names(envStack)
sort(envtCor)
names(envStack[[c(2,3,5,6,7,8,9,10,14,16,17,21,25,29)]])
env <- envStack[[-c(2,3,5,6,7,8,9,10,14,16,17,21,25,29)]] 
names(env)

##
setwd("PATH TO COCCURRENCE DATA")
points <- read.csv("POINTS DATA")[,2:3]
nrow(points)

# Reduce "points" to just one occurrence per cell (There may not be more than one, but juuuuust in case...).
points <- gridSample(xy = points, r = envStack[[1]], n = 1)

# If there are more than N points after first recution, further reduce to N to maximize distribution evenness.
if (nrow(points) > N) {
source("PATH TO thin_max.R")
points <- thin.max(points, c("longitude", "latitude"), N) # trim down to N
}

nrow(points)
points(points, col = "black")

### Designate background data
# Randomly sample 10,000 background points from one background extent raster (only one per cell without replacement). Note: Since the raster has <10,000 pixels, you'll get a warning and all pixels will be used for background. We will be sampling from the biome variable because it is missing some grid cells, and we are trying to avoid getting background points with NA.
bg <- randomPoints(env[[1]], n=10000)
bg <- as.data.frame(bg)

# Check the bg point distribution in g-space
plot(env[[1]], legend=FALSE)
points(bg, pch = 16, col='red')

### Model generation
#envReduced <- subset(env, c(1,2,5,6)) # For after you know what to get rid of
modeval <- ENMevaluate(occ = points[, c("longitude", "latitude")], 
                       env = envStack,
                       bg.coords = bg,
                       #categoricals = "Taxonomy", 
                       algorithm = 'maxent.jar',
                       RMvalues = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4),
                       fc = c("L", "H", "LQ", "LQH", "LQP", "LQPH", "LQPHT"), #"L", "H", , "LQH", "LQP", "LQPH"
                       method = "checkerboard2", #"block", #"randomkfold", #kfolds = 10,
                       aggregation.factor = c(2,2),
                       #overlap = TRUE,
                       clamp = TRUE, 
                       rasterPreds = TRUE,
                       parallel = TRUE,
                       numCores = 4,
                       bin.output = TRUE,
                       progbar = TRUE)

##########################################################################################
modeval

results <- modeval@results
# Tells you which model is the best
n <- which(results$delta.AICc == 0)

### Convert prediction to different output
modeval@predictions[[n]]  # raw output; Check model name!
p <- predict(modeval@models[[n]], env)

plot(p)  # logistic output
points(points)

##########################################################################################
### Save Model Output #change name for lines 133,135,137
setwd(dir = "PATH TO OUTPUT FOLDER")
save(modeval, file = "SPECIES_NAME")

writeRaster(x = p, filename = "SPECIES_NAME.asc", format = "ascii", NAFlag = "-9999", overwrite = T)

rm(list = ls())

devtools::session_info()

##########################################################################################

##########################################################################################
######################## GET MODEL EVALUATION STATS ######################################
load(file = "SPECIES_NAME.rda")

#
aic.opt <- modeval@models[[which(modeval@results$delta.AICc==0)]]
aic.opt
aic.opt@lambdas
parse_lambdas(aic.opt) # available in rmaxent package

#modeval@overlap
#modeval@models

# Gives you a table with the stats for all models
results <- modeval@results
kable(results)


#
plot(modeval@predictions[[which(modeval@results$delta.AICc==0)]], main="Relative occurrence rate")

# Tells you which model is the best
which(results$delta.AICc == 0)

# Data frame and barplot of variable importance
df <- var.importance(aic.opt)
df
barplot(df$permutation.importance, names.arg=df$variable, las=2, ylab="Permutation Importance")

#
eval.plot(results)

# Plots all of the predictions by the different models
maps <- modeval@predictions
plot(maps)

# Plots the predictions of the best model?
plot(maps[[which(results$delta.AICc == 0)]], main = "Models with lowest AICc")

# Plots all of the response curves for the best model
for (i in which(results$delta.AICc == 0)) {
  response(modeval@models[[i]])
}


response(modeval@models[[aic.opt]])