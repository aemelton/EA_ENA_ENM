### AE Melton

#

#
setwd("~/Dropbox/Niche_Things/")
source("RequiredLibraries.R")
#

#
setwd("~/Dropbox/Niche_Things/Functions/")
files.sources = list.files()
sapply(files.sources, source)
#

#
setwd("PATH") # We can start with making queries to data aggregators for occurrence data
GetOccDat(method = "scrubr", 
                          usr.query = "Genus species", 
                          usr.db = c("idigbio", "gbif"), # See spocc package for all options
                          #user.ref = bio1, 
                          usr.output = "FILENAME.csv") ## Change species name
#

#
setwd("PATH_TO_ENVIRONMENTAL_DATA_FOLDER")
env.files <- list.files(pattern = ".tif", full.names = TRUE) # Check the file type and put in the appropriate pattern
envStack <- stack(env.files)
#names(envStack) <-  c("bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19",
#                      "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9",
#                      "CEC", "Clay", "Elevation", "OCD", "ORC", "PH", "Sand", "Silt", "Taxonomy")
envStack <- setMinMax(envStack)
#projection(envStack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs" # GeoTiffs include information like this; May also change if  you are using data from different regions or that were collected using different coordinate systems
plot(envStack[[1]])
#

#
setwd("PATH") # I always have all the data and outputs for a given ENM project within a sort of "home" folder. Then, within that fold, I have folders for occurrecne data, species spefic raster layers, and the ENMEval outputs
#

#
occ.dat <- read.csv("Point_Data/FILENAME.csv")[,2:3] # If you used the previous lines of code to pull in the occurrence data, the filrst column should be the species name, the second and thirsd should be long and lat, and a fourth should be the date of collection
plot(envStack[[1]])
points(occ.dat, pch = 19)
nrow(occ.dat)
#

#
# 
usr.occs <- SpatialPoints(occ.dat)
#

#
x <- circles(p = usr.occs)
pol.a <-  gUnaryUnion(x@polygons)
plot(pol.a)
hull_buff <-  gBuffer(pol.a, width=0.25)
plot(envStack[[1]])
plot(pol.a, add = T)
plot(hull_buff, add = T)
points(usr.occs)
#

# Some raster files can be quite large. Since the files are so big, we sometimes have to read them in and edit them one-by-one....
# I will be adding an option into the RasterLayerProcessing function to let it handle large raster stacks.
for(i in 1:length(env.files)){
  setwd("PATH_TO_ENVIRONMENTAL_DATA")
  file <- raster(env.files[i])
  setwd("PATH_TO_SPECIES_RASTERS_FOLDER") # This will need to be changed for each species
  SPECIESMTraining <- mask(file, hull_buff) # Masking the raster stack  
  SPECIESMTraining <- crop(x = SPECIESMTraining, y = hull_buff)
  SPECIESMTraining <- setMinMax(SPECIESMTraining)
  writeRaster(x = SPECIESMTraining, filename = names(file),
                format = "GTiff", NAFlag = "-9999", overwrite = T) # GeoTiffs can be much more memory efficient than other commonly used formats for environmental data
}
#

#
#rm(list = ls())
#
  
#
#training.region <- RasterLayerProcessing(raster.stack = envStack, usr.occs = occ.dat, method = "buff_poly", buff.size = 0.25)
#plot(training.region)
#

#############################################################################################

#
setwd("PATH_TO_SPECIES_RASTERS_FOLDER")
env.files <- list.files(pattern = ".tif", full.names = TRUE)
envStack <- stack(env.files)
#names(envStack) <-  c("bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "CEC",
#                      "Clay", "Elevation", "ORC", "PH", "Sand", "Silt", "Slope", "Taxonomy", "TRI")
envStack <- setMinMax(envStack)
#projection(envStack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
#plot(envStack[[1]])
#

# This uses the caret package and picks layers with the highest average correlation coefficients to drop
red.envStack <- ReduceEnvPred(usr.env = envStack, stat = "pearson", corr.cutoff = 0.8)
names(red.envStack)
#

#
rm(envStack)
#

#############################################################################################
#writeRaster(red.envStack,
#      filename = "TEST_", 
#      format = "GTiff",
#      bylayer = T,
#      suffix=names(red.envStack),
#      NAFlag = "-9999",
#      overwrite = T)
#############################################################################################

# Just some stats about the environmental layers. Will be incorporated into other functions down the road
N <- ncell(red.envStack[[1]])
NAcount <- cellStats(x = red.envStack[[1]], stat = "countNA") 
cell.count <- N - NAcount
cell.count
#

#
setwd("PATH")
#

#
occ.dat <- read.csv("Point_Data/FILENAME.csv")[,2:3]
plot(red.envStack[[1]])
points(occ.dat, pch = 19)
graphics.off() # This gets rid of all the graphics you have plotted in RStudio. CAn be nice to clear out the memory - some runs can use a lot of memory
nrow(occ.dat)
#cell.count/nrow(occ.dat)
#cell.count/1500
#

# 
# This will make sure that your point data all have data for every raster layer
occ.dat.Extract <- raster::extract(red.envStack, sapply(occ.dat, as.numeric))
occ.dat.Extract <- cbind(occ.dat, occ.dat.Extract)
clean_occ.dat.Extract <- occ.dat.Extract[complete.cases(occ.dat.Extract),]
occ.dat <- clean_occ.dat.Extract[,1:2]
nrow(occ.dat)
head(occ.dat)
rm(occ.dat.Extract)
rm(clean_occ.dat.Extract)
#
  
#
# This makes sure that only one point is in eah cell and can maximize the distances between points. Spatial biases in data lead to biased models.
rare.pts <- RarefyOccurrences(usr.occ = occ.dat, usr.ref = red.envStack[[1]], usr.max = 100)
bg.pts <- GetBackgroundPoints(usr.occ = rare.pts, samp.method = "all.m", n = 10000, usr.env = red.envStack)
plot(red.envStack[[1]])
points(bg.pts, col = "red", pch = 19)
points(occ.dat, col = "black", pch = 19)
points(rare.pts, col = "blue", pch = 19)
nrow(occ.dat)
nrow(rare.pts)
nrow(bg.pts)
graphics.off()
#
  
#
modeval <- ENMevaluate(occ = rare.pts[, c("longitude", "latitude")], 
                       env = red.envStack,
                       bg.coords = bg.pts,
                       #categoricals = "Taxonomy", 
                       algorithm = 'maxent.jar',
                       #RMvalues = seq(0.5, 4, 0.5),
                       #fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"),
                       method = "checkerboard2", #randomkfold, block, checkerboard2, jackknife for <= 25 pts
                       #kfolds = 5,
                       aggregation.factor = c(2,2),
                       #overlap = TRUE,
                       #clamp = TRUE,   
                       #rasterPreds = TRUE,
                       parallel = TRUE,
                       numCores = 4,
                       bin.output = TRUE)
                       #progbar = TRUE)
#
  
#
results <- modeval@results
results
which(results$avg.test.orMTP <= 0.05 & results$delta.AICc <= 2)
#kable(results)
#range(results$train.AUC)
#which(results$train.AUC >= 0.7)
#range(results$avg.test.AUC)
#which(results$avg.test.AUC >= 0.7)
#results$avg.test.orMTP
#which(results$avg.test.orMTP <= 0.05)
#results$delta.AICc
#which(results$delta.AICc <= 10)
#
  
#
#min.AICc <- which(results$delta.AICc == 0)
#min.AICc

#mm <- max(results$avg.test.AUC)
#max.test.auc <- which(results$avg.test.AUC == mm)
#max.test.auc

#nn <- min(results$avg.diff.AUC)
#min.diff.auc <- which(results$avg.diff.AUC == nn)
#min.diff.auc
#

#
#results[min.AICc,]
#results[max.test.auc,]
#results[min.diff.auc,]
#

#
#CBI <- GetCBI(modeval.usr.sp = modeval, usr.occs = occ.dat, usr.raster = red.envStack) # Non-parallelized
#CBI
results$delta.AICc[is.na(results$delta.AICc)] <- 999 # Models with param # > samp size get an NA for dAICc, which throws off the foreach loop, since dAICc < 10 is a requirement
num.cores <- detectCores()
CBI <- GetCBIv2(modeval.usr.sp = modeval, usr.occs = occ.dat, usr.raster = red.envStack, mod.results = results, or.threshold = 0.05, d.AICc.threshold = 2, num.clust = 2)
CBI
nrow(CBI)
#CE <- GetCE(modeval.usr.sp = modeval, usr.occs = occ.dat, usr.raster = red.envStack, usr.bg = bg.pts)
#CE
#

#
#results <- cbind(results,
#                 CBI$cbi.training.Spearman.cor,
#                 CBI$avg.cbi.testing)
                 #CE$ECE,
                 #CE$ECE.equal.width,
                 #CE$MCE)
                 #CE$MCE.equal.width)
#

# just going with 37 for oligocephala. simplest model params input (lowest mod number), many models equally good - mostly flat models; 33 for patens - 33, 39, 45 had least bad AUC and perfect or and dAIC; 13 for provinccialis
g <- max(CBI$avg.cbi.testing, na.rm = T)
g
beep <- which(CBI$avg.cbi.testing == g)[1]
max.test.CBI <- CBI$mod.num[beep]
max.test.CBI 
#results[23:24,]
modeval@predictions[[max.test.CBI]]  # raw output; Check model name! 
p <- predict(modeval@models[[max.test.CBI]], red.envStack) # Convert prediction to different output
plot(p) # logistic output
points(occ.dat, pch = 19)
graphics.off()
#

#
write.csv(x = results, file = "~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_ModelEvals_ENMEval.csv")
write.csv(x = CBI, file = "~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_ModelEvals_CBI.csv")
#

#
pdf("~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_SuitPlot.pdf")
plot(p) # logistic output
points(occ.dat, pch = 19, cex = 0.5)
dev.off()
#

#
setwd(dir = "~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/Amphicarpaea_bracteata/")
var.imp <- var.importance(modeval@models[[max.test.CBI]])
write.csv(x = var.imp, file = "Amphicarpaea_bracteata_Variable_Importance.csv")
lambdas <- parse_lambdas(modeval@models[[max.test.CBI]])
save(lambdas, file = "Amphicarpaea_bracteata_Lambdas.RDA")
save(modeval, file = "Amphicarpaea_bracteata_ENMEval.RDA")
writeRaster(x = p, filename = "Amphicarpaea_bracteata", format = "GTiff", NAFlag = "-9999", overwrite = T)
#

#
bin.mod <- ThresholdModel(usr.raster = p, usr.occs = occ.dat, method = "MPT", output.type = "binary")
plot(bin.mod)
points(occ.dat)
writeRaster(x = bin.mod, filename = "Amphicarpaea_bracteata_BIN", format = "GTiff", NAFlag = "-9999", overwrite = T)

pdf("~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_BinPlot.pdf")
plot(bin.mod) # logistic output
points(rare.pts, pch = 19, cex = 0.5)
dev.off()
#

#############################################################################################
#
pca <- rasterPCA(img = envStack, spca = T) #
summary(pca$model)

PC1 <- pca$map$PC1
PC2 <- pca$map$PC2
PC3 <- pca$map$PC3
PC4 <- pca$map$PC4
PC5 <- pca$map$PC5
PC6 <- pca$map$PC6
PC7 <- pca$map$PC7
pcaStack <- stack(PC1, PC2, PC3, PC4, PC5) #, PC6) #, PC7)
plot(pcaStack)

HypVol <- GetHypervolume(bin.proj = bin.mod, usr.raster = pcaStack, do.scale = F, do.center = F, set.scale = F)
#

#
niche.widths <- BetaNicheWidth(usr.occs = occ.dat, usr.sdm = bin.mod, method = "sdm", usr.raster = envStack)
#

#
PlotEcoGeoCurves(method = "distribution", usr.env = envStack, usr.raster = bin.mod, usr.variable = "bio15")
#

#############################################################################################
# RERUN MODEL SELECTIONS AND IMAGE PRINTING FOR ALL AFTER PROVINCIALIS UNTIL TENUIFOLIA
setwd("~/Dropbox/UF_Research/Fabiana_ENM/")
occ.dat <- read.csv("Point_data/Amphicarpaea_bracteata_EDITS.csv")[,2:3]

#############################################################################################

#
setwd("~/Dropbox/UF_Research/Fabiana_ENM/Species_Layers/Amphicarpaea_bracteata/")
env.files <- list.files(pattern = ".tif", full.names = TRUE)
envStack <- stack(env.files)
#names(envStack) <-  c("bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "CEC",
#                      "Clay", "Elevation", "ORC", "PH", "Sand", "Silt", "Slope", "Taxonomy", "TRI")
envStack <- setMinMax(envStack)
#projection(envStack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
plot(envStack[[1]])
#

#
red.envStack <- ReduceEnvPred(usr.env = envStack, stat = "pearson", corr.cutoff = 0.8)
names(red.envStack)
#

#
rm(envStack)
#

#############################################################################################

setwd("~/Dropbox/UF_Research/Fabiana_ENM/ENMEval_Outputs/Amphicarpaea_bracteata/")
load(file = "Amphicarpaea_bracteata_ENMEval.RDA")
CBI <- read.csv("Amphicarpaea_bracteata_ModelEvals_CBI.csv")
g <- max(CBI$avg.cbi.testing, na.rm = T)
g
beep <- which(CBI$avg.cbi.testing == g)[1]
max.test.CBI <- CBI$mod.num[beep]
max.test.CBI
#results[23:24,]
modeval@predictions[[max.test.CBI]]  # raw output; Check model name! 
p <- predict(modeval@models[[max.test.CBI]], red.envStack) # Convert prediction to different output
plot(p) # logistic output
points(occ.dat, pch = 19)
#

#
#
#write.csv(x = results, file = "~/Dropbox/UF_Research/Fabiana_ENM/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_ModelEvals_ENMEval.csv")
#write.csv(x = CBI, file = "~/Dropbox/UF_Research/Fabiana_ENM/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_ModelEvals_CBI.csv")
#

#
pdf("~/Dropbox/UF_Research/Fabiana_ENM/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_SuitPlot.pdf")
plot(p) # logistic output
points(occ.dat, pch = 19)
dev.off()
#

#
setwd(dir = "~/Dropbox/UF_Research/Fabiana_ENM/ENMEval_Outputs/Amphicarpaea_bracteata/")
var.imp <- var.importance(modeval@models[[max.test.CBI]])
write.csv(x = var.imp, file = "Amphicarpaea_bracteata_Variable_Importance.csv")
lambdas <- parse_lambdas(modeval@models[[max.test.CBI]])
save(lambdas, file = "Amphicarpaea_bracteata_Lambdas.RDA")
#save(modeval, file = "Amphicarpaea_bracteata_ENMEval.RDA")
writeRaster(x = p, filename = "Amphicarpaea_bracteata", format = "GTiff", NAFlag = "-9999", overwrite = T)
#

#
bin.mod <- ThresholdModel(usr.raster = p, usr.occs = occ.dat, method = "MPT", output.type = "binary")
plot(bin.mod)
points(occ.dat)
writeRaster(x = bin.mod, filename = "Amphicarpaea_bracteata_BIN", format = "GTiff", NAFlag = "-9999", overwrite = T)

pdf("~/Dropbox/UF_Research/Fabiana_ENM/ENMEval_Outputs/Amphicarpaea_bracteata/Amphicarpaea_bracteata_BinPlot.pdf")
plot(bin.mod) # logistic output
points(modeval@occ.pts, pch = 19)
dev.off()
#
