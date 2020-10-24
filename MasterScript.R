### AE Melton
# This is a "master" script for a group of functions I wrote to help with ENM/SDM analyses.
#

#############################################################################################
##### Install / load libraries and set folder paths #####
#############################################################################################

#
source("~/Dropbox/Niche_Things/RequiredLibraries.R") # and sources function scripts
sessionInfo()
project.folder <- "SET_TO_FOLDER_PATH" # Set the main "umbrella" folder that all subfolders are in
setwd(project.folder)
#

#
num.cores <- detectCores() # For running functions in parallel
#

#############################################################################################
##### Obtain occurrence data #####
#############################################################################################
#
setwd(project.folder)
Point.Data.folder <- paste0(project.folder, "Point_Data/")
dir.create(Point.Data.folder)
setwd(Point.Data.folder)
#

#
GetOccurrenceData(method = "scrubr", 
                  usr.query = "Genus species", 
                  usr.db = c("idigbio", "gbif"),# See spocc package for details and more options
                  #user.ref = bio1, 
                  usr.output = "Genus_species.csv")
#

#############################################################################################
##### Data prep #####
#############################################################################################
setwd(project.folder)
dir.create("Species_Layers")
dir.create("ENMEval_Outputs")
Species.Layers.folder <- paste0(project.folder, "Species_Layers/")
ENMEval.Outputs.folder <- paste0(project.folder, "ENMEval_Outputs/")
#

#
setwd(Point.Data.folder)
point.data.list <- list.files(pattern = ".csv", full.names = TRUE) # Make a list of all species output folders
point.data.list
species.list <- gsub(pattern = "./", replacement = '', x = point.data.list)
species.list <- gsub(pattern = ".csv", replacement = '', x = species.list)
species.list
#

#
#############################################################################################
##### Data prep #####
#############################################################################################
#
species.names <- ""
#

#
print(species.name)
species.folder <- species.name
print("Loading unclipped rasters") 
# Load environmental rasters
#setwd("PATH_TO_ENVIRONMENTAL_DATA_FOLDER")
#env.files <- list.files(pattern = ".tif", full.names = TRUE)
#envStack <- stack(env.files)
#names(envStack) <-  c("bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19",
#                      "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9",
#                      "CEC", "Clay", "Elevation", "OCD", "ORC", "PH", "Sand", "Silt", "Taxonomy")
#envStack <- setMinMax(envStack)
#projection(envStack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
#plot(envStack[[1]])
#

#
setwd(Point.Data.folder)
#

#
occ.dat <- read.csv(point.data.list[1])[,2:3] #change [#] as needed 
#plot(envStack[[1]])
#points(occ.dat, pch = 19)
#nrow(occ.dat)
#

#
setwd(Species.Layers.folder)
dir.create(species.name)
setwd(species.name)
#

#
print("Clipping raster layers")
# Functions to prepare raster layers
#shpfile <- readOGR(dsn = "PATH_TO_SHAPEFILE")
#plot(shpfile)
#training.region <- RasterLayerProcessing(usr.env = envStack, mask.method = "shapefile", buffer.size = 0, shape = shpfile)
#RasterLayerProcessingByLoopMaskByShape(shape = shpfile,
#                                       rasters.folder = "PATH_TO_ENVIRONMENTAL_DATA",
#                                       na.flag = "NA",
#                                       output.format = "GTiff",
#                                       output.folder = "OUTPUT FOLDER")

RasterLayerProcessingByLoop(usr.occs = occ.dat, buffer.size = 0.25, path.to.rasters = "PATH_TO_ENVIRONMENTAL_DATA", output.format = "GTiff")
#plot(training.region[[1]])
#writeRaster(x = training.region, filename = species.name, bylayer = T, suffix=names(training.region), format = "GTiff", NAFlag = "NA", overwrite = T)
#

#
#graphics.off()
#

#
#rm(list = ls())
#
#############################################################################################
##### Modeling #####
#############################################################################################
# Load species-specific raster layers
print("Loading clipped layers")
setwd(project.folder)
setwd(Species.Layers.folder)
setwd(species.name)
env.files <- list.files(pattern = ".tif", full.names = TRUE)
envStack <- stack(env.files)
envStack <- setMinMax(envStack)
#

#
print("Predictor variable reduction")
#
red.envStack <- ReduceEnvPred(usr.env = envStack, stat = "pearson", corr.cutoff = 0.8) # Uses the caret package to get identify and drop highly correlated layers
KeptLayers <- names(red.envStack)
rm(envStack)
#plot(red.envStack[[1]])
#

#
#graphics.off()
#

#
#N <- ncell(red.envStack[[1]])
#NAcount <- cellStats(x = red.envStack[[1]], stat = "countNA") 
#cell.count <- N - NAcount
#cell.count
#

#
#setwd(Point.Data.folder)
#

# Load occurrence data
#occ.dat <- read.csv(point.data.list[1])[,2:3]
#plot(red.envStack[[1]])
#points(occ.dat, pch = 19)
#nrow(occ.dat)
#

#
#cell.count/nrow(occ.dat)
#cell.count/1500
#

#
#graphics.off()
#

#
print("Rarefy point data... Sample background points")
# Make sure you don't have occurrences lacking environmental data
occ.dat.Extract <- raster::extract(red.envStack, sapply(occ.dat, as.numeric))
occ.dat.Extract <- cbind(occ.dat, occ.dat.Extract)
clean_occ.dat.Extract <- occ.dat.Extract[complete.cases(occ.dat.Extract),]
occ.dat <- clean_occ.dat.Extract[,1:2]
#nrow(occ.dat)
#head(occ.dat)
rm(occ.dat.Extract)
rm(clean_occ.dat.Extract)
#
  
# Rarefy occurrence data to one point per cell or to a maximum number with distances maximized between points #used all.m for oligocephala
rare.pts <- RarefyOccurrences(usr.occ = occ.dat, usr.ref = red.envStack[[1]], usr.max = nrow(occ.dat)) # this reduces to just one per cell, can be set to any max number though
bg.pts <- GetBackgroundPoints(usr.occ = rare.pts, samp.method = "all.m", n = 10000, usr.env = red.envStack)
#plot(red.envStack[[1]])
#points(bg.pts, col = "red", pch = 19)
#points(occ.dat, col = "black", pch = 19)
#points(rare.pts, col = "blue", pch = 19)
colnames(rare.pts) <- c("longitude", "latitude")
#nrow(occ.dat)
#nrow(rare.pts)
#nrow(bg.pts)
#

#
#graphics.off() # Remove plots
#

# Pick a data partition method for model evaluation. This is a simple example for when I'm not tranferring models.
# Other uses may require different partitioning methods.

if(nrow(rare.pts) <= 50){
  eval.meth <- "jackknife"
}else{
  eval.meth <- "checkerboard2"
}
#

#
print("Running ENMEval")
#
modeval <- ENMevaluate(occ = rare.pts[, c("longitude", "latitude")], 
                       env = red.envStack,
                       bg.coords = bg.pts,
                       #categoricals = "Taxonomy", 
                       algorithm = 'maxent.jar',
                       #RMvalues = seq(0.5, 4, 0.5), # DEFAULT
                       #fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), # DEFAULT
                       method = eval.meth,
                       #overlap = TRUE, # DEFAULT
                       #clamp = TRUE, # DEFAULT
                       #rasterPreds = TRUE, # DEFAULT
                       parallel = TRUE,
                       numCores = num.cores,
                       bin.output = TRUE)
                       #progbar = TRUE # DEFAULT)
#
  
#
results <- modeval@results
results
#

#
if(nrow(rare.pts) < 20){
  or.threshold <- 1/(nrow(rare.pts))
  }else{
  or.threshold <- 0.05
}
which(results$avg.test.orMTP <= or.threshold & results$delta.AICc <= 2) # For < 20 pts, results$avg.test.orMTP <= (1/(nrow(rare.pts)))
mod.num <- which(results$avg.test.orMTP <= or.threshold & results$delta.AICc <= 2) # For < 20 pts, results$avg.test.orMTP <= (1/(nrow(rare.pts)))
mod.num
#

#
results$delta.AICc[is.na(results$delta.AICc)] <- 999 # Models with param # > samp size get an NA for dAICc, which throws off the foreach loop
#

#
print("Calculating CBI")
# Parallelized for multiple models
CBI <- GetCBIv2(modeval.usr.sp = modeval,
                usr.occs = occ.dat,
                usr.raster = red.envStack,
                mod.results = results, 
                or.threshold = or.threshold, 
                d.AICc.threshold = 2,
                num.clust = num.cores)#,
#                nclass = 100,
#                window.w = "default",
#                res = 100)
#CBI
#

# One model; Way more mem efficient. RAM usage drops and slowly builds back up when you start a new one, unlike with the parallelized version
#CBI <- GetCBI(modeval.usr.sp = modeval,
#                usr.occs = occ.dat,
#                usr.raster = red.envStack,
#                mod.results = results,
#                mod.num = mod.num[1])
#CBI.b <- GetCBI(modeval.usr.sp = modeval,
#                usr.occs = occ.dat,
#                usr.raster = red.envStack,
#                mod.results = results,
#                mod.num = mod.num[2])
#CBI.c <- GetCBI(modeval.usr.sp = modeval,
#                usr.occs = occ.dat,
#                usr.raster = red.envStack,
#                mod.results = results,
#                mod.num = mod.num[3])
#CBI.d <- GetCBI(modeval.usr.sp = modeval,
#                usr.occs = occ.dat,
#                usr.raster = red.envStack,
#                mod.results = results,
#                mod.num = mod.num[4])
#CBI.e <- GetCBI(modeval.usr.sp = modeval,
#                usr.occs = occ.dat,
#                usr.raster = red.envStack,
#                mod.results = results,
#                mod.num = mod.num[5])
#CBI.f <- GetCBI(modeval.usr.sp = modeval,
#                usr.occs = occ.dat,
#                usr.raster = red.envStack,
#                mod.results = results,
#                mod.num = mod.num[6])
#CBI <- rbind(CBI, CBI.b)#, CBI.c)#, CBI.d, CBI.e)#, CBI.f)
#CBI
#

#
results.trim <- results[mod.num,]
candidate.model.results <- cbind(results.trim, CBI)
#

#
print("Generating prediction rasters and outputs")
g <- max(CBI$cbi.training.Spearman.cor, na.rm = T)
g
beep <- which(CBI$cbi.training.Spearman.cor == g)[1]
max.CBI <- CBI$mod.num[beep]
max.CBI
#results[23:24,]
modeval@predictions[[max.CBI]]  # raw output; Check model name! 
p <- predict(modeval@models[[max.CBI]], red.envStack) # Convert prediction to different output
plot(p) # logistic output
points(occ.dat, pch = 19, cex = 0.25)
#

#
setwd(ENMEval.Outputs.folder)
dir.create(species.name)
setwd(species.name)
#

#
write.csv(x = rare.pts, file = "rare_pts.csv")[,2:3]
write.csv(x = bg.pts, file = "bg_pts.csv")[,2:3]
write.csv(x = results, file = "ModelEvals_ENMEval.csv")
write.csv(x = CBI, file = "ModelEvals_CBI.csv")
write.csv(x = candidate.model.results, file = "CandidateModelResults.csv")
write.csv(x = KeptLayers, file = "KeptLayers.csv")
#

#
pdf("SuitPlot.pdf")
plot(p) # logistic output
points(occ.dat, pch = 19, cex = 0.25)
dev.off()
#

#
var.imp <- var.importance(modeval@models[[max.CBI]])
write.csv(x = var.imp, file = "Variable_Importance.csv")
lambdas <- parse_lambdas(modeval@models[[max.CBI]])
save(lambdas, file = "Lambdas.RDA")
save(modeval, file = "ENMEval.RDA")
writeRaster(x = p, filename = species.name, format = "GTiff", NAFlag = "-9999", overwrite = T)
#

#
bin.mod <- ThresholdModel(usr.raster = p, usr.occs = occ.dat, method = "95pct", output.type = "binary")
plot(bin.mod)
points(occ.dat)
binary.raster.name <- paste0(species.name, "_BIN")
writeRaster(x = bin.mod, filename = binary.raster.name, format = "GTiff", NAFlag = "-9999", overwrite = T)
dev.off()

pdf("BinPlot.pdf")
plot(bin.mod) # logistic output
points(rare.pts, pch = 19, cex = 0.25)
dev.off()
#

#
graphics.off()
#
#}
#############################################################################################
##### Post-Modeling #####
#############################################################################################

# Raster breadth section. Loops over all of the predictions rasters to calculate breadth using ENMTools
#
setwd("PATH_TO_PREDICTION_RASTERS")
#

#
df <- GetRasterBreadths()
setwd(project.folder)
write.csv(x = df, file = "raster_breadth.csv", row.names = F)
#
#############################################################################################
# Turn all the ENM prediction rasters into binary estimated distributions
# Assumes nested series of folders and file names
setwd(ENMEval.Outputs.folder)
#

#
folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
folder.list
species.list <- gsub(pattern = "./", replacement = '', x = folder.list)
#

#
GetThresholdedPredictionRasters()
#
#############################################################################################
#############################################################################################

# Remake spreadsheet with all the ENMEval stats and some modeling input data
# Assumes nested series of folders and file names
setwd(ENMEval.Outputs.folder)
#

#
folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
folder.list
species.list <- gsub(pattern = "./", replacement = '', x = folder.list)
species.list
#

#
df <- data.frame(species = character(),
                 rare.pts = character(),
                 bg.pts = character(),
                 partition.method = character(),
                 train.AUC = character(),
                 avg.test.AUC = character(),
                 avg.diff.AUC = character(),
                 avg.test.orMTP = character(),
                 delta.AICc = character(),
                 g = character()) # Generate empty data frame to put results in
#

#
for(i in 1:length(folder.list)){
  #
  setwd(folder.list[i])
  
  tmp.species <- gsub(pattern = "_", replacement = " ", x = species.name)
  
  rare.pts <- read.csv("rare_pts.csv")[,2:3]
  colnames(rare.pts) <- c("longitude", "latitude")
  
  bg.pts <- read.csv("bg_pts.csv")[,2:3]
  colnames(bg.pts) <- c("longitude", "latitude")
  
  mod.file <- paste0(species.name, "_ENMEval.RDA")
  load(file = mod.file)
  partition.method <- modeval@partition.method[1]
  
  results.file <- paste0(species.name, "_ModelEvals_ENMEval.csv")
  results <- read.csv(results.file)
  CBI.file <- paste0(species.name, "_ModelEvals_CBI.csv")
  CBI <- read.csv(CBI.file)
  g <- max(CBI$cbi.training.Spearman.cor, na.rm = T)
  beep <- which(CBI$cbi.training.Spearman.cor == g)[1]
  mod.num <- CBI$mod.num[beep]
  results.trim <- results[mod.num,]
  candidate.model.results <- cbind(results.trim, g, partition.method)
  candidate.model.results <- candidate.model.results[,c("partition.method", "train.AUC", "avg.test.AUC", "avg.diff.AUC", "avg.test.orMTP", "delta.AICc", "g")]
  tmp.file.name <- paste0(species.name, "CandidateModelResults.csv")
  write.csv(x = candidate.model.results, file = tmp.file.name)
  tmp <- cbind(tmp.species, nrow(rare.pts), nrow(bg.pts), candidate.model.results)
  df <- rbind(df, tmp)
  setwd("~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/")
}
#

#
colnames(df) <- c("Species", "Rarefied Points", "Background Points", "Parition Method", "Training AUC", "Average Test AUC", "Average AUC Difference", "Averate Test orMTP", "Delta AICc", "CBI")
df
write.csv(x = df, file = "../ENM_Data_and_Evaluations.csv", row.names = F)
#

#############
csv <- read.csv("ENM_Data_and_Evaluations.csv")
head(csv)
EA <- csv[csv$Region == "EA",]
ENA <- csv[csv$Region == "ENA",]
#

#
df <- data.frame(mean(csv$Rarefied.Points), sd(csv$Rarefied.Points), range(csv$Rarefied.Points),
           mean(csv$Training.AUC), sd(csv$Training.AUC), range(csv$Training.AUC),
           mean(csv$Average.Test.AUC), sd(csv$Average.Test.AUC), range(csv$Average.Test.AUC),
           mean(csv$Average.AUC.Difference), sd(csv$Average.AUC.Difference), range(csv$Average.AUC.Difference),
           mean(csv$Average.Test.orMTP), sd(csv$Average.Test.orMTP), range(csv$Average.Test.orMTP),
           mean(csv$Delta.AICc), sd(csv$Delta.AICc), range(csv$Delta.AICc),
           mean(csv$CBI), sd(csv$CBI), range(csv$CBI))
df
write.csv(x = df, file = "Summary_Stats_for_Model_Evals.csv", row.names = F)

low.or <- subset(x = csv$Average.Test.orMTP, csv$Average.Test.orMTP <= 0.05) # 58 >= 0.7
length(low.or)

high.test.AUC <- subset(x = csv$Average.Test.AUC, csv$Average.Test.AUC >= 0.5) # 58 >= 0.7
length(high.test.AUC)

low.dAUC <- subset(x = csv$Average.AUC.Difference, csv$Average.AUC.Difference <= 0.05)
length(low.dAUC)

low.dAICc <- subset(x = csv$Delta.AICc, csv$Delta.AICc == 0)
length(low.dAICc)

low.cbi <- subset(x = csv$CBI, csv$CBI <= 0.7)
length(low.cbi)
#

#############
csv <- read.csv("ENM_Data_and_Evaluations.csv")
head(csv)
csv <- csv[csv$Region == "EA",]
#ENA <- csv[csv$Region == "ENA",]
#

#
df <- data.frame(mean(csv$Rarefied.Points), sd(csv$Rarefied.Points), range(csv$Rarefied.Points),
                 mean(csv$Training.AUC), sd(csv$Training.AUC), range(csv$Training.AUC),
                 mean(csv$Average.Test.AUC), sd(csv$Average.Test.AUC), range(csv$Average.Test.AUC),
                 mean(csv$Average.AUC.Difference), sd(csv$Average.AUC.Difference), range(csv$Average.AUC.Difference),
                 mean(csv$Average.Test.orMTP), sd(csv$Average.Test.orMTP), range(csv$Average.Test.orMTP),
                 mean(csv$Delta.AICc), sd(csv$Delta.AICc), range(csv$Delta.AICc),
                 mean(csv$CBI), sd(csv$CBI), range(csv$CBI))
df
write.csv(x = df, file = "EA_Summary_Stats_for_Model_Evals.csv", row.names = F)
#

#
csv <- read.csv("ENM_Data_and_Evaluations.csv")
head(csv)
#EA <- csv[csv$Region == "EA",]
csv <- csv[csv$Region == "ENA",]
#

#
df <- data.frame(mean(csv$Rarefied.Points), sd(csv$Rarefied.Points), range(csv$Rarefied.Points),
                 mean(csv$Training.AUC), sd(csv$Training.AUC), range(csv$Training.AUC),
                 mean(csv$Average.Test.AUC), sd(csv$Average.Test.AUC), range(csv$Average.Test.AUC),
                 mean(csv$Average.AUC.Difference), sd(csv$Average.AUC.Difference), range(csv$Average.AUC.Difference),
                 mean(csv$Average.Test.orMTP), sd(csv$Average.Test.orMTP), range(csv$Average.Test.orMTP),
                 mean(csv$Delta.AICc), sd(csv$Delta.AICc), range(csv$Delta.AICc),
                 mean(csv$CBI), sd(csv$CBI), range(csv$CBI))
df
write.csv(x = df, file = "ENA_Summary_Stats_for_Model_Evals.csv", row.names = F)
#

#
csv <- read.csv("ENM_Data_and_Evaluations.csv")
head(csv)
EA <- csv[csv$Region == "EA",]
ENA <- csv[csv$Region == "ENA",]
t.test(EA$Training.AUC, ENA$Training.AUC)
t.test(EA$Average.Test.AUC, ENA$Average.Test.AUC)
t.test(EA$Average.Test.orMTP, ENA$Average.Test.orMTP)
t.test(EA$CBI, ENA$CBI)
#
#############################################################################################
#
setwd(ENMEval.Outputs.folder)
#

#
folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
folder.list
#

#
df <- data.frame(longitude = character(), latitude = character())
for(i in 1:length(folder.list)){
  #
  setwd(folder.list[i])
  
  bg.pts <- read.csv("bg_pts.csv")[,2:3]
  colnames(bg.pts) <- c("longitude", "latitude")
  
  df <- rbind(df, bg.pts)
  setwd(project.folder)
  }
df
write.csv(x = df, file = "../all_bg_points.csv")
big.bg.pts.trim <- gridSample(xy = big.bg.pts, r = env[[1]], n = 1)
write.csv(x = big.bg.pts.trim, file = "all_bg_points_ONE_PER_CELL.csv")
#
#############################################################################################
# Get environmental breadths using env.breadth from ENMTools
df <- GetLevinsEnvBreadth(ENMEval.Outputs.folder = ENMEval.Outputs.folder, Species.Layers.folder = Species.Layers.folder, max.reps = 1000, tolerance = 1e-05)
df
# Not sure why need unlist... will investigate...
df$Species <- unlist(df$Species)
df$env.B2 <- unlist(df$env.B2)
colnames(df) <- c("Species", "Environmental Breadth")
df
write.csv(x = df, file = "~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/environmental_breadth.csv", row.names = F)
#

#############################################################################################
setwd("PCA_Layers_nSamp_1mil/")
env.files <- list.files(pattern = ".tif", full.names = TRUE)
env <- stack(env.files)
env <- setMinMax(env)

setwd(project.folder)
big.bg.pts <- read.csv(file = "all_bg_points_ONE_PER_CELL.csv", header = TRUE)

df <- GetNicheAreas(env = env, big.bg.pts = big.bg.pts)
write.csv(x = df, file = "niche_areas.csv", row.names = FALSE)
#
#############################################################################################
#
setwd("SDM_95pct/")
df <- GetGeographicAreaSizes()
df
colnames(df) <- c("Species", "Geographic Area")
write.csv(x = df, file = "../Geographic_Area_Estimate.csv", row.names = F)
#
#############################################################################################

#############################################################################################
##### Statistical tests #####
#############################################################################################
#
setwd(project.folder)
#

#
csv <- read.csv(file = "environmental_breadth.csv")
data.frame(min(csv$Environmental.Breadth), max(csv$Environmental.Breadth), mean(csv$Environmental.Breadth), sd(csv$Environmental.Breadth))
EA <- csv[csv$Region == "EA",]
ENA <- csv[csv$Region == "ENA",]

env.breadth.comp <- wilcox.test(x = EA$Environmental.Breadth, y = ENA$Environmental.Breadth) # B2 is reference by DLW in a warren et al paper

ea.df <- data.frame(mean(EA$Environmental.Breadth), sd(EA$Environmental.Breadth))
ena.df <- data.frame(mean(ENA$Environmental.Breadth), sd(ENA$Environmental.Breadth))
#

#
raster.breadth.results <- read.csv("~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/raster_breadth.csv")
data.frame(min(raster.breadth.results$B2), max(raster.breadth.results$B2), mean(raster.breadth.results$B2), sd(raster.breadth.results$B2))

EA <- raster.breadth.results[raster.breadth.results$Region == "EA",]
ENA <- raster.breadth.results[raster.breadth.results$Region == "ENA",]

#wilcox.test(x = EA$B1, y = ENA$B1) # Mann-Whitney test
rast.breadth.comp <- wilcox.test(x = EA$B2, y = ENA$B2) # B2 is reference by DLW in a warren et al paper

ea.df <- data.frame(mean(EA$B2), sd(EA$B2))
ena.df <- data.frame(mean(ENA$B2), sd(ENA$B2))
#

#
df <- cbind(ea.env.mean, ena.env.mean, env.breadth.comp$statistic, env.breadth.comp$null.value, env.breadth.comp$p.value, ea.rast.mean, ena.rast.mean, rast.breadth.comp$statistic, rast.breadth.comp$null.value, rast.breadth.comp$p.value)
df
write.csv(x = df, file = "raster_and_env_b2_comp.csv")
#

#
csv <- read.csv(file = "Niche_Area_and_95pct_G_Breadth_N_nucifera_CORRECTED.csv")
data.frame(min(csv$Geographic.Breadth..95..Threshold.), max(csv$Geographic.Breadth..95..Threshold.), mean(csv$Geographic.Breadth..95..Threshold.), sd(csv$Geographic.Breadth..95..Threshold.))
EA <- csv[csv$Region == "EA",]
ENA <- csv[csv$Region == "ENA",]

rast.breadth.comp <- wilcox.test(x = EA$Geographic.Breadth..95..Threshold., y = ENA$Geographic.Breadth..95..Threshold.) # B2 is reference by DLW in a warren et al paper

ea.df <- data.frame(mean(EA$Geographic.Breadth..95..Threshold.), sd(EA$Geographic.Breadth..95..Threshold.))
ena.df <- data.frame(mean(ENA$Geographic.Breadth..95..Threshold.), sd(ENA$Geographic.Breadth..95..Threshold.))

df <- cbind(rast.breadth.comp$statistic[[1]], rast.breadth.comp$p.value, ea.df$mean.EA.Geographic.Breadth..95..Threshold.., ea.df$sd.EA.Geographic.Breadth..95..Threshold.., ena.df$mean.ENA.Geographic.Breadth..95..Threshold.., ena.df$sd.ENA.Geographic.Breadth..95..Threshold..)
df
colnames(df) <- c("W statistic", "P-value", "EA mean", "EA SD", "ENA mean", "ENA SD")
write.csv(x = df, file = "95pct_raster_b2_comp_V2.csv")

#
#############################################################################################

### Background tests
#
setwd(ENMEval.Outputs.folder)
folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
folder.list
species.list <- gsub(pattern = "./", replacement = '', x = folder.list)
species.list
genera.list.all <- gsub(pattern = "_.*", replacement = '', x = species.list)
genera.list <- unique(genera.list.all)                
genera.list
#

#
setwd("~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/PCA_Layers_nSamp_1mil/")
env.files <- list.files(pattern = ".tif", full.names = TRUE)
env <- stack(env.files)
env <- setMinMax(env)
#

#
output.folder <- "~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/" # Set the main "umbrella" folder that all subfolders are in
setwd(output.folder)

ecospat.folder <- "~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/EcoSpat_BG_Test_Outputs/"
#

#
species.list
species.1 <- "Gymnocladus_chinensis" #
species.2 <- "Gymnocladus_dioicus" #

DoEcoSpatBGTest(species.1 = species.1, species.2 = species.2, env = env,
                test.type = "symmetric", bg.source = "points",
                output.folder = output.folder, ecospat.folder = ecospat.folder,
                R = 100, th.sp = 0, th.env = 0, nreps = 1000)
#

#
setwd(ecospat.folder)
output.files <- list.files(pattern = "*.csv", full.names = TRUE)
df <- data.frame(Species.1 = character(), Species.2 = character(), Obs.D = character(), Obs.I = character(), p.D = character(), p.I = character())
for(i in 1:length(output.files)){
  #
  csv.filename <- output.files[i]
  Species.Names <- gsub(pattern = "_BG_Test_Out.csv", replacement = "", x = csv.filename)
  Species.Names <- gsub(pattern = "./", replacement = "", x = Species.Names)
  boop <- strsplit(sub('(^[^_]+_[^_]+)_(.*)$', '\\1 \\2', Species.Names), ' ') # Thank you akron on https://stackoverflow.com/questions/32398427/r-split-a-character-string-on-the-second-underscore # OK, this didn't work for taxa w/ subspecies. Oops.
  Species.1 <- boop[[1]][1]
  Species.2 <- boop[[1]][2]
  csv <- read.csv(csv.filename)
  colnames(csv) <- c("Obs.D", "Obs.I", "p.D", "p.I")
  tmp <- data.frame(Species.1, Species.2, csv)
  #colnames(bg.pts) <- c("longitude", "latitude")
  
  df <- rbind(df, tmp)
}
df
write.csv(x = df, file = "All_BG_Tests_Out.csv", row.names = FALSE)
#
#############################################################################################
csv <- read.csv(file = "Niche_Area_and_95pct_G_Breadth_N_nucifera_CORRECTED.csv")
head(csv)
EA <- csv[csv$Region == "EA",]
ENA <- csv[csv$Region == "ENA",]

#wilcox.test(x = EA$B1, y = ENA$B1) # Mann-Whitney test
niche.area.comp <- wilcox.test(x = EA$Geographic.Breadth..95..Threshold., y = ENA$Geographic.Breadth..95..Threshold.) 

data.frame(min(csv$Geographic.Breadth..95..Threshold.), max(csv$Geographic.Breadth..95..Threshold.), mean(csv$Geographic.Breadth..95..Threshold.), sd(csv$Geographic.Breadth..95..Threshold.))
ea.df <- data.frame(mean(EA$Geographic.Breadth..95..Threshold.), sd(EA$Geographic.Breadth..95..Threshold.))
ena.df <- data.frame(mean(ENA$Geographic.Breadth..95..Threshold.), sd(ENA$Geographic.Breadth..95..Threshold.))
#

#
df <- cbind(niche.area.comp$statistic,
            niche.area.comp$null.value,
            niche.area.comp$p.value,
            ea.df$mean.EA.Geographic.Breadth..95..Threshold..,
            ea.df$sd.EA.Geographic.Breadth..95..Threshold..,
            ena.df$mean.ENA.Geographic.Breadth..95..Threshold..,
            ena.df$sd.ENA.Geographic.Breadth..95..Threshold..) 
df
write.csv(x = df, file = "g_space_comp_N_nucifera_corrected.csv", row.names = FALSE)
#############################################################################################

#############################################################################################
csv <- read.csv(file = "All_BG_Tests_Out_w_Regions_R2.csv")
head(csv)
EA.EA <- csv[csv$Comparison == "EA-EA",]
EA.ENA <- csv[csv$Comparison == "EA-ENA",]
ENA.ENA <- csv[csv$Comparison == "ENA-ENA",]
nrow(csv)
nrow(EA.EA) + nrow(EA.ENA) + nrow(ENA.ENA) # Just making sure everyhing made it in....

kruskal.test(Obs.D ~ Comparison, data = csv)

ea.ea.D.mean <- mean(EA.EA$Obs.D)
ea.ea.D.var <- var(EA.EA$Obs.D)
ea.ea.D.sd <- sd(EA.EA$Obs.D)

ea.ena.D.mean <- mean(EA.ENA$Obs.D)
ea.ena.D.var <- var(EA.ENA$Obs.D)
ea.ena.D.sd <- sd(EA.ENA$Obs.D)

ena.ena.D.mean <- mean(ENA.ENA$Obs.D)
ena.ena.D.var <- var(ENA.ENA$Obs.D)
ena.ena.D.sd <- sd(ENA.ENA$Obs.D)

df <- data.frame(ea.ea.D.mean, ea.ea.D.var, ea.ea.D.sd,
                 ea.ena.D.mean, ea.ena.D.var, ea.ena.D.sd,
                 ena.ena.D.mean, ena.ena.D.var, ena.ena.D.sd)
df
write.csv(x = df, file = "BackGround_Test_Comparison_Summary_Stats.csv", row.names = F)
#

#
sig <- subset(csv, p.D <= 0.05)

EA.EA <- sig[sig$Comparison == "EA-EA",]
EA.ENA <- sig[sig$Comparison == "EA-ENA",]
ENA.ENA <- sig[sig$Comparison == "ENA-ENA",]

nrow(EA.EA)
nrow(EA.ENA)
nrow(ENA.ENA)
#

#
pdf("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/GEB_REVISION/Figures/Background_Test_Results.pdf") #width = 8.5, height = 8.5, units = 'in', res = 300)
ggplot(csv, aes(x = Comparison, y = Obs.D, fill = Comparison)) +
  geom_violin() +
  ylab("Schoener's D") +
  ylim(0, 1) +
  xlab("Ecospat Background Test Results by Regional Comparison") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=15)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")
dev.off()
#
#############################################################################################
setwd(project.folder)
csv <- read.csv(file = "All_Breadth_Metrics_V2.csv")
head(csv)

g.plot <- ggplot(csv, aes(x = Region, y = RasterBreadth, fill = Region)) +
  geom_violin() +
  ylab("Levins' Niche Breadth") +
  ylim(0, 1) +
  xlab("Use of available G-space") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=12)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")

e.plot <- ggplot(csv, aes(x = Region, y = GeographicArea, fill = Region)) +
  geom_violin() +
  ylab("Geographic Area (km^2)") +
  #ylim(0, 1) +
  xlab("Available G-space") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=12)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")

n.plot <- ggplot(csv, aes(x = Region, y = NicheArea, fill = Region)) +
  geom_violin() +
  ylab("Niche Area") +
  ylim(0, 30) +
  xlab("Niche breadth in E-space") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=12)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")

big.grid <- plot_grid(e.plot, g.plot, n.plot, ncol = 3, labels = "AUTO")
pdf("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/GEB_REVISION/Figures/G_and_E_Breadth_V4.pdf",
    width = 8.5, height = 8.5)#, units = 'in', res = 300)
ggdraw(big.grid)
dev.off()
#
#############################################################################################
# Let's make a plot that shows some of the variation in sample size and prediction rasters 
# We already have plots for all of our species, but let's make a 2x3 grid with EA v ENA comparisons

output.folder <- "~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/ENMEval_Outputs/" # Set the main "umbrella" folder that all subfolders are in
species.list <- c("Apios_priceana",
                  "Apios_fortunei",
                  "Cornus_alternifolia",
                  "Cornus_controversa",
                  "Tipularia_discolor",
                  "Tipularia_japonica")



# Lowest EA and ENA species vs the highest in that genus, and then one genus with just a ton of points for both species
#apios.fortunei.pts <- read.csv("ENMEval_Outputs/Apios_fortunei/rare_pts.csv")[,2:3]
#apios.priceana.pts <- read.csv("ENMEval_Outputs/Apios_priceana/rare_pts.csv")[,2:3]

#cornus.alternifolia.pts <- read.csv("ENMEval_Outputs/Cornus_alternifolia/rare_pts.csv")[,2:3]
#cornus.controversa.pts <- read.csv("ENMEval_Outputs/Cornus_controversa/rare_pts.csv")[,2:3]

#tipularia.discolor.pts <- read.csv("ENMEval_Outputs/Tipularia_discolor/rare_pts.csv")[,2:3]
#tipularia.japonica.pts <- read.csv("ENMEval_Outputs/Tipularia_japonica/rare_pts.csv")[,2:3]
#

#
#apios.fortunei.rast <- raster("ENMEval_Outputs/Apios_fortunei/Apios_fortunei.tif")
#apios.priceana.rast <- raster("ENMEval_Outputs/Apios_priceana/Apios_priceana.tif")

#cornus.alternifolia.rast <- raster("ENMEval_Outputs/Cornus_alternifolia/Cornus_alternifolia.tif")
#cornus.controversa.rast <- raster("ENMEval_Outputs/Cornus_controversa/Cornus_controversa.tif")

#tipularia.discolor.rast <- raster("ENMEval_Outputs/Tipularia_discolor/Tipularia_discolor.tif")
#tipularia.japonica.rast <- raster("ENMEval_Outputs/Tipularia_japonica/Tipularia_japonica.tif")
#

# for testing
#plot(apios.fortunei.rast, zlim = c(0,1), title("Apios fortunei"))
#points(apios.fortunei.pts, pch = 19, cex = 0.20)
#

#
#pdf("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/GEB_REVISION/Figures/ENM_Examples.pdf")
#par(mfrow = c(3,2))

#plot(x = apios.fortunei.rast, zlim = c(0,1), main = "Apios fortunei")
#points(apios.fortunei.pts, pch = 19, cex = 0.10)

#plot(x = apios.priceana.rast, zlim = c(0,1), main = "Apios priceana")
#points(apios.priceana.pts, pch = 19, cex = 0.10)

#plot(x = cornus.controversa.rast, zlim = c(0,1), main = "Cornus controversa")
#points(cornus.controversa.pts, pch = 19, cex = 0.10)

#plot(x = cornus.alternifolia.rast, zlim = c(0,1), main = "Cornus alternifolia")
#points(cornus.alternifolia.pts, pch = 19, cex = 0.10)

#plot(x = tipularia.japonica.rast, zlim = c(0,1), main = "Tipularia japonica")
#points(tipularia.japonica.pts, pch = 19, cex = 0.10)

#plot(x = tipularia.discolor.rast, zlim = c(0,1), main = "Tipularia discolor")
#points(tipularia.discolor.pts, pch = 19, cex = 0.10)
#dev.off()
#

#############################################################################################
setwd(project.folder)
csv <- read.csv(file = "Niche_Area_and_95pct_G_Breadth_N_nucifera_CORRECTED.csv")
head(csv)
csv$Genus <- gsub(pattern = " .*", replacement = "", x = csv$Species)

EA <- csv[csv$Region == "EA",]
ENA <- csv[csv$Region == "ENA",]

EA.df <- EA %>%
  group_by(Genus) %>%
  summarise(meanNicheArea = mean(Niche.Breadth..Area.), sdNicheArea = sd(Niche.Breadth..Area.),
            meanG = mean(Geographic.Breadth..95..Threshold.), sdG = sd(Geographic.Breadth..95..Threshold.))
EA.df
write.csv(x = EA.df, file = "EA_Metrics_Per_GENUS_V2.csv")

ENA.df <- ENA %>%
  group_by(Genus) %>%
  summarise(meanNicheArea = mean(Niche.Breadth..Area.), sdNicheArea = sd(Niche.Breadth..Area.),
            meanG = mean(Geographic.Breadth..95..Threshold.), sdG = sd(Geographic.Breadth..95..Threshold.))
ENA.df
write.csv(x = ENA.df, file = "ENA_Metrics_Per_GENUS_V2.csv")

#
#############################################################################################
#
setwd(project.folder)
csv <- read.csv(file = "Niche_Area_and_95pct_G_Breadth_N_nucifera_CORRECTED.csv")
csv$Genus <- gsub(pattern = " .*", replacement = "", x = csv$Species)
head(csv)

csv$GeographicSize <- df$`Geographic Area`
EA <- csv[csv$Region == "EA",]
ENA <- csv[csv$Region == "ENA",]
wilcox.test(x = EA$GeographicSize, y = ENA$GeographicSize)
data.frame(min(csv$GeographicSize), max(csv$GeographicSize), mean(csv$GeographicSize), sd(csv$GeographicSize))
data.frame(mean(EA$GeographicSize), sd(EA$GeographicSize))
data.frame(mean(ENA$GeographicSize), sd(ENA$GeographicSize))
#

#
df
write.csv(x = df, file = "g_space_comp_N_nucifera_corrected.csv", row.names = FALSE)

df$Niche_Area <- csv$Niche.Breadth..Area.
shapiro.test(x = df$`Geographic Area`)
shapiro.test(x = df$Niche_Area)
cor(x = df$`Geographic Area`, y = df$Niche_Area, method = "kendall")

df.b <- df[-55,]
library("ggpubr")
ggscatter(df.b, x = "Geographic Area", y = "Niche_Area", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "kendall",
          xlab = "Geographic Area", ylab = "Niche Area")
#

#
EA.df <- EA %>%
  group_by(Genus) %>%
  summarise(meanNicheArea = mean(Niche.Breadth..Area.), sdNicheArea = sd(Niche.Breadth..Area.),
            meanGArea = mean(GeographicSize), sdGArea = sd(GeographicSize),
            meanG = mean(Geographic.Breadth..95..Threshold.), sdG = sd(Geographic.Breadth..95..Threshold.))
EA.df
write.csv(x = EA.df, file = "EA_Metrics_Per_GENUS_V3.csv")

ENA.df <- ENA %>%
  group_by(Genus) %>%
  summarise(meanNicheArea = mean(Niche.Breadth..Area.), sdNicheArea = sd(Niche.Breadth..Area.),
            meanGArea = mean(GeographicSize), sdGArea = sd(GeographicSize),
            meanG = mean(Geographic.Breadth..95..Threshold.), sdG = sd(Geographic.Breadth..95..Threshold.))
ENA.df
write.csv(x = ENA.df, file = "ENA_Metrics_Per_GENUS_V3.csv")
#
#############################################################################################
# Let's make some plots for all of the evaluaiton metrics

setwd(project.folder)
csv <- read.csv(file = "ENM_Data_and_Evaluations.csv")
head(csv)

trainingAUC.plot <- ggplot(csv, aes(x = Region, y = Training.AUC, fill = Region)) +
  geom_violin() +
  ylab("AUC") +
  ylim(0, 1) +
  xlab("Training AUC") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=12)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")

testAUC.plot <- ggplot(csv, aes(x = Region, y = Average.Test.AUC, fill = Region)) +
  geom_violin() +
  ylab("AUC") +
  ylim(0, 1) +
  xlab("Average Test AUC") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=12)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")

ORmtp.plot <- ggplot(csv, aes(x = Region, y = Average.Test.orMTP, fill = Region)) +
  geom_violin() +
  ylab("Omission Rate") +
  ylim(0, 1) +
  xlab("Minimum Training Presence Omission Rate") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=12)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")

CBI.plot <- ggplot(csv, aes(x = Region, y = CBI, fill = Region)) +
  geom_violin() +
  ylab("Continuous Boyce Index") +
  ylim(0, 1) +
  xlab("Continuous Boyce Index") +
  #scale_fill_grey(start=0.75, end=0.5) + 
  theme_classic() +
  theme(text = element_text(size=12)) +
  theme(legend.position="none") +
  geom_point(shape = 21, size = 2, position = position_jitterdodge(), color = "black", alpha = 1) +
  theme_pubr() +
  stat_summary(fun = "mean", geom = "point", shape = 19, size = 2, color = "black")

big.grid <- plot_grid(trainingAUC.plot, testAUC.plot, ORmtp.plot, CBI.plot, ncol = 2, labels = "AUTO")
pdf("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/GEB_REVISION/Figures/Evaluation_Metrics.pdf",
    width = 8.5, height = 8.5)#, units = 'in', res = 300)
ggdraw(big.grid)
dev.off()
#############################################################################################

.rs.restartR()

#############################################################################################
