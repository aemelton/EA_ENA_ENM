### AE Melton
# This script was used in analyzing ENM outputs and performing background tests.

#install.packages("devtools")
#library(devtools)
#install_github("danlwarren/ENMTools")
#library(ENMTools)

###Libraries
library(ENMTools) #To actually run some ENM stats tests

library(RStoolbox) #To run a PCA on layers
library(ggplot2)
library(FSA)
library(dunn.test)
library(raster)

###Load the median model raster file
ENM_OUTPUT <- raster(FILENAME)

###Visualize the models
#plot(ENM_OUTPUT)


###Calculate the niche breadth. B1 is for the model, B2 is for the environmental data
#Models projected in training region
raster.breadth(ENM_OUTPUT)

######Prep the layers for analyses
setwd("~/Dropbox/UF_Research/Climate_Data/wc2.0_2.5m_bio/")
env.files <- list.files(pattern = ".tif", full.names = TRUE)
envStack <- stack(env.files) # Check the file order before proceeding! Different operating systems will read them in different orders, I've noticed.
names(envStack) <- c("bio1", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19")
env <- setMinMax(envStack)
projection(envStack) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
plot(envStack[[1]])

###Generate species objects
setwd("FOLDER WITH OCCURRENCE DATA")

#Mitchella
repens <- enmtools.species(species.name = "NAME", 
                           presence.points = read.csv("OCCURRENCE DATA")[,2:3]) # columns 2 and 3 have long and lat
repens$range <- background.raster.buffer(repens$presence.points, 25000, mask = envStack)
repens$background.points <- background.points.buffer(points = repens$presence.points,
                                                     radius = 5000, n = 1000, mask = envStack[[1]])

undulata <- enmtools.species(species.name = "NAME", 
                             presence.points = read.csv("OCCURRENCE DATA")[,2:3])
undulata$range <- background.raster.buffer(undulata$presence.points, 25000, mask = envStack)
undulata$background.points <- background.points.buffer(points = undulata$presence.points,
                                                       radius = 5000, n = 1000, mask = envStack[[1]])

################################################Asym BG########################################################################################

bg.asym.A <- enmtools.ecospat.bg(species.1 = SP.A, species.2 = SP.B, env =  envStack, nreps = 1000, test.type = "asymmetric")
bg.asym.B <- enmtools.ecospat.bg(species.1 = SP.B, species.2 = SP.A, env =  envStack, nreps = 1000, test.type = "asymmetric")

################################################Stats########################################################################################

ggplot(data = bg.asym.A, aes(test.results$sim$D)) +
  geom_density()

A <- ggplot(bg.asym.A$test.results$sim, aes(x = D)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() +
  geom_vline(aes(xintercept = EMP.OVRLAP), colour="red", size = 1.5)

B <- ggplot(mitchella.esp.bg.asym.B$test.results$sim, aes(x = D)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() +
  geom_vline(aes(xintercept = EMP.OVRLAP), colour="red", size = 1.5)

grid <- plot_grid(A, B, labels = "AUTO", ncol = 2)

#############################################################################################################################################