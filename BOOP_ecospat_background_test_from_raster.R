#library(raster)
#library(ecospat)
#library(RStoolbox)
#library(ggplot2)
#library(cowplot)
#library(viridis)
#

# R = 1000, th.sp = 0, th.env = 0
# test.type = "symmetric"
#

#
species.1.folder <- paste0(output.folder, species.1)
species.2.folder <- paste0(output.folder, species.2)
#

# Load SDMs. Rough script will use binary predicted occurrence maps.
setwd(species.1.folder)
species.1.raster.to.load <- paste0(species.1, "_BIN.tif")
species.1.raster <- raster(species.1.raster.to.load)
setwd(species.2.folder)
species.2.raster.to.load <- paste0(species.2, "_BIN.tif")
species.2.raster <- raster(species.2.raster.to.load)

# Plot the rasters. Check, check, check... check yo data.
#par(mfrow=c(1, 2))
#plot(species.1.raster)
#plot(species.2.raster)

# Load environmental data
#setwd("~/Dropbox/Other_Research/ENM_Evaluation_Tests/wc2-5/")
#env.files <- list.files(pattern = ".bil", full.names = TRUE) #Always check for tiff or asc
#rr <- stack(env.files)
#names(rr) <- c("alt", "bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9")
#rr <- setMinMax(rr)
#projection(rr) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
#plot(rr[[1]])
#rr <- rr[[c("bio2", "bio5", "bio6", "bio12", "bio13", "bio14")]] # Cutting down on layers for testing

#
# Rasterize the predicted occurrences for both species.
# Generate background points from full extent of both rasters. 
setwd(species.1.folder)
species.1.bg.pts <- read.csv("bg_pts.csv")[,2:3]
colnames(species.1.bg.pts) <- c("longitude", "latitude")
setwd(species.2.folder)
species.2.bg.pts <- read.csv("bg_pts.csv")[,2:3]
colnames(species.2.bg.pts) <- c("longitude", "latitude")
bg.pts <- rbind(species.1.bg.pts, species.2.bg.pts)

species.1.raster[species.1.raster == 0] <- NA
species.1.pred.occ <- rasterToPoints(species.1.raster)#Need predicted occurrence points (calculated from thresholded model)
species.1.pred.occ <- species.1.pred.occ[,1:2]
colnames(species.1.pred.occ) <- c("longitude", "latitude")

species.2.raster[species.2.raster == 0] <- NA
species.2.pred.occ <- rasterToPoints(species.2.raster)#Need predicted occurrence points (calculated from thresholded model)
species.2.pred.occ <- species.2.pred.occ[,1:2]
colnames(species.2.pred.occ) <- c("longitude", "latitude")

# Do a PCA on the environmental data
#env <- rasterPCA(img = rr, nComp = 2, spca = T)
#layers <- names(env)

# Extract environmental data for background and both species.
bg.env <- extract(env, bg.pts);
bg.env <- bg.env[complete.cases(bg.env),];

species.1.env <- extract(env, species.1.pred.occ);
species.1.env <- species.1.env[complete.cases(species.1.env),];
species.1.bg.env <- extract(env, species.1.bg.pts);
species.1.bg.env <- species.1.bg.env[complete.cases(species.1.bg.env),];

species.2.env <- extract(env, species.2.pred.occ);
species.2.env <- species.2.env[complete.cases(species.2.env),];
species.2.bg.env <- extract(env, species.2.bg.pts);
species.2.bg.env <- species.2.bg.env[complete.cases(species.2.bg.env),];

# Sanity check - run a density plot of the three "env" objects
bg.dat <- as.data.frame(bg.env)
species.1.dat <- as.data.frame(species.1.env)
species.2.dat <- as.data.frame(species.2.env)
bg.dat$Species <- "bg"
species.1.dat$Species <- species.1
species.2.dat$Species <- species.2
r.dat <- rbind(bg.dat, species.1.dat, species.2.dat)
p.1 <- ggplot(r.dat, aes(PC1, fill = Species)) + 
  geom_density(alpha = 0.2)
p.2 <- ggplot(r.dat, aes(PC2, fill = Species)) + 
  geom_density(alpha = 0.2)
plot_grid(p.1, p.2)

# Develop "niche" object
species.1.niche <- ecospat.grid.clim.dyn(glob = bg.env, glob1 = species.1.bg.env, sp = species.1.env, R = 100, th.sp = 0, th.env = 0)
species.2.niche <- ecospat.grid.clim.dyn(glob = bg.env, glob1 = species.2.bg.env, sp = species.2.env, R = 100, th.sp = 0, th.env = 0)

# OMG RUN A TESTS???
bg <- ecospat.niche.similarity.test(species.1.niche, species.2.niche, rep=1000, rand.type = 1, ncores = num.cores)

empline <- c(bg$obs$D, bg$obs$I)
names(empline) <- c("D", "I")
reps.overlap <- rbind(empline, bg$sim)
p.values <- apply(reps.overlap, 2, function(x) 2 * (1 - max(mean(x > x[1]), mean(x < x[1]))))

d.plot <- qplot(bg$sim[,"D"], geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = bg$obs$D, linetype = "longdash") +
  xlim(-.05,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
  ggtitle(paste("Ecospat background test:", species.1, "vs.", species.2)) +
  theme(plot.title = element_text(hjust = 0.5))

i.plot <- qplot(bg$sim[,"I"], geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = bg$obs$I, linetype = "longdash") +
  xlim(-.05,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
  ggtitle(paste("Ecospat background test:", species.1, "vs.", species.2)) +
  theme(plot.title = element_text(hjust = 0.5))


species.1.bg.points <- data.frame(rasterToPoints(species.1.niche$Z))
colnames(species.1.bg.points) <- c("X", "Y", "Density")
species.1.bg.plot <-  ggplot(data = species.1.bg.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.1, "available environment")) +
  theme(plot.title = element_text(hjust = 0.5))

species.1.env.points <- data.frame(rasterToPoints(species.1.niche$z.uncor))
colnames(species.1.env.points) <- c("X", "Y", "Density")
species.1.env.plot <-  ggplot(data = species.1.env.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.1, "occurrence in environment space")) +
  theme(plot.title = element_text(hjust = 0.5))

species.1.env.corr.points <- data.frame(rasterToPoints(species.1.niche$z.cor))
colnames(species.1.env.corr.points) <- c("X", "Y", "Density")
species.1.env.plot.corr <-  ggplot(data = species.1.env.corr.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.1, "occurrence scaled by availability")) +
  theme(plot.title = element_text(hjust = 0.5))

species.2.bg.points <- data.frame(rasterToPoints(species.2.niche$Z))
colnames(species.2.bg.points) <- c("X", "Y", "Density")
species.2.bg.plot <-  ggplot(data = species.2.bg.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.2, "available environment")) +
  theme(plot.title = element_text(hjust = 0.5))

species.2.env.points <- data.frame(rasterToPoints(species.2.niche$z.uncor))
colnames(species.2.env.points) <- c("X", "Y", "Density")
species.2.env.plot <-  ggplot(data = species.2.env.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.2, "occurrence in environment space")) +
  theme(plot.title = element_text(hjust = 0.5))

species.2.env.corr.points <- data.frame(rasterToPoints(species.2.niche$z.cor))
colnames(species.2.env.corr.points) <- c("X", "Y", "Density")
species.2.env.plot.corr <-  ggplot(data = species.2.env.corr.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.2, "occurrence scaled by availability")) +
  theme(plot.title = element_text(hjust = 0.5))



#   image(log(chlor$Z), main="Chlorocyanus environment", col=rainbow(10))
#   #points(chlorpoints[,4:5], pch=3)
#   image(log(chlor$z.uncor), main="Chlorocyanus density", col=rainbow(10))
#   #points(chlorpoints[,4:5], pch=3)
#   image(log(chlor$z.cor), main="Chlorocyanus occupancy", col=rainbow(10))
#   points(chlorpoints[,4:5], pch=3)

output <- list(description = paste("\n\nEcospat background test", test.type, species.1, "vs.", species.2),
               species.1.env = species.1.env,
               species.2.env = species.2.env,
               species.1.bg.env = species.1.bg.env,
               species.2.bg.env = species.2.bg.env,
               bg.env = bg.env,
               species.1.niche = species.1.niche,
               species.2.niche = species.2.niche,
               species.1.bg.plot = species.1.bg.plot,
               species.1.env.plot = species.1.env.plot,
               species.1.env.plot.corr = species.1.env.plot.corr,
               species.2.bg.plot = species.2.bg.plot,
               species.2.env.plot = species.2.env.plot,
               species.2.env.plot.corr = species.2.env.plot.corr,
               test.results = bg,
               p.values = p.values,
               d.plot = d.plot,
               i.plot = i.plot)
class(output) <- "ecospat.bg.test"

return(output)

plot.ecospat.bg.test <- function(x, ...){
  grid.arrange(x$d.plot, x$i.plot, nrow = 2)
  grid.arrange(x$species.1.bg.plot, x$species.2.bg.plot,
               x$species.1.env.plot, x$species.2.env.plot,
               x$species.1.env.plot.corr, x$species.2.env.plot.corr, ncol = 2) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot.ecospat.bg.test(output)
output
