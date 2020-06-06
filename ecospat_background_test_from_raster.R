library(raster)
library(ecospat)
library(RStoolbox)
library(ggplot2)
library(cowplot)
library(viridis)

# Set the working directory
#setwd("~/Dropbox/UF_Research/Fabiana_ENM/ENMEval_Outputs/")

# Load SDMs. Rough script will use binary predicted occurrence maps.
sp.1 <- raster("~/Dropbox/UF_Research/EA_ENA_ENM/SDM_V2_ENMEval/Aesculus_glabraDist.asc")
sp.2 <- raster("~/Dropbox/UF_Research/EA_ENA_ENM/SDM_V2_ENMEval/Aesculus_flavaDist.asc")

# Plot the rasters. Check, check, check... check yo data.
par(mfrow=c(1, 2))
plot(sp.1)
plot(sp.2)

# Load environmental data
setwd("~/Dropbox/Other_Research/ENM_Evaluation_Tests/wc2-5/")
env.files <- list.files(pattern = ".bil", full.names = TRUE) #Always check for tiff or asc
rr <- stack(env.files)
names(rr) <- c("alt", "bio1", "bio10", "bio11", "bio12", "bio13", "bio14", "bio15", "bio16", "bio17", "bio18", "bio19", "bio2", "bio3", "bio4", "bio5", "bio6", "bio7", "bio8", "bio9")
rr <- setMinMax(rr)
projection(rr) <- "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
plot(rr[[1]])
rr <- rr[[c("bio2", "bio5", "bio6", "bio12", "bio13", "bio14")]] # Cutting down on layers for testing

#
test.type = "symmetric"
species.1 <- NULL
species.2 <- NULL
species.1$species.name <- "Aesculus glabra"
species.2$species.name <- "Aesculus flava"
# Rasterize the predicted occurrences for both species.
# Generate background points from full extent of both rasters. 
sp.1.bg.pts <- rasterToPoints(sp.1)
sp.2.bg.pts <- rasterToPoints(sp.2)
bg.pts <- rbind(sp.1.bg.pts, sp.2.bg.pts)

sp.1[sp.1 == 0] <- NA
sp.1.pred.occ <- rasterToPoints(sp.1)#Need predicted occurrence points (calculated from thresholded model)

sp.2[sp.2 == 0] <- NA
sp.2.pred.occ <- rasterToPoints(sp.2)#Need predicted occurrence points (calculated from thresholded model)

# Do a PCA on the environmental data
env <- rasterPCA(img = rr, nComp = 2)
layers <- names(env)

# Extract environmental data for background and both species.
bg.env <- extract(env$map, bg.pts[,1:2]);
bg.env <- bg.env[complete.cases(bg.env),];

sp.1.env <- extract(env$map, sp.1.pred.occ[,1:2]);
sp.1.env <- sp.1.env[complete.cases(sp.1.env),];
sp.1.bg.env <- extract(env$map, sp.1.bg.pts[,1:2]);
sp.1.bg.env <- sp.1.bg.env[complete.cases(sp.1.bg.env),];

sp.2.env <- extract(env$map, sp.2.pred.occ[,1:2]);
sp.2.env <- sp.2.env[complete.cases(sp.2.env),];
sp.2.bg.env <- extract(env$map, sp.2.bg.pts[,1:2]);
sp.2.bg.env <- sp.2.bg.env[complete.cases(sp.2.bg.env),];

# Extract environmental data for background and both species.
bg.env <- extract(env$map, bg.pts[,1:2]);
bg.env <- bg.env[complete.cases(bg.env),];

sp.1.env <- extract(env$map, sp.1.pred.occ[,1:2]);
sp.1.env <- sp.1.env[complete.cases(sp.1.env),];

sp.2.env <- extract(env$map, sp.2.pred.occ[,1:2]);
sp.2.env <- sp.2.env[complete.cases(sp.2.env),];

# Sanity check - run a density plot of the three "env" objects
bg.dat <- as.data.frame(bg.env)
sp.1.dat <- as.data.frame(sp.1.env)
sp.2.dat <- as.data.frame(sp.2.env)
bg.dat$Species <- "bg"
sp.1.dat$Species <- "sp.1"
sp.2.dat$Species <- "sp.2"
r.dat <- rbind(bg.dat, sp.1.dat, sp.2.dat)
p.1 <- ggplot(r.dat, aes(PC1, fill = Species)) + 
  geom_density(alpha = 0.2)
p.2 <- ggplot(r.dat, aes(PC2, fill = Species)) + 
  geom_density(alpha = 0.2)
plot_grid(p.1, p.2)

# Develop "niche" object
sp1.niche <- ecospat.grid.clim.dyn(glob = bg.env, glob1 = sp.1.bg.env, sp = sp.1.env, R = 100)
sp2.niche <- ecospat.grid.clim.dyn(glob = bg.env, glob1 = sp.2.bg.env, sp = sp.2.env, R = 100)

# OMG RUN A TESTS???
bg <- ecospat.niche.similarity.test(sp1.niche, sp2.niche, rep=100, rand.type = 1)

empline <- c(bg$obs$D, bg$obs$I)
names(empline) <- c("D", "I")
reps.overlap <- rbind(empline, bg$sim)
p.values <- apply(reps.overlap, 2, function(x) 2 * (1 - max(mean(x > x[1]), mean(x < x[1]))))

d.plot <- qplot(bg$sim[,"D"], geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = bg$obs$D, linetype = "longdash") +
  xlim(-.05,1) + guides(fill = FALSE, alpha = FALSE) + xlab("D") +
  ggtitle(paste("Ecospat background test:", species.1$species.name, "vs.", species.2$species.name)) +
  theme(plot.title = element_text(hjust = 0.5))

i.plot <- qplot(bg$sim[,"I"], geom = "histogram", fill = "density", alpha = 0.5) +
  geom_vline(xintercept = bg$obs$I, linetype = "longdash") +
  xlim(-.05,1) + guides(fill = FALSE, alpha = FALSE) + xlab("I") +
  ggtitle(paste("Ecospat background test:", species.1$species.name, "vs.", species.2$species.name)) +
  theme(plot.title = element_text(hjust = 0.5))


sp1.bg.points <- data.frame(rasterToPoints(sp1.niche$Z))
colnames(sp1.bg.points) <- c("X", "Y", "Density")
sp1.bg.plot <-  ggplot(data = sp1.bg.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.1$species.name, "available environment")) +
  theme(plot.title = element_text(hjust = 0.5))

sp1.env.points <- data.frame(rasterToPoints(sp1.niche$z.uncor))
colnames(sp1.env.points) <- c("X", "Y", "Density")
sp1.env.plot <-  ggplot(data = sp1.env.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.1$species.name, "occurrence in environment space")) +
  theme(plot.title = element_text(hjust = 0.5))

sp1.env.corr.points <- data.frame(rasterToPoints(sp1.niche$z.cor))
colnames(sp1.env.corr.points) <- c("X", "Y", "Density")
sp1.env.plot.corr <-  ggplot(data = sp1.env.corr.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.1$species.name, "occurrence scaled by availability")) +
  theme(plot.title = element_text(hjust = 0.5))

sp2.bg.points <- data.frame(rasterToPoints(sp2.niche$Z))
colnames(sp2.bg.points) <- c("X", "Y", "Density")
sp2.bg.plot <-  ggplot(data = sp2.bg.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.2$species.name, "available environment")) +
  theme(plot.title = element_text(hjust = 0.5))

sp2.env.points <- data.frame(rasterToPoints(sp2.niche$z.uncor))
colnames(sp2.env.points) <- c("X", "Y", "Density")
sp2.env.plot <-  ggplot(data = sp2.env.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.2$species.name, "occurrence in environment space")) +
  theme(plot.title = element_text(hjust = 0.5))

sp2.env.corr.points <- data.frame(rasterToPoints(sp2.niche$z.cor))
colnames(sp2.env.corr.points) <- c("X", "Y", "Density")
sp2.env.plot.corr <-  ggplot(data = sp2.env.corr.points, aes_string(y = "Y", x = "X")) +
  geom_raster(aes_string(fill = "Density")) +
  scale_fill_viridis(option = "B", guide = guide_colourbar(title = "Density")) +
  theme_classic() +
  ggtitle(paste(species.2$species.name, "occurrence scaled by availability")) +
  theme(plot.title = element_text(hjust = 0.5))



#   image(log(chlor$Z), main="Chlorocyanus environment", col=rainbow(10))
#   #points(chlorpoints[,4:5], pch=3)
#   image(log(chlor$z.uncor), main="Chlorocyanus density", col=rainbow(10))
#   #points(chlorpoints[,4:5], pch=3)
#   image(log(chlor$z.cor), main="Chlorocyanus occupancy", col=rainbow(10))
#   points(chlorpoints[,4:5], pch=3)

output <- list(description = paste("\n\nEcospat background test", test.type, species.1$species.name, "vs.", species.2$species.name),
               sp.1.env = sp.1.env,
               sp.2.env = sp.2.env,
               sp1.bg.env = sp.1.bg.env,
               sp2.bg.env = sp.2.bg.env,
               bg.env = bg.env,
               sp1.niche = sp1.niche,
               sp2.niche = sp2.niche,
               sp1.bg.plot = sp1.bg.plot,
               sp1.env.plot = sp1.env.plot,
               sp1.env.plot.corr = sp1.env.plot.corr,
               sp2.bg.plot = sp2.bg.plot,
               sp2.env.plot = sp2.env.plot,
               sp2.env.plot.corr = sp2.env.plot.corr,
               test.results = bg,
               p.values = p.values,
               d.plot = d.plot,
               i.plot = i.plot)
class(output) <- "ecospat.bg.test"

return(output)

plot.ecospat.bg.test <- function(x, ...){
  grid.arrange(x$d.plot, x$i.plot, nrow = 2)
  grid.arrange(x$sp1.bg.plot, x$sp2.bg.plot,
               x$sp1.env.plot, x$sp2.env.plot,
               x$sp1.env.plot.corr, x$sp2.env.plot.corr, ncol = 2) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot.ecospat.bg.test(output)
