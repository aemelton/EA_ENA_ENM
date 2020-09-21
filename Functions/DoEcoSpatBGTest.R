### AE Melton, 2020
#

DoEcoSpatBGTest <- function(species.1, species.2, output.folder, ecospat.folder, env,
                            test.type = "symmetric", bg.source = "points", nreps = nreps,
                            layers = NULL, th.sp = th.sp, th.env = th.env, R = R, nback = nback){
#
species.1.folder <- paste0(output.folder, species.1)
species.2.folder <- paste0(output.folder, species.2)
#

# Load binary maps just to generate background points....
setwd(species.1.folder)
species.1.raster.to.load <- paste0(species.1, "_BIN.tif")
species.1.raster <- raster(species.1.raster.to.load)
setwd(species.2.folder)
species.2.raster.to.load <- paste0(species.2, "_BIN.tif")
species.2.raster <- raster(species.2.raster.to.load)

#species.1.raster[species.1.raster == 0] <- NA
species.1.range <- as.data.frame(rasterToPoints(species.1.raster))#Need predicted occurrence points (calculated from thresholded model)
species.1.range.pts <- species.1.range[,1:2]
colnames(species.1.range.pts) <- c("longitude", "latitude")

#species.2.raster[species.2.raster == 0] <- NA
species.2.range <- as.data.frame(rasterToPoints(species.2.raster))#Need predicted occurrence points (calculated from thresholded model)
species.2.range.pts <- species.2.range[,1:2]
colnames(species.2.range.pts) <- c("longitude", "latitude")

total.bg.pts <- rbind(species.1.range.pts, species.2.range.pts)

# Rasterize the predicted occurrences for both species.
# Generate background points from full extent of both rasters. 
setwd(species.1.folder)
species.1.occ.pts <- read.csv("rare_pts.csv")[,2:3]
#species.1.raster[species.1.raster == 0] <- NA
#species.1.occ.pts <- as.data.frame(rasterToPoints(species.1.raster))
#species.1.occ.pts <- species.1.occ.pts[,1:2]
colnames(species.1.occ.pts) <- c("longitude", "latitude")
species.1.bg.pts <- read.csv("bg_pts.csv")[,2:3]
colnames(species.1.bg.pts) <- c("longitude", "latitude")
setwd(species.2.folder)
species.2.occ.pts <- read.csv("rare_pts.csv")[,2:3]
#species.2.raster[species.2.raster == 0] <- NA
#species.2.occ.pts <- as.data.frame(rasterToPoints(species.2.raster))
#species.2.occ.pts <- species.2.occ.pts[,1:2]
colnames(species.2.occ.pts) <- c("longitude", "latitude")
species.2.bg.pts <- read.csv("bg_pts.csv")[,2:3]
colnames(species.2.bg.pts) <- c("longitude", "latitude")

species.1.obj <- enmtools.species(species.name = species.1, presence.points = species.1.occ.pts)
species.1.obj$background.points <- as.data.frame(species.1.bg.pts)

species.2.obj <- enmtools.species(species.name = species.2, presence.points = species.2.occ.pts)
species.2.obj$background.points <- as.data.frame(species.2.bg.pts)

bg.out <- enmtools.ecospat.bg.mod(species.1 = species.1.obj, species.2 = species.2.obj, env = env, test.type = "symmetric", bg.source = "points", bg.pts = total.bg.pts, nreps = nreps,
                                  layers = NULL, th.sp = th.sp, th.env = th.env, R = R, nback = nback)

setwd(ecospat.folder)
bg.out.file.name <- paste0(species.1, "_", species.2, "_BG_Test_Out.RDA")
save(bg.out, file = bg.out.file.name)

genus <- gsub(pattern = "_.*", replacement = "", x = species.1)
bg.out$test.results$sim

boop <- ggplot(bg.out$test.results$sim, aes(x = D)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density() +
  ggtitle(genus) +
  geom_vline(aes(xintercept = bg.out$test.results$obs$D), colour="red")
bg.out.pdf.file.name <- paste0(species.1, "_", species.2, "_BG_Test_Out.pdf")
pdf(bg.out.pdf.file.name)
plot(boop)
dev.off()
#ggplot(bg.out$test.results$sim, aes(x = I)) + 
#  geom_histogram(aes(y = ..density..)) + 
#  geom_density() +
#  ggtitle(genus) +
#  geom_vline(aes(xintercept = bg.out$test.results$obs$I), colour="red")

bg.out.csv.file.name <- paste0(species.1, "_", species.2, "_BG_Test_Out.csv")
bg.df <- data.frame(bg.out$test.results$obs$D, bg.out$test.results$obs$I, bg.out$p.values[1], bg.out$p.values[2])
write.csv(x = bg.df, file = bg.out.csv.file.name, row.names = F)
}
#
