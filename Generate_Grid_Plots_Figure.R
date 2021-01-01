### AE Melton, 2020
# Make a nicer plot of niche model predictions

#
output.folder <- "~/Dropbox/UF_Research/EA_ENA_ENM/XX_Revisions/ENMEval_Outputs/" # Set the main "umbrella" folder that all subfolders are in
species.list <- c("Apios_fortunei",
                  "Apios_priceana",
                  "Cornus_controversa",
                  "Cornus_alternifolia",
                  "Tipularia_japonica",
                  "Tipularia_discolor")

#

#
for(i in 1:length(species.list)){
species.folder <- paste0(output.folder, species.list[i])
setwd(species.folder)
csv <- read.csv("rare_pts.csv")[,2:3]
colnames(csv) <- c("longitude", "latitude")
species.raster.filename <- paste0(species.list[i], "_95pct.tif")
species.raster <- raster(species.raster.filename)
#

#
spat.df <- rasterToPoints(species.raster, spatial = T)
df <- data.frame(spat.df)
colnames(df) <- c("Suitability", "Longitude", "Latitude", "optional")
#

#
species.name <- gsub(pattern = "_", replacement = " ", species.list[i])
plot.a <- ggplot() +
  geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = Suitability)) +
  #scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
  scale_fill_viridis(na.value="white", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle(label = species.name) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  geom_point(data = csv, aes(x = longitude, y = latitude), pch = 19, cex = 0.20, col = "grey")
#
plot(plot) # just here for testing....
}

big.grid <- plot_grid(plot.a, plot.b, plot.c, plot.d, plot.e, plot.f, ncol = 2)
pdf(output.file.name) #width = 8.5, height = 8.5, units = 'in', res = 300)
ggdraw(big.grid)
dev.off()

#############################################################################################
#############################################################################################
#############################################################################################
i <- 1
species.folder <- paste0(output.folder, species.list[i])
setwd(species.folder)
csv <- read.csv("rare_pts.csv")[,2:3]
colnames(csv) <- c("longitude", "latitude")
species.raster.filename <- paste0(species.list[i], "_95pct.tif")
species.raster <- raster(species.raster.filename)
#

#
spat.df <- rasterToPoints(species.raster, spatial = T)
df <- data.frame(spat.df)
colnames(df) <- c("Suitability", "Longitude", "Latitude", "optional")
#

#
species.name <- gsub(pattern = "_", replacement = " ", species.list[i])
plot.a <- ggplot() +
  geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = Suitability)) +
  #scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
  scale_fill_viridis(na.value="white", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle(label = species.name) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  geom_point(data = csv, aes(x = longitude, y = latitude), pch = 19, cex = 0.20, col = "grey")

i <- 2
species.folder <- paste0(output.folder, species.list[i])
setwd(species.folder)
csv <- read.csv("rare_pts.csv")[,2:3]
colnames(csv) <- c("longitude", "latitude")
species.raster.filename <- paste0(species.list[i], "_95pct.tif")
species.raster <- raster(species.raster.filename)
#

#
spat.df <- rasterToPoints(species.raster, spatial = T)
df <- data.frame(spat.df)
colnames(df) <- c("Suitability", "Longitude", "Latitude", "optional")
#

#
species.name <- gsub(pattern = "_", replacement = " ", species.list[i])
plot.b <- ggplot() +
  geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = Suitability)) +
  #scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
  scale_fill_viridis(na.value="white", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle(label = species.name) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  geom_point(data = csv, aes(x = longitude, y = latitude), pch = 19, cex = 0.20, col = "grey")



i <- 3
species.folder <- paste0(output.folder, species.list[i])
setwd(species.folder)
csv <- read.csv("rare_pts.csv")[,2:3]
colnames(csv) <- c("longitude", "latitude")
species.raster.filename <- paste0(species.list[i], "_95pct.tif")
species.raster <- raster(species.raster.filename)
#

#
spat.df <- rasterToPoints(species.raster, spatial = T)
df <- data.frame(spat.df)
colnames(df) <- c("Suitability", "Longitude", "Latitude", "optional")
#

#
species.name <- gsub(pattern = "_", replacement = " ", species.list[i])
plot.c <- ggplot() +
  geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = Suitability)) +
  #scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
  scale_fill_viridis(na.value="white", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle(label = species.name) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  geom_point(data = csv, aes(x = longitude, y = latitude), pch = 19, cex = 0.20, col = "grey")




i <- 4
species.folder <- paste0(output.folder, species.list[i])
setwd(species.folder)
csv <- read.csv("rare_pts.csv")[,2:3]
colnames(csv) <- c("longitude", "latitude")
species.raster.filename <- paste0(species.list[i], "_95pct.tif")
species.raster <- raster(species.raster.filename)
#

#
spat.df <- rasterToPoints(species.raster, spatial = T)
df <- data.frame(spat.df)
colnames(df) <- c("Suitability", "Longitude", "Latitude", "optional")
#

#
species.name <- gsub(pattern = "_", replacement = " ", species.list[i])
plot.d <- ggplot() +
  geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = Suitability)) +
  #scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
  scale_fill_viridis(na.value="white", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle(label = species.name) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  geom_point(data = csv, aes(x = longitude, y = latitude), pch = 19, cex = 0.20, col = "grey")





i <- 5
species.folder <- paste0(output.folder, species.list[i])
setwd(species.folder)
csv <- read.csv("rare_pts.csv")[,2:3]
colnames(csv) <- c("longitude", "latitude")
species.raster.filename <- paste0(species.list[i], "_95pct.tif")
species.raster <- raster(species.raster.filename)
#

#
spat.df <- rasterToPoints(species.raster, spatial = T)
df <- data.frame(spat.df)
colnames(df) <- c("Suitability", "Longitude", "Latitude", "optional")
#

#
species.name <- gsub(pattern = "_", replacement = " ", species.list[i])
plot.e <- ggplot() +
  geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = Suitability)) +
  #scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
  scale_fill_viridis(na.value="white", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle(label = species.name) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  geom_point(data = csv, aes(x = longitude, y = latitude), pch = 19, cex = 0.20, col = "grey")




i <- 6
species.folder <- paste0(output.folder, species.list[i])
setwd(species.folder)
csv <- read.csv("rare_pts.csv")[,2:3]
colnames(csv) <- c("longitude", "latitude")
species.raster.filename <- paste0(species.list[i], "_95pct.tif")
species.raster <- raster(species.raster.filename)
#

#
spat.df <- rasterToPoints(species.raster, spatial = T)
df <- data.frame(spat.df)
colnames(df) <- c("Suitability", "Longitude", "Latitude", "optional")
#

#
species.name <- gsub(pattern = "_", replacement = " ", species.list[i])
plot.f <- ggplot() +
  geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = Suitability)) +
  #scale_fill_gradientn(colors = c("blue", "yellow", "red")) +
  scale_fill_viridis(na.value="white", limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  ggtitle(label = species.name) +
  theme(plot.title = element_text(face = "italic", hjust = 0.5)) +
  geom_point(data = csv, aes(x = longitude, y = latitude), pch = 19, cex = 0.20, col = "grey")


########


big.grid <- plot_grid(plot.a, plot.b, plot.c, plot.d, plot.e, plot.f, ncol = 2)
png("~/Dropbox/Manuscripts/UFL/EA_ENA_ENM/XX_REVISION/Figures/ENM_plots_grid_XX_REV.png", width = 8.5, height = 8.5, units = 'in', res = 300)
ggdraw(big.grid)
dev.off()
