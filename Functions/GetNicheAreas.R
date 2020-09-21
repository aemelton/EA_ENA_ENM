### AE Melton, 2020
# Use EcoSpat to calculate niche area in e-space

GetNicheAreas <- function(env, big.bg.pts, R = 1000, th.sp = 0, th.env = 0){

setwd(main.folder)

print("Prepping total background data.")
bg.env <- extract(env, big.bg.pts);
bg.env <- bg.env[complete.cases(bg.env),];
write.csv(x = bg.env, file = "bg_env.csv", col.names = F)
#bg.env <- read.csv(file = "bg_env.csv")
  
Niche.Plot.Outputs.folder <- paste0(main.folder, "Niche_Area_Plots/")
ENMEval.Outputs.folder <- paste0(main.folder, "ENMEval_Outputs/")
setwd(ENMEval.Outputs.folder)
folder.list <- list.files(pattern = "*", full.names = TRUE) # Make a list of all species output folders
#

#
df <- data.frame(niche_area = character(), PC1_position = character(), PC2_position = character()) # Generate empty data frame to put results in
tmp <- data.frame(niche_area = character(), PC1_position = character(), PC2_position = character()) # Generate empty data frame to put results in

for (i in 1:length(folder.list)) {  #length(folder.list)
  setwd(folder.list[i])
  species.name <- gsub(x = folder.list[i], pattern = "./", replacement = "")
  print(species.name)
  print("Reading bg points and sampling occurrences.")
  bg.pts <- read.csv("bg_pts.csv")[,2:3]
  
  species.raster.file <- paste0(species.name, "_BIN.tif")
  sp <- raster(species.raster.file)
  sp[sp == 0] <- NA
  sp.pred.occ <- rasterToPoints(sp) #Need predicted occurrence points (calculated from thresholded model)
  #
  
  #
  print("Extracting environmental data.")
  sp.env <- extract(env, sp.pred.occ[,1:2]);
  sp.env <- sp.env[complete.cases(sp.env),];
  sp.bg.env <- extract(env, bg.pts[,1:2]);
  sp.bg.env <- bg.env[complete.cases(bg.env),];
  #
  
  #
  print("Running ecospat.grid.clim.dyn")
  z <- ecospat.grid.clim.dyn(glob = bg.env, glob1 = sp.bg.env, sp = sp.env, R = R, th.sp = th.sp, th.env = th.env)
  species <- species.name
  
  ###
  print("Calculating niche area.")
  niche <- matrix(nrow = 1, ncol = 4)
  colnames(niche) <- c("pos1","breadth1","pos2","breadth2")
  
  # Getting samples that aren't positive will throw this off. Get the number of positives and if not 1000, just take as many as possible
  non0 <- subset(x = values(z$z.uncor), values(z$z.uncor) > 0)
  length(non0)
  if (length(non0) >= 1000) {
    n <- 1000
  } else {
    n <- length(non0)
  }
  #
  
  c <-sample(1:(R*R), n, prob = values(z$z.uncor)) #row index of 1000 random pixel weighted by density along PC1 and PC2 
  y = (c%/%R) + 1; x = c%%R # coordinates of the pixels along CP1 and CP2
  CP.sim <- z$x[x] # scores of random pixels on CP1
  niche[1,1] <- median(CP.sim) # niche position on CP1
  niche[1,2] <- var(CP.sim) # niche breadth on CP1
  CP2.sim <- z$y[y] # scores of random pixels on CP2
  niche[1,3] <- median(CP2.sim) # niche position on CP2
  niche[1,4] <-var(CP2.sim) # niche breadth on CP2
  
  tmp <- data.frame(niche[1,2]*niche[1,4], niche[1,1], niche[1,3])
  colnames(tmp) <- c("niche_area", "PC1 position", "PC2 position")
  rownames(tmp) <- species.name
  #tmp <- cbind(species, niche_area)
  df <- rbind(df, tmp)
  setwd(Niche.Plot.Outputs.folder)
  csv.file.name <- paste0(species.name, "_Niche_Area.csv")
  write.csv(x = tmp, file = csv.file.name)
  pdf.file.name <- paste0(species.name, "_Niche_Area.pdf")
  pdf(pdf.file.name)
  ecospat.plot.niche(z = z, name.axis1 = "PC1", name.axis2 = "PC2", title = species.name)
  dev.off()
  
  # Go home to start the loop over
  setwd(ENMEval.Outputs.folder)
  }
return(df)
}
