### AE Melton, 2020
# The "mod" is pulling in bg info from pre-determined background points, instead of using PointstoRaster on the entire env raster. 

enmtools.ecospat.bg.mod <- function (species.1, species.2, env, nreps = nreps, layers = NULL, 
                                     test.type = "symmetric", th.sp = th.sp, th.env = th.env, R = R, 
                                     nback = nback, bg.source = "points", bg.pts, ...) {
  #check.packages("ecospat")
  #species.1 <- check.bg(species.1, env, nback)
  #species.2 <- check.bg(species.2, env, nback)
  #if (length(names(env)) == 2) {
  #  layers <- names(env)
  #}
  #else if (is.null(layers)) {
  #  message("More than two layers in environment stack and no layers argument passed, performing PCA...")
  #  env <- raster.pca(env, n = 2)
  #  env <- env$rasters
  #  layers <- names(env)
  #}
  #ecospat.bg.precheck(species.1, species.2, env, nreps, layers)
  
  layers <- names(env)
  sp1.env <- extract(env, species.1$presence.points)
  sp1.env <- cbind(rep(species.1$species.name, nrow(species.1$presence.points)), 
                   species.1$presence.points, sp1.env)
  sp1.env <- sp1.env[complete.cases(sp1.env), ]
  colnames(sp1.env) <- c("Species", colnames(species.1$presence.points), 
                         layers)
  sp1.bg.env <- extract(env, species.1$background.points)
  sp1.bg.env <- cbind(rep(paste0(species.1$species.name, ".bg"), 
                          nrow(species.1$background.points)), species.1$background.points, 
                      sp1.bg.env)
  sp1.bg.env <- sp1.bg.env[complete.cases(sp1.bg.env), ]
  colnames(sp1.bg.env) <- c("Species", colnames(species.1$presence.points), 
                            layers)
  sp2.bg.env <- extract(env, species.2$background.points)
  sp2.bg.env <- cbind(rep(species.2$species.name, nrow(species.2$background.points)), 
                      species.2$background.points, sp2.bg.env)
  sp2.bg.env <- sp2.bg.env[complete.cases(sp2.bg.env), ]
  colnames(sp2.bg.env) <- c("Species", colnames(species.1$presence.points), 
                            layers)
  sp2.env <- extract(env, species.2$presence.points)
  sp2.env <- cbind(rep(paste0(species.2$species.name, ".bg"), 
                       nrow(species.2$presence.points)), species.2$presence.points, 
                   sp2.env)
  sp2.env <- sp2.env[complete.cases(sp2.env), ]
  colnames(sp2.env) <- c("Species", colnames(species.2$presence.points), 
                         layers)
  background.env <- as.data.frame(extract(x = env, y = bg.pts))
  background.env <- cbind(rep("background"), bg.pts, background.env)
  colnames(background.env) <- c("Species", colnames(species.1$presence.points), 
                                names(env))
  background.env <- background.env[complete.cases(background.env),]
  sp1.niche <- ecospat::ecospat.grid.clim.dyn(background.env[, 4:5], sp1.bg.env[, 4:5], sp1.env[, 4:5], th.sp = th.sp, 
                                              th.env = th.env, R = R)
  sp2.niche <- ecospat::ecospat.grid.clim.dyn(background.env[, 4:5], sp2.bg.env[, 4:5], sp2.env[, 4:5], th.sp = th.sp, 
                                              th.env = th.env, R = R)
  if (test.type == "symmetric") {
    rand.type = 1
  }
  else {
    rand.type = 2
  }
  bg <- ecospat::ecospat.niche.similarity.test(sp1.niche, sp2.niche, 
                                               rep = nreps, rand.type = rand.type, ...)
  empline <- c(bg$obs$D, bg$obs$I)
  names(empline) <- c("D", "I")
  reps.overlap <- rbind(empline, bg$sim)
  p.values <- apply(reps.overlap, 2, function(x) 2 * (1 - max(mean(x > 
                                                                     x[1]), mean(x < x[1]))))
  d.plot <- qplot(bg$sim[, "D"], geom = "histogram", fill = "density", 
                  alpha = 0.5) + geom_vline(xintercept = bg$obs$D, linetype = "longdash") + 
    xlim(-0.05, 1.05) + guides(fill = FALSE, alpha = FALSE) + 
    xlab("D") + ggtitle(paste("Ecospat background test:", 
                              species.1$species.name, "vs.", species.2$species.name)) + 
    theme(plot.title = element_text(hjust = 0.5))
  i.plot <- qplot(bg$sim[, "I"], geom = "histogram", fill = "density", 
                  alpha = 0.5) + geom_vline(xintercept = bg$obs$I, linetype = "longdash") + 
    xlim(-0.05, 1.05) + guides(fill = FALSE, alpha = FALSE) + 
    xlab("I") + ggtitle(paste("Ecospat background test:", 
                              species.1$species.name, "vs.", species.2$species.name)) + 
    theme(plot.title = element_text(hjust = 0.5))
  sp1.bg.points <- data.frame(rasterToPoints(sp1.niche$Z))
  colnames(sp1.bg.points) <- c("X", "Y", "Density")
  sp1.bg.plot <- ggplot(data = sp1.bg.points, aes_string(y = "Y", 
                                                         x = "X")) + geom_raster(aes_string(fill = "Density")) + 
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) + 
    theme_classic() + ggtitle(paste(species.1$species.name, 
                                    "available environment")) + theme(plot.title = element_text(hjust = 0.5))
  sp1.env.points <- data.frame(rasterToPoints(sp1.niche$z.uncor))
  colnames(sp1.env.points) <- c("X", "Y", "Density")
  sp1.env.plot <- ggplot(data = sp1.env.points, aes_string(y = "Y", 
                                                           x = "X")) + geom_raster(aes_string(fill = "Density")) + 
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) + 
    theme_classic() + ggtitle(paste(species.1$species.name, 
                                    "occurrence in environment space")) + theme(plot.title = element_text(hjust = 0.5))
  sp1.env.corr.points <- data.frame(rasterToPoints(sp1.niche$z.cor))
  colnames(sp1.env.corr.points) <- c("X", "Y", "Density")
  sp1.env.plot.corr <- ggplot(data = sp1.env.corr.points, aes_string(y = "Y", 
                                                                     x = "X")) + geom_raster(aes_string(fill = "Density")) + 
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) + 
    theme_classic() + ggtitle(paste(species.1$species.name, 
                                    "occurrence scaled by availability")) + theme(plot.title = element_text(hjust = 0.5))
  sp2.bg.points <- data.frame(rasterToPoints(sp2.niche$Z))
  colnames(sp2.bg.points) <- c("X", "Y", "Density")
  sp2.bg.plot <- ggplot(data = sp2.bg.points, aes_string(y = "Y", 
                                                         x = "X")) + geom_raster(aes_string(fill = "Density")) + 
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) + 
    theme_classic() + ggtitle(paste(species.2$species.name, 
                                    "available environment")) + theme(plot.title = element_text(hjust = 0.5))
  sp2.env.points <- data.frame(rasterToPoints(sp2.niche$z.uncor))
  colnames(sp2.env.points) <- c("X", "Y", "Density")
  sp2.env.plot <- ggplot(data = sp2.env.points, aes_string(y = "Y", 
                                                           x = "X")) + geom_raster(aes_string(fill = "Density")) + 
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) + 
    theme_classic() + ggtitle(paste(species.2$species.name, 
                                    "occurrence in environment space")) + theme(plot.title = element_text(hjust = 0.5))
  sp2.env.corr.points <- data.frame(rasterToPoints(sp2.niche$z.cor))
  colnames(sp2.env.corr.points) <- c("X", "Y", "Density")
  sp2.env.plot.corr <- ggplot(data = sp2.env.corr.points, aes_string(y = "Y", 
                                                                     x = "X")) + geom_raster(aes_string(fill = "Density")) + 
    scale_fill_viridis_c(option = "B", guide = guide_colourbar(title = "Density")) + 
    theme_classic() + ggtitle(paste(species.2$species.name, 
                                    "occurrence scaled by availability")) + theme(plot.title = element_text(hjust = 0.5))
  output <- list(description = paste("\n\nEcospat background test", 
                                     test.type, species.1$species.name, "vs.", species.2$species.name), 
                 sp1.env = sp1.env, sp2.env = sp2.env, sp1.bg.env = sp1.bg.env, 
                 sp2.bg.env = sp2.bg.env, background.env = background.env, 
                 sp1.niche = sp1.niche, sp2.niche = sp2.niche, sp1.bg.plot = sp1.bg.plot, 
                 sp1.env.plot = sp1.env.plot, sp1.env.plot.corr = sp1.env.plot.corr, 
                 sp2.bg.plot = sp2.bg.plot, sp2.env.plot = sp2.env.plot, 
                 sp2.env.plot.corr = sp2.env.plot.corr, test.results = bg, 
                 p.values = p.values, d.plot = d.plot, i.plot = i.plot)
  class(output) <- "ecospat.bg.test"
  return(output)
}
