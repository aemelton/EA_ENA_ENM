GetCBI <- function(modeval.usr.sp, usr.occs, usr.raster, mod.results, mod.num, nclass = 0, window.w = "default", res = 100){

enmeval.cbi <- NULL
enmeval.cbi.training <- NULL
avg.cbi.testing <- NULL

mod.num <- mod.num # 
mod.settings <- results$settings[[mod.num]]
usr.proj <- predict(modeval.usr.sp@models[[mod.num]], red.envStack)
    
cbi.training <- ecospat.boyce(fit = usr.proj, obs = occ.dat, PEplot = F, nclass = nclass, window.w = window.w, res = res)
enmeval.cbi.training <- data.frame(cbi.training$Spearman.cor)
CBI <- cbind(mod.num, mod.settings, enmeval.cbi.training)#, avg.cbi.testing, enmeval.cbi.testing1, enmeval.cbi.testing2, enmeval.cbi.testing3, enmeval.cbi.testing4, enmeval.cbi.testing5)
}
