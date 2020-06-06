### AE Melton
# This script calculates boyce index for all output models from an ENMEval run.
# It is currently a little slow - future updates will focus on increasing resource efficiency
# during CBI calculations

#
# modeval.usr.sp == an ENMEval output object
# usr.occs == a data.frame of occurrence data
#

GetCBI <- function(modeval.usr.sp, usr.occs, usr.raster){
  
enmeval.cbi <- NULL
enmeval.cbi.training <- NULL
avg.cbi.testing <- NULL
enmeval.cbi.testing1 <- NULL
enmeval.cbi.testing2 <- NULL
enmeval.cbi.testing3 <- NULL
enmeval.cbi.testing4 <- NULL
folds <- kfold(x = usr.occs, k = 4)
testy1 <- usr.occs[folds == 1,]
testy2 <- usr.occs[folds == 2,]
testy3 <- usr.occs[folds == 3,]
testy4 <- usr.occs[folds == 4,]
test1 <- testy1[,1:2]
test2 <- testy2[,1:2]
test3 <- testy3[,1:2]
test4 <- testy4[,1:2]

for (i in 1:length(modeval.usr.sp@models)) {
  usr.proj <- predict(modeval.usr.sp@models[[i]], usr.raster)
    
  cbi.training <- ecospat.boyce(fit = usr.proj, obs = usr.occs, PEplot = F)
  df.training <- data.frame(cbi.training$Spearman.cor)
  enmeval.cbi.training <- rbind(enmeval.cbi.training, df.training)
  
  cbi.testing1 <- ecospat.boyce(fit = usr.proj, obs = test1, PEplot = F)
  df.testing1 <- data.frame(cbi.testing1$Spearman.cor)
  enmeval.cbi.testing1 <- rbind(enmeval.cbi.testing1, df.testing1)
  
  cbi.testing2 <- ecospat.boyce(fit = usr.proj, obs = test2, PEplot = F)
  df.testing2 <- data.frame(cbi.testing2$Spearman.cor)
  enmeval.cbi.testing2 <- rbind(enmeval.cbi.testing2, df.testing2)
  
  cbi.testing3 <- ecospat.boyce(fit = usr.proj, obs = test3, PEplot = F)
  df.testing3 <- data.frame(cbi.testing3$Spearman.cor)
  enmeval.cbi.testing3 <- rbind(enmeval.cbi.testing3, df.testing3)
  
  cbi.testing4 <- ecospat.boyce(fit = usr.proj, obs = test4, PEplot = F)
  df.testing4 <- data.frame(cbi.testing4$Spearman.cor)
  enmeval.cbi.testing4 <- rbind(enmeval.cbi.testing4, df.testing4)
  
  }
  
  cbi.testing.df <- data.frame(enmeval.cbi.testing1, enmeval.cbi.testing2, enmeval.cbi.testing3, enmeval.cbi.testing4)
  avg.cbi.testing <- rowMeans(cbi.testing.df)  
  enmeval.cbi <- cbind(enmeval.cbi.training, avg.cbi.testing, enmeval.cbi.testing1, enmeval.cbi.testing2, enmeval.cbi.testing3, enmeval.cbi.testing4)

  return(enmeval.cbi)
}
