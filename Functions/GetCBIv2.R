### AE Melton
# This script calculates boyce index for all output models from an ENMEval run.
# It is currently a little slow - future updates will focus on increasing resource efficiency
# during CBI calculations

#
# modeval.usr.sp == an ENMEval output object
# usr.occs == a data.frame of occurrence data
# usr.raster == the raster stack that was used in model development
# mod.results == data frame containing the evaluation results for the ENMEval models. This was simply included to extract model number and setting info to keep track of models going into the loop.
# num.clust == How many clusters to use in parallel?
#

GetCBIv2 <- function(modeval.usr.sp, usr.occs, usr.raster, mod.results, or.threshold, d.AICc.threshold, num.clust, nclass = 0, window.w = "default", res = 100){
  
enmeval.cbi <- NULL
enmeval.cbi.training <- NULL
avg.cbi.testing <- NULL
#enmeval.cbi.testing1 <- NULL
#enmeval.cbi.testing2 <- NULL
#enmeval.cbi.testing3 <- NULL
#enmeval.cbi.testing4 <- NULL
#enmeval.cbi.testing5 <- NULL
#folds <- kfold(x = usr.occs, k = k)
#testy1 <- usr.occs[folds == 1,]
#testy2 <- usr.occs[folds == 2,]
#testy3 <- usr.occs[folds == 3,]
#testy4 <- usr.occs[folds == 4,]
#testy5 <- usr.occs[folds == 5,]
#test1 <- testy1[,1:2]
#test2 <- testy2[,1:2]
#test3 <- testy3[,1:2]
#test4 <- testy4[,1:2]
#test5 <- testy5[,1:2]

cl <- makeCluster(num.clust)
registerDoParallel(cl)
#i <- 1:length(modeval@models)
enmeval.cbiv3 <- foreach(i = 1:length(modeval.usr.sp@models), .combine = rbind, .inorder = TRUE, .packages = list.of.packages) %dopar% {
  if(mod.results$avg.test.orMTP[[i]] <= or.threshold & mod.results$delta.AICc[[i]] <= d.AICc.threshold){
 #mod.results$train.AUC[[i]] >= 0.7 & mod.results$avg.test.orMTP[[i]] <= 0.25 & mod.results$delta.AICc[[i]]
  mod.num <- i
  mod.settings <- mod.results$settings[[i]]

  usr.proj <- predict(modeval.usr.sp@models[[i]], usr.raster)
  
  cbi.training <- ecospat.boyce(fit = usr.proj, obs = usr.occs, PEplot = F, nclass = nclass, window.w = window.w, res = res)
  enmeval.cbi.training <- data.frame(cbi.training$Spearman.cor)
  #enmeval.cbi.training <- rbind(enmeval.cbi.training, df.training)
  
  #cbi.testing1 <- ecospat.boyce(fit = usr.proj, obs = test1, PEplot = F)
  #enmeval.cbi.testing1 <- data.frame(cbi.testing1$Spearman.cor)
  #enmeval.cbi.testing1 <- rbind(enmeval.cbi.testing1, df.testing1)
  
  #cbi.testing2 <- ecospat.boyce(fit = usr.proj, obs = test2, PEplot = F)
  #enmeval.cbi.testing2 <- data.frame(cbi.testing2$Spearman.cor)
  #enmeval.cbi.testing2 <- rbind(enmeval.cbi.testing2, df.testing2)
  
  #cbi.testing3 <- ecospat.boyce(fit = usr.proj, obs = test3, PEplot = F)
  #enmeval.cbi.testing3 <- data.frame(cbi.testing3$Spearman.cor)
  #enmeval.cbi.testing3 <- rbind(enmeval.cbi.testing3, df.testing3)
  
  #cbi.testing4 <- ecospat.boyce(fit = usr.proj, obs = test4, PEplot = F)
  #enmeval.cbi.testing4 <- data.frame(cbi.testing4$Spearman.cor)
  #enmeval.cbi.testing4 <- rbind(enmeval.cbi.testing4, df.testing4)
  
  #cbi.testing5 <- ecospat.boyce(fit = usr.proj, obs = test5, PEplot = F)
  #enmeval.cbi.testing5 <- data.frame(cbi.testing5$Spearman.cor)
  #enmeval.cbi.testing4 <- rbind(enmeval.cbi.testing4, df.testing4)
  
  #cbi.testing.df <- data.frame(enmeval.cbi.testing1, enmeval.cbi.testing2, enmeval.cbi.testing3, enmeval.cbi.testing4, enmeval.cbi.testing5)
  #avg.cbi.testing <- rowMeans(cbi.testing.df, na.rm = T) 
  
  FinMat <- cbind(mod.num, mod.settings, enmeval.cbi.training) #avg.cbi.testing, enmeval.cbi.testing1, enmeval.cbi.testing2, enmeval.cbi.testing3, enmeval.cbi.testing4, enmeval.cbi.testing5)
  }
}
return(enmeval.cbiv3)
stopCluster(cl)
}

