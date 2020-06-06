### AE Melton
# This script utilizes code from the CalibratR package to calculate calibration error metrics for 
# models stored in an ENMEval output object

#
# modeval.usr.sp == an ENMEval output object
# usr.occs == a data.frame of occurrence data
#

GetCE <- function(modeval.usr.sp, usr.occs, usr.raster, usr.bg){
  
ECE <- NULL
ECE.equal.width <- NULL
MCE <- NULL
MCE.equal.width <- NULL
  
actual <- usr.occs[,1:2]
folds <- kfold(x = actual, k = 4)
testy1 <- actual[folds == 1,]
testy2 <- actual[folds == 2,]
testy3 <- actual[folds == 3,]
testy4 <- actual[folds == 4,]
test1 <- testy1[,1:2]
test2 <- testy2[,1:2]
test3 <- testy3[,1:2]
test4 <- testy4[,1:2]

  for (i in 1:length(modeval.usr.sp@models)) {
  predictions <- predict(modeval.usr.sp@models[[i]], usr.raster)
  
  probables <- extract(x = predictions, y = actual)
  bg.probables <- extract(x = predictions, y = usr.bg)

  # For models that need to be transformed from logit to probabilities
  p <- exp(probables)/(1 + exp(probables))
  a <- exp(bg.probables)/(1 + exp(bg.probables))

  # Pack it all up
  pred.df <- data.frame(prob = c(p, a),
                      obs = c(rep("presence", length(p)), rep("absence", length(a))))

  #
  a <- getECE(pred.df$obs, pred.df$prob, n_bins = 11)
  #b <- get_ECE_equal_width(pred.df$obs, pred.df$prob)
  c <- getMCE(pred.df$obs, pred.df$prob, n_bins = 11)
  #d <- get_MCE_equal_width(pred.df$obs, pred.df$prob)
  
  ECE <- rbind(ECE, a)
  #ECE.equal.width <- rbind(ECE.equal.width, b)
  MCE <- rbind(MCE, c)
  #MCE.equal.width <- rbind(MCE.equal.width, d)
  }

CE_Stats <- data.frame(ECE = ECE,
               #ECE.equal.width = ECE.equal.width,
               MCE = MCE)
               #MCE.equal.width = MCE.equal.width)

return(CE_Stats)
}
