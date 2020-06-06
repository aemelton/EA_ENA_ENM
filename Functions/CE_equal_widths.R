##########

##########
get_ECE_equal_width <- function(actual, predicted, bins=10){ #equal width bins
  
  pred_actual <- cbind(predicted, actual)
  
  if(all(predicted<=1) && all(predicted>=0)){
    hist_x <- graphics::hist(pred_actual[,1], breaks=seq(0,1,1/bins), plot=F)
  }
  else{
    hist_x <- graphics::hist(pred_actual[,1], breaks=bins, plot=F)
  }
  
  breaks_y <- hist_x$breaks
  y_true <- graphics::hist(subset(pred_actual[,1], pred_actual[,2]=="1"), breaks=breaks_y, plot=F)
  divided <- cut(pred_actual[,1], breaks=c(hist_x$breaks), label = seq(1,length(y_true$mids)), include.lowest = T)
  prediction_in_bin <- list()
  expected <- c()
  
  for (i in as.numeric(levels(divided))){
    prediction_in_bin[[i]] <- pred_actual[which(divided==i),1]
    expected[i] <- mean(prediction_in_bin[[i]]) #mean prediction in that bin
    #expected[i] <- hist_x$mids[i] #hist mids as mean prediction in that bin
  }
  
  
  counts_all <- hist_x$counts
  counts_true <- y_true$counts
  zeros <- which(counts_all==0)
  
  prevalence <- counts_true/counts_all
  prevalence[zeros] <- 0 #set prevalence to 0 when no observations are in the bin
  expected[zeros] <- hist_x$mids[zeros] #set expectation to the mid bin point, when no elements are in bin
  
  S_2 <- abs(prevalence-expected)
  W_2 <- counts_all/(length(predicted))
  
  
  return(as.numeric(t(S_2)%*%W_2))
}

get_MCE_equal_width <- function(actual, predicted, bins=10){ #equal width bins
  
  predicted <- predicted
  labels <- actual
  idx <- order(predicted)
  pred_actual <- (cbind(predicted[idx], labels[idx]))
  
  hist_x <- graphics::hist(pred_actual[,1],breaks=bins, plot=F)
  breaks_y <- hist_x$breaks
  y_true <- graphics::hist(subset(pred_actual[,1], pred_actual[,2]=="1"),breaks=breaks_y, plot=F)
  divided <- cut(pred_actual[,1], breaks=c(hist_x$breaks),label = seq(1,length(y_true$mids)),include.lowest = T)
  prediction_in_bin <- list()
  expected <- c()
  
  for (i in as.numeric(levels(divided))){
    prediction_in_bin[[i]] <- pred_actual[which(divided==i),1]
    #expected[i] <- hist_x$mids[i] #mean prediction in that bin
    expected[i] <- mean(pred_actual[which(divided==i),1]) #mean prediction in that bin
  }
  
  counts_all <- hist_x$counts
  counts_true <- y_true$counts
  zeros <- which(counts_all==0)
  
  prevalence <- counts_true/counts_all
  prevalence[zeros] <- 0 #set prevalence to 0 when no observations are in the bin
  expected[zeros] <- hist_x$mids[zeros] #set expectation to the mid bin point, when no elements are in bin
  
  S_2 <- abs(prevalence-expected)
  W_2 <- counts_all/(length(predicted))
  return(max(S_2*W_2))
}