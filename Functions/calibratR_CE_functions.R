# This code comes from the CalibratR package, and is being copy/pasted because it's not exported by the original package.
getECE <- function(actual, predicted, n_bins=10){ #equal frequency bins

  predicted <- predicted
  labels <- actual
  idx <- order(predicted)
  pred_actual <- (cbind(predicted[idx], labels[idx]))

  N <- nrow(pred_actual)
  rest <- N%%n_bins
  S <- 0
  W <- c()
  B <- min(N,n_bins) #if less then n_bins elements in data set, then use that number of bins
  groups <- list()

  for (i in 1:B){ #i von 1 bis B
    if (i <= rest){ #put rest elements into each bin
      group_pred <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),1])
      group_actual <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),2])
    }
    else {
      group_pred <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),1])#group size=N/B
      group_actual <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),2])
    }

    n_ <- length(group_pred)
    expected <- mean(group_pred) #mean of predictions in bin b
    observed <- mean(group_actual) #true fraction of pos.instances = prevalence in bin b

    S[i] <- abs(observed-expected) #absolut difference of observed value-predicted value in bin
    W[i] <- n_/N #empirical frequence of all instances that fall into bin i, should be equal when using equal freq binning approach
    groups[[i]] <- group_pred

  }

  mean_prediction <- lapply(groups, mean)
  min_group <- lapply(groups, min)
  max_group <- lapply(groups, max)

  res <- t(S)%*%W
  return(as.numeric(res))
}


# This code comes from the CalibratR package, and is being copy/pasted because it's not exported by the original package.
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

# This code comes from the CalibratR package, and is being copy/pasted because it's not exported by the original package.
getMCE <- function(actual, predicted, n_bins=10){

  predicted <- predicted
  labels <- actual
  idx <- order(predicted)
  pred_actual <- (cbind(predicted[idx], actual[idx]))
  N <- nrow(pred_actual)
  rest <- N%%n_bins
  B <- min(N,n_bins)

  S <- 0
  W <- c()
  for (i in 1:B){ #i von 1 bis B
    if (i <= rest){ #put rest elements into each bin
      group_pred <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),1])
      group_actual <- (pred_actual[(((i-1)*ceiling(N/n_bins)+1) : (i*ceiling(N/n_bins))),2])
    }
    else {
      group_pred <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),1])#group size=N/B
      group_actual <- (pred_actual[((rest+(i-1)*floor(N/n_bins)+1) : (rest+i*floor(N/n_bins))),2])
    }

    n <- length(group_pred)
    expected <- mean(group_pred) #mean of predictions in bin b
    observed <- mean(group_actual) #true fraction of pos.instances = prevalence in bin b

    S[i] <- abs(observed-expected) #absolut difference of observed value-predicted value in bin
    W[i] <- n/N #empirical frequence of all instances that fall into bin i, should be pretty much the same among all bins
  }

  res <- max(S*W)
  return(res)
}

# This code comes from the CalibratR package, and is being copy/pasted because it's not exported from that package.
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
