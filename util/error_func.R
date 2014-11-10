# Root Mean Squared Error, Mean Absolute Error
pointErrorMeasures <- function(target, fit) {
  rmse <- rmse(target, fit)
  mae <- mae(target, fit)
  mape <- mape(target, fit)
  return(list(RMSE=rmse, MAE=mae, MAPE=mape))
}

# Root Mean Squared Error
rmse <- function(target, fit) {
  rmse <- sqrt( mean((target-fit)^2 , na.rm = TRUE) )
}

# Mean Absolute Error 
mae <- function(target, fit) {
  mae <- mean( abs(target-fit), na.rm = TRUE )
  return(mae)
}

mape <- function(target, fit) {
  mape <- mean( abs(target-fit)/target, na.rm = TRUE )
}

# Pinball Error
pinball <- function(fit_quantiles, target) {
  q <- seq(from=0.01, to=0.99, by=0.01)
  scores <- matrix(0, nrow=length(target), ncol=length(q))
  m_lscores <- list()
  # loop over targets
  for(i in 1:length(target)) {
    # evaluate score L for every quantile
    y <- target[i]
    for(j in 1:length(q)) {
      qij <- fit_quantiles[i,j]
      if(y < qij) {
        scores[i,j] <- (1-q[j])*(qij - y)
      } else {
        scores[i,j] <- q[j]*(y - qij)
      }
    }
  }
  pinball_loss <- mean(apply(scores, 1, mean))
  return(pinball_loss)  
}