### TODO: find way to pass plot function name and then call function from savePlot
savePlot <- function(path, fn, begin.dt, PLOTFUN, param.list) {
  png(filename=paste("models/load/temp/plots", paste(paste(as.character(begin.dt), as.character(addMonth(begin.dt)), sep="_"), "png", sep="."), sep="/"))
  PLOTFUN(unlist(param.list))
  dev.off()
}

plotSimple <- function(tms, y, xlabel, ylabel, title, ylimits) {
  plot(y ~ tms, col="blue", type="l", ylim=ylimits, xlab=xlabel, ylab=ylabel, main=title, xaxt='n')  
  axis.Date(1, at=tms[hour(tms)%%24==1], format="%H")
  #axis(1, dm$Date, format(dm$Date, "%b %d"), cex.axis = .7)
}

plotSet <- function(tms, y, xlabel, ylabel, title, ylimits) {
  plot(y ~ tms, col="blue", type="l", ylim=ylimits, xlab=xlabel, ylab=ylabel, main=title, xaxt='n')  
  seq <- seq(tms[1], tms[length(tms)], by='years')# tms[hour(tms)%%(24*365)==3]
  #axis(1, dm$Date, format(dm$Date, "%b %d"), cex.axis = .7)
  axis(1, at=seq, labels=format(seq, "%d/%y"), cex.axis=.9)

}
plotLoad <- function(tms, y, xlabel, ylabel, title, ylimits) {
  plot(y ~ tms, col="blue", type="l", ylim=ylimits, xlab=xlabel, ylab=ylabel, main=title, xaxt="n")  
  #axis.Date(1, at=tms[hour(tms)%%24==1], format="%d-%m-%Y", labels=TRUE)
  # %a - abbreviated weekday name
  seq <- tms[hour(tms)%%24==12]
  #axis.Date(1, at=seq, format=("%a %d %b"), tick=FALSE)
  axis(1, at=seq, labels=format(seq, "%a %d %b"), cex.axis=.9)
}

plotTraining <- function(tms, target, fit, residuals, xlabel, ylabel, title) {
  print(tail(target))
  print(tail(fit))
  plot(target ~ tms, col="black", type="l", xlab=xlabel, ylab=ylabel, main=title)
  lines(fit ~ tms, col="red", type="l")
  axis.Date(1, at=tms[mday(tms)%%7==1], format="%d-%m-%Y")
  #lines(residuals, col="red")
}

# same as for prediction?
plotValidation <- function(fit, xlabel, ylabel, title) {
  plot(fit, col="black", type="l", xlab=xlabel, ylab=ylabel, main=title)
  # fit
  # target
  # residuals
}

plotPredictionResiduals <- function(tms, target, fit, residuals, chunks, xlabel, ylabel, title, ylimits) {
  listToChunks <- function(x, n) { split(x, cut(1:length(x), n, labels=FALSE)) }
  #matrixToChunks <- function(x, n) { split(as.data.frame(x), cut(1:nrow(x), n, labels=FALSE)) }
  target.chunks <- listToChunks(target, chunks)
  fit.chunks <- listToChunks(fit, chunks)
  tms.chunks <- listToChunks(tms, chunks)
  offset <- max(abs(residuals))+10
  residuals.chunks <- listToChunks(residuals, chunks)
  length.chunk <- length(residuals.chunks)

  for(k in 1:chunks) {
    plot(fit.chunks[[k]] ~ tms.chunks[[k]], col="red", type="l", ylim=ylimits, xlab=xlabel, ylab=ylabel, main=title)  
    # or, only plot for indices in q.seq
    #lines(rep(mean(residuals)+offset, length(tms.chunks[[k]])) ~ tms.chunks[[k]], col="blue", type="l")
    #lines(residuals.chunks[[k]]+offset ~ tms.chunks[[k]], col="red", type="l")
    # lw - line width
    lines(target.chunks[[k]] ~ tms.chunks[[k]], col="black", type="l")
    axis.Date(1, at=tms.chunks[[k]][mday(tms.chunks[[k]])%%7==1], format="%d-%m-%Y")
  }
}

plotPredictionQuantiles <- function(tms, target, fit, fit.quantiles, chunks, xlabel, ylabel, title) {
  #xlab="Hours", ylab="Temperature in Fahrenheit", main=paste("Validation period starting from", valid_chr, sep=" "))
  listToChunks <- function(x, n) { split(x, cut(1:length(x), n, labels=FALSE)) }
  matrixToChunks <- function(x, n) { split(as.data.frame(x), cut(1:nrow(x), n, labels=FALSE)) }
  target.chunks <- listToChunks(target, chunks)
  fit.chunks <- listToChunks(fit, chunks)
  fitq.chunks <- matrixToChunks(fit.quantiles, chunks)
  tms.chunks <- listToChunks(tms, chunks)
  
  #q.seq <- union(c(1),seq(0,ncol(fit.quantiles),9)[-1])
  for(k in 1:chunks) {
    plot(fit.chunks[[k]] ~ tms.chunks[[k]], col="white", type="l", ylim=c(40, 320), xlab=xlabel, ylab=ylabel, main=title)  
    # or, only plot for indices in q.seq
    for (i in 1:ncol(fit.quantiles)) {
      lines(fitq.chunks[[k]][, i] ~ tms.chunks[[k]], col="yellow", type="l")
    }
    # lw - line width
    lines(target.chunks[[k]] ~ tms.chunks[[k]], col="black", type="l")
    lines(fit.chunks[[k]] ~ tms.chunks[[k]], col="red", type="l")
    axis.Date(1, at=tms.chunks[[k]][mday(tms.chunks[[k]])%%7==1], format="%d-%m-%Y")
  }
}
