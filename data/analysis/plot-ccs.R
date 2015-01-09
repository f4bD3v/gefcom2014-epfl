require('lubridate')
in.path <- 'data/analysis/'

tstations <- readRDS(paste0(in.path, "temp-df.rds"))
tstations.avg <- readRDS(paste0(in.path, "temp-stations-avg-df.rds"))
pca.tstations.avg <- readRDS(paste0(in.path, "temp-stations-pca-df.rds"))
#tstations <- readRDS("temp-df.rds")
#tstations.avg <- readRDS("temp-stations-avg-df.rds")

w.stations <- tstations[, 2:ncol(tstations)]
w.station.1 <- w.stations[, 1]
fn.pdf <- 'weather-station-cc-plots.pdf'
pdf(file=fn.pdf, width=8, height=11)
#par(mfrow=c(1,1))
#ccf(w.station.1, w.station.1)
par(mfrow=c(5,2), mai=c(0.65,0.65,0.65,0.65))
for(i in 1:ncol(w.stations)) {
  title <- paste("CC for Temperature Series\n of Weather Stations 1 and", i)
  ccf(w.station.1, w.stations[, i], lag.max=35*24, type="covariance", main=title)
}
dev.off()

avg.series.by.year <- split(tstations.avg, year(as.Date(tstations.avg$TMS)))
i <- 1
for(avg.series.df in avg.series.by.year) {
  curr.temp <- avg.series.df[, -1]  
  row <- cbind(mean(curr.temp), median(curr.temp), sd(curr.temp), min(curr.temp), max(curr.temp))  
  if(i==1) avg.temp.stats.by.year <- row else avg.temp.stats.by.year <- rbind( avg.temp.stats.by.year, row)
  i <- i + 1
}
colnames(avg.temp.stats.by.year) <- c("Mean", "Median", "StD", "Min", "Max")
print(avg.temp.stats.by.year)
rownames(avg.temp.stats.by.year) <- seq(2001,2011)
write.table(avg.temp.stats.by.year, file=paste0(in.path, "temp-stats.csv"), col.names=NA, sep=",")
print(avg.temp.stats.by.year)

fn.pdf <- paste0(in.path, 'acf-comparison.pdf')
pdf(file=fn.pdf, width=8, height=11)
par(mfrow=c(3,1), mai=c(0.65,0.65,0.65,0.65))
acf(w.station.1, lag.max = 35*24, main="Autocorrelation for Temperature Series of First Weather Station")
acf(tstations.avg$MTEMP, lag.max = 35*24, main="Autocorrelation for Average Temperature Series")
acf(pca.tstations.avg, lag.max = 35*24, main="Autocorrelation for First Principal Component extract from all Weather Series")
dev.off()

acf(w.station.1, lag.max=100)
acf(tstations.avg$MTEMP, lag.max=8760)

train.df <- readRDS(paste0(in.path, "train-df.rds"))
load <- na.omit(train.df$LOAD)
load.tms <- na.omit(train.df[,2:3])
load.by.year <- split(load.tms, year(as.Date(load.tms$TIMESTAMP)))

load.lags <- c(1,7,14,21,28,35)

fn.pdf <- paste0(in.path, 'acf-load-lag-var-days.pdf')
pdf(file=fn.pdf, width=8, height=11)
par(mfrow=c(3,2), mai=c(0.65,0.65,0.65,0.65))
for(ll in load.lags) {
  title <- paste0("Load Series ACF up until lag of ", ll, " days")
  acf(load, lag.max= 24*ll, main=title)
}
dev.off()
    

i <- 1
for(load.df in load.by.year) {
  curr.load <- load.df[, -1]  
  row <- cbind(mean(curr.load), median(curr.load), sd(curr.load), min(curr.load), max(curr.load))  
  if(i==1) load.stats.by.year <- row else load.stats.by.year <- rbind(load.stats.by.year, row)
  i <- i + 1
}
plot(curr.load, type="l")
colnames(load.stats.by.year) <- c("Mean", "Median", "StD", "Min", "Max")
rownames(load.stats.by.year) <- c(2005,2006,2007,2008,2009,2010,2011)
write.table(load.stats.by.year, file=paste0(in.path, "load-stats.csv"), col.names=NA, sep=",")
print(load.stats.by.year)

load.mean <- mean(load)
load.sd <- sd(load)
load.min <- min(load)
load.max <- max(load)