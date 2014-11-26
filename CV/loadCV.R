source("../util/error_func.R")
source("../util/date_helpers.R")
source("../util/plot_helpers.R")
source("../util/preprocess.R")
source("../models/load/load_model.R")
source("../models/load/temp/temp_model.R")
source("../models/load/feature_helpers.R")

### call: Rscript loadCV.R --args [#model]

calcPosition <- function(leaderboard, row.index, score) {
  col.index <- 1
  for(j in 1:ncol(leaderboard)) {
    board.entry <- leaderboard[row.index, j]
    if(board.entry >= score) {
      print(board.entry)
      col.index <- j
      return(col.index)
    }
  }
  return(ncol(leaderboard))
}

aschr <- function(date) {
  return(as.character(date))
}

args <- as.numeric(commandArgs(TRUE)[-1])
print(args)

num.model <- args[1]
k <- num.model
print(k)

dir.exists <- function(path) FALSE

# clear workspace: rm(list = ls(all = TRUE))

use.temp <- args[2]
pred.traintemp <- args[3]
use.pca <- args[4]
use.temp1 <- args[5]
htype <- args[6]
units <- args[7]

in.path <- "data/load/train"
out.path <- "data/load/test/CV"

leaderboard.path <- "data/load/submissions/leaderboard.csv"
firstpos_benchmark.path <- "data/load/submissions/firstpos_benchmark.csv"
leaderboard <- read.csv(leaderboard.path, header=TRUE, row.names=1, sep=",")
print(leaderboard)
firstpos_benchmark <- read.csv(firstpos_benchmark.path, header=TRUE, sep=",")

getPredictionType(htype, units) {
  if(htype == 2) {
    return(paste0(units, "m"))
  } else if(htype == 1) {
    return(paste0(units, "w"))
  } else {
    return(paste0(units, "d"))
  }
}

if(htype == 2) {
  hdesc <- "monthlyhorz" 
  hinfo <- "months"
} else if(htype == 1) {
  hdesc <- "weeklyhorz"
  hinfo <- "weeks"
} else {
  hdesc <- "dailyhorz"
  hinfo <- "days"
}


today <- as.character(Sys.Date())
if(pred.traintemp) today <- paste0(today, "_train_pred")
if(use.pca) today <- paste0(today, "_pca") else if(use.temp1) today <- paste0(today, "_temp1")
today.path <- paste(out.path, today, sep="/")
if (!dir.exists(today.path)) dir.create(today.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

last.dt <- getLastDt()
train.df <- createTrainDF(loadCSVs(in.path), getFirstDt(), getLastDt())

### PREPARE DATA ###
temp.df <- reduceToTempDF(train.df)
avg.temp.series <- avgTempSeries(temp.df)
if (use.temp1) {
    temp1.series <- temp.df[ ,c(1,2)]
    colnames(temp1.series) <- c("TMS", "MTEMP")
    avg.temp.series <- temp1.series
}
#avg.temp.list.yearly <- listSeriesByYear(avg.temp.series, "empm")
#avg.temp.yearly <- mergeSeriesByHour(avg.temp.list.yearly)
hash <- hashDtYear(avg.temp.series$TMS)
avg.temp <- cbind(avg.temp.series, HASH=hash)

pca <- prcomp(temp.df[,-1], retx=TRUE, tol=0.2)
pc1 <- pca$x
colnames(pc1) <- "MTEMP"
pc.temp <- data.frame(TMS=avg.temp.series$TMS, MTEMP=pc1, HASH=hash)
if(use.pca) avg.temp <- pc.temp

load.df <- na.omit(reduceToLoadDF(train.df)) # remove first years of NA values 
hash <- hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)

### DEFINE DATES & HORIZON ###

# test set
test.start.dt <- addYears(getFirstDt(), 10)
test.dt <- test.start.dt
test.init.dt <- subMonths(test.start.dt, 3)
test.dt <- test.init.dt
print(paste0("testdt", test.dt))
test.len <- 8 + 4 

test.horizon <- units

# training set
train.start.dt <- addYears(getFirstDt(), 2)
train.dt <- train.start.dt
print(paste0("traindt", train.dt))
train.horizon <- 8*12 - 3 

load.train.start.dt <- subYears(test.start.dt, 5)
load.train.dt <- load.train.start.dt
print(paste0("loadtraindt", load.train.dt))
load.train.horizon <- 5*12 - 3 #train.horizon - 5*12

test.first <- format(test.dt, "%d-%m-%Y")
test.last <- format(as.Date(addMonths(test.dt, test.len)), "%d-%m-%Y")
subfolder <- paste(aschr(test.first), aschr(test.last), sep="_")
test.path <- paste(today.path, subfolder, sep="/")

test.path <- paste(today.path, as.character(as.Date(addMonths(test.dt, test.len))), sep="/")
if (!dir.exists(test.path)) dir.create(test.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

out.path <- paste(test.path, as.character(train.horizon), as.character(load.train.horizon), sep="/")
if (!dir.exists(out.path)) dir.create(out.path, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# gam models
temp.model.formulas <-list("mean",
                      #"s(LAGM)",
                      #"s(LAGM) + s(LAGMD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(WLAG52, by=TOY, k=24) + s(HOUR, k=24)",
                      "s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      #"s(LAGM) + s(LAGMD) + s(DLAG, TOY, k=52)",
                      "s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, by=TOY, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, by=MONTH, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)")

#temp.model.formulas <- list("mean")
# What about mean feature?

# with 5week lag, once without
load.model.formulas <- list("s(CTEMP, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(MTL7D, k=24) + s(MAXT24H, k=52) + s(MINT24H, k=52) + s(TM24H, k=52) + s(TM48H, k=52) + s(TM2H) + s(TM1H)")

#"DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=24) + s(DLAG, k=24) + s(WLAG52, k=24)",
### CROSSVALIDATION ###

CV.res <- list()
load.features <- createLoadFeatures(load.df, load.train.dt, load.train.horizon + test.len)
temp.features <- createTempFeatures(avg.temp, train.dt, train.horizon + test.len)
print("going on")

feat.arr <- c(paste0("model", k), hdesc, test.horizon)
fn <- paste("loadCV", paste(feat.arr, collapse="_"), sep="_")
fn.pdf <- paste0(fn, "_plots.pdf")
fn.csv <- paste0(fn, "_results.csv")
plots.path <- paste(out.path, fn.pdf, sep="/")
pdf(file=plots.path, width=8, height=11)
par(mfrow=c(3,1))
output.file <- paste(out.path, fn.csv, sep="/")

#for(k in 1:length(load.model.formulas)) {
load.model.chr <- paste0("Load Model ", k, ": ", load.model.formulas[[k]])
load.train.chr <- paste0("Load Model Training Length: ", load.train.horizon, " ", hinfo)
appendToFile(load.model.chr, output.file)
appendToFile(load.train.chr, output.file)
cat("\n", file = output.file, append = TRUE)

for(i in 1:length(temp.model.formulas)) {
  PINBALL <- c()
  POSITIONS <- c()
  temp.model.chr <- paste0("Temp Model ", i, ": ", temp.model.formulas[[i]])
  temp.train.chr <- paste0("Temp Model Training Length: ", train.horizon, " ", hinfo)
  appendToFile(temp.model.chr, output.file)
  appendToFile(temp.train.chr, output.file)
  cat("\n", file = output.file, append = TRUE)
  temp.load.train.dt <- train.dt
  temp.load.pred.dt <- load.train.dt

  flex.horizon <- 3*12
  if(pred.traintemp) {
    index <- which(avg.temp$HASH==hashDtYear(load.train.dt))
    avg.temp <- avg.temp[-c(index:nrow(avg.temp)), ]
    write.table(avg.temp, "test.csv")
    for (h in 1:(load.train.horizon+test.len)) {
      print(h)
out.path <- paste(test.path, as.character(train.horizon), as.character(load.train.horizon), sep="/")
if (!dir.exists(out.path)) dir.create(out.path, showWarnings = TRUE, recursive = TRUE, mode = "0777")

# gam models
temp.model.formulas <-list("mean",
                      #"s(LAGM)",
                      #"s(LAGM) + s(LAGMD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(WLAG52, by=TOY, k=24) + s(HOUR, k=24)",
                      "s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      #"s(LAGM) + s(LAGMD) + s(DLAG, TOY, k=52)",
                      "s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, by=TOY, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, by=MONTH, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)")

#temp.model.formulas <- list("mean")
# What about mean feature?

# with 5week lag, once without
load.model.formulas <- list("s(CTEMP, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(MTL7D, k=24) + s(MAXT24H, k=52) + s(MINT24H, k=52) + s(TM24H, k=52) + s(TM48H, k=52) + s(TM2H) + s(TM1H)")

#"DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=24) + s(DLAG, k=24) + s(WLAG52, k=24)",
### CROSSVALIDATION ###

CV.res <- list()
load.features <- createLoadFeatures(load.df, load.train.dt, load.train.horizon + test.len)
temp.features <- createTempFeatures(avg.temp, train.dt, train.horizon + test.len)
print("going on")

if(htype == 2) {
  hdesc <- "monthlyhorz" 
  hinfo <- "months"
} else if(htype == 1) {
  hdesc <- "weeklyhorz"
  hinfo <- "weeks"
} else {
  hdesc <- "dailyhorz"
  hinfo <- "days"
}

feat.arr <- c(paste0("model", k), hdesc, test.horizon)
fn <- paste("loadCV", paste(feat.arr, collapse="_"), sep="_")
fn.pdf <- paste0(fn, "_plots.pdf")
fn.csv <- paste0(fn, "_results.csv")
plots.path <- paste(out.path, fn.pdf, sep="/")
pdf(file=plots.path, width=8, height=11)
par(mfrow=c(3,1))
output.file <- paste(out.path, fn.csv, sep="/")

#for(k in 1:length(load.model.formulas)) {
load.model.chr <- paste0("Load Model ", k, ": ", load.model.formulas[[k]])
load.train.chr <- paste0("Load Model Training Length: ", load.train.horizon, " ", hinfo)
appendToFile(load.model.chr, output.file)
appendToFile(load.train.chr, output.file)
cat("\n", file = output.file, append = TRUE)

for(i in 1:length(temp.model.formulas)) {
  PINBALL <- c()
  POSITIONS <- c()
  temp.model.chr <- paste0("Temp Model ", i, ": ", temp.model.formulas[[i]])
  temp.train.chr <- paste0("Temp Model Training Length: ", train.horizon, " ", hinfo)
  appendToFile(temp.model.chr, output.file)
  appendToFile(temp.train.chr, output.file)
  cat("\n", file = output.file, append = TRUE)
  temp.load.train.dt <- train.dt
  temp.load.pred.dt <- load.train.dt

  flex.horizon <- 3*12
  if(pred.traintemp) {
      test.features <- getFeatures(temp.features, temp.load.pred.dt, test.horizon, test.horizon, htype)
      
      pred.stop.dt <- getStopDtByHorizon(temp.load.pred.dt, test.horizon, htype)
      test.dt.seq <- seq(temp.load.pred.dt, pred.stop.dt, by="hour")
      index1 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[1], arr.ind=TRUE)
      index2 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[length(test.dt.seq)], arr.ind=TRUE)
      
      if(temp.model.formulas[[i]] == "mean") {
        test.fit <- temp.features[index1:index2, "LAGM"]      
        target <- temp.features[index1:index2, "LAGM"]      
      }
      else {
        temp.model <- trainTempModel(train.features, temp.model.formulas[[i]], temp.load.train.dt)
        test.fit <- predict.gam(temp.model, test.features[, -(1:2)])
        target <- temp.features[index1:index2, 2]
      }
      fit <- data.frame(TMS=test.dt.seq, MTEMP=test.fit, HASH=hashDtYear(test.dt.seq))
      #colnames(fit) <- colnames(avg.temp)

      #index <- which(avg.temp$HASH == hashDtYear(temp.load.pred.dt), arr.ind = TRUE)  
      #if (length(index) != 0) avg.temp <- avg.temp[-c(index:nrow(avg.temp)),]
      pred.temp <- fit
      avg.temp <- rbind(avg.temp, pred.temp)
      # Update
      temp.load.pred.dt <- incrementDt(temp.load.pred.dt, test.horizon, htype)
      print(paste0("temp.load.train.dt", temp.load.train.dt))
      print(paste0("temp.load.pred.dt", temp.load.pred.dt))
      flex.horizon <- flex.horizon + 1
    }
  }
  print(tail(avg.temp))
  for (j in 1:test.len) {
   print(j)
    ### TEMPERATURE VALIDATION
    if (!pred.traintemp) {
      print("notusingpredictedtemp")
      train.features <- getFeatures(temp.features, train.dt, test.horizon, train.horizon, htype)
      test.features <- getFeatures(temp.features, test.dt, test.horizon, test.horizon, htype)
      
#       train.result <- trainLoadModelFormula(load.train.features, load.model.formulas[[k]], train.dt)
#       load.model <- train.result[["model"]]  
#       capture.output(summary(load.model), file="load_models_CV.txt", append=TRUE)
#       train.residuals <- train.result[["residuals"]]

      test.stop.dt <- getStopDtByHorizon(test.dt, test.horizon, htype)
      test.dt.seq <- seq(test.dt, test.stop.dt, by="hour")
      index1 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[1], arr.ind=TRUE)
      index2 <- which(temp.features$HASH==hashDtYear(test.dt.seq)[length(test.dt.seq)], arr.ind=TRUE)
      
      if(temp.model.formulas[[i]] == "mean") {
        test.fit <- temp.features[index1:index2, "LAGM"]      
      }
      else {
        temp.model <- trainTempModel(train.features, temp.model.formulas[[i]], train.dt)
        test.fit <- predict.gam(temp.model, test.features[, -(1:2)])
      }
      fit <- data.frame(TMS=test.dt.seq, MTEMP=test.fit, HASH=hashDtYear(test.dt.seq))
      #colnames(fit) <- colnames(avg.temp)
      pred.temp <- fit
      
      if(temp.model.formulas[[i]] == "mean") {
        target <- temp.features[index1:index2, "LAGM"]
      }
      else {
        target <- temp.features[index1:index2, 2]
      }
      #plotTraining(test.dt.seq, target, fit$MTEMP, 0, xlabel=paste(as.character(test.dt), "1 month in hours", sep=" +"),
      #             ylabel="Temperature in Fahrenheit", title=paste0("Temperature Model Validation, Model: ", temp.model.formulas[[i]]))
      
      ## why this?
      if (test.dt < getLastDt() && !pred.traintemp) {
        index <- which(avg.temp$HASH == hashDtYear(test.dt), arr.ind = TRUE)  
        if (length(index) != 0) avg.temp <- avg.temp[-c(index:nrow(avg.temp)),]
      }
      if (use.temp) {
        pred.temp <- data.frame(TMS=test.dt.seq, MTEMP=target, HASH=hashDtYear(test.dt.seq))
      }
      avg.temp <- rbind(avg.temp, pred.temp)
      print(tail(avg.temp))
    }
    
