source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")
source("CV_helpers.R")

### call: Rscript loadCV.R --args [#model]
in.path <- "data/load/train"
out.path <- "output/test/CV"

args <- as.numeric(commandArgs(TRUE)[-1])
print(args)

num.model <- args[1]
k <- num.model

dir.exists <- function(path) FALSE
# clear workspace: rm(list = ls(all = TRUE))

use.temp <- args[2]
pred.traintemp <- args[3]
use.pca <- args[4]
use.temp1 <- args[5]
htype <- args[6]
units <- args[7]


### LOAD LEADERBOARD ###
leaderboard.path <- "data/load/submissions/leaderboard.csv"
firstpos_benchmark.path <- "data/load/submissions/firstpos_benchmark.csv"
leaderboard <- read.csv(leaderboard.path, header=TRUE, row.names=1, sep=",")
print(leaderboard)
firstpos_benchmark <- read.csv(firstpos_benchmark.path, header=TRUE, sep=",")
#########################


### PREPARE DATA ###
last.dt <- getLastDt()
train.df <- createTrainDF(loadCSVs(in.path), getFirstDt(), last.dt)

#** TEMPERATURE **#
temp.df <- reduceToTempDF(train.df)
# Use average temperature over 25 stations
avg.temp.series <- avgTempSeries(temp.df)
# Use first series
if (use.temp1) {
    temp1.series <- temp.df[ ,c(1,2)]
    colnames(temp1.series) <- c("TMS", "MTEMP")
    avg.temp.series <- temp1.series
}
# hash timestamps for easy comparison operations
hash <- hashDtYear(avg.temp.series$TMS)
avg.temp <- cbind(avg.temp.series, HASH=hash)

#** TEMP PCA **#
pca <- prcomp(temp.df[,-1], retx=TRUE, tol=0.2)
pc1 <- pca$x
colnames(pc1) <- "MTEMP"
pc.temp <- data.frame(TMS=avg.temp.series$TMS, MTEMP=pc1, HASH=hash)
if(use.pca) avg.temp <- pc.temp

#** LOAD **#
load.df <- na.omit(reduceToLoadDF(train.df)) # remove first years of NA values 
hash <- hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)
######################


### DEFINE DATES & HORIZON ###

#** TEST DATES **#
test.stop.dt <- last.dt 
test.start.dt <- subYear(addHours(test.stop.dt, 1))
print(paste0("Test Start Dt", test.start.dt))
print(paste0("Test Stop Dt", test.stop.dt))

test.month.len <- 12
test.horizon <- units

#** TEMP TRAINING DATES **#
temp.train.year.len <- 7

temp.train.stop.dt <- subHours(test.start.dt, 1)
temp.train.start.dt <- subYears(test.start.dt, temp.train.year.len)
print(paste0("Train Start Dt", temp.train.dt))
print(paste0("Train Stop Dt", temp.train.start.dt))
temp.train.dt <- temp.train.start.dt

temp.train.month.len <- temp.train.year.len * 12

#** LOAD TRAINING SET **#
load.train.year.len <- 4

load.train.stop.dt <- temp.train.stop.dt
load.train.start.dt <- subYears(test.start.dt, load.train.year.len)
load.train.dt <- load.train.start.dt
print(paste0("Load Train Start Dt", load.train.dt))

load.train.month.len <- load.train.year.len * 12
################################################


### CREATE FOLDER STRUCTURE ###
today <- format(as.character(Sys.Date()), "%d-%m-%Y")

if(pred.traintemp) folder <- paste0(today, "_train_pred") else if(use.pca) folder <- paste0(today, "_pca") else if(use.temp1) folder <- paste0(today, "_temp1")

folder.path <- paste(out.path, today, sep="/")
if (!dir.exists(folder.path)) dir.create(folder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

#---

test.first <- format(test.start.dt, "%d-%m-%Y")
test.last <- format(test.stop.dt, "%d-%m-%Y")
pred.type <- getPredictionType(htype, units)

subfolder <- paste(aschr(test.first), aschr(test.last), pred.type, sep="_")
subfolder.path <- paste(folder.path, subfolder, sep="/")
if (!dir.exists(subfolder.path)) dir.create(test.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

#--- FILES
str.htype <- htypeToString(htype)

#* pdf
fn.pdf <- paste0(paste("plots", pred.type, sep="_"), ".pdf")
plots.path <- paste(out.path, fn.pdf, sep="/")
pdf(file=plots.path, width=8, height=11)
par(mfrow=c(3,1))

#* csv
fn.csv <- paste0(paste("scores", pred.type, sep="_"), ".csv")
output.file <- paste(out.path, fn.csv, sep="/")

# Where to put length of train and test?
########################################


### DEFINE GAM MODELS ###

#** TEMPERATURE MODELS **#
# limit number of models - to decrease amount of computation
# mean - is direct average over yearly lags, no model involved
temp.model.formulas <-list("mean",
                      #"s(LAGM)",
                      #"s(LAGM) + s(LAGMD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(WLAG52, by=TOY, k=24) + s(HOUR, k=24)",
                      #"s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      #"s(LAGM) + s(LAGMD) + s(DLAG, TOY, k=52)",
                      "s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, by=TOY, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)")
                      #"s(DLAG, by=MONTH, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)")

#** LOAD MODELS **#
load.model.formulas <- list("s(CTEMP, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
                            "s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(MTL7D, k=24) + s(MAXT24H, k=52) + s(MINT24H, k=52) + s(TM24H, k=52) + s(TM48H, k=52) + s(TM2H) + s(TM1H)")
#"DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=24) + s(DLAG, k=24) + s(WLAG52, k=24)",
#####################################


### CREATE FEATURES FOR WHOLE CV ###
#** createLoadFeatures: 
# - load.df: dataframe of tms, load, hash
# - start.dt: start of load training
# - horizon: length of features in htype respective units (until test.stop.dt)
# - htype=2
load.features <- createLoadFeatures(load.df, load.train.dt, load.train.horizon + test.len)

#** createTempFeatures:
# -
# -
# -
# -
# -
temp.features <- createTempFeatures(avg.temp, temp.train.dt, temp.train.month.len + test.month.len)
print("going on")


#-- PRINT LOAD MODEL TYPE TO FILE
load.model.chr <- paste0("Load Model ", k, ": ", load.model.formulas[[k]])
load.train.chr <- paste0("Load Model Training Length: ", load.train.month.len, " ", str.htype)
appendToFile(load.model.chr, output.file)
appendToFile(load.train.chr, output.file)
cat("\n", file = output.file, append = TRUE)
#-----------------------------



#########################
### CROSS VALIDATION ###
########################
for(i in 1:length(temp.model.formulas)) {
  PINBALL <- c()
  POSITIONS <- c()
  #-- PRINT TEMP MODEL TYPE TO FILE
  temp.model.chr <- paste0("Temp Model ", i, ": ", temp.model.formulas[[i]])
  temp.train.chr <- paste0("Temp Model Training Length: ", temp.train.month.len, " ", str.htype)
  appendToFile(temp.model.chr, output.file)
  appendToFile(temp.train.chr, output.file)
  cat("\n", file = output.file, append = TRUE)
  #-------------------------------

  #** PREDICTED TEMPERATURE VS TRUE TEMPERATURE **#
  if(!use.temp) {
    if(pred.traintemp) {
        #** LOAD Train and Test Period Temp Prediction
        temp.load.train.dt <- temp.train.dt
        temp.load.pred.dt <- load.train.dt

        flex.horizon <- temp.train.month.len - load.train.month.len
        train.len <- flex.horizon

        # delete true temperature starting from load training initial date
        index <- which(avg.temp$HASH==hashDtYear(load.train.dt))
        avg.temp <- avg.temp[-c(index:nrow(avg.temp)), ]
        print(tail(avg.temp, 5))

        run.len <- load.train.month.len + test.month.len
    } else {
        #** Test Period Temp Prediction **#
        temp.load.train.dt <- load.train.dt
        temp.load.pred.dt <- test.start.dt

        # delete true temperature starting from initial test date
        index <- which(avg.temp$HASH==hashDtYear(test.start.dt))
        avg.temp <- avg.temp[-c(index:nrow(avg.temp)), ]
        print(tail(avg.temp, 5))

        run.len <- test.month.len
        train.len <- temp.train.len
        # predict temperature on test.horizon rolling basis until test.stop.dt
    }
    for (h in 1:run.len) {
        # increase training period with every month
        # getFeatures()
        # - feature.df
        # - start.dt
        # - lag.horizon for days.lag
        # - horizon
        # - htype`
        train.features <- getFeatures(temp.features, temp.load.train.dt, test.horizon, temp.train.len, htype)
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
        pred.temp <- fit
        avg.temp <- rbind(avg.temp, pred.temp)

        temp.load.pred.dt <- incrementDt(temp.load.pred.dt, test.horizon, htype)
        print(paste0("temp.load.train.dt", temp.load.train.dt))
        print(paste0("temp.load.pred.dt", temp.load.pred.dt))
        # IMPLEMENT DIFFERENT WAY OF HANDLING HORIZONS
        if(pred.traintemp) {
            flex.horizon <- flex.horizon + 1
        else {
            temp.load.train.dt <- temp.load.train.dt + 1
        }
    }
  }
  print(tail(avg.temp))

  ### CROSS VALIDATION ###
  for (j in 1:test.len) {
    #** GET FEATURES FOR CURRENT TRAIN AND TEST PERIODS **#
    load.train.features <- assembleFeatures(load.features, avg.temp, load.train.dt, test.horizon, load.train.horizon, htype)
    load.test.features <- assembleFeatures(load.features, avg.temp, test.dt, test.horizon, test.horizon, htype)

    load.train.features.fn <- paste0(paste("load-train-features", "model", k, "instance", j, pred.type, sep="_"), ".txt")
    saveRDS(load.train.features, file=paste(out.path, load.train.features.fn, sep="/"), compress=TRUE)
    load.test.features.fn <- paste0(paste("load-test-features", "model", k, "instance", j, pred.type, sep="_"), ".txt")
    saveRDS(load.test.features, file=paste(out.path, load.test.features.fn, sep="/"), compress=TRUE)
    
    #** CREATE & SAVE LOAD MODEL **#
    train.result <- trainLoadModelFormula(load.train.features, load.model.formulas[[k]], load.train.dt)
    load.model <- train.result[["model"]]  
    load.model.fn <- paste(out.path, paste0(paste("model", k, "instance", j, pred.type, sep="_"), ".txt"), sep="/")
    capture.output(summary(load.model), file=load.model.fn)
    
    #** CREATE PREDICTION **#
    load.fit <- predict.gam(load.model, load.test.features[, -(1:2)])

    #** USE TRAINING RESIDUALS TO COMPUTE PREDICTION QUANTILES **#
    train.residuals <- train.result[["residuals"]]
    test.quantiles <- createPredQuantiles(load.fit, train.residuals)

    #** CREATE QUANTILE PLOT FOR EVERY WEEK **# 
    plotPredictionQuantiles(load.test.features$TMS, load.test.features$Y, load.fit, test.quantiles, 4,
                            paste0(test.dt, " + 1 month in hours"), "load in MW", "Plot of Percentiles")
    
    err.scores <- pointErrorMeasures(load.test.features$Y, load.fit)
    err.pinball <- pinball(test.quantiles, load.test.features$Y)

    #** CALC LEADERBOARD POSITION FOR GIVEN MONTH **#
    position <- calcPosition(leaderboard, j, err.pinball)
    POSITIONS <- c(POSITIONS, position)

    tms.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.dt), TEST.TMS=as.character(test.dt))
    err.row <- cbind(do.call(cbind.data.frame, err.scores), PINBALL=err.pinball)
    res.row <- cbind(tms.row, err.row)
    if (j == 1) res <- res.row else res <- rbind(res, res.row)

    #** UPDATE DATES **# 
    load.train.dt <- incrementDt(load.train.dt, test.horizon, htype)
    test.dt <- incrementDt(test.dt, test.horizon, htype) 
    print(paste0("Load Train Dt",load.train.dt))
    print(paste0("Load Test Dt",test.dt))
    #******************#
  }
  res.last.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.start.dt), TEST.TMS=as.character(incrementDt(subHours(test.dt, 1), test.horizon, htype)), RMSE=mean(res$RMSE), MAE=mean(res$MAE), MAPE=mean(res$MAPE), PINBALL=mean(res$PINBALL))
  score.board <- rbind(res, res.last.row)

  position.board <- cbind(MAPE=res$MAPE, PINBALL=res$PINBALL, POSITION=POSITIONS, firstpos_benchmark[1:test.len, ])
  dates <- res[1:(nrow(res)), c(1,2)]

  position.board <- cbind(dates, position.board)

  row.names(res) <- c(c(1:test.len), "MODEL CV MEAN")
  row.names(scores) <- c(c(1:test.len))#, "MODEL CV MEAN")

  appendTableToFile(res, output.file)
  cat("\n", file = output.file, append = TRUE)
  appendTableToFile(scores, output.file)
  cat("\n", file = output.file, append = TRUE)
  
  ### IMPORTANT: RESET DATES ###
  temp.train.dt <- temp.train.start.dt
  load.train.dt <- load.train.start.dt
  test.dt <- test.start.dt
  ###############

  cat("\n", file = output.file, append = TRUE)
  cat("\n", file = output.file, append = TRUE)
}

dev.off()
