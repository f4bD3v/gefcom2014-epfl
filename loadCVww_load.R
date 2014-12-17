source("config.R")
source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")
source("util/CV_helpers.R")

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
global.htype <- args[6]
htype <- global.htype
units <- args[7]

print(htype)

pred.type <- getPredictionType(htype, units)

### LOAD LEADERBOARD ###
leaderboard.path <- "data/load/competition-stats/leaderboard.csv"
firstpos_benchmark.path <- "data/load/competition-stats/firstpos_benchmark.csv"
leaderboard <- read.csv(leaderboard.path, header=TRUE, row.names=1, sep=",")
firstpos_benchmark <- read.csv(firstpos_benchmark.path, header=TRUE, sep=",")
#########################


### PREPARE DATA ###
#last.dt <- getLastDt()
train.df <- createTrainDF(loadCSVs(in.path), first.dt, last.dt)
train.df.fn <- paste0("train-df", ".rds")
saveRDS(train.df, file=train.df.fn, compress=TRUE)

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
load.df <- reduceToLoadDF(train.df)
load.df.fn <- paste0("load-df", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)

hash <- hashDtYear(load.df$TMS)
load.df <- cbind(load.df, HASH=hash)
load.df.fn <- paste0("load-df-hash", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)

start.index <- which(load.df$HASH == hashDtYear(addYears(first.dt, 4)))
load.df <- load.df[start.index:nrow(load.df),]
#load.df <- na.omit(load.df) # remove first years of NA values 
load.df.fn <- paste0("load-df-omit", ".rds")
saveRDS(load.df, file=load.df.fn, compress=TRUE)

#####################


### DEFINE DATES & HORIZON ###

#** TEST DATES **#
test.month.len <- 2 

test.stop.dt <- last.dt 
test.start.dt <- subMonths(addHours(test.stop.dt, 1), test.month.len)
test.dt <- test.start.dt
last.test.dt <- test.dt
cat(paste0("Test Start Dt: ", test.start.dt), sep="\n")
cat(paste0("Test Stop Dt: ", test.stop.dt), sep="\n")
cat("\n")

test.horizon <- units

#** TEMP TRAINING DATES **#
temp.train.year.len <- 7

temp.train.stop.dt <- subHour(test.start.dt)
temp.train.start.dt <- subYears(test.start.dt, temp.train.year.len)
cat(paste0("Train Start Dt: ", temp.train.start.dt), sep="\n")
cat(paste0("Train Stop Dt: ", temp.train.stop.dt), sep="\n")
cat("\n")
temp.train.dt <- temp.train.start.dt

temp.train.month.len <- temp.train.year.len * 12

#** LOAD TRAINING SET **#
load.train.year.len <- 4

load.train.stop.dt <- temp.train.stop.dt
load.train.start.dt <- subYears(test.start.dt, load.train.year.len)
load.train.dt <- load.train.start.dt
last.load.train.dt <- load.train.dt
cat(paste0("Load Train Start Dt: ", load.train.dt), sep="\n")
cat("\n")

load.train.month.len <- load.train.year.len * 12
################################################


### CREATE FOLDER STRUCTURE ###
today <- format(Sys.Date(), "%d-%m-%Y")

if(pred.traintemp) folder <- paste0(today, "_train_pred") else if(use.pca) folder <- paste0(today, "_pca") else if(use.temp1) folder <- paste0(today, "_temp1")

folder.path <- paste(out.path, today, sep="/")
if (!dir.exists(folder.path)) dir.create(folder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

#---

test.first <- format(test.start.dt, "%d-%m-%Y")
test.last <- format(test.stop.dt, "%d-%m-%Y")

subfolder <- paste(aschr(test.first), aschr(test.last), pred.type, sep="_")
subfolder.path <- paste(folder.path, subfolder, sep="/")
if (!dir.exists(subfolder.path)) dir.create(subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

tempf.subfolder.path <- paste(subfolder.path, "temp-features", sep="/")
if (!dir.exists(tempf.subfolder.path)) dir.create(tempf.subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
loadf.subfolder.path <- paste(subfolder.path, "load-features", sep="/")
if (!dir.exists(loadf.subfolder.path)) dir.create(loadf.subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

#--- FILES
str.htype <- htypeToString(htype)

### ADAPT FILES ACCORDING TO TEMP MODELS AT LATER STAGE
#* pdf
fn.pdf <- paste0(paste("plots", pred.type, sep="_"), ".pdf")
plots.path <- paste(subfolder.path, fn.pdf, sep="/")
pdf(file=plots.path, width=8, height=11)
par(mfrow=c(3,1))

#* csv
fn.csv <- paste0(paste("scores", pred.type, sep="_"), ".csv")
output.file <- paste(subfolder.path, fn.csv, sep="/")

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
load.features <- createLoadFeatures(load.df, load.train.dt, load.train.month.len + test.month.len)
load.features.fn <- paste0("all-load-features", ".rds")
saveRDS(load.features, file=paste(loadf.subfolder.path, load.features.fn, sep="/"), compress=TRUE)

#** createTempFeatures:
# -
# -
# -
# -
# -
temp.features <- createTempFeatures(avg.temp, temp.train.dt, temp.train.month.len + test.month.len)
temp.features.fn <- paste0("all-temp-features", ".rds")
saveRDS(temp.features, file=paste(tempf.subfolder.path, temp.features.fn, sep="/"), compress=TRUE)


#** CREATE LOOP VARS MONTH AND WEEK PREDICTIONS
loop.vars = list(c(2,1),
			c(1,1),
  			c(1,2),
  			c(1,3),
  			c(1,4))

#** CREATE LIST OF RESULT TABLES
res <- list()


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

  #### TODO: CHECK IF THERE IS PROBLEM WITH AVG.TEMP IN LOOP

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
        # predict temperature on test.horizon rolling basis until test.stop.dt
    } else {
        #** Test Period Temp Prediction **#
        temp.load.train.dt <- load.train.dt
        temp.load.pred.dt <- test.start.dt

        # delete true temperature starting from initial test date
        index <- which(avg.temp$HASH==hashDtYear(test.start.dt))
        avg.temp <- avg.temp[-c(index:nrow(avg.temp)), ]
        print(tail(avg.temp, 5))

        run.len <- test.month.len
        train.len <- temp.train.month.len
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
        train.features <- getFeatures(temp.features, temp.load.train.dt, test.horizon, temp.train.month.len, htype)
        test.features <- getFeatures(temp.features, temp.load.pred.dt, test.horizon, test.horizon, htype)

    	train.features.fn <- paste0(paste("train-temp-features", "model", k, "instance", h, pred.type, sep="_"), ".rds")
    	saveRDS(train.features, file=paste(tempf.subfolder.path, train.features.fn, sep="/"), compress=TRUE)
    	test.features.fn <- paste0(paste("test-temp-features", "model", k, "instance", h, pred.type, sep="_"), ".rds")
    	saveRDS(test.features, file=paste(tempf.subfolder.path, test.features.fn, sep="/"), compress=TRUE)
        
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
        cat(paste0("Train Start Dt for Temp Prediction: ", temp.load.train.dt), sep="\n")
        cat(paste0("Test Start Dt for Temp Prediction: ", temp.load.pred.dt), sep="\n")
		cat("\n")
        # IMPLEMENT DIFFERENT WAY OF HANDLING HORIZONS
        if(pred.traintemp) {
            flex.horizon <- flex.horizon + 1
		} else {
            temp.load.train.dt <- temp.load.train.dt + 1
        }
    }
  }
  print(tail(avg.temp))

  ### CROSS VALIDATION ###
  for (j in 1:test.month.len) {
	# LOOP: CREATE MONTHLY LOAD PREDICTION (DLAG = 35), CREATE PREDICTION FOR FIRST WEEK (DLAG = 7), SECOND WEEK (DLAG = 14), THIRD WEEK (DLAG = 21), FOURTH WEEK (DLAG = 28)
	for (h in 1:length(loop.vars)) { 
		var.set <- loop.vars[[h]]
		htype = var.set[1]
		test.horizon = 1
		lag = var.set[2]

		pred.type <- getPredictionType(htype, test.horizon)

  		print(tail(avg.temp))
  		print(load.train.month.len)
		#** GET FEATURES FOR CURRENT TRAIN AND TEST PERIODS **#
		# - lag for training changes
		load.train.features <- assembleLoadFeatures(load.features, avg.temp, load.train.dt, lag, load.train.month.len, 2)
		# - htype for test horizon changes
		load.test.features <- assembleLoadFeatures(load.features, avg.temp, test.dt, lag, test.horizon, htype)

		load.train.features.fn <- paste0(paste("train-load-features", "model", k, "instance", j, pred.type, sep="_"), ".rds")
		saveRDS(load.train.features, file=paste(loadf.subfolder.path, load.train.features.fn, sep="/"), compress=TRUE)
		load.test.features.fn <- paste0(paste("test-load-features", "model", k, "instance", j, pred.type, sep="_"), ".rds")
		saveRDS(load.test.features, file=paste(loadf.subfolder.path, load.test.features.fn, sep="/"), compress=TRUE)
		
		#** CREATE & SAVE LOAD MODEL **#
		train.result <- trainLoadModelFormula(load.train.features, load.model.formulas[[k]], load.train.dt)
		load.model <- train.result[["model"]]  
		load.model.fn <- paste(subfolder.path, paste0(paste("model", k, "instance", j, pred.type, sep="_"), ".txt"), sep="/")
		capture.output(summary(load.model), file=load.model.fn)
		
		#** CREATE PREDICTION **#
		load.fit <- predict.gam(load.model, load.test.features[, -(1:2)])

		#** USE TRAINING RESIDUALS TO COMPUTE PREDICTION QUANTILES **#
		train.residuals <- train.result[["residuals"]]
		test.quantiles <- createPredQuantiles(load.fit, train.residuals)

		if(h==1) {
			chunks <- 4
		} else {
			chunks <- test.horizon
		}
		#** CREATE QUANTILE PLOT FOR EVERY WEEK **# 
		#plotPredictionQuantiles(load.test.features$TMS, load.test.features$Y, load.fit, test.quantiles, chunks,
		#						paste0(test.dt, " + 1 month in hours"), "load in MW", "Plot of Percentiles")
		
		err.scores <- pointErrorMeasures(load.test.features$Y, load.fit)
		err.pinball <- pinball(test.quantiles, load.test.features$Y)

		#** CALC ERROR FOR DAYS AFTER W4 **#
		if(h==1) {
			rest.days <- c((4*7*24):nrow(load.fit))
			if(length(rest.days) == 1) {
				rest.err.scores <- list(RMSE=0,MAE=0,MAPE=0)
				rest.err.pinball <- 0
			} else {
				rest.err.scores <- pointErrorMeasures(load.test.features$Y[rest.days], load.fit[rest.days])
				rest.err.pinball <- pinball(test.quantiles[rest.days,], load.test.features$Y[rest.days])
			}
		}

		#** CALC LEADERBOARD POSITION FOR GIVEN MONTH **#
		if(h==1) {
			position <- calcPosition(leaderboard, j, err.pinball)
			POSITIONS <- c(POSITIONS, position)
		}

    	tms.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.dt), TEST.TMS=as.character(test.dt))
    	err.row <- cbind(do.call(cbind.data.frame, err.scores), PINBALL=err.pinball)
    	res.row <- cbind(tms.row, err.row)
    	cat("h ", h, sep="\n")

		#** RECORD ERROR FOR DAYS AFTER W4 **#
		if (h==1) {
    		if (j == 1) res[[h]] <- res.row else res[[h]] <- rbind(res[[h]], res.row)
			rest.test.dt <- incrementDt(test.dt, 4, 1)
    		tms.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.dt), TEST.TMS=as.character(rest.test.dt))
    		err.row <- cbind(do.call(cbind.data.frame, rest.err.scores), PINBALL=rest.err.pinball)
    		res.row <- cbind(tms.row, err.row)
    		cat("length loop.vars", length(loop.vars), sep="\n")
    		hp <- h + 1
    		if (j == 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
    	} else {
    		hp <- h + 1
    		if (j == 1) res[[hp]] <- res.row else res[[hp]] <- rbind(res[[hp]], res.row)
    	}
		print(res[[hp]])

		#** UPDATE DATES **# 
		if(h==1) {
			last.test.dt <- test.dt
			last.load.train.dt <- load.train.dt
		}

		if(h > 1 && lag != 4) {
			load.train.dt <- incrementDt(load.train.dt, 1, htype)
			test.dt <- incrementDt(test.dt, 1, htype) 
		} else if (lag == 4) {
			load.train.dt <- incrementDt(last.load.train.dt, 1, 2)
			test.dt <- incrementDt(last.test.dt, 1, 2) 
		}
		cat(paste0("Load Train Dt: ", load.train.dt), sep="\n")
		cat(paste0("Load Test Dt: ", test.dt), sep="\n")
		cat("\n")
		#******************#
	}
  }

  score.board <- list()
  for(h in 1:(length(loop.vars)+1)) {
  	res.last.row <- cbind(LOAD.TRAIN.TMS=as.character(load.train.start.dt), TEST.TMS=as.character(last.test.dt), RMSE=mean(res[[h]]$RMSE), MAE=mean(res[[h]]$MAE), MAPE=mean(res[[h]]$MAPE), PINBALL=mean(res[[h]]$PINBALL))
  	score.board[[h]] <- rbind(res[[h]], res.last.row)
  	row.names(score.board[[h]]) <- c(c(1:test.month.len), "MODEL CV MEAN")
  }

  position.board <- cbind(MAPE=res[[1]]$MAPE, PINBALL=res[[1]]$PINBALL, POSITION=POSITIONS, PINBALL_FIRSTPOS=firstpos_benchmark[1:test.month.len, ])
  dates <- res[[1]][1:nrow(res[[1]]), 1:2]
  position.board <- cbind(dates, position.board)
  row.names(position.board) <- c(1:test.month.len)#, "MODEL CV MEAN")

  ### LATER: DO STATS AND RECOMPUTE 
  for(h in 1:(length(loop.vars)+1)) {
  	appendTableToFile(score.board[[h]], output.file)
  	cat("\n", file = output.file, append = TRUE)
	scores.subfolder.path <- paste(subfolder.path, "scores", sep="/")
	if (!dir.exists(scores.subfolder.path)) dir.create(scores.subfolder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
	if (h==1) {
		scores.fn <- paste0(paste("tempm", i, "scores", "1m", sep="_"), ".rds")

		col <- position.board[1:test.month.len, 1:4]
		comp <- col
	} else if (h==2) {
		scores.fn <- paste0(paste("tempm", i, "scores", "5wrest", sep="_"), ".rds")
	} else {
		var.set <- loop.vars[[h-1]]
		htype = var.set[1]
		test.horizon = var.set[2]
		scores.fn <- paste0(paste("tempm", i, "scores", getPredictionType(htype, test.horizon), sep="_"), ".rds")

		sb <- score.board[[h]]
		col <- sb$PINBALL[1:test.month.len]
		col <- as.numeric(col)
		prev.coln <- colnames(comp)
		comp <- cbind(comp, col)
		colnames(comp) <- c(prev.coln, c(paste0("PINBALL_w", h-2)))
	}
  	### SAVE SCORES TO RDS FILES ###
   	saveRDS(score.board[[h]], file=paste(scores.subfolder.path, scores.fn, sep="/"), compress=TRUE)
  }
  appendTableToFile(position.board, output.file)
  cat("\n", file = output.file, append = TRUE)
 
  colnames(comp) <- c("TRAIN.TMS", "TEST.TMS", "1m", "1m_pos", "1w", "2w", "3w", "4w")
  #comp <- data.frame(TRAIN.TMS=comp[,1], TEST.TMS=comp[,2], PINBALL_1m=as.numeric(comp[,3]), POS_1m=as.numeric(comp[,4]), PINBALL_1w=as.numeric(comp[,5]), PINBALL_2w=as.numeric(comp[,6]), PINBALL_3w=as.numeric(comp[,7]), PINBALL_4w=as.numeric(comp[,8]))
  comp <- data.frame(TRAIN.TMS=comp[,1], TEST.TMS=comp[,2], PINBALL_1m=comp[,3], POS_1m=comp[,4], PINBALL_1w=comp[,5], PINBALL_2w=comp[,6], PINBALL_3w=comp[,7], PINBALL_4w=comp[,8])
  #sapply(comp, mode)
  #sapply(comp, class)
  #comp[,3:8] <- sapply(comp[, 3:8], as.numeric)

  combined.score <- c()
  combined.pos <- c()
  #ul <- unname(unlist(comp))
  #print(ul)
  scores <- rowMeans(comp[, 5:8, drop=FALSE], na.rm=TRUE)
  #scores <- apply(comp[,5:8, drop=FALSE], 1, mean, na.rm=TRUE)	
  print(as.numeric(scores))
  print(scores[1])
  for(h in 1:test.month.len) {
    score <- scores[h]
	#print(paste0("score", score))
	combined.score <- c(combined.score, score)
	combined.pos <- calcPosition(leaderboard, h, score)
  }
  comparison.board <- cbind(comp, PINBALL_wavg=combined.score, POS_wavg=combined.pos)
  appendTableToFile(comparison.board, output.file)
  cat("\n", file = output.file, append = TRUE)

  comparison.board.fn <- paste0("comparison_board_tempm_", as.character(i), ".rds")
  saveRDS(comparison.board, file=paste(scores.subfolder.path, comparison.board.fn, sep="/"), compress=TRUE)
  
  ### IMPORTANT: RESET DATES ###
  temp.train.dt <- temp.train.start.dt
  load.train.dt <- load.train.start.dt
  test.dt <- test.start.dt

  htype <- global.htype
  test.horizon <- units
  ###############

  cat("\n", file = output.file, append = TRUE)
  cat("\n", file = output.file, append = TRUE)
}

dev.off()
