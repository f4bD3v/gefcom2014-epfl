source("util/error_func.R")
source("util/date_helpers.R")
source("util/plot_helpers.R")
source("util/preprocess.R")
source("models/load/load_model.R")
source("models/load/temp/temp_model.R")
source("models/load/feature_helpers.R")

dir.exists <- function(path) FALSE
# clear workspace: rm(list = ls(all = TRUE))
in.path <- "data/load/train"
out.path <- "data/load/test/temp/CV"

today <- as.character(Sys.Date())
today.path <- paste(out.path, today, sep="/")
if (!dir.exists(today.path)) dir.create(today.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

last.dt <- getLastDt()
train.df <- createTrainDF(loadCSVs(in.path), getFirstDt(), getLastDt())

### PREPARE DATA ###
temp.df <- reduceToTempDF(train.df)
avg.temp.series <- avgTempSeries(temp.df)
avg.temp.list.yearly <- listSeriesByYear(avg.temp.series, "empm")
avg.temp.yearly <- mergeSeriesByHour(avg.temp.list.yearly)
avg.temp <- cbind(avg.temp.series, HASH=hashDtYear(avg.temp.series$TMS))

### CROSSVALIDATION ###

# test set
test.start.dt <- addYears(getFirstDt(), 10)
test.dt <- test.start.dt
test.stop.dt <- subHours(addMonth(test.dt), 1)
test.horizon <- 1 
test.len <-7 

# training set
train.start.dt <- addYears(getFirstDt(), 2)
train.dt <- train.start.dt
train.stop.dt <- subHours(test.dt, 1)
train.horizon = 8*12 

# "WLAG52 + TOY + HOUR", 
# "WLAG5 + WLAG52 + TOY + HOUR",
# "WLAG52 + s(TOY, k=10) + s(HOUR, k=10)",
# "s(WLAG5, k=24) + s(WLAG52, k=24) + s(TOY, k=10) + s(HOUR, k=10)",
# "s(WLAG5, k=40) + s(WLAG52, k=24) + s(TOY, k=12) + s(HOUR, k=24)",

test.path <- paste(today.path, as.character(as.Date(addMonths(test.dt, test.len))), sep="/")
if (!dir.exists(test.path)) dir.create(test.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")

out.path <- paste(test.path, as.character(train.horizon), sep="/")
if (!dir.exists(out.path)) dir.create(out.path, showWarnings = TRUE, recursive = TRUE, mode = "0777")# gam models

model.formulas <-list("mean",
                      "s(LAGM)",
                      "s(LAGM) + s(LAGMD)",
                      "s(LAGM) + s(LAGMD) + s(LAGSD)",
                      "s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN)",
                      "s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      "s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      "s(LAGM) + s(WLAG52, by=TOY, k=24) + s(HOUR, k=24)",
                      "s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(WLAG5, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(LAGM) + s(LAGMD) + s(WLAG5, TOY, k=52)",
                      "s(WLAG5, TOY, k=52) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(WLAG5, by=TOY, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(WLAG5, by=MONTH, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)")
# What about mean feature?

CV.res <- list()
CV.models <- list()
features <- createTempFeatures(avg.temp, train.dt, train.horizon + test.len)

plots.path <- paste(out.path, "tempCVplots.pdf", sep="/")
pdf(file=plots.path)
output.file <- paste(out.path, "tempCVlog.csv", sep="/")
for(k in 1:length(model.formulas)) {
  temp.model.chr <- paste0("Temp Model ", k, ": ", model.formulas[[k]])
  temp.train.chr <- paste0("Temp Model train horizon", train.horizon)
  appendToFile(temp.model.chr, output.file)
  appendToFile(temp.train.chr, output.file)
  rmse.vec <- numeric(test.len)
  mae.vec <- rmse.vec
  mape.vec <- rmse.vec
  train.dt.vec <- character(test.len) 
  test.dt.vec <- train.dt.vec
  
  for (i in 1:test.len) {
    train.features <- getFeatures(features, train.dt, train.horizon)
    test.features <- getFeatures(features, test.dt, test.horizon)
    if(model.formulas[[k]] == "mean") {
      point.errs <- pointErrorMeasures(test.features$Y, test.features$LAGM)
    } else {
      temp.model <- trainTempModel(train.features, model.formulas[[k]], train.dt)
      capture.output(summary(temp.model), file="temp_models.txt", append=TRUE)
      test.res <- validateTempModel(temp.model, test.features, test.dt, model.formulas[[k]])
      point.errs <- test.res[["point.errs"]]
    }  
    
    # use hash instead of TMS?
    tms.row <- cbind(TRAIN.TMS=as.character(train.dt), TEST.TMS=as.character(test.dt))
    err.row <- do.call(cbind.data.frame, point.errs)
    res.row <- cbind(tms.row, err.row)
    if (i == 1) res <- res.row else res <- rbind(res, res.row)
    
    # Update
    train.dt <- addMonth(train.dt)
    print(paste0("traindt",train.dt))
    test.dt <- addMonth(test.dt)
    print(paste0("testdt",test.dt))
  }
  res.final.row <- cbind(TRAIN.TMS=as.character(test.start.dt), TEST.TMS=as.character(addMonth(subHours(test.dt, 1))), RMSE=mean(res$RMSE), MAE=mean(res$MAE), MAPE=mean(res$MAPE))
  res <- rbind(res, res.final.row)
  row.names(res) <- c(c(1:test.len), "MODEL CV MEAN")
  appendTableToFile(res, output.file)
  
  CV.res[[k]] <- res
  
  train.dt <- train.start.dt
  test.dt <- test.start.dt
  
  #write.table(results.df, paste(paste("models/load/CV/", "results", sep=""), "csv", sep="."), quote=FALSE, row.names=FALSE, sep=",")
  appendToFile("\n", output.file)
}
dev.off()
#lapply(mylist, write.table, "Results_TempCV.txt", append=TRUE) # specify ncolumns?