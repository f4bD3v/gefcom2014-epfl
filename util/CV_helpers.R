calcPosition <- function(leaderboard, row.index, score) {
  col.index <- 1
  for(j in 1:ncol(leaderboard)) {
    board.entry <- leaderboard[row.index, j]
    if(board.entry >= score) {
      col.index <- j
      return(col.index)
    }
  }
  return(ncol(leaderboard))
}

getPredictionType <- function(htype, units) {
  if(htype == 2) {
    return(paste0(units, "m"))
  } else if(htype == 1) {
    return(paste0(units, "w"))
  } else {
    return(paste0(units, "d"))
  }
}

htypeToString <- function(htype) {
  if(htype == 2) {
    return("months")
  } else if(htype == 1) {
    return("weeks")
  } else {
    return("days")
  }
}

aschr <- function(date) {
  return(as.character(date))
}

writeLoadHeader <- function(scores.path, load.method, load.method.formula, interval, load.train.month.len, str.htype, load.train.start.dt) {
	load.method.str <- paste0("Load Method ", load.method.names[[load.method]], "; ", interval, " prediction intervals; using formula: ", load.method.formula) 
	load.train.str <- paste0("Load Model Training Length: ", load.train.month.len, " ", str.htype, " starting ", load.train.start.dt)
	writeToFile(load.method.str, scores.path)
	appendToFile(load.train.str, scores.path)
	cat("\n", file=scores.path, append = TRUE)
}

writeTempHeader <- function(scores.path, temp.method, temp.method.formula, interval, train.start.dt, train.stop.dt, temp.train.len, str.htype) { 
	temp.model.str <- paste0("Temp Method ", temp.method, "; ", interval, " prediction intervals; using formula: ", temp.method.formula)
	temp.train.str <- paste0("Temp Training Period: ", as.character(as.Date(train.start.dt)), "-", as.character(as.Date(train.stop.dt)), "; Length: ", temp.train.len, " ", str.htype)
	writeToFile(temp.model.str, scores.path)
	appendToFile(temp.train.str, scores.path)
	cat("\n", file=scores.path, append = TRUE)
}


writeLoadTempHeader <- function(scores.path, no.temp.formula, temp.method, temp.method.formula, pred.temp, interval) {
	if(no.temp.formula) {
		temp.model.str <- paste0("Temp Source ", temp.method)
		temp.train.str <- paste0("No model training necessary")
	} else {
		### TODO INCLUDE OPTIONS
		temp.model.str <- paste0("Temp Method ", temp.method, "; ", interval, " prediction intervals; using formula: ", temp.method.formula)
		### TODO: GET temp.train.month.len from .rds attribute, as well as first date and last date
		temp.start.dt <- attr(pred.temp, "train.start")
		temp.stop.dt <- attr(pred.temp, "train.stop")
		temp.len <- attr(pred.temp, "train.len")
		temp.str.htype <- attr(pred.temp, "str.htype")
		temp.train.str <- paste0("Temp Training Period: ", as.character(as.Date(temp.start.dt)), "-", as.character(as.Date(temp.stop.dt)), "; Length: ", temp.len, " ", str.htype)
	}
	appendToFile(temp.model.str, scores.path)
	appendToFile(temp.train.str, scores.path)
	cat("\n", file=scores.path, append = TRUE)
}

underscoreJoin <- function(elem1, elem2) {
	elem <- paste(elem1, elem2, sep="_")
	return(elem)
}

pathJoin <- function(path1, path2) {
	path <- paste(path1, path2, sep="/")
	return(path)
}

extensionJoin <- function(filename, extension) {
	filename <- paste0(filename, ".", extension)
	return(filename)
}

createDir <- function(folder.path) {
	if (!dir.exists(folder.path)) dir.create(folder.path, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

createBaseFolder <- function(out.path, test.start.dt, test.stop.dt) {
	test.first <- format(test.start.dt, "%d-%m-%Y")
	test.last <- format(test.stop.dt, "%d-%m-%Y")

	folder <- underscoreJoin(aschr(test.first), aschr(test.last))
	folder.path <- pathJoin(out.path, folder)
	createDir(folder.path)
	return(folder.path)
}

createMethodFolder <- function(base.path, type, pred.method, method.option, formula, PCA, station) {
	path <- pathJoin(base.path, type)
	createDir(path)
	folder <- pred.method
	if(PCA) {
		folder <- underscoreJoin(pred.method, "PCA")
	} else if(station != FALSE) {
		folder <- underscoreJoin(pred.method, paste0("Station", station))
	}
	method.path <- pathJoin(path, folder)
	createDir(method.path)
	if(method.option != "NONE") {
		option <- method.option
		# hidden unit, ntree case
		method.path <- pathJoin(method.path, option)
		createDir(method.path)
	}
	if(formula != "NONE") {
		formula <- underscoreJoin("formula", formula)
		method.path <- pathJoin(method.path, formula)
		createDir(method.path)
	}
	return(method.path)
}

createFolder <- function(base.path, folder.name) {
	path <- pathJoin(base.path, folder.name)
	createDir(path)
	return(path)
}

getTempMethodPaths <- function(temp.method.path, temp.method, temp.method.option, no.temp.formula) {
	temp.method.options <- list()
	temp.method.options.paths <- list()
	temp.method.formulas <- list()
	temp.method.formulas.paths <- list()
	if(temp.method.option == "NONE") {
		temp.method.options.paths <- list(pathJoin(temp.method.path, temp.method))
		temp.method.options <- list(temp.method)
		dirs <- list.dirs(temp.method.path, full.names = TRUE, recursive=FALSE)
  		dirs <- dirs[order(nchar(dirs), dirs)]
  		if(no.temp.formula) {
			temp.method.formulas.paths[[temp.method]] <- list(temp.method.path)
			temp.method.formulas[[temp.method]] <- list(temp.method)
		} else {
			temp.method.formulas.paths[[temp.method]] <- list()
			temp.method.formulas[[temp.method]] <- list()
		}
		formula.count <- 1
		for(dir in dirs) {
			if(grepl('formula', dir)) {
				temp.method.formulas.paths[[temp.method]][[formula.count]] <- dir
				temp.method.formulas[[temp.method]][[formula.count]] <- temp.methods.formulas[[temp.method]][[formula.count]]
				formula.count <- formula.count + 1
			}		
		}
	} else {
		dirs <- list.dirs(temp.method.path, full.names = TRUE, recursive=FALSE)
  		dirs <- dirs[order(nchar(dirs), dirs)]
		option.count <- 1

		for(dir in dirs) {
			if(grepl('(hidden-units|ntrees)', dir)) {
				print(dir)
				temp.method.options.paths[[option.count]] <- dir
				dir.parts <- strsplit(dir, "/")[[1]]
				option.name <- dir.parts[length(dir.parts)]
				print(option.name)
				temp.method.options[[option.count]] <- option.name
				temp.method.options.paths[[option.count]] <- list()
				sub.dirs <- list.dirs(dir, full.names = TRUE, recursive=FALSE)
				temp.method.formulas.paths[[option.name]] <- list()
				temp.method.formulas[[option.name]] <- list()
				formula.count <- 1
				for(sub.dir in sub.dirs) {
					if(grepl('formula', sub.dir)) {
						temp.method.formulas.paths[[option.name]][[formula.count]] <- sub.dir
						temp.method.formulas[[option.name]][[formula.count]] <- temp.methods.formulas[[temp.method]][[formula.count]]
						formula.count <- formula.count + 1
					}
				}
				option.count <- option.count + 1
			}
		}
	}
	return(list(temp.method.options.paths, temp.method.options, temp.method.formulas.paths, temp.method.formulas))
}
