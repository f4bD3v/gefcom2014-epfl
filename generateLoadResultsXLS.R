#require('WriteXLS')
require('XLConnect')
require('methods')
# http://stackoverflow.com/questions/17623697/how-can-i-automate-data-frame-naming-in-r

sheetNameFromAttr <- function(df.attr, pred.train) {
	sheet.name <- ''
	tempm <- df.attr$temp_method
	sheet.name <- tempm
	if(grepl("(MEAN|TRUE)", tempm)) {
		return(sheet.name)
	} else {
		tempm.opt <- df.attr$temp_method_option
		formula.i <- df.attr$temp_method_formula_index
		if(tempm == tempm.opt) {
			sheet.name <- paste(sheet.name, tempm.opt, sep="_")
			sheet.name <- gsub('hidden-units', 'hu', sheet.name)
			sheet.name <- gsub('ntrees', 'nt', sheet.name)
			sheet.name <- paste(sheet.name, paste0("f", formula.i), sep="_")
			if(pred.train == FALSE) {
				sheet.name <- paste(sheet.name, 'no-pt', sep="_")
			}
			return(sheet.name)
		}	
		sheet.name <- paste(sheet.name, paste0("f", formula.i), sep="_")
		if(pred.train == FALSE) {
			sheet.name <- paste(sheet.name, 'no-pt', sep="_")
		}
		return(sheet.name)
	}
}

createLoadHeader <- function(df.attr) {
	loadm <- df.attr$load_method
	if(grepl("(MEAN|TRUE)", loadm)) {
		loadm.opt <- "NONE"
		loadm.formula <- "NONE"
	} else {
		loadm.opt <- df.attr$load_method_option
		# normally already set to NONE for load
		if(loadm == loadm.opt) {
			loadm.opt <- "NONE"
		}
		loadm.formula <- df.attr$load_formula
		loadm.formula.i <- df.attr$load_formula_index
	}
	train.len <- paste(df.attr$load_train_len, "months", sep=" ")
	load.header <- data.frame(LOAD.METHOD=loadm, LOAD.OPTION=loadm.opt, LOAD.FORMULA.IND=loadm.formula.i, LOAD.FORMULA=loadm.formula, LOAD.TRAINING=train.len) 
	return(load.header)
}
			
createTempHeader <- function(df.attr) {
	tempm <- df.attr$temp_method
	if(grepl("(MEAN|TRUE)", tempm)) {
		tempm.opt <- "NONE"
		tempm.formula <- "NONE"
	} else {
		tempm.opt <- df.attr$temp_method_option
		if(tempm == tempm.opt) {
			tempm.opt <- "NONE"
		}
		tempm.formula <- df.attr$temp_method_formula
		tempm.formula.i <- df.attr$temp_method_formula_index
	}
	train.len <- paste(df.attr$temp_train_len, "months", sep=" ")
	temp.header <- data.frame(TEMP.METHOD=tempm, TEMP.OPTION=tempm.opt, TEMP.FORMULA.IND=tempm.formula.i, TEMP.FORMULA=tempm.formula, TEMP.TRAINING=train.len) 
	return(temp.header)
}

createTempSheet <- function(wb, file, df, pred.train) {
	df.attr <- attributes(df)
	sheet.name <- sheetNameFromAttr(df.attr, pred.train)
	createSheet(wb, sheet.name)

	load.head.df <- createLoadHeader(df.attr)
	writeWorksheet(wb, data=load.head.df, startRow=2, startCol=2, sheet=sheet.name, header=TRUE, rownames=NULL)
	temp.head.df <- createTempHeader(df.attr)
	writeWorksheet(wb, data=temp.head.df, startRow=5, startCol=2, sheet=sheet.name, header=TRUE, rownames=NULL)

	writeWorksheet(wb, data=df, startRow=8, startCol=2, sheet=sheet.name, header=TRUE, rownames=NULL)
}

createMethodOptionXLS <- function(method, option, path, meth.dir) {
	parts <- strsplit(path, "/")[[1]]
	formula <- parts[[length(parts)]]
	scores.path <- paste(path, "scores", sep="/")
	# weekly vs monthly temp in different XLS
	horizons <- c("weekly", "monthly")
	for(h in 1:length(horizons)) { 
		ntemp <- paste(horizons[h], "temp", sep="-")
		if(option != FALSE) wb.name <- paste(method, option, formula, ntemp, sep="_") else wb.name <- paste(method, formula, ntemp, sep="_")
		wb.fn <- paste(meth.dir, paste0(wb.name, '.xlsx'), sep="/")
		wb <- loadWorkbook(wb.fn, create = TRUE)
		temp.dirs <- list.dirs(scores.path, full.names = TRUE, recursive=FALSE)
		for(temp.dir in temp.dirs) {
			# get only comparison boards
			horz.pattern <- paste0('comparison(.*)', horizons[h], '\\.rds')
			# weekly vs monthly temp
			files <-  dir(temp.dir, pattern=horz.pattern, full.names = TRUE) 
			files <- files[order(nchar(files), files)]
			# predtrain vs no predtrain
			# how to handle predtrain?
			#if(pred.train) temp.files <- dir(temp.formula.path, pattern='^predtrain(.*)\\.rds', full.names = TRUE) else temp.files <- dir(temp.formula.path, full.names=TRUE)[grepl("^((?<!predtrain).)*\\.rds", dir(temp.formula.pat
			for(file in files) {
				df <- readRDS(file)
				pred.train <- grepl('predtrain', file)
				createTempSheet(wb, file, df, pred.train)
			}
		}
		print(paste0("saving ", wb.fn))
		saveWorkbook(wb, wb.fn)
	}
}

path <- '.'
dirs <- list.dirs(path, full.names = TRUE, recursive=FALSE)
for(meth.dir in dirs) {
	parts <- strsplit(meth.dir, "/")[[1]]
	method <- parts[[length(parts)]]
	opt.dirs <- list.dirs(meth.dir, full.names = TRUE, recursive=FALSE)
	for(opt.dir in opt.dirs) {
		if(grepl('formula', opt.dir)) {
			createMethodOptionXLS(method, FALSE, opt.dir, meth.dir)
		} else {
		#option
			parts <- strsplit(opt.dir, "/")[[1]]
			option <- parts[[length(parts)]]
			form.dirs <- list.dirs(opt.dir, full.names = TRUE, recursive=FALSE)
			for(form.dir in form.dirs) {
				if(grepl('formula', form.dir)) {
					createMethodOptionXLS(method, option, form.dir, opt.dir)
				}
			}		
		}
	}
}
