#require('WriteXLS')
require('XLConnect')
require('methods')
# http://stackoverflow.com/questions/17623697/how-can-i-automate-data-frame-naming-in-r

convert.magic <- function(obj, type){
    FUN1 <- switch(type, character = as.character, numeric = as.numeric, factor = as.factor)
    out <- lapply(obj, FUN1)
    as.data.frame(out)
}

sheetNameFromAttr <- function(df.attr, method) {
	sheet.name <- ''
	tempm <- method 
	print(tempm)
	sheet.name <- tempm
	if(grepl("(MEAN|TRUE)", tempm)) {
		return(sheet.name)
	} else {
		tempm.opt <- df.attr$option
		print(tempm.opt)
		formula.i <- df.attr$formula_index
		if(tempm != tempm.opt) {
			sheet.name <- paste(sheet.name, tempm.opt, sep="_")
			sheet.name <- gsub('hidden-units', 'hu', sheet.name)
			sheet.name <- gsub('ntrees', 'nt', sheet.name)
			sheet.name <- paste(sheet.name, paste0("f", formula.i), sep="_")
			return(sheet.name)
		}	
		sheet.name <- paste(sheet.name, paste0("f", formula.i), sep="_")
		return(sheet.name)
	}
}
		
createTempHeader <- function(df.attr, method) {
	tempm <- method
	print(tempm)
	if(grepl("(MEAN|TRUE)", tempm)) {
		tempm.opt <- "NONE"
		tempm.formula <- "NONE"
	} else {
		tempm.opt <- df.attr$option
		if(tempm == tempm.opt) {
			tempm.opt <- "NONE"
		}
		tempm.formula <- df.attr$formula
		tempm.formula.i <- df.attr$formula_index
	}
	train.len <- paste(df.attr$train.len, "months", sep=" ")
	temp.header <- data.frame(TEMP.METHOD=tempm, TEMP.OPTION=tempm.opt, TEMP.FORMULA.IND=tempm.formula.i, TEMP.FORMULA=tempm.formula, TEMP.TRAINING=train.len) 
	return(temp.header)
}

createTempSheet <- function(wb, path, method) {
	parts <- strsplit(path, "/")[[1]]
	formula <- parts[[length(parts)]]
	scores.path <- paste(path, "scores", sep="/")
	# weekly vs monthly temp in different XLS
	horz.pattern <- paste0('comparison(.*)', '\\.rds')
	# weekly vs monthly temp
	file <-  dir(scores.path, pattern=horz.pattern, full.names = TRUE) 
	print(file)
	df <- readRDS(file)

	parts <- strsplit(formula, "_")[[1]]
	fit.formula <- paste0("predtrain(.*)", parts[[length(parts)]], "_monthly")
	fits <-  dir(path, pattern=fit.formula, full.names = TRUE) 
	print(fits)
	fit.df <- readRDS(fits)

	# get attributes from fits
	df.attr <- attributes(fit.df)
	sheet.name <- sheetNameFromAttr(df.attr, method)
	createSheet(wb, sheet.name)

	temp.head.df <- createTempHeader(df.attr, method)
	writeWorksheet(wb, data=temp.head.df, startRow=2, startCol=2, sheet=sheet.name, header=TRUE, rownames=NULL)

    df[, c(3:length(df))] <- convert.magic(df[, c(3:length(df))], "numeric") 
    df <- df[c((nrow(df)-15):nrow(df)),]
    print(tail(df))
	writeWorksheet(wb, data=df, startRow=5, startCol=1, sheet=sheet.name, header=TRUE, rownames=row.names(df))
}

createMethodOptionXLS <- function(method, meth.dir, opt.dirs) {
	wb.fn <- paste0(method, '.xlsx')
	print(wb.fn)
    if (file.exists(wb.fn)) file.remove(wb.fn)
	wb <- loadWorkbook(wb.fn, create = TRUE)
	if(grepl("(TRUE|MEAN)", method)) {
		return()
	}
	for(opt.dir in opt.dirs) {
		if(grepl('formula', opt.dir)) {
			print(opt.dir)
			createTempSheet(wb, opt.dir, method)
		} else {
		#option
			parts <- strsplit(opt.dir, "/")[[1]]
			option <- parts[[length(parts)]]
			form.dirs <- list.dirs(opt.dir, full.names = TRUE, recursive=FALSE)
			print(form.dirs)
			for(form.dir in form.dirs) {
				if(grepl('formula', form.dir)) {
					print(form.dir)
					createTempSheet(wb, form.dir, method)
				}
			}
		}
	}
	saveWorkbook(wb, wb.fn)
	return()
}

path <- '.'
dirs <- list.dirs(path, full.names = TRUE, recursive=FALSE)
for(meth.dir in dirs) {
	print(meth.dir)
	parts <- strsplit(meth.dir, "/")[[1]]
	method <- parts[[length(parts)]]
	opt.dirs <- list.dirs(meth.dir, full.names = TRUE, recursive=FALSE)
	createMethodOptionXLS(method, meth.dir, opt.dirs)
}
