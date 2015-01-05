### GEFCOM PROJECT CONFIG ###
source("~/gefcom2014-epfl/util/date_helpers.R")

#** SET FIRST AND LAST DATES OF TRAINING SET **#
first.dt <- as.POSIXct("01/01/2001 01:00", format="%m/%d/%Y %H:%M", tz="EST")
last.dt <- as.POSIXct("12/01/2011 00:00", format="%m/%d/%Y %H:%M", tz="EST")

#** CREATE LIST OF HOLIDAYS UP TO 2011 **#
holidays_2005 <- c("12/26/2005", "11/24/2005", "11/11/2005", "10/10/2005", "09/05/2005", "07/04/2005", "05/30/2005", "02/21/2005", "01/17/2005", "12/31/2004")
holidays_2006 <- c("12/25/2006", "11/23/2006", "11/10/2006", "10/09/2006", "09/04/2006", "07/04/2006", "05/29/2006", "02/20/2006", "01/16/2006", "01/02/2006")
holidays_2007 <- c("12/25/2007", "11/22/2007", "11/12/2007", "10/08/2007", "09/03/2007", "07/04/2007", "05/28/2007", "02/19/2007", "01/15/2007", "01/01/2007")
holidays_2008 <- c("12/25/2008", "11/27/2008", "11/11/2008", "10/13/2008", "09/01/2008", "07/04/2008", "05/26/2008", "02/18/2008", "01/21/2008", "01/01/2008")
holidays_2009 <- c("12/25/2009", "11/26/2009", "11/11/2009", "10/12/2009", "09/07/2009", "07/03/2009", "05/25/2009", "02/16/2009", "01/19/2009", "01/01/2009")
holidays_2010 <- c("12/24/2010", "11/25/2010", "11/11/2010", "10/11/2010", "09/06/2010", "07/05/2010", "05/31/2010", "02/15/2010", "01/18/2010", "01/01/2010")
holidays_2011 <- c("12/26/2011", "11/24/2011", "11/11/2011", "10/10/2011", "09/05/2011", "07/04/2011", "05/30/2011", "02/21/2011", "01/17/2011", "12/31/2010")
holidays <- c(holidays_2005, holidays_2006, holidays_2007, holidays_2008, holidays_2009, holidays_2010, holidays_2011)
holidays <- as.Date(holidays, format="%m/%d/%Y")
holidays <- as.character(holidays)

### DEFINE DATES & HORIZON ###

#** TEST DATES **#
test.month.len <- 13

test.stop.dt <- last.dt 
test.start.dt <- subMonths(addHours(test.stop.dt, 1), test.month.len)
test.dt <- test.start.dt
last.test.dt <- test.dt
cat(paste0("Test Start Dt: ", test.start.dt), sep="\n")
cat(paste0("Test Stop Dt: ", test.stop.dt), sep="\n")
cat("\n")

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


