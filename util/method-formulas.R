### TEMPERATURE formulas ###

LM.temp.formulas <- list("DLAG + WLAG52 + TOY + HOUR",
                       "WLAG52 + TOY + HOUR",
                       "DLAG + WLAG52 + HOUR",
                       "WLAG52")

# mean - is direct average over yearly lags, no model involved
GAM.temp.formulas <-list(#"s(LAGM)",
                      #"s(LAGM) + s(LAGMD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(LAGMD) + s(LAGMAX) + s(LAGMIN) + s(LAGSD)",
                      #"s(LAGM) + s(WLAG52, by=TOY, k=24) + s(HOUR, k=24)",
                      #"s(LAGM) + s(LAGMD) + s(DLAG, TOY, k=52)",
                      "s(DLAG, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)", # without DLAG
                      "s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)",
                      "s(DLAG, by=TOY, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)", # no need 4
                      "s(DLAG, by=MONTH, k=24) + s(WLAG52, k=24) + s(TOY, k=52) + s(HOUR, k=24)") # no need 5

NN.temp.formulas <- list("DLAG + WLAG52 + TOY + HOUR",
                       "WLAG52 + TOY + HOUR",
                       "DLAG + WLAG52 + HOUR")

RF.temp.formulas <- list("DLAG + WLAG52 + TOY + HOUR",
                       "WLAG52 + TOY + HOUR",
                       "DLAG + WLAG52 + HOUR")

temp.methods.formulas <- list("GAM"=GAM.temp.formulas, "GAM_PCA"=GAM.temp.formulas, "GAM_Station1"=GAM.temp.formulas, "LM"=LM.temp.formulas, "NN"=NN.temp.formulas, "RF"=RF.temp.formulas)
#names(temp.methods.formulas) <- c("GAM", "LM", "NN", "RF")

### LOAD formulas ###

### GAM LOAD formulas ###
LM.load.formulas <- list("CTEMP + DAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + DAYT + HOUR + TOY + WLAG52",
						"DAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + SDAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + WDAYT + HOUR + TOY + WLAG52 + DLAG")

GAM.load.formulas <- list("s(CTEMP, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)", # temp
                   	"DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)", # no temp
					"s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(WLAG52, k=24)", # temp by hour without DLAG
					"s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)", # temp by hour 
					"s(CTEMP, HOUR, k=24) + SDAYT + s(HOUR, by=SDAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)", # temp by hour - different daytype
					"s(CTEMP, HOUR, k=24) + WDAYT + s(HOUR, by=WDAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)", # temp by hour - different daytype
                   	"s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(MTL7D, k=24) + s(MAXT24H, k=52) + s(MINT24H, k=52) + s(TM24H, k=52) + s(TM48H, k=52) + s(TM2H) + s(TM1H)",
                   	"s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(WLAG52, k=24) + s(MTL7D, k=24) + s(MAXT24H, k=52) + s(MINT24H, k=52) + s(TM24H, k=52) + s(TM48H, k=52) + s(TM2H) + s(TM1H)")
                   	# during weekends and certain hours temperature more likely to affect demand: s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)", # temp by hour 

NN.load.formulas <- list("CTEMP + DAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + DAYT + HOUR + TOY + WLAG52",
						"DAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + SDAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + WDAYT + HOUR + TOY + WLAG52 + DLAG")

RF.load.formulas <- list("CTEMP + DAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + DAYT + HOUR + TOY + WLAG52",
						"DAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + SDAYT + HOUR + TOY + WLAG52 + DLAG",
                        "CTEMP + WDAYT + HOUR + TOY + WLAG52 + DLAG")

load.methods.formulas <- list(GAM=GAM.load.formulas, LM=LM.load.formulas, NN=NN.load.formulas, RF=RF.load.formulas)
names(load.methods.formulas) <- c("GAM", "LM", "NN", "RF")

load.method.names <- list(GAM="General Additive Model",
						LM="Standard Linear Model",
						NN="Simple Neural Network (1 Hidden Layer)",
						RF="Random Forest")
