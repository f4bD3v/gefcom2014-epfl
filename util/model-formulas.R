### TEMPERATURE MODELS ###

#** GAM TEMP MODELS **#
# mean - is direct average over yearly lags, no model involved
gam.temp.models <-list(#"s(LAGM)",
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

lm.temp.models <- list("DLAG + WLAG52 + TOY + HOUR")

nnet.temp.models <- list("DLAG + WLAG52 + TOY + HOUR")

randomforest.temp.models <- list("DLAG + WLAG52 + TOY + HOUR")

#temp.methods.formulas <- list(GAM=gam.temp.formulas, LM=lm.temp.formulas, NN=gam

### LOAD MODELS ###

### GAM LOAD MODELS ###
lm.load.models <- list("CTEMP + DAYT + HOUR + TOY + WLAG52 + DLAG")

gam.load.models <- list("s(CTEMP, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
					"s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24)",
                   	"s(CTEMP, HOUR, k=24) + DAYT + s(HOUR, by=DAYT, k=24) + s(TOY, k=52) + s(DLAG, TOY, k=52) + s(WLAG52, k=24) + s(MTL7D, k=24) + s(MAXT24H, k=52) + s(MINT24H, k=52) + s(TM24H, k=52) + s(TM48H, k=52) + s(TM2H) + s(TM1H)")

nnet.load.models <- list("CTEMP + DAYT + HOUR + TOY + WLAG52 + DLAG")

randomforest.load.models <- list("CTEMP + DAYT + HOUR + TOY + WLAG52 + DLAG")
