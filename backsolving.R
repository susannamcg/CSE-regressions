
 sheet = read.csv("model predictions.csv")
 sheet$eapred = as.numeric(as.character(sheet$eapred))
 sheet$ETHAN.ALLEN = as.numeric(as.character(sheet$ETHAN.ALLEN))
 sheet$adpred = as.numeric(as.character(sheet$adpred))
 sheet$ADIRONDACK = as.numeric(as.character(sheet$ADIRONDACK))
 sheet$espred = as.numeric(as.character(sheet$espred))
 sheet$ES17 = as.numeric(as.character(sheet$ES17))
 sheet$slpred = as.numeric(as.character(sheet$slpred))
 sheet$SURFLINER = as.numeric(as.character(sheet$SURFLINER))
 sheet$EW17.12pred = as.numeric(as.character(sheet$EW17.12pred))
 sheet$EW17.12 = as.numeric(as.character(sheet$EW17.12))
 sheet$EW18.02pred = as.numeric(as.character(sheet$EW18.02pred))
 sheet$EW18.02 = as.numeric(as.character(sheet$EW18.02))
 sheet$ES17.12pred = as.numeric(as.character(sheet$ES17.12pred))
 sheet$ES17.12 = as.numeric(as.character(sheet$ES17.12))
 sheet$ES18.01pred = as.numeric(as.character(sheet$ES18.01pred))
 sheet$ES18.01 = as.numeric(as.character(sheet$ES18.01))
 
 rCSIsl = 0.6951
 rmatchsl = 0.679952
 
 rCSIUea = 0.6576
 rmatchea = 0.6621774
 
 rCSIad = 0.7041
 rmatchad = 0.6519285
 
 rCSIes = 0.6753
 rmatches = 0.6645365
 
 
 eafit = lm(eapred ~ ETHAN.ALLEN, data = sheet)
 esfit = lm(espred ~ ES17, data = sheet)
 adfit = lm(adpred ~ ADIRONDACK, data = sheet)
 slfit = lm(slpred ~ SURFLINER, data = sheet)
 EW17.12fit = lm(EW17.12pred ~ EW17.12, data = sheet)
 EW18.02fit = lm(EW18.02pred ~ EW18.02, data = sheet)
 ES17.12fit = lm(ES17.12pred ~ ES17.12, data = sheet)
 ES18.01fit = lm(ES18.01pred ~ ES18.01, data = sheet)
 
 perf = function(x) {x}
 par(mfrow = c(1,1))
 
 plot(eapred ~ ETHAN.ALLEN, data = sheet, xlim = c(0,100), ylim = c(0,100), 
      xlab = "satisfaction data", ylab = "satisfaction model prediction", main = "Ethan Allen") 
 coef <- coefficients(eafit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)
 
 plot(espred ~ ES17, data = sheet, xlim = c(0,100), ylim = c(0,100),
      xlab = "satisfaction data", ylab = "satisfaction model prediction",main = "Empire South 17")
 coef <- coefficients(esfit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)
 
 plot(adpred ~ ADIRONDACK, data = sheet, xlim = c(0,100), ylim = c(0,100),
      xlab = "satisfaction data", ylab = "satisfaction model prediction",main = "Adirondack")
 coef <- coefficients(adfit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)
 
 plot(slpred ~ SURFLINER, data = sheet, xlim = c(0,100), ylim = c(0,100),
      xlab = "satisfaction data", ylab = "satisfaction model prediction",main = "Surfliner")
 coef <- coefficients(slfit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)
 
 plot( EW17.12pred ~  EW17.12, data = sheet, xlim = c(0,100), ylim = c(0,100),
       xlab = "satisfaction data", ylab = "satisfaction model prediction",main = " EW17.12")
 coef <- coefficients(EW17.12fit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)
 
 plot( EW18.02pred ~  EW18.02, data = sheet, xlim = c(0,100), ylim = c(0,100),
       xlab = "satisfaction data", ylab = "satisfaction model prediction",main = " EW18.02")
 coef <- coefficients(EW18.02fit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)
 
 plot( ES17.12pred ~  ES17.12, data = sheet, xlim = c(0,100), ylim = c(0,100),
       xlab = "satisfaction data", ylab = "satisfaction model prediction",main = "Empire South December 2017")
 coef <- coefficients(ES17.12fit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)
 
 plot( ES18.01pred ~  ES18.01, data = sheet, xlim = c(0,100), ylim = c(0,100),
       xlab = "satisfaction data", ylab = "satisfaction model prediction",main = "Empire South January 2018")
 coef <- coefficients(ES18.01fit)
 fcqfit <- function(x) {coef[1] + x * coef[2]}
 plot(fcqfit, xlim = c(0,100), ylim = c(0,100), add = TRUE)
 plot(perf, xlim = c(0,100), ylim = c(0,100), col = "red", add = TRUE)