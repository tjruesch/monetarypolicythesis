rm(list = ls())
source('preamble_BT.R')

##### Raw Data Import #####

link = 'raw_data/Data FAVAR.xlsx'

exratesM <- zoo(
                ts(read_excel(link, sheet = "Exchange Rates - M", 
                              col_types = 'numeric', na = "NA"), 
                   start = c(1990, 1), frequency = 12), 
                frequency = 12)


surveysM <- zoo(
                ts(read_excel(link, sheet = "Surveys - M", na = "NA"), 
                   start = c(1990, 1), frequency = 12), 
                frequency = 12)


finmarketsM <- zoo(
                   ts(read_excel(link, sheet = "Financial Markets - M", 
                                 col_types = 'numeric', na = "NA"), 
                      start = c(1990, 1), frequency = 12), 
                   frequency = 12)

Rt <- zoo(
          ts(read.csv("raw_data/ecb.csv")[,1], start = c(1999,1), end = c(2019,9), 
             frequency = 12), 
          frequency = 12)

finmarketsM <- cbind(finmarketsM[,1:30],Rt)


pricesM <- zoo(
               ts(read_excel(link, sheet = "Prices - M", col_types = 'numeric', 
                             na = "NA"), 
                  start = c(1990, 1), frequency = 12), 
               frequency = 12)

laborM <- zoo(
              ts(read_excel(link, sheet = "(Un)employment - M", col_types = 'numeric',
                            na = "NA"), 
                 start = c(1990, 1), frequency = 12), 
              frequency = 12)


productionM <- zoo(
                   ts(read_excel(link, sheet = "Production - M", col_types = 'numeric',
                                 na = "NA"), 
                      start = c(1991, 1), frequency = 12), 
                   frequency = 12)


exratesQ <- zoo(
                ts(read_excel(link, sheet = "Exchange Rates - Q", 
                              col_types = 'numeric', na = "NA"), 
                   start = c(1990,1), frequency = 4), 
                frequency = 4)


inconsQ <- zoo(
               ts(read_excel(link, sheet = "Income-Consumption - Q", col_types = 'numeric', 
                             na = "NA"), 
                  start = c(1990,1), frequency = 4), 
               frequency = 4)
inconsQ <- cbind(inconsQ, exratesQ)
rm(exratesQ)

##### Create Spreads #####

attach(as.data.frame(finmarketsM))
EURIBOR3M1W <- BDSU0316R - BDSU0307R
EURIBOR6M3M <- BDS0325G - BDSU0316R
EURIBOR6M1W <- BDS0325G - BDSU0307R

FEDSEC2Y1Y <- BDT3401 - BDT3400
FEDSEC8Y5Y <- BDT9553 - BDT9552
FEDSEC30Y8Y <- BDT9555 - BDT9553
FEDSEC30Y1Y <- BDT9555 - BDT3400

PUBDEB4Y1Y <- BDT0914 - BDT0912
PUBDEB7Y1Y <- BDT0918 - BDT0912
PUBDEB7Y4Y <- BDT0918 - BDT0914
detach(as.data.frame(finmarketsM))

spreadsM <- data.frame(EURIBOR3M1W, EURIBOR6M3M, EURIBOR6M1W, 
                       FEDSEC2Y1Y, FEDSEC8Y5Y, FEDSEC30Y8Y, FEDSEC30Y1Y, 
                       PUBDEB4Y1Y, PUBDEB7Y1Y, PUBDEB7Y4Y)
spreadsM <- zoo(ts(spreadsM, start = c(1990,1), frequency = 12), frequency = 12)

rm(list = c('EURIBOR3M1W', 'EURIBOR6M3M', 'EURIBOR6M1W', 
            'FEDSEC2Y1Y', 'FEDSEC8Y5Y', 'FEDSEC30Y8Y', 'FEDSEC30Y1Y', 
            'PUBDEB4Y1Y', 'PUBDEB7Y1Y', 'PUBDEB7Y4Y')
   )


##### Drop Series with too Many NA #####

laborM <- laborM[,-c(1,13:17,19:21)]
pricesM <- pricesM[,-c(10,13:15,18)]
surveysM <- surveysM[,-c(3:5)]
inconsQ <- inconsQ[,-c(2,43)]

##### Seasonal Adjustment #####

# Exrates
seasonal(exratesM)

# Fin Markets
seasonal(finmarketsM)

finmarketsM[,3] <- zoo(ts(c(series(seas(as.ts(finmarketsM[,3])), 
                                   series = "s11"), NA, NA), 
                          start = start(finmarketsM), frequency = 12), 
                       frequency = 12)

finmarketsM[,5] <- zoo(ts(c(series(seas(as.ts(finmarketsM[,5])), 
                                   series = "s11"), NA, NA), 
                          start = start(finmarketsM), frequency = 12), 
                       frequency = 12)
finmarketsM[,7] <- zoo(ts(c(series(seas(as.ts(finmarketsM[,5])), 
                                   series = "s11"), NA, NA), 
                          start = start(finmarketsM), frequency = 12), 
                       frequency = 12)
finmarketsM[,8] <- zoo(ts(c(series(seas(as.ts(finmarketsM[,8])), 
                                   series = "s11"), NA, NA), 
                          start = start(finmarketsM), frequency = 12), 
                       frequency = 12)
finmarketsM[,9] <- zoo(ts(c(series(seas(as.ts(finmarketsM[,9])), 
                                   series = "s11"), NA, NA), 
                          start = start(finmarketsM), frequency = 12), 
                       frequency = 12)

#which(is.na(finmarketsM[,4])==T)
#finmarketsM[,4] <- zoo(ts(c(series(seas(as.ts(finmarketsM[,4])), series = "s11"), NA, NA), start = start(finmarketsM), frequency = 12), frequency = 12)
# Problems with series 4

# Income and Consumption
which(seasonal(inconsQ))

for (i in 2:29) {
  inconsQ[,i] <- zoo(ts(c(rep(NA,4), series(seas(as.ts(inconsQ[,i])), series = "s11"), 
                          NA, NA), start = start(inconsQ), frequency = 4), 
                     frequency = 4)
}

inconsQ[,41] <- zoo(ts(c(rep(NA,4),series(seas(as.ts(inconsQ[,41])), series = "s11"),
                         NA, NA,NA), start = start(inconsQ), frequency = 4), 
                    frequency = 4)
inconsQ[,43] <- zoo(ts(c(rep(NA,4),series(seas(as.ts(inconsQ[,43])), series = "s11"),
                         NA, NA), start = start(inconsQ), frequency = 4), 
                    frequency = 4)

# Labor
which(seasonal(laborM))

for (i in c(1,4,7)) {
  laborM[,i] <- zoo(ts(c(series(seas(as.ts(laborM[,i])), series = "s11"),
                         NA), start = start(laborM), frequency = 12), 
                    frequency = 12)
}

laborM[,2] <- zoo(ts(c(rep(NA,8), series(seas(as.ts(laborM[,2])), series = "s11"), NA),
                     start = start(laborM), frequency = 12), 
                  frequency = 12)

for (i in c(5,6)) {
  laborM[,i] <- zoo(ts(c(rep(NA,9), series(seas(as.ts(laborM[,i])), series = "s11"), NA),
                       start = start(laborM), frequency = 12), 
                    frequency = 12)
}

laborM[,8] <- zoo(ts(c(rep(NA,12), series(seas(as.ts(laborM[,8])), series = "s11"),
                       NA, NA),
                     start = start(laborM), frequency = 12), 
                  frequency = 12)

for (i in c(9,10)) {
  laborM[,i] <- zoo(ts(c(rep(NA,12), series(seas(as.ts(laborM[,i])), series = "s11"),
                         rep(NA,7)), 
                       start = start(laborM), frequency = 12), 
                    frequency = 12)
}

laborM[,11] <- zoo(ts(c(rep(NA,6), series(seas(as.ts(laborM[,11])), series = "s11"),
                        rep(NA,7)), 
                      start = start(laborM), frequency = 12), 
                   frequency = 12)

laborM[,12] <- zoo(ts(c(series(seas(as.ts(laborM[,12])), series = "s11"),
                        rep(NA,3)),
                      start = start(laborM), frequency = 12), 
                   frequency = 12)

laborM[1:17,3] <- rep(NA,17)
laborM[,3] <- zoo(ts(c(rep(NA,17), series(seas(as.ts(laborM[,3])), series = "s11"), NA),
                     start = start(laborM), frequency = 12), 
                  frequency = 12)

# Prices
which(seasonal(pricesM))

c1 <- which(seasonal(pricesM))[c(1,2)]
c2 <- which(seasonal(pricesM))[5:11]
c3 <- which(seasonal(pricesM))[c(12,14)]

for (i in c1) {
  pricesM[,i] <- zoo(ts(c(series(seas(as.ts(pricesM[,i])), series = "s11"), NA), 
                        start = start(pricesM), frequency = 12), 
                     frequency = 12)
}

for (i in c2) {
  pricesM[,i] <- zoo(ts(c(rep(NA,12), series(seas(as.ts(pricesM[,i])), series = "s11"),
                          NA), 
                        start = start(pricesM), frequency = 12), 
                     frequency = 12)
}

for (i in c3) {
  pricesM[,i] <- zoo(ts(c(series(seas(as.ts(pricesM[,i])), series = "s11"),NA,NA), 
                        start = start(pricesM), frequency = 12), 
                     frequency = 12)
}

pricesM[,4] <- zoo(ts(c(rep(NA,72), series(seas(as.ts(pricesM[,4])), series = "s11"),
                        NA), 
                      start = start(pricesM), frequency = 12), 
                   frequency = 12)
pricesM[,5] <- zoo(ts(c(rep(NA,73), series(seas(as.ts(pricesM[,5])), series = "s11"),
                        NA), start = start(pricesM), frequency = 12),
                   frequency = 12)
pricesM[,29] <- zoo(ts(c(rep(NA,60), series(seas(as.ts(pricesM[,29])), series = "s11"),
                         NA, NA), 
                       start = start(pricesM), frequency = 12), 
                    frequency = 12)

# Production
seasonal(productionM)

# Spreads
seasonal(spreadsM)

# Surveys
seasonal(surveysM)

###### Stationarity #####

# Exrates, all 5

library(dygraphs)

exratesM <- window(as.ts(exratesM), start = c(1998,12))
exratesM_adj <- window(as.ts(exratesM), start = c(1999,1))

for (series in c(1,3,5,9,10)) {
  if (unitroot(exratesM[,series], level = .1) == 'stationary') {
    exratesM_adj[,series] <- exratesM[-1,series]
  } else {
    exratesM_adj[,series] <- diff(log(exratesM[,series]))
  }
}
for (series in c(2,4,6,7,8)) {
  if (unitroot(exratesM[,series], level = .1) == 'stationary') {
    exratesM_adj[,series] <- exratesM[-1,series]
  } else {
    ts <- exratesM[-1,series]
    exratesM_adj[,series] <- c(NA,diff(log(ts)))
  }
}

dygraph(standardize(na.omit(exratesM_adj[,-6])),
        main = 'Standardized and Adjusted Exchange Rate Data') %>%
  dyLegend(show = 'onmouseover') %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))


# Fin Markets
# interest rates 2
# money and credit stuff 5
# stock prices 5

finmarketsM <- window(as.ts(finmarketsM), start = c(1998,12))
finmarketsM_adj <- window(as.ts(finmarketsM), start = c(1999,1))

# interest_rates
unitroot(finmarketsM[,10:26], pvalues = TRUE)
finmarketsM_adj[,10:26] <- finmarketsM[-1,10:26]

dygraph(standardize(na.omit(finmarketsM[,10:26])),
        main = 'Standardized and Adjusted Interest Rates') %>%
  dyLegend(show = 'onmouseover') %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

# money
unitroot(finmarketsM[,1:9], pvalues = TRUE)
finmarketsM_adj[,1:9] <- diff(log(finmarketsM[,1:9]))

dygraph(standardize(na.omit(finmarketsM_adj[,1:9])),
        main = 'Standardized and Adjusted Money Data') %>%
  dyLegend(show = 'onmouseover') %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

# stock market
unitroot(finmarketsM[,27:30], pvalues = TRUE)
finmarketsM_adj[,27:30] <- diff(log(finmarketsM[,27:30]))

dygraph(standardize(na.omit(finmarketsM_adj[,27:30])),
        main = 'Standardized and Adjusted Stock Market Returns') %>%
  dyLegend(show = 'onmouseover') %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))

# Rt
unitroot(finmarketsM[,31], pvalues = T)
finmarketsM_adj[,31] <- finmarketsM[-1,31]


dygraph(standardize(na.omit(finmarketsM_adj)),
        main = 'Standardized and Adjusted Stock Market Returns') %>%
  dyLegend(show = 'onmouseover') %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))


# Income/Consumption
# all 5 exept 21 23 26 40 41

inconsQ <- window(as.ts(inconsQ), start = c(1998,4))
inconsQ_adj <- window(as.ts(inconsQ), start = c(1999,1))

View(inconsQ)


for (i in 1:39) {
  ts <- inconsQ[-c(85,86),i]
  if (min(ts) < 0) {
    ts <- ts + abs(min(ts)) + 1
  }
  inconsQ_adj[,i] <- c(diff(log(ts)), NA, NA)
}
inconsQ_adj[,40] <- inconsQ[-1,40]

for (i in 41:43) {
  ts <- inconsQ[-c(84,85,86),i]
  if (min(ts) < 0) {
    ts <- ts + abs(min(ts)) + 1
  }
  inconsQ_adj[,i] <- c(diff(log(ts)), NA, NA, NA)
}


dygraph(standardize(na.omit(inconsQ_adj)),
        main = 'Standardized and Adjusted Income and Expenditure Data') %>%
  dyLegend(show = 'follow') %>%
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>%
  dyAxis('y', label = 'Growth Rates*')


# Labor, all 5
laborM <- laborM[,c(1:8,12,9:11)]

######
#HERE#
######


unitroot(laborM, pvalues = T)

View(laborM)

unitroot(laborM[,'EGEMPSTWP'])
unitroot(laborM[,'WGEMPSTWP'])
plot(laborM[,'BDEMPSTWP'])

colnames(laborM)[8]

#' 1 changes
#' 2 changes



laborM_adj <- diff(log(laborM))

# Prices
# all 5 except 2 3 5 6 7 24
zero = rep(NA,dim(pricesM)[2])
for (i in 1:dim(pricesM)[2]){
  zero[i] <- ifelse(min(na.remove(pricesM[,i]))<0,1,0)
}
dif <- c(2,3,5,6,7,24)

pricesM_adj_diff <- diff(pricesM[,dif])
pricesM_adj_log <- diff(log(pricesM[,-dif]))
pricesM_adj <- cbind(pricesM_adj_diff,pricesM_adj_log)

# Production, all 5
productionM_adj <- diff(log(productionM))

# Spreads, all 2
spreadsM_adj <- diff(spreadsM)

# Surveys, all 1
surveysM_adj <- surveysM

# Saving ####

M1 <- ts(read.csv("C:/Users/tomjo/Dropbox/Uni/Semester 6/Bachelor Thesis/data/m1.csv", header=TRUE)[,3], start = c(1981,1), freq = 12)
APP <- ts(read_excel("C:/Users/tomjo/Dropbox/Uni/Semester 6/Bachelor Thesis/data/APP.xlsx", sheet = "Sheet1")$Sum, start = c(2014,10), freq = 12, end = c(2019,9))

save(list = c("exratesM_adj", "finmarketsM_adj", "inconsQ_adj", "laborM_adj", "pricesM_adj", "productionM_adj", "spreadsM_adj", "surveysM_adj", "M1", "APP"), file="data.Rdata")
