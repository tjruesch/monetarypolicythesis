rm(list = ls())
source("preamble_BT.R")
load("A_data.Rdata")

# Correct time Span ####
fin <- window(as.ts(finmarketsM_adj),
              start = c(1999, 1), end = c(2019, 9), freq = 12)
labor <- window(as.ts(laborM_adj),
                start = c(1999, 1), end = c(2019, 9), freq = 12)
prices <- window(as.ts(pricesM_adj),
                 start = c(1999, 1), end = c(2019, 9), freq = 12)
production <- window(as.ts(productionM_adj),
                     start = c(1999, 1), end = c(2019, 9), freq = 12)
spreads <- window(as.ts(spreadsM_adj),
                  start = c(1999, 1), end = c(2019, 9), freq = 12)
surveys <- window(as.ts(surveysM_adj),
                  start = c(1999, 1), end = c(2019, 9), freq = 12)
exrates <- window(as.ts(exratesM_adj),
                  start = c(1999, 1), end = c(2019, 9), freq = 12)
inconq <- window(as.ts(inconsQ_adj), start = c(1999, 1),
                 end = c(2019, 3), freq = 4)

incon <- ts(matrix(NA, 249, 43), start = c(1999, 1), frequency = 12)
for (t in 1:83) incon[3 * t, ] <- inconq[t, ]
colnames(incon) <- colnames(inconq)

colnames(fin)[31] <- "Rt"

# Slow and Fast moving ####
Rt <- fin[, 31]
fin <- fin[, c(1:13, 15:30, 14)]
labor <- labor[, c(1:8, 12, 9:11)]
spreads <- spreads[, c(1, 4:10, 2, 3)]
surveys <- surveys[, c(1, 4:7, 9:17, 2, 3, 8, 18:21)]

Data <- cbind(prices, production, exrates, labor, spreads, surveys, fin, incon)
colnames(Data) <- c(colnames(prices), colnames(production), colnames(exrates),
                    colnames(labor), colnames(spreads), colnames(surveys),
                    colnames(fin), colnames(inconq))
varnames <- colnames(Data)

slow <- c(rep(1,ncol(prices)), rep(1,ncol(production)), rep(0,ncol(exrates)), rep(1,ncol(labor)), rep(0,ncol(spreads)),
          rep(0,ncol(surveys)), rep(0,ncol(fin)), rep(1,ncol(incon)))
Xslow <- Data[, which(slow == 1)]
X <- Data

rm(list = c("exrates", "exratesM_adj", "fin", "finmarketsM_adj", "inconq", "inconsQ_adj", 'labor', "laborM_adj",
            "packages", "prices", "pricesM_adj", "production", "productionM_adj", "surveys", "surveysM_adj", "spreads",
            "spreadsM_adj", "t"))

# Imputation Slow ####

# nb1 <- my_estim_ncpPCA(Xslow, ncp.min = 20, ncp.max = 55, method="Regularized",
#                        method.cv = "Kfold", nbsim = 100, pNA=.05, maxiter = 1e6)
nb1 <- 14

XslowPCA <- imputePCA(Xslow, ncp = nb1, method = "Regularized",
                      maxiter = 1e10)$fittedX

slowmonthly <- 1:which(colnames(Xslow) == "BDGDP...D")
slowquarterly <- (which(colnames(Xslow) == "BDGDP...D")+1):ncol(Xslow)
Xslowimp <- Xslow

# monthly observations

for (i in slowmonthly) {
  for (t in 1:249) {
    if (is.na(Xslow[t, i]) == TRUE) Xslowimp[t, i] <- XslowPCA[t, i]
  }
}

# quarterly observations

for (i in (ncol(Xslow)-3):ncol(Xslow)) {
  Xslowimp[249, i] <- XslowPCA[249, i]
  Xslow[249, i] <- Xslowimp[249, i]
}

for (i in slowquarterly) {
  for (t in 1:249) {
    if (is.na(Xslowimp[t, i]) == TRUE) {
      if (t %% 3 == 0) {
        tau <- t
      }else{
        if (t %% 2 == 0 & t %% 6 != 4) tau <- t + 1
        if (t %% 2 == 0 & t %% 6 == 4) tau <- t + 2
        if (t %% 2 != 0 & t %% 6 != 5) tau <- t + 2
        if (t %% 2 != 0 & t %% 6 == 5) tau <-  t + 1
      }
      Xslowimp[t, i] <- XslowPCA[t, i] + Xslow[tau, i] -
                       (1 / 3) * (XslowPCA[(tau - 2), i] +
                       XslowPCA[(tau - 1), i] + XslowPCA[tau, i])
    }
  }
}

# sum(is.na(Xslowimp)) == 0     # Check completeness of imputation

# Imputation Fast (Full Data Set) ####

#nb2 <- estim_ncpPCA(X, ncp.min = 0, ncp.max = 30, method = "EM",
#                    method.cv = "Kfold", nbsim = 50, pNA = 0.01)

nb2 <- 14

# replace
for (name in colnames(Xslowimp)) {
  X[,name] <- Xslowimp[,name]
}

Ximp <- cbind(X, Rt); X <- cbind(X, Rt)
XPCA <- imputePCA(X, ncp = nb2, method = "Regularized", maxiter = 1e10)$fittedX

# monthly observations

for (i in 1:ncol(X)) {
  for (t in 1:249) {
    if (is.na(X[t, i]) == TRUE) Ximp[t, i] <- XPCA[t, i]
  }
}

Rt <- Ximp[,ncol(Ximp)]
X <- Ximp[,-ncol(Ximp)]

# sum(is.na(X)) == 0      # Check completeness of imputation

# Graph Imputation
colnames(X) <- varnames

highchart(type = "chart") %>%
  hc_title(text = paste("Imputed Quarterly", "BDGGOVBLA", "Data")) %>%
  hc_xAxis(type = "datetime") %>%
  hc_add_series(incon[,"BDGGOVBLA"], name = "Original Observations", type = "scatter") %>%
  hc_add_series(X[,'BDGGOVBLA'], name = "Imputed Series") %>%
  hc_exporting(enabled = TRUE)


highchart(type = "chart") %>%
  hc_title(text = paste("Imputed Quarterly", "BDPERSAVE", "Data")) %>%
  hc_xAxis(type = "datetime") %>%
  hc_add_series(incon[,"BDPERSAVE"], name = "Original Observations", type = "scatter") %>%
  hc_add_series(X[,"BDPERSAVE"], name = "Imputed Series") %>%
  hc_exporting(enabled = TRUE)

##### Standardization #####

for (i in 1:dim(X)[2]) {
  X[, i] <- (X[, i] - mean(X[, i])) / sd(X[, i])
}
X <- ts(X, start = c(1999,1), end = c(2019, 9), frequency = 12)
Xslow <- X[, which(slow == 1)]
XR <- cbind(X, Rt)
colnames(XR) <- c(varnames, "Rt")

# Saving ####
# write.csv(XR, file = "adjdata_alternative.csv")
save(list = c("XR", "slow", "APP", "M1"), file = "B_adjdata.Rdata")
