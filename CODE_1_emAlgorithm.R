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

incon <- ts(matrix(NA, 249, 43), start = c(1999, 1), freq = 12)
for (t in 1:83) incon[3 * t, ] <- inconq[t, ]

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
slow <- c(rep(1,33), rep(1,7), rep(0,10), rep(1,12), rep(0,10),
          rep(0,21), rep(0,30), rep(1,43))
Xslow <- Data[, which(slow == 1)]
X <- Data

# Imputation Slow ####

#nb1 <- estim_PC(Xslow, ncp.min = 50, ncp.max = 55, method="Regularized",
#                method.cv = "Kfold", nbsim = 2, pNA=.01)
nb1 <- 30
XslowPCA <- imputePCA(Xslow, ncp = nb1, method = "Regularized",
                      maxiter = 1e10)$fittedX

slowmonthly <- 1:52
slowquarterly <- 53:95
Xslowimp <- Xslow

# monthly observations

for (i in slowmonthly) {
  for (t in 1:249) {
    if (is.na(Xslow[t, i]) == TRUE) Xslowimp[t, i] <- XslowPCA[t, i]
  }
}

# quarterly observations

Xslowimp[249, 95] <- XslowPCA[249, 95]
Xslow[249, 95] <- Xslowimp[249, 95]

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

# Imputation Fast (Full Data Set) ####

#nb2 <- estim_ncpPCA(X, ncp.min = 0, ncp.max = 30, method = "EM",
#                    method.cv = "Kfold", nbsim = 50, pNA = 0.01)

nb2 <- 28

s <- which(slow == 1)
for (i in length(s)) {
  X[, s[i]] <- Xslowimp[, i]
}

monthly <- 1:167
Ximp <- cbind(X, Rt)
X <- cbind(X, Rt)

XPCA <- imputePCA(X, ncp = nb2, method = "Regularized", maxiter = 1e10)$fittedX

# monthly observations

for (i in monthly) {
  for (t in 1:249) {
    if (is.na(X[t, i]) == TRUE) Ximp[t, i] <- XPCA[t, i]
  }
}

Rt <- Ximp[, 167]
X <- Ximp[, -167]
sum(is.na(X)) # <- must be 0

# Graph Imputation

highchart(type = "chart") %>%
  hc_title(text = paste("Imputed Quarterly", "BDIANIATA", "Data")) %>%
  hc_xAxis(type = "datetime") %>%
  hc_add_series(incon[, 40], name = "Original Observations", type = "scatter") %>%
  hc_add_series(X[, 163], name = "Imputed Series") %>%
  hc_exporting(enabled = TRUE)

highchart(type = "chart") %>%
  hc_title(text = paste("Imputed Quarterly", colnames(X)[152], "Data")) %>%
  hc_xAxis(type = "datetime") %>%
  hc_add_series(incon[, 29], name = "Original Observations", type = "scatter") %>%
  hc_add_series(X[, 152], name = "Imputed Series") %>%
  hc_exporting(enabled = TRUE)

# Standardization ####

for (i in 1:dim(X)[2]) {
  X[, i] <- (X[, i] - mean(X[, i])) / sd(X[, i])
}
X <- ts(X, start = c(1999,1), end = c(2019, 9), freq = 12)
Xslow <- X[, which(slow == 1)]
XR <- cbind(X, Rt)
colnames(XR) <- c(varnames, "Rt")

# Saving ####
# write.csv(XR, file = "adjdata_alternative.csv")
save(list = c("XR", "slow", "APP", "M1"), file = "A_adjdata.Rdata")
