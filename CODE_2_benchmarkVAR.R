rm(list = ls())
source("preamble_BT.R")
load("B_data.Rdata")

# Default instrument: Rt ####

Rt <- window(as.ts(na.remove(finmarketsM_adj[, 31])), start = c(1999, 6),
             end = c(2019, 3), freq = 12)
Rt <- na.remove(diff(Rt))

IP <- window(as.ts(productionM_adj[, "IPGENERAL"]), start = c(1999, 7),
             end = c(2019, 3), freq = 12)
CPI <- window(as.ts(pricesM_adj[,"BDUUFA01F"]), start = c(1999, 7),
              end = c(2019, 3), freq = 12)

Rt <- (Rt - mean(Rt)) / sd(Rt)
IP <- (IP - mean(IP)) / sd(IP)
CPI <- (CPI - mean(CPI)) / sd(CPI)

Yt <- cbind(IP, CPI, Rt)

var.baseline <- VAR(Yt, lag.max = 30, ic = "AIC", type = "const") # change p to change the lag length
var.baseline$p

amat <- matrix(c(NA, 0, 0, NA, NA, 0, NA, NA, 1), 3, 3, byrow = T)
svar.baseline <- SVAR(var.baseline, estmethod = "direct", Amat = amat)
irf.baseline <- irf(svar.baseline, impulse = "Rt", n.ahead = 48,
                    ortho = F, runs = 50)
irfc.baseline <- irf(svar.baseline, impulse = "Rt", n.ahead = 48,
                     ortho = F, cumulative = T, runs = 50)

# IRF Graphs
months <- 0:48

# IP
lower <- irf.baseline$Lower$Rt[, 1]
upper <- irf.baseline$Upper$Rt[, 1]
irf <- irf.baseline$irf$Rt[, 1]
irf.IP <- data.frame(months, lower, irf, upper)

graph.IP_print <- irf.graph.p(irf.IP, name = "Industrial Production Growth", level = "95%",
                              height = 900, width = 600)
graph.IP <- irf.graph.d(irf.IP, name = "Industrial Production Growth", level = "95%")

# CPI
lower <- irf.baseline$Lower$Rt[, 2]
upper <- irf.baseline$Upper$Rt[, 2]
irf <- irf.baseline$irf$Rt[, 2]
irf.CPI <- data.frame(months, lower, irf, upper)

graph.CPI_print <- irf.graph.p(irf.CPI, name = "Consumer Price Index Growth", level = "95%")
graph.CPI <- irf.graph.d(irf.CPI, name = "Consumer Price Index Growth", level = "95%")

# Rt
lower <- irf.baseline$Lower$Rt[, 3]
upper <- irf.baseline$Upper$Rt[, 3]
irf <- irf.baseline$irf$Rt[, 3]
irf.Rt <- data.frame(months, lower, irf, upper)

graph.Rt_print <- irf.graph.p(irf.Rt, name = "MRO Interest Rate", level = "95%")
graph.Rt <- irf.graph.d(irf.Rt, name = "MRO Interest Rate", level = "95%")

graph.Rt
graph.CPI
graph.IP





# Alternative instrument: M1 ####

M1 <- window(M1, start = start(Yt), end = end(Yt), freq = frequency(Yt))
M1 <- (M1 - mean(M1)) / sd(M1)

Xt <- cbind(Yt[,1:2],M1); colnames(Xt) <- c("IP", "CPI", "M1")

var.baseline2 <- VAR(Xt, type = "none", ic = "AIC", lag.max = 30)
var.baseline2$p

amat <- matrix(c(NA, 0, 0, NA, NA, 0, NA, NA, 1), 3, 3, byrow = T)
svar.baseline2 <- SVAR(var.baseline2, estmethod = "direct", Amat = amat)
irf.baseline2 <- irf(svar.baseline2, impulse = "M1", n.ahead = 48,
                     ortho = F, runs = 500)
irfc.baseline2 <- irf(svar.baseline2, impulse = "M1", n.ahead = 48,
                      ortho = F, cumulative = T, runs = 500)

# IP
months <- 0:48
lower <- -irf.baseline2$Lower$M1[, 1]
upper <- -irf.baseline2$Upper$M1[, 1]
irf <- -irf.baseline2$irf$M1[, 1]

irf.IP2 <- data.frame(months, lower, irf, upper)

graph.IP2_print <- irf.graph.p(irf.IP2, name = "Industrial Production Growth",
                               level = "95%")
graph.IP2 <- irf.graph.d(irf.IP2, name = "Industrial Production", level = "95%")

# CPI
lower <- -irf.baseline2$Lower$M1[, 2]
upper <- -irf.baseline2$Upper$M1[, 2]
irf <- -irf.baseline2$irf$M1[, 2]

irf.CPI2 <- data.frame(months, lower, irf, upper)

graph.CPI2_print <- irf.graph.p(irf.CPI2, name = "Consumer Price Index Growth",
                                level = "95%")
graph.CPI2 <- irf.graph.d(irf.CPI2, name = "Consumer Price Index",
                          level = "95%")

# M1
lower <- -irf.baseline2$Lower$M1[, 3]
upper <- -irf.baseline2$Upper$M1[, 3]
irf <- -irf.baseline2$irf$M1[, 3]

irf.Rt2 <- data.frame(months, lower, irf, upper)

graph.Rt2_print <- irf.graph.p(irf.Rt2, name = "M1 Money Aggregate",
                               level = "95%")
graph.Rt2 <- irf.graph.d(irf.Rt2, name = "M1 Money Aggregate", level = "95%")

graph.Rt2
graph.CPI2
graph.IP2

# Alternative: APP

APP <- (APP - mean(APP)) / sd(APP)

CPI <- window(CPI, start = start(APP), end = end(CPI))
IP <- window(IP, start = start(APP), end = end(CPI))
APP <- window(APP, start = start(APP), end = end(CPI))

Zt <- cbind(IP, CPI, APP)

var.baseline2 <- VAR(Zt, p = 5)
var.baseline2$p

amat <- matrix(c(NA, 0, 0, NA, NA, 0, NA, NA, 1), 3, 3, byrow = T)
svar.baseline2 <- SVAR(var.baseline2, estmethod = "direct", Amat = amat)
irf.baseline2 <- irf(svar.baseline2, impulse = "APP", n.ahead = 48,
                     ortho = F, runs = 500)
irfc.baseline2 <- irf(svar.baseline2, impulse = "APP", n.ahead = 48,
                      ortho = F, cumulative = T, runs = 500)

# IP
months <- 0:48
lower <- -irf.baseline2$Lower$APP[, 1]
upper <- -irf.baseline2$Upper$APP[, 1]
irf <- -irf.baseline2$irf$APP[, 1]

irf.IP3 <- data.frame(months, lower, irf, upper)

graph.IP3_print <- irf.graph.p(irf.IP3, name = "Industrial Production Growth",
                               level = "95%")
graph.IP3 <- irf.graph.d(irf.IP3, name = "Industrial Production", level = "95%")

# CPI
lower <- -irf.baseline2$Lower$APP[, 2]
upper <- -irf.baseline2$Upper$APP[, 2]
irf <- -irf.baseline2$irf$APP[, 2]

irf.CPI3 <- data.frame(months, lower, irf, upper)

graph.CPI3_print <- irf.graph.p(irf.CPI3, name = "Consumer Price Index Growth",
                                level = "95%")
graph.CPI3 <- irf.graph.d(irf.CPI3, name = "Consumer Price Index",
                          level = "95%")

# APP
lower <- -irf.baseline2$Lower$APP[, 3]
upper <- -irf.baseline2$Upper$APP[, 3]
irf <- -irf.baseline2$irf$APP[, 3]

irf.Rt3 <- data.frame(months, lower, irf, upper)

graph.Rt3_print <- irf.graph.p(irf.Rt3, name = "APP", level = "95%")
graph.Rt3 <- irf.graph.d(irf.Rt3, name = "APP", level = "95%")


graph.Rt3
graph.CPI3
graph.IP3

# comparative graphs

R1 <- irf.Rt$irf
R2 <- irf.Rt2$irf
R3 <- irf.Rt3$irf

R_comp <- highchart() %>%
  hc_title(text = "Monetary Policy Tool", style = list(fontSize = "30px")) %>%
  hc_yAxis(plotLines = list(
    list(color = "#000000",
         dashStyle = "DashDot",
         width = 2,
         value = 0)),
    labels = list(style = list(fontSize = "18px"))) %>%
  hc_xAxis(labels=list(style = list(fontSize = "18px"))) %>%
  hc_add_series(R1, color = "black", name = "MRO Interest Rate", marker = F) %>%
  hc_add_series(R2, color = "grey", name = "M1 Monetary Aggregate", marker = F) %>%
  hc_add_series(R3, color = "orange", name = "Asset Purchasing Programme", marker = F) %>%
  hc_legend(enabled = TRUE, itemStyle = list(fontSize = "20px")) %>%
  hc_size(width = 600, height = 600) %>%
  hc_exporting(enabled = TRUE)

# IP

IP1 <- irf.IP$irf
IP2 <- irf.IP2$irf
IP3 <- irf.IP3$irf

IP_comp <- highchart() %>%
  hc_title(text = "Industrial Production Growth", style = list(fontSize = "30px")) %>%
  hc_yAxis(plotLines = list(
    list(color = "#000000",
         dashStyle = "DashDot",
         width = 2,
         value = 0)),
    labels = list(style = list(fontSize = "18px"))) %>%
  hc_xAxis(labels=list(style = list(fontSize = "18px"))) %>%
  hc_add_series(IP1, color = "black", name = "MRO Interest Rate", marker = F) %>%
  hc_add_series(IP2, color = "grey", name = "M1 Monetary Aggregate", marker = F) %>%
  hc_add_series(IP3, color = "orange", name = "Asset Purchasing Programme", marker = F) %>%
  hc_legend(enabled = TRUE, itemStyle = list(fontSize = "20px")) %>%
  hc_size(width = 600, height = 600) %>%
  hc_exporting(enabled = TRUE)

# CPI

CPI1 <- irf.CPI$irf
CPI2 <- irf.CPI2$irf
CPI3 <- irf.CPI3$irf

CPI_comp <- highchart() %>%
  hc_title(text = "Consumer Price Index Growth", style = list(fontSize = "30px")) %>%
  hc_yAxis(plotLines = list(
    list(color = "#000000",
         dashStyle = "DashDot",
         width = 2,
         value = 0)),
    labels = list(style = list(fontSize = "18px"))) %>%
  hc_xAxis(labels=list(style = list(fontSize = "18px"))) %>%
  hc_add_series(CPI1, color = "black", name = "MRO Interest Rate", marker = F) %>%
  hc_add_series(CPI2, color = "grey", name = "M1 Monetary Aggregate", marker = F) %>%
  hc_add_series(CPI3, color = "orange", name = "Asset Purchasing Programme", marker = F) %>%
  hc_legend(enabled = TRUE, itemStyle = list(fontSize = "20px")) %>%
  hc_size(width = 600, height = 600) %>%
  hc_exporting(enabled = TRUE)


R_comp
IP_comp
CPI_comp


# Saving ####

save(list = c("graph.CPI", "graph.CPI2", "graph.Rt", "graph.Rt2",
              "graph.IP", "graph.IP2", "graph.IP3", "graph.CPI3",
              "graph.Rt3", "R_comp", "IP_comp", "CPI_comp"),
              file = "A_benchmark_graphs.Rdata")
