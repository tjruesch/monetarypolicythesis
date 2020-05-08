rm(list = ls())
source("preamble_BT.R")
load("A_adjdata.Rdata")

# This code will take some time to run.
# I would recommend that you run the GRAPH files and only run this code if necessary

for (i in 1:2) {
  # generally, one could also run this code for the APP policy instrument.
  # However, because the time span for this tool is relatively short, it is not suitable for a seriesous discussion.
  run = i

  #### get policy instruments and data ####

  Rt <- XR[, 167];Rt <- (Rt - mean(Rt)) / sd(Rt)
  M1 <- window(M1, start = start(Rt), end = end(Rt)); M1 <- (M1 - mean(M1)) / sd(M1)
  APP <- (APP - mean(APP)) / sd(APP)

  X <- XR[, -c(167, 168)]

  # Second Time Run:
  if (run == 2) Rt <- -M1

  # Third Time Run:
  if (run == 3) {
    Rt <- APP
    X <- window(X, start = start(Rt), end = end(Rt), freq = 12)
  }

  Xslow <- X[, which(slow == 1)]
  Xfast <- X[, which(slow == 0)]

  #### Slow Factors ####
  # 1 to 5 Factors
  t <- dim(Xslow)[1]; n <- dim(Xslow)[2]
  
  # 1

  L_s1 <- (eigen(cov(Xslow))$vectors[,1])#/sqrt(n)
  F_s1 <- Xslow %*% L_s1
  
  # 2
  L_s2 <- (eigen(cov(Xslow))$vectors[,1:2])#/sqrt(n)
  F_s2 <- Xslow %*% L_s2
  
  # 3
  L_s3 <- (eigen(cov(Xslow))$vectors[,1:3])#/sqrt(n)
  F_s3 <- Xslow %*% L_s3
  
  # 4
  L_s4 <- (eigen(cov(Xslow))$vectors[,1:4])#/sqrt(n)
  F_s4 <- Xslow %*% L_s4
  
  # 5
  L_s5 <- (eigen(cov(Xslow))$vectors[,1:5])#/sqrt(n)
  F_s5 <- Xslow %*% L_s5
  
  #### Fast Factors ####
  # 1 to 5 Factors
  t <- dim(Xfast)[1]; n <- dim(Xfast)[2]
  
  # 1

  L_f1 <- (eigen(cov(Xfast))$vectors[, 1])#/sqrt(n)
  F_f1 <- Xfast %*% L_f1
  F_f1 <- F_f1 - lm(F_f1 ~ F_s1 + Rt)$coefficients[3] * Rt

  # 2
  L_f2 <- (eigen(cov(Xfast))$vectors[, 1:2])#/sqrt(n)
  F_f2 <- Xfast %*% L_f2
  F_f2 <- F_f2 - Rt %*% t(lm(F_f2 ~ F_s2 + Rt)$coefficients[4, ])

  # 3
  L_f3 <- (eigen(cov(Xfast))$vectors[,1:3])#/sqrt(n)
  F_f3 <- Xfast%*%L_f3
  F_f3 <- F_f3-Rt%*%t(lm(F_f3 ~ F_s3 + Rt)$coefficients[5,])

  # 4
  L_f4 <- (eigen(cov(Xfast))$vectors[,1:4])#/sqrt(n)
  F_f4 <- Xfast%*%L_f4
  F_f4 <- F_f4-Rt%*%t(lm(F_f4 ~ F_s4 + Rt)$coefficients[6,])

  # 5
  L_f5 <- (eigen(cov(Xfast))$vectors[,1:5])#/sqrt(n)
  F_f5 <- Xfast%*%L_f5
  F_f5 <- F_f5-Rt%*%t(lm(F_f5 ~ F_s5 + Rt)$coefficients[7,])

  #### Estimated number of factors ####
  # Estimated Factors Bai Ng

  #rslow <- baingtest(Xslow,20,ic = "AIC", lagmax = 30)$static
  #rfast <- baingtest(Xfast,20,ic = "AIC", lagmax = 30)$static

  # -> generally large number of factors, rfast does not has a solution

  # Estimated Factors ED
  # do not be confused by the error message style of the result, this is due to my limited programming skills
  #ED(Xfast,10)
  rfast <- 2
  #ED(Xslow,15)
  rslow <- 4

  alternative = TRUE # set to `TRUE` to run additional specifications
  if (alternative){ # alternative specifications: 1:1, 2:2, 3:3 slow:fast factors
    # 1 1 ####
    Yt <- cbind(F_s1,Rt,F_f1)
    p = 7
    if (run == 3) p = 5
    var.11 <- VAR(Yt, type = "none", p = p)
    var.11$p

    amat <- matrix(c(NA, 0, 0, NA, 1, 0, NA, NA, NA), 3,3, byrow=T)
    svar.11 <- SVAR(var.11, estmethod = "direct", Amat = amat)
    irf.11 <- irf(svar.11,impulse="Rt",n.ahead = 48, ortho = F, runs = 50)
    irfc.11 <- irf(svar.11,impulse="Rt",n.ahead = 48, ortho = F, cumulative = T, runs = 50)

    # IRF Graphs
    months <- 0:48

    # IRF Fslow
    lower <- irf.11$Lower$Rt[, 1]
    upper <- irf.11$Upper$Rt[, 1]
    irf <- irf.11$irf$Rt[, 1]
    irf.11slow1 <- data.frame(months, lower, irf, upper)

    # IRF Rt
    lower <- irf.11$Lower$Rt[, 2]
    upper <- irf.11$Upper$Rt[, 2]
    irf <- irf.11$irf$Rt[, 2]
    irf.11Rt <- data.frame(months, lower, irf, upper)

    # IRF Ffast
    lower <- irf.11$Lower$Rt[, 3]
    upper <- irf.11$Upper$Rt[, 3]
    irf <- irf.11$irf$Rt[, 3]
    irf.11fast1 <- data.frame(months, lower, irf, upper)

    # Graphs
    graph.11slow1 <- irf.graph.d(irf.11slow1, name = "Slow Factor", level = "95%")
    graph.11Rt <- irf.graph.d(irf.11Rt, name = "MRO interest rate", level = "95%")
    graph.11fast1 <- irf.graph.d(irf.11fast1, name = "Fast Factor", level = "95%")

    # 2 2 ####
    Yt <- cbind(F_s2,Rt,F_f2)
    var.22 <- VAR(Yt, type = "none", p = 13)
    var.22$p

    amat <- matrix(c(NA, 0, 0, 0, 0,
                     NA, NA, 0, 0, 0,
                     NA, NA, 1, 0, 0,
                     NA, NA, NA, NA, 0,
                     NA, NA, NA, NA, NA),
                     5, 5, byrow=T)
    svar.22 <- SVAR(var.22, estmethod = "direct", Amat = amat)
    irf.22 <- irf(svar.22, impulse="Rt", n.ahead = 48, ortho = F, runs = 50)
    irfc.22 <- irf(svar.22, impulse="Rt", n.ahead = 48, ortho = F, cumulative = T, runs = 50)

    # IRF Fslow
    lower <- irf.22$Lower$Rt[, 1]
    upper <- irf.22$Upper$Rt[, 1]
    irf <- irf.22$irf$Rt[, 1]
    irf.22slow1 <- data.frame(months, lower, irf, upper)

    lower <- irf.22$Lower$Rt[, 2]
    upper <- irf.22$Upper$Rt[, 2]
    irf <- irf.22$irf$Rt[, 2]
    irf.22slow2 <- data.frame(months, lower, irf, upper)

    # IRF Rt
    lower <- irf.22$Lower$Rt[, 3]
    upper <- irf.22$Upper$Rt[, 3]
    irf <- irf.22$irf$Rt[, 3]
    irf.22Rt <- data.frame(months, lower, irf, upper)

    # IRF Ffast
    lower <- irf.22$Lower$Rt[, 4]
    upper <- irf.22$Upper$Rt[, 4]
    irf <- irf.22$irf$Rt[, 4]
    irf.22fast1 <- data.frame(months, lower, irf, upper)

    lower <- irf.22$Lower$Rt[, 5]
    upper <- irf.22$Upper$Rt[, 5]
    irf <- irf.22$irf$Rt[, 5]
    irf.22fast2 <- data.frame(months, lower, irf, upper)

    # 3 3 ####
    Yt <- cbind(F_s3, Rt, F_f3)
    var.33 <- VAR(Yt, type = "none", p = 13)
    var.33$p

    amat <- matrix(c(NA, 0, 0, 0, 0, 0, 0,
                     NA, NA, 0, 0, 0, 0, 0,
                     NA, NA, NA, 0, 0, 0, 0,
                     NA, NA, NA, 1, 0, 0, 0,
                     NA, NA, NA, NA, NA, 0, 0,
                     NA, NA, NA, NA, NA, NA, 0,
                     NA, NA, NA, NA, NA, NA, NA), 7, 7, byrow = T)
    svar.33 <- SVAR(var.33, estmethod = "direct", Amat = amat)
    irf.33 <- irf(svar.33, impulse = "Rt", n.ahead = 48, ortho = F, runs = 50)
    irfc.33 <- irf(svar.33, impulse = "Rt", n.ahead = 48, ortho = F, cumulative = T, runs = 500)

    # IRF Fslow
    lower <- irf.33$Lower$Rt[,1]
    upper <- irf.33$Upper$Rt[,1]
    irf <- irf.33$irf$Rt[,1]
    irf.33slow1 <- data.frame(months,lower,irf,upper)

    lower <- irf.33$Lower$Rt[,2]
    upper <- irf.33$Upper$Rt[,2]
    irf <- irf.33$irf$Rt[,2]
    irf.33slow2 <- data.frame(months,lower,irf,upper)

    lower <- irf.33$Lower$Rt[,3]
    upper <- irf.33$Upper$Rt[,3]
    irf <- irf.33$irf$Rt[,3]
    irf.33slow3 <- data.frame(months,lower,irf,upper)

    # IRF Rt
    lower <- irf.33$Lower$Rt[,4]
    upper <- irf.33$Upper$Rt[,4]
    irf <- irf.33$irf$Rt[,4]
    irf.33Rt <- data.frame(months,lower,irf,upper)

    # IRF Ffast
    lower <- irf.33$Lower$Rt[,5]
    upper <- irf.33$Upper$Rt[,5]
    irf <- irf.33$irf$Rt[,5]
    irf.33fast1 <- data.frame(months,lower,irf,upper)

    lower <- irf.33$Lower$Rt[,6]
    upper <- irf.33$Upper$Rt[,6]
    irf <- irf.33$irf$Rt[,6]
    irf.33fast2 <- data.frame(months,lower,irf,upper)

    lower <- irf.33$Lower$Rt[,7]
    upper <- irf.33$Upper$Rt[,7]
    irf <- irf.33$irf$Rt[,7]
    irf.33fast3 <- data.frame(months,lower,irf,upper)

  }

  #### 4 2 (estimated) ####
  Yt <- cbind(F_s4,Rt,F_f2)
  p = 13
  if (run == 3) p = 5
  var.42 <- VAR(Yt, p = p)
  #var.42 <- VAR(Yt, ic = "AIC", lag.max = 20)
  #var.42$p

  amat <- matrix(c(NA,0,0,0,0,0,0, NA,NA,0,0,0,0,0, NA,NA,NA,0,0,0,0, NA,NA,NA,NA,0,0,0,
                   NA,NA,NA,NA,1,0,0, NA,NA,NA,NA,NA,NA,0, NA,NA,NA,NA,NA,NA,NA), 7,7, byrow = T)
  svar.42 <- SVAR(var.42, estmethod = "direct", Amat = amat)
  irf.42 <- irf(svar.42,impulse ="Rt",n.ahead = 48, ortho = F, runs = 500, ci = 0.95)
  irfc.42 <- irf(svar.42,impulse ="Rt",n.ahead = 48, ortho = F, cumulative = T, ci = 0.95, runs = 500)

  months <- 0:48

  # IRF Fslow
  # irfc
  lower <- irfc.42$Lower$Rt[,1]
  upper <- irfc.42$Upper$Rt[,1]
  irf <- irfc.42$irf$Rt[,1]
  irfc.42slow1 <- data.frame(months,lower,irf,upper)

  lower <- irfc.42$Lower$Rt[,2]
  upper <- irfc.42$Upper$Rt[,2]
  irf <- irfc.42$irf$Rt[,2]
  irfc.42slow2 <- data.frame(months,lower,irf,upper)

  lower <- irfc.42$Lower$Rt[,3]
  upper <- irfc.42$Upper$Rt[,3]
  irf <- irfc.42$irf$Rt[,3]
  irfc.42slow3 <- data.frame(months,lower,irf,upper)

  lower <- irfc.42$Lower$Rt[,4]
  upper <- irfc.42$Upper$Rt[,4]
  irf <- irfc.42$irf$Rt[,4]
  irfc.42slow4 <- data.frame(months,lower,irf,upper)

  # irf
  lower <- irf.42$Lower$Rt[,1]
  upper <- irf.42$Upper$Rt[,1]
  irf <- irf.42$irf$Rt[,1]
  irf.42slow1 <- data.frame(months,lower,irf,upper)

  lower <- irf.42$Lower$Rt[,2]
  upper <- irf.42$Upper$Rt[,2]
  irf <- irf.42$irf$Rt[,2]
  irf.42slow2 <- data.frame(months,lower,irf,upper)

  lower <- irf.42$Lower$Rt[,3]
  upper <- irf.42$Upper$Rt[,3]
  irf <- irf.42$irf$Rt[,3]
  irf.42slow3 <- data.frame(months,lower,irf,upper)

  lower <- irf.42$Lower$Rt[,4]
  upper <- irf.42$Upper$Rt[,4]
  irf <- irf.42$irf$Rt[,4]
  irf.42slow4 <- data.frame(months,lower,irf,upper)

  # IRF Rt
  # irfc
  lower <- irfc.42$Lower$Rt[,5]
  upper <- irfc.42$Upper$Rt[,5]
  irf <- irfc.42$irf$Rt[,5]
  irfc.42Rt <- data.frame(months,lower,irf,upper)

  # irf
  lower <- irf.42$Lower$Rt[,5]
  upper <- irf.42$Upper$Rt[,5]
  irf <- irf.42$irf$Rt[,5]
  irf.42Rt <- data.frame(months,lower,irf,upper)

  # IRF Ffast
  # irfc
  lower <- irfc.42$Lower$Rt[,6]
  upper <- irfc.42$Upper$Rt[,6]
  irf <- irfc.42$irf$Rt[,6]
  irfc.42fast1 <- data.frame(months,lower,irf,upper)

  lower <- irfc.42$Lower$Rt[,7]
  upper <- irfc.42$Upper$Rt[,7]
  irf <- irfc.42$irf$Rt[,7]
  irfc.42fast2 <- data.frame(months,lower,irf,upper)

  #irf
  lower <- irf.42$Lower$Rt[,6]
  upper <- irf.42$Upper$Rt[,6]
  irf <- irf.42$irf$Rt[,6]
  irf.42fast1 <- data.frame(months,lower,irf,upper)

  lower <- irf.42$Lower$Rt[,7]
  upper <- irf.42$Upper$Rt[,7]
  irf <- irf.42$irf$Rt[,7]
  irf.42fast2 <- data.frame(months,lower,irf,upper)

  #### Graph on evolution of impulse responses####

  if (run == 1 & alternative == TRUE){
    # IP
    irf.ip.11 <- irf.11slow1*L_s1[which(colnames(Xslow)=="IPGENERAL")]
    irf.ip.22 <- irf.22slow1*L_s2[which(colnames(Xslow)=="IPGENERAL"),1] +
      irf.22slow2*L_s2[which(colnames(Xslow)=="IPGENERAL"),2]
    irf.ip.33 <- irf.33slow1*L_s3[which(colnames(Xslow)=="IPGENERAL"),1] +
      irf.33slow2*L_s3[which(colnames(Xslow)=="IPGENERAL"),2] +
      irf.33slow3*L_s3[which(colnames(Xslow)=="IPGENERAL"),3]
    irf.ip.42 <- irf.42slow1*L_s4[which(colnames(Xslow)=="IPGENERAL"),1] +
      irf.42slow2*L_s4[which(colnames(Xslow)=="IPGENERAL"),2] +
      irf.42slow3*L_s4[which(colnames(Xslow)=="IPGENERAL"),3] +
      irf.42slow4*L_s4[which(colnames(Xslow)=="IPGENERAL"),4]

    irf1 <- highchart() %>%
      hc_title(text = "Industrial Production Growth", style = list(fontSize = "30px")) %>%
      hc_yAxis(plotLines = list(
        list(color = "#000000",
             dashStyle = "DashDot",
             width = 2,
             value = 0)),
        labels = list(style = list(fontSize = "18px"))) %>%
      hc_xAxis(labels=list(style = list(fontSize = "18px"))) %>%
      hc_add_series(c(cffilter(as.ts(irf.ip.11$irf),root=T)$trend),
                    name = "One Factor", marker = F, color = "black") %>%
      hc_add_series(c(cffilter(as.ts(irf.ip.22$irf),root=T)$trend),
                    name = "Two Factors", marker = F, color = "grey") %>%
      hc_add_series(c(cffilter(as.ts(irf.ip.33$irf),root=T)$trend),
                    name = "Three Factors", marker = F, color = "#CC7000") %>%
      hc_add_series(c(cffilter(as.ts(irf.ip.42$irf),root=T)$trend),
                    name = "Four Factors", marker = F, color = "orange") %>%
      hc_legend(enabled = TRUE, itemStyle = list(fontSize = "22px")) %>%
      hc_size(width = 600, height = 900) %>%
      hc_exporting(enabled = TRUE)

    # Unemployment
    irf.un.11 <- irf.11slow1*L_s1[which(colnames(Xslow)=="BDUNPTOTP")]
    irf.un.22 <- irf.22slow1*L_s2[which(colnames(Xslow)=="BDUNPTOTP"),1] +
      irf.22slow2*L_s2[which(colnames(Xslow)=="BDUNPTOTP"),2]
    irf.un.33 <- irf.33slow1*L_s3[which(colnames(Xslow)=="BDUNPTOTP"),1] +
      irf.33slow2*L_s3[which(colnames(Xslow)=="BDUNPTOTP"),2] +
      irf.33slow3*L_s3[which(colnames(Xslow)=="BDUNPTOTP"),3]
    irf.un.42 <- irf.42slow1*L_s4[which(colnames(Xslow)=="BDUNPTOTP"),1] +
      irf.42slow2*L_s4[which(colnames(Xslow)=="BDUNPTOTP"),2] +
      irf.42slow3*L_s4[which(colnames(Xslow)=="BDUNPTOTP"),3] +
      irf.42slow4*L_s4[which(colnames(Xslow)=="BDUNPTOTP"),4]

    irf2 <-  highchart() %>%
      hc_title(text = "Unemployment Growth", style = list(fontSize = "30px")) %>%
      hc_yAxis(plotLines = list(
        list(color = "#000000",
             dashStyle = "DashDot",
             width = 2,
             value = 0)),
        labels = list(style = list(fontSize = "18px"))) %>%
      hc_xAxis(labels=list(style = list(fontSize = "18px"))) %>%
      hc_add_series(c(cffilter(as.ts(irf.un.11$irf),root=T)$trend),
                    name = "One Factor", marker = F, color = "black") %>%
      hc_add_series(c(cffilter(as.ts(irf.un.22$irf),root=T)$trend),
                    name = "Two Factors", marker = F, color = "grey") %>%
      hc_add_series(c(cffilter(as.ts(irf.un.33$irf),root=T)$trend),
                    name = "Three Factors", marker = F, color = "#CC7000") %>%
      hc_add_series(c(cffilter(as.ts(irf.un.42$irf),root=T)$trend),
                    name = "Four Factors", marker = F, color = "orange") %>%
      hc_legend(enabled = TRUE, itemStyle = list(fontSize = "22px")) %>%
      hc_size(width = 600, height = 900) %>%
      hc_exporting(enabled = TRUE)
  }


  if (run == 1) save(list = ls(), file = "A_FAVAR_IRFs_Rt.Rdata")
  if (run == 2) save(list = ls(), file = "A_FAVAR_IRFs_M1.Rdata")
  if (run == 3) save(list = ls(), file = "A_FAVAR_IRFs_APP.Rdata")
}
