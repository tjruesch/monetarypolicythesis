rm(list = ls())
source("preamble_BT.R")

####
# To get unsmoothed impulse responses set `smooth = FALSE` below and run this file and then `GRAPHS_2_Rt`
####

smooth = TRUE

for (i in 1:2){
  run = i

  if (run == 1) load("A_FAVAR_IRFs_Rt.Rdata")
  if (run == 2) load("A_FAVAR_IRFs_M1.Rdata")
  if (run == 3) load("A_FAVAR_IRFs_APP.Rdata")

  # IRFs for Variables ####

  # Main Results:
  
  # CPI Growth -> inflation
  CPI_42p <- FAVAR.IRF("BDUUFA01F", title = "CPI", growth = T, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # IP Growth
  IP_42p <- FAVAR.IRF("IPGENERAL", title = "Industrial Production", growth = T, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Rt Growth
  
  if (run == 1) Rt_42p <- irf.graph.p(irf.42Rt, name = "MRO Interest Rate", height = 600)
  if (run == 2){
    irf.42Rt[,2:4] = -irf.42Rt[,2:4]
    Rt_42p <- irf.graph.p(irf.42Rt, name = "M1 Money Aggregate", height = 600)
  }
  if (run == 3) Rt_42p <- irf.graph.p(irf.42Rt, name = "APP Balance", height = 600)

  # Additional Results:
  # Slow:

  # Terms of Trade: BDTOTPRCF
  ToT_42p <- FAVAR.IRF("BDTOTPRCF", title = "Terms of Trade", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Export Price: BDEXPPRCF
  EXP_42p <- FAVAR.IRF("BDEXPPRCF", title = "Export Price", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Import Price: BDIMPPRCF
  IMP_42p <- FAVAR.IRF("BDIMPPRCF", title = "Import Price", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Unemployment level: BDUNPTOTP
  UNL_42p <- FAVAR.IRF("BDUNPTOTP", title = "Unemployment Level", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # GDP: BDGDP...D
  GDP_42p <- FAVAR.IRF("BDGDP...D", title = "GDP", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Consumer Spending: BDCONEXPA
  CSP_42p <- FAVAR.IRF("BDCONEXPA", title = "Consumer Expenditure", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Government Spending: BDEAFFGTA
  GSP_42p <- FAVAR.IRF("BDEAFFGTA", title = "Government Expenditure", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Disposable Income: BDIADPTRA
  DIN_42p <- FAVAR.IRF("BDIADPTRA", title = "Disposable Income", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Savings: BDIANISVA
  SAV_42p <- FAVAR.IRF("BDIANISVA", title = "Savings", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Net lending/borrowing: BDIANILBA
  NLB_42p <- FAVAR.IRF("BDIANILBA", title = "Net Lending/Borrowing", growth = FALSE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # General Gov deficit: BDGGOVBLA
  GDF_42p <- FAVAR.IRF("BDGGOVBLA", title = "General Government Deficit/Surplus", growth = FALSE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))

  # Fast:

  # JPY/EUR: JPEURSP
  JPY_42p <- FAVAR.IRF("JPEURSP", slow = FALSE, title = "JPY/EUR Exchange Rate", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # USD/EUR: USEURSP
  USD_42p <- FAVAR.IRF("USEURSP", FALSE, title = "USD/EUR Exchange Rate", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # ZEW Economic Sentiment: BDZEWECSR
  ZES_42p <- FAVAR.IRF("BDZEWECSR", FALSE, title = "ZEW: Economic Sentiment", growth = FALSE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # ZEW Longterm Interest Rate: BDZEWIL.R
  ZLI_42p <- FAVAR.IRF("BDZEWIL.R", FALSE, title = "ZEW: Long Term Interest Rate", growth = FALSE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Long term Fed Sec: BDT9555
  LTF_42p <- FAVAR.IRF("BDT9555", FALSE, title = "Long Term Federal Securities", growth = FALSE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Foreign Bonds: BDT0024
  FOB_42p <- FAVAR.IRF("BDT0024", FALSE, title = "Foreign Bonds", growth = FALSE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Short term Fed Sec: BDT3400
  STF_42p <- FAVAR.IRF("BDT3400", FALSE, title = "Short Term Federal Securities", growth = FALSE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))
  # Stock markets: DAXINDX
  DAX_42p <- FAVAR.IRF("DAXINDX", FALSE, title = "Stock Markets: DAX Index", growth = TRUE, height = 600, smoothing = ifelse(smooth,TRUE,FALSE))

  if (run == 1){
    save(list = c("CPI_42p","IP_42p","Rt_42p","JPY_42p", "USD_42p", "ZES_42p", "ZLI_42p", "LTF_42p", "FOB_42p", "STF_42p", "DAX_42p",
                  "GDF_42p", "NLB_42p", "SAV_42p", "DIN_42p", "GSP_42p", "CSP_42p", "GDP_42p", "UNL_42p",
                  "IMP_42p", "EXP_42p", "ToT_42p", "irf1", "irf2"), file = "A_IRF_graphs_Rt.Rdata")
  }
  if (run == 2){
    save(list = c("CPI_42p","IP_42p","Rt_42p","JPY_42p", "USD_42p", "ZES_42p", "ZLI_42p", "LTF_42p", "FOB_42p", "STF_42p", "DAX_42p",
                  "GDF_42p", "NLB_42p", "SAV_42p", "DIN_42p", "GSP_42p", "CSP_42p", "GDP_42p", "UNL_42p",
                  "IMP_42p", "EXP_42p", "ToT_42p"), file = "A_IRF_graphs_M1.Rdata")
  }
  if (run == 3){
    save(list = c("CPI_42p","IP_42p","Rt_42p","JPY_42p", "USD_42p", "ZES_42p", "ZLI_42p", "LTF_42p", "FOB_42p", "STF_42p", "DAX_42p",
                  "GDF_42p", "NLB_42p", "SAV_42p", "DIN_42p", "GSP_42p", "CSP_42p", "GDP_42p", "UNL_42p",
                  "IMP_42p", "EXP_42p", "ToT_42p"), file = "A_IRF_graphs_APP.Rdata")
  }
}




