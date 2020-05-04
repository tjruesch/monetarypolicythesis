rm(list = ls())

#### functions ####

# Packages installing/loading ####

packinstall <- function(x){
  for (i in 1:length(x)){
    if (!require(x[i],character.only = TRUE)){
      install.packages(x[i],dep=TRUE)
      if(!require(x[i],character.only = TRUE)) stop("Package not found")
    }
  }
}

# Unit root testing ####

unitroot <- function(ts, level = 0.011, pvalues = FALSE){
  length <- ifelse(is.null(dim(ts)), 1, dim(ts)[2])
  
  if (length > 1){
    p.values <- rep(0,length)
    for (i in 1:length){
      p.values[i] = suppressWarnings(adf.test(na.remove(ts[,i]))$p.value)
    }
  } else {
    p.values <- 0
    p.values = suppressWarnings(adf.test(na.remove(ts))$p.value)
  }
  
  if (pvalues == TRUE){
    return(p.values)
  } else {
    if (sum(p.values < level) == length(p.values)) return("stationary")
    if (sum(p.values > level) == length(p.values)) return("nonstationary")
    else return(ifelse(p.values < level, "stationary", "nonstationary"))
  }
}

# Seasonality testing ####

seasonal <- function(ts, test = "qs", level = 0.01, pvalues = FALSE){
  length <- ifelse(is.null(dim(ts)), 1, dim(ts)[2])
  freq = frequency(ts)
  
  if (length > 1){
    seasonal = rep(FALSE, dim(ts)[2])
    p.value = rep(0, dim(ts)[2])
    for (i in 1:length){
      x = na.remove(ts[,i])
      
      if (tolower(test) == "wo") {
        statistic <- seastests::wo(x, freq = freq)
        seasonal[i] = ifelse(statistic$stat == TRUE, TRUE, FALSE)
        p.value[i] = NA
      }
      if (tolower(test) == "qs") {
        statistic <- seastests::qs(x, freq = freq, residuals = T, diff = F)
        seasonal[i] = ifelse(statistic$Pval < level, TRUE, FALSE)
        p.value[i] = statistic$Pval
      }
      if (tolower(test) == "fried") {
        statistic <- seastests::fried(x, freq = freq, residuals = T, diff = F)
        seasonal[i] = ifelse(statistic$Pval < level, TRUE, FALSE)
        p.value[i] = statistic$Pval
      }
      if (tolower(test) == "kw") {
        statistic <- seastests::kw(x, freq = freq, residuals = T, diff = F)
        seasonal[i] = ifelse(statistic$Pval < level, TRUE, FALSE)
        p.value[i] = statistic$Pval
      }
      if (tolower(test) == "seasdum") {
        statistic <- seastests::seasdum(x, freq = freq)
        seasonal[i] = ifelse(statistic$Pval < level, TRUE, FALSE)
        p.value[i] = statistic$Pval
      }
      if (tolower(test) == "welch") {
        statistic <- seastests::welch(x, freq = freq, residuals = T, diff = F)
        seasonal[i] = ifelse(statistic$Pval < level, TRUE, FALSE)
        p.value[i] = statistic$Pval
      }
    }
  } else {
    x = na.remove(ts)
    
    if (tolower(test) == "wo") {
      statistic <- seastests::wo(x, freq = freq)
      seasonal = ifelse(statistic$stat == TRUE, TRUE, FALSE)
      p.value = NA
    }
    if (tolower(test) == "qs") {
      statistic <- seastests::qs(x, freq = freq, residuals = T, diff = F)
      seasonal = ifelse(statistic$Pval < level, TRUE, FALSE)
      p.value = statistic$Pval
    }
    if (tolower(test) == "fried") {
      statistic <- seastests::fried(x, freq = freq, residuals = T, diff = F)
      seasonal = ifelse(statistic$Pval < level, TRUE, FALSE)
      p.value = statistic$Pval
    }
    if (tolower(test) == "kw") {
      statistic <- seastests::kw(x, freq = freq, residuals = T, diff = F)
      seasonal = ifelse(statistic$Pval < level, TRUE, FALSE)
      p.value = statistic$Pval
    }
    if (tolower(test) == "seasdum") {
      statistic <- seastests::seasdum(x, freq = freq)
      seasonal = ifelse(statistic$Pval < level, TRUE, FALSE)
      p.value = statistic$Pval
    }
    if (tolower(test) == "welch") {
      statistic <- seastests::welch(x, freq = freq, residuals = T, diff = F)
      seasonal = ifelse(statistic$Pval < level, TRUE, FALSE)
      p.value = statistic$Pval
    }
  }
  
  if (pvalues == FALSE){
    return(seasonal)
  } else {
    return(p.value)
  }
}

# Graphs for IRFs #### 
# ci option currently not in use

irf.graph.p <- function(X, name = NULL, level = "95%", ci = TRUE, height = 600, width = 600){
  hc <- highchart()%>%
    hc_title(text = name, style = list(fontSize = "30px")) %>%
    hc_yAxis(plotLines = list(
      list(color = "#000000",
           dashStyle = "DashDot",
           width = 2,
           value = 0)),
      labels = list(style = list(fontSize = "18px"))) %>%
    hc_xAxis(labels=list(style = list(fontSize = "18px"))) %>%
    hc_add_series(X$irf, color = "#000000", name = "Impulse Response", marker = F) %>%
    hc_legend(enabled = FALSE, itemStyle = list(fontSize = "20px")) %>%
    hc_size(width = width, height = height) %>%
    hc_exporting(enabled = TRUE)

    hc <- hc %>% hc_add_series(X, hcaes(x = months, low = lower, high = upper), 
                               type = "arearange", color = "#FF0000", marker = F, 
                               fillOpacity = 0.1, name = paste(level, "Confidence Interval"))
  hc
}

irf.graph.d <- function(X, name = NULL, level = "95%", ci = TRUE){
  highchart()%>%
    hc_title(text = name) %>%
    hc_yAxis(plotLines = list(
      list(color = "#000000",
           dashStyle = "DashDot",
           width = 2,
           value = 0))) %>%
    hc_add_series(X, hcaes(x = months, low = lower, high = upper),
                  type = "arearange", color = "#FF0000", marker = F, 
                  fillOpacity = 0.1, name = paste(level, "Confidence Interval")) %>%
    hc_add_series(X$irf, color = "#000000", name = "Impulse Response", showInLegend = FALSE) %>%
    hc_exporting(enabled = TRUE)
}

# ED test ####

ED <- function(X,j){
  ED1 <- function(X,j){
    lambda <- eigen(cov(X))$values
    # Step 1
    x <- c(lambda[j:(j+4)])
    y <- c((
      j+seq(-1,3))^(2/3))
    betahat <- lm(x ~ y)$coefficients[2]
    delta <- 2*abs(betahat)
    rdelta <- rep(0,length(lambda))
    
    for(i in 1:length(lambda)){
      rdelta[i] <- lambda[i]-lambda[i+1]
    }
    rhatdelta <- max(which(rdelta>delta))
    return(rhatdelta)
  }
  j <- ED1(X,j)
  for (i in 1:10){
    E1 = ED1(X,j);Ei = ED1(X,j+i)
    if(E1 == Ei) stop(ED1(X,j+i))
  }
  stop("No convergence")
}

# Bai & Ng IC and test ######

baingtest <- function(X,rmax,factors=c("static","dynamic"),plot.IC = FALSE,ic="FPE",lagmax=10,p=NA,cor=FALSE,delta=0.1,m=1,stops=TRUE){
  stopQuietly <- function(...) {
    blankMsg <- sprintf("\r%s\r", paste(rep(" ", getOption("width")-1L), collapse=" "));
    stop(simpleError(blankMsg));
  }
  factors <- match.arg(factors,c("static","dynamic"),several.ok = TRUE)
  ## Bai & Ng (2002) IC
  n = dim(X)[2]
  t = dim(X)[1]
  
  Gamma0 <- cov(X)
  H <- eigen(Gamma0)$vectors[,1:rmax]
  D <- diag(eigen(Gamma0)$values[1:rmax])
  V <- rep(NA, rmax)
  
  for (r in 1:rmax){
    I = X%*%(diag(n) - H[,1:r]%*%t(H[,1:r]))
    V[r] = sum(sum(I^2))/(n*t)
  }
  
  e = (n+t)/(n*t)
  p1 = rep(NA,rmax); p2=p1; p3=p1
  
  p1 = (1:rmax)*e*log(1/e)
  p2 = (1:rmax)*e*log(min(n,t))
  p3 = (1:rmax)*(log(min(n,t))/min(n,t))
  
  IC = matrix(NA, rmax, 3)
  IC[,1] = log(V) + p1
  IC[,2] = log(V) + p2
  IC[,3] = log(V) + p3
  
  if (plot.IC == TRUE){
    for (i in 1:3){
      IC[,i] <- (IC[,i]-mean(IC[,i]))/sd(IC[,i])
    }
    return(highchart() %>%
             hc_add_series(IC[,1], name = "Penalty Function 1") %>%
             hc_add_series(IC[,2], name = "Penalty Function 2") %>%
             hc_add_series(IC[,3], name = "Penalty Function 3") %>%
             hc_exporting(enabled = TRUE))
    stopQuietly()
  }
  
  resstatic <- c(which.min(IC[,1]),which.min(IC[,2]),which.min(IC[,3]))
  static <- data.frame(which.min(IC[,1]),which.min(IC[,2]),which.min(IC[,3]))
  colnames(static) <- c("Penalty 1", "Penalty 2", "Penalty 3")
  
  if (stops == TRUE){
    if(static[,1]==rmax&static[,2]==rmax&static[,3]==rmax){
      stop("Number of static factors hits upper bound")
    }
  }
  
  if(is.element("dynamic",factors) == FALSE) {
    return(static)
    stopQuietly()
  }
  ## Bai & Ng (2007) Test
  nrF <- static[,2]
  # Estimation of Fhat
  Lambdahat <- (eigen(cov(X))$vectors*sqrt(dim(X)[2]))[,1:nrF]
  Fhat <- (X%*%Lambdahat)/dim(X)[2]
  # VAR estimation
  if(is.na(p)==TRUE){ 
    var <- suppressWarnings(VAR(Fhat, ic=ic))
  } else {
    var <- suppressWarnings(VAR(Fhat, p=p))
  }
  var.p <- var$p
  # Sigma_uu
  uhat <- residuals(var)
  Sigma_uu <- cov(uhat)
  if (cor == TRUE) Sigma_uu <- cov2cor(Sigma_uu)
  # D1 and D2
  D1 <- function(k){
    c2_k1 <- (eigen(Sigma_uu)$values[(k+1)])^2
    sum_c <- sum((eigen(Sigma_uu)$values[1:nrF])^2)
    D1 <- sqrt(c2_k1/sum_c)
    return(D1)
  }
  D2 <- function(k){
    sum_ck1 <- sum((eigen(Sigma_uu)$values[(k+1):nrF])^2)
    sum_c <- sum((eigen(Sigma_uu)$values[1:nrF])^2)
    D2 <- sqrt(sum_ck1/sum_c)
    return(D2)
  }
  # Test 1
  M <- min(dim(X)[1]^(.5-delta),dim(X)[2]^(.5-delta))
  K3 <- rep(NA,nrF)
  for (k in 1:nrF){
    K3[k] <- D1(k)
  }
  q3 <- min(which(K3<(m/M)))
  # Test 2
  K4 <- rep(NA,nrF)
  for (k in 1:nrF){
    K4[k] <- D2(k)
  }
  q4 <- min(which(K4<(m/M)))
  
  resdynamic <- c(q3,q4)
  
  static <- c("", "dynamic factors", "")
  p1 <- c(resstatic[1], "q3", q3)
  p2 <- c(resstatic[2], "q4", q4)
  p3 <- c( resstatic[3], "VAR order", var.p)
  
  table = as.data.frame(rbind(static,p1,p2,p3), optional = TRUE)
  row.names(table)[1] <- "static factors"
  
  result <- list(
    table = table,
    static = min(resstatic),
    dynamic = min(resdynamic)
  )
  
  return(result)
}

# FAVAR IRFs ####

FAVAR.IRF <- function(name,slow = TRUE,title = NULL,print = TRUE,growth = FALSE, level = "95%", ci = FALSE, smoothing = TRUE, height = 900, width = 600){
  if (growth==FALSE){
    if (slow==TRUE){
      IRF <- irfc.42slow1*L_s4[which(colnames(Xslow)==name),1] +
        irfc.42slow2*L_s4[which(colnames(Xslow)==name),2] +
        irfc.42slow3*L_s4[which(colnames(Xslow)==name),3] +
        irfc.42slow4*L_s4[which(colnames(Xslow)==name),4]
      IRF$months <- 0:48
      if (smoothing == TRUE){
        IRF$irf <- c(cffilter(as.ts(IRF$irf),root=T)$trend)
        IRF$upper <- c(cffilter(as.ts(IRF$upper),root=T)$trend)
        IRF$lower <- c(cffilter(as.ts(IRF$lower),root=T)$trend)
      }
      graph.d <- irf.graph.d(IRF, name = title, level = level, ci = ci)
      graph.p <- irf.graph.p(IRF, name = title, level = level, ci = ci, height = height, width = width)
    } else {
      IRF <- irfc.42fast1*L_f2[which(colnames(Xfast)==name),1] +
        irfc.42fast2*L_f2[which(colnames(Xfast)==name),2] +
        irfc.42Rt*lm(Xfast[,which(colnames(Xfast)==name)]~Rt)$coefficients[2]
      IRF$months <- 0:48
      if (smoothing == TRUE){
        IRF$irf <- c(cffilter(as.ts(IRF$irf),root=T)$trend)
        IRF$upper <- c(cffilter(as.ts(IRF$upper),root=T)$trend)
        IRF$lower <- c(cffilter(as.ts(IRF$lower),root=T)$trend)
      }
      graph.d <- irf.graph.d(IRF, name = title, level = level, ci = ci)
      graph.p <- irf.graph.p(IRF, name = title, level = level, ci = ci, height = height, width = width)
    }
    if (print==TRUE){
      graph.p
    } else {
      graph.d
    }
  } else {
    if (slow==TRUE){
      IRF <- irf.42slow1*L_s4[which(colnames(Xslow)==name),1] +
        irf.42slow2*L_s4[which(colnames(Xslow)==name),2] +
        irf.42slow3*L_s4[which(colnames(Xslow)==name),3] +
        irf.42slow4*L_s4[which(colnames(Xslow)==name),4]
      IRF$months <- 0:48
      if (smoothing == TRUE){
        IRF$irf <- c(cffilter(as.ts(IRF$irf),root=T)$trend)
        IRF$upper <- c(cffilter(as.ts(IRF$upper),root=T)$trend)
        IRF$lower <- c(cffilter(as.ts(IRF$lower),root=T)$trend)
      }
      graph.d <- irf.graph.d(IRF, name = paste(title,"Growth"), level = level, ci = ci)
      graph.p <- irf.graph.p(IRF, name = paste(title,"Growth"), level = level, ci = ci, height = height, width = width)
    } else {
      IRF <- irf.42fast1*L_f2[which(colnames(Xfast)==name),1] +
        irf.42fast2*L_f2[which(colnames(Xfast)==name),2] +
        irf.42Rt*lm(Xfast[,which(colnames(Xfast)==name)]~Rt)$coefficients[2]
      IRF$months <- 0:48
      if (smoothing == TRUE){
        IRF$irf <- c(cffilter(as.ts(IRF$irf),root=T)$trend)
        IRF$upper <- c(cffilter(as.ts(IRF$upper),root=T)$trend)
        IRF$lower <- c(cffilter(as.ts(IRF$lower),root=T)$trend)
      }
      graph.d <- irf.graph.d(IRF, name = paste(title,"Growth"), level = level, ci = ci)
      graph.p <- irf.graph.p(IRF, name = paste(title,"Growth"), level = level, ci = ci, height = height, width = width)
    }
    if (print==TRUE){
      graph.p
    } else {
      graph.d
    }
  }
}
#### install packages ####

packages <- c("readr","readxl","zoo","xts","tseries","car","lmtest","dynlm","seastests","strucchange","seasonal",
              "fanplot","ggplot2","gridExtra","stargazer","mFilter","missMDA","ggfortify","dygraphs","highcharter",
              "knitr", "vars", "pracma", "grDevices")
packinstall(packages)




