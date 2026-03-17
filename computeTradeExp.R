computeTradeExp <- function(tPeriod, exporter, partner){
  
  print(tPeriod)
  
  # Load final use matrix
  Fd <- readRDS(here(paste0(path, "Fd_",tPeriod,".rds")))
  
  # Load technical coefficients matrix
  A <- readRDS(here(paste0(path, "A_",tPeriod,".rds")))
  
  # Load global Leontief
  B <- readRDS(here(paste0(path, "B_",tPeriod,".rds")))
  
  # Upload local Leontief matrix, and select ctry L
  L <- readRDS(here(paste0(path, "L_",tPeriod,".rds")))
  
  toSel <- paste0(exporter, "_", industry64)
  toSelPart <- paste0(partner, "_", industry64)
  
  if (exporter == "EU") {
    
    # Load regional Leontief
    R <- readRDS(here(paste0(path, "R_",tPeriod,".rds")))
    
    # Obtain inputs of EU in ctry production
    AcimCtry <- A[ctr_ind_eu,toSelPart]
    
    # Filter ctry direct and indirect inputs
    Bc <- B[toSelPart,]
    
    # Filter final use of ctry for products of origin (EU)
    fcI <- Fd %>% filter(refArea %in% EuCountries,
                         counterpartArea == partner) %>% 
      pull(obsValue)
    
    # Filter final use of ctry
    fcII <- Fd %>% filter(counterpartArea == partner) %>% 
      pull(obsValue)
    
    # Filter final use for other countries different from ctry
    fcIII <- Fd %>% filter(!counterpartArea == partner) %>% 
      group_by(refArea, rowIi) %>% summarise(obsValue = sum(obsValue)) %>% 
      ungroup() %>% pull(obsValue)
    
    # Load EU GVA intensity
    GvaEu <- readRDS(here(paste0(path, "GvaEu_",tPeriod,".rds")))
    
    # Obtain inputs of EU in production of countries different from ctry
    Ac <- A[ctr_ind_eu,c(!colnames(A) %in% c(toSelPart, ctr_ind_eu))]
    
    # Filter other countries direct and indirect inputs
    Bnc <- B[c(!colnames(A) %in% c(toSelPart, ctr_ind_eu)),]
    
    # Estimate components I to IV
    compI <- rowSums(t(t(R*GvaEu)*fcI))
    
    compII <- rowSums(t(t((R*GvaEu) %*% AcimCtry %*% Bc)*fcII))
    
    compIII <- rowSums(t(t((R*GvaEu) %*% AcimCtry %*% Bc)*fcIII))
    
    compIV <- rowSums(t(t((R*GvaEu) %*% Ac %*% Bnc)*fcII))
    
    originLabel <- EuCountries
    impLabel <- partner
    
  }
  
  if(partner == "EU"){
    
    # Upload local Leontief matrix, and select ctry L
    L <- readRDS(here(paste0(path, "L_",tPeriod,".rds")))
    LCtry <- L[toSel,toSel]
    
    # Obtain inputs of ctry in EU production
    AcexCtry <- A[toSel, ctr_ind_eu]
    
    # ctry industry output
    xc <- readRDS(here(paste0(path, "X_",tPeriod,".rds"))) %>% 
      filter(counterpartArea == exporter) %>% 
      pull(P1)
    
    # ctry gross value added
    gvaCtry <- readRDS(here(paste0(path, "dtGva_",tPeriod,".rds"))) %>% 
      filter(counterpartArea == exporter) %>% 
      pull(obsValue)
    
    # Calculate gross value added intensity
    gvaCtry <- gvaCtry/xc
    gvaCtry[!is.finite(gvaCtry)] <- 0
    
    # Filter EU direct and indirect inputs
    Beu <- B[ctr_ind_eu,]
    
    # Filter final use of EU of products of ctry
    fcI <- Fd %>% filter(refArea == exporter,
                         counterpartArea %in% EuCountries) %>% 
      group_by(refArea, rowIi) %>% summarise(obsValue = sum(obsValue)) %>% 
      ungroup() %>% pull(obsValue)
    
    # Filter final use of EU
    fcII <- Fd %>% filter(counterpartArea %in% EuCountries) %>% 
      group_by(refArea, rowIi) %>% summarise(obsValue = sum(obsValue)) %>% 
      ungroup() %>% pull(obsValue)
    
    # Filter final use of EU products for other countries different from ctry
    fcIII <- Fd %>% filter(!counterpartArea %in% EuCountries) %>% 
      group_by(refArea, rowIi) %>% summarise(obsValue = sum(obsValue)) %>% 
      ungroup() %>% pull(obsValue)
    
    # Obtain inputs of ctry in production of countries different from EU
    Ac <- A[toSel,c(!colnames(A) %in% c(toSel, ctr_ind_eu))]
    
    # Filter other countries direct and indirect inputs
    Bnc <- B[c(!colnames(A) %in% c(toSel, ctr_ind_eu)),]
    
    # Estimate components I to IV
    compI <- rowSums(t(t(LCtry*gvaCtry)*fcI))
    
    compII <- rowSums(t(t((LCtry*gvaCtry) %*% AcexCtry %*% Beu)*fcII))
    
    compIII <- rowSums(t(t((LCtry*gvaCtry) %*% AcexCtry %*% Beu)*fcIII))
    
    compIV <- rowSums(t(t((LCtry*gvaCtry) %*% Ac %*% Bnc)*fcII))
    
    originLabel <- exporter
    impLabel <- "EU"
    
  }
  
  if(exporter != "EU" &
     partner != "EU"){
    
    LCtry <- L[toSel,toSel]
    
    # ctry industry output
    xc <- readRDS(here(paste0(path, "X_",tPeriod,".rds"))) %>% 
      filter(counterpartArea == exporter) %>% 
      pull(P1)
    
    # ctry gross value added
    gvaCtry <- readRDS(here(paste0(path, "dtGva_",tPeriod,".rds"))) %>% 
      filter(counterpartArea == exporter) %>% 
      pull(obsValue)
    
    # Calculate gross value added intensity
    gvaCtry <- gvaCtry/xc
    gvaCtry[!is.finite(gvaCtry)] <- 0
    
    # Obtain inputs of ctry in EU production
    AcexCtry <- A[toSel, toSelPart]
    
    # Obtain inputs of ctry in production of countries different from partner
    Ac <- A[toSel,c(!colnames(A) %in% c(toSel, toSelPart))]
    
    # Filter EU direct and indirect inputs
    Bpartn <- B[toSelPart,]
    
    # Filter other countries direct and indirect inputs
    Bnc <- B[c(!colnames(A) %in% c(toSel, toSelPart)),]
    
    # Filter final use of partner of products of exporter
    fcI <- Fd %>% filter(refArea == exporter,
                         counterpartArea %in% partner) %>% 
      group_by(refArea, rowIi) %>% summarise(obsValue = sum(obsValue)) %>% 
      ungroup() %>% pull(obsValue)
    
    # Filter final use of partner
    fcII <- Fd %>% filter(counterpartArea %in% partner) %>% 
      group_by(refArea, rowIi) %>% summarise(obsValue = sum(obsValue)) %>% 
      ungroup() %>% pull(obsValue)
    
    # Filter final use of partner products for other countries different from exporter
    fcIII <- Fd %>% filter(!counterpartArea %in% partner) %>% 
      group_by(refArea, rowIi) %>% summarise(obsValue = sum(obsValue)) %>% 
      ungroup() %>% pull(obsValue)
    
    # Estimate components I to IV
    compI <- rowSums(t(t(LCtry*gvaCtry)*fcI))
    
    compII <- rowSums(t(t((LCtry*gvaCtry) %*% AcexCtry %*% Bpartn)*fcII))
    
    compIII <- rowSums(t(t((LCtry*gvaCtry) %*% AcexCtry %*% Bpartn)*fcIII))
    
    compIV <- rowSums(t(t((LCtry*gvaCtry) %*% Ac %*% Bnc)*fcII))
    
    originLabel <- exporter
    impLabel <- partner
    
  }

  dtInd <- expand_grid(
    originArea = originLabel,
    originIi = industry64,
    impArea = impLabel,
    timePeriod = tPeriod) %>% 
    mutate(I = compI,
           II = compII,
           III = compIII,
           IV = compIV) %>% 
    pivot_longer(-c("originArea", "originIi", "impArea", "timePeriod"),
                 names_to = "component")
  
  return(dtInd)
  
}
