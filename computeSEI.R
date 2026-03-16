computeSEI <- function(exporter, partner, output){
  
  toSel <- paste0(exporter, "_", listIndustry64)
  toSelPart <- paste0(partner, "_", listIndustry64)
  
  # Load final use matrix
  Fd <- readRDS(here(paste0("~/input-output/7. EUGVC/ECFIN-forecast/Spring25/Back_Forw/Fd_",tPeriod,".rds")))
  
  # Load technical coefficients matrix
  A <- readRDS(here(paste0("~/input-output/7. EUGVC/ECFIN-forecast/Spring25/Back_Forw/A_",tPeriod,".rds")))
  
  # Load global Leontief
  B <- readRDS(here(paste0("~/input-output/7. EUGVC/ECFIN-forecast/Spring25/Back_Forw/B_",tPeriod,".rds")))
  
  # Upload local Leontief matrix, and select ctry L
  L <- readRDS(here(paste0("~/input-output/7. EUGVC/ECFIN-forecast/Spring25/Back_Forw/L_",tPeriod,".rds")))
  LCtry <- L[toSel,toSel]
  
  # ctry industry output
  xc <- readRDS(here(paste0("~/input-output/7. EUGVC/ECFIN-forecast/Spring25/Back_Forw/X_",tPeriod,".rds"))) %>% 
    filter(counterpartArea == exporter) %>% 
    pull(P1)
  
  if(exporter == "CN"){
    
    gvaCtry <- readRDS(here(paste0("~/input-output/7. EUGVC/ECFIN-forecast/Spring25/Back_Forw/GvaCN_", tPeriod,".rds")))
    
  } else if(exporter != "CN"){
    
    # ctry gross value added
    gvaCtry <- readRDS(here(paste0("~/input-output/7. EUGVC/ECFIN-forecast/Spring25/Back_Forw/dtGva_",tPeriod,".rds"))) %>% 
      filter(counterpartArea == exporter) %>% 
      pull(obsValue)
    
    # Calculate gross value added intensity
    gvaCtry <- gvaCtry/xc
    gvaCtry[!is.finite(gvaCtry)] <- 0
    
  }

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
  
  originArea <- exporter
  impArea <- partner

  dtInd <- expand_grid(
    originArea = originArea,
    originIi = listIndustry64,
    impArea = impArea,
    timePeriod = tPeriod) %>% 
    mutate(obsValue = compI + compII + compIII + compIV)
  
  return(dtInd)
  
}
