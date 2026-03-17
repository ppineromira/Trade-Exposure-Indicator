pacman::p_load(tidyverse, data.table)
options(dplyr.summarise.inform = FALSE)

# Path to data
path <- pathToData

# Classifications
load(paste0(pathToData, "classif.RData"))

years <- c(2023:2010)

# Function for the Trade Exposure Indicator
source("computeTradeExp.R")

# Trade Exposure between EU and China
tpe_CN_EU <- lapply(years, computeTradeExp, exporter = "CN", partner = "EU") %>% 
  rbindlist()

tpe_EU_CN <- lapply(years, computeTradeExp, exporter = "EU", partner = "CN") %>% 
  rbindlist()

# Trade Exposure between EU and the US
tpe_US_EU <- lapply(years, computeTradeExp, exporter = "US", partner = "EU") %>% 
  rbindlist()

tpe_EU_US <- lapply(years, computeTradeExp, exporter = "EU", partner = "US") %>% 
  rbindlist()

# Trade Exposure between China and the US
tpe_US_CN <- lapply(years, computeTradeExp, exporter = "US", partner = "CN") %>% 
  rbindlist()

tpe_CN_US <- lapply(years, computeTradeExp, exporter = "CN", partner = "US") %>% 
  rbindlist()

