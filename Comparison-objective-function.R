## Naming conventions
## 'sim' --> Related to Ecosim
## 'spa' --> Related to Ecospace
## 'obs' --> Related to observed timeseries data, i.e., that Ecosim was fitted to. 
## 'B'   --> Denotes biomass
## 'C'   --> Denotes catch

## Setup -----------------------------------------------------------------------
rm(list=ls())
source("./functions.R") ## Pull in functions
library(dplyr)

## Input set up ----------------------------------------------------------------
ewe_name     = "EwE_Outputs"
sim_scenario = "sim-spa_01"
obs_TS_name  = "TS_updated_IB13"
srt_year     = 1980
spa_scenarios  = c("spa_00", "spa_01", "spa_02_MOM6-ISIMIP3a", "spa_03_MOM6-ISIMIP3a_PP-phyc-vint")
spa_scen_names = c("01 Base", "02 MODIS-ChlA", "03 MOM6-ChlA", "04 MOM6-Vint")

## User-defined output parameters ----------------------------------------------
today_date <- format(Sys.Date(), "%Y-%m-%d")
out_file_notes = "multispa"
plot_name_xY = paste0("B_scaled_xY_", out_file_notes)
#plot_name_xM = paste0("B_scaled_xM_", out_file_notes)

## Set scaling parameters 
#init_years_toscale = 5
init_years_toscale  = 2016-1980 + 1 ## 36. This will scale all outputs to the global average
init_months_toscale = init_years_toscale * 12

## -----------------------------------------------------------------------------
##
## Read-in ANNUAL Observed, Ecosim, and Ecospace TS
dir_sim = paste0("./", ewe_name, "/ecosim_", sim_scenario, "/")

## Read-in Ecosim annual biomass 
filename = paste0(dir_sim, "biomass_annual.csv")
num_skip_sim = f.find_start_line(filename, flag = srt_year)

simB_xY <- read.csv(paste0(dir_sim, "biomass_annual.csv"), skip = num_skip_sim)
years = simB_xY$year.group ## Get date range from Ecosim
simB_xY$year.group = NULL

## Read-in Ecosim annual catches
simC_xY <- read.csv(paste0(dir_sim, "catch_annual.csv"), skip = num_skip_sim)
simC_xY$year.group = NULL

## Read-in Ecospace annual biomass and catches ---------------------------------
ls_spaB_xY <- list()
ls_spaC_xY <- list()
for (i in 1:length(spa_scenarios)) {
  dir_spa = paste0("./", ewe_name, "/", spa_scenarios[i], "/")
  filename <- paste0(dir_spa, "Ecospace_Annual_Average_Biomass.csv")
  num_skip_spa <- f.find_start_line(filename, flag = "Year")
  spaB_xY <- read.csv(paste0(dir_spa, "Ecospace_Annual_Average_Biomass.csv"), 
                      skip = num_skip_spa, header = TRUE)
  spaB_xY$Year = NULL
  
  ## Standardize FG names ------------------------------
  fg_names = f.standardize_group_names(colnames(spaB_xY))
  num_fg = length(fg_names)
  fg_df <- data.frame(
    pool_code  = 1:num_fg,
    group_name = paste(sprintf("%02d", 1:num_fg),
                       gsub("_", " ", fg_names))
  )
  
  ## Set row and column names
  rownames(spaB_xY) = rownames(simB_xY) = years
  colnames(spaB_xY) = colnames(simB_xY) = fg_df$group_name
  
  ## Add current spaB_xY reading into the list object ----
  ls_spaB_xY[[i]] <- spaB_xY
  
  ## Read-in Ecospace annual catch
  spaC_xY <- read.csv(paste0(dir_spa, "Ecospace_Annual_Average_Catch.csv"), 
                      skip = num_skip_spa, header = TRUE)
  spaC_xY$Year = NULL
  ls_spaC_xY[[i]] <- spaC_xY
}

## -----------------------------------------------------------------------------
## Prepare months and date series objects
start_y <- min(years)
end_y   <- max(years)
date_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 month")
year_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 year")
ym_series <- format(date_series, "%Y-%m")

## Read in MONTHLY biomasses
simB_xM <- read.csv(paste0(dir_sim, "biomass_monthly.csv"), skip = num_skip_sim); simB_xM$timestep.group = NULL
simC_xM <- read.csv(paste0(dir_sim, "catch_monthly.csv"), skip = num_skip_sim); simC_xM$timestep.group = NULL
spaB_xM <- read.csv(paste0(dir_spa, "Ecospace_Average_Biomass.csv"), skip = num_skip_spa, header = TRUE); spaB_xM$TimeStep = NULL
rownames(spaB_xM) = rownames(simB_xM) = ym_series

## Read in "observed" timeseries -----------------------------------------------
dir_obs = paste0("./", ewe_name, "/", obs_TS_name, ".csv")
obs.list = f.read_ecosim_timeseries(dir_obs, num_row_header = 4)
for(i in 1:length(obs.list)){assign(names(obs.list)[i],obs.list[[i]])} #make separate dataframe for each list element

obsB.head <- merge(obsB.head, fg_df, by = "pool_code", all.x = TRUE)
obsC.head <- merge(obsC.head, fg_df, by = "pool_code", all.x = TRUE)

colnames(obsB) = obsB.head$group_name
colnames(obsC) = obsC.head$group_name

## -----------------------------------------------------------------------------
##
## 
## 

## -----------------------------------------------------------------------------
## 

init_years_toscale = 5

fit_metrics <- data.frame(
  nll_spa_obs = NA, nll_sim_obs = NA, nll_spa_sim = NA,
  pb_spa_obs = NA, pb_sim_obs = NA, pb_spa_sim = NA,
  mae_spa_obs = NA, mae_sim_obs = NA, mae_spa_sim = NA)

for(i in 1:num_fg){
  # i = 6
  ## Get biomass for individual FG: Observed, Ecosim, and Ecospace
  (grp  = fg_df$group_name[i])
  simB = simB_xY[,i] 
  spaB_ls <- lapply(ls_spaB_xY, function(df) df[, i]) ## Extract the i column from each data frame in the list
  
  ## Check to see if observed data is available
  obsB_scaled=NULL
  if(i %in% obsB.head$pool_code){
    obs.idx     = which(obsB.head$pool_code==i)
    obs_df      = suppressWarnings( ## Suppress warnings thrown when obs not available
      data.frame(year_series, obsB = as.numeric(obsB[ ,obs.idx]))
    )
    non_na_obsB = obs_df$obsB[!is.na(obs_df$obsB)] # Extract non-NA values from obs_df$obsB
    if_else (length(non_na_obsB) < init_years_toscale,
             years_to_scale <- length(non_na_obsB),
             years_to_scale <- init_years_toscale)
    mean_init_years = mean(non_na_obsB[1:years_to_scale]) # Calculate the mean of the first 'init_years_toscale' non-NA values
    obsB_scaled = obs_df$obsB / mean_init_years # Scale the entire obs_df$obsB by this mean
  } else obsB_scaled=rep(NA, length(simB))  
  
  ## Scale to the average of a given timeframe
  simB_scaled = simB / mean(simB[1:init_years_toscale], na.rm = TRUE) ## Ecosim scaled
  spaB_scaled_ls = list() ## List of Ecospace scaled
  for(j in 1:length(spa_scenarios)){
    spaB               <- spaB_ls[[j]]
    spaB_scaled        <- spaB / mean(spaB[1:init_years_toscale], na.rm = TRUE)
    spaB_scaled_ls[[j]] <- spaB_scaled
  }
  
  ## Create data frame to compare observed, Ecosim, and Ecospace
  comp_df <- data.frame(obs = obsB_scaled, sim = simB_scaled, spa = spaB_scaled)
  
  ## Calculate log-liklihood ---------------------------------------------------
  # Calculate log-likelihood for spa vs obs
  resids <- comp_df$obs - comp_df$spa
  nll_spa_obs <- -(-length(comp_df$obs)/2 * log(2*pi*var(resids, na.rm=TRUE)) - 1/(2*var(resids, na.rm=TRUE)) * sum(resids^2, na.rm=TRUE))
  
  # Calculate log-likelihood for sim vs obs
  resids <- comp_df$obs - comp_df$sim
  nll_sim_obs <- -(-length(comp_df$obs)/2 * log(2*pi*var(resids, na.rm=TRUE)) - 1/(2*var(resids, na.rm=TRUE)) * sum(resids^2, na.rm=TRUE))
  
  # Calculate log-likelihood for spa vs sim
  resids <- comp_df$sim - comp_df$spa
  nll_spa_sim <- -(-length(comp_df$sim)/2 * log(2*pi*var(resids, na.rm=TRUE)) - 1/(2*var(resids, na.rm=TRUE)) * sum(resids^2, na.rm=TRUE))
  
  ## Calculate percent bias
  ## Note: This measure of percent bias aggregates all prediction errors, 
  ## both positive and negative, into a single number. This can mask the 
  ## variability of the errors across different observations.
  pbi_spa_obs <- 100 * (sum(comp_df$spa - comp_df$obs, na.rm=TRUE) / sum(comp_df$obs,na.rm=TRUE))
  pbi_sim_obs <- 100 * (sum(comp_df$sim - comp_df$obs, na.rm=TRUE) / sum(comp_df$obs,na.rm=TRUE))
  pbi_spa_sim <- 100 * (sum(comp_df$spa - comp_df$sim, na.rm=TRUE) / sum(comp_df$sim,na.rm=TRUE))
  
  ## Calculate mean absolute error (MAE) ---------------------------------------
  ## Average magnitude of errors between the predictions and observations, 
  ## treating all errors with equal weight regardless of their size.
  mae_spa_obs = mean(abs(comp_df$spa - comp_df$obs), na.rm = TRUE)
  mae_sim_obs = mean(abs(comp_df$sim - comp_df$obs), na.rm = TRUE)
  mae_spa_sim = mean(abs(comp_df$spa - comp_df$sim), na.rm = TRUE)
  
  ## Calculate root mean absolute error (RMSE) ---------------------------------
  rmse_spa_obs <- sqrt(mean((comp_df$spa - comp_df$obs)^2, na.rm = TRUE))
  rmse_sim_obs <- sqrt(mean((comp_df$sim - comp_df$obs)^2, na.rm = TRUE))
  rmse_spa_sim <- sqrt(mean((comp_df$spa - comp_df$sim)^2, na.rm = TRUE))
  
  ## Store calculations
  fit <-  c(nll_spa_obs, nll_sim_obs, nll_spa_sim, 
            pbi_spa_obs, pbi_sim_obs, pbi_spa_sim,
            mae_spa_obs, mae_sim_obs, mae_spa_sim); fit
  
  fit_metrics[i,] <- fit
}
rownames(fit_metrics) = fg_df$group_name
fit_metrics = round(fit_metrics, 2)
(fit_sums <- colSums(fit_metrics, na.rm = TRUE))

  ## 
  