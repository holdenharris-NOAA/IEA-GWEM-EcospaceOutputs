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
spa_scenarios  = c("spa_ST00_base-no-drivers", "spa_ST01a_surf-sal", 
                   "spa_ST01b_temp", "spa_ST01c_PP-MODIS", 
                   "spa_ST01c_PP-MOM6")
spa_scen_names = c("01 No drivers",  "02 Salinity", 
                   "03 Temperature", "04 PP (MODIS)",
                   "05 PP (MOM6)")


## User-defined output parameters ----------------------------------------------
num_plot_pages = 4 ## Sets number of pages for PDF file
today_date <- format(Sys.Date(), "%Y-%m-%d")
out_file_notes = "test-STEdrivers"
init_years_toscale = 6 ## In plotting, this sets the "1 line" to the average of this number of years

## Set up output folders -------------------------------------------------------
dir_out <- "./Scenario_comps/Test_EnvDrivers/" ## Folder where outputs will be stored
if (!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE) ## Create the folder if it doesn't exist

## Plot output names
dir_pdf_out  = paste0(dir_out)  ## Folder for plots
dir_tab_out  = paste0(dir_out) ## Folder for tables with fit metrics
if (!dir.exists(dir_pdf_out)) dir.create(dir_pdf_out, recursive = TRUE) ## Create the folder if it doesn't exist
if (!dir.exists(dir_tab_out)) dir.create(dir_tab_out, recursive = TRUE) ## Create the folder if it doesn't exist

folder_name <- paste0(dir_tab_out, "Scaled_", init_years_toscale, "y") ## Folder name based on `init_years_toscale`
(plot_name_xY = paste0("BxY_scaled_", init_years_toscale, "y-", out_file_notes, ".PDF"))
pdf_file_name= paste0(dir_pdf_out, plot_name_xY)


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
  #simB_xM <- read.csv(paste0(dir_sim, "biomass_monthly.csv"), skip = num_skip_sim); simB_xM$timestep.group = NULL
  #simC_xM <- read.csv(paste0(dir_sim, "catch_monthly.csv"), skip = num_skip_sim); simC_xM$timestep.group = NULL
  #spaB_xM <- read.csv(paste0(dir_spa, "Ecospace_Average_Biomass.csv"), skip = num_skip_spa, header = TRUE); spaB_xM$TimeStep = NULL
  #rownames(spaB_xM) = rownames(simB_xM) = ym_series
  
  ## -----------------------------------------------------------------------------
  ##
  ## Read in OBSERVED timeseries -----------------------------------------------
  dir_obs = paste0("./", ewe_name, "/", obs_TS_name, ".csv")
  obs.list = f.read_ecosim_timeseries(dir_obs, num_row_header = 4)
  for(i in 1:length(obs.list)){assign(names(obs.list)[i],obs.list[[i]])} #make separate dataframe for each list element
  
  obsB.head <- merge(obsB.head, fg_df, by = "pool_code", all.x = TRUE)
  obsC.head <- merge(obsC.head, fg_df, by = "pool_code", all.x = TRUE)
  
  colnames(obsB) = obsB.head$group_name
  colnames(obsC) = obsC.head$group_name
  
  ## ---------------------------------------------------------------------------
  ##
  ## Get weights
  weights <- read.csv(dir_obs, nrows = 3) ## Read in header from TS file
  weights <- as.data.frame(t(weights)); rownames(weights) = NULL ## Transpose to long
  colnames(weights) = c("weight", "pool_code", "type"); weights <- weights[-1, ] ## Make column names the first column
  weights$type <- as.integer(weights$type)
  weights$pool_code <- as.integer(weights$pool_code)
  weights$weight <- as.numeric(weights$weight)
  weights <- subset(weights, weights$type == 0); weights$type <- NULL
  
  ## Merge weights into `fg_df` and set NAs to 1. 
  fg_weights <- merge(fg_df, weights, by = "pool_code", all.x = TRUE)
  fg_weights$weight[is.na(fg_weights$weight)] <- 1
  
  ## ---------------------------------------------------------------------------
  ##
  ## OBJECTIVE COMPARISON METRICS
  ## Create and run objective functions for multiple Ecospace scenarios
  ## Compare fits to observed data and Ecosim results
  
  fit_metrics_ls = list() ##
  spa_fit_sums = data.frame()
  
  ## Loop through each Ecospace scenario ------------------------------------------
  ##
  for(j in 1:length(spa_scenarios)){
    
    fit_metrics <- data.frame(
      ssr_spa_obs=NA, ssr_spa_sim=NA, ssr_sim_obs=NA, 
      #pbi_spa_obs=NA, pbi_spa_sim=NA, pbi_sim_obs=NA, 
      mae_spa_obs=NA, mae_spa_sim=NA, mae_sim_obs=NA)
    
    ## -----------------------------------------------------------------------------
    ## 
    ## Loop through every functional group to make the `fit_metrics` data frame
    for(i in 1:num_fg){
      
      (grp  = fg_df$group_name[i])
      
      ## Get biomass for individual FG: Observed, Ecosim, and Ecospace -----------
      ## Scale to the average of a given timeframe 
      
      ## Ecospace
      spaB_ls <- lapply(ls_spaB_xY, function(df) df[, i]) ## Extract the i column from each data frame in the list
      spaB               <- spaB_ls[[j]] ## Pull from the j'th scenario
      spaB_scaled        <- spaB / mean(spaB[1:init_years_toscale], na.rm = TRUE)
      
      ## Ecosim
      simB = simB_xY[,i] 
      simB_scaled = simB / mean(simB[1:init_years_toscale], na.rm = TRUE) ## Ecosim scaled
      
      ## Observed
      ## For observed, there maybe not be reference data so we check to see if observed data is available
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
        obsB_i      = obs_df$obsB
      } else {  ## Make NAs if there is no observed data     
        obsB_scaled = rep(NA, length(simB))
        obsB_i      = rep(NA, length(simB)) 
      }
        
      ## Create data frame to compare observed, Ecosim, and Ecospace
      ##
      comp_df <- data.frame(obs = obsB_i, sim = simB, spa = spaB)
      
      ## Calculate raw sum of squared residuals (SSR) ------------------------------
      ## commonly used metric for evaluating the goodness of fit in many models, 
      ## especially in cases where we're not specifically concerned with the statistical properties 
      ## of the residuals (like in NLL, which assumes normally distributed errors).
      raw_ssr_spa_obs <- sum((comp_df$obs - comp_df$spa)^2, na.rm = TRUE) ## SSR for spa vs obs
      raw_ssr_spa_sim <- sum((comp_df$sim - comp_df$spa)^2, na.rm = TRUE) ## SSR for spa vs sim
      raw_ssr_sim_obs <- sum((comp_df$obs - comp_df$sim)^2, na.rm = TRUE) ## SSR for sim vs obs
      
      ## SSR for log biomasses
      ## This is similiar to how goodness of fit is calculated in Ecosim
      ssr_spa_obs <- sum((log(comp_df$obs + 1) - log(comp_df$spa + 1))^2, na.rm = TRUE) ## SSR for log(obs + 1) vs log(spa + 1)
      ssr_spa_sim <- sum((log(comp_df$sim + 1) - log(comp_df$spa + 1))^2, na.rm = TRUE) ## SSR for log(sim + 1) vs log(spa + 1)
      ssr_sim_obs <- sum((log(comp_df$obs + 1) - log(comp_df$sim + 1))^2, na.rm = TRUE) ## SSR for log(obs + 1) vs log(sim + 1)
      
      ## Calculate percent bias ------------------------------------------------
      ## Note: This measure of percent bias aggregates all prediction errors, 
      ## both positive and negative, into a single number, which may mask the 
      ## variability of the errors across different observations.
      pbi_spa_obs <- 100 * (sum(comp_df$spa - comp_df$obs, na.rm=TRUE) / sum(comp_df$obs,na.rm=TRUE))
      pbi_spa_sim <- 100 * (sum(comp_df$spa - comp_df$sim, na.rm=TRUE) / sum(comp_df$sim,na.rm=TRUE))
      pbi_sim_obs <- 100 * (sum(comp_df$sim - comp_df$obs, na.rm=TRUE) / sum(comp_df$obs,na.rm=TRUE))
      
      ## Calculate mean absolute error (MAE) ---------------------------------------
      ## Average magnitude of errors between the predictions and observations, 
      ## treating all errors with equal weight regardless of their size.
      mae_spa_obs = mean(abs(comp_df$spa - comp_df$obs), na.rm = TRUE)
      mae_spa_sim = mean(abs(comp_df$spa - comp_df$sim), na.rm = TRUE)
      mae_sim_obs = mean(abs(comp_df$sim - comp_df$obs), na.rm = TRUE)
      
      ## Calculate root mean absolute error (RMSE) ---------------------------------
      rmse_spa_obs <- sqrt(mean((comp_df$spa - comp_df$obs)^2, na.rm = TRUE))
      rmse_spa_sim <- sqrt(mean((comp_df$spa - comp_df$sim)^2, na.rm = TRUE))
      rmse_sim_obs <- sqrt(mean((comp_df$sim - comp_df$obs)^2, na.rm = TRUE))
      
      ## Store calculations
      fit <-  c(ssr_spa_obs, ssr_spa_sim, ssr_sim_obs,         ## NLL
#                pbi_spa_obs, pbi_spa_sim, pbi_sim_obs,        ## Percent bias
                mae_spa_obs, mae_spa_sim, mae_sim_obs); fit    ## Mean avg error
      
      fit_metrics[i,] <- fit
    }
    
    ## Make pretty
    rownames(fit_metrics) = fg_df$group_name
    fit_metrics = round(fit_metrics, 2)
    
    ## Weighting, summing, and saving ------------------------------------------
    fit_sums          <- as.data.frame(t(colSums(fit_metrics, na.rm = TRUE)))
    fit_sums$weighting <- "none"
    
    ## Incorporate weights via element-wise multiplication
    weighted_fits <- cbind(fg_weights$weight, fit_metrics * fg_weights$weight)
    weighted_fit_sums  <- colSums(weighted_fits[,2:ncol(weighted_fits)], na.rm = TRUE)
    weighted_fit_sums <- as.data.frame(t(weighted_fit_sums))
    weighted_fit_sums$weighting <- "weighted"
    
    ## Square-root weighting
    sqrt_weighted_fits <- cbind(fg_weights$weight, fit_metrics * sqrt(fg_weights$weight))
    sqrt_weighted_fit_sums <- colSums(sqrt_weighted_fits[,2:ncol(weighted_fits)], na.rm = TRUE)
    sqrt_weighted_fit_sums <- as.data.frame(t(sqrt_weighted_fit_sums))
    sqrt_weighted_fit_sums$weighting <- "root-weighted"
  
    ## Same to running list
    scen_fit_sums <- data.frame(scenario = spa_scen_names[j])
    scen_fit_sums <- cbind(scen_fit_sums,
                           rbind(fit_sums, weighted_fit_sums, sqrt_weighted_fit_sums))
    
    spa_fit_sums <- rbind(spa_fit_sums, scen_fit_sums)
    fit_metrics_ls[[j]] = round(weighted_fits, 2)
    #fit_metrics_ls[[j]] = round(fit_metrics, 2)
  }; spa_fit_sums
  
  ## Extract of Ecosim fits and append to make one table -------------
  ## Remove Ecosim columns and add them as scenarios for final table -----------

  ## Extract the Ecosim fits
    ecosim_df <- spa_fit_sums %>% 
       select(weighting, ssr_sim_obs, mae_sim_obs) %>%
       distinct() %>% # Ensure we select distinct/unique rows
       mutate(scenario = "00 Ecosim", # Add the scenario column with "Ecosim"
                          ssr_spa_obs = NA,  # Set other columns to NA
                          ssr_spa_sim = NA,
                          mae_spa_obs = NA,
                          mae_spa_sim = NA) %>%
      select(scenario, everything()) # Ensure 'scenario' is the first column
   
  ## Append, arrange by weighting, and round
     spa_fit_sums_expanded <- 
       rbind(spa_fit_sums, ecosim_df) %>%
       arrange(weighting, scenario) %>%
       mutate_if(is.numeric, round) ## Round to whole number
   
  ## Move "Ecosim" values and remove unwanted columns
     spa_fit_sums_updated <- spa_fit_sums_expanded %>%
       mutate(ssr_spa_obs = if_else(scenario == "00 Ecosim", ssr_sim_obs, ssr_spa_obs), # Move values for Ecosim
                         mae_spa_obs = if_else(scenario == "00 Ecosim", mae_sim_obs, mae_spa_obs)) %>%
       select(-ssr_sim_obs, -mae_sim_obs) %>% # Remove the columns
       arrange(weighting, scenario) 
     
     spa_fit_sums_updated

  ## Write out tables as CSV Files
  write.csv(spa_fit_sums_updated, file = paste0(dir_tab_out, "Ecospace-fits-summed-" , out_file_notes, ".csv"), row.names = FALSE)
  
  ## Also, write out as an Excel file with each scenario as a different Tab
  library(openxlsx)
  wb <- createWorkbook() # Create a new workbook
  
  replace_NaN <- function(x) {
    x[sapply(x, is.nan)] <- NA
    return(x)
  }
  
  for (j in seq_along(fit_metrics_ls)) { # Loop through the list of data frames and add each as a new sheet
    sheet_name <- spa_scen_names[j]   # Create a sheet name based on the names in spa_scen_names
    addWorksheet(wb, sheet_name) # Add a sheet to the workbook with the data frame
    writeData(wb, sheet_name, replace_NaN(fit_metrics_ls[[j]]), na.string = "NA", rowNames = TRUE)
  }
  
  saveWorkbook(wb, paste0(dir_tab_out, "Fit_metrics_", out_file_notes, ".xlsx"), overwrite = TRUE) # Save the workbook as an Excel file
  
  ## -----------------------------------------------------------------------------
  ##
  ## Plot biomasses
  ## Note: Make sure PDF readers are closed before running pdf()

  ## Plotting parameters
  col_obs = 'black'
  col_sim = rgb(0.2, 0.7, .1, alpha = 0.6) ## rgb (red, green, blue, alpha)
  col_spa <- c("darkgoldenrod", "indianred2", "steelblue4", "darkorchid4", "darkgreen")
  #col_spa <- adjustcolor(col_spa, alpha.f = 1) ## Adjust transparancy
  
  x = year_series
  num_plot_pages = 4; x_break = 5; y_break = 4; x_cex = 0.9; y_cex = 0.9; x_las = 2;
  sim_lty = 1; spa_lty = 1
  sim_lwd = 2; spa_lwd = 1; obs_pch = 16; obs_cex = 0.8;
  main_cex = 0.85; leg_cex = 0.9; leg_pos = 'topleft';leg_inset = 0.1
  #simB_scaled = spaB_scaled_ls
  
  print(paste("Writing", pdf_file_name))
  pdf(pdf_file_name, onefile = TRUE)
  
  ## Set number of plots per page
  set.mfrow = f.get_plot_dims(x=num_fg / num_plot_pages, round2=4)
  par(mfrow=set.mfrow, mar=c(1, 2, 1, 2))
  plots_per_pg = set.mfrow[1] * set.mfrow[2]
  
  for(i in 1:num_fg){
    #  for(i in 1:19){
    grp  = fg_df$group_name[i]
    simB = simB_xY[,i] 
    spaB_ls <- lapply(ls_spaB_xY, function(df) df[, i]) ## Extract the i column from each data frame in the list
    
    ## Check to see if observed data is available
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
    simB_scaled = simB / mean(simB[1:init_years_toscale], na.rm = TRUE)
    spaB_scaled_ls = list()
    for(j in 1:length(spa_scenarios)){
      spaB               <- spaB_ls[[j]]
      spaB_scaled        <- spaB / mean(spaB[1:init_years_toscale], na.rm = TRUE)
      spaB_scaled_ls[[j]] <- spaB_scaled
    }
    
    ##-------------------------------------------------------------------------------  
    ## PLOT 
    
    ## Legend plots -------------------------------------------
    if(i %in% seq(1, num_fg, by = plots_per_pg-1)) {
      plot(0, 0, type='n', xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', 
           xlab='', ylab='', bty='n') # Create an empty plot
      legend(leg_pos, inset = 0.1, bg="gray90", box.lty = 0,
             legend=c('Observed','Ecosim', spa_scen_names),
             lty = c(NA, sim_lty, rep(spa_lty, length(spaB_scaled_ls))), 
             lwd = c(NA, sim_lwd+1, rep(spa_lwd+1, length(spaB_scaled_ls))),
             pch=c(obs_pch, NA, rep(NA, length(spaB_scaled_ls))), 
             col =c(col_obs, col_sim, col_spa), 
             cex = leg_cex)
    }
    
    ## Data plots -------------------------------------------
    ## Determine y-axis range and initialize plot
    min = min(obsB_scaled, simB_scaled, unlist(spaB_scaled_ls), na.rm=T) * 0.8
    max = max(obsB_scaled, simB_scaled, unlist(spaB_scaled_ls), na.rm=T) * 1.2
    plot(x, rep("", length(x)), type='b', 
         ylim = c(min, max), xaxt = 'n', yaxt = 'n',
         xlab = '', ylab='', bty = 'n')
    title(main = grp, line=-.6, cex.main = main_cex) ## Add title
    
    ## Get years from date series
    posx = as.POSIXlt(date_series)
    x_years = unique(posx$year + 1900)
    end_y = max(x_years)
    start_y = min(x_years)
    
    ## Setup X-axis
    year_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 year")
    num_breaks_x <- round((end_y - start_y) / x_break) ## Determine x-axis breaks
    x_ticks <- pretty(x, n = num_breaks_x)
    xlab = paste0("'", substring(format(x_ticks, "%Y"), nchar(format(x_ticks, "%Y")) - 1))
    axis(1, at = x_ticks, labels = xlab, cex.axis = x_cex, las = x_las)
    
    ## Setup Y-axis
    y_ticks = pretty(seq(min, max, by = (max-min)/10), n = y_break)
    axis(2, at = y_ticks, labels = y_ticks, las = 1, cex.axis = y_cex)
    abline(h=1, col='lightgray')
    
    ## Plot outputs: Ecosim (green line), Ecospace (blue line), Observed (black dots)
    if(length(obsB_scaled)>0) points(year_series, obsB_scaled, pch=16, cex=obs_cex, col = col_obs) ## Plot observed data, if it's present
    lines(x, simB_scaled, lty=sim_lty, lwd = sim_lwd,  col = col_sim) ## Plot Ecosim
    if(is.list(spaB_scaled_ls)) {     ## If it's a list, loop through each element and plot
      for(j in seq_along(spaB_scaled_ls)) {
        lines(x, spaB_scaled_ls[[j]], lty=spa_lty, lwd=spa_lwd, col=col_spa[j]) # Plot each Ecospace projection. Use the j-th color in the palette for each line.
      }
      #} else if(is.list(spaB_scaled_ls)==FALSE) { # If it's not a list, but a vector, plot directly
      #  lines(x, spaB_scaled, lty=1, lwd=spa_lwd, col=col_spa[1]) # Plot Ecospace
    }
  }
  dev.off()    