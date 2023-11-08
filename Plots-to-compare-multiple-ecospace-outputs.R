## Naming conventions
## 'sim' --> Related to Ecosim
## 'spa' --> Related to Ecospace
## 'obs' --> Related to observed timeseries data, i.e., that Ecosim was fitted to. 
## 'B'   --> Denotes biomass
## 'C'   --> Denotes catch

## Setup -----------------------------------------------------------------------
rm(list=ls()); graphics.off(); gc()
source("./functions.R") ## Pull in functions
library(dplyr)

## Input set up ----------------------------------------------------------------
dir_pdf_out  = "./PDF_plots/"
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
#plot_name_C  = paste0("C_", spa_scenario, out_file_notes)
num_plot_pages = 4 ## Sets number of pages for PDF file

## Set scaling parameters 
#init_years_toscale = 1
init_years_toscale  = 2016-1980 ## 36. This will scale all outputs to the global average
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
#simB_xM <- read.csv(paste0(dir_sim, "biomass_monthly.csv"), skip = num_skip_sim); simB_xM$timestep.group = NULL
#simC_xM <- read.csv(paste0(dir_sim, "catch_monthly.csv"), skip = num_skip_sim); simC_xM$timestep.group = NULL
#spaB_xM <- read.csv(paste0(dir_spa, "Ecospace_Average_Biomass.csv"), skip = num_skip_spa, header = TRUE); spaB_xM$TimeStep = NULL
#rownames(spaB_xM) = rownames(simB_xM) = ym_series


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
## Plot biomasses
## Note: Make sure PDF readers are closed before running pdf()

## -----------------------------------------------------------------------------
## Plot and compare ANNUAL biomass 

## Plotting parameters
col_obs = 'black'
col_sim = rgb(0.2, 0.7, .1, alpha = 0.6) ## rgb (red, green, blue, alpha)
#col_spa <- colorRampPalette(c("deepskyblue", "blue1", "blue4", "purple4", "darkviolet"))(length(spaB_scaled_ls))
#col_spa <- rainbow(length(spaB_scaled_ls))
col_spa <- c("darkgoldenrod", "indianred2", "steelblue4", "darkorchid4")
#col_spa <- adjustcolor(col_spa, alpha.f = 1) ## Adjust transparance

x = year_series
num_plot_pages = 4; x_break = 5; y_break = 4; x_cex = 0.9; y_cex = 0.9; x_las = 2;
sim_lty = 1; spa_lty = 1
sim_lwd = 2; spa_lwd = 1; obs_pch = 16; obs_cex = 0.8;
main_cex = 0.85; leg_cex = 0.9; leg_pos = 'topleft';leg_inset = 0.1
#simB_scaled = spaB_scaled_ls

(dir_pdf_out_xY <- paste0(dir_pdf_out, plot_name_xY, ".PDF"))
pdf(dir_pdf_out_xY, onefile = TRUE)
  
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
    obsB_scaled=NULL
    if(i %in% obsB.head$pool_code){
      obs.idx     = which(obsB.head$pool_code==i)
      obs_df      = suppressWarnings( ## Suppress warnings thrown when obs not available
        data.frame(year_series, obsB = as.numeric(obsB[ ,obs.idx]))
      )
      obsB_scaled = obs_df$obsB / mean(na.omit(obs_df$obsB)[1:init_years_toscale], na.rm = TRUE)
      rm(obs_df)
      rm(obs.idx)
    }
    
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

  
  
  
  