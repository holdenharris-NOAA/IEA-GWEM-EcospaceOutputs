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
dir_pdf_out = "./PDF_plots/"
ewe_name = "EwE_Outputs"
sim_scenario = "sim-spa_01"
spa_scenario = "spa_00"
obs_TS_name  = "TS_updated_IB13"
#num_skip_spa = 32
#num_skip_sim = 13

## User-defined output parameters ----------------------------------------------
today_date <- format(Sys.Date(), "%Y-%m-%d")
out_file_notes = ""
plot_name_xY = paste0("Biomass_scaled_xY_", spa_scenario, out_file_notes)
plot_name_xM = paste0("Biomass_scaled_xM_", spa_scenario, out_file_notes)
num_plot_pages = 4 ## Sets number of pages for PDF file

## Set scaling parameters 
#init_years_toscale = 1
init_years_toscale  = 2016-1980 ## 36. This will scale all outputs to the global average
init_months_toscale = init_years_toscale * 12

## -----------------------------------------------------------------------------
##
## Read-in ANNUAL Observed, Ecosim, and Ecospace TS
dir_sim = paste0("./", ewe_name, "/ecosim_", sim_scenario, "/")
dir_spa = paste0("./", ewe_name, "/", spa_scenario, "/")

## Read-in Ecosim annual biomass 
filename = paste0(dir_sim, "biomass_annual.csv")
num_skip_sim = f.find_start_line(filename, flag = 1980)

simB_xY <- read.csv(paste0(dir_sim, "biomass_annual.csv"), skip = num_skip_sim)
years = simB_xY$year.group ## Get date range from Ecosim
simB_xY$year.group = NULL

## Read-in Ecosim annual catches
simC_xY <- read.csv(paste0(dir_sim, "catch_annual.csv"), skip = num_skip_sim)
simC_xY$year.group = NULL

## Read-in Ecospace annual biomass and catches ---------------------------------
filename <- paste0(dir_spa, "Ecospace_Annual_Average_Biomass.csv")
num_skip_spa <- f.find_start_line(filename)
         
spaB_xY <- read.csv(paste0(dir_spa, "Ecospace_Annual_Average_Biomass.csv"), 
                    skip = num_skip_spa, header = TRUE)
spaB_xY$Year = NULL

## Read-in Ecospace annual catch
spaC_xY <- read.csv(paste0(dir_spa, "Ecospace_Annual_Average_Catch.csv"), 
                    skip = num_skip_spa, header = TRUE)
spaC_xY$Year = NULL

## Standardize FG names --------------------------------------------------------
fg_names = f.standardize_group_names(colnames(spaB_xY))
num_fg = length(fg_names)

## Make dataframe of functional groups
fg_df <- data.frame(
  pool_code  = 1:num_fg,
  group_name = paste(sprintf("%02d", 1:num_fg),
                     gsub("_", " ", fg_names))
)

## -----------------------------------------------------------------------------
## Read-in MONTHLY data

## Prepare months and date series objects
start_y <- min(years)
end_y   <- max(years)
date_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 month")
year_series <- seq(as.Date(paste0(start_y, "-01-01")), as.Date(paste0(end_y,   "-12-01")), by = "1 year")
ym_series <- format(date_series, "%Y-%m")

## Read in MONTHLY biomasses
## Ecosim
simB_xM <- read.csv(paste0(dir_sim, "biomass_monthly.csv"), skip = num_skip_sim)
simB_xM$timestep.group = NULL

simC_xM <- read.csv(paste0(dir_sim, "catch_monthly.csv"), skip = num_skip_sim)
simC_xM$timestep.group = NULL

## Ecospace -------------------------------------------------------------------
spaB_xM <- read.csv(paste0(dir_spa, "Ecospace_Average_Biomass.csv"), 
                    skip = num_skip_spa, header = TRUE)
spaB_xM$TimeStep = NULL

spaC_xY <- read.csv(paste0(dir_spa, "Ecospace_Average_Catch.csv"), 
                    skip = num_skip_spa, header = TRUE)
spaC_xY$Year = NULL

## Set row and column names
rownames(spaB_xY) = rownames(simB_xY) = years
rownames(spaB_xM) = rownames(simB_xM) = ym_series
colnames(spaB_xY) = colnames(simB_xY) = colnames(spaB_xM) = colnames(simB_xM) = fg_df$group_name

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
pdf(paste0(dir_pdf_out, plot_name_xY, ".PDF"), onefile = TRUE)
## Set number of plots per page
set.mfrow = f.get_plot_dims(x=num_fg / num_plot_pages, round2=4)
par(mfrow=set.mfrow, mar=c(1, 2, 1, 2))

for(i in 1:num_fg){
  #i=6
  grp  = fg_df$group_name[i]
  spaB = spaB_xY[,i] 
  simB = simB_xY[,i] 
  
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
  spaB_scaled = spaB / mean(spaB[1:init_years_toscale], na.rm = TRUE)
  simB_scaled = simB / mean(simB[1:init_years_toscale], na.rm = TRUE)
  
  ## Plot colors
  col_obs = 'black'
  col_sim = rgb(0.2, 0.7, .1, alpha = 0.9) ## rgb (red, green, blue, alpha)
  col_spa = rgb(0.1, 0, 1, alpha = 0.8) 
  
  ## Call plotting function
  f.plot_outputs_obs_sim_spa(x = year_series, grp = grp, 
                             spaB_scaled = spaB_scaled, simB_scaled = simB_scaled, 
                             obsB_scaled = obsB_scaled, 
                             col_spa = col_spa, col_sim = col_sim, col_obs = col_obs,
                             num_fg = length(fg_names), num_plot_pages = 4, 
                             sim_lwd = 2, spa_lwd = 2)
}
dev.off()    

## -----------------------------------------------------------------------------
## Plot and compare MONTHLY biomass 
pdf(paste0(dir_pdf_out, plot_name_xM, ".PDF"), onefile = TRUE)
## Set number of plots per page
set.mfrow = f.get_plot_dims(x=num_fg / num_plot_pages, round2=4)
par(mfrow=set.mfrow, mar=c(1.2, 2, 1.2, 2))

for(i in 1:num_fg){
  #i=6
  grp = fg_df$group_name[i]
  spaB = spaB_xM[,i] 
  simB = simB_xM[,i] 
  
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
  spaB_scaled = spaB / mean(spaB[1:init_months_toscale], na.rm = TRUE)
  simB_scaled = simB / mean(simB[1:init_months_toscale], na.rm = TRUE)
  
  ## Plot colors
  col_obs = 'black'
  col_sim = rgb(0.2, 0.7, .1, alpha = 0.8) ## rgb (red, green, blue, alpha)
  col_spa = rgb(0.1, 0, 1, alpha = 0.6) 
  
  ## Call plotting function
  f.plot_outputs_obs_sim_spa(x = date_series, grp = grp, 
                             spaB_scaled = spaB_scaled, simB_scaled = simB_scaled, obsB_scaled = obsB_scaled, 
                             col_spa = col_spa, col_sim = col_sim, col_obs = col_obs,
                             num_fg = length(fg_names), num_plot_pages = 4, 
                             sim_lwd = 1, spa_lwd = 1)
}
dev.off()    

## -----------------------------------------------------------------------------
##
## Read-in and prepare Ecospace catches

## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

spaC_xY = lapply(dir_spa, FUN=function(x)read.csv(paste0(dir_spa, "/Ecospace_Annual_Average_Catch.csv"),skip=32,header=T))
spaC_xY = lapply(spaC_xY, "[", -1)
spaC_xY.a = array(unlist(spaC_xY),dim=c(dim(spaC_xY[[1]]),length(spaC_xY)))
idx.nm = unlist(gregexpr('\\.\\.\\.\\.', names(spaC_xY[[1]])))+4
spaC_xY.grps = substr(names(spaC_xY[[1]]), idx.nm, 1000)
spaC_xY.grps = f.standardize_group_names(spaC_xY.grps) ## Correct group names
spaC_xY.grpnum = as.numeric(fg_df$pool_code[match(spaC_xY.grps,gsub("/","_", fg_df$group_name))])
print(cbind(spaC_xY.grps,spaC_xY.grpnum))
spaC_xY.a2 = array(NA,dim=c(dim(spaC_xY.a)[1],length(unique(spaC_xY.grps)),dim(spaC_xY.a)[3]),
                   dimnames=list(years,sort(unique(spaC_xY.grps)), paste0('run',1:dim(spaC_xY.a)[3])))
for(i in 1:dim(spaC_xY.a)[3]){
  #i=1
  tmp = aggregate(.~spaC_xY.grps, data=as.data.frame(t(spaC_xY.a[,,i])),sum)
  arr = as.matrix(t(tmp[,-1]))
  spaC_xY.a2[,,i] = arr
  rm(tmp,arr)
}


## Plot catches 
set.mfrow = f.get_plot_dims(x=nrow(obsC.head),round2=4)
set.mfrow = f.get_plot_dims(x=num_fg / num_plot_pages, round2=4)
par(mfrow=set.mfrow,mar=c(1,3,1,1))
#for(i in 1:nrow(obsC.head)){

for(i in 1:2){
  #i=1
  grp.num = obsC.head$pool_code[i]
  grp = fg_names[grp.num]
  spaC.idx = grep(grp, dimnames(spaC_xY.a2)[[2]], ignore.case = TRUE) ## Pulls all indexes of catching that group
  #spaC.idx = which(dimnames(spaC_xY.a2)[[2]]==grp)
  #spaC.idx = which(unique(spaC_xY.a2.grpnum)==grp.num)
  spaC  = spaC_xY.a2[ , spaC.idx, ]
  obsC = obsC[,i]
  
  #scale obsC biomass to bestfit (last) iteration
  q = mean(spaC, na.rm=T) / mean(obsC,na.rm=T)
  obsC = obsC*q
  
  plot(years, obsC, pch=16, ylim=c(min(obsC,spaC,na.rm=T)*0.8, max(obsC, spaC,na.rm=T)*1.2), 
       cex=0.8,xlab='Year',ylab='Catch', bty='n')
  if(is.matrix(spaC)){
    matlines(years, spaC, lty=1, col=c(rep('gray', ncol(spaC)-1),'blue'))
  }
  if(is.vector(spaC)){
    lines(years,spaC,lty=1,col='blue')
  }
  title(main=grp,line=-.6,cex.main=0.9)
}
