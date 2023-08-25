#Plot-outputs-ecosim-ecospace-and-observed
# R Code Description

**Naming Conventions**
   * `sim` - Related to Ecosim
   * `spa` - Related to Ecospace
   * `obs` - Related to observed timeseries data, i.e., that Ecosim was fitted to.
   * `B` - Denotes biomass
   * `C` - Denotes catch

### Set input and output names
The user will set these to the output directories from EwE
```R
## Input set up ----------------------------------------------------------------
dir_pdf_out  = "./PDF_plots/"
ewe_name     = "EwE_Outputs"
sim_scenario = "sim-spa_01"
spa_scenario = "spa_01"
obs_TS_name  = "TS_updated_IB13"
srt_year     = 1980

## User-defined output parameters ----------------------------------------------
out_file_notes = ""
plot_name_xY = paste0("Biomass_scaled_xY_", spa_scenario, out_file_notes)
plot_name_xM = paste0("Biomass_scaled_xM_", spa_scenario, out_file_notes)
plot_name_C  = paste0("Catches_", spa_scenario, out_file_notes)
num_plot_pages = 4 ## Sets number of pages for PDF file
```
### Set scaling parameters
This sets the years to scale. This can be set to scale to the first year (or initial years) to give outputs similiar to Ecosim, or set to the whole time period which normalizes the outputs. 
```R
init_years_toscale  = 2016-1980 ## 36. This will scale all outputs to the global average
init_months_toscale = init_years_toscale * 12
```
### Read-in ANNUAL Observed, Ecosim, and Ecospace timeseries
- The directory names are defined by the input set up, above. 
- Note that `num_skip_sim` and `num_skip_space` are determined with `f.find_start_line(filename, flag = srt_year)`
- Functional group names are standardized with `f.standardize_group_names(colnames(spaB_xY))`

### Read-in MONTHLY Observed, Ecosim, and Ecospace timeseries
- Similar to reading in annual timeseries, although months need to be denoted with `as.Date` objects
- Observed timeseries are made by making separate data farmes for each list element
```R
## Read in "observed" timeseries -----------------------------------------------
dir_obs = paste0("./", ewe_name, "/", obs_TS_name, ".csv")
obs.list = f.read_ecosim_timeseries(dir_obs, num_row_header = 4)
for(i in 1:length(obs.list)){assign(names(obs.list)[i],obs.list[[i]])} #make separate dataframe for each list element
obsB.head <- merge(obsB.head, fg_df, by = "pool_code", all.x = TRUE)
obsC.head <- merge(obsC.head, fg_df, by = "pool_code", all.x = TRUE)
colnames(obsB) = obsB.head$group_name
colnames(obsC) = obsC.head$group_name
```
## **Plotting Biomass**
Here, biomasses are plotted. PDF readers should be closed before running the PDF creation function.
   * **Annual Biomass**: Biomass data is scaled, colors for the plot are set, and a plotting function is called.
   * **Monthly Biomass**: Similarly to the annual data, monthly biomass is scaled and plotted.
```R
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
```

## **Ecospace Catches**
- Ecospace catches are read in and processed.
```R
## Read-in and prepare Ecospace catches
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
```
- Catches are plotted and saved in a PDF file, handling unique fleets and their corresponding colors.
```R
## Plot catches ----------------------------------------------------------------
pdf(paste0(dir_pdf_out, plot_name_C, ".PDF"), onefile = TRUE)
  set.mfrow = round(f.get_plot_dims(x = num_catches, round2=2)/2)
  plots_per_pg = set.mfrow[1] * set.mfrow[2]
  par(mfrow=set.mfrow, mar=c(1,3,1,1))
  
  for(i in 1:num_catches){
    #i=17
    if(i %in% seq(1, num_catches, by = plots_per_pg-1)) {
      plot(0, 0, type='n', xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', 
           xlab='', ylab='', bty='n') # Create an empty plot
      legend('top', legend = fleet_cols$fleet_out, bty = 'n',
             col=fleet_cols$colors, lwd =3, cex=0.6)
    }
    
    grp.num = obsC.head$pool_code[i]
    grp = fg_names[grp.num]
    spaC.idx = grep(grp, c_names, ignore.case = TRUE) ## Pulls all indexes of catching that group
    i_spaC  = spaC_xY.a2[ , spaC.idx, ]
    i_obsC = obsC[,i]
    
    #scale i_obsC biomass to bestfit (last) iteration
    q = mean(i_spaC, na.rm=T) / mean(i_obsC,na.rm=T)
    i_obsC = i_obsC*q
    
    ## Get fleet colors
    i_fleets = colnames(i_spaC)
    i_fleet_prefix <- unique(sapply(strsplit(i_fleets, "__"), `[`, 1)) ## Extract the prefix from the i_fleets vector
    matched_colors <- fleet_cols$colors[match(i_fleet_prefix, fleet_cols$fleet_name)] ## # Match the fleet_prefix to fleet_name and get the corresponding color
    
    ## Make plot
    plot(years, i_obsC, pch=16, ylim=c(min(i_obsC,i_spaC,na.rm=T)*0.8, max(i_obsC, i_spaC,na.rm=T)*1.2), 
         cex=0.8,xlab='Year',ylab='Catch', bty='n')
    if(is.matrix(i_spaC)){
      matlines(years, i_spaC, lty=1, col=matched_colors)
    }
    lines(years, rowMeans(i_spaC, na.rm = TRUE), lwd = 4, col = 'gray60') ## Plot average
    if(is.vector(i_spaC)){
      lines(years,i_spaC,lty=1,col='blue')
    }
    title(main = paste(i, gsub('_', ' ', grp)),
          line =-.6,cex.main=0.9)
  }
dev.off()
```

# functions.R
R functions developed to assist with reading and processing data from the Ecopath with Ecosim (EwE) software suite. This includes the reading of timeseries data from Ecosim, standardizing functional group names, determining multipanel plot dimensions, and plotting of outputs.

### READ ECOSIM TIMESERIES FILE
- This function is designed to read and process data from an ecosim timeseries file (assumed to be in CSV format) and return four data frames: 
obsB.head, obsB, obsC.head, and obsC. 
- The first 4 rows contain information on 'Title', 'Weight, 'Pool code', and 'Type', respectively
- Data type notations: 0 = BiomassRel, 6 = CatchesRel

```R
f.read_ecosim_timeseries = function(filename, num_row_header = 4){
  fnm.obs_ts = filename
  obs.ts.head = as.data.frame(t(read.csv(fnm.obs_ts,header=F,nrows=num_row_header))) ## Reads the first four rows of the CSV file specified by filename and transposes the resulting data frame using t(). It converts this transposed data frame into another data frame named obs.ts.head.
  names(obs.ts.head) = obs.ts.head[1,]; obs.ts.head = obs.ts.head[-1,]  ## sets the column names of obs.ts.head to be the values from the first row of obs.ts.head. Then, it removes the first row from obs.ts.head, as it was used for column names.
  obs.ts.head[,2:num_row_header] = as.numeric(as.matrix(obs.ts.head[,2:num_row_header])) ## Reads the rest of the CSV file (starting from the fifth row) into a new data frame obs.ts. 
  obs.ts = read.csv(fnm.obs_ts,header=F,skip=num_row_header)
  rownames(obs.ts) = obs.ts[,1]; obs.ts[,1] = NULL
  if(nrow(obs.ts.head) != ncol(obs.ts)) print('HEADER AND TIMESERIES DIMENSION DO NOT MATCH!!!')
  
  ## Make four dataframes
  obsB.head = obs.ts.head[obs.ts.head[,num_row_header] %in% c(0,1),]
  obsB      = obs.ts[,which(obs.ts.head[,num_row_header] %in% c(0,1))]
  obsC.head = obs.ts.head[obs.ts.head[,num_row_header] %in% c(6,61,-6),]
  obsC      = obs.ts[,which(obs.ts.head[,num_row_header] %in% c(6,61,-6))]
  names(obsB.head) = names(obsC.head) = gsub(" ","_",names(obsB.head))
  ret = list(obsB.head,obsB,obsC.head,obsC)
  names(ret) = c('obsB.head','obsB','obsC.head','obsC')
  return(ret)
}
```
### FUNCTION TO STANDARDIZE EWE FUNCTIONAL GROUP NAMES
```R
f.standardize_group_names <- function(fg_names){
  fg_names = sub("\\.$", "", fg_names)
  fg_names = sub("\\.\\.", ".", fg_names)
  fg_names = gsub("(\\d)\\.(\\d)", "\\1-\\2", fg_names)
  fg_names = gsub("\\.yr", "+yr", fg_names)
  fg_names = gsub("\\.", "_", fg_names)
  fg_names = gsub("yr", "", fg_names)
}
```
### FUNCTION TO SET SET SKIPLINES FOR READING IN ECOSPACE FILES
```R
f.find_start_line <- function(filename, flag = 1980) {
  con <- file(filename, open = "r")
  line_number <- 0
  while(TRUE) {
    line <- readLines(con, n = 1)
    line_number <- line_number + 1
    if(length(line) == 0) { # End of file
      break
    }
    fields <- strsplit(line, ",")[[1]]
    # Check if fields has any elements before attempting to access its first element
    if(length(fields) >= 1 && fields[1] == flag) {
      close(con)
      return(line_number - 2) # Minus 2 because headers are counted in read.csv and we want to start on the next line
    }
  }
```
### FUNCTION TO SET MULTIPANEL PLOT DIMENSIONS
```R
f.get_plot_dims = function(x=nrow(obsB.head),round2=4){
  #x=nrow(df.names)/2;round2=4
  x2=plyr::round_any(x,round2,f=ceiling)
  xfact = numeric()
  for(i in 1:x2){
    if((x2 %% i) == 0){
      if(i == sqrt(x2)){xfact = c(xfact,c(i,i))
      } else {xfact = c(xfact,i)}
    }
  }
  mfrow.pars = rev(xfact[(length(xfact)/2):(1+length(xfact)/2)])
  return(mfrow.pars)
}
```
### FUNCTION MAKE PLOT
Note that X must be a Date object
```R
f.plot_outputs_obs_sim_spa <- function(x, grp, spaB_scaled, simB_scaled, obsB_scaled,
                                       col_spa = 'blue', col_sim = 'green', col_obs = 'black',
                                       num_fg  = 78, num_plot_pages = 4,
                                       x_break = 5, y_break = 4, x_cex = 0.9, y_cex = 0.9, x_las = 2,
                                       sim_lwd = 1, spa_lwd = 1, obs_pch = 16, obs_cex = 0.6,
                                       main_cex = 0.85, leg_cex = 0.7, leg_pos = 'topleft',
                                       leg_inset = 0.1){
  
  ## Determine y-axis range and initialize plot
  min = min(obsB_scaled, simB_scaled, spaB_scaled, na.rm=T) * 0.8
  max = max(obsB_scaled, simB_scaled, spaB_scaled, na.rm=T) * 1.2
  plot(x, rep("", length(x)), type='b', 
       ylim = c(min, max), xaxt = 'n', yaxt = 'n',
       xlab = '', ylab='', bty = 'n')
  
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
  lines(x, simB_scaled, lty=1, lwd = sim_lwd,  col = col_sim) ## Plot Ecosim
  if(is.vector(spaB)) lines(x, spaB_scaled, lty = 1, lwd = spa_lwd, col = col_spa) ## Plot Ecospace
  if(is.matrix(spaB)) matlines(x, spaB_scaled, lty=1, lwd = spa_lwd, col=c(rep('lightblue', ncol(spaB)-1), col_spa)) ## Plot multiple Ecospace projections, if available.
  if(length(obsB_scaled)>0) points(year_series, obsB_scaled, pch=16, cex=0.6, col = col_obs) ## Plot observed data, if it's present
  
  ## Add legend and title
  title(main = grp, line=-.6, cex.main = main_cex)
  if(i %in% seq(1, num_fg, by = ceiling(num_fg / num_plot_pages))) {
    legend(leg_pos, inset = 0.1, bg="gray90", box.lty = 0,
           legend=c('Observed','Ecosim','Ecospace'),
           lty =c(NA, 1, 1), lwd = c(NA, sim_lwd, spa_lwd), pch=c(obs_pch, NA, NA), 
           col =c(col_obs, col_sim, col_spa), cex = leg_cex)
  }
}
```
### Example code to test plotting 
```R
f.plot_outputs_obs_sim_spa(x = year_series, grp = grp, 
                           spaB_scaled = spaB_scaled, simB_scaled = simB_scaled, obsB_scaled = obsB_scaled, 
                           col_spa = col_spa, col_sim = col_sim, col_obs = col_obs,
                           num_fg = length(fg_names), num_plot_pages = 4, 
                           sim_lwd = 2, spa_lwd = 2)

f.plot_outputs_obs_sim_spa(x = date_series, grp = grp, 
                           spaB_scaled = spaB_scaled, simB_scaled = simB_scaled, obsB_scaled = obsB_scaled, 
                           col_spa = col_spa, col_sim = col_sim, col_obs = col_obs,
                           num_fg = length(fg_names), num_plot_pages = 4)
```

## Disclaimer
This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
