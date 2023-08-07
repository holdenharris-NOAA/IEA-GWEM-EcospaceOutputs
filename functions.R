rm(list=ls());graphics.off();rm(.SavedPlots);gc();windows(record=T)

#-------------------------------------------------------------------------------  
# READ ECOSIM TIMESERIES FILE
#-------------------------------------------------------------------------------
## This function is designed to read and process data from an ecosim timeseries 
## file (assumed to be in CSV format) and return four data frames: 
## obsB.head, obsB, obsC.head, and obsC. 
## The first 4 rows contain information on 'Title', 'Weight, 'Pool code', and 'Type', respectively
## Data type notations: 0 = BiomassRel, 6 = CatchesRel, 

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

#-------------------------------------------------------------------------------  
# FUNCTION TO STANDARDIZE EWE FUNCTIONAL GROUP NAMES
#-------------------------------------------------------------------------------
f.standardize_group_names <- function(fg_names){
  fg_names = sub("\\.$", "", fg_names)
  fg_names = sub("\\.\\.", ".", fg_names)
  fg_names = gsub("(\\d)\\.(\\d)", "\\1-\\2", fg_names)
  fg_names = gsub("\\.yr", "+yr", fg_names)
  fg_names = gsub("\\.", "_", fg_names)
  fg_names = gsub("yr", "", fg_names)
}

#-------------------------------------------------------------------------------  
# FUNCTION TO SET MULTIPANEL PLOT DIMENSIONS
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------  
# FUNCTION MAKE PLOT
#-------------------------------------------------------------------------------

## Note that X must be a Date object
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

## Test
#f.plot_outputs_obs_sim_spa(x = year_series, grp = grp, 
#                           spaB_scaled = spaB_scaled, simB_scaled = simB_scaled, obsB_scaled = obsB_scaled, 
#                           col_spa = col_spa, col_sim = col_sim, col_obs = col_obs,
#                           num_fg = length(fg_names), num_plot_pages = 4, 
#                           sim_lwd = 2, spa_lwd = 2)

#f.plot_outputs_obs_sim_spa(x = date_series, grp = grp, 
#                           spaB_scaled = spaB_scaled, simB_scaled = simB_scaled, obsB_scaled = obsB_scaled, 
#                           col_spa = col_spa, col_sim = col_sim, col_obs = col_obs,
#                           num_fg = length(fg_names), num_plot_pages = 4)