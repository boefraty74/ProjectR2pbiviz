set.seed(42)

Time = seq(1999,2012, by = 0.25) 
Value = data.frame( v = rep(c(1,2,3,4), length.out = length(Time)) + runif(length(Time), min=0, max=3))


# from : http://stats.stackexchange.com/questions/1207/period-detection-of-a-generic-time-series
find.freq <- function(x)
{
  n <- length(x)
  spec <- spec.ar(c(x),plot=FALSE)
  if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
  {
    period <- round(1/spec$freq[which.max(spec$spec)])
    if(period==Inf) # Find next local maximum
    {
      j <- which(diff(spec$spec)>0)
      if(length(j)>0)
      {
        nextmax <- j[1] + which.max(spec$spec[j[1]:500])
        period <- round(1/spec$freq[nextmax])
      }
      else
        period <- 1
    }
  }
  else
    period <- 1
  return(period)
}

#drawWhat: clean, trend, seasonal, all, noise

#********* PBI Parameters Block ***************
if(!exists("Time"))
	Time = NULL 

if(!exists("Value"))
	Value = NULL 

if(exists("settings_tsdecomp_params_show") && settings_tsdecomp_params_show == FALSE)
	rm(list= ls(pattern = "settings_tsdecomp_params_" ))

##PBI_PARAM: display_name: frequency, tooltip:number of samples per period
# Type: numeric, default:12, 
# Min: 2, Max:10000
freq = 12  #default
if (exists("settings_tsdecomp_params_freq")) 
{
	freq = settings_tsdecomp_params_freq
	freq = max( min (freq, 10000), 2)
}

##PBI_PARAM: display_name: degree, tooltip:degree of locally-fitted polynomial in seasonal extraction and trend extraction
# Type: bool, default:FALSE, 
# Min: , Max:
degree = FALSE  #default
if (exists("settings_tsdecomp_params_degree")) 
{
	degree = settings_tsdecomp_params_degree
}

##PBI_PARAM: display_name: robust, tooltip:logical indicating if robust fitting be used in the loess procedure
# Type: bool, default:TRUE, 
# Min: , Max:
robustToOutliers = TRUE  #default
if (exists("settings_tsdecomp_params_robustToOutliers")) 
{
	robustToOutliers = settings_tsdecomp_params_robustToOutliers
}

##PBI_PARAM: display_name: model, tooltip:switch between additive and multiplicative decomposition models 
# Type: enumeration, default:'additive', 
# Min: , Max:
# enumeration options: additive ,multiplicative ,
model = 'additive'  #default
if (exists("settings_tsdecomp_params_model")) 
{
	model = settings_tsdecomp_params_model
}

##PBI_PARAM: display_name: smoothness, tooltip:trend and seasonality smoothness
# Type: numeric, default:10, 
# Min: 0, Max:100
smoothness = 10  #default
if (exists("settings_tsdecomp_params_percentile")) 
{
	smoothness = settings_tsdecomp_params_percentile
	smoothness = max( min (smoothness, 100), 0)
}

##PBI_PARAM: display_name: line width, tooltip:line width
# Type: numeric, default:10, 
# Min: 0, Max:50
lineWidth = 10  #default
if (exists("settings_tsdecomp_params_weight")) 
{
	lineWidth = settings_tsdecomp_params_weight
	lineWidth = max( min (lineWidth, 50), 0)
}

##PBI_PARAM: display_name: plot, tooltip:specify the plot type
# Type: enumeration, default:'all', 
# Min: , Max:
# enumeration options: all ,trend ,seasonal ,clean ,noise ,
drawWhat = 'all'  #default
if (exists("settings_tsdecomp_params_drawWhat")) 
{
	drawWhat = settings_tsdecomp_params_drawWhat
}

 
{
  ## Convert to time series
  myts <- ts(Value[,1], start = min(Time), frequency= freq)
  
  ## Decompose the timeseries object using the Loess function 
  fit <- stl(myts, s.window="periodic", robust = robustToOutliers, s.degree = degree)
  
  ## Plot the result
  if(drawWhat == "all")
    plot(fit, col="blue", main = "", lwd = lineWidth, col.range = "light gray")
  
  clean = fit$time.series[,1]+fit$time.series[,2]
  #fit$time.series = cbind(fit$time.series, clean = clean)
  
  seasonal = fit$time.series[,1]+mean(fit$time.series[,2])
  #fit$time.series = cbind(fit$time.series, seasonality = seasonality)
  
  noise = fit$time.series[,3]+mean(Value[,1])
  trend = fit$time.series[,2]
  #fit$time.series = cbind(fit$time.series, trend = trend )
  dfTSD = data.frame(clean= clean, seasonal = seasonal, trend = trend, noise = noise)
  
  if(drawWhat != "all")
  {
    plot(myts, col="gray",
       main="",
       ylab=names(Value)[1], xlab="", lwd = lineWidth/2)
    
    lines(dfTSD[,drawWhat],col="red",ylab = drawWhat, lwd = lineWidth)
    
    abline(a = mean(fit$time.series[,2]), b = 0, col = "green", lty = 2)
    #TODO: legend
  }
  
}

#TimeSeriesDecomposition(Time,Value, freq = 4, drawWhat = "noise")# clean, trend, seasonal, all, energy, noise)
