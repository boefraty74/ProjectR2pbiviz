set.seed(42)

Time = seq(1999,2012, by = 0.25) 
Value = data.frame( v = rep(c(1,2,3,4), length.out = length(Time)) + runif(length(Time), min=0, max=3))

#drawWhat: clean, trend, seasonal, all, energy, seasonalyAdjasted

#********* PBI Parameters Block ***************
if(!exists("Time"))
	Time = NULL 

if(!exists("Value"))
	Value = NULL 

if(exists("settings_tsdecomp_params_show") && settings_tsdecomp_params_show == FALSE)
	rm(list= ls(pattern = "settings_tsdecomp_params_" ))

if(exists("settings_demo_params_show") && settings_demo_params_show == FALSE)
	rm(list= ls(pattern = "settings_demo_params_" ))

##PBI_PARAM: display_name: frequency, tooltip:frequency
# Type: numeric, default:12, 
# Min: 2, Max:10000
freq = 12  #default
if (exists("settings_tsdecomp_params_freq")) 
{
	freq = settings_tsdecomp_params_freq
	freq = max( min (freq, 10000), 2)
}

##PBI_PARAM: display_name: degree, tooltip:degree
# Type: bool, default:FALSE, 
# Min: , Max:
degree = FALSE  #default
if (exists("settings_tsdecomp_params_degree")) 
{
	degree = settings_tsdecomp_params_degree
}

##PBI_PARAM: display_name: robustToOutliers, tooltip:robustToOutliers
# Type: bool, default:TRUE, 
# Min: , Max:
robustToOutliers = TRUE  #default
if (exists("settings_tsdecomp_params_robustToOutliers")) 
{
	robustToOutliers = settings_tsdecomp_params_robustToOutliers
}

##PBI_PARAM: display_name: additive, tooltip:additive
# Type: bool, default:TRUE, 
# Min: , Max:
additive = TRUE  #default
if (exists("settings_tsdecomp_params_additive")) 
{
	additive = settings_tsdecomp_params_additive
}

##PBI_PARAM: display_name: trendSmoothness, tooltip:trendSmoothness
# Type: numeric, default:10, 
# Min: 0, Max:100
trendSmoothness = 10  #default
if (exists("settings_tsdecomp_params_trendSmoothness")) 
{
	trendSmoothness = settings_tsdecomp_params_trendSmoothness
	trendSmoothness = max( min (trendSmoothness, 100), 0)
}

##PBI_PARAM: display_name: drawWhat, tooltip:drawWhat
# Type: enumeration, default:'', 
# Min: , Max:
# enumeration options: all ,trend ,seasonalyAdjasted ,clean ,
drawWhat = ''  #default
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
    plot(fit, col="blue", main = "", lwd = lineWidth)
  
  clean = fit$time.series[,1]+fit$time.series[,2]
  fit$time.series = cbind(fit$time.series, clean = clean)
  
  if(drawWhat != "all")
  {
    plot(myts, col="gray",
       main="",
       ylab=names(Value)[1], xlab="", lwd = lineWidth/2)
    
    lines(fit$time.series[,drawWhat],col="red",ylab = drawWhat, lwd = lineWidth)
    #TODO: legend
  }
  
}

TimeSeriesDecomposition(Time,Value)
