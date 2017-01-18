#----------------------------------
#toydata
set.seed(42)
my.dates = seq(as.Date("1999/1/1"), as.Date("2012/1/1"), "3 months") 
Time = format(my.dates, format = "%Y-%m-%dT%H:%M:%S",tz="UTC")
names(Time)[1] = "myTime"
Value = data.frame( v = seq(1,length(Time))/10 + rep(c(1,2,3,4), length.out = length(Time)) + runif(length(Time), min=0, max=3))


###############Library Declarations###############

libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***",sep=""))
}

libraryRequireInstall("zoo")
libraryRequireInstall("proto")



#################################################


minSeasonalityEnergyFactor = 0.5 # if seasonality should reduce reminder by 50% at least 


# tiny function to deal with verl long strings on plot
cutStr2Show = function(strText, strCex = 0.8, abbrTo = 100, isH = TRUE, maxChar = 3, partAvailable = 1)
{
  # partAvailable, wich portion of window is available, in [0,1]
  if(is.null(strText))
    return (NULL)
  
  SCL = 0.075*strCex/0.8
  pardin = par()$din
  gStand = partAvailable*(isH*pardin[1]+(1-isH)*pardin[2]) /SCL
  
  # if very very long abbreviate
  if(nchar(strText)>abbrTo && nchar(strText)> 1)
    strText = abbreviate(strText, abbrTo)
  
  # if looooooong convert to lo...
  if(nchar(strText)>round(gStand) && nchar(strText)> 1)
    strText = paste(substring(strText,1,floor(gStand)),"...",sep="")
  
  # if shorter than maxChar remove 
  if(gStand<=maxChar)
    strText = NULL
  
  return(strText) 
}

#format labels on X-axis automatically 
flexFormat = function(dates, orig_dates, freq = 1, myformat = NULL)
{
  
  days=(as.numeric(difftime(dates[length(dates)],dates[1]),units="days"))
  months = days/30
  years = days/365.25
  
  
  constHour = length(unique(orig_dates$hour))==1
  constMin = length(unique(orig_dates$min))==1
  constSec = length(unique(orig_dates$sec))==1
  constMon = length(unique(orig_dates$mon))==1
  
  timeChange = any(!constHour,!constMin,!constSec)
  
  if(is.null(myformat))
  {
    if(years > 10){
      if(constMon)
      {
        myformat = "%Y" #many years => only year :2001
      }else{
        myformat = "%m/%y" #many years + months :12/01
      }
    }else{
      if(years > 1 && N < 50){
        myformat = "%b %d, %Y" #several years, few samples:Jan 01, 2010
      }else{
        if(years > 1){
          myformat = "%m/%d/%y" #several years, many samples: 01/20/10
        }else{
          if(years <= 1 && !timeChange)
            myformat = "%b %d" #1 year,no time: Jan 01
        }  
      }
    }
  }
  if(is.null(myformat) && timeChange)
    if(years>1){
      myformat = "%m/%d/%y %H:%M" # 01/20/10 12:00
    }else{
      if(days>1){
        myformat = "%b %d, %H:%M" # Jan 01 12:00
      }else{
        if(days<=1){
          myformat = "%H:%M" # Jan 01 12:00
        }  
      }
    }
  if(!is.null(myformat)){
    if(myformat == "%Y,Q%q")
      dates = as.yearqtr(dates)
    dates1= format(dates,  myformat)
  }else{
    dates1 = as.character(1:length(dates)) # just id 
  }
  return(dates1)
}



# from : http://stats.stackexchange.com/questions/1207/period-detection-of-a-generic-time-series
freqFromValue <- function(x)
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


# verify if "perSeason" is good for "frequency" parameter
freqSeason = function(seasons,perSeason)
{
  if((seasons > 5 && perSeason > 3) || (seasons>2 && perSeason > 7))
    return (perSeason)
  
  return(1)
}

# find frequency using the dates, targetS is a "recommended" seasonality 
findFreqFromDates = function(dates, targetS = "autodetect from dates")
{
  freq = 1
  N = length(dates)
  nnn = c("hour", "day", "week", "month", "quater", "year")
  seasons = rep(NaN,6)
  names(seasons) = nnn
  perSeason = seasons
  
  seasons["day"]=round(as.numeric(difftime(dates[length(dates)],dates[1]),units="days"))
  seasons["hour"]=round(as.numeric(difftime(dates[length(dates)],dates[1]),units="hours"))
  seasons["week"]=round(as.numeric(difftime(dates[length(dates)],dates[1]),units="weeks"))
  seasons["month"] = seasons["day"]/30
  seasons["year"] = seasons["day"]/365.25
  seasons["quater"] = seasons["year"]*4
  
  perSeason = N/seasons
  
  if(targetS!="autodetect from date") # target 
    freq = perSeason[targetS]
  
  if(freq < 2 || round(freq)>24) # if TRUE, target season factor is not good 
    freq = 1
  
  for( s in rev(nnn)) # check year --> Quater --> etc
    if(freq==1 || round(freq)>24)
      freq = freqSeason(seasons[s],perSeason[s])
  
  
  if(round(freq)>24) # limit of exp smoothing R implementation
    freq = 1
  
  return(round(freq))
}



#decompose into 3 components, known frequency 
flexTSdecomposition = function(Time, Value, freq, trendSmoothness, myts, robustToOutliers, degree)
{
  N = length(Time)
  twin = getSTwindows(N, trendSmoothness= trendSmoothness, freq = freq)
  
  if(freq ==1)
  {
    s = (100*trendSmoothness/70)
    span = max(s,0.2) # get from t smoothness
    #fit <- loess(Value[,1] ~ Time, degree = 1+ degree, span = span)
    fit <- loess(Value[,1] ~ seq(1,length(Time)), degree = 1+ degree, span = span)
    fit$time.series.df = data.frame(seasonal = rep(0, length(Time)), trend = fit$fitted, residuals = fit$residuals, data = Value[,1])
    
  } 
  if(freq >1)
  {
    ## Convert to time series
    fit <- stl(myts,  robust = robustToOutliers, s.degree = degree, s.window = "periodic", t.window = twin)
    fit$time.series.df = as.data.frame(fit$time.series)
    fit$fitted = fit$time.series.df$seasonal+ fit$time.series.df$trend
    fit$residuals = Value[,1] - fit$fitted
    
  }
  
  
  clean = fit$time.series.df[,1]+fit$time.series.df[,2]
  seasonal = fit$time.series.df[,1]+mean(fit$time.series.df[,2])
  reminder = fit$time.series.df[,3]+mean(Value[,1])
  trend = fit$time.series.df[,2]
  
  dfTSD = data.frame(clean= clean, seasonal = seasonal, trend = trend, reminder = reminder)
  
  
  
}

#find relative part of signal
explained = function(sigModeled, sig)
{
  normL2sig = norm(sig, type ="2")
  normL2err = norm(sig - sigModeled, type ="2")
  return(1-(normL2err/(normL2sig+0.00001)))
}

nextodd = function(num)
{
  return (round(num)+(round(num) %% 2 == 0))
}

getByPos = function(arr,frac)
  arr[max(1,round(length(arr)*frac))]


getSTwindows = function(numSamples, trendSmoothness= 0.5, freq = 4)
{
  t = nextodd(freq*1.5)# default
  
  allTS = seq(3,max(7,max(t*2, nextodd(numSamples/2))), by = 2)
  
  return(getByPos(allTS,trendSmoothness))
}

getFrequency = function(parsed_dates,val,tS,f)
{
  
  myFreq = f
  grp = c("autodetect from value","none","manual")
  
  if(length(intersect(tS,grp))==0) #detect from date
  {  
    myFreq = findFreqFromDates(parsed_dates, targetS = tS)
  }else{
    if(tS == "none")
    { myFreq = 1}
    else
    {
    if(tS == "autodetect from value")
      myFreq = freqFromValue(val)
    }
  }
  numPeriods = floor(length(val)/myFreq)
  if(numPeriods< 2)
    myFreq = findFreqFromDates(parsed_dates, targetS = "autodetect from dates")
  return(myFreq)
}


#plotType: clean, trend, seasonal, all, reminder

#********* PBI Parameters Block ***************
if(!exists("Time"))
	Time = NULL 

if(!exists("Value"))
	Value = NULL 

showWarnInfo = TRUE  #default
if (exists("settings_extra_params_show")) 
{
	showWarnInfo = settings_extra_params_show
}

if(exists("settings_model_params_show") && settings_model_params_show == FALSE)
	rm(list= ls(pattern = "settings_model_params_" ))

if(exists("settings_algo_params_show") && settings_algo_params_show == FALSE)
	rm(list= ls(pattern = "settings_algo_params_" ))

if(exists("settings_plot_params_show") && settings_plot_params_show == FALSE)
	rm(list= ls(pattern = "settings_plot_params_" ))

if(exists("settings_extra_params_show") && settings_extra_params_show == FALSE)
	rm(list= ls(pattern = "settings_extra_params_" ))

##PBI_PARAM: display_name: Decomposition model, tooltip:switch between additive and multiplicative decomposition models 
# Type: enumeration, default:'automatic', 
# Min: , Max:
# enumeration options: additive ,multiplicative ,automatic ,
modelType = 'automatic'  #default
if (exists("settings_model_params_modelType")) 
{
	modelType = settings_model_params_modelType
}

##PBI_PARAM: display_name: Seasonal factor, tooltip:specify target seasonal factor
# Type: enumeration, default:'auto-detect from dates', 
# Min: , Max:
# enumeration options: autodetect from value ,autodetect from date ,none ,manual ,hour ,day ,week ,month ,quater ,year ,
targetSeasonality = 'auto-detect from dates'  #default
if (exists("settings_model_params_targetSeasonality")) 
{
	targetSeasonality = settings_model_params_targetSeasonality
}

##PBI_PARAM: display_name: Frequency, tooltip:Number of samples per period
# Type: numeric, default:12, 
# Min: 1, Max:10000
freq = 12  #default
if (exists("settings_model_params_freq")) 
{
	freq = settings_model_params_freq
	freq = max( min (freq, 10000), 1)
}

##PBI_PARAM: display_name: Degree, tooltip:degree of locally-fitted polynomial in seasonal extraction and trend extraction
# Type: bool, default:FALSE, 
# Min: , Max:
degree = FALSE  #default
if (exists("settings_algo_params_degree")) 
{
	degree = settings_algo_params_degree
}

##PBI_PARAM: display_name: Robust to outliers, tooltip:logical indicating if robust fitting be used in the loess procedure
# Type: bool, default:TRUE, 
# Min: , Max:
robustToOutliers = TRUE  #default
if (exists("settings_algo_params_robustToOutliers")) 
{
	robustToOutliers = settings_algo_params_robustToOutliers
}

##PBI_PARAM: display_name: Trend smoothness, tooltip:Trend smoothness
# Type: numeric, default:50, 
# Min: 1, Max:100
trendSmoothness = 50  #default
if (exists("settings_algo_params_percentile")) 
{
	trendSmoothness = settings_algo_params_percentile
	trendSmoothness = max( min (trendSmoothness, 100), 1)
}

##PBI_PARAM: display_name: Plot type, tooltip:specify the plot type
# Type: enumeration, default:'all', 
# Min: , Max:
# enumeration options: all ,trend ,seasonal ,clean ,reminder ,byseason ,
plotType = 'all'  #default
if (exists("settings_plot_params_plotType")) 
{
	plotType = settings_plot_params_plotType
}

##PBI_PARAM: display_name: Line width, tooltip:line width
# Type: numeric, default:10, 
# Min: 1, Max:50
lineWidth = 10  #default
if (exists("settings_plot_params_weight")) 
{
	lineWidth = settings_plot_params_weight
	lineWidth = max( min (lineWidth, 50), 1)
}

##PBI_PARAM: display_name: Line color, tooltip:line color
# Type: fill, default:'red', 
# Min: , Max:
lineCol = 'red'  #default
if (exists("settings_plot_params_lineCol")) 
{
	lineCol = settings_plot_params_lineCol
}

##PBI_PARAM: display_name: Labels color, tooltip:labels color
# Type: fill, default:'orange', 
# Min: , Max:
labelsCol = 'orange'  #default
if (exists("settings_plot_params_labelsCol")) 
{
	labelsCol = settings_plot_params_labelsCol
}

##PBI_PARAM: display_name: Labels font size, tooltip:labels font size
# Type: numeric, default:10, 
# Min: 8, Max:40
labelsFont = 10  #default
if (exists("settings_plot_params_textSize")) 
{
	labelsFont = settings_plot_params_textSize
	labelsFont = max( min (labelsFont, 40), 8)
}

##PBI_PARAM: display_name: Font size, tooltip:
# Type: numeric, default:8, 
# Min: 8, Max:40
infoFontSize = 8  #default
if (exists("settings_extra_params_textSize")) 
{
	infoFontSize = settings_extra_params_textSize
	infoFontSize = max( min (infoFontSize, 40), 8)
}

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
{
  #update params to correct scale
  trendSmoothness = trendSmoothness/100
  labelsFont = labelsFont/10 # convert from range 8-40
  lineWidth = lineWidth / 8 # convert from 1-50 
  infoFontSize = infoFontSize/10
  
  
  N = length(Time)
  
  parsed_dates=strptime(Time,"%Y-%m-%dT%H:%M:%S",tz="UTC")
  interval = difftime(parsed_dates[length(parsed_dates)],parsed_dates[1])/(length(parsed_dates)-1) # force equal spacing 
  x = as.POSIXlt(seq(from=parsed_dates[1], by= interval, length.out=length(parsed_dates)))
  
  
  
  
  
  #to be able to change lwd 
  mypanel= function(x, bg, pch, ...){ lines(x, bg, pch, col.lab = labelsCol, ...)}
  
  
  #detect the frequency 
  freq = getFrequency(parsed_dates,Value[,1],targetSeasonality,freq)
  print(freq)
  
  # Convert to time series
  myts <- ts(Value[,1], start = as.Date(min(Time)), frequency = freq)
  
  
  # decompose
  dfTSD <- flexTSdecomposition(Time, Value, freq, trendSmoothness, myts, robustToOutliers, degree)
  
  
  #plots 
  if (plotType == "byseason" && freq == 1)
    plotType = "all"
  
  
  #format  x_with_f
 # numTicks = FindTicksNum(NpF,freq) # find based on plot size
  
  # x_formatted = flexFormat(dates = x, orig_dates = parsed_dates, freq = freq)
  # 
  # numTicks = 5
  # correction = (N-1)/(numTicks-1) # needed due to subsampling of ticks
  # 
  
  myts_zoo = as.zoo(myts)
  time(myts_zoo) = parsed_dates
  
  
  ## Plot the result
  if(plotType == "all" )
  {
    
    mts0 = cbind(data=Value[,1], seasonal = dfTSD$seasonal  , trend=dfTSD$trend, remainder=dfTSD$reminder)
    mts1 = ts(mts0, start = as.Date(min(Time)), frequency= freq)
    
    mts1_zoo = as.zoo(mts1)
    time(mts1_zoo) = parsed_dates
    
   
    p <- proto(plot.zoo = plot.zoo, mtext = function(...) 
      graphics::mtext(..., cex = labelsFont, col = labelsCol))
    
    with(p, plot.zoo)(mts1_zoo, main="", col = lineCol, plot.type = c("multiple"),
         yax.flip = TRUE,
         col.lab = labelsCol,  panel = mypanel, lwd =lineWidth,  cex.lab = labelsFont)
   
    
  # (mts1_zoo) 
    
    
  }
  else
    if(plotType != "byseason")
    {
      
      plot(myts_zoo, col="gray",
           main="",
           lwd = lineWidth/2,  col.lab = labelsCol, cex.lab = labelsFont, xlab = "", ylab = "")
      
      dfTSD <- ts(dfTSD, start = as.Date(min(Time)), frequency= freq)
     
      line_zoo = as.zoo(dfTSD[,plotType])
      time(line_zoo) = parsed_dates
      
      # lines(dfTSD[,plotType], col = lineCol, lwd = lineWidth)
      lines(line_zoo, col = lineCol, lwd = lineWidth)
      
      abline(a = mean(Value[,1]), b = 0, col = "green", lty = 2, col.lab = labelsCol)
      
      title(main="", 
            xlab=names(Time)[1], ylab=names(Value)[1],
            col.lab=labelsCol, cex.lab=labelsFont)
      #axis(1, at = 1+((0:(53-1))/freq), labels = x_formatted)
      #TODO: legend
      #legend("topleft", c("data",plotType,"mean"), lty=c(1,1,2), pch = NA,  col = c("gray",lineCol,"green"))
    } else {
      if(plotType == "byseason")
      {
        fit <- stl(myts, s.window=5)
        par(lwd = lineWidth, col = lineCol)
        monthplot(fit$time.series[,"seasonal"], main="", ylab="Seasonal", 
                  col.lab=labelsCol, cex.lab=labelsFont, col.base = "gray", lwd.base = lineWidth/2, bty='n')
        box(col = 'black', lwd =1)
      }
    }
  
}

#########################
# 
# TimeSeriesDecomposition(Time,Value,
#                         targetSeasonality = "month", # one of "autodetect from date", "autodetect from value", none, manual, year, month, ...
#                         freq = 4, # for manual seasonality
#                         modelType = "additive", # additive/multiplicative/automatic
#                         trendSmoothness = 50,# in 0-100
#                         degree = 1,
#                         robustToOutliers = TRUE,
#                         plotType = "byseason", # all, clean, trend, seasonal, reminder, byseason
#                         lineWidth = 20, #
#                         lineCol = "red",
#                         labelsCol = "orange",
#                         labelsFont = 5,
#                         infoFontSize = 8,
#                         showInfoWarn = TRUE)
