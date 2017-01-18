#----------------------------------




###############Library Declarations###############

libraryRequireInstall = function(packageName, ...)
{
  if(!require(packageName, character.only = TRUE)) 
    warning(paste("*** The package: '", packageName, "' was not installed ***",sep=""))
}

libraryRequireInstall("zoo")
libraryRequireInstall("proto")

################Inner parameters #################################
#to 
Sys.setlocale("LC_ALL","English")# internationalization

# minimum samples for analysis
minSamples2run = 9 

# minimum unique values
minUniqueValues = 3 


################Inner functions #################################
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

#Info string to be plotted under the chart
CreateInfo = function(modelType, freqv, evars, plotType)
{
  #all, clean, trend, seasonal, reminder, byseason
  pbiInfo =""
  if(plotType == 'all')
    pbiInfo = paste(paste(names(evars), ": ", evars,"%",sep=""), collapse = ", ")
  else
    if(plotType %in% c('clean', 'byseasonClean'))
      pbiInfo = paste(paste('clean', ": ", 100-evars[3],"%",sep=""), collapse = ", ")
    else
      if(plotType %in% c('trend', 'seasonal', 'reminder'))
        pbiInfo = paste(paste(names(evars[plotType]), ":", evars[plotType],"%",sep=""), collapse = ", ")
      else
        if(plotType == 'byseason')
          pbiInfo = paste(paste(names(evars), ":", evars,"%",sep=""), collapse = ", ")
        
        sN=""
        if(!is.null(names(freqv)[1]) && !is.na(names(freqv)[1]))
          sN = paste(" / ", names(freqv)[1], sep="")
        
        pbiInfo = paste("Model: ", modelType, ", freq = ", as.character(freqv),sN, ", ", pbiInfo, sep ="")
        print(pbiInfo)
        return(pbiInfo)
}

# log on numeric array 
makeLog = function (val)
{
  add = 1 + max(0,min(val)) - min(val)  # heuristic
  transVal = val + add # shift to positive range (>1)
  logVal = log(transVal, base = exp(1))
  mul = norm(val, type ="2")/ norm(logVal, type ="2")
  logVal =logVal*mul 
  return(list(add = add, mul = mul, logVal = logVal, base = exp(1)))
}

# invert makeLog 
makeUnLog = function (logVal, add, mul, base = exp(1))
{
  transVal = exp(logVal/mul)
  val = transVal - add 
  return(val)
}

# verify if "perSeason" is good for "frequency" parameter
freqSeason = function(seasons,perSeason)
{
  if((seasons > 5 && perSeason > 3) || (seasons>2 && perSeason > 7))
    return (perSeason)
  
  return(1)
}

# find frequency using the dates, targetS is a "recommended" seasonality 
findFreqFromDates = function(dates, targetS = "autodetect from date")
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
  
  if(targetS!="autodetect from date" && targetS!="autodetect from date") # target 
    freq = perSeason[targetS]
  
  if(freq < 2) # if TRUE, target season factor is not good 
    freq = 1
  
  for( s in rev(nnn)) # check year --> Quater --> etc
    if(freq == 1 )
      freq = freqSeason(seasons[s],perSeason[s])
  
  return(round(freq))
}

#decompose into 3 components, known frequency 
flexTSdecomposition = function(Time, vals, freq, trendSmoothness, myts, robustToOutliers, degree)
{
  N = length(Time)
  twin = getSTwindows(N, trendSmoothness= trendSmoothness, freq = freq)
  
  if(freq ==1)
  {
    s = (100 * trendSmoothness / 70)
    span = max(s,0.2) # get from t smoothness
    fit <- loess(vals ~ seq(1,length(Time)), degree = 1 + degree, span = span)
    fit$time.series.df = data.frame(seasonal = rep(0, length(Time)), trend = fit$fitted, residuals = fit$residuals, data = vals)
    
  } 
  if(freq >1)
  {
    ## Convert to time series
    fit <- stl(myts,  robust = robustToOutliers, s.degree = degree, t.degree = degree, s.window = "periodic", t.window = twin)
    fit$time.series.df = as.data.frame(fit$time.series)
    fit$fitted = fit$time.series.df$seasonal + fit$time.series.df$trend
    fit$residuals = vals - fit$fitted
    
  }
  
  clean = fit$time.series.df[,1] + fit$time.series.df[,2]
  seasonal = fit$time.series.df[,1] + mean(fit$time.series.df[,2])
  reminder = fit$time.series.df[,3] + mean(vals)
  trend = fit$time.series.df[,2]
  dfTSD = data.frame(clean= clean, seasonal = seasonal, trend = trend, reminder = reminder)
  return(list(fit = fit, dfTSD = dfTSD))
  
}

#find relative part of signal
explained = function(sigModeled, sig)
{
  sig = sig - mean(sig)
  sigModeled = sigModeled - mean(sigModeled)
  normL2sig = norm(sig, type = "2")
  normL2err = norm(sigModeled, type = "2")
  return(( normL2err / ( normL2sig + 0.00001 )))
}

#next odd number
nextodd = function(num)
  return (round(num)+(round(num) %% 2 == 0))

#get smoothness parameters for STL function
getSTwindows = function(numSamples, trendSmoothness= 0.5, freq = 4)
{
  getByPos = function(arr,frac)
    arr[max(1,round(length(arr)*frac))]
  
  t = nextodd(freq*1.5)# default
  allTS = seq(3,max(7,max(t*2, nextodd(numSamples/2))), by = 2)
  return(getByPos(allTS,trendSmoothness))
}

#get valid frequency parameter, based on input from user 
getFrequency = function(parsed_dates, values, tS, f)
{
  myFreq = f
  grp = c("autodetect from value","none","manual")
  
  if(!(tS %in% c("autodetect from value","none","manual"))) #detect from date
  {  
    myFreq = findFreqFromDates(parsed_dates, targetS = tS)
  }else{
    if(tS == "none")
    { myFreq = 1}
    else
    {
      if(tS == "autodetect from value")
        myFreq = freqFromValue(values)
    }
  }
  numPeriods = floor(length(values)/myFreq)
  if(numPeriods< 2)
    myFreq = findFreqFromDates(parsed_dates, targetS = "autodetect from date")
  return(myFreq)
}


#plotType: clean, trend, seasonal, all, reminder
TimeSeriesDecomposition = function(Time, # formatted time 1 column, Time, Date, Time & Date equaly sampled 
                                   Value, # numeric 1 column 
                                   targetSeasonality = "year", # one of autoFromTime, autoFromValues, none, manual, year, month, ...
                                   freq = 4, # for manual seasonality 
                                   modelType = "additive", # additive/multiplicative/automatic
                                   trendSmoothness = 50,
                                   degree = 1, 
                                   robustToOutliers = TRUE,
                                   plotType = "all", # all, clean, trend, seasonal, decomposition, reminder  
                                   lineWidth = 2, # 
                                   lineCol = "red", 
                                   labelsCol = "red", 
                                   labelsFont = 8,
                                   infoFontSize = 8,
                                   showWarnInfo = TRUE, 
                                   infoCol = "gray")
{
  
  #update params to correct scale
  trendSmoothness = trendSmoothness/100 # from % to [0,1]
  labelsFont = labelsFont/10 # convert from range 8-40
  lineWidth = lineWidth / 8 # convert from 1-50 
  infoFontSize = infoFontSize/10
  
  pbiInfo ="" # warning or info string 
  
  #check if all Roles exist
  if(!(exists("Value") && exists("Time")))
  {
    Value = NULL; Time = ts(); plotType = "empty"
  }
  else
  {
    nameTime = names(Time)[1]
    Time = as.character(Time[,1])
    names(Time)[1] = nameTime
    N = length(Time)
    if(N < minSamples2run)
    {
      Value = NULL; Time = ts(); plotType = "empty"
      pbiInfo ="Warning: Not enough samples for analysis"
    }
  }
  if(plotType != "empty")
  {
    parsed_dates=strptime(Time,"%Y-%m-%dT%H:%M:%S",tz="UTC")
    if((any(is.na(parsed_dates))))
    {
      Value = NULL; Time = ts(); plotType = "empty"
      pbiInfo ="Warning: Only 'Date', 'Time', 'Date/Time' types are allowed for Time"
    }
    else
      if(!is.numeric(Value[,1]))
      {
        Value = NULL; Time = ts(); plotType = "empty"
        pbiInfo ="Warning: Only numeric types are allowed for Value"
      }
    else
      if(length(unique(Value[,1])) < minUniqueValues)
      {
        Value = NULL; Time = ts(); plotType = "empty"
        pbiInfo ="Warning: No sufficient variance in Value"
      }
  }
  
  
  
  if(plotType != "empty")
  {
    interval = difftime(parsed_dates[length(parsed_dates)],parsed_dates[1])/(length(parsed_dates)-1) # force equal spacing 
    
    vals = avals = Value[,1]
    mvals = NULL
    
    if(modelType != "additive")
    {
      mvals = makeLog(vals)
      vals = mvals$logVal
    }
    
    #detect the frequency 
    freqv = getFrequency(parsed_dates, vals, targetSeasonality, freq)
    
    # Convert to time series
    mytsAdd = myts <- ts(vals, start = as.Date(min(Time)), frequency = freqv)
    
    if(!is.null(mvals))
    {
      afreq = getFrequency(parsed_dates,avals,targetSeasonality,freq)
      mytsAdd <- ts(avals, start = as.Date(min(Time)), frequency = afreq)
    }
    
    # decompose (additive or multiplicative with or without seasonality)
    flexTSres <- flexTSdecomposition(Time, vals, freqv, trendSmoothness, myts, robustToOutliers, degree)
    
    dfTSD = flexTSres$dfTSD
    
    #explained variance
    if(is.null(mvals))
      evars = apply(dfTSD[,-1],2,FUN = explained, sig = vals)
    else
      evars = apply(makeUnLog(dfTSD[,-1],mvals$add, mvals$mul),2,FUN = explained, sig = avals)
    
    
    evars = round(100*evars/sum(evars))
    
    if(modelType == "automatic")
    {#compute the same for additive model, compare and select one 
      flexTSresAdd <- flexTSdecomposition(Time, avals, afreq, trendSmoothness, mytsAdd, robustToOutliers, degree)
      dfTSDadd <- flexTSresAdd$dfTSD
      evarsAdd = apply(dfTSDadd[,-1], 2, FUN = explained, sig = avals)
      evarsAdd = round(100*evarsAdd/sum(evarsAdd))
      
      if(evarsAdd[3]<evars[3]) # do additive
      {
        modelType = "additive"
        myts = mytsAdd
        mvals = NULL
        freqv = afreq
        dfTSD = dfTSDadd
        evars = evarsAdd
        flexTSres = flexTSresAdd
        vals = avals
      }
      else  # do multiplicative
        modelType = "multiplicative" 
    }
    
    #plots 
    if (plotType %in% c("byseason","byseasonClean") && freqv == 1)
      plotType = "clean"
    
    if(showWarnInfo)
      pbiInfo = CreateInfo(modelType, freqv, evars, plotType)
  }
  pbiInfo = cutStr2Show(pbiInfo, strCex = infoFontSize )
  
  ## Plot the result
  if(plotType == "all" )
  {
    if(is.null(mvals))
      mts0 = data.frame(data=vals, seasonal = dfTSD$seasonal  , trend=dfTSD$trend, reminder = vals- dfTSD$clean)
    else
    {
      mts0 = data.frame(data=avals, seasonal =  makeUnLog(dfTSD$seasonal, mvals$add, mvals$mul), 
                        trend=makeUnLog(dfTSD$trend,mvals$add, mvals$mul))
      mts0$reminder = mts0$data - makeUnLog(dfTSD$clean,mvals$add, mvals$mul)
    }
    
    
    names(mts0) = sapply(names(mts0),cutStr2Show, strCex = labelsFont , isH = F, partAvailable = 0.24, maxChar = 1)
    print(names(mts0))
    
    mts1 = ts(mts0, start = as.Date(min(Time)), frequency= freqv)
    
    mts1_zoo = as.zoo(mts1)
    time(mts1_zoo) = parsed_dates
    
    
    p <- proto(plot.zoo = plot.zoo, mtext = function(...) 
      graphics::mtext(..., cex = labelsFont, col = labelsCol))
    
    with(p, plot.zoo)(mts1_zoo, main="", col = lineCol, plot.type = c("multiple"),
                      yax.flip = TRUE,
                      col.lab = labelsCol,  lwd =lineWidth,  cex.lab = labelsFont)
    
    title(main="", sub = pbiInfo, cex.sub = infoFontSize, col.sub = infoCol)
  }
  else
    if((plotType %in% c("clean","trend", "reminder", "seasonal"))) # clean, trend, reminder
    {
      myts_zoo = as.zoo(mytsAdd)# changed to zoo type because it allows to replace dates
      time(myts_zoo) = parsed_dates
      
      # plot input data
      plot(myts_zoo, col="gray", 
           main="",
           lwd = lineWidth/2,  col.lab = labelsCol, cex.lab = labelsFont, xlab = "", ylab = "")
      
      
      if(!is.null(mvals))
      {
        dfTSD[,plotType] = makeUnLog(dfTSD[,plotType],mvals$add, mvals$mul)
        if(plotType=="reminder")
          dfTSD$clean = makeUnLog(dfTSD$clean,mvals$add, mvals$mul)
      }
      m = 0; meanAvals = mean(avals)
      
      dfTSD$reminder = avals - dfTSD$clean + meanAvals
      
      dfTSD <- ts(dfTSD, start = as.Date(min(Time)), frequency= freqv)
      
      
      
      if(plotType %in% c("seasonal")) # draw around mean
        m = - mean(dfTSD[,plotType]) + meanAvals
      
      line_zoo = as.zoo(dfTSD[,plotType] + m)
      time(line_zoo) = parsed_dates
      
      lines(line_zoo, col = lineCol, lwd = lineWidth)
      
      abline(a = meanAvals, b = 0, col = "green", lty = 2, col.lab = labelsCol)
      
      xlab=names(Time)[1]; ylab=names(Value)[1] 
      ylab = cutStr2Show(ylab,labelsFont, isH = FALSE, maxChar = 1)
      xlab = cutStr2Show(xlab,labelsFont, isH = TRUE, maxChar = 1)
      title(main="", 
            xlab = xlab, ylab = ylab,
            col.lab=labelsCol, cex.lab=labelsFont, sub = pbiInfo, cex.sub = infoFontSize, col.sub = infoCol)
      
    } else 
      if(plotType %in% c("byseason","byseasonClean"))
      {
        #fit <- stl(myts, s.window=5)
        fit <- flexTSres$fit
        par(lwd = lineWidth, col = lineCol)
        d = fit$time.series[,"trend"] + fit$time.series[,"seasonal"]
        ylab =paste("Clean", names(Value)[1], sep=" ")
        
        if(plotType == "byseason")
        {
          d = d + fit$time.series[,"remainder"]
          ylab =names(Value)[1]
        }
        if(!is.null(mvals))
          ylab =paste("log(", ylab, ")", sep="")
        xlab = "Season"
        
        ylab = cutStr2Show(ylab,labelsFont, isH = FALSE, maxChar = 1)
        xlab = cutStr2Show(xlab,labelsFont, isH = TRUE, maxChar = 1)
        
        monthplot(d , main="", ylab = ylab, xlab = xlab,  
                  col.lab=labelsCol, cex.lab=labelsFont, col.base = "gray", lwd.base = lineWidth/2, bty='n')
        box(col = 'black', lwd =1)
      }
  else #empty
  {
    plot.new()
    title(main="", sub = pbiInfo, cex.sub = infoFontSize, col.sub = infoCol)
  }
  
}

