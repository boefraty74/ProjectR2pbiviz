#----------------------------------
#toydata
set.seed(42)
my.dates = seq(as.Date("1999/1/1"), as.Date("2012/1/1"), "3 months") 
Time = format(my.dates, format = "%Y-%m-%dT%H:%M:%S",tz="UTC")
names(Time)[1] = "myTime"

#additive
ValueAdd = data.frame( va = seq(1,length(Time))/10 + rep(c(1,2,3,4), length.out = length(Time)) + runif(length(Time), min=-1, max=1))

#multiplicative
ValueMul = data.frame( vm = seq(1,length(Time))/10 * rep(c(1,2,3,4), length.out = length(Time)) * runif(length(Time), min=0.75, max=1.25))


Time = data.frame(Time)
Value = ValueAdd


source("tsDecompositionWithLoess.R")

TimeSeriesDecomposition(Time,Value, # 2 roles 
                        targetSeasonality = "manual", # one of "autodetect from date", "autodetect from value", none, manual, year, month, ...
                        freq = 4, # for manual seasonality
                        modelType = "automatic", # additive/multiplicative/automatic
                        trendSmoothness = 50,# in 0-100
                        degree = 1,
                        robustToOutliers = TRUE,
                        plotType = "trend", # all, clean, trend, seasonal, reminder, byseason
                        lineWidth = 10, #
                        lineCol = "blue",
                        labelsCol = "orange",
                        labelsFont = 10,
                        infoFontSize = 8,
                        showWarnInfo = TRUE,
                        infoCol = "brown")
