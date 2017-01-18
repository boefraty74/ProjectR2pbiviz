#rcvUtilities.R
library(rjson)
library(RJSONIO)
#library(jsonlite)

getEnumerationStrings = function(e)
{
  N = length(e)
  s = character()
  if(N>0)
    for (i in (1:N))
      s[i] = e[[i]][2]

  return(s)
}


rcvJson2rdata = function(rcvJson)
{
  if(is.character(rcvJson))
    rcvJson <- RJSONIO::fromJSON(rcvJson)
  
  rcvRdata = initRcvRdata(rcvJson$pbiviz$apiVersion)
  #info
  if(!is.null(rcvJson$info))
    rcvRdata$info = rcvJson$info
  #rscript
  if(!is.null(rcvJson$sourceRscript))
    rcvRdata$sourceRscript = rcvJson$sourceRscript
  
  #roles
  rcvRdata$roles = rcvJson$dataRoles
  
  #groups and params 
  rcvRdata$groups = list(name =list(), displayName= list(), description =list(), showType = list(), extraData = list(), 
                         listParamNames = list(), listParamInds = list())
  rcvRdata$params = list(name =list(), rname = list(), displayName= list(),
                         description =list(), type = list(), typeAsIs = list(),  pmin = list(), pmax = list(), default = list(),   
                         groupName = list(), enumerationData = list(), extraData = list())
  
  NG = length(rcvJson$parameters)
  countp = 1
  for (g in (1:NG))
  {
    rcvRdata$groups$name[[g]] = names(rcvJson$parameters[g])
    rcvRdata$groups$displayName[[g]] =  rcvJson$parameters[[g]]$displayName
    rcvRdata$groups$description[[g]] = rcvJson$parameters[[g]]$description 
    rcvRdata$groups$showType[[g]] = rcvJson$parameters[[g]]$properties$show$default
    rcvRdata$groups$listParamNames[[g]] = names(rcvJson$parameters[[g]]$properties)
    
    NP = length(rcvJson$parameters[[g]]$properties)
    
    rcvRdata$groups$listParamInds[[g]] = countp:(countp+NP-1)
    
    for (p in (1:NP))
    {
      rcvRdata$params$name[[countp]] = names(rcvJson$parameters[[g]]$properties)[p]
      rcvRdata$params$rname[[countp]] = rcvJson$parameters[[g]]$properties[[p]]$rname
      rcvRdata$params$displayName[[countp]] = rcvJson$parameters[[g]]$properties[[p]]$displayName
      rcvRdata$params$description[[countp]] = rcvJson$parameters[[g]]$properties[[p]]$description
      rcvRdata$params$type[[countp]] = names(rcvJson$parameters[[g]]$properties[[p]]$type)
      rcvRdata$params$default[[countp]] = rcvJson$parameters[[g]]$properties[[p]]$default
      rcvRdata$params$pmin[[countp]] =  rcvJson$parameters[[g]]$properties[[p]]$min
      rcvRdata$params$pmax[[countp]] =  rcvJson$parameters[[g]]$properties[[p]]$max
      rcvRdata$params$groupName[[countp]] = rcvRdata$groups$name[[g]]
      rcvRdata$params$enumerationData[[countp]] = NA
      if(rcvRdata$params$type[[countp]] == "enumeration")
        rcvRdata$params$enumerationData[[countp]] = rcvJson$parameters[[g]]$properties[[p]]$type$enumeration
      rcvRdata$params$extraData[[countp]] = NA
      rcvRdata$params$typeAsIs[[countp]] = rcvJson$parameters[[g]]$properties[[p]]$type
      
      countp = countp +1
    }
    
    
  }
  
  
  
  #dependencies
  rcvRdata$dependencies = rcvJson$dependencies
  
  #pbiviz
  rcvRdata$pbiviz = rcvJson$pbiviz
  
  return(rcvRdata)
  
}

rdata2rcvJson = function(rcvRdata){
  
  stam = 1
  stam =2
  stam =3
  
}

initRcvRdata = function(ver = "1.2.0")
{
  rcvRdata = list()
  rcvRdata$info = list(nameParentDir = "", rcvRdata$namePbivizDir, created= date(), lastUpdate = date())
  rcvRdata$sourceRscript = list(inputRscriptName = "", funcName = NULL)
  
  rcvRdata$roles = list()
  rcvRdata$groups = list()
  rcvRdata$params = list()
  
  rcvRdata$dependencies = list()
  rcvRdata$pbiviz = list()
  
  return(rcvRdata)
}


GetCodeIntoSourceTS = function(rcvRdata, rcvJson, ind)
{
  t2t = c("bool", "numeric", "fill", "enumeration", "string")
  names(t2t) = c("boolean", "number", "string", "string", "string")
  NG = length(rcvRdata$groups$name)
  oL = character()
  cL = 1
  for(g in (1:NG))
  {
    nameg = rcvRdata$groups$name[[g]]
    if(ind == 1)
    {
      oL[cL] =paste("interface Visual_", nameg, " {", sep =""); cL = cL +1
      for (p in rcvRdata$groups$listParamInds[[g]])
      {
        namep = rcvRdata$params$name[[p]]
        typep = rcvRdata$params$type[[p]] 
        type_ts = names(t2t[t2t==typep])
        oL[cL] =paste("\t", namep, ": ", type_ts, ";", sep =""); cL = cL +1
      }
      oL[cL] ="}\n"; cL = cL +1
    }
    if(ind == 2)
    {
      oL[cL] =paste("private ", nameg, ": Visual_", nameg, ";", sep =""); cL = cL +1
    }
    if(ind == 3)
      {
        oL[cL] =paste("this.", nameg, " = <Visual_", nameg, "> {", sep =""); cL = cL +1
        for (p in rcvRdata$groups$listParamInds[[g]])
        {
          namep = rcvRdata$params$name[[p]]
          typep = rcvRdata$params$type[[p]] 
          type_ts = names(t2t[t2t==typep])
          defp = rcvRdata$params$default[[p]]
          if(type_ts == "string")
            defp = paste('"', defp, '"', sep ="" )
          if(typep=="bool")
            defp=tolower(defp)
              
          oL[cL] =paste("\t", namep, ": ", defp, ",", sep =""); cL = cL +1
        }
        oL[cL] ="};\n"; cL = cL +1
    }
    if(ind == 4)
    {
      oL[cL] =paste("this.", nameg, " = <Visual_", nameg, "> {", sep =""); cL = cL +1
      for (p in rcvRdata$groups$listParamInds[[g]])
      {
        namep = rcvRdata$params$name[[p]]
        typep = rcvRdata$params$type[[p]] 
        type_ts = names(t2t[t2t==typep])
        defp = rcvRdata$params$default[[p]]
        if(type_ts == "string")
          defp = paste('"', defp, '"', sep ="" )
        if(typep=="bool")
          defp=tolower(defp)
        #show: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'show', false),
        oL[cL] =paste("\t", namep, " getValue <",type_ts,">( dataView.metadata.objects, '", nameg, "', '",
                      namep, "', ", defp,")",  sep =""); cL = cL +1
      }
      oL[cL] ="};\n"; cL = cL +1
    }
    if(ind == 5)
    {
      # case 'settings_labeling_params':
      #   objectEnumeration.push({
      #     objectName: objectName,
      #     properties: {
      oL[cL] =paste("\tcase '", nameg,"':", sep = ""); cL = cL +1
      oL[cL] =paste("\tobjectEnumeration.push({ \n \t objectName: objectName,\n \t\t  properties: { ", sep =""); cL = cL +1
      for (p in rcvRdata$groups$listParamInds[[g]])
      {
        namep = rcvRdata$params$name[[p]]
        typep = rcvRdata$params$type[[p]] 
        type_ts = names(t2t[t2t==typep])
        defp = rcvRdata$params$default[[p]]
        if(type_ts == "string")
          defp = paste('"', defp, '"', sep ="" )
        if(typep=="bool")
          defp=tolower(defp)
        
        minp = rcvRdata$params$pmin[[p]]
        maxp = rcvRdata$params$pmax[[p]]
        oL[cL] =paste("\t", namep, ": this.", nameg, ".", namep, ",",   sep =""); 
         if(!is.null(minp) && !is.null(maxp) && typep == "numeric")
           oL[cL] =paste("\t", namep, ": inMinMax(this.", nameg, ".", namep, ", ", 
                         as.character(minp), ", ", as.character(maxp), "),",   sep ="");
           cL = cL +1
      }
      oL[cL] ="},\n selector: null \n }); \n break;"; cL = cL +1
    }
  }
  
  
  return(oL)
  
}

#********* PBI Parameters Block ***************
if(exists("settings_prepocessing_params") && settings_prepocessing_params==FALSE)
	rm(list= ls(pattern = ""settings_prepocessing_params_" ))

if(exists("settings_clusterNum_params") && settings_clusterNum_params==FALSE)
	rm(list= ls(pattern = ""settings_clusterNum_params_" ))

if(exists("settings_viz_params") && settings_viz_params==FALSE)
	rm(list= ls(pattern = ""settings_viz_params_" ))

if(exists("settings_labeling_params") && settings_labeling_params==FALSE)
	rm(list= ls(pattern = ""settings_labeling_params_" ))

if(exists("settings_representative_params") && settings_representative_params==FALSE)
	rm(list= ls(pattern = ""settings_representative_params_" ))

if(exists("settings_legend_params") && settings_legend_params==FALSE)
	rm(list= ls(pattern = ""settings_legend_params_" ))

if(exists("settings_additional_params") && settings_additional_params==FALSE)
	rm(list= ls(pattern = ""settings_additional_params_" ))

##PBI_PARAM: display_name: , tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
show = TRUE  #default
if exists("settings_prepocessing_params_show")) 
{
	show = settings_prepocessing_params_show
}

##PBI_PARAM: display_name: Scale data, tooltip:Used to standardize the range of features of data
# Type: bool, default:FALSE, 
# Min: , Max:
scaleData = FALSE  #default
if exists("settings_prepocessing_params_scaleData")) 
{
	scaleData = settings_prepocessing_params_scaleData
}

##PBI_PARAM: display_name: Apply PCA, tooltip:Recommended for data with more than two dimensions
# Type: bool, default:FALSE, 
# Min: , Max:
applyPCA = FALSE  #default
if exists("settings_prepocessing_params_applyPCA")) 
{
	applyPCA = settings_prepocessing_params_applyPCA
}

##PBI_PARAM: display_name: , tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
show = TRUE  #default
if exists("settings_clusterNum_params_show")) 
{
	show = settings_clusterNum_params_show
}

##PBI_PARAM: display_name: Number of clusters, tooltip:
# Type: enumeration, default:'auto', 
# Min: , Max:
# enumeration options: auto ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12 ,
numOfClusters = 'auto'  #default
if exists("settings_clusterNum_params_numOfClusters")) 
{
	numOfClusters = settings_clusterNum_params_numOfClusters
}

##PBI_PARAM: display_name: Method, tooltip:Select a method automate the number of clusters displayed
# Type: enumeration, default:'fast', 
# Min: , Max:
# enumeration options: moderate ,slow ,fast ,
numClustersMethods = 'fast'  #default
if exists("settings_clusterNum_params_numClustersMethods")) 
{
	numClustersMethods = settings_clusterNum_params_numClustersMethods
}

##PBI_PARAM: display_name: , tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
show = TRUE  #default
if exists("settings_viz_params_show")) 
{
	show = settings_viz_params_show
}

##PBI_PARAM: display_name: Draw Ellipse, tooltip:
# Type: bool, default:FALSE, 
# Min: , Max:
drawEllipse = FALSE  #default
if exists("settings_viz_params_drawEllipse")) 
{
	drawEllipse = settings_viz_params_drawEllipse
}

##PBI_PARAM: display_name: Draw Convex Hull, tooltip:
# Type: bool, default:FALSE, 
# Min: , Max:
drawConvexHull = FALSE  #default
if exists("settings_viz_params_drawConvexHull")) 
{
	drawConvexHull = settings_viz_params_drawConvexHull
}

##PBI_PARAM: display_name: Draw Centroid, tooltip:
# Type: bool, default:FALSE, 
# Min: , Max:
drawCentroid = FALSE  #default
if exists("settings_viz_params_drawCentroid")) 
{
	drawCentroid = settings_viz_params_drawCentroid
}

##PBI_PARAM: display_name: Point opacity, tooltip:
# Type: numeric, default:30, 
# Min: 0, Max:100
pointOpacity = 30  #default
if exists("settings_viz_params_percentile")) 
{
	pointOpacity = settings_viz_params_percentile
	pointOpacity = max( min (pointOpacity, 100), 0)
}

##PBI_PARAM: display_name: Point size, tooltip:
# Type: numeric, default:10, 
# Min: 1, Max:50
pointSize = 10  #default
if exists("settings_viz_params_weight")) 
{
	pointSize = settings_viz_params_weight
	pointSize = max( min (pointSize, 50), 1)
}

##PBI_PARAM: display_name: , tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
show = TRUE  #default
if exists("settings_labeling_params_show")) 
{
	show = settings_labeling_params_show
}

##PBI_PARAM: display_name: Font size, tooltip:
# Type: numeric, default:8, 
# Min: 8, Max:40
labelingFontSize = 8  #default
if exists("settings_labeling_params_textSize")) 
{
	labelingFontSize = settings_labeling_params_textSize
	labelingFontSize = max( min (labelingFontSize, 40), 8)
}

##PBI_PARAM: display_name: Label opacity, tooltip:
# Type: numeric, default:100, 
# Min: 0, Max:100
labelOpacity = 100  #default
if exists("settings_labeling_params_percentile")) 
{
	labelOpacity = settings_labeling_params_percentile
	labelOpacity = max( min (labelOpacity, 100), 0)
}

##PBI_PARAM: display_name: Limit label length, tooltip:Abbreviate labels to a limited length
# Type: numeric, default:5, 
# Min: 1, Max:100
maxLenPointLabel = 5  #default
if exists("settings_labeling_params_maxLenPointLabel")) 
{
	maxLenPointLabel = settings_labeling_params_maxLenPointLabel
	maxLenPointLabel = max( min (maxLenPointLabel, 100), 1)
}

##PBI_PARAM: display_name: Percentage of points labeled, tooltip:Avoids cluttering text
# Type: numeric, default:100, 
# Min: 0, Max:100
percentile1 = 100  #default
if exists("settings_labeling_params_percentile1")) 
{
	percentile1 = settings_labeling_params_percentile1
	percentile1 = max( min (percentile1, 100), 0)
}

##PBI_PARAM: display_name: , tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
show = TRUE  #default
if exists("settings_representative_params_show")) 
{
	show = settings_representative_params_show
}

##PBI_PARAM: display_name: Font size, tooltip:
# Type: numeric, default:8, 
# Min: 1, Max:40
representativeFontSize = 8  #default
if exists("settings_representative_params_textSize")) 
{
	representativeFontSize = settings_representative_params_textSize
	representativeFontSize = max( min (representativeFontSize, 40), 1)
}

##PBI_PARAM: display_name: Limit label length, tooltip:Abbreviate labels to a limited length
# Type: numeric, default:100, 
# Min: 1, Max:100
maxLenDelegateLabel = 100  #default
if exists("settings_representative_params_maxLenDelegateLabel")) 
{
	maxLenDelegateLabel = settings_representative_params_maxLenDelegateLabel
	maxLenDelegateLabel = max( min (maxLenDelegateLabel, 100), 1)
}

##PBI_PARAM: display_name: , tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
show = TRUE  #default
if exists("settings_legend_params_show")) 
{
	show = settings_legend_params_show
}

##PBI_PARAM: display_name: Pallete type, tooltip:Color pallete type
# Type: enumeration, default:'rainbow', 
# Min: , Max:
# enumeration options: rainbow ,gray ,cm ,topo ,terrain ,heat ,
palleteType = 'rainbow'  #default
if exists("settings_legend_params_palleteType")) 
{
	palleteType = settings_legend_params_palleteType
}

##PBI_PARAM: display_name: , tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
show = TRUE  #default
if exists("settings_additional_params_show")) 
{
	show = settings_additional_params_show
}

##PBI_PARAM: display_name: Show warnings, tooltip:
# Type: bool, default:TRUE, 
# Min: , Max:
showWarnings = TRUE  #default
if exists("settings_additional_params_showWarnings")) 
{
	showWarnings = settings_additional_params_showWarnings
}

##PBI_PARAM: display_name: Color of warnings, tooltip:
# Type: fill, default:'orange', 
# Min: , Max:
warningsColor = 'orange'  #default
if exists("settings_additional_params_warningsColor")) 
{
	warningsColor = settings_additional_params_warningsColor
}

##PBI_PARAM: display_name: Minimum clusters, tooltip:Minimum clusters to search with auto-clusters
# Type: numeric, default:2, 
# Min: 1, Max:15
minClusters = 2  #default
if exists("settings_additional_params_minClusters")) 
{
	minClusters = settings_additional_params_minClusters
	minClusters = max( min (minClusters, 15), 1)
}

##PBI_PARAM: display_name: Maximum clusters, tooltip:Maximum clusters to search with auto-clusters
# Type: numeric, default:12, 
# Min: 1, Max:15
maxClusters = 12  #default
if exists("settings_additional_params_maxClusters")) 
{
	maxClusters = settings_additional_params_maxClusters
	maxClusters = max( min (maxClusters, 15), 1)
}

##PBI_PARAM: display_name: Maximum iterations, tooltip:Maximum iterations to convergence of k-means clustering
# Type: numeric, default:10, 
# Min: 1, Max:100
maxIter = 10  #default
if exists("settings_additional_params_maxIter")) 
{
	maxIter = settings_additional_params_maxIter
	maxIter = max( min (maxIter, 100), 1)
}

##PBI_PARAM: display_name: Number of initializations, tooltip:Number of random initialziations for k-means clustering
# Type: numeric, default:5, 
# Min: 1, Max:100
nStart = 5  #default
if exists("settings_additional_params_nStart")) 
{
	nStart = settings_additional_params_nStart
	nStart = max( min (nStart, 100), 1)
}

 
{
  scriptBlock = character()
  #if(exists("settings_prepocessing_params_show") && settings_prepocessing_params_show == FALSE)
  #rm(list= ls(pattern = "settings_prepocessing_params_"))
  
  # drawEllipse = FALSE
  # if(exists("settings_viz_params_drawEllipse"))
  #   drawEllipse = settings_viz_params_drawEllipse 
  NG = length(rcvRdata$groups)
  lineCount =1
  for (g  in (1:NG))
  {
    gn = rcvRdata$groups$name[[g]]
    scriptBlock[lineCount] = paste('if(exists("', gn, '") && ', gn, "==FALSE)",  "", sep ="" );  lineCount =  lineCount + 1
    scriptBlock[lineCount] = paste('\trm(list= ls(pattern = ""', gn, '_"',"\n", sep ="" );  lineCount =  lineCount + 1
  }
  
  for (g  in (1:NG))
  {
    gn = rcvRdata$groups$name[[g]]
    NP = length(rcvRdata$groups$listParamInds[[g]]) 
    for (p  in (1:NP))
    {
      rnamep = NULL
      ip = rcvRdata$groups$listParamInds[[g]][p]
      namep = rcvRdata$params$name[[ip]]
      if(length(rcvRdata$params$rname)>=ip)
        rnamep = rcvRdata$params$rname[[ip]]
      if(is.null(rnamep))
        rnamep = namep
      
      typep = rcvRdata$params$type[[ip]]
      minp = rcvRdata$params$pmin[[ip]]
      maxp = rcvRdata$params$pmax[[ip]]
      defaultp = rcvRdata$params$default[[ip]]
      descrp =  rcvRdata$params$description[[ip]]
      dispnamep =  rcvRdata$params$displayName[[ip]]
      gn_namep = paste(gn, "_", namep, sep ="")
      
      if(typep=="string" || typep == "enumeration" || typep == "fill")
        defaultp = paste("'", defaultp, "'", sep ="")
      
      scriptBlock[lineCount] = paste("##PBI_PARAM: display_name: ", dispnamep, ", tooltip:",descrp, sep ="" );  lineCount =  lineCount + 1
      scriptBlock[lineCount] = paste("# Type: ", typep, ", default:",as.character(defaultp), ", ", sep ="" );  lineCount =  lineCount + 1
      scriptBlock[lineCount] = paste("# Min: ",  as.character(minp), ", Max:",as.character(maxp) , sep ="" );  lineCount =  lineCount + 1
      if(!is.na(rcvRdata$params$enumerationData[[ip]])[1])
      {
        scriptBlock[lineCount] = paste("# enumeration options: ", paste(paste(getEnumerationStrings(rcvRdata$params$enumerationData[[ip]]),',', sep =" "), collapse ='', sep =" " ) , sep ="" );  lineCount =  lineCount + 1
      }
      
      scriptBlock[lineCount] = paste( rnamep, " = ", as.character(defaultp),"  #default",   sep ="" );  lineCount =  lineCount + 1
      
      scriptBlock[lineCount] = paste("if exists(\"", gn_namep, "\")) \n{", sep =""); lineCount =  lineCount + 1
      scriptBlock[lineCount] = paste(rnamep, " = ", gn_namep, sep =""); lineCount =  lineCount + 1
      if(!is.null(minp) && !is.null(maxp))
      {
        scriptBlock[lineCount] = paste(rnamep, " = max( min (", rnamep, ", ", as.character(maxp), "), ", 
                                       as.character(minp), ")", sep =""); lineCount =  lineCount + 1
      }
      
      
      scriptBlock[lineCount] = paste("}\n",sep =""); lineCount =  lineCount + 1
      
    }
  }
  
  return (scriptBlock)
  
  
}

