#readFuncData

readFuncData = function(scriptName, funcName)
{
  
  #source the script (?)
  rm(list = ls(all = TRUE))
  source(scriptName, echo = FALSE, verbose = FALSE)
  #grab the function handle
  fh = get(funcName)
  
  params= formalArgs(fh) # names of parameters
  defaults = formals(fh)
  withDefault = (lapply(defaults,class)!= "name")
  
  types = lapply(defaults,class)
  types[!withDefault] = NA
  #params, defaults, types 
  dataRfunc = list(params = params, withDefault = withDefault, types= types )
  
  #dependencies
  loadedPackages = (.packages())
  allPackages = as.data.frame(library()$results)
  dataRfunc$packages = allPackages[allPackages$Package %in% loadedPackages,]
  
  return(dataRfunc)
}


#TODO: convert dataRfunc to rcvRdata and save it to JSON if needed 
initializeRCVjson = function(scriptName, funcName, jsonTemplate = NULL, outJsonName = NULL, extraParams = NULL)
{
  fD = readFuncData (scriptName, funcName)
  
  # with defaults -> params 
  # without defaults (or NULL) -> roles
  # logicals --> booleans
  # characters --> strings 
  # find colors 
  # NULL --> roles
  # NA --> strings
  
  
  
  
  
  
}


#example 
scriptName = "c:/Users/boefraty/projects/PBI/R/custom_R_visuals/dbscan_clustering/function_visGal_clustering_with_outliers.R"
funcName = "funcClusterDbscan"
fD = readFuncData (scriptName, funcName)