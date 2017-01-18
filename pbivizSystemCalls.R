source("rcvUtilities.R")
source("rcv2files.R")


pbivizPackageSystemCall <- function(pbivizOutFolderPath)
{
  tempPath = getwd()
  setwd(pbivizOutFolderPath)
  callCommand = "pbiviz package"
  system(callCommand, wait = TRUE)
  setwd(tempPath)
}

pbivizNewSystemCall <- function(parentPbivizOutFolder,pbivizOutName,replaceIfExist = FALSE)
{
  subDir = file.path(parentPbivizOutFolder,pbivizOutName)
  actionFlag = ((!file.exists(subDir)) || (file.exists(subDir) && replaceIfExist)) 
  if(actionFlag)
  {
    tempPath = getwd()
    setwd(parentPbivizOutFolder)
    #pbiviz new sampleCorrPlotRVisual -t rvisual
    callCommand = paste("pbiviz new",pbivizOutName,"-t rvisual",sep =" ")
    system(callCommand, wait = TRUE)
    setwd(tempPath)
    
  
      
  }
  else
    print(paste( subDir, "already exists."))
}


CopyFilesFromFolder <- function(dirWithfiles2Copy, toDir, replaceIfExist = FALSE)
{
  ddd = dir(dirWithfiles2Copy, all.files = TRUE)
  ddd = ddd[ddd!='.']
  ddd = ddd[ddd!='..']
  for (d in ddd)
  {
    print(paste("copying", d)) 
    file.copy(from=file.path(dirWithfiles2Copy,d), to = toDir, recursive = TRUE,  copy.mode = TRUE, overwrite = replaceIfExist)
  }
}


ConvertR2pbivizFull = function(rcvJsonName , parentFolder=NULL, pbivizOutName = NULL, rScriptName = NULL, rFuncName = NULL,
                               replaceIfExist = FALSE, dirProjectR2pbiviz = getwd(), dirWithFiles2Copy = NULL, templateFolder =NULL)
{
  if(is.null(dirWithFiles2Copy))
    dirWithFiles2Copy  = file.path(dirProjectR2pbiviz,"pbivizFiles2Copy")
  
  if(is.null(templateFolder))
    templateFolder = file.path(dirProjectR2pbiviz,"PowerBI-visuals-template")
  
  rcvRdata = rcvJson2rdata(rcvJsonName)
  
  # fill in all NULL inputs from rcvRdata
  if(is.null(parentFolder))
    parentFolder = rcvRdata$info$nameParentDir
  
  if(is.null(pbivizOutName))
    pbivizOutName = rcvRdata$info$namePbivizDir
  
  if(is.null(rScriptName))
    rScriptName = rcvRdata$sourceRscript["inputRscriptName"]
  
  if(is.null(rFuncName))
    rFuncName = rcvRdata$sourceRscript["funcName"]
  
  
  
  
  
  # create new folder (system call)
  pbivizNewSystemCall(parentPbivizOutFolder = parentFolder, pbivizOutName = pbivizOutName, replaceIfExist = replaceIfExist)
  
  
  pbivizOutFolderPath = file.path(parentFolder, pbivizOutName)
  
  #templates
  capTemplateFile <- file.path(templateFolder,"template_capabilities.json")
  visualtsTemplateFile <- file.path(templateFolder,"src","template_visual.ts")
  depTemplateFile <- file.path(templateFolder,"template_dependencies.json")
  pbivizJsonTemplateFile <- file.path(templateFolder,"template_pbiviz.json")
  
  #files to prepare
  capOutFile <- file.path(pbivizOutFolderPath,"capabilities.json")
  visualtsOutFile <- file.path(pbivizOutFolderPath,"src", "visual.ts")
  depOutFile <- file.path(pbivizOutFolderPath,"dependencies.json")
  pbivizJsonOutFile <- file.path(pbivizOutFolderPath,"pbiviz.json")
  rScriptOutFile <- file.path(pbivizOutFolderPath,"script.r")
  
  # generate: capabilities.json, visual.ts, dependencies.json, pbiviz.json 
  rcv2capabilities(rcvFile = rcvJsonName, capOutFile, capTemplateFile)
  rcv2visualts(rcvFile = rcvJsonName, visualtsOutFile, visualtsTemplateFile)
  rcv2dependencies(rcvFile = rcvJsonName, depOutFile, depTemplateFile)
  rcv2pbivizJson(rcvFile = rcvJsonName, pbivizJsonOutFile, pbivizJsonTemplateFile)
  
  if(!is.null(dirWithFiles2Copy))
  {
    CopyFilesFromFolder(dirWithFiles2Copy, toDir = pbivizOutFolderPath, replaceIfExist = TRUE)
  }
  
  #generate script.r
  if(!is.null(rScriptName))
    rcv2scriptR(rcvFile  = rcvJsonName, scriptRname = rScriptName, functionRname = rFuncName, newScriptRName = rScriptOutFile)
  
  # package pbiviz (system call)
  pbivizPackageSystemCall(file.path(parentFolder, pbivizOutName))
  
}




