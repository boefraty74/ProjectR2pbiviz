#flatten R-script
source("rcvUtilities.R")

#find all "source" commands and copy the code into 1 big R-script  
flattenGivenRscript = function(inScriptName, outScriptName, workingDir = NULL, listNotToSource = list())
{
   rememberWD = getwd()
   if(is.null(workingDir))
     workingDir = rememberWD
   setwd(workingDir)
  
  fin = file(inScriptName, open ="r")
  allLines = readLines(fin)
  outLines = allLines
  close(fin)
  L = length(allLines)
  
  mapNewOutLines = list(lineID= NULL, lineBlocks = list())# contains line indexes + line blocks to insert
  numSources = 0
  
  for (li in (1:L))
  {
    #find source text not commented 
    res1 = as.numeric(regexpr("source",allLines[li]))
    res2 = as.numeric(regexpr("#",allLines[li]))
    if((res1!=-1 && res2==-1) || (res1!=-1 && res1 < res2)) 
    {
      # find fileName in source(fileName)
      fileName = FindFileNameInSourceCall(allLines = allLines,targetLine = li, targetSymbol = res1)
      #call recoursively for flattenGivenRscript(fileName,outScriptName = NULL, workingDir = workingDir, listNotToSource= listNotToSource)
      if(!is.null(fileName) && !(fileName %in% listNotToSource))
      {
        
        resData = flattenGivenRscript(fileName,outScriptName = NULL, workingDir = workingDir, listNotToSource= listNotToSource)
        #replace source lines by result of previous call
        flatScript = resData$outLines
        listNotToSource = resData$listNotToSource
        #append listNotToSource
        listNotToSource[[length(listNotToSource)+1]] = fileName
        
        # map li to  flatScript for future use
        numSources = numSources+1
        mapNewOutLines$lineID[numSources] = li
        mapNewOutLines$lineBlocks[[numSources]] = flatScript
        
      }
    }
  }
  
  #apply mapNewOutLines to outLines
  outLines = pushNewLines(outLines,mapNewOutLines) 
  setwd(rememberWD)
  
  if(!is.null(outScriptName))
  {
    fout = file(outScriptName, open ="w")
    writeLines(outLines, con = fout)
    close(fout)
  }else{
    return(list(outLines= outLines, listNotToSource= listNotToSource) )
  }
  
  
}