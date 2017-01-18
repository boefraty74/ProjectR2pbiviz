#rcv2files.R
source("rcvUtilities.R")

rcv2capabilities = function(rcvFile, capOutFile, capTemplateFile)
{
  rcvRdata = rcvJson2rdata(rcvFile)
  rcvJson <- RJSONIO::fromJSON(rcvFile)
  
  capTemplate <- RJSONIO::fromJSON(capTemplateFile)
  
  capOutData <- capTemplate #initialization
  
  #roles block
  NR = length(rcvRdata$roles)
  capOutData$dataRoles = rcvJson$dataRoles
  for(r in (1:NR))
    capOutData$dataRoles[[r]]$conditions = NULL
  #components of "dataViewMappings"
  #change conditions
  bbb = list()
  for(r in (1:NR))
  {
    bbb[r]=rcvRdata$roles[[r]]$conditions[[1]]["max"]
    names(bbb)[r] =rcvRdata$roles[[r]]$name
    names(bbb[[r]])="max"
  }
  capOutData$dataViewMappings[[1]]$conditions[[1]] = bbb
  
  #change scriptResult
  # capOutData$dataViewMappings[[1]]$scriptResult$dataInput$table$rows$select
  bbb = list()
  for(r in (1:NR))
  {
   
    bbb[[r]] =list(`for` = rcvRdata$roles[[r]]$name)
    names(bbb[[r]]$`for`) = "in"
  }
  
  capOutData$dataViewMappings[[1]]$scriptResult$dataInput$table$rows$select = bbb  
  
  #parameters block 
  rcv_script = capOutData$objects$rcv_script
  capOutData$objects = rcvJson$parameters
  capOutData$objects$rcv_script = rcv_script
  
  
  NG = length(rcvRdata$groups) # works wrong with only 1 group 
  NG = length(rcvRdata$groups$name)
  #clean parameters
  for(g in (1:NG))
  {
    if(!is.null(capOutData$objects[[g]]$properties))
    {
      NP = length(capOutData$objects[[g]]$properties)
      for (p in (1:NP))
      {
        capOutData$objects[[g]]$properties[[p]]$default = NULL
        capOutData$objects[[g]]$properties[[p]]$min = NULL
        capOutData$objects[[g]]$properties[[p]]$max = NULL
        capOutData$objects[[g]]$properties[[p]]$rname = NULL
      }
    }
    
  }
  
  
  write(RJSONIO::toJSON(capOutData, pretty = TRUE) ,capOutFile)
}

rcv2visualts  = function(rcvFile, visualtsOutFile, visualtsTemplateFile)
{
  searchFor =c("//PBI_TEMPLATE_1","//PBI_TEMPLATE_2","//PBI_TEMPLATE_3","//PBI_TEMPLATE_4","//PBI_TEMPLATE_5")
  
  rcvRdata = rcvJson2rdata(rcvFile)
  rcvJson <- RJSONIO::fromJSON(rcvFile)
  
  fin =file(visualtsTemplateFile,open="r")
  allLines=readLines(fin, warn = FALSE)
  close(fin)
  
  lseq = seq(1,length(allLines))
  
  currSearch = 1
  placeHolders = rep(NA,length(searchFor))
  for (li in lseq)
  {
    placeHolders[currSearch] = as.numeric(regexpr(searchFor[currSearch],allLines[li]))
    if(placeHolders[currSearch]!=-1)
    {
      placeHolders[currSearch] = li
      currSearch = currSearch +1
    }
    if( currSearch> length(searchFor))
      break;
  }
  
  #print(placeHolders)
  fout = file(visualtsOutFile,open="w")
  star = 1
  for(j in (1:length(searchFor)))
  {
    lll = allLines[star:placeHolders[j]]
    writeLines(lll, con = fout)
    star = placeHolders[j]+1
    
    insertLines ="***"
    insertLines = GetCodeIntoSourceTS(rcvRdata,rcvJson,j)
    writeLines(insertLines, con = fout)
    
  }
  lll = allLines[star:length(allLines)]
  writeLines(lll, con = fout)
  
  close(fout)
  
}

rcv2dependencies  = function(rcvFile, depOutFile, depTemplateFile = NULL)
{
  rcvRdata = rcvJson2rdata(rcvFile)
  write(RJSONIO::toJSON(rcvRdata$dependencies),depOutFile)
}

rcv2pbivizJson  = function(rcvFile, pbivizJsonOutFile, pbivizJsonTemplateFile)
{
  rcvRdata = rcvJson2rdata(rcvFile)
  write(RJSONIO::toJSON(rcvRdata$pbiviz),pbivizJsonOutFile)
  
}

rcv2scriptR  = function(rcvFile, scriptRname = NULL, functionRname = NULL, newScriptRName, placeHolder = NULL)
{
  rcvRdata = rcvJson2rdata(rcvFile)
  rcvJson <- RJSONIO::fromJSON(rcvFile)
  
  # generate smartParamsBlock
  smartParamsBlock = GenerateParamsBlockForRscript(rcvRdata)
  
  fin = file(scriptRname, "r")
  inLines = readLines(fin, warn = FALSE)
  close(fin)
  
  NL = length(inLines); LLL = 1:NL
  
  lineStartFunc = -1
  lineStartBlock = -1 
  symbStartBlock = -1
  placeHolderLine = -1
  
  if(is.null(placeHolder))
  {
  #TODO Find start symbol of function name 
  #TODO Find start symbol of "\\{" just after function name  
  #Put placeHolder instead of function(***)
    
    for(li in LLL)
    {
      myline = inLines[li]
      if(grepl("function", myline) && grepl(functionRname, myline) && (grepl("=", myline) || grepl("<-", myline)))
        lineStartFunc = li
      if(lineStartFunc > 0 && lineStartBlock <= 0) # look for { and remember symbol
      {
        placeHolderLine = lineStartFunc
        inLines[li] = " "
        if(grepl("\\{", myline))
        {
          lineStartBlock = li
          symbStartBlock =gregexpr("\\{", myline)[[1]][1]
          inLines[li] = substr(myline,symbStartBlock,1000)
          break
        }
        
      }
    }
    
    
  } else {
    #find placeHolder
    for(li in LLL)
    {
      if(grepl(placeHolder, inLines[li]))
        { placeHolderLine = li; break;}
    }
  }
  
  #replace placeHolder by smart params block
  fout = file(newScriptRName, "w")
  writeLines(inLines[1:placeHolderLine-1], fout)
  writeLines(smartParamsBlock, fout)
  writeLines(inLines[placeHolderLine:NL], fout)
  
  close(fout)
  
  
}