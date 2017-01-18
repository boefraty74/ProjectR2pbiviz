#R2pbivizScript.R

source("pbivizSystemCalls.R")
source("rcv2files.R")




# # create new folder (system call)
# pbivizNewSystemCall(parentPbivizOutFolder,pbivizOutName,replaceIfExist = FALSE,files2Copy = files2Copy)
# 
# #---here suppose to generate rcvFile JSON ---
# 
# # generate: capabilities.json, visual.ts, dependencies.json, pbiviz.json , 
# rcv2capabilities(rcvFile,capOutFile,capTemplateFile)
# rcv2visualts(rcvFile,visualtsOutFile,visualtsTemplateFile)
# rcv2dependencies(rcvFile,depOutFile,depTemplateFile)
# rcv2pbivizJson(rcvFile,pbivizJsonOutFile,pbivizJsonTemplateFile)
# 
# 
# 
# # package pbiviz (system call)
# pbivizPackageSystemCall(file.path(parentPbivizOutFolder,pbivizOutName))




# rcvFile <- "c:\\Users\\boefraty\\projects\\PBI\\R\\ProjectR2pbiviz\\inputs\\rcv.json"
# 
# 
# files2Copy = 'c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/pbivizFiles2Copy'
# 
# capTemplateFile <- "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/PowerBI-visuals-template/template_capabilities.json"
# visualtsTemplateFile <- "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/PowerBI-visuals-template/src/template_visual.ts"
# depTemplateFile <- "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/PowerBI-visuals-template/template_dependencies.json"
# pbivizJsonTemplateFile <- "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/PowerBI-visuals-template/template_pbiviz.json"
# 
# 
# #parentDirpbivizOutFolder = dirname(pbivizOutFolderPath)
# parentPbivizOutFolder = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz"
# pbivizOutName = "newViz"
# 
# pbivizOutFolderPath = file.path(parentPbivizOutFolder, pbivizOutName)
# 
# capOutFile <- file.path(pbivizOutFolderPath,"capabilities.json")
# visualtsOutFile <- file.path(pbivizOutFolderPath,"visual.ts")
# depOutFile <- file.path(pbivizOutFolderPath,"dependencies.json")
# pbivizJsonOutFile <- file.path(pbivizOutFolderPath,"pbiviz.json")
# 


# ConvertR2pbivizFull(rcvJsonName = rcvFile,
#                     parentFolder = parentPbivizOutFolder,
#                     pbivizOutName = "newRpbiviz",
#                     rScriptName = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/inputs/rName.r", 
#                     rFuncName = "GenerateParamsBlockForRscript",
#                     replaceIfExist = FALSE, 
#                     dirWithFiles2Copy  = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/pbivizFiles2Copy", 
#                     templateFolder = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/PowerBI-visuals-template")


#rcvFileDbscan = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/inputs/dbscan_clustering/rcv_dbscan_ver3.json"
rcvFileTsDecomp = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/inputs/tsDecomp/rcv_ts_decomp_v3.json"

ConvertR2pbivizFull(rcvJsonName = rcvFileTsDecomp,
                    dirProjectR2pbiviz = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz")

