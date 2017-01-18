#R2pbivizScript.R

dirProjectR2pbiviz = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz"
rcvFileTsDecomp = "c:/Users/boefraty/projects/PBI/R/ProjectR2pbiviz/inputs/tsDecomp/rcv_ts_decomp_v1.json"

setwd(dirProjectR2pbiviz)

source("pbivizSystemCalls.R")

ConvertR2pbivizFull(rcvJsonName = rcvFileTsDecomp,
                    dirProjectR2pbiviz = dirProjectR2pbiviz)

