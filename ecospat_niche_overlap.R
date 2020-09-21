# AEM, 2020

#
library(ecospat)
#

#
setwd("~/Dropbox/UF_Research/EA_ENA_ENM/GEB_Revisions/")
#

#
load("ENMEval_Outputs/Adlumia_asiatica/Adlumia_asiatica_ENMEval.RDA")
sp1 <- modeval
rm(modeval)
#

#
load("ENMEval_Outputs/Adlumia_fungosa/Adlumia_fungosa_ENMEval.RDA")
sp2 <- modeval
rm(modeval)
#

#
z1 <- ecospat.grid.clim.dyn()
#