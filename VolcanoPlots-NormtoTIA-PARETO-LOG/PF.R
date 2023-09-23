

install.packages("dplyr")
library(tidyverse)
library(dplyr)
library(magrittr)
library(readxl)
library(tidyverse)
library(openxlsx)
library(MetaboAnalystR)
library(qs)


mSet<-InitDataObjects("conc", "stat", FALSE)
mSet<-Read.TextData(mSet, "CortxFCIFCS-NormData_Pareto_LogTra_TIA.csv", "colu", "disc");
mSet<-SanityCheckData(mSet)
mSet<-ReplaceMin(mSet);
mSet<-SanityCheckData(mSet)
mSet<-FilterVariable(mSet, "iqr", 5, "F", 25, F)
mSet<-PreparePrenormData(mSet)
mSet<-Normalization(mSet, "NULL", "NULL", "NULL", ratio=FALSE, ratioNum=20)
mSet<-PlotNormSummary(mSet, "norm_0_", "png", 300, width=NA)
mSet<-Volcano.Anal(mSet, FALSE, 1.0, 0, T, 4.0, FALSE, "raw")
mSet<-PlotVolcano(mSet, "volcano_4_",1, 0, "png", 300, width=NA)