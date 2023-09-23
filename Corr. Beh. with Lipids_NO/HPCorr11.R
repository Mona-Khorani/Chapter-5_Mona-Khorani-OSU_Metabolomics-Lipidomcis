

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
install.packages("ggpubr")
library("ggpubr")


Data=read.csv("Metabolomics-Behav-NO_Cotx-Irr.csv")



parameters2=data.frame(index=seq_len(145), pvalAdjSpear=NA,pvalunadjSpear=NA,estimateSpear=NA,
                       pvalAdjPear=NA,pvalunadjPearson=NA,estimatePearson=NA)

for (i in 1:145){
  ARn=cor.test(Data$PctTime_FamiliarObj, Data[,i+5], method=c( "pearson"), adjust="none")
  parameters2[i,4]=ARn$estimate
  parameters2[i,2]=ARn$p.value
  ARn=cor.test(Data$PctTime_FamiliarObj, Data[,i+5], method=c( "pearson"), adjust="none")
  parameters2[i,3]=ARn$p.value
  ARn=cor.test(Data$PctTime_FamiliarObj, Data[,i+5], method=c( "pearson"), adjust="fdr")
  parameters2[i,7]=ARn$estimate
  parameters2[i,5]=ARn$p.value
  ARn=cor.test(Data$PctTime_FamiliarObj, Data[,i+5], method=c( "pearson"), adjust="fdr")
  parameters2[i,6]=ARn$p.value
}

write.csv(parameters2,"HASpearmancor.csv")
