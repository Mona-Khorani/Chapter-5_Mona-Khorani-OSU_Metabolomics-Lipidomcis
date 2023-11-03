
#Irradiated 
Data=read.csv("Lipids-Behav-FC_Cortx-Irr.csv")

parameters2=data.frame(index=seq_len(135),estimatSpearmanNONAdjst	= NA,
                       pvalSpearmanNONAdjst = NA,	estimatSpearmanFDRAdjst	=NA,
                       pvalSpearmanFDRAdjst = NA)

for (i in 1:135){
  ARn=cor.test(Data$ContTot_PctFrze
               , Data[,i+20], method=c( "spearman"), adjust="none")
  parameters2[i,2]=ARn$estimate
  parameters2[i,3]=ARn$p.value
  ARn=cor.test(Data$ContTot_PctFrze
               , Data[,i+20], method=c( "spearman"), adjust="fdr")
  parameters2[i,4]=ARn$estimate
  parameters2[i,5]=ARn$p.value
}

write.csv(parameters2,"Lipids-Behav-FC_Cortx-Irr-result.csv")



#Sham
Data=read.csv("Lipids-Behav-FC_Cortxsham.csv")

parameters2=data.frame(index=seq_len(135),estimatSpearmanNONAdjst	= NA,
                       pvalSpearmanNONAdjst = NA,	estimatSpearmanFDRAdjst	=NA,
                       pvalSpearmanFDRAdjst = NA)

for (i in 1:135){
  ARn=cor.test(Data$ContTot_PctFrze
               , Data[,i+20], method=c( "spearman"), adjust="none")
  parameters2[i,2]=ARn$estimate
  parameters2[i,3]=ARn$p.value
  ARn=cor.test(Data$ContTot_PctFrze
               , Data[,i+20], method=c( "spearman"), adjust="fdr")
  parameters2[i,4]=ARn$estimate
  parameters2[i,5]=ARn$p.value
}

write.csv(parameters2,"Lipids-Behav-FC_Cortxsham-result.csv")