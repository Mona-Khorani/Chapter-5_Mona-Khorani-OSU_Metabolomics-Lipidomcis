m=read.csv("volcano-CortxNOINOS-NormData_Pareto_LogTra_TIA.csv", sep=",")


with(m, plot(log2FC,neglogp, pch=20, main="Volcano plot for comparision of Tg0 vs WT0",xlab="Log2(Fold Change)",ylab="-Log10(pvalue)", cex=1.6, xlim=c(-2,2)))

with(subset(m, pval<.01 ), points(log2FC,neglogp, pch=24,cex=1.6, col="dark red"))
with(subset(m, pval<0.05& pval>.01), points(log2FC,neglogp, pch=24,cex=1.6, col="dark orange"))

legend(0.8,4.5, legend=c("pvalue<0.01", "pvalue <0.05 & >0.01"),
       col=c("red", "dark orange "), pch=20, cex=0.9)

geom_vline(xintercept=c(-0.6, 0.6), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")


D=c(1:180)


#For coloring only pval
#ggplot
m$fcthre <- "pvalue >0.05"
m$fcthre[m$pval < 0.01& m$pval<0.01] <- "pvalue <0.01"
m$fcthre[m$pval < 0.05& m$pval>0.01 ] <- "pvalue <0.05 & >0.01"

m$labelmet=NA
m$labelmet[m$fcthre == "pvalue <0.01"] <- m$Metabolite[m$fcthre == "pvalue <0.01"]
m$labelmet[m$fcthre == "pvalue <0.05 & >0.01"] <- m$Metabolite[m$fcthre == "pvalue <0.05 & >0.01"]

#k=m$Metabolite[m$fcthre == "pvalue <0.05 & >0.01"]
#k=c(1:30)
#m$labelmet[m$fcthre == "pvalue <0.05 & >0.01"] <- k
#m$labelmet[m$fcthre == "pvalue <0.05"] <- k

png(file="pngnew107.tiff", width=7500, height=7073,res = 340)


library(ggrepel)
# plot adding up all layers we have seen so far
ggplot(data=m, aes(x=log2FC, y=neglogp, col=m$fcthre, label=labelmet) )+
  geom_point(size=6.5) + labs(color = "p-value", face="bold")+
  guides(color = guide_legend(override.aes = list(size = 6.5)))+xlim(-12, 12)+
  theme_minimal() +
  theme(axis.text.y =element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), face="bold",size=40, color = "black", hjust=2),axis.text.x =element_text(face="bold",size=40,color = "black", vjust=-1,margin = margin(t = 0, r = 0, b = 10, l = 0)),
        axis.line.x = element_line(color="black", size = 1) ,
        axis.line.y = element_line(color="black", size = 1),
        plot.title = element_text(hjust=0.5, vjust=2, face="bold",margin = margin(b = 25), size=50),
        legend.text = element_text(face = "bold",size=28),
        legend.key.width=unit(1,"lines"),
        legend.position=c(0.81,0.99),legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black", linetype = "dashed"),
        legend.justification=c(0, 1), 
        plot.margin = unit(c(1.01, 12, 0.5, 0.5), "line"),
        legend.title = element_text(face = "bold",size=30),
        axis.title.x = element_text(face="bold", size=42,margin = margin(t =14, r = 0, b = 0, l = 0)),
        axis.title.y=element_text(face="bold",size=42,margin = margin(t =0, r = 14, b = 0, l = 0)),
        
  )+
  geom_text_repel(fontface="bold",size=12,max.overlaps = Inf, segment.size = 0.6,seed = 42,box.padding =0.5 ,
                  force = 2,point.padding = unit(1, "lines")
  ) +
  scale_color_manual(values=c("red", "slateblue", "black")) +
  geom_vline(xintercept=c(-0.6, 0.6), col="red", size=2) +
  geom_hline(yintercept=-log10(0.05), col="red",size=2)+
  labs(title="Volcano plot for comparision of Cortx NOI versus NOS", x="Log2(Fold Change)",y="-Log10(pvalue)")

graphics.off()



unit(0.52, "lines")

