## code for CAZymes in maize and rice soils
## https://github.com/Micropenglab/Pipeline-for-Maize-Soils
#Email: 1372890765@qq.com 
#Author: Xingjie Wu
# Fig. 1 Soil carbon degradation genes in maize and rice soils
design<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/design.csv",row.names = 1)
#boxplot
aa<-design
library(ggplot2)
#Polymer
aa$Classification<-c(rep("Polymer (%)",72))
p1=ggplot(aa,mapping = aes(y=Polymer,x=Habitat,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=2)+
  labs(x="", y="Relative abundance", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  facet_grid( ~ aa$Classification, drop=TRUE,scale="free",space="free_x")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=11))
#Cellulose
aa$Classification<-c(rep("Cellulose (%)",72))
p2=ggplot(aa,mapping = aes(y=aa$Cellulose,x=aa$Habitat,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=2)+
  labs(x="", y="", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  facet_grid( ~ aa$Classification, drop=TRUE,scale="free",space="free_x")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=11))
#Chitin
aa$Classification<-c(rep("Chitin (%)",72))
p3=ggplot(aa,mapping = aes(y=aa$Chitin,x=aa$Habitat,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=2)+
  labs(x="", y="", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  facet_grid( ~ aa$Classification, drop=TRUE,scale="free",space="free_x")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=11))
#Hemicellulose
aa$Classification<-c(rep("Hemicellulose (%)",72))
p4=ggplot(aa,mapping = aes(y=aa$Hemicellulose,x=aa$Habitat,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=2)+
  labs(x="", y="", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  facet_grid( ~ aa$Classification, drop=TRUE,scale="free",space="free_x")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=11))
#Pectin
aa$Classification<-c(rep("Pectin (%)",72))
p5=ggplot(aa,mapping = aes(y=aa$Pectin,x=aa$Habitat,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=2)+
  labs(x="", y="", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  facet_grid( ~ aa$Classification, drop=TRUE,scale="free",space="free_x")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=11))
#Xylan
aa$Classification<-c(rep("Xylan (%)",72))
p6=ggplot(aa,mapping = aes(y=aa$Xylan,x=aa$Habitat,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=2)+
  labs(x="", y="", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  facet_grid( ~ aa$Classification, drop=TRUE,scale="free",space="free_x")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=11))

#Fig. 1 Spearman's correlations between DOC and carbon degradation genes in maize and rice soils
design<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/design.csv",row.names = 1)
df<-design[1:36,] #maize soils
#Polymer
p1=ggplot(df,aes(y=df$DOC,x=df$Polymer,color=Habitat))+#x,y
  geom_point(size=3.5,aes())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  geom_smooth(method=lm,level=0.95,se=T,size=1.2)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
   stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.995,aes(color=Habitat))+#或spearman
  theme(axis.text=element_text(colour='black',size=9)) +
  labs(y="DOC (mg kg-1)",x="Polymer (%)")
#Pectin
p2=ggplot(df,aes(y=df$DOC,x=df$Pectin,color=Habitat))+#x,y
  geom_point(size=3.5,aes())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  geom_smooth(method=lm,level=0.95,se=T,size=1.2)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.995,aes(color=Habitat))+
  theme(axis.text=element_text(colour='black',size=9)) +
  labs(y="DOC (mg kg-1)",x="Pectin (%)")
#Xylan
p3=ggplot(df,aes(y=df$DOC,x=df$Xylan,color=Habitat))+
  geom_point(size=3.5,aes())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  geom_smooth(method=lm,level=0.95,se=T,size=1.2)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.995,aes(color=Habitat))+
  theme(axis.text=element_text(colour='black',size=9)) +
  labs(y="DOC (mg kg-1)",x="Xylan (%)")
#Hemicellulose
p4=ggplot(df,aes(y=df$DOC,x=df$Hemicellulose,color=Habitat))+
  geom_point(size=3.5,aes())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  geom_smooth(method=lm,level=0.95,se=T,size=1.2)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.995,aes(color=Habitat))+
  theme(axis.text=element_text(colour='black',size=9)) +
  labs(y="DOC (mg kg-1)",x="Hemicellulose (%)")
#Cellulose
p5=ggplot(df,aes(y=df$DOC,x=df$Cellulose,color=Habitat))+
  geom_point(size=3.5,aes())+#点大小
  scale_color_manual(values=c("#D35800","#006EB0"))+
  geom_smooth(method=lm,level=0.95,se=T,size=1.2)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.995,aes(color=Habitat))+
  theme(axis.text=element_text(colour='black',size=9)) +
  labs(y="DOC (mg kg-1)",x="Cellulose (%)")
#Chitin
p6=ggplot(df,aes(y=df$DOC,x=df$Chitin,color=Habitat))+
  geom_point(size=3.5,aes())+#点大小
  scale_color_manual(values=c("#D35800","#006EB0"))+
  geom_smooth(method=lm,level=0.95,se=T,size=1.2)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  stat_cor(method = "spearman",label.x.npc ="left",label.y.npc = 0.995,aes(color=Habitat))+
  theme(axis.text=element_text(colour='black',size=9)) +
  labs(y="DOC (mg kg-1)",x="Chitin (%)")

#Fig.2 PCoA and mantel test of cazyme taxa and gene
library(ggplot2)
library(dplyr)
library(vegan)
library(ape)
cazy<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/cazyme_taxa.csv",row.names = 1)
cazy_gene<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/cazyme_gene.csv",row.names = 1)
design<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/design.csv",row.names = 1)
#CAZyme taxa
data <- vegdist(t(cazy[,9:80]), method = "bray")
#PCoA analysis
pcoa<- pcoa(data, correction = "none", rn = NULL)
#plot
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)
aa<- data.frame(rownames(pcoa$vectors),PC1,PC2)
colnames(aa) <-c("sample","PC1","PC2")
points<-cbind(design,aa)
#color by habitat
p1=ggplot(points, aes(PC1, PC2,color=Habitat))+
  geom_point(aes(),size=3.3)+
  scale_colour_manual(values=c("#D35800","#006EB0"))+
  theme(text=element_text(size=9))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  stat_ellipse(aes(fill=Habitat),geom = "polygon",level = 0.95,size=1.1,alpha = 0.001)+
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  scale_fill_manual(values=c("#C18E6A","#006EB0"))+
  theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9))

#CAZyme gene
# PCA分析 
data <- vegdist(t(cazy_gene), method = "bray")
#进行PCoA分析
pcoa<- pcoa(data, correction = "none", rn = NULL)
#制作绘图文件
PC1 = pcoa$vectors[,1]
PC2 = pcoa$vectors[,2]
pc1 <-floor(pcoa$values$Relative_eig[1]*100)
pc2 <-floor(pcoa$values$Relative_eig[2]*100)
aa<- data.frame(rownames(pcoa$vectors),PC1,PC2)
colnames(aa) <-c("sample","PC1","PC2")
points<-cbind(design[,],aa)
#color by phylum
p2=ggplot(points, aes(PC1, PC2,color=Habitat))+
  geom_point(aes(),size=3.3)+
  scale_colour_manual(values=c("#D35800","#006EB0"))+
  theme(text=element_text(size=9))+
  geom_vline(aes(xintercept = 0),linetype="dotted")+
  geom_hline(aes(yintercept = 0),linetype="dotted")+
  stat_ellipse(aes(fill =Habitat),geom = "polygon",size=1.1,level = 0.95,alpha = 0.001)+
  xlab(paste("PC1 ( ",pc1,"%"," )",sep="")) + 
  ylab(paste("PC2 ( ",pc2,"%"," )",sep=""))+
  theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9))


#Fig. 2 Mantel test for CAZyme taxa and gene
#mantel test
library(ggcor)
library(ggplot2)
library(dplyr)
cazy_taxa1<-cazy_taxa[,9:80]
spe<-rbind(cazy_taxa1,cazy_gene) # 6757, 518
spe<-t(spe[,37:72])
colnames(design)[20]<-"AGS"
soil<-design[37:72,c(6:20)]
soil<-soil[,-c(4,5)]
#mantel test
mantel <- mantel_test(spe, soil,spec.select =list(CAZyme_taxa = 1:6757, CAZyme_gene = 6758:7275)) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf), labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p.value, breaks = c(-Inf, 0.01, 0.05, Inf), labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
#mantel<-subset(mantel,mantel$p.value<=0.05)
#visualization
options(ggcor.link.inherit.aes = T) 
p=quickcor(soil,cor.test = TRUE, type = "upper", show.diag = F) +
  geom_square() + 
  anno_link(aes(colour = pd, size = rd), data = mantel,curvature=0.1) +
  scale_size_manual(values = c(0.8,1.5,2)) + 
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_colour_manual(values = c("#D95F02","#1B9E77","gray88")) +
  # scale_fill_gradient2(midpoint = 0, low = "#0172B6", mid = "white",
  # high ="#BD3C29" , space = "Lab" )+
  guides(size = guide_legend(title = "Mantel's r",override.aes = list(colour = "grey35"),order = 2),
         colour = guide_legend(title = "Mantel's p", override.aes = list(size = 3),order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))+expand_axis(x = -6) 

#Fig 3 Polymer decomposition capacity and polymer decomposers
df<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/Bacterial_community_CAZyme.csv",row.names = 1)
design<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/design_new.csv",row.names = 1)
library(ggplot2)
library(ggpubr)
# Abundance of polymer decomposers
aa<-cbind(design,df[,13:20])
#Proteo
p1=ggplot(aa,mapping = aes(x=Habitat,y=aa$Proteo_polymer....,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=3)+
  labs(x="", y="Relative abundance (%)", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=9))
#Acido
p2=ggplot(aa,mapping = aes(x=Habitat,y=aa$Acido_polymer....,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=3)+
  labs(x="", y="Relative abundance (%)", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=9))
#Gemmat
p3=ggplot(aa,mapping = aes(x=Habitat,y=aa$Gemmat_polymer....,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=3)+
  labs(x="", y="Relative abundance (%)", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=9))
#Actino
p4=ggplot(aa,mapping = aes(x=Habitat,y=aa$Actino_polymer....,color=Habitat))+
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.3,color="gray2",size=1)+
  geom_boxplot(aes(),notch = F,color="gray2",size=0.7)+
  geom_jitter(binaxis="y",position = position_jitter(0.2),satckdir="center",dotsize=0.6,size=3)+
  labs(x="", y="Relative abundance (%)", title = "")+ theme_bw(base_line_size = 1.05,base_rect_size = 1.05)+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(axis.text=element_text(colour='black',size=9))

#polymer decomposers and associated with genes
aa<-cbind(design,df[,13:20])
#Proteo
p1=ggplot(aa, aes(x=aa$Polymer, y=aa$Proteo_polymer....))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=2.5, alpha=1)+
  labs(x="Polymer (%)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,level=0.95,size=1.5,se=F)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Acido
p2=ggplot(aa, aes(x=aa$Polymer, y=aa$Acido_ratio....))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=2.5, alpha=1)+
  labs(x="Polymer (%)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,level=0.95,size=1.5,se=F)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Gemmat
p3=ggplot(aa, aes(x=aa$Polymer, y=aa$Gemmat_ratio....))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=2.5, alpha=1)+
  labs(x="Polymer (%)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,level=0.95,size=1.5,se=F)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Actino
p4=ggplot(aa, aes(x=aa$Polymer, y=aa$Actino_ratio....))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=2.5, alpha=1)+
  labs(x="Polymer (%)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,level=0.95,size=1.5,se=F)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 

#Fig.4
library(ggplot2)
aa<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/design_MAGs_four_phyla.csv",row.names = 1)
#delete out liner
aa<-aa[-131,]
#barplot
plot_data2<-aa[,c(5,39)] 
mean_df<-aggregate(plot_data2[,2],by=list(plot_data2[,1]),FUN=mean)
rownames(mean_df)<-mean_df[,1]
#error bar
sd_df<-aggregate(plot_data2[,2],by=list(plot_data2[,1]),FUN=sd)
rownames(sd_df)<-sd_df[,1]
sd_df<-sd_df[,-1]
#errobar
count<-as.data.frame(c(30,12,20,3,21,5,51,30))
sd_df<-sd_df/sqrt(count) 
se<-as.data.frame(sd_df)
plot_data1<-cbind(mean_df,se)
colnames(plot_data1)<-c("group","mean","sd")
colnames(plot_data2)<-c("group","Retive_Abundance")
#order the phylum
plot_data1$group= factor(plot_data1$group, levels=c('Proteobacteria_Maize','Acidobacteria_Maize','Gemmatimonadetes_Maize',
                                                    'Actinobacteria_Maize','Proteobacteria_Rice','Acidobacteria_Rice','Gemmatimonadetes_Rice','Actinobacteria_Rice'))
plot_data2$group= factor(plot_data2$group, levels=c( 'Proteobacteria_Maize','Acidobacteria_Maize','Gemmatimonadetes_Maize',
                                                     'Actinobacteria_Maize','Proteobacteria_Rice','Acidobacteria_Rice','Gemmatimonadetes_Rice','Actinobacteria_Rice'))
#visualization
p=ggplot()+geom_bar(data=plot_data1,mapping=aes(x=group,y=mean), 
                  fill = "white",size = 1.03, 
                  color = c("#F1CD49","#F1CD49","gray34","gray34","#0073B5","#0073B5","#C9543B","#C9543B"),position="dodge", 
                  stat="identity", width = 0.7)+  
  geom_jitter(data=plot_data2, mapping=aes(x=group,y=Retive_Abundance,fill = group,colour = group),
              size = 1.5,alpha=0.3,height = 0.01,width = 0.15)+ 
  scale_color_manual(values = c("#C9543B","#F1CD49","#0073B5","gray34","#C9543B","#F1CD49","#0073B5","gray34"))+ 
  geom_errorbar(data=plot_data1,mapping=aes(x = group,ymin = mean-sd, ymax = mean+sd),width = 0.3,
                color = c("#F1CD49","#F1CD49","gray34","gray34","#0073B5","#0073B5","#C9543B","#C9543B"),size=0.8)+
  coord_flip()+labs(y="Cellular growth (%)", x="")+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  geom_vline(aes(xintercept =4.5), size=0.8, linetype="dashed", colour="gray2")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9))

#Fig.5 Links between genome size and polymer decomposition
#Metagenomics: genome size and polymer decomposition
library(ggpubr)
library(ggplot2)
aa<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/design.csv",row.names = 1)
#Polymer
p1=ggplot(aa, aes(y=Polymer, x=Genome.size))+
  geom_jitter(aes(x=,color=Habitat),position=position_jitter(0.17), size=3, alpha=1)+
  labs(x="AGS (Mbp)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=11)) 
#Hemicellulose
p2=ggplot(aa, aes(y=Hemicellulose, x=aa$Genome.size))+
  geom_jitter(aes(x=,color=Habitat),position=position_jitter(0.17), size=3, alpha=1)+
  labs(x="AGS (Mbp)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=11)) 
#Cellulose
p3=ggplot(aa, aes(y=aa$Cellulose, x=aa$Genome.size))+
  geom_jitter(aes(x=,color=Habitat),position=position_jitter(0.17), size=3, alpha=1)+
  labs(x="AGS (Mbp)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=11)) 
#Xylan
p4=ggplot(aa, aes(y=aa$Xylan, x=aa$Genome.size))+
  geom_jitter(aes(x=,color=Habitat),position=position_jitter(0.17), size=3, alpha=1)+
  labs(x="AGS (Mbp)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=11)) 
#Pectin
p5=ggplot(aa, aes(y=aa$Pectin, x=aa$Genome.size))+
  geom_jitter(aes(x=,color=Habitat),position=position_jitter(0.17), size=3, alpha=1)+
  labs(x="AGS (Mbp)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=11)) 
#Chitin
p6=ggplot(aa, aes(y=aa$Chitin, x=aa$Genome.size))+
  geom_jitter(aes(x=,color=Habitat),position=position_jitter(0.17), size=3, alpha=1)+
  labs(x="AGS (Mbp)", y="")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=11)) 

#MAGs: genome size and polymer decomposition
aa<-read.csv("J://wxj_all_data/bins_jiaoshuo/metagenomic/cazyme/design_MAGs.csv",row.names = 1)
#Polymer
p1=ggplot(aa, aes(y=aa$Polymer.ratio, x=aa$Genome.size))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=1.5, alpha=1)+
  labs(y="", x="Genome size (Mbp)")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Cellulose
p2=ggplot(aa, aes(y=aa$Cellulose, x=aa$Genome.size))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=1.5, alpha=1)+
  labs(y="", x="Genome size (Mbp)")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Chitin
p3=ggplot(aa, aes(y=aa$Chitin,x=aa$Genome.size))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=1.5, alpha=1)+
  labs(y="", x="Genome size (Mbp)")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Xylan
p4=ggplot(aa, aes(y=aa$Xylan, x=aa$Genome.size))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=1.5, alpha=1)+
  labs(y="", x="Genome size (Mbp)")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Pectin
p5=ggplot(aa, aes(y=aa$Pectin, x=aa$Genome.size))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=1.5, alpha=1)+
  labs(y="", x="Genome size (Mbp)")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
#Hemicellulose
p6=ggplot(aa, aes(y=aa$Hemicellulose, x=aa$Genome.size))+
  geom_jitter(aes(color=Habitat),position=position_jitter(0.17), size=1.5, alpha=1)+
  labs(y="", x="Genome size (Mbp)")+
  geom_smooth(aes(color=Habitat),method=lm,se=F,level=0.95,size=1.5)+
  theme_bw(base_line_size = 1.05,base_rect_size =1.05)+
  stat_cor(aes(color=Habitat),method = "spearman",label.x.npc ="left",label.y.npc = 0.95)+
  scale_color_manual(values=c("#D35800","#006EB0"))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text=element_text(colour='black',size=9)) 
