##Figures for N.melanderi Age and experience-dependent plasticity
##MA Hagadorn
##Last Modified Images: 9/20/2020
##Last Modified Text: 9/24/2020
##Data Used: Nmel_NELrRep2016_structurevolume_proportiondata.csv; generated as a csv output while running Nmel_Experiment1_DataAnalysis


#loadlibraries
library(ggplot2)
library(reshape2)
library(cowplot)


#loading in the proportion data (relative to whole brain)
##Using setwd() you need to designate your respective file path; MAH file path excluded for privacy
setwd() #designate file path
reldata <- read.table("Nmel_NELrRep2016_structurevolume_proportiondata.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
reldata <-reldata[,-c(1,6,8:14)]



#Assigning factors
reldata$tracer <- factor(reldata$tracer)
reldata$bed <- factor(reldata$bed)

#Verify that factors are assigned appropriately
print(str(reldata))





#Melt Data into Appropriate Format
##Make subset for N, KC, AL
NKCAL <- reldata[,c("specimen_ID", "code","neuropil_relWB", "kcs_relWB", "als_relWB")]

#melt data
melt_NKCAL <- melt(NKCAL)

#assign order levels
melt_NKCAL$code <- factor(melt_NKCAL$code, levels=c("NewEmerge", "LabReared", "Nesting"))


addline_format <- function(x,...){
  gsub('\\s','\n',x)
}


#Assigning specific sizes, changes to figures will reflect changes made here. Keep consistent with Exp.2 figures
generalsize <- 18
legendsize <- 13
lettersize <- 5.2



#GENERATE FIGURE FOR NEUROPIL, KCS, AND ALS
par(mar=c(0,2,4,6)) #(bottom, left, top, right)
NKCAL_plot <- ggplot(melt_NKCAL, aes(x=variable, y=value, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black") +
  scale_fill_manual(values = c("white", "gray70", "gray38"), labels=c("Newly-Emerged", "Lab-Reared", "Nesting")) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=.2, seed=667), size=3.1, stroke=.7) +
  geom_point(aes(color=factor(code)), shape=16, position=position_jitterdodge(jitter.width=.2, seed=667), alpha=.7, size=3) +
  scale_color_manual(values=c("NewEmerge"="mediumblue", "LabReared"="violetred4","Nesting"="darkorange"), labels=c("Newly-Emerged", "Lab-Reared", "Nesting"), ) +
  
  #Significance annotations from post-hoc results
  annotate(geom="text", x=1-0.25, y=max(NKCAL[NKCAL$code=="NewEmerge", 3]) +.006, label="a", color="black", size = lettersize) + #NE
  annotate(geom="text", x=1, y=max(NKCAL[NKCAL$code=="LabReared", 3]) +.006, label="a", color="black", size = lettersize) + #LR
  annotate(geom="text", x=1+0.25, y=max(NKCAL[NKCAL$code=="Nesting", 3]) +.006, label="b", color="black", size = lettersize) + #REP
  ggtitle("(a)") +
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.03,.181), breaks=seq(0.03,.18, .03)) +
  scale_x_discrete(name = "", breaks=unique(melt_NKCAL$variable), labels=addline_format(c("MB Neuropil", "Kenyon Cells", "Antennal Lobes"))) +
  theme(text = element_text(color="black", size = generalsize),
        axis.title = element_text(color="black"), 
        axis.text.x = element_text(color="black", size = generalsize), 
        axis.text.y = element_text(color="black", size = generalsize, margin = margin(l = 10, r=5)),
        axis.ticks.margin=unit(0,'cm'),
        axis.ticks.length = unit (.2,"cm"),
        legend.title = element_blank(),
        legend.position = c(.78,.865),
        legend.margin = margin(2,0,2,2),
        legend.spacing.y = unit(0, "pt"),
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"),
        legend.text = element_text(size=legendsize),
        legend.key = element_rect(fill="white"),
        legend.key.size = unit(2,"line"),
        legend.key.width = unit(.5, "cm"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust=-.3, vjust=2.12, size = 18))
NKCAL_plot <- NKCAL_plot + guides(colour = guide_legend(override.aes = list(alpha = .7)))







#NK RATIO PLOT
NK <- reldata[,c("specimen_ID", "code","nk_relWB")]

#assign order levels
NK$code <- factor(NK$code, levels=c("NewEmerge", "LabReared", "Nesting"))

#Boxplot
par(mar=c(0,6,4,.5))
NK_plot <- ggplot(NK, aes(x=code, y=nk_relWB, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black") +
  scale_fill_manual(values = c("white", "gray70", "gray38"), labels=c("Newly-emerged", "Lab-reared", "Nesting")) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=1, seed=333), size=3.1, stroke=.7) +
  geom_point(aes(color=factor(code)), shape=16, position=position_jitterdodge(jitter.width=1, seed=333), alpha=.7, size=3) +
  scale_color_manual(values=c("NewEmerge"="mediumblue", "LabReared"="violetred4","Nesting"="darkorange"), labels=c("Newly-emerged", "Lab-reared", "Nesting")) +
  
  #Significance annotations from post-hoc results
  annotate(geom="text", x=1, y=(max(NK[NK$code=="NewEmerge", 3]) *.75)+.07, label="a", color="black", size=lettersize) + #NE
  annotate(geom="text", x=2, y=max(NK[NK$code=="LabReared", 3]) +.1, label="a", color="black", size=lettersize) + #LR
  annotate(geom="text", x=3, y=(max(NK[NK$code=="Nesting", 3])*.87) +.11, label="b", color="black", size=lettersize) + #REP
  ggtitle("(b)") +
  scale_y_continuous(name = "Ratio of Neuropil to Kenyon Cell Volume", limits = c(2.0,4.5), breaks=seq(2.0,4.5, 0.5)) +
  scale_x_discrete(name = "", labels = c("NE\n","LR","NS")) +
  theme(text = element_text(color="black", size = generalsize),
        axis.title = element_text(color="black"), 
        axis.text.x = element_text(color="black", size = generalsize),
        axis.text.y = element_text(color="black", size = generalsize, margin = margin(l = 10, r=5)),
        axis.ticks.margin=unit(0,'cm'),
        axis.ticks.length = unit (.2,"cm"),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",colour = "black"),
        plot.title = element_text(hjust=-.7, vjust=2.12, size = 18))


multipanel_exp1largestructures <- plot_grid(NKCAL_plot, NULL, NK_plot, rel_widths = c(.65,.03,.35), nrow = 1) #null adds more space between

jpeg("NmelExp1_bigstructures.jpeg", height=6, width = 8, units = "in", res = 500)

multipanel_exp1largestructures

dev.off()








##SPECIFICALLY MB NEUROPIL
#Melt Data into Appropriate Format
MB_neuropil <- reldata[,c("specimen_ID", "code","lip_relWB", "col_relWB", "calyces_relWB", "mblobes_relWB")]

#melt data
melt_MB_neuropil <- melt(MB_neuropil)

#assign order levels
melt_MB_neuropil$code <- factor(melt_MB_neuropil$code, levels=c("NewEmerge", "LabReared", "Nesting"))





##plot
par(mar=c(0,2,2,.5)) #(bottom, left, top, right)
MBNeuropil_plot <- ggplot(melt_MB_neuropil, aes(x=variable, y=value, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black") +
  scale_fill_manual(values = c("white", "gray70", "gray38"), labels=c("Newly-emerged", "Lab-reared", "Nesting")) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=.2, seed=12531), size=3.1, stroke=.7) +
  geom_point(aes(color = factor(code)), pch=16, position=position_jitterdodge(jitter.width=.2, seed=12531), alpha=.7, size=3) +
  scale_color_manual(values=c("NewEmerge"="mediumblue", "LabReared"="violetred4","Nesting"="darkorange"), labels=c("Newly-emerged", "Lab-reared", "Nesting")) +
  
  #Significance annotations from post-hoc results
  #LIPS
  annotate(geom="text", x=1-0.25, y=max(MB_neuropil[MB_neuropil$code=="NewEmerge", 3]) +.004, label="a", color="black", size = lettersize) + #NE
  annotate(geom="text", x=1, y=max(MB_neuropil[MB_neuropil$code=="LabReared", 3]) +.004, label="a", color="black", size = lettersize) + #LR
  annotate(geom="text", x=1+0.25, y=max(MB_neuropil[MB_neuropil$code=="Nesting", 3]) + .004, label="b", color="black", size =lettersize) + #REP
  
  #COLLARS
  annotate(geom="text", x=2-0.25, y=max(MB_neuropil[MB_neuropil$code=="NewEmerge", 4]) +.004, label="a", color="black", size = lettersize) + #NE
  annotate(geom="text", x=2, y=max(MB_neuropil[MB_neuropil$code=="LabReared", 4]) +.004, label="a", color="black", size = lettersize) + #LR
  annotate(geom="text", x=2+0.25, y=max(MB_neuropil[MB_neuropil$code=="Nesting", 4]) + .004, label="b", color="black", size = lettersize) + #REP
  
  #CALYCES
  annotate(geom="text", x=3-0.25, y=max(MB_neuropil[MB_neuropil$code=="NewEmerge", 5]) +.004, label="a", color="black", size = lettersize) + #NE
  annotate(geom="text", x=3, y=max(MB_neuropil[MB_neuropil$code=="LabReared", 5]) +.004, label="a", color="black",size = lettersize) + #LR
  annotate(geom="text", x=3+0.25, y=max(MB_neuropil[MB_neuropil$code=="Nesting", 5]) + .004, label="b", color="black", size = lettersize) + #
  
  #MBLOBE
  annotate(geom="text", x=4-0.25, y=max(MB_neuropil[MB_neuropil$code=="NewEmerge", 6]) +.004, label="a", color="black", size = lettersize) + #NE
  annotate(geom="text", x=4, y=max(MB_neuropil[MB_neuropil$code=="LabReared", 6]) +.004, label="a", color="black", size = lettersize) + #LR
  annotate(geom="text", x=4+0.25, y=max(MB_neuropil[MB_neuropil$code=="Nesting", 6]) + .004, label="b", color="black", size = lettersize) + #REP
  
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.02,.12), breaks=seq(0.02,.12, .02)) +
  scale_x_discrete(name = "", labels = c("Lips", "Collars", "Calyces", "MB Lobes")) +
  theme(text = element_text(color="black", size = generalsize),
        axis.title = element_text(color="black"), 
        axis.text.x = element_text(color="black", size = generalsize),
        axis.text.y = element_text(color="black", size = generalsize, margin = margin(l = 10, r=5)),
        axis.ticks.margin=unit(0,'cm'),
        axis.ticks.length = unit (.2,"cm"),
        legend.title = element_blank(),
        legend.position = c(.865,.875),
        legend.margin = margin(2,2,2,2),
        legend.spacing.y = unit(0, "pt"),
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size=legendsize),
        legend.key.size = unit(2,"line"),
        legend.key.width = unit(.5, "cm"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid",colour = "black"),
        plot.title = element_text(hjust=0))

MBNeuropil_plot <- MBNeuropil_plot + guides(colour = guide_legend(override.aes = list(alpha = .7)))

jpeg("NmelExp1_mbneuropil.jpeg", height=5.5, width = 7, units = "in", res = 500)

MBNeuropil_plot

dev.off()



