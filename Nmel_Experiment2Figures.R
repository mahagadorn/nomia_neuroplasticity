##Figures for N.melanderi Neuroplasticity; Social Experience 
##MA Hagadorn
##Last Modified Images: 9/20/2020
##Last Modified Text: 9/24/2020
##Data Used: Nmel_socialbrain2018_structurevolume_proportiondata.csv; generated as a csv output while running Nmel_Experiment2_DataAnalysis

#loadlibraries
library(ggplot2)
library(reshape2)
library(cowplot)



#loading in the proportion data (relative to whole brain)
##Using setwd() you need to designate your respective file path; MAH file path excluded for privacy
setwd() #designate file path
reldata <- read.table("Nmel_socialbrain2018_structurevolume_proportiondata.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
reldata <-reldata[,-c(1,6,8:14)]

#Assigning factors
reldata$tracer <- factor(reldata$tracer)

#Verify that factors are assigned appropriately
print(str(reldata))


#Melt Data into Appropriate Format
##Make subset for N, KC, AL
NKCAL <- reldata[,c("sample", "code","neuropil_relWB", "kcs_relWB", "als_relWB")]

#melt data
melt_NKCAL <- melt(NKCAL)

#assign order levels
melt_NKCAL$code <- factor(melt_NKCAL$code, levels=c("solo", "paired"))


#Assigning specific sizes, changes to figures will reflect changes made here. Keep consistent with Exp.1 figures
generalsize <- 18
legendsize <- 13
lettersize <- 5.2



#GENERATE FIGURE FOR NEUROPIL, KCS, AND ALS
par(mar=c(0,2,4,6)) #(bottom, left, top, right)
NKCAL_plot <- ggplot(melt_NKCAL, aes(x=variable, y=value, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black") +
  scale_fill_manual(values = c("white", "gray70"), labels=c("Solo", "Paired")) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=246), size=3.1, stroke=.7) +
  geom_point(aes(color = factor(code)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=246), alpha=.7, size=3) +
  ggtitle("(a)") +
  scale_color_manual(values=c("solo"="forestgreen", "paired"="purple3"), labels=c("Solo", "Paired")) +
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.02,.18), breaks=seq(0.02,.18, .04)) +
  scale_x_discrete(name = "", labels = c("MB\nNeuropil", "Kenyon\nCells", "Antennal\nLobes")) +
  theme(text = element_text(color="black", size = generalsize),
        axis.title = element_text(color="black"), 
        axis.text.x = element_text(color="black", size = generalsize),
        axis.text.y = element_text(color="black", size = generalsize, margin = margin(l = 10, r=5)),
        axis.ticks.margin=unit(0,'cm'),
        axis.ticks.length = unit (.2,"cm"),
        legend.title = element_blank(),
        legend.position = c(.88,.94),
        legend.margin = margin(2,2,2,2),
        legend.spacing.y = unit(0, "pt"),
        legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"),
        legend.text = element_text(size=legendsize),
        panel.background = element_rect(fill = "white"),
        legend.key = element_rect(fill="white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 1, linetype = "solid", colour = "black"),
        plot.title = element_text(hjust=-.32, vjust=2.12, size = generalsize))

NKCAL_plot <- NKCAL_plot + guides(colour = guide_legend(override.aes = list(alpha = .7)))








#NK RATIO PLOT
NK <- reldata[,c("sample", "code","nk_relWB")]

#assign order levels
NK$code <- factor(NK$code, levels=c("solo", "paired"))


#Boxplot
par(mar=c(0,6,4,.5)) #(bottom, left, top, right)
NK_plot <- ggplot(NK, aes(x=code, y=nk_relWB, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black") +
  scale_fill_manual(values = c("white", "gray70"), labels=c("Solo", "Paired")) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=1.1, seed=1), size=3.1, stroke=.7) +
  geom_point(aes(color = factor(code)), pch=16, position=position_jitterdodge(jitter.width=1.1, seed=1), alpha=.7, size=3) +
  ggtitle("(b)") +
  scale_color_manual(values=c("solo"="forestgreen", "paired"="purple3"), labels=c("Solo", "Paired")) +
  scale_y_continuous(name = "Ratio of Neuropil to Kenyon Cell Volume", limits = c(2.0, 3.5), breaks=seq(2.0, 3.5, 0.5)) +
  scale_x_discrete(name = "", labels = c("Solo\n", "Paired")) +
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
        plot.title = element_text(hjust=-.8, vjust=2.12, size = generalsize))

multipanel_exp2largestructures <- plot_grid(NKCAL_plot, NULL, NK_plot, rel_widths = c(.65,.03,.35), nrow = 1)


jpeg("NmelExp2_bigstructures.jpeg", height=6, width = 7.5, units = "in", res = 500)

multipanel_exp2largestructures

dev.off()












##SPECIFICALLY MB NEUROPIL
#Melt Data into Appropriate Format
MB_neuropil <- reldata[,c("sample", "code","lip_relWB", "col_relWB", "calyces_relWB", "mblobes_relWB")]

#melt data
melt_MB_neuropil <- melt(MB_neuropil)

#assign order levels
melt_MB_neuropil$code <- factor(melt_MB_neuropil$code, levels=c("solo", "paired"))






##plot
par(mar=c(0,2,2,.5)) #(bottom, left, top, right)
MBNeuropil_plot <- ggplot(melt_MB_neuropil, aes(x=variable, y=value, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black") +
  scale_fill_manual(values = c("white", "gray70"), labels=c("Solo", "Paired")) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=666), size=3.1, stroke=.7) +
  geom_point(aes(color = factor(code)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=666), alpha=.7, size=3) +
  scale_color_manual(values=c("solo"="forestgreen", "paired"="purple3"), labels=c("Solo", "Paired")) +
  
  #<0.001==***, <0.01==**, <0.05=*
  #LIPS
  annotate(geom="text", x=1, y=max(MB_neuropil[MB_neuropil$code=="paired", 3]) + .0035, label="**", color="black", size = lettersize*2) + #paired
  
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.02,.12), breaks=seq(0.02,.12, .02)) +
  scale_x_discrete(name = "", labels = c("Lips", "Collars", "Calyces", "MB Lobes")) +
  theme(text = element_text(color="black", size = generalsize),
        axis.title = element_text(color="black"), 
        axis.text.x = element_text(color="black", size = generalsize), 
        axis.text.y = element_text(color="black", size = generalsize, margin = margin(l = 10, r=5)),
        axis.ticks.margin=unit(0,'cm'),
        axis.ticks.length = unit (.2,"cm"),
        legend.title = element_blank(),
        legend.position = c(.91,.91),
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

MBNeuropil_plot <- MBNeuropil_plot + guides(colour = guide_legend(override.aes = list(alpha = 1)))



jpeg("NmelExp2_mbneuropil.jpeg", height=5.5, width = 6.5, units = "in", res = 500)

MBNeuropil_plot

dev.off()




