##Figures for N.melanderi Neuroplasticity; Social Experience 
##MA Hagadorn
##Last Modified Images: 12/15/2020
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
generalsize <- 12
legendsize <- 7
wordsize <- 8
lettersize <- 3
dotsize <- 1.8




#GENERATE FIGURE FOR NEUROPIL, KCS, AND ALS
par(mar=c(0,0,0,0)) #(bottom, left, top, right)
NKCAL_plot <- ggplot(melt_NKCAL, aes(x=variable, y=value, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black", lwd=0.3) +
  scale_fill_manual(values = c("white", "gray45"), labels=c("Solo", "Paired")) +
  geom_point(aes(color = factor(code)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=246), alpha=.75, size=dotsize) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=246), size=dotsize+.01, stroke=.3) +
  ggtitle("A") +
  scale_color_manual(values=c("solo"="forestgreen", "paired"="purple3"), labels=c("Solo", "Paired")) +
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.02,.18), breaks=seq(0.02,.18, .04)) +
  scale_x_discrete(name = "", labels = c("MB\nNeuropil", "Kenyon\nCells", "Antennal\nLobes")) +
  theme(text = element_text(color="black", size = generalsize, family="sans"),
        axis.title = element_text(color="black", size=9), 
        axis.text.x = element_text(color="black", size = wordsize, margin = margin(l=0, r=0, t=2, b=-10)), 
        axis.text.y = element_text(color="black", size = wordsize, margin = margin(l = 2, r=2)),
        axis.ticks.margin=unit(0,'cm'),
        axis.ticks.length = unit (.2,"cm"),
        axis.ticks = element_line(color="black", size = 0.3),
        legend.title = element_blank(),
        legend.position = c(.90,.925),
        legend.margin = margin(l=.6, r=.6, t=.5, b=.5),
        legend.spacing.y = unit(0, "pt"),
        legend.spacing.x = unit(0.3, "pt"),
        legend.background = element_rect(fill="white", size=0.25, linetype="solid", colour ="black"),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size=legendsize),
        legend.key.height = unit(17, "pt"),
        legend.key.width = unit(.4, "cm"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.3, linetype = "solid", colour = "black"),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
        plot.title = element_text(size = generalsize, margin = margin(t = -9, r = 0, b = 0, l = 0), face="bold", vjust = -3.3, hjust = .01))

NKCAL_plot <- NKCAL_plot + guides(colour = guide_legend(override.aes = list(alpha = .75)))








#NK RATIO PLOT
NK <- reldata[,c("sample", "code","nk_relWB")]

#assign order levels
NK$code <- factor(NK$code, levels=c("solo", "paired"))


#Boxplot
par(mar=c(0,0,0,0)) #(bottom, left, top, right)
NK_plot <- ggplot(NK, aes(x=code, y=nk_relWB, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black", lwd=.3) +
  scale_fill_manual(values = c("white", "gray45"), labels=c("Solo", "Paired")) +
  geom_point(aes(color = factor(code)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=246), alpha=.75, size=dotsize) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=246), size=dotsize+.01, stroke=.3) +
  ggtitle("B") +
  scale_color_manual(values=c("solo"="forestgreen", "paired"="purple3"), labels=c("Solo", "Paired")) +
  scale_y_continuous(name = "Ratio of Neuropil to Kenyon Cell Volume", limits = c(2.0, 3.5), breaks=seq(2.0, 3.5, 0.5)) +
  scale_x_discrete(name = "", labels = c("Solo\n", "Paired")) +
  theme(text = element_text(color="black", size = generalsize, family="sans"),
        axis.title = element_text(color="black", size=9), 
        axis.text.x = element_text(color="black", size = wordsize, margin = margin(l=0, r=0, t=2, b=-10)), 
        axis.text.y = element_text(color="black", size = wordsize, margin = margin(l = 2, r=2)),
        axis.ticks.length = unit (.2,"cm"),
        axis.ticks = element_line(color="black", size=.3),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.3, linetype = "solid",colour = "black"),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
        plot.title = element_text(size = generalsize, margin = margin(t = -9, r = 0, b = 0, l = 0), face="bold", vjust = -3.3, hjust = .03))

multipanel_exp2largestructures <- plot_grid(NKCAL_plot, NULL, NK_plot, rel_widths = c(.78,.05,.32), nrow = 1)


ggsave("Figure4.pdf",
       plot = last_plot(),
       device = "pdf",
       path = "D:/USUFiles_02072020/Documents/PHD/Manuscripts/Nomia_Age-Experience_20162018/Submissions/JournalofExperimentalBiology/Figures",
       scale = 1,
       width = 4.5,
       height = 3.5,
       units = "in",
       dpi = 300,
       limitsize = TRUE)








##SPECIFICALLY MB NEUROPIL
#Melt Data into Appropriate Format
MB_neuropil <- reldata[,c("sample", "code","lip_relWB", "col_relWB", "calyces_relWB", "mblobes_relWB")]

#melt data
melt_MB_neuropil <- melt(MB_neuropil)

#assign order levels
melt_MB_neuropil$code <- factor(melt_MB_neuropil$code, levels=c("solo", "paired"))


##plot
par(mar=c(0,0,0,0)) #(bottom, left, top, right)
MBNeuropil_plot <- ggplot(melt_MB_neuropil, aes(x=variable, y=value, fill=factor(code))) +
  geom_boxplot(outlier.shape = NA, color="black", lwd=.3) +
  scale_fill_manual(values = c("white", "gray45"), labels=c("Solo", "Paired")) +
  geom_point(aes(color = factor(code)), pch=16, position=position_jitterdodge(jitter.width=.35, seed=246), alpha=.75, size=dotsize) +
  geom_point(aes(fill=factor(code)), color="black", shape=1, position=position_jitterdodge(jitter.width=.35, seed=246), size=dotsize+.01, stroke=.3) +
  scale_color_manual(values=c("solo"="forestgreen", "paired"="purple3"), labels=c("Solo", "Paired")) +
  
  #<0.001==***, <0.01==**, <0.05=*
  #LIPS
  annotate(geom="text", x=1, y=max(MB_neuropil[MB_neuropil$code=="paired", 3]) + .0035, label="**", color="black", size = lettersize*1.75) + #paired
  scale_y_continuous(name = "Volume Relative to Whole Brain", limits = c(0.02,.12), breaks=seq(0.02,.12, .02)) +
  scale_x_discrete(name = "", labels = c("Lips", "Collars", "Calyces", "MB Lobes")) +
  theme(text = element_text(color="black", size = generalsize),
        axis.title = element_text(color="black", size=9), 
        axis.text.x = element_text(color="black", size = wordsize, margin = margin(l=0, r=0, t=2, b=-10)), 
        axis.text.y = element_text(color="black", size = wordsize, margin = margin(l = 2, r=2)),
        axis.ticks.length = unit (.2,"cm"),
        axis.ticks = element_line(color="black", size=.3),
        legend.title = element_blank(),
        legend.position = c(.92,.92),
        legend.margin = margin(l=.6, r=.6, t=.5, b=.5),
        legend.spacing.y = unit(0, "pt"),
        legend.spacing.x = unit(0.3, "pt"),
        legend.background = element_rect(fill="white", size=0.25, linetype="solid", colour ="black"),
        legend.key = element_rect(fill="white"),
        legend.text = element_text(size=legendsize),
        legend.key.height = unit(17, "pt"),
        legend.key.width = unit(.4, "cm"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin= grid::unit(c(0, 0, 0, 0), "in"),
        axis.line = element_line(size = .3, linetype = "solid",colour = "black"))



ggsave("Figure5.pdf",
       plot = last_plot(),
       device = "pdf",
       path = "D:/USUFiles_02072020/Documents/PHD/Manuscripts/Nomia_Age-Experience_20162018/Submissions/JournalofExperimentalBiology/Figures",
       scale = 1,
       width = 3.5,
       height = 3.5,
       units = "in",
       dpi = 300,
       limitsize = TRUE)

