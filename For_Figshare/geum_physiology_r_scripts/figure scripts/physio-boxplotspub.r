####Boxplots associated with phase one of hypotheses testing for physiology paper####
###Zebadiah.Yoko@gmail.com###
#load necessary libraries @ Startup

##Working Directory:##
setwd("C:/Users/zippy/OneDrive/NDSU/physiology-git")
library(tidyverse); library(cowplot)
##load data##
dato <- read.csv("merged_2018-physio-fitness_final.csv")
##Region color scheme (for ggplot)##
##set colors	#Prairie   #GL ALvar  #MB Alvar##
col.esa <- c("#18563E", "#82BE42", "#FFC423")
col.bw <- c("gray7", "gray49", "gray64")
##I want to compress into panels by type: physio, morpho, stomata (ab) and stomata (ad)
dato$Region<-factor(dato$Region, levels = c("Prairie", "MB_Alvar", "GL_Alvar"))
cfr <- ggplot(dato, aes(x=Region, y =Chlorophyll.Content, color = Region)) +
	theme_zpub()+
	geom_boxplot(color =col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Chlorophyll Content")+
	annotate('text', x=1, y=625,label='A', size=7)+
	annotate('text', x=2, y=625,label='AB', size=7)+
	annotate('text', x=3, y=625,label='B', size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))
cfr
class(cfr)
##########################
########################
##next variable##
#Specific Leaf Area##
##Remove rows that do not have sla samples##
slagg <- ggplot(dato, aes(x=Region, y =SLA, color = Region)) +
	geom_boxplot(color= col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Specific Leaf Area")+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
slagg
class(slagg)
#######################
##Next Variable--collected w/SLA##
ldmcgg <- ggplot(dato, aes(x=Region, y =Leaf.Dry.Matter.Content, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	annotate('text', x=1, y=.625,label='A', col="black",size=7)+
	annotate('text', x=2, y=.625,label='AB', col="black",size=7)+
	annotate('text', x=3, y=.625,label='B', col="black",size=7)+
	xlab("Source Region") + ylab("Leaf Dry Matter Content")+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
ldmcgg
class(ldmcgg)
########################
##next variable##
#Carbon isotope##
##Remove rows that do not have sla samples##
dc13 <- ggplot(dato, aes(x=Region, y =dC13, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	annotate('text', x=1, y=-26.5,label='A', col="black",size=7)+
	annotate('text', x=2, y=-26.5,label='AB', col="black",size=7)+
	annotate('text', x=3, y=-26.5,label='B', col="black",size=7)+
	xlab("Source Region") + ylab("Carbon isotop discrimination")+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
dc13
class(dc13)
##Physiology panel##
library(ggpubr)
library(cowplot)
plot_grid(cfr, slagg, ldmcgg, dc13, labels=c("1", "2", "3", "4"))

#############################
##next variable##
##Conductance##
congg <- ggplot(dato, aes(x=Region, y =Conductance, color = Region)) +
	geom_boxplot(col =col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Conductance")+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
congg

###############################
##next variable##
########################
##graph Stomata Ratio##
##VOID##
ratio.s <- ggplot(dato, aes(x=Region, y =Ratio, color = Region)) +
	geom_boxplot(outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Stomata Ratio")+
#	annotate('text', x=1, y=2.5,label='A', col="black",size=7)+
#	annotate('text', x=2, y=2.5,label='B', col="black",size=7)+
#	annotate('text', x=3, y=2.5,label='A', col="black",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
ratio.s

#################################
##graph Stomata size (top)##
tops <- ggplot(dato, aes(x=Region, y =Av_GCLengthT, color = Region)) +
	geom_boxplot(col=col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Average Size (top)")+
	annotate('text', x=1, y=.035,label='A', col="black",size=7)+
	annotate('text', x=2, y=.035,label='B', col="black",size=7)+
	annotate('text', x=3, y=.035,label='B', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
tops


#################################
##graph Stomata size (bottom)##
bottoms <- ggplot(dato, aes(x=Region, y =Av_GCLengthB, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Average Size (bottom)")+
	annotate('text', x=1, y=.035,label='A', col="black",size=7)+
	annotate('text', x=2, y=.035,label='B', col="black",size=7)+
	annotate('text', x=3, y=.035,label='C', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
bottoms


################################
##graph Stomata size (total)##
totals <- ggplot(dato, aes(x=Region, y =Av_GCLength, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Average Size (bottom)")+
	annotate('text', x=1, y=.035,label='A', col="black",size=7)+
	annotate('text', x=2, y=.035,label='AB', col="black",size=7)+
	annotate('text', x=3, y=.035,label='B', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
totals

#################################
##graph Midvein Length##
mvl <- ggplot(dato, aes(x=Region, y =Mid_Vein_Length, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Mid Vein length")+
	annotate('text', x=1, y=11, label='A', col="black",size=7)+
	annotate('text', x=2, y=11,label='AB', col="black",size=7)+
	annotate('text', x=3, y=11,label='B', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
mvl
max(dato$Mid_Vein_Length, na.rm = T)

lsd <- ggplot(dato, aes(x=Region, y =Sinus_Depth, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Leaf Sinus Depth")+
	#annotate('text', x=1, y=3, label='A', col="black",size=7)+
	#annotate('text', x=2, y=3,label='AB', col="black",size=7)+
	#annotate('text', x=3, y=3,label='B', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
lsd

##Morphology##
plot_grid(mvl, lsd, labels=c("1", "2"))
####


################################
##graph Stomata area index (bottom)##
saib <- ggplot(dato, aes(x=Region, y =SAI_B, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("SAI (bottom)")+
	annotate('text', x=1, y=15,label='A', col="black",size=7)+
	annotate('text', x=2, y=15,label='AB', col="black",size=7)+
	annotate('text', x=3, y=15,label='B', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
saib

sait <- ggplot(dato, aes(x=Region, y =SAI_T)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black", na.rm = T) +
	xlab("Source Region") + ylab("SAI (top)")+
	annotate('text', x=1, y=10,label='A', col="black",size=7)+
	annotate('text', x=2, y=10,label='B', col="black",size=7)+
	annotate('text', x=3, y=10,label='C', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
sait
prs <- subset(dato, Region == "Prairie")
mbs <- subset(dato, Region == "MB_Alvar")
gls <- subset(dato, Region == "GL_Alvar")
mean(mbs$SAI_T, na.rm =T)
mean(gls$SAI_T, na.rm=T)
mean(prs$SAI_T, na.rm =T)
?mean
?stat_summary
densb <- ggplot(dato, aes(x=Region, y =Density_B, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Density (bottom)")+
	annotate('text', x=1, y=550,label='A', col="black",size=7)+
	annotate('text', x=2, y=550,label='AB', col="black",size=7)+
	annotate('text', x=3, y=550,label='B', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
densb

denst <- ggplot(dato, aes(x=Region, y =Density_T, color = Region)) +
	geom_boxplot(col = col.bw, outlier.color = "black") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, col = "black") +
	xlab("Source Region") + ylab("Density (top)")+
	annotate('text', x=1, y=400,label='A', col="black",size=7)+
	annotate('text', x=2, y=400,label='B', col="black",size=7)+
	annotate('text', x=3, y=400,label='B', col="black",size=7)+
	scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()
denst


##Stomata(conductance, + top)##
plot_grid(congg, denst, tops, sait, labels=c("1", "2", "3", "4"))
####

##Stomata(conductance, + bottom)##
plot_grid(congg, densb, bottoms, saib, labels=c("1", "2", "3", "4"))
####


