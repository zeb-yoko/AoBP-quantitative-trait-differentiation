####Boxplots associated with phase one of hypotheses testing for physiology paper####
###Zebadiah.Yoko@gmail.com###
#load necessary libraries @ Startup

##Working Directory:##
setwd("C:/Users/zippy/OneDrive/NDSU/physiology-git")
library(tidyverse)
##load data##
dato <- read.csv("merged_2018-physio-fitness_final.csv")
##Region color scheme (for ggplot)##
##set colors	#Prairie   #GL ALvar  #MB Alvar##
col.esa <- c("#18563E", "#82BE42", "#FFC423")
dato$Region<-factor(dato$Region, levels = c("Prairie", "MB_Alvar", "GL_Alvar"))
cfr <- ggplot(dato, aes(x=Region, y =Chlorophyll.Content, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Chlorophyll Content")+
	annotate('text', x=1, y=625,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=625,label='AB', col="#005979",size=7)+
	annotate('text', x=3, y=625,label='B', col="#005979",size=7)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
cfr
ggsave("chlorophyllxregion.tiff")
##########################
########################
##next variable##
#Specific Leaf Area##
##Remove rows that do not have sla samples##
slagg <- ggplot(dato, aes(x=Region, y =SLA, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Specific Leaf Area")+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
slagg
ggsave("SLAxregion.tiff")
#######################
##Next Variable--collected w/SLA##
ldmcgg <- ggplot(dato, aes(x=Region, y =Leaf.Dry.Matter.Content, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	annotate('text', x=1, y=.625,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=.625,label='AB', col="#005979",size=7)+
	annotate('text', x=3, y=.625,label='B', col="#005979",size=7)+
	xlab("Source Region") + ylab("Leaf Dry Matter Content")+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
ldmcgg
ggsave("drymatterxRegion.tiff")

########################
##next variable##
#Carbon isotope##
##Remove rows that do not have sla samples##
dc13 <- ggplot(dato, aes(x=Region, y =dC13, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	annotate('text', x=1, y=-27,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=-27,label='AB', col="#005979",size=7)+
	annotate('text', x=3, y=-27,label='B', col="#005979",size=7)+
	xlab("Source Region") + ylab("Carbon isotop discrimination")+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
dc13
ggsave("isotopexregion.tiff")

#############################
##next variable##
##Conductance##
Congg <- ggplot(dato, aes(x=Region, y =Conductance, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Conductance")+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
Congg
ggsave("ConductancexRegion.tiff")

###############################
##next variable##
########################
##graph Stomata Ratio##
ratio.s <- ggplot(dato, aes(x=Region, y =Ratio, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Stomata Ratio")+
#	annotate('text', x=1, y=2.5,label='A', col="#005979",size=7)+
#	annotate('text', x=2, y=2.5,label='B', col="#005979",size=7)+
#	annotate('text', x=3, y=2.5,label='A', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
ratio.s
ggsave("FullStomataRatioxRegion.tiff")	

#################################
##graph Stomata size (top)##
tops <- ggplot(dato, aes(x=Region, y =Av_GCLengthT, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Average Size (top)")+
	annotate('text', x=1, y=.035,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=.035,label='B', col="#005979",size=7)+
	annotate('text', x=3, y=.035,label='B', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
tops

ggsave("TopStomatasizexRegion.tiff")	

#################################
##graph Stomata size (bottom)##
bottoms <- ggplot(dato, aes(x=Region, y =Av_GCLengthB, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Average Size (bottom)")+
	annotate('text', x=1, y=.035,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=.035,label='B', col="#005979",size=7)+
	annotate('text', x=3, y=.035,label='C', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
bottoms

ggsave("BottomStomatasizexRegion.tiff")	

################################
##graph Stomata size (total)##
totals <- ggplot(dato, aes(x=Region, y =Av_GCLength, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Average Size (bottom)")+
	annotate('text', x=1, y=.035,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=.035,label='AB', col="#005979",size=7)+
	annotate('text', x=3, y=.035,label='B', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
totals

ggsave("totalStomatasizexRegion.tiff")	

#################################
##graph Midvein Length##
mvl <- ggplot(dato, aes(x=Region, y =Mid_Vein_Length, color = Region)) +
	geom_boxplot(outlier.color = "tan") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Average Size (top)")+
	annotate('text', x=1, y=11, label='A', col="#005979",size=7)+
	annotate('text', x=2, y=11,label='AB', col="#005979",size=7)+
	annotate('text', x=3, y=11,label='B', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
mvl
max(dato$Mid_Vein_Length, na.rm = T)
ggsave("Midveinxregion.tiff")	

