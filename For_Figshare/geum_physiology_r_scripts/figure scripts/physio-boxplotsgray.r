####Boxplots associated with phase one of hypotheses testing for physiology paper####
###Zebadiah.Yoko@gmail.com###
#load necessary libraries @ Startup

##Working Directory:##
setwd("C:/Users/zippy/OneDrive/NDSU/physiology-git")
library(tidyverse)
##load data##
dato <- read.csv("physio-2018.csv")
##Region color scheme (for ggplot)##
##set colors	#Prairie   #GL ALvar  #MB Alvar##
col.esa <- c("#18563E", "#82BE42", "#FFC423")

##fix import (if necessary)##
colnames(dato)[1] = "Region"

##Add in Habitat Column##
dato <- mutate(dato, Habitat = ifelse(Region == "Prairie", "Prairie", "Alvar"))

##Remove rows that do not have cfr samples##
chl.1 <- dato[-which(is.na(dato$Chlorophyll.Content)==T),]
nrow(chl.1)

##Chlorophyll Fluoresence##
chl.1$Region<-factor(chl.1$Region,	levels = c("Prairie","MB_Alvar","GL_Alvar"))
cfr <- ggplot(chl.1, aes(x=chl.1$Region, y =chl.1$Chlorophyll.Content, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Chlorophyll Content")+
	theme(axis.text.x = element_text(angle=90, vjust = .25))+
	annotate('text', x=1, y=625,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=625,label='A', col="#005979",size=7)+
	annotate('text', x=3, y=625,label='B', col="#005979",size=7)+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
cfr
ggsave("chlorophyllxregion.tiff")
##########################
########################
##next variable##
#Specific Leaf Area##
##Remove rows that do not have sla samples##
sla.1 <- dato
nrow(sla.1)
sla.1$Region<-factor(sla.1$Region,	levels = c("Prairie","MB_Alvar","GL_Alvar"))
slagg <- ggplot(sla.1, aes(x=sla.1$Region, y =sla.1$SLA, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Specific Leaf Area")+
	theme_bw()+theme(panel.grid.major = element_blank(),
						  panel.grid.minor = element_blank())+
	scale_color_grey()
slagg
ggsave("SLAxregiongray.tiff")
#######################
##Next Variable--collected w/SLA##
ldmcgg <- ggplot(sla.1, aes(x=sla.1$Region, y =sla.1$Leaf.Dry.Matter.Content, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	annotate('text', x=1, y=.625,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=.625,label='AB', col="#005979",size=7)+
	annotate('text', x=3, y=.625,label='B', col="#005979",size=7)+
	xlab("Source Region") + ylab("Leaf Dry Matter Content")+
	theme(axis.text.x = element_text(angle=90, vjust = .25))+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
ldmcgg
ggsave("drymatterxRegion.tiff")
#############################
##next variable##
##Conducgray69ce##
##check if NA's present##
nrow(dato[which(is.na(dato$Conducgray69ce)==T),])
##Remove rows that do not have conducgray69ce data##
cond <- dato[-which(is.na(dato$Conducgray69ce)==T),]
nrow(cond)
cond$Region<-factor(cond$Region,	levels = c("Prairie","MB_Alvar","GL_Alvar"))
Congg <- ggplot(cond, aes(x=cond$Region, y =cond$Conducgray69ce, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Conducgray69ce")+
	theme(axis.text.x = element_text(angle=90, vjust = .25))+
	scale_color_grey()
#(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
Congg
ggsave("Conducgray69cexRegion.tiff")

###############################
##next variable##
##Stomata##
##DIFFERENT DATASHEET##
sto <-read.csv("stomata.AVG.size.csv")
##Fix first column (if necessary)##
colnames(sto)[1] = "Date.Collected"
##Order Regions by longitude NOTE DIFFERENT LEVELS#####   v        v  
sto$Region<-factor(sto$Region,	levels = c("Prairie","M Alvar","GL Alvar"))

##check if NA's present##
nrow(sto[which(is.na(sto$Ratio)==T),])
##Remove rows IF NAs present & do not have ratio data##
sto.1<- sto[-which(is.na(sto$Ratio)==T),]
########################
##graph Stomata Ratio##
ratio.s <- ggplot(sto.1, aes(x=sto.1$Region, y =sto.1$Ratio, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Stomata Ratio")+
	annotate('text', x=1, y=2.5,label='A', col="#005979",size=7)+
	annotate('text', x=2, y=2.5,label='B', col="#005979",size=7)+
	annotate('text', x=3, y=2.5,label='A', col="#005979",size=7)+
	theme_bw()+theme(panel.grid.major = element_blank(),
						  panel.grid.minor = element_blank())+
	scale_color_grey()
ratio.s
ggsave("FullStomataRatioxRegion.tiff")	

#################################
##graph Stomata size (top)##
sto.T <- sto[-which(is.na(sto$Avg.T)==T),]
tops <- ggplot(sto.T, aes(x=sto.T$Region, y =sto.T$Avg.T, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Average Size (top)")+
	#annotate('text', x=1, y=2.5,label='A', col="#005979",size=7)+
	#annotate('text', x=2, y=2.5,label='B', col="#005979",size=7)+
	#annotate('text', x=3, y=2.5,label='A', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
tops

ggsave("TopStomatasizexRegion.tiff")	

##################################
##graph Stomata size (bottom)##
sto.B <- sto[-which(is.na(sto$Avg.B)==T),]
bottoms <- ggplot(sto.B, aes(x=sto.B$Region, y =sto.B$Avg.B, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Average Size (Bottom)")+
	#annotate('text', x=1, y=2.5,label='A', col="#005979",size=7)+
	#annotate('text', x=2, y=2.5,label='B', col="#005979",size=7)+
	#annotate('text', x=3, y=2.5,label='A', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
bottoms

ggsave("BottomStomatasizexRegion.tiff")	

################################
##graph Stomata size (total)##
stoSizeAVG <- sto[-which(is.na(sto$SizeAVG)==T),]
totals <- ggplot(stoSizeAVG, aes(x=stoSizeAVG$Region, y =stoSizeAVG$SizeAVG, color = Region)) +
	geom_boxplot(outlier.color = "gray69") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Source Region") + ylab("Average Size (total)")+
	#annotate('text', x=1, y=2.5,label='A', col="#005979",size=7)+
	#annotate('text', x=2, y=2.5,label='B', col="#005979",size=7)+
	#annotate('text', x=3, y=2.5,label='A', col="#005979",size=7)+
	#scale_x_discrete(labels = c("Prairie", "MB Alvar", "GL Alvar"))+
	scale_color_manual(values = col.esa, labels = c("Prairie", "MB Alvar", "GL Alvar"))
totals

ggsave("totalStomatasizexRegion.tiff")	
