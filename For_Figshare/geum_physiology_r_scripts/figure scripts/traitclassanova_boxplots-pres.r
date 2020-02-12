##Anova figures for proportion of variance##
#trt <- read.csv("Propvarwbinaries.csv")
trt<- read.csv("Proportions of variancewanova.csv")
trt <- droplevels(trt[-which(trt$Trait =="Stomatal Conductance"),])
trt$Trait_class <-gsub("physiological", "Resource Allocation",trt$Trait_class)
##blue used: #005979 ##
library(ggplot2)
regclass <- ggplot(trt, aes(x=Trait_class, y =Region, color = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Trait Class") + ylab("Variance explained by Region")+
	annotate('text', x=1, y=.25,label='B', col="#005979",size=7)+
	annotate('text', x=2, y=.25,label='A', col="#005979",size=7)+
	annotate('text', x=3, y=.25,label='A', col="#005979",size=7)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual("Legend",values =c("#e7298a", "#66a61e", "#e6ab02"))
regclass
ggsave("regiontraitclassboxplot-pres.png")

popclass <- ggplot(trt, aes(x=Trait_class, y =Population, color = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Trait Class") + ylab("Variance explained by Population")+
	#	annotate('text', x=1, y=625,label='A', col="#005979",size=7)+
	#	annotate('text', x=2, y=625,label='AB', col="#005979",size=7)+
	#	annotate('text', x=3, y=625,label='B', col="#005979",size=7)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual("Legend",values =c("#e7298a", "#66a61e", "#e6ab02"))
popclass
ggsave("populationtraitclassboxplot-pres.png")
