##Anova figures for proportion of variance##
#trt <- read.csv("Propvarwbinaries.csv")
trt<- read.csv("Proportions of variancewanova.csv")
trt <- droplevels(trt[-which(trt$Trait =="Stomatal Conductance"),])
trt$Trait_class <-gsub("physiological", "Resource Allocation",trt$Trait_class)

library(ggplot2)
regclass <- ggplot(trt, aes(x=Trait_class, y =Region, color = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Trait Class") + ylab("Variance explained by Region")+
	annotate('text', x=1, y=.25,label='B', col="black",size=7)+
	annotate('text', x=2, y=.25,label='A', col="black",size=7)+
	annotate('text', x=3, y=.25,label='A', col="black",size=7)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual("Legend",values =c("gray69", "gray43", "black"))
regclass
?ggsave
ggsave("regiontraitclassboxplot.png")
popclass <- ggplot(trt, aes(x=Trait_class, y =Population, color = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Trait Class") + ylab("Variance explained by Population")+
	#	annotate('text', x=1, y=625,label='A', col="#005979",size=7)+
	#	annotate('text', x=2, y=625,label='AB', col="#005979",size=7)+
	#	annotate('text', x=3, y=625,label='B', col="#005979",size=7)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual("Legend",values =c("gray69", "gray43", "black"))
popclass
ggsave("populationtraitclassboxplot.png")
##EXCLUDING SLA AND Conductance##
##removing outliers with no significant difference##
trt <- trt[-2,]
trt <- trt[-3,]
trt
regclass2 <- ggplot(trt, aes(x=Trait_class, y =Region, color = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Trait Class") + ylab("Proportion of Variance explained by Region effect")+
		annotate('text', x=1, y=.25,label='*', col="black",size=7)+
		annotate('text', x=2, y=.25,label='*', col="black",size=7)+
		annotate('text', x=3, y=.25,label='*', col="black",size=7)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual("Legend",values =c("gray69", "gray43", "black"))
regclass2

popclass2 <- ggplot(trt, aes(x=Trait_class, y =Population, color = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4) +
	xlab("Trait Class") + ylab("Proportion of Variance explained by Population effect")+
	#	annotate('text', x=1, y=625,label='A', col="#005979",size=7)+
	#	annotate('text', x=2, y=625,label='AB', col="#005979",size=7)+
	#	annotate('text', x=3, y=625,label='B', col="#005979",size=7)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	scale_color_manual("Legend",values =c("gray69", "gray43", "black"))
popclass2
