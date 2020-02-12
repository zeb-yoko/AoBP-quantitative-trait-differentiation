##Anova figures for proportion of variance##
##excluding conductance##
#trt <- read.csv("Propvarwbinaries.csv")
trt<- read.csv("Proportions of variancewanova.csv")
trt <- droplevels(trt[-which(trt$Trait =="Stomatal Conductance"),])
trt$Trait_class <-gsub("physiological", "Resource Allocation",trt$Trait_class)
trt$Trait_class <-gsub("morphological", "Morphological",trt$Trait_class)
trt$Trait_class <-gsub("stomatal", "Stomatal Arrangement",trt$Trait_class)
View(trt)

library(ggplot2)
regclass <- ggplot(trt, aes(x=Trait_class, y =Region, fill = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, color ='white') +
	xlab("Trait Class") + ylab("Proportion of variance by Region")+
	ylim(c(0, 0.3))+
	annotate('text', x=1, y=.25,label='b', col="black",size=3.5)+
	annotate('text', x=2, y=.25,label='a', col="black",size=3.5)+
	annotate('text', x=3, y=.25,label='a', col="black",size=3.5)+
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	theme(legend.title = element_blank())+theme(legend.position = "none")+	
	scale_fill_manual("Legend",values =c("gray80", "black", "gray43"))
regclass
ggsave("regiontraitclassboxplot-grscl.png")

popclass <- ggplot(trt, aes(x=Trait_class, y =Population, fill = Trait_class)) +
	geom_boxplot(outlier.color = "gray") +
	stat_summary(fun.y = 'mean', geom = 'point', shape = 43, size = 4, color = 'white') +
	xlab("Trait Class") + ylab("Proportion of variance by Population")+
	ylim(c(0, 0.3))+ 
	theme_zpub()+theme(axis.text.x = element_blank(), axis.ticks.x=element_blank())+
	theme(legend.title = element_blank())+theme(legend.position = "none")+
	scale_fill_manual("Legend",values =c("gray80", "black", "gray43"))
popclass
ggsave("populationtraitclassboxplot-grscl.png")

library(cowplot)
library(ggpubr)
plots <- ggarrange(regclass, popclass, labels = c("A", "B"), label.x = .89, label.y = 0.99, common.legend = T, legend = "top")
plots
ggsave("Figure_4.jpg", dpi =600)
?ggarrange
