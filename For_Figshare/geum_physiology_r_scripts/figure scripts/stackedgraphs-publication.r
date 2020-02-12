#######Stacked Bar plots of Proportion of Variance#######
##Zebadiah.yoko@gmail.com##
##load libraries##
rm(list = ls())
##^^IF YOU RUN THIS YOU HAVE TO RERUN THEME FUNCTION##
library(tidyverse);library(reshape2)
##working directory##
setwd("C:/Users/zippy/OneDrive/NDSU/physiology-git")
##load data##
df <- read.csv("Proportions of variancewanova.csv")
df$Trait <- gsub("dC13 isotope", "Carbon Isotope Discrimination", df$Trait) 
df<-df[,-5]
str(df)
df[14,3]<-1-(df[14,2]+df[14,4])
df[15,3]<-1-(df[15,2]+df[15,4])

##change layout of data table for visualization##
df1 <- melt(df, id.var="Trait")
df1<- as.data.frame(df1)
df1$variable <- factor(df1$variable, levels = c("Residual", "Population", "Region"))
df1$Trait <- factor(df1$Trait, levels = c("Lobed minileaflets",
														"Presence of mini leaflets",
														"Sinus Depth", "Midvein Length", 
														"Stomatal Conductance",
														"Stomatal Size (abaxial)", "SAI (adaxial)", 
														"Stomatal Size (adaxial)","SAI (abaxial)",
														"Stomatal Density (abaxial)",
														"Stomatal Density (adaxial)", 
														"Specific Leaf Area",
														"Leaf Dry Matter Content",
														"Chlorophyll Content", "Carbon Isotope Discrimination"))

##ADD LABELS YET##
stk <- ggplot(df1, aes(x= Trait, y = value, fill = variable)) +
	geom_bar(stat= 'identity') + theme_zpub() + coord_flip() +
	scale_fill_manual("Legend",values =c("gray80", "gray43", "black"))+
	#axes flipped, remember in axis labels#
	ylab("Proportion of Variance") + xlab("Trait")
stk
#ggsave("propvarbarpub.png", dpi = 300)
##AoBP guidelines:##
ggsave("figure_3.jpg", dpi = 600)

