#######Stacked Bar plots of Proportion of Variance#######
##Zebadiah.yoko@gmail.com##
##load libraries##
rm(list = ls())
##^^IF YOU RUN THIS YOU HAVE TO RERUN THEME FUNCTION##
library(tidyverse);library(reshape2)
##working directory##
setwd("C:/Users/zippy/OneDrive/NDSU/physiology-git")
##load data##
df <- read.csv("Proportions of variance.csv")
df<-df[,-1]
df<-df[,-5]
str(df)
df[14,3]<-1-(df[14,2]+df[14,4])
df[15,3]<-1-(df[15,2]+df[15,4])

View(df)

df1 <- melt(df, id.var="Trait")
df1
df1<- as.data.frame(df1)
df1$variable <- factor(df1$variable, levels = c("Residual", "Population", "Region"))
df1$Trait <- factor(df1$Trait, levels = c("Lobed minileaflets","Presence of mini leaflets",
														"Sinus Depth", "Specific Leaf Area", 
														"Stomatal Conductance", "Midvein Length", 
														"Stomatal Size (abaxial)", "SAI (adaxial)", 
														"SAI (abaxial)", "Stomatal Size (adaxial)",
														"Stomatal Density (abaxial)",
														"Stomatal Density (adaxial)", "Leaf Dry Matter Content",
														"Chlorophyll Content", "dC13 isotope"))
##set colors  #Prairie   #MB Alvar  #GL ALvar
col.esa <- c("#18563E", "#82BE42", "#FFC423")
##blue used: #005979 ##
##probs not useful for region/pop/residual, find others to complement##

##ADD LABELS YET##
stk <- ggplot(df1, aes(x= Trait, y = value, fill = variable)) +
	geom_bar(stat= 'identity') + theme_zpub() + coord_flip() +
	#scale_color_brewer("Legend", type = "qual", palette = 3, aesthetics = "fill")+
	scale_fill_manual("Legend",values =c("gray69", "gray43", "black"))+
	#axes flipped, remember in axis labels#
	ylab("Proportion of Variance") + xlab("Trait")
stk
ggsave("propvarbarpub.tiff")



