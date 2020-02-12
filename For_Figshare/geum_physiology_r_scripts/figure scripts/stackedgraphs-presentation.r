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
df<-df[,-5]
str(df)
df[14,3]<-1-(df[14,2]+df[14,4])
df[15,3]<-1-(df[15,2]+df[15,4])

#View(df)

df1 <- melt(df, id.var="Trait")
df1
df1<- as.data.frame(df1)
df1$variable <- factor(df1$variable, levels = c("Residual","Population","Region"))
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
														"Chlorophyll Content", "dC13 isotope"))
##set colors  #Prairie   #MB Alvar  #GL ALvar
col.esa <- c("#18563E", "#82BE42", "#FFC423")
##blue used: #005979 ##
##probs not useful for region/pop/residual, find others to complement##

##contrasting golds: 
gd <- c("#fff7bc", "#fec44f", "#d95f0e")

##ADD LABELS YET##
stk <- ggplot(df1, aes(x= Trait, y = value, fill = variable)) +
	geom_bar(stat= 'identity') + theme_zpub() + coord_flip() +
	scale_color_brewer("Legend", type = "seq", palette = "Greens", aesthetics = "fill")+
	#scale_fill_manual("Legend",values =col.esa)+
	#axes flipped, remember in axis labels#
	ylab("Proportion of Variance") + xlab("Trait")
stk
?scale_color_brewer
ggsave("propvarbarpres.tiff")
##Complementary colors additional to brewer scheme:##
## #e7298a, #66a61e #e6ab02 ##
##RGB: #e72... 231 41 138 ##
##RGB: #66a... 102 166 30 ##
##RGB: #e6a... 230 171 02 ##
