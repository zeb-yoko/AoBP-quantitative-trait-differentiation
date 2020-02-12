#PCA & RDA for Yoko et al.- Scale of trait variation

setwd('/Volumes/LaCie/Homecopy/Documents/Manuscripts/Yoko-ScaleofTraitVariation/AOB_FirstSubmission/AOB_Revisions/Data-RScripts')

data<-read.csv('climate_trait_Pop_averages.csv',h=T)

library(ggplot2)
library(MASS)
library(ellipse)
library(FactoMineR)
library(grid)
library(gridExtra)

head(data)

#Data for PCA analysis
climate_region<-data[,c(1:2,19:44)]
head(climate_region)

row.has.na <- apply(climate_region, 1, function(x){any(is.na(x))}) #which rows have NA values
sum(row.has.na) #1 row (population) has NA values
levels(climate_region$Region)[levels(climate_region$Region)=='Prairie']<-'Prairie'
levels(climate_region$Region)[levels(climate_region$Region)=='GL_Alvar']<-'GLA'
levels(climate_region$Region)[levels(climate_region$Region)=='MB_Alvar']<-'MBA'

a2<-climate_region[!row.has.na,] #remove rows that have NA
#Verify columns labeled correctly
Region<-a2[,2]
names(a2)[5]<-'Elevation' #rename Elvation.m to Elevation
Climate<-a2[,3:28]
climate_PCA<-prcomp(Climate,center=TRUE,scale.=TRUE)
print(climate_PCA)
plot(climate_PCA)
summary(climate_PCA)
biplot(climate_PCA)

#Visualize PCA
library(ggbiplot)
##basic PCA visualization
##set colors	#Prairie  #MB Alvar# #GL ALvar##
col.esa <- c("#18563E", "#82BE42", "#FFC423")
#GL ALvar  #MB Alvar  #Prairie##
col.reversed <- c("#FFC423", "#82BE42", "#18563E")

P1<-ggbiplot(climate_PCA,obs.scale=1,var.scale=1,group=Region,ellipse=TRUE,loadings.label.colour='black')+
  theme_bw()+theme(panel.border=element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  geom_point(aes(colour=Region),size=4,pch=16)+
  scale_color_manual(values = c("#FFC423", "#82BE42", "#18563E"))+
  geom_vline(xintercept=0,lty=2)+ geom_hline(yintercept=0.0,lty=2)+
  theme(legend.title=element_blank(),legend.background=element_rect(),legend.key=element_blank(),panel.background=element_rect(colour='white'),panel.grid.minor=element_blank())+
  theme(legend.text=element_text(size=15))+
  theme(axis.title.x=element_text(size=15))+
  theme(axis.title.y=element_text(size=15))+
  theme(legend.direction='horizontal',legend.position='top')+
  annotate(geom='text',x=4.5,y=4.5,label='A',color='black',size=10)
P1

#RDA analysis
data<-read.csv('climate_trait_Pop_averages.csv',h=T)

library(vegan)
library(permute)
library(lattice)
library(ggplot2)
library(grid)

library(MASS)
library(ellipse)
library(FactoMineR)

head(data)
data<-data[-c(3),] #remove AB-RL

traits<-data[,3:17] #trait variables estimated within the common garden - using population averages
climate<-data[,19:44] #climate associated with population provenance
climate.z<-scale(climate) #scaled climate data
traits.z<-scale(traits) #scaled trait data
Region<-data[,2]
env.ra<-data.frame(climate.z,Region) #climate data scaled with Region variable
col.reversed <- c("#FFC423", "#82BE42", "#18563E")

trait.rda<- rda(traits~.,env.ra,scale=T,na=na.omit, subset=complete.cases(traits)) #Populations with missing data removed
summary(trait.rda)

#Calculate R-squared and the adjusted R-squared
R2<-RsquareAdj(trait.rda)$r.squared
R2adj<-RsquareAdj(trait.rda)$adj.r

#Make a triplot with scaling 2
plot(trait.rda,xlab='RDA1 (43.15%)',ylab='RDA2 (24.46%)') #rough plot

P2<-plot(trait.rda,xlab='RDA1 (43.15%)',ylab='RDA2 (24.46%)',display=c('cn','lc','sp'),type='n',xlim=c(-1.5,1.5),ylim=c(-1.5,1.5))
sites.sc<-scores(trait.rda,choices=1:2,scaling=2,display="lc")
points(sites.sc,pch=21,col='black',bg='black')
va.sc<-scores(trait.rda,choices=1:2,scaling=2,display='sp')
rownames(va.sc)<-c('dC13','SLA','LDMC','CC','Cond','GCL_B','GCL_T','SD-B','SD-T','SAI-B','SAI-T','Sinus Depth','MVL','MLP','MLL')
text(va.sc,row.names(va.sc),cex=0.8)
env.sc<-scores(trait.rda,choices=1:2,scaling=2,display='bp')
arrows(0,0,env.sc[1:16,1],env.sc[1:16,2],lty=1,lwd=1.5,length=0.1,col='lightgrey')
env.names<-c('Latitude','Longitude','Elevation','MAT','MWMT','MCMT','TD','MAP','MSP','AHM','SHM','DD0','DD5','DD_18','DD18','NFFD')
text(env.sc,row.names(env.sc),cex=0.6,font=2,pos=2,col='lightgrey')
Region.names<-c('Great Lake Alvar','MB Alvar','Prairie')
Region.sc<-scores(trait.rda,choices=1:2,scaling=2,display='cn')
points(Region.sc,pch=23,col='black',bg=col.reversed,cex=1.75)
text(Region.sc[c(1,2,3),],Region.names[c(1,2,3)],cex=0.8,font=1,pos=4,col='red')
text(x=2.5,y=1.3,'B',cex=2)





