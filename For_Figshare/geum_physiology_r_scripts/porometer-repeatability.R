##porometer repeatability##
rp <- read.csv("porometer-standardization.csv")
names(rp)[1] <- "Region"
str(rp)
##view distribution##
hist(rp$mmol.s2)
##or density curve?##
library(ggplot2)
p<- ggplot()+geom_density(data=rp, aes(x =rp$mmol.s2))
p
library(lme4);library(lmerTest)
rtest <- lmer(rp$mmol.s2~rp$Sample.ID+(1|rp$temp)+(1|rp$humidity))
anova(rtest)
hist(residuals(rtest))
library(rptR)
rpttest<-rptGaussian(formula = mmol.s2~Sample.ID+(1|temp)+(1|humidity), 
							grname = c("temp","humidity","Fixed", "Residual"),data = rp,
							nboot=0,ratio = T, adjusted = F)
rpttest

rtest2<-lm(mmol.s2~Sample.ID, data =rp)
hist(residuals(rtest2))
anova(rtest2)
summary(rtest2)
