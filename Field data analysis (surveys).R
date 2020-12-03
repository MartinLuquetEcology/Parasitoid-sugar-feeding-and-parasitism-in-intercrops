library(lme4)
library(car)
library(glmmTMB)
library(RVAideMemoire)
library(DHARMa)
library(ggplot2)
library(Rmisc)
library(dplyr)

# Set working directory here
#setwd("~/Boulot/Thèse 2016-2019/AOBox/R/Scripts/Terrain 2018")

####################IMPORTING DATA#######################

  # Field data importation
field_surv <- read.table(file="field_data_surveys.txt",h=T,dec=",")   

  # Calculating parasitism rate field per field and week per week
field_surv$pRate <- field_surv$Tot_mummies/(field_surv$Tot_aphid + field_surv$Tot_mummies)

##################ANALYSES - APHID DENSITY############################

    ### Some data visualization first

field_surv$Position <- as.factor(field_surv$Position)
field_surv$Position <- relevel(field_surv$Position ,'5m')
field_surv$Type <- as.factor(field_surv$Type)
levels(field_surv$Type) <- c('Intercrop','Single crop')

  ## Population dynamics

  # Plotting the raw data with smoothers
ggplot(field_surv,aes(x=Week,y=Tot_aphids,colour=Type,fill=Type))+
  facet_wrap(~Position)+
  geom_point(size=1.5)+
  geom_smooth(alpha=0.15)+  ## Fitting a loess smoother to visualize the general dynamics
  xlab("Week")+
  ylab("Aphid abundance")+
  theme(legend.title=element_text(face='bold'),legend.position="bottom")+
  labs(colour='Field type',fill='Field type')


  # Mean +/- SE per week
  # Getting mean, sd and se values for each field type, position and week
summAphids <- summarySE(field_surv, measurevar="Tot_aphids", groupvars=c("Type","Position","Week"),na.rm=T)

ggplot(summAphids,aes(y=Tot_aphids,x=Week,colour=Type))+
  facet_wrap(~Position)+
  geom_errorbar(aes(ymin=Tot_aphids-se, ymax=Tot_aphids+se), width=.1)+
  geom_line()+
  geom_point(size=1.5)+
  xlab("Week")+
  ylab("Aphid abundance")+
  theme(legend.title=element_text(face='bold'),legend.position="bottom")+
  labs(colour='Field type')

  # Boxplots
ggplot(field_surv,aes(y=Tot_aphids,fill=Type,x=Type))+
  facet_wrap(~Position)+
  geom_boxplot(alpha=0.6,outlier.shape = NA)+
  geom_point(aes(colour=Type),alpha=1,size=1.5,
             position = position_jitterdodge())


   ### Statistical analysis

  # Fitting the model

  # Testing a Poisson model first
modAphid1 <- glmer(Tot_aphids~Type*Position+(1|Week)+(1|Field),data=field_surv,family='poisson')

plotresid(modAphid1)
resAphid1 <- simulateResiduals(modAphid1)
plot(resAphid1,asFactor=T) # Significant deviation
testZeroInflation(resAphid1) # Zero-inflation
testDispersion(resAphid1)  # No overdispersion

  # There is clear zero-inflation that may be due to overdispersion -> let's try glmer.nb

  # Negative-binomial model
modAphid2 <- glmer.nb(Tot_aphids~Type*Position+(1|Week)+(1|Field),data=field_surv)

  # Model summary
summary(modAphid2)

  # Residual checks
plotresid(modAphid2) # Ok though some values are under-estimated
resAphid2 <- simulateResiduals(modAphid2)
plot(resAphid2,asFactor=T) # Second check: OK
testZeroInflation(resAphid2) # No zero-inflation
testDispersion(resAphid2)  # No overdispersion

  # Testing for effects
Anova(modAphid2)

##################ANALYSES - PARASITISM RATES############################

    ### Some data visualization first

  # Plotting the raw data with smoothers (quite messy)
ggplot(field_surv,aes(x=Week,y=pRate,colour=Type,fill=Type))+
  facet_wrap(~Position)+
  geom_point(size=1.5)+
  geom_smooth(alpha=0.15)+  ## Fitting a loess smoother to visualize the general dynamics
  xlab("Week")+
  ylab("Parasitism rate")+
  theme(legend.title=element_text(face='bold'),legend.position="bottom")+
  labs(colour='Field type',fill='Field type')

  # Getting mean, sd and se values for each field type, position and week
summPara <- summarySE(field_surv, measurevar="pRate", groupvars=c("Type","Position","Week"),na.rm=T)

  # Parasitism dynamics plot
ggplot(summPara,aes(y=pRate,x=Week,colour=Type))+
  facet_wrap(~Position)+
  geom_errorbar(aes(ymin=pRate-se, ymax=pRate+se), width=.1)+
  geom_line()+
  geom_point(size=1.5)+
  xlab("Week")+
  ylab("Parasitism rate")+
  theme(legend.title=element_text(face='bold'),legend.position="bottom")+
  labs(colour='Field type')

  # Boxplots
ggplot(field_surv,aes(y=pRate,fill=Type,x=Type))+
  facet_wrap(~Position)+
  geom_boxplot(alpha=0.6,outlier.shape = NA)+
  geom_point(aes(colour=Type),alpha=1,size=1.5,
             position = position_jitterdodge())

  # Let's also look at the parasisitm rate distribution for each field position and type
  # And look at correspondence for each field and week
  # There does not seem to be a particular trend
ggplot(field_surv,aes(Position,pRate,colour=Type,group=interaction(Field,Week)))+
  geom_point(aes(shape=Type))+
  scale_colour_brewer(palette="Dark2")+
  geom_line(aes(group=interaction(Field,Week)),color='grey')


    ### Statistical analysis

  # Getting the 2-column matrix with number of living aphids and mummies
prMatrix <- as.matrix(field_surv[,c(12,5)])

  # Fitting the model
modPara <- glmer(prMatrix~Type*Position+(1|Week)+(1|Field),data=field_surv,family="binomial")

  # Model summary
summary(modPara)

  # Residual checks
plotresid(modPara) # Ok though residuals are not perfectly homogeneous

  # Adding type, position and field-week pairs
  # OK: there does not seem to be a particular trend
field_surv$fitted <- fitted(modPara)
field_surv$resid <- resid(modPara,type="pearson")
ggplot(field_surv,aes(fitted,resid))+
  geom_line(aes(group=interaction(Field,Week)),colour="gray")+
  geom_point(aes(colour=Type,shape=Position))+
  geom_smooth()

  # Additional checks using simulated residuals (DHARMa)
resPara <- simulateResiduals(modPara)
plot(resPara,asFactor=T) # Second check: OK
testZeroInflation(resPara) # No sign of zero-inflation
testDispersion(resPara)  # No sign of overdispersion

  # Testing for effects
Anova(modPara)

##################CREATING FIGURE 5############################

  ## Creating a single object

summAphids$var <- "Aphid abundance"
summPara$var <- "Parasitism rate"

names(summAphids)[5] <- 'Variable'
names(summPara)[5] <- 'Variable'

summVars <- rbind(summAphids,summPara)

  ## Plot

Fig5 <- ggplot(summVars,aes(y=Variable,x=Week))+
  facet_grid(var~Position,scales='free',switch='y')+
  geom_errorbar(aes(ymin=Variable-se, ymax=Variable+se,colour=Type), width=.1)+
  geom_line(aes(colour=Type))+
  geom_point(size=1.5,aes(colour=Type))+
  xlab("Week")+
  ylab(NULL)+
  theme(legend.title=element_text(face='bold'),legend.position="bottom")+
  labs(colour='Field type')+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white'),
        strip.text.x = element_text(face="bold"),
        strip.placement = "outside",
        strip.background.y = element_blank(),
        panel.spacing.y = unit(1.5, "lines"))+
  geom_hline(yintercept=0,lwd=1)+
  scale_y_continuous(limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.02)))+
  scale_x_continuous(limits=c(16.8,24.3),expand=expansion(mult=c(0,0)))+
  geom_vline(data=filter(summVars, Position=="5m"), aes(xintercept = 16.8),lwd=1)

#ggsave(Fig5,filename='Figure_5.png',dpi=1000,width=16,height=16,units='cm')
  # Note: "A" and "B" labels were added manually on GIMP