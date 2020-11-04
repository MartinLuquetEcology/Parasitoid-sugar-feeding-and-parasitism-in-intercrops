library(ggplot2)
library(Rmisc)
library(lme4)
library(car)
library(emmeans)
library(scales)
library(reshape2)
library(RVAideMemoire)
library(vegan)

  # Set working directory here
  #setwd("~/Boulot/Thèse 2016-2019/AOBox/R/Scripts/Terrain 2018")



      ##### Sugar profiles of foods and insects #####

  ## Importing the data
  ### This is the complete dataset with the sugar profiles for insects and food types
sugar.lab.data <- read.table(file='all_lab_data.txt',h=T,dec=',')

  ## Selecting the relevant variables and transforming the table
FI.table <- sugar.lab.data[,c(3,4,c(6:12),16)]
FI.melt <- melt(FI.table,id.vars=c('Sugar_treatment','Hour_treatment','Type'))

  ## In this table we get the mean, sd and se value for each food and insect treatment
  ## (For insects: Sugar * time treatment)
allSL <- summarySE(FI.melt, measurevar="value", groupvars=c("Type","Sugar_treatment","Hour_treatment","variable"))
allSL

  ## Now let's look at the correspondance between food source and insect sugar profiles
  ## We take food sources and insects that just fed or emerged
  ## i.e. only the '0h' treatment
FI.melt0 <- FI.melt[FI.melt$Hour_treatment=='0h' | FI.melt$Type=='Food_source',]

  ## We can have a first view of the sugar profiles of each treatment on this graph
  ## Although it's hard to examine the data because VfN and VsN have values way higher than the other treatments
ggplot(FI.melt0,aes(x=Sugar_treatment,y=value,fill=variable))+
  stat_summary(fun="mean", geom="bar",position='stack')+
  geom_bar(stat='identity')+
  facet_wrap(~Type,ncol=1)

  ## In the next graph we'll look at relative sugar compositions (proportionally to the total sugar level for each treatment)

  ## Calculating the mean value for each treatment
FI.means <- aggregate(FI.melt0$value,
                      list(FI.melt0$Sugar_treatment,FI.melt0$Type,FI.melt0$variable)
                      ,mean )

  ## Renaming everything
colnames(FI.means) <- c('Treatment','Type','Sugar','Value')

  ## Some re-arrangement
FI.means$Sugar <- factor(FI.means$Sugar,levels=levels(FI.means$Sugar)[c(1,2,3,6,5,4,7)])
FI.means$Treatment <- as.factor(FI.means$Treatment)
FI.means$Treatment <- relevel(FI.means$Treatment,'Unfed')
levels(FI.means$Treatment )[1] <- 'No food'
FI.means$Type2 <- as.character(FI.means$Treatment)
FI.means$Type2[FI.means$Treatment=='AfH' | FI.means$Treatment=='ApH' | 
                 FI.means$Treatment=='SaH'] <- 'Honeydew'
FI.means$Type2[FI.means$Treatment=='VfN' | FI.means$Treatment=='VsN' ] <- 'Nectar'
FI.means$Type2 <- relevel(as.factor(FI.means$Type2),'No food')
levels(FI.means$Treatment )[1] <- ''
levels(FI.means$Type)[1] <- 'Food source'

  ## Preparing the graph
Fig1 <- ggplot(FI.means,aes(x=Treatment,y=Value,fill=Sugar))+
  geom_bar(position=position_fill(reverse = TRUE),stat='identity')+
  facet_grid(Type~Type2,scale='free',space='free',switch='both')+
  theme(axis.title.x = element_blank(),
        strip.text.y = element_text(size=11, face="bold"),
        strip.background.y = element_rect(colour="black"),
        legend.position='bottom',
        legend.title=element_blank(),
        legend.key.size = unit(0.8,"line"),
        legend.spacing.x = unit(0.15, 'cm'))+
  ylab('Relative sugar concentration')+
  guides(fill = guide_legend(nrow = 1))+
  scale_fill_manual(values = c('#D55E00','#56B4E9','#E69F00',
                               '#009E73','#F0E442','#0072B2',
                               '#CC79A7'))


## Figure 1 is here
Fig1

#ggsave(plot=Fig1,filename='Figure_1.png',unit='cm',height=16,width=16,dpi=1000)

  ## Now let's look sugar per sugar





      #### Dynamic of total sugar levels according to the sugar treatment ####

  ## Let's look at the temporal dynamics of sugar profiles in insects
  ## We'll keep only the insect data
insects.data <- sugar.lab.data[sugar.lab.data$Type=='Parasitoid',-16]

  ## Let's create a function to plot the dynamics

dynamics.plot <- function(variable,variable.name,rm=F) {
  
  ins.Sug <- summarySE(insects.data, measurevar=as.character(variable), groupvars=c("Hour_treatment",
                                                                                    "Sugar_treatment"))
  
  ## Redefining levels in the right order
  ins.Sug$Hour_treatment <- factor(ins.Sug$Hour_treatment,
                                   levels(as.factor(ins.Sug$Hour_treatment))[c(1,3,2,4,5)])
  
  ## Redifining the "Time" variable
  ins.Sug$Time <- as.numeric(sub("\\h.*", "", ins.Sug$Hour_treatment))
  
  ## Creating a new variable to order treatments according to sugar treatment type 
  ## (honeydew, nectar, no sugar)
  ins.Sug$SugType <- c(c('Honeydew','Honeydew','Honeydew','Unfed','Nectar','Nectar'),
                       rep(c('Honeydew','Honeydew','Unfed','Nectar'),4))
  
  if(rm==T){ins.Sug <- ins.Sug[ins.Sug$Sugar_treatment!='VsN' & ins.Sug$Sugar_treatment!='AfH' ,]}
  
  ## Creating a character variable
  ins.Sug$Sugar <- as.character(ins.Sug$Sugar_treatment)
  
  ## Creates a graph showing sugar dynamics for each sugar treatment
  
  ins.Sug$variable <- ins.Sug[,variable]
  
  Dynamics <- ggplot(ins.Sug,aes(y=variable,x=Time,colour=Sugar,shape=Sugar))+
    geom_errorbar(aes(ymin=variable-se, ymax=variable+se), width=.1)+
    geom_line(aes(linetype=Sugar))+
    geom_point(size=3)+
    xlab("Time (h)")+
    ylab(paste(variable.name,"(µg/mg insect)"))+
    theme(legend.position="bottom",legend.title=element_text(face='bold'))+
    guides(color=guide_legend(nrow=1))
  
  return(Dynamics)
}




  ## The plots are here
dynamics.plot("TotSug","Total Sugar Amount") 
dynamics.plot("TotSug","Total Sugar Amount",rm=T) #rm=T to remove AfH and VsN, for which we don't have the dynamics 
dynamics.plot("Glucose","Glucose",rm=T)+ggtitle('Glucose')
dynamics.plot("Fructose","Fructose",rm=T)+ggtitle('Fructose')
dynamics.plot("Sucrose","Sucrose", rm=T)+ggtitle('Sucrose')
dynamics.plot("Erlose","Erlose",rm=T)+ggtitle('Erlose')
dynamics.plot("Melezitose","Melezitose",rm=T)+ggtitle('Melezitose')
dynamics.plot("Maltose","Maltose",rm=T)+ggtitle('Maltose')
dynamics.plot("Stacchyose","Stacchyose",rm=T)+ggtitle('Stacchyose')
dynamics.plot("GF_Ratio","GF Ratio",rm=T)+ylab('Glucose/(Glucose+Fructose)')+ggtitle('GF Ratio')
dynamics.plot("H_Ratio","H Ratio",rm=T)+ylab('(Maltose+Melezitose+Erlose)/Total Sugar Amount')+ggtitle('H Ratio')

  ## Let's make a pretty figure (Fig.2)

Fig2 <- dynamics.plot("TotSug","Total Sugar Amount")+
                      scale_colour_manual(name = "Food source",
                          labels = c("AfH", "ApH", "SaH", "Unfed","VfN","VsN"),
                          values =c("#F8766D", "#B79F00", "#619CFF", "#00BA38", 
                                     "#00BFC4", "#F564E3")) +
                      scale_shape_manual(name = "Food source",
                          labels = c("AfH", "ApH", "SaH", "Unfed","VfN","VsN"),
                          values = c(16,16,16,15,17,17))+
                      scale_linetype_manual(name = "Food source",
                          labels = c("AfH", "ApH", "SaH", "Unfed","VfN","VsN"),
                          values = c("solid","solid","solid",22,42,42))

Fig2

  ## To save Figure 2
#ggsave(Fig2,filename = 'Figure_2.png',units='cm',dpi=1000,width=16,height=12)

# To get the graphs for the other variables, just select line 98 to 138 and replace all mentions of 'TotSug'
# for the variable of interest



      #### Comparing overall sugar levels  according to sugar treatment ####

  ## A. Food sources

modSL_sug <- glm(TotSug ~ Sugar_treatment, data=sugar.lab.data[sugar.lab.data$Type=='Food_source',],family=Gamma)
plotresid(modSL_sug)
Anova(modSL_sug)
LSM_sug <- lsmeans(modSL_sug,~Sugar_treatment)
contrast(LSM_sug,'pairwise')


  ## B. Insects
  ## In this model we consider only the insects that have just fed, or that just have just emerged
  ## Thus, only the '0h' treatment

modSL_ins <- glm(TotSug ~ Sugar_treatment, data=insects.data[insects.data$Hour_treatment=='0h',],family=Gamma)
plotresid(modSL_ins)
Anova(modSL_ins)
LSM_ins <- lsmeans(modSL_ins,~Sugar_treatment)
contrast(LSM_ins,'pairwise')


      #### Comparing sugar levels  according to parasitoid origin ####

## Some graphs first

PO.melt <- melt(insects.data[,c(2:4,6:15,17)],id.vars=c('Parasitoid_Origin','Sugar_treatment','Hour_treatment','Tmt.12'))

  # Let's first see how the data is distributed across each sugar and time treatment for each variable and origin
ggplot(PO.melt,aes(x=Parasitoid_Origin,y=value,fill=Parasitoid_Origin))+
  facet_wrap(~variable,scales='free')+
  geom_boxplot(alpha=0.6,outlier.shape = NA)+
  geom_point(aes(colour=Parasitoid_Origin),alpha=1,size=0.5,
             position = position_jitterdodge())

  # Now along each Feeding class
ggplot(PO.melt,aes(x=Parasitoid_Origin,y=value,fill=Parasitoid_Origin))+
  facet_grid(Tmt.12~variable,scales='free')+
  geom_boxplot(alpha=0.6,outlier.shape = NA)+
  geom_point(aes(colour=Parasitoid_Origin),alpha=1,size=0.5,
             position = position_jitterdodge())

  # Now doing the redundancy analysis
RDA_origin <- rda(insects.data[,c(6,15)]~as.factor(insects.data$Parasitoid_Origin))
plot(RDA_origin)
set.seed(26)
anova.cca(RDA_origin)
ordistep(RDA_origin,perm.max=1000)

