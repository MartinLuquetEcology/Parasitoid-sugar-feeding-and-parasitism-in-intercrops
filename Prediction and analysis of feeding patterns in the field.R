library(randomForest)
library(ggplot2)
library(RVAideMemoire)
library(car)
library(ggpubr)
library(vegan)
library(ade4)

    #### Loading some functions ####

  # Set working directory here
  # setwd("~/Boulot/Thèse 2016-2019/AOBox/R/Scripts/Terrain 2018")

source('Functions - for publication.R', encoding = 'UTF-8')



    #### Importing Data ####

  # setwd("~/Boulot/Thèse 2016-2019/AOBox/R/Scripts/Terrain 2018")
lab_training <- read.table('labdata_for_predictions.txt',h=T)
  # This is a randomly selected subsample of the original dataset, as balanced as possible
  # See "Creating the training table" script
  # Contains 183 individuals
  # Note that all individuals that fed less than 12h ago are considered as "fed"
  # Hence the treatment variable is named "Tmt.12"
lab_training$Tmt.12 <- as.factor(lab_training$Tmt.12)

  # Field data
field <- read.table('field_data_sugars.txt',h=T)
field$TP <- paste(field$Field_type,field$Field_position) #Field type+position

    #### Looking at patterns in the lab data ####

  # Let's first look at relationships within the lab dataset
  # GF Ratio and the Total Sugar Amount are known to be important
  # markers of sugar feeding
  # let's also look at other variables,
  # notably the H Ratio that should discriminate unfed from honeydew individuals

  # Let's first plot GF Ratio vs Total Sugar Amount
ggplot(lab_training,aes(y=GF_Ratio,x=TotSug,colour=Tmt.12))+
  geom_point(size=2)+
  labs(fill='Feeding class')+
  ylab('GF Ratio')+
  xlab('Total Sugar Amount (µg/mg insect)')+
  labs(colour='Feeding class')+
  theme(legend.title=element_text(face='bold'),legend.position='bottom')


  # Total Sugar Amount will not be a predictor in the model as all sugars will be present
  # Let's use Fructose, the main sugar in the insects, instead of Total Sugar Amount
Fig3 <- ggplot(lab_training,aes(y=GF_Ratio,x=Fructose,colour=Tmt.12))+
  geom_point(size=2)+
  labs(fill='Feeding class')+
  ylab('GF Ratio')+
  xlab('Fructose (µg/mg insect)')+
  labs(colour='Feeding class')+
  theme(legend.title=element_text(face='bold'))

  #We get quite the same thing
  #To save the plot:
  #ggsave(Fig3,filename='Figure_3.png',units='cm',dpi=1000,width=16,height=10)


  # To understand better these patterns, let's look how insect profiles vary both according to their
  # sugar treatment and time treatment
  # Unfed insects with high sugar levels could actually be "fed then starved" insects

    # Let's just create a variable "Tmt"
    # This is the "real" status of the insects: insects that fed from nectar 24 or 48h ago will be depicted
    # as nectar-fed here
lab_training$Tmt <- rep(0,nrow(lab_training))
lab_training[lab_training$Sugar_treatment=='ApH'|lab_training$Sugar_treatment=='SaH'|
               lab_training$Sugar_treatment=='AfH','Tmt'] <- 'Honeydew'
lab_training[lab_training$Sugar_treatment=='EFN'|lab_training$Sugar_treatment=='VPN',
             'Tmt'] <- 'Nectar'
lab_training[lab_training$Sugar_treatment=='Unfed','Tmt'] <- 'Unfed'
lab_training$Hour_treatment <- factor(lab_training$Hour_treatment,
                                      levels=levels(as.factor(lab_training$Hour_treatment))
                                                                               [c(1,3,2,4,5)])

# Total Sugar Amount vs GF Ratio
ggplot(lab_training,aes(y=GF_Ratio,x=TotSug,colour=Tmt,shape=Hour_treatment))+
  geom_point(size=2)+
  theme(legend.position = 'bottom',
        legend.title=element_blank())+
  xlab('Total Sugar Amount (µg/mg insect)')+
  ylab('GF Ratio')+
  ggtitle('Total Sugar Amount vs GF Ratio')   ## Some insects that fed on nectar a long time ago still have high profiles
                                              ## But with a lot of variation
                                              ## Changing the detection time would not be very useful
                                              ## As some others have low profiles
                                              ## Even more in honeydew-fed insects

# Fructose vs GF Ratio
ggplot(lab_training,aes(y=GF_Ratio,x=Fructose,colour=Tmt,shape=Hour_treatment))+
  geom_point(size=2)+
  theme(legend.position = 'bottom',
        legend.title=element_blank())+
  xlab('Fructose (µg/mg insect)')+
  ylab('GF Ratio')+
  ggtitle('Fructose vs GF Ratio')

# Fructose vs Glucose
ggplot(lab_training,aes(y=Glucose,x=Fructose,colour=Tmt,shape=Hour_treatment))+
  geom_point(size=2)+
  theme(legend.position = 'bottom',
        legend.title=element_blank())+
  ylab('Glucose (µg/mg insect)')+
  xlab('Fructose (µg/mg insect)')+
  ggtitle('Fructose vs Glucose')

# Fructose vs Honeydew Ratio
ggplot(lab_training,aes(y=H_Ratio,x=Fructose,colour=Tmt,shape=Hour_treatment))+
  geom_point(size=2)+
  theme(legend.position = 'bottom',
        legend.title=element_blank())+
  xlab('Fructose (µg/mg insect)')+
  ylab('Honeydew ratio')+
  ggtitle('Fructose vs H Ratio')  ## Not perfect but we see a separation between nectar-fed and honeydew-fed insects



    #### Calculating the 'noise index' on the lab data ####

  ## Okay so now we can try to build a classifier with our lab data
  ## Using sugar variables
  ## In order to predict field data later
  ## Let's calculate the 'noise index' to choose a method (Luquet et al. 2020, Ecol. Entom. -> see paper)

rda.lab_training <- rda(lab_training[,6:14],lab_training$Tmt.12)

  # Noise index:
1 - summary(rda.lab_training )$constr.chi/summary(rda.lab_training )$tot.chi
  # The noise index is 0.58 --> Random Forest (we didn't need it anyway)



      #### Training the model ####

# Random seed
set.seed(9)
# Defining the formula here
formula.sug  <- as.formula(paste("Tmt.12 ~ ", 
                                 paste(c('Glucose','Fructose','Sucrose','GF_Ratio','H_Ratio',
                                         'Melezitose','Erlose','Maltose','Stachyose'), 
                                 collapse= "+")))

# Training the Random Forest
RF.sug <- randomForest(formula.sug,data=lab_training)

  # Confusion matrix
RF.sug 
  # Variable relative importance
varImpPlot(RF.sug)



      #### Predicting field data ####

  ## Prediciton using Random Forest
field.pred <- predict(RF.sug,newdata=field)


  ## Let's visualize without adjusting the counts

  # creating the table
tabPred <- data.frame(Pred = field.pred, Type = field$Field_type,
                      Position = field$Field_position)
tabPred$Pred <- relevel(tabPred$Pred,'Unfed')

  # plot
ggplot(tabPred , aes(Position, fill = Pred))+
  geom_bar(position = "fill")+
  facet_wrap(~Type)+
  ggtitle('Field data - Random Forest predictions- Unadjusted')


  ## Now let's adjust (see function at the end)
tabField <- table(field$TP,field.pred)
table.adj <- apply(tabField,1,function(x) adjust.prev(x,RF.sug$confusion[,1:3]))

  ## Creating the table
   # Transforming frequencies into counts
transf <- as.data.frame(as.table(table.adj))
tabAdj <- countsToCases(transf)

tabAdj$Type <- sub(" 5.*", "", tabAdj$Var2)
tabAdj$Position <- sub(".* ", "", tabAdj$Var2)

  ## Visualization
ggplot(tabAdj , aes(Position, fill = field.pred))+
  geom_bar(position = "fill")+
  facet_wrap(~Type)+
  ggtitle('Field data - Random Forest predictions- Adjusted')

## Same as unadjusted, except there the few unfed individuals are now predicted as honeydew-fed



  #### Comparing prevalences (chisq test) ####
print(chisq.test(table.adj[1:2,]))


  #### Figure 4 ####

  ## Some re-arragement
tabAdj$'Feeding_class' <- as.character(tabAdj[,1])
tabAdj$'Feeding_class'[tabAdj$'Feeding_class'=='Honeydew'] <- 'Honeydew-fed'
tabAdj$'Feeding_class'[tabAdj$'Feeding_class'=='Nectar'] <- 'Nectar-fed'
tabAdj$Position <- relevel(as.factor(tabAdj$Position ),'5m')

  ## The figure
Fig4 <- ggplot(tabAdj , aes(Position, fill = Feeding_class))+
  geom_bar(position = "fill")+
  facet_wrap(~Type)+
  ggtitle('Percentage of individuals from each feeding class')+
  ylab('Percentage of individuals')+
  xlab('Distance from the border')+
  theme(legend.position="bottom")+
  labs(fill='Feeding class')+
  theme(legend.title=element_text(face='bold'))

#ggsave(Fig4,filename='Figure_4.png',height=14,width=16, unit='cm',dpi=1000)



      #### Exploring field data distribution ####

  ## To see how the sugar levels of insects are distributed according to the field type
  ## and distance from the border


    #Just some transformations for the graphs
field$Field_position <- as.factor(field$Field_position)
field$Field_position <- relevel(field$Field_position,'5m')
field$Field <- factor(field$Field,
                      levels=levels(as.factor(field$Field))[c(4,5,6,7,1,2,3)]
                      )


  ## A. Sugar levels according to crop type
ggplot(field, aes(y=TotSug, x= Field_type,fill=Field_type))+
facet_wrap(~Field_position)+
  geom_boxplot(alpha=0.6,outlier.shape = NA)+
  geom_point(aes(colour=Field_type),alpha=1,size=1.5,
             position = position_jitterdodge())+
  xlab('Field')+
  ylab('Total sugar content (µg/mg insect)')


  ## B. Looking field per field

ggplot(field, aes(y=TotSug, x= Field,fill=Field_type)) +
  facet_wrap(~Field_position)+
  geom_boxplot(alpha=0.6,outlier.shape = NA)+
  geom_point(aes(colour=Field_type),alpha=1,size=1.5,
             position = position_jitterdodge())+
  xlab('Field')+
  ylab('Total sugar content (µg/mg insect)')+
  labs(fill='Field type',colour='Field type')+
  theme(legend.title=element_text(face='bold'))


  ## C. Density graphs

    ## C1. Distribution of sugar levels according to crop types

Triticale <- ggplot(field[field$Field_type=='Triticale',], aes(x=TotSug, fill=Field_position)) +
  geom_histogram(aes(y=..density..), alpha=0.2, position="identity",binwidth=10)+
  geom_density(alpha=0.5)+
  ggtitle('Pure Triticale')+
  scale_x_continuous(lim=c(0,450))

Intercrop <- ggplot(field[field$Field_type=='Intercrop',], aes(x=TotSug, fill=Field_position)) +
  geom_histogram(aes(y=..density..), alpha=0.2, position="identity",binwidth=10)+
  geom_density(alpha=0.5)+
  ggtitle('Triticale - Faba bean intercrop')+
  scale_x_continuous(lim=c(0,450))

# The graph is here
ggarrange(Triticale,Intercrop,common.legend=T)

  ## C1. Distribution of sugar levels according to distance from the border

FiveM <- ggplot(field[field$Field_position=='5m',], aes(x=TotSug, fill=Field_type)) +
  geom_histogram(aes(y=..density..), alpha=0.2, position="identity",binwidth=10)+
  geom_density(alpha=0.5)+
  ggtitle('5m from field border')+
  scale_x_continuous(lim=c(0,450))

FiftyM <- ggplot(field[field$Field_position=='50m',], aes(x=TotSug, fill=Field_type)) +
  geom_histogram(aes(y=..density..), alpha=0.2, position="identity",binwidth=10)+
  geom_density(alpha=0.5)+
  ggtitle('50m from field border')+
  scale_x_continuous(lim=c(0,450))

# The graph is here
ggarrange(FiveM,FiftyM,common.legend=T)



      #### Comparing sugar levels ####

modTotSug <- glm(TotSug ~ Field_position*Field_type,family='Gamma',data=field)

plotresid(modTotSug) # model is ok although residuals are not totally homogeneous
Anova(modTotSug) # no significant difference

modGF <- lm(GF_Ratio ~ Field_position*Field_type,data=field)

plotresid(modGF) # model is ok although extreme values are poorly predicted
Anova(modGF) # no significant difference


      #### Figure 5 ####

Fig5 <- ggplot(field, aes(y=TotSug, x= Field,fill=Field_type)) +
  facet_wrap(~Field_position)+
  geom_boxplot(alpha=0.6,outlier.shape = NA)+
  geom_point(aes(colour=Field_type),alpha=1,size=1.5,
             position = position_jitterdodge())+
  xlab('Field')+
  ylab('Total sugar content (µg/mg insect)')+
  labs(fill='Field type',colour='Field type')+
  theme(legend.title=element_text(face='bold'))
#ggsave(Fig5,filename='Figure_5.png',height=10,width=16, unit='cm',dpi=1000)

