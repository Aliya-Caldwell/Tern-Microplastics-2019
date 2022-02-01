 
rm(list=ls())



library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(grid)
library(palettetown)
library(colorspace)
library(RColorBrewer)
library(pals)
library(wesanderson)




# PROPORTION OF FIBER IDENTITIES ------------------------------------------


getwd()
setwd("/Users/aliyacaldwell/Box/Undergraduate Research/Tern microplastics/FTIR/FinalFTIRdata")
setwd("C:/Users/aec1075/Box/Pre PhD Research/Tern microplastics/FTIR/FinalFTIRdata")

df1<-read.csv("allFTIRsamples.csv",fileEncoding="UTF-8-BOM")

df1feces<-subset(df1, SampleType=="blank")

#----------------------------------------#
# proportion of synthetic vs natural etc #
#----------------------------------------#

#proportion across all sample tupes
dfsummary<-df1 %>% 
  dplyr::group_by(Categorization) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n)) 

#prop for blanks vs feces
dfsummary<-df1 %>% 
  dplyr::group_by(SampleType, Categorization) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n)) 


#plot proportions of identities beween feces and blanks
ggplot(dfsummary)+
  geom_col(aes(x=SampleType, y=prop, fill=Categorization))+theme_bw()+
  ylab("Proportion")+xlab("Sample Type")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_poke(pokemon="surskit", spread=5)

#chi squared to see if these differ#
  #field blanks vs cote vs rost
tbl<-table(df1$Species,df1$Categorization)
tbl
chisq.test(table(df1$Species,df1$Categorization))

#-----------------------------------#
#proportion of different fiber types#
#-----------------------------------#

  ## for COTE vs ROST in NH ##
NHterns<-subset(df1,CollectionState=="NH")
dfterns<-subset(NHterns,Species=="COTE"|Species=="ROST")

dfsummary2<-dfterns %>% 
  dplyr::group_by(Species, ResultSummary) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n))

ggplot(dfsummary2)+
  geom_col(aes(x=Species, y=prop, fill=ResultSummary))+theme_bw()+
  ylab("Proportion")+xlab("Fiber Type")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_poke(pokemon="surskit", spread=11)

## for COTE AHY VS HY IN NH ##
NHterns<-subset(df1,CollectionState=="NH")
dfterns<-subset(NHterns,Species=="COTE")

dfsummary5<-dfterns %>% 
  dplyr::group_by(Species, Age, ResultSummary) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n))

ggplot(dfsummary5)+
  geom_col(aes(x=Age, y=prop, fill=ResultSummary))+theme_bw()+
  ylab("Proportion")+xlab("Fiber Type")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_poke(pokemon="surskit", spread=11)

    ## for COTE vs ROST chicks in NH ##
NHchicks<-subset(df1,CollectionState=="NH"&Age=="HY")

dfsummary3<-NHchicks %>% 
  dplyr::group_by(Species, ResultSummary) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n))

ggplot(dfsummary3)+
  geom_col(aes(x=Species, y=prop, fill=ResultSummary))+theme_bw()+
  ylab("Proportion")+xlab("Fiber Type")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_poke(pokemon="surskit", spread=11)

    ##for adult COTE across states##
dfCOTE<-subset(df1, Species=="COTE" & Age=="AHY")

dfsummary4<-dfCOTE %>% 
  dplyr::group_by(CollectionState, ResultSummary) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n))

ggplot(dfsummary4)+
  geom_col(aes(x=CollectionState, y=prop, fill=ResultSummary))+theme_bw()+
  ylab("Proportion")+xlab("Fiber Type")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_poke(pokemon="surskit", spread=11)

    ## by substrate ##
NHterns<-subset(df1,CollectionState=="NH"&Species=="COTE"|Species=="ROST")
NHsubstrates<-subset(NHterns, Substrate=="skin"|Substrate=="cloth"|Substrate=="metal"|
                       Substrate=="data sheet"|Substrate=="straw hat")                  

dfsummary5<-NHsubstrates %>% 
  dplyr::group_by(Substrate, ResultSummary) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n))

ggplot(dfsummary5)+
  geom_col(aes(x=Substrate, y=prop, fill=ResultSummary))+theme_bw()+
  ylab("Proportion")+xlab("Fiber Type")+
  theme(axis.text = element_text(size=12), axis.title = element_text(size=15),
        legend.text = element_text(size=12),legend.title = element_text(size=15),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  scale_fill_poke(pokemon="surskit", spread=11)


# PROPORTIONS OF FIBER COLORS ---------------------------------------------


getwd()
setwd("/Users/aliyacaldwell/Box/Undergraduate Research/Tern microplastics/FTIR/FinalFTIRdata")
setwd("C:/Users/aec1075/Box/Pre PhD Research/Tern microplastics/FTIR/FinalFTIRdata")

df1<-read.csv("allFTIRsamples.csv",fileEncoding="UTF-8-BOM")

#in fecal samples only
dfsummary0<-subset(df1, SampleType=="feces")

dfsummary<-dfsummary0 %>% 
  dplyr::group_by(ParticleColor) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n)) 

#in fecal samples only and for only anthropogenically derived particles 

dfsummaryanthro<-subset(df1, SampleType=="blank" & Categorization == "Synthetic"|Categorization=="Semi_Synthetic"|Categorization=="Dyed Natural")

dfsummaryanthro<-dfsummaryanthro %>% 
  dplyr::group_by(ParticleColor) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::mutate(prop=n/sum(n))



# FIBER SIZES -------------------------------------------------------------


df1feces<-subset(df1, SampleType=="feces")

df1synth<-subset(df1feces, Categorization=="Synthetic"|
                   Categorization=="Semi-Synthetic")

## SYNTHETIC AND SEMI SYNTHETIC ONLY ##

   #COTE vs ROST HY in NH
df1synthCOTEROST<-subset(df1synth, CollectionState=="NH"&Age=="HY")
ggplot(df1synthCOTEROST, aes(x=Species, y=ParticleSizemm))+
  geom_boxplot()
  #obviously not a big enough ss to do this

  #AHY COTE from all three states
df1synthAHY<-subset(df1synth, Age=="AHY" & Species=="COTE")

ggplot(df1synthAHY, aes(x=CollectionState, y=ParticleSizemm))+
  geom_boxplot()

## SYNTHETIC, SEMI SYNTHETIC AND DYED NATURAL ##

df1feces<-subset(df1, SampleType=="feces")

df1all<-subset(df1feces, Categorization=="Synthetic"|
                   Categorization=="Semi-Synthetic" |
                 Categorization=="Dyed Natural")

#mean size in feces
mean(df1all$ParticleSizemm, na.rm=TRUE)
sd(df1all$ParticleSizemm, na.rm=TRUE)

#means across groups

meansize<-df1all %>% 
  group_by(Species,Age,CollectionState) %>% 
  summarise(n=n(), mean=mean(ParticleSizemm, na.rm=TRUE), sd=sd(ParticleSizemm, na.rm=TRUE))
          

  #look at distribution
hist(df1all$ParticleSizemm)

  #check for normality
shapiro.test(df1all$ParticleSizemm)
     #p<0.0001 i.e. not normal
shapiro.test(log(df1all$ParticleSizemm+1)) #log trans
     #p=0.001 i.e. not normal
shapiro.test(sqrt(df1all$ParticleSizemm)) #sqrt trans
    #p<0.0001 i.e. not normal

  #COTE vs ROST HY in NH
df1allCOTEROST<-subset(df1all, CollectionState=="NH"&Age=="HY")
wilcox.test(df1allCOTEROST$ParticleSizemm~df1allCOTEROST$Species)
    #p=0.5308 W = 50.5,
ggplot(df1allCOTEROST, aes(x=Species, y=ParticleSizemm))+
  geom_boxplot()
    
  #AHY COTE from all three states
df1allAHY<-subset(df1all, Age=="AHY" & Species=="COTE")
kruskal.test(df1allAHY$ParticleSizemm~df1allAHY$CollectionState)
    #p=0.2806, chi-squared = 2.5415, df = 2
ggplot(df1allAHY, aes(x=CollectionState, y=ParticleSizemm))+
  geom_boxplot()

  #COTE AHY vs HY in NH
df1allCOTENH<-subset(df1all, CollectionState=="NH"&Species=="COTE")
wilcox.test(df1allCOTENH$ParticleSizemm~df1allCOTENH$Age)
    #W = 82, p-value = 0.01475

#boxplot AHY vs HY in NH
ggplot(df1allCOTENH, aes(x=Age, y=ParticleSizemm, fill=Age))+
  geom_boxplot()+theme_bw()+
  theme(legend.position = "none", axis.line.x = element_line(colour="black"),
        panel.grid = element_blank())+
  scale_fill_manual(values=c("khaki4","yellow3"))+
  labs(y="Particle size (mm)", x="")+
  scale_x_discrete(labels=c("AHY" = "Adult", "HY" = "Chick"))+
  theme(axis.text.x = element_text(size = 14, color="black"),axis.text.y=element_text(size=14,color="black"),
        axis.title.y=element_text(size=14))+
  annotate("text", x=1, y=4.8, label="a")+
  annotate("text", x=2, y=2.3, label="b")

#histogram AHY vs HY in NH
ggplot(df1allCOTENH,aes(x=ParticleSizemm,fill=Age,colour=Age))+
  geom_density(alpha=0.8,show.legend = TRUE)+
  theme_bw()+
  theme(legend.position = "right",
        panel.grid = element_blank())+
  xlab("Fiber Length (mm)")+
  theme(axis.text=element_text(size=14, color="black"),
        axis.title=element_text(size=15, face="bold",color="black"))+
  theme(legend.title = element_text(color = "black", face="bold",size = 15),
        legend.text = element_text(color = "black", size=14))+
  scale_fill_manual(name="Age Class",values=c("khaki3","salmon4"), labels = c("Adult", "Chick"))+
  scale_color_manual(name="Age Class",values=c("khaki3","salmon4"), labels = c("Adult", "Chick"))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 7)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, .5))




# STATS ON DATA CORRECTED USING FTIR RESULTS ------------------------------


getwd()
setwd("/Users/aliyacaldwell/Box/Pre PhD Research/Tern microplastics/FTIR/FinalFTIRdata")
setwd("C:/Users/aec1075/Box/Pre PhD Research/Tern microplastics/FTIR/FinalFTIRdata")

df<-read.csv("fecalFTIRsamples.csv",fileEncoding="UTF-8-BOM")


# DO ADULTS AND CHICKS VARY IN SHIT WEIGHT? -------------------------------


dfecal<-subset(df, SampleType=="fecal"& Species=="COTE"&CollectionState=="NH")
wilcox.test(dfecal$SampleMassGrams~dfecal$Age)
#p<0.0001

## visualization ##
ggplot(dfecal, aes(x=Age, y=SampleMassGrams))+
  geom_boxplot()

#--------------------#
##test for normality##
#--------------------#
  #p<0.05 = NOT NORMAl

  #all data including blanks
shapiro.test(df$SyntheticPerGramSample)
    #p<0.0001
shapiro.test(sqrt(df$SyntheticPerGramSample))
    #p<0.0001
shapiro.test(log(df$SyntheticPerGramSample+1))
    #p=0.001287 --> LOL almost

#---------------------#
#create useful subsets#
#---------------------#
    #by state
NHfeces<-subset(df, CollectionState=="NH"&SampleType=="fecal")
MAfeces<-subset(df, CollectionState=="MA"&SampleType=="fecal")
NJfeces<-subset(df, CollectionState=="NJ"&SampleType=="fecal")
    #COTE vs ROST for NH
NHfecesCOTE<-subset(NHfeces, Species=="COTE"&SampleType=="fecal")
    #blanks and feces from nh
NHblanksANDfeces<-subset(df, CollectionState="NH") %>% 
  filter(!Species %in% "lab blank")

#--------------#
#means and shit#
#--------------#

dffeces<-subset(df, SampleType=="fecal") %>% 
  group_by(Species,Age,CollectionState) %>% 
  summarise(n=n(), meansynthetic=mean(SyntheticPerGramSample), sdsynthetic=sd(SyntheticPerGramSample),
            meansemisynthetic=mean(SemiSyntheticPerGramSample),sdsemisynthetic=sd(SemiSyntheticPerGramSample),
            meandyednatural=mean(DyedNaturalPerGramSample),sdnaturaldyed=sd(DyedNaturalPerGramSample))

dfblanks<-subset(df, Species=="field blank") %>% 
  summarise(n=n(), meansynthetic=mean(SyntheticPerGramSample), sdsynthetic=sd(SyntheticPerGramSample),
            meansemisynthetic=mean(SemiSyntheticPerGramSample),sdsemisynthetic=sd(SemiSyntheticPerGramSample),
            meandyednatural=mean(DyedNaturalPerGramSample),sdnaturaldyed=sd(DyedNaturalPerGramSample))

#-----------------#
# BLANKS vs FECES #
#-----------------#

  ##number of synthetic fibers##
    #pairwise tests
wilcox.test(NHblanksANDfeces$SyntheticPerGramSample~NHblanksANDfeces$SampleType)
    #p<0.0001; W=436

  ## visualization ##
ggplot(NHblanksANDfeces, aes(x=SampleType, y=SyntheticPerGramSample))+
  geom_boxplot()


#----------------#
# NH vs MA vs NJ #
#----------------#
AHYfecesCOTE<-subset(df, Age=="AHY" & Species=="COTE")

  ## synthetic fibers in AHY COTE from NH,MA, and NJ
AHYfecesCOTE<-subset(df, Age=="AHY" & Species=="COTE")
kruskal.test(AHYfecesCOTE$SyntheticPerGramSample~AHYfecesCOTE$CollectionState)
    #p=0.01036, chi-squared = 9.1403, df = 2,
  ##post-hoc pairwise comparisons
NHandNJ<-subset(AHYfecesCOTE, CollectionState=="NH"|CollectionState=="NJ")
wilcox.test(NHandNJ$SyntheticPerGramSample~NHandNJ$CollectionState)
    #p=0.1087 W = 187
NHandMA<-subset(AHYfecesCOTE, CollectionState=="NH"|CollectionState=="MA")
wilcox.test(NHandMA$SyntheticPerGramSample~NHandMA$CollectionState)
    #p=0.002329 W = 377,
MAandNJ<-subset(AHYfecesCOTE, CollectionState=="MA"|CollectionState=="NJ")
wilcox.test(MAandNJ$SyntheticPerGramSample~MAandNJ$CollectionState)
    #p=0.1937 W = 237

  #visualize#
ggplot(AHYfecesCOTE, aes(x=CollectionState, y=SyntheticPerGramSample, fill=CollectionState))+
  geom_boxplot()+
  geom_jitter(width=0.09, shape=21)+
  theme_bw()+
  theme(legend.position = "none", axis.line.x = element_line(colour="black"),
                         panel.grid = element_blank())+
  scale_fill_manual(values=c("darkslategray","khaki3","darkseagreen4"))+
  labs(y="microplastic fibers/g feces", x="")+ylim(0,200)+
  geom_hline(yintercept=19.3, linetype="dashed", 
             color = "red", size=0.5)+
  theme(axis.text.x = element_text(size = 15,face="bold", color="black"),axis.text.y=element_text(size=14,color="black"),
        axis.title.y=element_text(size=15, face="bold"))+
  scale_x_discrete(limits=c("NH","NJ","MA"))+
  annotate("text", x=1, y=133, label="a", size=6, fontface=2)+
  annotate("text", x=2, y=132, label="a/b", size=6, fontface=2)+
  annotate("text", x=3, y=185, label="b", size=6, fontface=2)
  
  

#-----------#
# AHY vs HY #
#-----------#
#NH only, no MA HY and not enough NJ HY

  ##number of synthetic in NH COTE AHY vs HY##
wilcox.test(NHfecesCOTE$SyntheticPerGramSample~NHfecesCOTE$Age)
        #p=0.5903,W = 382
      #visualize NH only
ggplot(NHfecesCOTE, aes(x=Age, y=SyntheticPerGramSample,fill=Age))+
  geom_boxplot()+
  geom_jitter(width=0.09, shape=21)+
  theme_bw()+
  theme(legend.position = "none", axis.line.x = element_line(colour="black"),
        panel.grid = element_blank())+
  scale_fill_manual(values=c("khaki3","salmon4"))+
  labs(y="microplastic fibers/g feces", x="")+ylim(0,200)+
  geom_hline(yintercept=19.3, linetype="dashed", 
             color = "red", size=0.5)+
  scale_x_discrete(labels=c("AHY" = "Adult", "HY" = "Chick"))+
  theme(axis.text.x = element_text(size = 15, face="bold", color="black"),axis.text.y=element_text(size=14,color="black"),
        axis.title.y=element_text(size=15, face="bold"))+
  annotate("text", x=1, y=133, label="a", size=6, fontface=2)+
  annotate("text", x=2, y=93, label="a", size=6, fontface=2)

#--------------#
# COTE vs ROST #
#--------------#
#NH only, no ROST in NJ or MA

  ##number of synthetic in NH COTE vs ROST all ages##
wilcox.test(NHfeces$SyntheticPerGramSample~NHfeces$Species)
    #p=0.011

  ##number of synthetic in NH COTE vs ROST only HY##
NHfeceschicks<-subset(NHfeces, Age=="HY")
wilcox.test(NHfeceschicks$SyntheticPerGramSample~NHfeceschicks$Species)
    #p=0.006966 W = 155

  #visualize all ages#
ggplot(NHfeces, aes(x=Species, y=SyntheticPerGramSample))+
  geom_boxplot()
  #visualize HY only#
ggplot(NHfeceschicks, aes(x=Species, y=SyntheticPerGramSample,fill=Species))+
  geom_boxplot()+
  geom_jitter(width=0.09, shape=21)+
  theme_bw()+
  theme(legend.position = "none", axis.line.x = element_line(colour="black"),
        panel.grid = element_blank())+
  scale_fill_manual(values=c("lightcyan3","indianred4"))+
  labs(y="microplastic fibers/g feces", x="")+ylim(0,200)+
  geom_hline(yintercept=19.3, linetype="dashed", 
             color = "red", size=0.5)+
  scale_x_discrete(labels=c("COTE" = "Common Tern", "ROST" = "Roseate Tern"))+
  theme(axis.text.x = element_text(size = 15, face="bold", color="black"),axis.text.y=element_text(size=14,color="black"),
        axis.title.y=element_text(size=15, face="bold"))+
  annotate("text", x=1, y=94, label="a", fontface=2, size=6)+
  annotate("text", x=2, y=167, label="b", fontface=2, size=6)
  

