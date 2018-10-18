setwd("C:/Users/scm_c/Dropbox/GLR_MSU_postdoc/LTER KBS Project/DataBase/Deep_soilcores_Calc(2001-2013)")
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

################################################## Dataset 2001 #######################################################

tablecn<- read.csv("164-soilCN(2001).csv")
tablebd <-read.csv("308-soilBD(2001).csv")

### Merging TableCN with Tablebd

mydata <- (join(tablebd, tablecn, type = "left"))
NA_data <- mydata[rowSums(is.na(mydata)) >0,]
newdata <- dplyr::filter(mydata, !is.na(c_percent))

####Note: Total C and N in 2001 was only measured in core 1, therefore, NAs for core 2.
#         Missing data for T2 and T3 all bulk density and C&N
#         Missing C&N on core 1 only from 12 samples, which are mostly from deep sections.

newdata$soilC<- newdata$'c_percent'*newdata$'total_bulk_density'*newdata$'horizon_length' 

#Graph boxplot 2001 Soil C
qplot(section,soilC,data=newdata,geom='boxplot')+
  scale_x_discrete(limits=c("Surface","Middle","Deep"), labels = c("S","M","D"))+
  facet_grid(year~treatment)+
  labs(y = "Soil Carbon (Mg/ha)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
ggsave("2001_soilC.png",width= 10, height = 5, units="in") 

TotalSoilC <- aggregate(soilC ~ year + treatment + replicate, data = newdata, sum)

qplot(treatment,soilC,data=TotalSoilC,geom='boxplot')+
   labs(y = "Total Soil Carbon (Mg/ha)",
       x = "treatment") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
ggsave("2001_TotalSoilC.png",width= 10, height = 5, units="in") 

TotalSoilC2 <- aggregate(soilC ~ year + treatment + replicate + section, data = newdata, sum)

qplot(section,soilC,data=TotalSoilC2,geom='boxplot')+
  labs(y = "Total Soil Carbon (Mg/ha)",
       x = "Soil profile section") + 
  facet_wrap(~treatment)+
  scale_x_discrete(limits=c("Surface","Middle","Deep"), labels = c("S","M","D"))+
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())

ggsave("2001_TotalSoilC per section.png",width= 10, height = 5, units="in") 

######################################### Dataset 2013 ##########################################

table2013 <-read.csv("2013_soil_cn_with_bd.csv")
table2013$soilC2 <- table2013$'c_percent'*table2013$'total_bulk_density'*table2013$'horizon_length'
levels(table2013$section)<-c(levels(table2013$section),"10-25")
table2013$section[table2013$section=='25-Oct']<-'10-25'

#Summ all by rep
TotalSoilC3 <- aggregate(soilC2 ~ year + treatment + replicate, data = table2013, sum)

qplot(treatment,soilC2,data=TotalSoilC3,geom='boxplot')+
  labs(y = "Total Soil Carbon (Mg/ha)",
       x = "Treatment") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
ggsave("2013_TotalSoilC.png",width= 10, height = 5, units="in") 
#Sum all By Section
TotalSoilC4 <- aggregate(soilC2 ~ year + treatment + replicate + section, data = table2013, sum)

q<- qplot(section,soilC2,data=TotalSoilC4,geom='boxplot')+
  labs(y = "Total Soil Carbon (Mg/ha)",
       x = "Soil profile section (cm)") + 
  facet_wrap(~treatment)+
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
q+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(limits=c("0-10","10-25","25-50","50-end"))
ggsave("2013_TotalSoilC per section.png",width= 10, height = 5, units="in") 

######## Combining two datasets of 2001 and 2013 to calculate change in SoilC

finaltable <-merge(TotalSoilC,TotalSoilC3, by = c("treatment","replicate"))
finaltable$ChangeSoilC <- finaltable$'soilC2' - finaltable$'soilC'

qplot(treatment,ChangeSoilC,data=finaltable,geom='boxplot')+
  labs(y = "Total Soil Carbon (Mg/ha)",
       x = "Treatment") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())

ggsave("ChangeTotalSoilC 2001-2013.png",width= 10, height = 5, units="in") 



########################################### CC: The below code is to plot separately all variables #######################################
# Averaging samples collected per rep (station and cores) at different depths
#######
cntidy <- tablecn%>%
  select(-station,-core,-replicate)%>%
  gather(Variable,Value,-c(year,treatment,section))%>%
  group_by(Variable,year,treatment,section)%>%
  summarize(
    means=mean(Value,na.rm=T),
    se=sd(Value,na.rm=T),
    n=length(Value)
  )

bdtidy <- tablebd%>%
  select(-station,-core, -horizon_length,-replicate)%>%
  gather(Variable,Value,-c(year,treatment,section))%>%
  group_by(Variable,year,treatment,section)%>%
  summarize(
    means=mean(Value,na.rm=T),
    se=sd(Value,na.rm=T),
    n=length(Value)
  )

y2013tidy <- table2013%>%
  select(-station,-core,-replicate,-horizon_length,-core_volume_cm3,-gravel_free_bulk_density,-top_depth,-bottom_depth,-depth)%>%
  gather(Variable,Value,-c(year,treatment,section))%>%
  group_by(Variable,year,treatment,section)%>%
  summarize(
    means=mean(Value,na.rm=T),
    se=sd(Value,na.rm=T),
    n=length(Value)
  )
######
#Graphs per year 2001-2010
#####
#Graph boxplot 2001 vs 2010 %C
qplot(section,c_percent,data=tablecn,geom='boxplot')+
  scale_x_discrete(limits=c("Surface","Middle","Deep"), labels = c("S","M","D"))+
  facet_grid(year~treatment)+
  labs(y = "Soil Carbon (%)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
ggsave("2001-2010_cpercent.png",width= 10, height = 5, units="in") 

#Graph boxplot 2001 vs 2010 %N
qplot(section,n_percent,data=tablecn,geom='boxplot')+
  scale_x_discrete(limits=c("Surface","Middle","Deep"), labels = c("S","M","D"))+
  facet_grid(year~treatment)+
  labs(y = "Soil Nitrogen (%)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())

ggsave("2001-2010_npercent.png",width= 10, height = 5, units="in") 

#Graph boxplot 2001 vs 2010 Bulk density
qplot(section,total_bulk_density,data=tablebd,geom='boxplot')+
  scale_x_discrete(limits=c("Surface","Middle","Deep"), labels = c("S","M","D"))+
  facet_grid(year~treatment)+
  labs(y = "Soil bulk density(g/cm3)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())

ggsave("2001-2010_bulkdensity.png",width= 10, height = 5, units="in") 

#####
#Graphs 2013 
#Graph boxplot 2013 %C
q <- qplot(section,c_percent,data=table2013,geom='boxplot')+
  facet_grid(~treatment)+
  labs(y = "Soil Carbon (%)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
q+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(limits=c("0-10","10-25","25-50","50-end"))+
  scale_y_continuous(limits=c(0,4))

ggsave("2013_cpercent.png",width= 10, height = 3.5, units="in") 

###GRaph boxplot 2013 %N
q <- qplot(section,n_percent,data=table2013,geom='boxplot')+
  facet_grid(~treatment)+
  labs(y = "Soil Nitrogen (%)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
q+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(limits=c("0-10","10-25","25-50","50-end"))+
  scale_y_continuous(limits=c(0,0.3))

ggsave("2013_npercent.png",width= 10, height = 3.5, units="in") 

#Graph boxplot 2013 Soil C (Mg/ha)

q <- qplot(section,soilC,data=table2013,geom='boxplot')+
  facet_grid(~treatment)+
  labs(y = "Soil Carbon (Mg/ha)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
q+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(limits=c("0-10","10-25","25-50","50-end"))
  #scale_y_continuous(limits=c(0,4))

ggsave("2013_soilC.png",width= 10, height = 3.5, units="in") 

###GRaph boxplot 2013 bulkdensity
q <- qplot(section,total_bulk_density,data=table2013,geom='boxplot')+
  facet_grid(~treatment)+
  labs(y = "Soil bulk density (g/cm3)",
       x = "Soil section depth") + 
  ggthemes::theme_base(base_size =20)+
  theme(plot.background = element_blank(),
        strip.background = element_blank())
q+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(limits=c("0-10","10-25","25-50","50-end"))+
  #scale_y_continuous(limits=c(0,0.3))

ggsave("2013_soilbd.png",width= 10, height = 3.5, units="in") 

#new<- new%>%filter(year=="2001")%>%na.omit()                       


#sample_date2<-format(as.Date(tablebd1$sample_date, format = "%Y-%m-%d"), "%Y")
#df<- data.frame(year = sample_date2)
#df$year <- as.factor(df$year)
#new <- do.call(cbind, list(tablebd1,df))





