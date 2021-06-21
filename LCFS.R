library(data.table)
library(ggridges)
library(ggplot2)
library(viridis)
library(dplyr)
library(tidyr)
library(scales)
library(extrafont)
library(ragg)
library(paletteer)
library(forcats)

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Transaction level data from the Living Costs and Food Survey 
#(not publicly available, sorry)
data<-fread("Data/LCFSTrans.csv")

data$wgt<-data$alccontent*data$weight

data$ageband<-case_when(
  data$age<25 ~ 1,
  data$age<35 ~ 2,
  data$age<45 ~ 3,
  data$age<55 ~ 4,
  data$age<65 ~ 5,
  data$age<75 ~ 6,
  data$age<85 ~ 7
)

data$drinkcat <- case_when(
  data$alcunitswk<=14 ~ 1,
  data$alcunitswk<=35 & data$sex=="Female" ~ 2,
  data$alcunitswk<=50 & data$sex=="Male" ~ 2,
  TRUE ~ 3)

data$channel<-case_when(
  data$bevtype<=5 ~ "Off-trade",
  data$bevtype>5 ~ "On-trade"
)

data$region <- case_when(
  data$Gor==1 ~ "North East",
  data$Gor==2 ~ "North West",
  data$Gor==3 ~ "Yorkshire & Humber",
  data$Gor==4 ~ "East Midlands",
  data$Gor==5 ~ "West Midlands",
  data$Gor==6 ~ "Eastern",
  data$Gor==7 ~ "London",
  data$Gor==8 ~ "South East",
  data$Gor==9 ~ "South West"
)

temp<-subset(data, unitprice<=2.5) %>%
  group_by(ageband) %>%
  summarise(adjwgt=sum(wgt))

data<-merge(data, temp, by="ageband")

data$wgt2<-data$wgt/data$adjwgt

ggplot(data, aes(x=unitprice, y=ageband, group=ageband, fill=ageband))+
  geom_density_ridges(data=filter(data, unitprice<=3), aes(height=..density.., weight=wgt2), stat="density", rel_min_weight=0.01)+
  theme_classic()+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7), labels=c("18-24", "25-34", "35-44", "45-54", "55-65", "65-74", "75+"), name="Age")

ggplot(subset(data, unitprice<5), aes(x=unitprice, y=drinkcat, group=drinkcat, fill=..x..))+
  geom_density_ridges_gradient(rel_min_height=0.01, scale=5)+
  scale_fill_viridis()

ggplot(data, aes(x=unitprice, y=as.factor(Gor), group=Gor, fill=..x..))+
  geom_density_ridges_gradient()+
  theme_classic()+
  xlim(0,2.5)+
  geom_segment(aes(x=0.5,xend=0.5,y=1,yend=12))+
  scale_y_discrete(labels=c("North East", "Scotland", "Wales", "South West", "South East", "London", "Eastern",
                            "West Midlands", "East Midlands", "Yorkshire & Humber", "North West", "North East"))

ggplot(subset(data, channel=="Off-trade" & Gor<10), aes(x=unitprice, y=region, group=region, fill=..x..))+
  geom_density_ridges_gradient()+
  theme_classic()+
  geom_segment(aes(x=0.5,xend=0.5,y=0,yend=11), size=0.8, linetype=2)+
  scale_x_continuous(name="Price per unit of off-trade alcohol", breaks=c(0,0.25,0.5,0.75,1), 
                     labels=c("£0", "£0.25", "£0.5", "£0.75", "£1"), limits=c(0,1))+
  scale_y_discrete(name="")+
  scale_fill_viridis(option="inferno", guide=FALSE)


temp <-subset(data, channel="On-trade") %>%
  group_by(ageband) %>%
  summarise(onsum=sum(wgt))

temp2<-subset(data, channel=="Off-trade") %>%
  group_by(ageband) %>%
  summarise(offsum=sum(wgt))

temp3<-merge(temp, temp2, by="ageband")
temp3$onprop=temp3$onsum/(temp3$onsum+temp3$offsum)

temp3_long<-gather(temp3, channel, chanwgt, onsum, offsum)
temp3_long$channel<-case_when(
  temp3_long$channel=="onsum" ~ "On-trade",
  temp3_long$channel=="offsum" ~ "Off-trade")

data<-merge(data, temp3_long, by=c("ageband", "channel"))

data$chanwgt2<-data$wgt/data$chanwgt

ggplot(data, aes(x=unitprice, y=ageband, group=ageband))+
  geom_density_ridges(data=filter(data, channel=="On-trade" & unitprice<=3), aes(height=..density.., weight=chanwgt2), stat="density", rel_min_weight=0.01, fill="Orange", alpha=0.5)+
  geom_density_ridges(data=filter(data, channel=="Off-trade" & unitprice<=3), aes(height=..density.., weight=chanwgt2), stat="density", rel_min_weight=0.01, fill="Skyblue", alpha=0.5)+
  theme_classic()+
  scale_y_continuous(breaks=c(1,2,3,4,5,6,7), labels=c("18-24", "25-34", "35-44", "45-54", "55-65", "65-74", "75+"), name="Age")

############################

data$channel<-ifelse(data$bevtype<6, "Off-trade", "On-trade")

temp <-subset(data, channel="On-trade") %>%
  summarise(onsum=sum(wgt))

temp2<-subset(data, channel=="Off-trade") %>%
  summarise(offsum=sum(wgt))

data$wgt2 <- ifelse(data$channel=="On-trade",temp$onsum, temp2$offsum)
data$wgt2 <- data$wgt*data$wgt2


ggplot(subset(data, unitprice<=3), aes(x=unitprice, y=1,fill=channel))+
  geom_density_ridges(aes(height=..density.., weight=wgt2), stat="density", alpha=0.5)+
  scale_fill_manual(values=c("DodgerBlue", "Orange"), name="")+
  theme_classic()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(),
        text = element_text(size=20), legend.position=c(0.9,0.5))+
  scale_x_continuous(name="Price per unit", labels = c("£0", "£0.50", "£1", "£1.50", "£2", "£2.50","£3"),
                     breaks=c(0,0.5,1,1.5,2,2.5,3))+
  geom_segment(x=0.5,xend=0.5,y=0,yend=8e+13, colour="Black", linetype=2, size=1)
  
ggplot(data, aes(x=unitprice, y=Gor))+
  geom_density_ridges(aes(height=..density..,weight=wgt2), stat="density")+
  theme_classic()

agg_tiff("Outputs/LCFSPricesxIncome.tiff", units="in", width=8,
         height=6, res=800)
ggplot(subset(data, unitprice<1.5 & channel=="Off-trade"), 
       aes(x=unitprice, y=as.factor(income5cat), fill=as.factor(income5cat)))+
  geom_density_ridges(show.legend=FALSE, colour="white")+
  scale_fill_paletteer_d("ggsci::green_material", direction=-1)+
  theme_custom()+
  scale_x_continuous(name="Price paid per unit of alcohol",
                     labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="Income quintile", 
                   labels=c("Highest", "", "", "", "Lowest"))+
  labs(title="Lower income drinkers buy cheaper alcohol",
       subtitle="Distribution of prices paid for off-trade (shop-bought) alcohol in 2010-15 by quintiles of equivalised household income",
       caption="Data from the Living Costs and Food Survey | Plot by @VictimOfMaths")

dev.off()


agg_tiff("Outputs/LCFSPricesxDrinkcat.tiff", units="in", width=8,
          height=6, res=800)
ggplot(subset(data, unitprice<1.5 & channel=="Off-trade"), 
       aes(x=unitprice, y=as.factor(drinkcat), fill=as.factor(drinkcat)))+
  geom_density_ridges(show.legend=FALSE, alpha=0.6, rel_min_height=0.01)+
  scale_fill_manual(values=c("#92d050", "#ffc000", "#c00000"))+
  theme_custom()+
  scale_x_continuous(name="Price paid per unit of alcohol",
                     labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="", 
                   labels=c("Moderate\ndrinkers", "Increasing Risk\ndrinkers", 
                            "Higher Risk\nDrinkers"))+
  labs(title="Heavier drinkers buy cheaper alcohol",
       subtitle="Distribution of prices paid for off-trade (shop-bought) alcohol in the UK in 2010-15 by drinking level",
       caption="`Moderate` = within the current UK drinking guidelines of 14 units/week.\n`Increasing Risk` = Exceeding the guidelines, but drinking no more than 50 units/week for men and 35 for women\n`Higher risk` = Drinking more than 50 units/week for men and 35 for women\nData from the Living Costs and Food Survey | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/LCFSPricesxDrink.tiff", units="in", width=8,
         height=6, res=800)
ggplot(subset(data, unitprice<1.5 & channel=="Off-trade"), 
       aes(x=unitprice, y=as.factor(bevtype), fill=as.factor(bevtype)))+
  geom_density_ridges(show.legend=FALSE, alpha=0.6, rel_min_height=0.01)+
  scale_fill_manual(values=c("#ffc000", "#00b050", "#7030a0", "#00b0f0",
                             "#ff0000"))+
  theme_custom()+
  scale_x_continuous(name="Price paid per unit of alcohol",
                     labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="", 
                   labels=c("Beer", "Cider/Perry", "Wine", "Spirits", 
                            "Alcopops/\nPre-mixed drinks"))+
  labs(title="Cider drinkers pay the cheapest prices",
       subtitle="Distribution of prices paid for off-trade (shop-bought) alcohol in the UK in 2010-15 by drink category",
       caption="Data from the Living Costs and Food Survey | Plot by @VictimOfMaths")

dev.off()

agg_tiff("Outputs/LCFSPricesxAge.tiff", units="in", width=8,
         height=6, res=800)
ggplot(subset(data, unitprice<1.5 & channel=="Off-trade"), 
       aes(x=unitprice, y=as.factor(ageband), fill=as.factor(ageband)))+
  geom_density_ridges(show.legend=FALSE, alpha=0.6, rel_min_height=0.01)+
  #scale_fill_manual(values=c("#ffc000", "#00b050", "#7030a0", "#00b0f0",
  #                           "#ff0000"))+
  theme_custom()+
  scale_x_continuous(name="Price paid per unit of alcohol",
                     labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="", 
                   labels=c("Beer", "Cider/Perry", "Wine", "Spirits", 
                            "Alcopops/\nPre-mixed drinks"))+
  labs(title="Cider drinkers pay the cheapest prices",
       subtitle="Distribution of prices paid for off-trade (shop-bought) alcohol in the UK in 2010-15 by drink category",
       caption="Data from the Living Costs and Food Survey | Plot by @VictimOfMaths")

dev.off()

data2 <- data %>% 
  group_by(region, channel) %>% 
  mutate(meanprice=mean(unitprice)) %>% 
  ungroup()

agg_tiff("Outputs/LCFSPricesxRegion.tiff", units="in", width=8,
         height=6, res=800)
ggplot(subset(data2, channel=="Off-trade" & Gor<10 & unitprice<=1), 
       aes(x=unitprice, y=fct_reorder(region, meanprice), 
           fill=fct_reorder(region, meanprice)))+
  geom_density_ridges(show.legend=FALSE, rel_min_height=0.01, colour="white")+
  theme_custom()+
  scale_x_continuous(name="Price per unit of off-trade alcohol", 
                     labels=label_dollar(prefix="£"))+
  scale_y_discrete(name="")+
  scale_fill_paletteer_d("Redmonder::sPBIRdPu")+
  labs(title="Drinkers in the North of England pay the lowest prices",
       subtitle="Distribution of prices paid for off-trade (shop-bought) alcohol in England in 2010-15 by region",
       caption="Data from the Living Costs and Food Survey | Plot by @VictimOfMaths")

dev.off()