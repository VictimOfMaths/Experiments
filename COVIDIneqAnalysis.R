rm(list=ls())

library(tidyverse)
library(paletteer)
library(PHEindicatormethods)
library(ggtext)

load("Data/Output_1_Model_Deaths.RData")

#Calculate excess deaths
Deaths.IMD <- Deaths.IMD %>% 
  mutate(excess.deaths=observed.deaths-expected.deaths,
         excess.rate=excess.deaths*100000/exposures,
         observed.rate=observed.deaths*100000/exposures,
         expected.rate=expected.deaths*100000/exposures)

#Plot excess deaths over time
ggplot(subset(Deaths.IMD, model=="glm.serfling"))+
  geom_line(aes(x=date, y=observed.deaths, colour=as.factor(IMD)))+
  geom_line(aes(x=date, y=expected.deaths, colour=as.factor(IMD)), linetype=2)+
  scale_colour_paletteer_d("dichromat::BluetoOrange_10", name="IMD decile", 
                           labels=c("1 (most deprived)","2","3","4","5","6","7","8","9","10 (least deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Weekly deaths")+
  facet_grid(Sex~IMD)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))

#Compare excess rates
ggplot(Deaths.IMD, aes(x=date, y=excess.rate, colour=as.factor(IMD)))+
  geom_line(size=0.3)+
  geom_segment(aes(x=as.Date("2005-01-01"), xend=as.Date("2020-08-01"), y=0, yend=0),
               colour="Grey30")+
  scale_colour_paletteer_d("dichromat::BluetoOrange_10", name="IMD decile", 
                           labels=c("1 (most deprived)","2","3","4","5","6","7","8","9","10 (least deprived)"))+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Weekly excess deaths per 100,000")+
  facet_grid(Sex~model)+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))

#Calculate SII and RII
Ineq.Measures.obs <- Deaths.IMD %>% 
  filter(!is.na(observed.deaths) & model=="glm.serfling") %>% 
  group_by(Sex, time, date) %>% 
  phe_sii(quantile=IMD, population=exposures, value=observed.deaths, 
          lower_cl=lower.PI, upper_cl=upper.PI , rii=TRUE)

Ineq.Measures.exp <- Deaths.IMD %>% 
  filter(!is.na(observed.deaths) & model=="glm.serfling") %>% 
  group_by(Sex, time, date) %>% 
  phe_sii(quantile=IMD, population=exposures, value=expected.deaths, 
          lower_cl=lower.PI, upper_cl=upper.PI , rii=TRUE)

#Bring them together
Ineq.Measures.obs <- Ineq.Measures.obs[,c(1:9)]
colnames(Ineq.Measures.obs) <- c("Sex", "time", "date", "sii.obs", "rii.obs", "sii.upper.ci.obs", 
                                 "sii.lower.ci.obs", "rii.lower.ci.obs", "rii.upper.ci.obs")
Ineq.Measures.exp <- Ineq.Measures.exp[,c(1:9)]
colnames(Ineq.Measures.exp) <- c("Sex", "time", "date", "sii.exp", "rii.exp", "sii.upper.ci.exp", 
                                 "sii.lower.ci.exp", "rii.lower.ci.exp", "rii.upper.ci.exp")
Ineq.Measures <- merge(Ineq.Measures.obs, Ineq.Measures.exp, all.x=TRUE)

#Negate SII, since this places the deciles in the opposite order by default
Ineq.Measures$sii.obs <- -Ineq.Measures$sii.obs
Ineq.Measures$sii.exp <- -Ineq.Measures$sii.exp
Ineq.Measures$sii.lower.ci.exp <- -Ineq.Measures$sii.lower.ci.exp
Ineq.Measures$sii.lower.ci.obs <- -Ineq.Measures$sii.lower.ci.obs
Ineq.Measures$sii.upper.ci.exp <- -Ineq.Measures$sii.upper.ci.exp
Ineq.Measures$sii.upper.ci.obs <- -Ineq.Measures$sii.upper.ci.obs

#Plot them
#Observed inequality
ggplot(Ineq.Measures)+
  geom_ribbon(data=subset(Ineq.Measures, date<as.Date("2019-01-01")), 
              aes(x=date, ymin=sii.lower.ci.obs, ymax=sii.upper.ci.obs), fill="Grey80")+
  geom_ribbon(data=subset(Ineq.Measures, date>as.Date("2020-03-01")), 
              aes(x=date, ymin=sii.lower.ci.obs, ymax=sii.upper.ci.obs), fill="Grey80")+
  geom_line(data=subset(Ineq.Measures, date<as.Date("2019-01-01")),
            aes(x=date, y=sii.obs), colour="red")+
  geom_line(data=subset(Ineq.Measures, date>as.Date("2020-03-01")),
            aes(x=date, y=sii.obs), colour="red")+
  geom_segment(aes(x=as.Date("2005-01-01"), xend=as.Date("2020-08-01"), y=0, yend=0),
               colour="Grey30")+
  facet_grid(~Sex)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Slope Index of Inequality")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Trends in observed mortality inequalities")

ggplot(Ineq.Measures)+
  geom_ribbon(data=subset(Ineq.Measures, date<as.Date("2019-01-01")), 
              aes(x=date, ymin=rii.lower.ci.obs, ymax=rii.upper.ci.obs), fill="Grey80")+
  geom_ribbon(data=subset(Ineq.Measures, date>as.Date("2020-03-01")), 
              aes(x=date, ymin=rii.lower.ci.obs, ymax=rii.upper.ci.obs), fill="Grey80")+
  geom_line(data=subset(Ineq.Measures, date<as.Date("2019-01-01")),
            aes(x=date, y=rii.obs), colour="red")+
  geom_line(data=subset(Ineq.Measures, date>as.Date("2020-03-01")),
            aes(x=date, y=rii.obs), colour="red")+
  geom_segment(aes(x=as.Date("2005-01-01"), xend=as.Date("2020-08-01"), y=0, yend=0),
               colour="Grey30")+
  facet_grid(~Sex)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Relative Index of Inequality")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Trends in observed mortality inequalities")


#Expected inequality
ggplot(Ineq.Measures)+
  geom_ribbon(aes(x=date, ymin=sii.lower.ci.exp, ymax=sii.upper.ci.exp), fill="SkyBlue2")+
  geom_line(aes(x=date, y=sii.exp), colour="red")+
  geom_segment(aes(x=as.Date("2005-01-01"), xend=as.Date("2020-08-01"), y=0, yend=0),
               colour="Grey30")+
  facet_grid(~Sex)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Slope Index of Inequality")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Trends in expected mortality inequalities")

ggplot(Ineq.Measures)+
  geom_ribbon(aes(x=date, ymin=rii.lower.ci.exp, ymax=rii.upper.ci.exp), fill="SkyBlue2")+
  geom_line(aes(x=date, y=rii.exp), colour="red")+
  geom_segment(aes(x=as.Date("2005-01-01"), xend=as.Date("2020-08-01"), y=0, yend=0),
               colour="Grey30")+
  facet_grid(~Sex)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Relative Index of Inequality")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)))+
  labs(title="Trends in expected mortality inequalities")

#Observed vs. expected
ggplot(Ineq.Measures)+
  geom_line(data=subset(Ineq.Measures, date<as.Date("2019-01-01")), 
                        aes(x=date, y=sii.obs), colour="#40A0D8")+
  geom_line(data=subset(Ineq.Measures, date>as.Date("2020-03-01")), 
            aes(x=date, y=sii.obs), colour="#40A0D8")+
  geom_line(aes(x=date, y=sii.exp), colour="#F89088")+
  facet_grid(~Sex)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Slope Index of Inequality")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_markdown())+
  labs(title="Trends in <span style='color:#40A0D8;'>observed</span> vs. <span style='color:#F89088;'>expected</span> mortality inequalities")

ggplot(Ineq.Measures)+
  geom_line(data=subset(Ineq.Measures, date<as.Date("2019-01-01")), 
            aes(x=date, y=rii.obs), colour="#40A0D8")+
  geom_line(data=subset(Ineq.Measures, date>as.Date("2020-03-01")), 
            aes(x=date, y=rii.obs), colour="#40A0D8")+
  geom_line(aes(x=date, y=rii.exp), colour="#F89088")+
  facet_grid(~Sex)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="Relative Index of Inequality")+
  theme_classic()+
  theme(strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
        plot.title=element_markdown())+
  labs(title="Trends in <span style='color:#40A0D8;'>observed</span> vs. <span style='color:#F89088;'>expected</span> mortality inequalities")


              
