rm(list=ls())

library(tidyverse)
library(paletteer)
library(extrafont)
library(ragg)
library(scales)
library(patchwork)

data1 <- read.csv("Data/IMPAACT1.csv") %>% 
  gather(age, callouts, c(2:6)) %>% 
  mutate(age=case_when(
    age=="X0.24" ~ "0-24",
    age=="X25.39" ~ "25-39",
    age=="X40.54" ~ "40-54",
    age=="X55.69" ~ "55-69",
    TRUE ~ "70+"))


agg_tiff("Outputs/IMPAACTFig1.tiff", units="in", width=9, height=6, res=500)
ggplot(data1, aes(x=hour, y=callouts, colour=age))+
  geom_line()+
  scale_x_continuous(limits=c(0,23), name="Hour of the day", breaks=c(0,6,12,18),
                     labels=c("0:00", "6:00", "12:00", "18:00"))+
  scale_y_continuous(name="Alcohol-related callouts")+
  scale_colour_paletteer_d("LaCroixColoR::PeachPear", name="Age")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"))
dev.off()

data2 <- read.csv("Data/IMPAACT2.csv") %>% 
  gather(cause, callouts, c(3:4)) %>% 
  group_by(cause, Indicator) %>% 
  mutate(prop=callouts/sum(callouts)) %>% 
  ungroup() %>% 
  mutate(Item=factor(Item, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
                                    "Saturday", "January", "February", "March", "April", "May", "June",
                                    "July", "August", "September", "October", "November", "December",
                                    "Green", "Yellow", "Amber", "Red", "Purple", "0-24", "25-39",
                                    "40-54", "55-69", "70+", "Female", "Male", "1 (most deprived)", "2",
                                    "3", "4", "5", "6", "7", "8", "9", "10 (least deprived)", 
                                    "Large urban area", "Other urban area", "Accessible small town",
                                    "Remote small town", "Accessible rural area", "Remote rural area")))

ageplot <- ggplot(data2 %>% filter(Indicator=="Age group"))+
  geom_line(aes(x=Item, y=prop, colour=cause, group=cause), show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("palettetown::porygon", name="Cause")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(face="bold"))+
  labs(title="Age group")

dayplot <- ggplot(data2 %>% filter(Indicator=="Day of the week"))+
  geom_line(aes(x=Item, y=prop, colour=cause, group=cause), show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Proportion of callouts", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("palettetown::porygon", name="Cause")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(face="bold"))+
  labs(title="Day of the week")

monthplot <- ggplot(data2 %>% filter(Indicator=="Month of the year"))+
  geom_line(aes(x=Item, y=prop, colour=cause, group=cause), show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("palettetown::porygon", name="Cause")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(face="bold"), 
        axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="Month")

severityplot <- ggplot(data2 %>% filter(Indicator=="Emergency code"))+
  geom_col(aes(x=Item, y=prop, fill=cause), position="dodge", show.legend=FALSE)+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Proportion of callouts", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(face="bold"))+
  labs(title="Emergency code")

sexplot <- ggplot(data2 %>% filter(Indicator=="Sex"))+
  geom_col(aes(x=Item, y=prop, fill=cause), position="dodge")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(face="bold"),
        legend.title=element_text(face="bold"))+
  labs(title="Sex")

SIMDplot <- ggplot(data2 %>% filter(Indicator=="SIMD-inc"))+
  geom_line(aes(x=Item, y=prop, colour=cause, group=cause), show.legend=FALSE)+
  scale_x_discrete(name="", labels=c("1\n(most\ndeprived)", "2", "3", "4", "5", "6", "7", "8", "9",
                                     "10\n(least\ndeprived)"))+
  scale_y_continuous(name="Proportion of callouts", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_colour_paletteer_d("palettetown::porygon", name="Cause")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(face="bold"))+
  labs(title="Deprivation (individuals)")

urbanplot <- ggplot(data2 %>% filter(Indicator=="Rurality"))+
  geom_col(aes(x=Item, y=prop, fill=cause), position="dodge", show.legend=FALSE)+
  scale_x_discrete(name="", labels=c("Large\nurban\narea", "Other\nurban\narea", 
                                     "Accessible\nsmall\ntown", "Remote\nsmall\ntown", 
                                     "Accessible\nrural\narea", "Remote\nrural\narea"))+
  scale_y_continuous(name="", limits=c(0,NA),
                     labels=label_percent(accuracy=1))+
  scale_fill_paletteer_d("palettetown::porygon", name="Cause")+
  theme_classic()+
  theme(text=element_text(family="Times New Roman"),
        plot.title=element_text(face="bold"))+
  labs(title="Urban/Rural classification")

agg_tiff("Outputs/IMPAACTFig2.tiff", units="in", width=12, height=8, res=500)
(dayplot | monthplot)/ (severityplot | ageplot | sexplot) / (SIMDplot | urbanplot)
dev.off()



