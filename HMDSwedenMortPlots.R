#Code/inspiration borrowed from Saloni: https://github.com/owid/notebooks/blob/main/SaloniDattani/Life-expectancy/Mortality-rates-over-time-countries/inputs/mortality-rates-over-time-countries.R

rm(list=ls())

library(tidyverse)
library(HMDHFDplus)
library(paletteer)
library(ragg)
library(extrafont)
library(scales)
library(ggtext)
library(keyring)
library(RcppRoll)
library(ragg)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family=font),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}


options(scipen=10000)

rawdata <- readHMDweb(CNTRY="SWE", "cMx_1x10", key_list("mortality.org")[1,2], 
                    key_get("mortality.org", key_list("mortality.org")[1,2])) %>% 
  filter(Year>=1800 & Age<=95) %>% 
  mutate(Ratio=Male/Female, Cohortlabel=paste0(Year, "s")) 

data <- rawdata %>% 
  gather(Sex, mx, c("Male", "Female", "Total"))

ggplot(data %>% filter(Sex=="Total"), aes(x=Age, y=mx, colour=Cohortlabel))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 100, by=10))+
  scale_y_continuous(trans="log2", name="Mortality rate\n(log scale)",
                     breaks = c(0.0001, 0.001, 0.01, 0.1, 1),
                     labels=c("0.01%", "0.1%", "1%", "10%", "100%"))+
  scale_colour_viridis_d(option="turbo", name="Birth cohort")+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="grey90"),
        axis.line=element_blank(), axis.ticks = element_blank())

agg_png("Outputs/MortRateEvolutionSwedenxSex.png", units="in", width=11, height=6, res=800)
ggplot(data %>% filter(Sex!="Total"), aes(x=Age, y=mx, colour=Cohortlabel))+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 100, by=10))+
  scale_y_continuous(trans="log2", name="Mortality rate\n(log scale)",
                     breaks = c(0.0001, 0.001, 0.01, 0.1, 1),
                     labels=c("0.01%", "0.1%", "1%", "10%", "100%"))+
  scale_colour_viridis_d(option="turbo", name="Birth cohort")+
  facet_wrap(~Sex)+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="grey90"),
        axis.line=element_blank(), axis.ticks = element_blank())+
  labs(title="Death rates have declined across all ages",
       subtitle="Annual mortality rates in Sweden by Birth Cohort 1800s-1980s",
       caption="Data from mortality.org\nAdapted from Saloni (https://bsky.app/profile/scientificdiscovery.dev)\nPlot by @VictimOfMaths")
dev.off()

agg_png("Outputs/MortRateSexRatioSweden.png", units="in", width=8, height=6, res=800)
rawdata %>% group_by(Year) %>% 
  mutate(RatioRoll=roll_mean(Ratio, align="center", n=5, fill=NA)) %>% 
ggplot(aes(x=Age, y=RatioRoll, colour=Cohortlabel))+
  geom_hline(yintercept=1, colour="grey30")+
  geom_line()+
  scale_x_continuous(breaks = seq(0, 100, by=10))+
  scale_y_continuous(trans="log2", name="Ratio of Male to Female death rates\n(log scale)")+
  scale_colour_viridis_d(option="turbo", name="Birth cohort")+
  theme_custom()+
  theme(panel.grid.major.y = element_line(colour="grey90"),
        axis.line=element_blank(), axis.ticks = element_blank())+
  labs(title="The changing age profile of male excess mortality",
       subtitle="Sex ratio of annual mortality rates in Sweden by Birth Cohort 1800s-1980s",
       caption="Data from mortality.org\nInspired by Saloni & Michel Nivard\nPlot by @VictimOfMaths")
dev.off()
