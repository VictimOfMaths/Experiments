rm(list=ls())

library(tidyverse)
library(pdftools)
library(ragg)
library(extrafont)
library(scales)
library(paletteer)
library(forcats)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"))
}

#Download the supplementary material to the paper itself (https://doi.org/10.1177/01410768231206033 
#and amend the code below to refer to wherever it is saved as appropriate)

Table <- as.data.frame(pdf_data("Valabhji et al Appendix.pdf")[5])[-c(1:52, 157:160),] %>% 
  mutate(Conditions=c(1:6, rep(c(NA, 1:6), times=14)),
         Grouping=c(rep("Overall", times=6), rep("Age", times=42),
                    rep("Sex", times=21), rep("Ethnicity", times=35)),
         Value=if_else(is.na(Conditions), text, NA)) %>% 
  fill(Value) %>% 
  filter(!is.na(Conditions)) %>% 
  select(text:Value) %>% 
  spread(Conditions, text) %>% 
  bind_rows(as.data.frame(pdf_data("Valabhji et al Appendix.pdf")[6])[-c(1:26, 108,109),] %>% 
              mutate(text=if_else(text=="(6+)", "Unknown", text)) %>% 
              filter(!text %in% c("(most", "(least", "deprived)")) %>% 
              mutate(Conditions=rep(c(NA,1:6), times=11),
                     Grouping=c(rep("Ethnicity", times=7),
                                rep("Deprivation", times=70)),
                     Value=if_else(is.na(Conditions), text, NA)) %>% 
              fill(Value) %>% 
              filter(!is.na(Conditions)) %>% 
              select(text:Value) %>% 
              spread(Conditions, text))

Proportions <- Table %>% filter(Grouping=="Age") %>% 
  set_names("Grouping", "Value", "1", "2", "3", "4", "5", "6+") %>% 
  gather(Conditions, Proportion, c(3:8)) %>% 
  mutate(Proportion=as.numeric(gsub("%", "", Proportion))/100) %>% 
  group_by(Value) %>% 
  mutate(IndProp=case_when(
    Conditions=="5" ~ Proportion-Proportion[Conditions=="6+"],
    Conditions=="4" ~ Proportion-Proportion[Conditions=="5"],
    Conditions=="3" ~ Proportion-Proportion[Conditions=="4"],
    Conditions=="2" ~ Proportion-Proportion[Conditions=="3"],
    Conditions=="1" ~ Proportion-Proportion[Conditions=="2"],
    TRUE ~ Proportion)) %>% 
  ungroup() 

Proportions <- Proportions %>% 
  bind_rows(Proportions %>% group_by(Value) %>% 
              summarise(IndProp=1-sum(IndProp), .groups="drop") %>% 
              mutate(Conditions="None")) %>% 
  mutate(Conditions=factor(Conditions, levels=c("None", "1", "2", "3", "4", "5", "6+")),
         Conditions=fct_rev(Conditions),
         AgeMin=if_else(Value=="0-19",0, as.numeric(substr(Value, 1, 2))),
         AgeMax=case_when(Value=="0-19" ~ 19, Value=="80+" ~ 90,
                          TRUE ~ as.numeric(substr(Value, 4,5)))) %>% 
  group_by(Value) %>% 
  arrange(Conditions) %>% 
  mutate(ymax=cumsum(IndProp),
         ylag=lag(ymax, 1),
         ylag=if_else(is.na(ylag), 0, ylag)) %>% 
  ungroup()

agg_png("ValabhjiFig1.png", units="in", width=7, height=5, res=800)
ggplot(Proportions, aes(xmin=AgeMin, xmax=AgeMax, ymin=ylag, 
                        ymax=ymax, fill=fct_rev(Conditions)))+
  geom_rect()+
  scale_x_continuous(name="Age")+
  scale_y_continuous(name="Proportion of population", 
                     labels=label_percent(accuracy=1))+
  scale_fill_manual(values=c("#ecf7f9", "#cad0e8", "#b3bede", "#8fa1cf",
                             "#6e87bf", "#436daf", "#00549d"), name="Conditions")+
  theme_custom()+
  labs(title="Underlying conditions are everywhere",
       subtitle="Proportion of the English population with one or more long-term health conditions",
       caption="Data from Valabhji et al. 2023\nPlot by @VictimOfMaths inspired by Barnett et al. 2012")

dev.off()


