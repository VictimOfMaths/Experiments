rm(list=ls())

library(tidyverse)
library(forcats)
library(extrafont)

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

#Data on Life Expectancy at birth and Quality-Adjusted Life Expectancy at birth from Love-Koh et al.
#https://link.springer.com/article/10.1007/s40273-023-01264-9/tables/3
data <- tibble(IMDq=rep(c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"), times=2),
               Sex=rep(c("Female", "Male"), each=5),
               LE0=c(85.76, 84.62, 83.57, 82.15, 79.67, 82.87, 81.45, 80.17, 78.23, 75.18),
               QALE0=c(73.42, 71.59, 69.55, 65.28, 62.17, 73.02, 70.64, 69.49, 65.52, 62.36)) %>% 
  mutate(diff=LE0-QALE0) %>% 
  gather(Metric, Value, c(3:5))

agg_tiff("Outputs/QALEvsLE.tiff", units="in", width=9, height=6, res=800)
ggplot(data %>% filter(Metric!="LE0"), aes(x=Value, y=fct_rev(IMDq), fill=Metric))+
  geom_col(position="stack")+
  geom_text(aes(label=format(round(Value, digits=1), nsmall = 1), colour=Metric), 
            position=position_stack(vjust=0.5), show.legend=FALSE)+
  facet_grid(Sex~., switch="both")+
  scale_x_continuous(name="Years of life", breaks=c(0,20,40,60,80))+
  scale_y_discrete(name="IMD quintile")+
  scale_fill_manual(values=c("#019875", "#2A363B"), guide=guide_legend(reverse=TRUE),
                    name="", labels=c("Years Lost to Ill Health", "Quality-Adjusted Life Expectancy"))+
  scale_colour_manual(values=c("black", "white"))+
  theme_custom()+
  theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(),
        panel.grid.major.x=element_line(colour="grey90"), legend.position="top")+
  labs(title="More deprived groups live shorter lives in poorer health",
       subtitle="Overall and Quality-Adjusted Life Expectancy at birth in England by sex and quintile of the Index of Multiple Deprivation",
       caption="Data from Love-Koh et al. 2023 | Plot by @VictimOfMaths")

dev.off()