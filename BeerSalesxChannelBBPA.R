rm(list=ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(paletteer)
library(curl)
library(ragg)
library(ggtext)
library(ggrepel)
library(scales)
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

#Data comes from the British Beer & Pub Association statistical handbook, 
#which you will have to buy if you want the underlying data, sorry. 

data <- read_excel("X:/ScHARR/SARG_BBPA/General/2022/Digital Handbook 2022 FINAL.xlsx",
                   sheet="A6", range="A5:C56") %>% 
  set_names("Year", "On-trade", "Off-trade") %>% 
  gather(Channel, Prop, c(2,3)) %>% 
  mutate(Prop=Prop/100) %>% 
  merge(read_excel("X:/ScHARR/SARG_BBPA/General/2022/Digital Handbook 2022 FINAL.xlsx",
                   sheet="A1", range="A4:B56")) %>% 
  mutate(TotalPints=`Thousand hectolitres`*100000/0.568,
         Pints=TotalPints*Prop)

agg_tiff("Outputs/BeerSalesxChannel.tiff", units="in", width=8, height=5.5, res=800)
ggplot(data, aes(x=Year, y=Pints/1000000000, colour=Channel))+
  geom_line(show.legend=FALSE)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Total pints of beer sold (billions)", limits=c(0,NA))+
  scale_colour_manual(values=c("#fc7a1e",
                               "#3185fc"))+
  theme_custom()+
  theme(plot.title=element_markdown(), plot.subtitle=element_markdown())+
  labs(title="Beer drinking has shifted from <span style='color:#3185fc;'>the pub</span> to <span style='color:#fc7a1e;'>homes",
       subtitle="Total beer sales in the UK in <span style='color:#3185fc;'>pubs, bars, restaurants and nightclubs</span> compared to <span style='color:#fc7a1e;'>shops and supermarkets",
       caption="Data from BBPA | Plot by @VictimOfMaths")

dev.off()


