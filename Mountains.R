rm(list=ls())

#Only need to run the line below once
devtools::install_github("hrbrmstr/waffle")

library(waffle)
library(tidyverse)
library(lubridate)
library(stringr)
library(paletteer)
library(ragg)

data <- read.csv("Data/mountainsclimbed.csv") %>% 
  mutate(date=as.Date(climbed, format="%d/%m/%Y"),
         year=year(date), month=month(date),
         class=str_replace(class, "=", "")) %>% 
  separate(class, sep=",", into=c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")) %>% 
  mutate(class2=case_when(
    a=="M" | b=="M" | c=="M" | d=="M" | e=="M" | f=="M" | g=="M" | h=="M" | i=="M" | j=="M" | k=="M" | l=="M" ~ "Munro",
    a=="C" | b=="C" | c=="C" | d=="C" | e=="C" | f=="C" | g=="C" | h=="C" | i=="C" | j=="C" | k=="C" | l=="C" ~ "Corbett",
    a=="G" | b=="G" | c=="G" | d=="G" | e=="G" | f=="G" | g=="G" | h=="G" | i=="G" | j=="G" | k=="G" | l=="G" ~ "Graham",
    a=="Hew" | b=="Hew" | c=="Hew" | d=="Hew" | e=="Hew" | f=="Hew" | g=="Hew" | h=="Hew" | i=="Hew" | j=="Hew" | k=="Hew" | l=="Hew" ~ "Hewitt",
    a=="N" | b=="N" | c=="N" | d=="N" | e=="N" | f=="N" | g=="N" | h=="N" | i=="N" | j=="N" | k=="N" | l=="N" ~ "Other Nuttall",
    a=="Ma" | b=="Ma" | c=="Ma" | d=="Ma" | e=="Ma" | f=="Ma" | g=="Ma" | h=="Ma" | i=="Ma" | j=="Ma" | k=="Ma" | l=="Ma" ~ "Other Marilyn",
    a=="Hu" | b=="Hu" | c=="Hu" | d=="Hu" | e=="Hu" | f=="Hu" | g=="Hu" | h=="Hu" | i=="Hu" | j=="Hu" | k=="Hu" | l=="Hu" ~ "Other HuMP",
    TRUE ~ "Other Hill")) %>% 
  select(hillnumber, hillname, metres, feet, drop, date, year, month, class2) %>% 
  mutate(class2=factor(class2, levels=c("Munro", "Corbett", "Graham", "Hewitt", "Other Nuttall", 
                                        "Other Marilyn", "Other HuMP", "Other Hill"))) %>% 
  mutate(class2=factor(class2, levels=c("Other Hill", "Other HuMP", "Other Marilyn", "Other Nuttall", "Hewitt", 
                                        "Graham", "Corbett", "Munro"))) %>% 
  
  arrange(hillnumber, date) %>% 
  group_by(hillnumber) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(year, class2) %>% 
  tally()


agg_tiff("Outputs/Mountains.tiff", units="in", width=12, height=4, res=500)
ggplot(data, aes(fill = class2, values = n)) +
  geom_waffle(colour="White", size = .25, n_rows=5, flip = TRUE) +
  facet_wrap(~as.factor(year), nrow=1, strip.position = "bottom")+
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 5, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_paletteer_d("colorblindr::OkabeIto", name="Classification")+
  coord_equal() +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), axis.ticks.y = element_line(),
        plot.title=element_text(face="bold", size=rel(1.2))) +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(
    title = "Mountains on my mind",
    subtitle = "Every British hill or mountain I've ever climbed, by classification",
    x = "Year",
    y = "Mountains climbed"
  )
dev.off()

data2 <- read.csv("Data/mountainsclimbed.csv") %>% 
  mutate(date=as.Date(climbed, format="%d/%m/%Y"),
         year=year(date), month=month(date),
         group=case_when(
           feet>=3000 ~ "Over 3,000ft",
           feet>=2000 ~ "Over 2,000ft",
           TRUE ~ "Below 2,000ft"
         )) %>% 
  arrange(hillnumber, date) %>% 
  group_by(year, group) %>% 
  tally()

library(showtext)
font_add_google("Playfair Display")
showtext_auto()

agg_tiff("Outputs/Mountains2.tiff", units="in", width=12, height=3.2, res=500)
ggplot(data2, aes(fill = group, values = n)) +
  geom_waffle(colour="LightBlue1", size = .25, n_rows=5, flip = TRUE) +
  facet_wrap(~as.factor(year), nrow=1, strip.position = "bottom")+
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 5, # make this multiplyer the same as n_rows
                     expand = c(0,0)) +
  scale_fill_manual(values=c("forestgreen", "tan4", "white"), name="Mountain height")+
  coord_equal() +
  theme_minimal() +
  theme(text=element_text(family="Playfair Display"),
        panel.grid = element_blank(), axis.ticks.y = element_line(),
        plot.title=element_text(face="bold", size=rel(13)),
        plot.subtitle=element_text(size=rel(6)),
        axis.text=element_text(size=rel(3)),
        axis.title=element_text(size=rel(4)),
        strip.text =element_text(size=rel(3)),
        legend.text = element_text(size=rel(4)),
        legend.title=element_text(size=rel(5), vjust=-8),
        plot.background = element_rect(fill="LightBlue1", colour="LightBlue1"),
        panel.background=element_rect(fill="LightBlue1", colour="LightBlue1")) +
  guides(fill = guide_legend(reverse = TRUE))+
  labs(
    title = "A lifetime of ups and downs",
    subtitle = "All of the hills or mountains in the UK that I've ever climbed",
    x = "Year",
    y = "Mountains climbed"
  )
dev.off()

