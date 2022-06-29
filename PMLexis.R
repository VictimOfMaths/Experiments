rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(stringr)
library(extrafont)

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

temp <- tempfile()

#21/22
url <- "https://en.wikipedia.org/wiki/List_of_prime_ministers_of_the_United_Kingdom"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data <- as.data.frame(html_table(temp[2]))[-c(1), -c(1:2)] %>% 
  set_names("PM", "From", "To", "Misc", "Title", "Party", "Govt", "Monarch", "Ref") %>% 
  select("PM", "From", "To", "Party") %>% 
  filter(substr(From, 1, 8)!="See also" & substr(PM, 1, 5)!="Title") %>% 
  mutate(flag=if_else(PM==lag(PM, 1), 0, 1),
         flag=if_else(is.na(flag), 1, flag),
         To=if_else(To=="Incumbent", "28 June2022", To)) %>% 
  filter(flag==1) %>% 
  mutate(From=as.Date(From, "%d %B%Y"), To=as.Date(To, "%d %B%Y")) %>% 
  separate(PM, into=c("Name", "Const"), sep="MP") %>%
  separate(Name, into=c("Name", "Dates"), sep="\\(") %>% 
  separate(Const, into=c("Const", "Dates2"), sep="\\(") %>% 
  mutate(Dates=if_else(is.na(Dates), Dates2, Dates)) %>% 
  select(Name, Dates, From, To, Party) %>% 
  mutate(YOB=case_when(
    substr(Dates,1,4)=="born" ~ as.numeric(substr(Dates, 6, 9)),
    TRUE ~ as.numeric(substr(Dates, 1, 4))),
    YOD=case_when(
      substr(Dates, 1, 4)=="born" ~ 2022, TRUE ~ as.numeric(substr(Dates, 6,9))),
    YOB=as.Date(paste0(YOB, "-01-01")),
    YOD=as.Date(paste0(YOD, "-01-01")))

ggplot(data)+
  geom_segment(aes(x=From, xend=To, y=YOB, yend=YOB))+
  
  theme_custom()+
  coord_equal()

