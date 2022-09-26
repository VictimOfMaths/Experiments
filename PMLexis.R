rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(stringr)
library(extrafont)
library(ragg)

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
         To=if_else(To=="Incumbent", "26 October2022", To)) %>% 
  filter(flag==1 & PM!="Prime MinisterOffice(Lifespan)") %>% 
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
    #Assign everyone a birthday in the middle of the year, because it really doesn't matter much and
    #I don't have time to look them all up, sorry.
    YOB=as.Date(paste0(YOB, "-10-01")),
    YOD=as.Date(paste0(YOD, "-10-01")),
    AgeAtDeath=YOD-YOB, AgeWhenPM=interval(YOB, From) %>% as.numeric("years"),
    AgeWhenNotPM=interval(YOB, To) %>% as.numeric("years"),
    Party=gsub("\\(.*", "", Party), Name=gsub("\\[.*", "", Name),
    Party=factor(Party, levels=c("Whig", "Tory", "Conservative", "Peelite", "Liberal", "Labour")))

dummy <- data.frame(Year=seq.Date(from=as.Date("1720-07-01"), to=max(data$To), by="years"))

agg_tiff("Outputs/PMLexis.tiff", units="in", width=8, height=7, res=500)
ggplot()+
  geom_ribbon(data=dummy %>% filter(Year<as.Date("1980-01-01")), 
              aes(x=Year, ymin=Year, ymax=as.Date("1980-01-01")), fill="Grey80")+
  geom_ribbon(data=dummy, 
              aes(x=Year, ymax=Year-years(100), ymin=max(as.Date("1665-01-01"), min(Year)-years(100))), 
              fill="Grey80")+
  geom_line(data=dummy, aes(x=Year, y=Year-years(20)), colour="Grey90")+
  geom_line(data=dummy, aes(x=Year, y=Year-years(40)), colour="Grey90")+
  geom_line(data=dummy, aes(x=Year, y=Year-years(60)), colour="Grey90")+
  geom_line(data=dummy, aes(x=Year, y=Year-years(80)), colour="Grey90")+
  geom_segment(data=data, aes(x=From, xend=To, y=YOB, yend=YOB, colour=Party), size=2)+
  scale_x_date()+
  scale_y_date(name="Year of birth", limits=c(as.Date("1665-01-01"), as.Date("1980-01-01")))+
  scale_colour_manual(values=c("#FF7F00", "Purple", "#0087DC", "#99FF99", "#ffd700", "#E4003B"))+
  theme_custom()+
  coord_equal()+
  annotate("text", x=as.Date("1984-01-01"), y=as.Date("1970-01-01"), angle=45, label="Age 20",
           colour="Grey60", family="Lato", size=rel(3))+
  annotate("text", x=as.Date("2004-01-01"), y=as.Date("1970-01-01"), angle=45, label="Age 40",
           colour="Grey60", family="Lato", size=rel(3))+
  annotate("text", x=as.Date("2004-01-01"), y=as.Date("1939-01-01"), angle=45, label="Age 60",
           colour="Grey60", family="Lato", size=rel(3))+
  annotate("text", x=as.Date("2004-01-01"), y=as.Date("1919-01-01"), angle=45, label="Age 80",
           colour="Grey60", family="Lato", size=rel(3))+
  labs(title="A timeline of British Prime Ministers",
       subtitle="Serving dates of Prime Ministers, ordered by date of birth of the encumbent",
       caption="Date from Wikipedia\nInspired by Carl Schmertmann @CSchmert\nPlot by @VictimOfMaths")
dev.off()


