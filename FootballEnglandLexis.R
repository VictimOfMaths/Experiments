rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(ggstream)

url <- "https://en.wikipedia.org/wiki/List_of_England_national_football_team_World_Cup_and_European_Championship_squads"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1950 <- as.data.frame(html_table(temp[1])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1950, No.=NA_integer_,
         start=as.Date("1950-06-24"))

data.1954 <- as.data.frame(html_table(temp[2])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1954, Player=gsub("\\*", "", Player),
         start=as.Date("1954-06-16"))

data.1958 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1958, Player=gsub("\\*", "", Player),
         start=as.Date("1958-06-08"))

data.1962 <- as.data.frame(html_table(temp[4])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1962, Player=gsub("\\*", "", Player),
         start=as.Date("1962-05-30"))

data.1966 <- as.data.frame(html_table(temp[5])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1966, Player=gsub("\\*", "", Player),
         start=as.Date("1966-07-11"))

data.1968 <- as.data.frame(html_table(temp[6])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1968, Player=gsub("\\*", "", Player),
         start=as.Date("1968-06-05"))

data.1970 <- as.data.frame(html_table(temp[7])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1970, Player=gsub("\\*", "", Player),
         start=as.Date("1970-05-31"))

data.1980 <- as.data.frame(html_table(temp[8])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1980, Player=gsub("\\*", "", Player),
         start=as.Date("1980-06-11"))

data.1982 <- as.data.frame(html_table(temp[9])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1982, Player=gsub("\\*", "", Player),
         start=as.Date("1982-06-13"))

data.1986 <- as.data.frame(html_table(temp[10])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1986, Player=gsub("\\*", "", Player),
         start=as.Date("1986-05-31"))

data.1988 <- as.data.frame(html_table(temp[11])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1988, Player=gsub("\\*", "", Player),
         start=as.Date("1988-06-10"))

data.1990 <- as.data.frame(html_table(temp[12])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1990, No.=as.numeric(gsub("\\*", "", No.)),
         Player=gsub("\\*", "", Player), start=as.Date("1990-06-08"))

data.1992 <- as.data.frame(html_table(temp[13])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1992, Player=gsub("\\*", "", Player),
         start=as.Date("1992-06-10"))

data.1996 <- as.data.frame(html_table(temp[14])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1996, Player=gsub("\\*", "", Player),
         start=as.Date("1996-06-08"))

data.1998 <- as.data.frame(html_table(temp[15])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=1998, Player=gsub("\\*", "", Player),
         start=as.Date("1998-06-10"))

data.2000 <- as.data.frame(html_table(temp[16])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2000, Player=gsub("\\*", "", Player),
         start=as.Date("2000-06-10"))

data.2002 <- as.data.frame(html_table(temp[17])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2002, Player=gsub("\\*", "", Player),
         start=as.Date("2002-05-31"))

data.2004 <- as.data.frame(html_table(temp[18])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2004, Player=gsub("\\*", "", Player),
         start=as.Date("2004-06-12"))

data.2006 <- as.data.frame(html_table(temp[19])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2006, Player=gsub("\\*", "", Player),
         start=as.Date("2006-06-09"))

data.2010 <- as.data.frame(html_table(temp[20])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2010, Player=gsub("\\*", "", Player),
         start=as.Date("2010-06-11"))

data.2012 <- as.data.frame(html_table(temp[21])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2012, Player=gsub("\\*", "", Player),
         start=as.Date("2012-06-08"))

data.2014 <- as.data.frame(html_table(temp[22])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2014, Player=gsub("\\*", "", Player),
         start=as.Date("2014-06-12"))

data.2016 <- as.data.frame(html_table(temp[23])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2016, Player=gsub("\\*", "", Player),
         start=as.Date("2016-06-10"))

data.2018 <- as.data.frame(html_table(temp[24])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2018, Player=gsub("\\*", "", Player),
         start=as.Date("2018-06-14"))

data <- bind_rows(data.1950, data.1954, data.1958, data.1962, data.1966, data.1968,
                  data.1970, data.1980, data.1982, data.1986, data.1988, data.1990,
                  data.1992, data.1996, data.1998, data.2000, data.2002, data.2004,
                  data.2006, data.2010, data.2012, data.2014, data.2016, data.2018)

#Create age in years at start of tournament
data$age <- as.numeric(difftime(data$start, data$dob, units="weeks"))/52.25

#Create unique ID (2 Dave Watsons mess things up otherwise)
data$ID <- paste0(data$Player, data$dob)

#Get mean ages of each squad
meanage <- data %>% 
  group_by(year) %>% 
  summarise(meanage=mean(age)) %>% 
  ungroup()

#Calculate number of appearances per player
apps <- data %>% 
  group_by(ID) %>% 
  summarise(appearances=n()) %>% 
  ungroup()

data <- merge(data, apps, by="ID")

#Lexis diagram of squads inspired by https://sites.google.com/site/timriffepersonal/
tiff("Outputs/FootballLexis.tiff", units="in", width=14, height=5.7, res=500)
ggplot()+  
  geom_line(data=data, aes(x=year, y=age, group=ID, alpha=appearances^2), 
            show.legend = FALSE)+
  geom_point(data=data, aes(x=year, y=age, colour=tournament), alpha=0.5)+
  #geom_line(data=meanage, aes(x=year, y=meanage))+
  scale_x_continuous(name="Tournament year", breaks=seq(1950, 2020, by=10))+
  scale_y_continuous(name="Age")+
  scale_colour_manual(values=c("#027DA5", "#FFC200"), name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(2)))+
  annotate("text", x=2009, y=19.5, label="Theo Walcott", size=rel(2.2),
           colour="Grey30", vjust=0, angle=47)+
  annotate("text", x=2008.2, y=22, label="Wayne Rooney", size=rel(2.2),
           colour="Grey30", vjust=0, angle=47)+
  annotate("text", x=1984.5, y=34.3, label="Peter Shilton", size=rel(2.2),
           colour="Grey30", vjust=0, angle=47)+
  annotate("text", x=2001, y=25.3, label="The 'Golden Generation'", size=rel(2.2),
           colour="Grey30", vjust=0, angle=47)+
  annotate("text", x=1975, y=27, label="Emlyn Hughes", size=rel(2.2),
           colour="Grey30", vjust=0, angle=47)+
  annotate("text", x=1952.3, y=36.8, label="Stanley Matthews", size=rel(2.2),
           colour="Grey30", vjust=0, angle=47)+
  annotate("text", x=1963, y=39, label="Each dot represents one player", size=rel(3),
           colour="Grey30", vjust=0)+
  annotate("text", x=1975, y=36, label="Lines represent players picked\nfor multiple tournaments", size=rel(3),
           colour="Grey30", vjust=0)+
  annotate("text", x=1994, y=37, label="Darker lines reflect players picked\nfor more squads over their career", size=rel(3),
           colour="Grey30", vjust=0)+
  labs(title="Who made the squad?",
       subtitle="England squads for every major football tournament",
       caption="Inspired by @timriffe1 | Data from Wikipedia | Plot by @VictimOfMaths")
dev.off()



data %>% 
  mutate(clubgroup=case_when(Club %in% c("Liverpool", "Tottenham Hotspur", "Everton", "Manchester United",
                                         "Manchester City", "Arsenal", "Leeds United", "Rangers",
                                         "Wolverhampton Wanderers", "West Bromwich Albion",
                                         "Aston Villa", "West Ham United", "Newcastle United",
                                         "Nottingham Forest") ~ Club,
                             TRUE ~ "Other")) %>% 
  group_by(year, clubgroup) %>% 
  summarise(count=n()) %>% 
  ungroup() %>% 
ggplot()+
  geom_stream(aes(x=year, y=count, fill=clubgroup), n_grid=10000)

