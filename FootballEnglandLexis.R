rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(stringr)
library(extrafont)

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

data.2021 <- as.data.frame(html_table(temp[25])) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2021, Player=gsub("\\*", "", Player),
         start=as.Date("2021-06-11"))

data.2022 <- as.data.frame(html_table(temp[26])) %>% 
  mutate(tournament="World Cup", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2022, Player=gsub("\\*", "", Player),
         start=as.Date("2022-12-01"))

data.2024 <- as.data.frame(html_table(temp[27])) %>% 
  filter(!is.na(Club)) %>% 
  mutate(tournament="Euros", dob=as.Date(substr(Date.of.birth..age., 2, 11)),
         position=substr(Pos., 2, 3), year=2024, Player=gsub("\\*", "", Player),
         start=as.Date("2024-06-21"),
         `No.`=as.numeric(`No.`))

data <- bind_rows(data.1950, data.1954, data.1958, data.1962, data.1966, data.1968,
                  data.1970, data.1980, data.1982, data.1986, data.1988, data.1990,
                  data.1992, data.1996, data.1998, data.2000, data.2002, data.2004,
                  data.2006, data.2010, data.2012, data.2014, data.2016, data.2018,
                  data.2021, data.2022, data.2024)

#Create age in years at start of tournament
data$age <- as.numeric(difftime(data$start, data$dob, units="weeks"))/52.25

#Remove captain designations
data$Player <- gsub(" \\(c\\)", "", data$Player)
data$Player <- gsub(" \\(captain\\)", "", data$Player)
data$Player <- gsub(" \\(vice-captain\\)", "", data$Player)

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
png("Outputs/FootballLexis.png", units="in", width=15.5, height=5.7, res=800)
ggplot()+  
  geom_line(data=data, aes(x=start, y=age, group=ID, alpha=appearances^2), 
            show.legend = FALSE)+
  geom_point(data=data, aes(x=start, y=age, colour=tournament), alpha=0.5)+
  scale_x_continuous(name="Tournament year", breaks=as.Date(c("1950-07-01", "1960-07-01", "1970-07-01",
                                                              "1980-07-01", "1990-07-01", "2000-07-01",
                                                              "2010-07-01", "2020-07-01")),
                     labels=c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020"))+
  scale_y_continuous(name="Player age")+
  scale_colour_manual(values=c("#027DA5", "#FFC200"), name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Lato"))+
  annotate("text", x=as.Date("2019-10-01"), y=21.3, label="Marcus Rashford", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("2018-10-01"), y=23.3, label="Raheem Stirling", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("2023-03-01"), y=19.1, label="Jude Bellingham", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("2020-04-01"), y=30.2, label="Jordan Henderson", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("2009-05-01"), y=19.5, label="Theo Walcott", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("2008-07-01"), y=22, label="Wayne Rooney", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("1994-05-01"), y=23.1, label="Alan Shearer", size=rel(2.2),
          colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("1998-11-01"), y=27.4, label="Gareth Southgate", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("2006-02-01"), y=34.8, label="David James", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("1984-10-01"), y=34.3, label="Peter Shilton", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("2001-04-01"), y=25.3, label="Man Utd's 'Golden Generation'", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("1975-04-01"), y=27, label="Emlyn Hughes", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("1952-07-01"), y=36.8, label="Stanley Matthews", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("1960-06-01"), y=22, label="Bobby Charlton", size=rel(2.2),
           colour="Grey30", vjust=0, angle=45, family="Lato")+
  annotate("text", x=as.Date("1962-02-01"), y=38.5, label="Each dot represents one player", size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  annotate("text", x=as.Date("1975-02-01"), y=36, label="Lines represent players picked\nfor multiple tournaments", size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  annotate("text", x=as.Date("1994-02-01"), y=37, label="Darker lines reflect players picked\nfor more squads over their career", size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  geom_curve(aes(x=as.Date("1959-08-01"), y=37.6, xend=as.Date("1954-09-01"), yend=35.65), curvature=-0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=as.Date("1975-02-01"), y=35.5, xend=as.Date("1977-04-01"), yend=30.4), curvature=0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=as.Date("1993-02-01"), y=38.6, xend=as.Date("1989-07-01"), yend=39.5), curvature=0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  labs(title="Who made the squad?",
       subtitle="Age breakdown of the England men's squad for every major football tournament",
       caption="Inspired by @timriffe1 | Data from Wikipedia | Plot by @VictimOfMaths")

dev.off()


png("Outputs/FootballLexisMeanAge.png", units="in", width=14, height=5.7, res=800)
ggplot()+
  geom_point(data=data, aes(x=year, y=age, colour=tournament), alpha=0.5)+
  geom_line(data=meanage, aes(x=year, y=meanage), colour="Grey50")+
  scale_x_continuous(name="Tournament year", breaks=seq(1950, 2020, by=10))+
  scale_y_continuous(name="Player age")+
  scale_colour_manual(values=c("#027DA5", "#FFC200"), name="")+
  theme_classic()+
  theme(plot.title=element_text(face="bold", size=rel(2)),
        text=element_text(family="Lato"))+
  annotate("text", x=1962, y=38, label="Each dot represents one player", size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  annotate("text", x=1975, y=30, label="The average age\nof the squad", size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  annotate("text", x=1953, y=23, label="The 1958 World Cup squad\nwas the youngest ever", 
           size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  annotate("text", x=2010, y=33.5, label="The 2010 World Cup squad\nwas the oldest", 
           size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  annotate("text", x=2028, y=24, label="Mean age of the\ncurrent squad is\n26.5 years", 
           size=rel(3),
           colour="Black", vjust=0, family="Lato")+
  geom_curve(aes(x=1959.5, y=37.6, xend=1954.4, yend=35.6), curvature=-0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  geom_curve(aes(x=1975, y=29.7, xend=1974, yend=28), curvature=-0.15, 
             arrow=arrow(length=unit(0.1, "cm"), type="closed"))+
  coord_equal()+
  labs(title="England's Euro 2020 squad is one of the youngest ever",
       subtitle="Age breakdown of the England men's squad for every major football tournament",
       caption="Inspired by @timriffe1 | Data from Wikipedia | Plot by @VictimOfMaths")

dev.off()
