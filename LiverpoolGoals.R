rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(stringr)
library(extrafont)

temp <- tempfile()

#21/22
url <- "https://lfcstats.co.uk/20212022goalscorers.html"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.2122 <- as.data.frame(html_table(temp[1]))[-1,-2] %>% 
  set_names(c("Player", "21/22")) %>% 
  mutate(Player=gsub("Position.*", "", Player),
         Player=stringr::str_trim(Player))

#20/21
url <- "https://lfcstats.co.uk/20202021goalscorers.html"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.2021 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "20/21"))

#19/20
url <- "https://lfcstats.co.uk/20192020goalscorers.html"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1920 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "19/20"))

#18/19
url <- "https://lfcstats.co.uk/20182019goalscorers.html"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1819 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "18/19"))

#17/18
url <- "https://lfcstats.co.uk/20172018goalscorers.html"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1718 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "17/18"))

#16/17
url <- "https://lfcstats.co.uk/20162017goalscorers.html"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1617 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "16/17")) %>% 
  mutate(Player=if_else(Player=="Georgino Wijnaldum", "Georginio Wijnaldum", Player))

#15/16
url <- "https://lfcstats.co.uk/20152016oalscorers.html"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
data.1516 <- as.data.frame(html_table(temp[1]))[-1,] %>% 
  set_names(c("Player", "15/16")) 

LFCgoals <- merge(data.2122, data.2021, all=TRUE) %>% 
  merge(data.1920, all=TRUE) %>% 
  merge(data.1819, all=TRUE) %>% 
  merge(data.1718, all=TRUE) %>% 
  merge(data.1617, all=TRUE) %>% 
  merge(data.1516, all=TRUE) %>% 
  gather(Year, Goals, c(2:8)) %>% 
  mutate(Goals=as.numeric(Goals))

ggplot(LFCdata, aes(x=Year, y=Player, size=Goals))+
  geom_point(shape=21)+
  theme_classic()

#Get DOB data from wikipedia
#21/22
url <- "https://en.wikipedia.org/wiki/2021%E2%80%9322_Liverpool_F.C._season"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.2122 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.Of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#20/21
url <- "https://en.wikipedia.org/wiki/2020%E2%80%9321_Liverpool_F.C._season"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.2021 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#19/20
url <- "https://en.wikipedia.org/wiki/2019%E2%80%9320_Liverpool_F.C._season"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1920 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#18/19
url <- "https://en.wikipedia.org/wiki/2018%E2%80%9319_Liverpool_F.C._season"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1819 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#17/18
url <- "https://en.wikipedia.org/wiki/2017%E2%80%9318_Liverpool_F.C._season"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1718 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#16/17
url <- "https://en.wikipedia.org/wiki/2016%E2%80%9317_Liverpool_F.C._season"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1617 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

#1516
url <- "https://en.wikipedia.org/wiki/2015%E2%80%9316_Liverpool_F.C._season"

#Grab html tables of major tournament squads from Wikipedia
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up and stick them together
ages.1516 <- as.data.frame(html_table(temp[3])) %>% 
  mutate(DoB=as.Date(substr(Date.of.Birth, 2,11), format="%Y-%m-%d")) %>% 
  filter(!is.na(DoB)) %>% 
  select(c("Name", "DoB")) %>% 
  set_names(c("Player", "DoB")) %>% 
  mutate(Player=gsub("\\(.*", "", Player),
         Player=stringr::str_trim(Player))

ages <- bind_rows(ages.2122, ages.2021, ages.1920) %>% 
  unique() %>% 
  mutate(`21/22`=interval(DoB, as.Date("2021-09-01")) / years(1),
         `20/21`=interval(DoB, as.Date("2020-09-01")) / years(1),
         `19/20`=interval(DoB, as.Date("2019-09-01")) / years(1),
         `18/19`=interval(DoB, as.Date("2018-09-01")) / years(1),
         `17/18`=interval(DoB, as.Date("2017-09-01")) / years(1),
         `16/17`=interval(DoB, as.Date("2016-09-01")) / years(1),
         `15/16`=interval(DoB, as.Date("2015-09-01")) / years(1)) %>% 
  gather(Year, Age, c(3:9))

LFCdata <- merge(LFCgoals, ages, all.x=TRUE)

meanage <- LFCdata %>% 
  filter(!is.na(Goals)) %>% 
  group_by(Year) %>% 
  summarise(meanage=weighted.mean(Age, Goals, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(flag="yes")

ggplot()+
  geom_line(data=LFCdata %>% filter(!is.na(Goals)), aes(x=Year, y=Age, group=Player))+
  geom_point(data=LFCdata %>% filter(!is.na(Goals)), aes(x=Year, y=Age, size=Goals), shape=21, fill="White", colour="Red")+
  #geom_line(data=meanage, aes(x=Year, y=meanage, group=flag), colour="Red")+
  coord_equal()



