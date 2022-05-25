rm(list=ls())

library(tidyverse)
library(xml2)
library(rvest)
library(lubridate)
library(stringr)
library(extrafont)
library(ragg)
library(ggstream)
library(forcats)
library(ggtext)

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

#EPL
url <- "https://en.wikipedia.org/wiki/List_of_English_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
EPL <- as.data.frame(html_table(temp[4]))[,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Winners=gsub("\\[.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Premier League")

#Ligue 1
url <- "https://en.wikipedia.org/wiki/List_of_French_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
Ligue1 <- as.data.frame(html_table(temp[3]))[,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Ligue 1") %>% 
  slice(-c(1:56))

#La Liga
url <- "https://en.wikipedia.org/wiki/List_of_Spanish_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
LaLiga <- as.data.frame(html_table(temp[3]))[,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="La Liga") %>% 
  slice(-c(1:64))

#BundesLiga
url <- "https://en.wikipedia.org/wiki/List_of_German_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
Bundesliga <- as.data.frame(html_table(temp[8]))[,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Bundesliga") %>% 
  slice(-c(1:29))

#Serie A
url <- "https://en.wikipedia.org/wiki/List_of_Italian_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
SerieA <- as.data.frame(html_table(temp[7]))[,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Serie A") %>% 
  slice(-c(1:63))

#Primeira Liga
url <- "https://en.wikipedia.org/wiki/List_of_Portuguese_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
PrimeiraLiga <- as.data.frame(html_table(temp[2]))[,c(2,3)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Primeira Liga") %>% 
  slice(-c(1:61)) %>% 
  filter(Season!="Primeir")

#J-League
url <- "https://en.wikipedia.org/wiki/List_of_Japanese_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
JLeague <- as.data.frame(html_table(temp[4]))[-1,c(1,2)] %>% 
  bind_rows(as.data.frame(html_table(temp[5]))[c(1,2)]) %>% 
  bind_rows(as.data.frame(html_table(temp[6]))[c(1,2)]) %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="J-League") 

data <- bind_rows(EPL, Ligue1, LaLiga, SerieA, Bundesliga, PrimeiraLiga, JLeague) %>% 
  group_by(league, Winners) %>% 
  summarise(wins=n()) %>% 
  ungroup()

#Eredivisie
url <- "https://en.wikipedia.org/wiki/List_of_Dutch_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
Eredivisie <- as.data.frame(html_table(temp[3]))[,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Eredivisie") %>% 
  slice(-c(1:36)) %>% 
  filter(Season!="2019–20")

#Scotland
url <- "https://en.wikipedia.org/wiki/List_of_Scottish_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
Scotland <- as.data.frame(html_table(temp[7]))[,c(1,2)] %>% 
  bind_rows(as.data.frame(html_table(temp[8]))[,c(1,2)]) %>% 
  bind_rows(as.data.frame(html_table(temp[9]))[,c(1,2)]) %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Scottish Premiership") %>% 
  filter(Season!="Season") %>% 
  slice(-c(1:17))  

#MLS
url <- "https://en.wikipedia.org/wiki/List_of_American_and_Canadian_soccer_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
MLS <- as.data.frame(html_table(temp[4]))[,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="MLS") %>% 
  filter(Season!="Season") 

#Brazil
url <- "https://en.wikipedia.org/wiki/List_of_Brazilian_football_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
Brazil <- as.data.frame(html_table(temp[9]))[,c(1,2)] %>% 
  bind_rows(as.data.frame(html_table(temp[10]))[,c(1,2)]) %>% 
  bind_rows(as.data.frame(html_table(temp[11]))[,c(1,2)]) %>% 
  set_names("Season", "Winners") %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="Campeonato Brasileiro") %>% 
  filter(Season!="Season") %>% 
  slice(-c(1:3))  

#Australia
url <- "https://en.wikipedia.org/wiki/List_of_Australian_soccer_champions"

#Grab html tables 
temp <- url %>% read_html %>% html_nodes("table")

#Tidy them up 
ALeague <- as.data.frame(html_table(temp[5]))[-1,c(1,2)] %>% 
  set_names("Season", "Winners") %>% 
  bind_rows(as.data.frame(html_table(temp[6]))[,c(1,2)] %>% 
  set_names("Season", "Winners")) %>% 
  mutate(Winners=gsub(" \\(.*", "", Winners),
         Season=substr(Season, 1, 7),
         league="A-League") %>% 
  filter(Season!="Season") %>% 
  slice(-c(1:9))  

data <- bind_rows(EPL, Ligue1, LaLiga, SerieA, Bundesliga, PrimeiraLiga, JLeague, Eredivisie,
                  Scotland, MLS, Brazil, ALeague) %>% 
  group_by(league, Winners) %>% 
  summarise(wins=n()) %>% 
  ungroup()

agg_png("Outputs/LeagueComparisons.png", units="in", width=14, height=8, res=500)
ggplot(data, aes(x=wins, y=fct_reorder(Winners, wins), fill=wins))+
  geom_col(show.legend=FALSE)+
  scale_x_continuous(name="League titles")+
  scale_y_discrete(name="")+
  scale_fill_paletteer_c("viridis::rocket", direction=-1)+
  facet_wrap(~league, scales="free_y")+
  theme_custom()+
  theme(plot.title=element_text(face="bold", size=rel(2.5), hjust=0,
        margin=margin(0,0,5.5,0)))+
  labs(title="European football shares league trophies around less",
       subtitle="League winners in selected major football leagues since 1992-3",
       caption="Data from Wikipedia | Plot by @VictimOfMaths")

dev.off()

data2 <- bind_rows(EPL, Ligue1, LaLiga, SerieA, Bundesliga, PrimeiraLiga, JLeague, Eredivisie,
                   Scotland, MLS, Brazil, ALeague) %>% 
  mutate(year=as.numeric(substr(Season, 1, 4))) %>% 
  group_by(league) %>% 
  mutate(change=if_else(Winners==lag(Winners, 1), "No change", "Change")) %>% 
  ungroup()

data3 <- data2 %>% 
  group_by(league, change) %>% 
  summarise(changes=n()) %>% 
  ungroup()

agg_png("Outputs/LeagueChanges.png", units="in", width=9, height=6, res=800)
ggplot(data2 %>% filter(year>1992), aes(x=year, y=fct_rev(league), fill=change))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#EBC915", "#018AC4"), na.value="White", name="Title holders",
                    labels=c("Change", "No change", ""))+
  theme_custom()+
  theme(plot.subtitle=element_markdown())+
  labs(title="Germany, France, Scotland and Italy have the most dynastic football leagues",
       subtitle="Years in which the football league title <span style='color:#EBC915;'>changed hands <span style='color:#018AC4;'>or not<span style='color:Grey40;'> compared to the preceding season",
       caption="Data from Wikipedia | Plot by @VictimOfMaths")

dev.off()


agg_png("Outputs/LeagueChanges2.png", units="in", width=9, height=6, res=800)
ggplot(data3, aes(x=changes, y=league, fill=change))+
  geom_col()+
  scale_x_continuous(name="")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("#EBC915", "#018AC4"), na.value="White", name="Title holders",
                    labels=c("Change", "No change", ""))+
  theme_custom()+
  labs(title="USA! USA! USA!",
       subtitle="Number of years when the football league title has changed hands since 1992/3",
       caption="Data from Wikipedia | Plot by @VictimOfMaths")

dev.off()
