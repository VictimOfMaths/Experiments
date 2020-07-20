rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(lubridate)

#2005-2018 deaths registed from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10929weeklydeathsregistrationsbyimdsexandagegroupenglandandwales2005to2018
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10929weeklydeathsregistrationsbyimdsexandagegroupenglandandwales2005to2018/wklydthsimdsexage2005to2018final.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

#Read in data
deaths2005 <- read_excel(temp, sheet="Table 1", range="A4:BC144")
deaths2006 <- read_excel(temp, sheet="Table 3", range="A4:BC144")
deaths2007 <- read_excel(temp, sheet="Table 5", range="A4:BC144")
deaths2008 <- read_excel(temp, sheet="Table 7", range="A4:BC144")
deaths2009 <- read_excel(temp, sheet="Table 9", range="A4:BD144")
colnames(deaths2009)[56] <- "Week 53"
deaths2010 <- read_excel(temp, sheet="Table 11", range="A4:BC144")
deaths2011 <- read_excel(temp, sheet="Table 13", range="A4:BC144")
deaths2012 <- read_excel(temp, sheet="Table 15", range="A4:BC144")
deaths2013 <- read_excel(temp, sheet="Table 17", range="A4:BC144")
deaths2014 <- read_excel(temp, sheet="Table 19", range="A4:BC144")
deaths2015 <- read_excel(temp, sheet="Table 21", range="A4:BD144")
deaths2016 <- read_excel(temp, sheet="Table 23", range="A4:BC144")
deaths2017 <- read_excel(temp, sheet="Table 25", range="A4:BC144")
deaths2018 <- read_excel(temp, sheet="Table 27", range="A4:BC144")

#Tidy up and merge
deaths2005$year <- 2005
deaths2006$year <- 2006
deaths2007$year <- 2007
deaths2008$year <- 2008
deaths2009$year <- 2009
deaths2010$year <- 2010
deaths2011$year <- 2011
deaths2012$year <- 2012
deaths2013$year <- 2013
deaths2014$year <- 2014
deaths2015$year <- 2015
deaths2016$year <- 2016
deaths2017$year <- 2017
deaths2018$year <- 2018

#Merge and collapse age groups
deaths0518 <- bind_rows(deaths2005, deaths2006, deaths2007, deaths2008, deaths2009, deaths2010,
                        deaths2011, deaths2012, deaths2013, deaths2014, deaths2015, deaths2016,
                        deaths2017, deaths2018) %>% 
  select(year, everything()) %>% 
  group_by(IMD, Sex, year) %>% 
  summarise(across(`Week 1`:`Week 53`, sum)) %>% 
  ungroup() %>% 
  gather(week, deaths, `Week 1`:`Week 53`) %>% 
  filter(!(week=="Week 53" & is.na(deaths)))

deaths0518$week <- as.numeric(substr(deaths0518$week,6,7))

#Bring in populations
#2005-17 data from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/009316populationsbysexsingleyearofageandindexofmultipledeprivationimdengland2001to2017
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/adhocs/009316populationsbysexsingleyearofageandindexofmultipledeprivationimdengland2001to2017/populationbyageimdengland20012017.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
engpop0117 <- read_excel(temp, sheet="Table 1", range="A14:E30953", col_names=FALSE)
colnames(engpop0117) <- c("year", "Sex", "Age", "IMD", "exposure")

#collapse ages
engpop0117 <- engpop0117 %>% 
  group_by(year, Sex, IMD) %>% 
  summarise(exposure=sum(exposure)) %>% 
  ungroup()

engpop0117$Sex <- if_else(engpop0117$Sex==1, "Male", "Female")

#2018 data from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10832deathsandpopulationsbyindexofmultipledeprivationimdenglandandwales2018registrations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/10832deathsandpopulationsbyindexofmultipledeprivationimdenglandandwales2018registrations/deathspopsimdengwal2018.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
engpop18.m <- read_excel(temp, sheet=4, range="B4:CO13", col_names=FALSE)
engpop18.f <- read_excel(temp, sheet=4, range="B15:CO24", col_names=FALSE)
engpop18.m$Sex <- "Male"
engpop18.f$Sex <- "Female"
engpop18 <- bind_rows(engpop18.m, engpop18.f)

engpop18 <- gather(engpop18, Age, exposure, c(2:92))
colnames(engpop18) <- c("IMD", "Sex", "Age", "exposure")

#Collapse ages and merge into 01-17 data
engpop <- engpop18 %>% 
  group_by(Sex, IMD) %>% 
  summarise(exposure=sum(exposure)) %>% 
  ungroup() %>% 
  mutate(year=2018) %>% 
  bind_rows(engpop0117)

engpop$time <- case_when(
  engpop$year==2001 ~ -26-2*52-53,
  engpop$year==2002 ~ -26-52-53,
  engpop$year==2003 ~ -26-53,
  engpop$year==2004 ~ -26,
  engpop$year==2005 ~ 26,
  engpop$year==2006 ~ 26+52,
  engpop$year==2007 ~ 26+52*2,
  engpop$year==2008 ~ 26+52*3,
  engpop$year==2009 ~ 26+52*3+53,
  engpop$year==2010 ~ 26+52*4+53,
  engpop$year==2011 ~ 26+52*5+53,
  engpop$year==2012 ~ 26+52*6+53,
  engpop$year==2013 ~ 26+52*7+53,
  engpop$year==2014 ~ 26+52*8+53,
  engpop$year==2015 ~ 26+52*8+2*53,
  engpop$year==2016 ~ 26+52*9+2*53,
  engpop$year==2017 ~ 26+52*10+2*53,
  engpop$year==2018 ~ 26+52*11+2*53
)

#Interpolate missing weeks between mid-year figures (take to be week 26)
#At the same time extrapolate out to mid-year 2020
exposures.splines <- function(data){
  interpolation <- spline(data$time, data$exposure, xout=seq(from=1, to=26+52*13+2*53, 1))
#Set up week structure
weeks <- c(unlist(lapply(c(52,52,52,52,53,52,52,52,52,52,53,52,52,52,52,26), function(x){1:x})))
years <- c(rep(2005:2020, c(52,52,52,52,53,52,52,52,52,52,53,52,52,52,52,26)))
results <- data.frame(cbind(year=years, week=weeks, exposures=interpolation$y))

return(results)
}

engpop.interpolated <- engpop %>% 
  arrange(year) %>% 
  group_by(IMD, Sex) %>% 
  group_modify(~ exposures.splines(.x)) %>% 
  mutate(index=seq(1:n()), date=as.Date("2005-01-03")+weeks(index-1)) %>% 
  ungroup()
  
#Check visually
ggplot()+
  geom_line(data=engpop.interpolated, aes(x=date, y=exposures))+
  geom_point(data=engpop, aes(x=as.Date(paste0(year, "-06-01")), y=exposure))+
  facet_grid(Sex~IMD)+
  theme_classic()

ggplot()+
  geom_line(data=subset(engpop.interpolated, Sex=="Male" & IMD==10), aes(x=date, y=exposures))+
  geom_point(data=subset(engpop, Sex=="Male" & IMD==10), aes(x=as.Date(paste0(year, "-06-01")), y=exposure))+
  theme_classic()

#Merge interpolated populations into 2005-2018 deaths data
data <- merge(engpop.interpolated, deaths0518, all.x=TRUE)[,-c(6)]

#Bring in 2020 deaths
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fdeathsinvolvingcovid19bylocalareaanddeprivation%2f1march2020to31may2020/referencetablesworkbook1.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
newdata <- read_excel(temp, sheet="Table 3", range="A6:X95", col_names=FALSE)[,c(1,2,3,5,11,17)]
colnames(newdata) <- c("cause", "Sex", "IMD", "March", "April",
                      "May")
newdata$IMD <- rep(c(1:10),9)
newdata <- subset(newdata, Sex!="Persons")

newdata <- newdata %>% 
  gather(month, deaths, c(4:6)) %>% 
  spread(cause, deaths)

newdata$Sex <- if_else(newdata$Sex=="Males", "Male", "Female")
colnames(newdata) <- c("Sex", "IMD", "month", "deaths", "COVID_deaths", "Other_deaths")
newdata$year <- 2020

#Set up for merging
data$month <- case_when(
  data$year==2020 & data$week %in% c(10:13) ~ "March",
  data$year==2020 & data$week %in% c(14:18) ~ "April",
  data$year==2020 & data$week %in% c(19:22) ~ "May"
)

data <- merge(data, newdata, by=c("Sex", "IMD", "year", "month"), all.x=TRUE)

#Adjust 2020 deaths to weekly estimates - assume deaths in 2020 are evenly spread across weeks within month
data$deaths.y <- case_when(
  data$month %in% c("March", "May") ~ data$deaths.y/4,
  data$month=="April" ~ data$deaths.y/5)
data$COVID_deaths <- case_when(
  data$month %in% c("March", "May") ~ data$COVID_deaths/4,
  data$month=="April" ~ data$COVID_deaths/5)
data$Other_deaths <- case_when(
  data$month %in% c("March", "May") ~ data$Other_deaths/4,
  data$month=="April" ~ data$Other_deaths/5)

data$deaths <- coalesce(data$deaths.x, data$deaths.y)

data <- data[,-c(4,8,9)]

write.csv(data, "IMDDataForJM.csv")

#Plot mortality rates
data %>% 
  mutate(mortrate=deaths*100000/exposures) %>% 
  ggplot(aes(x=date, y=mortrate, colour=as.factor(IMD)))+
  geom_line()+
  facet_grid(Sex~.)+
  theme_classic()
