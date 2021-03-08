library(curl)
library(readxl)
library(tidyverse)
library(HMDHFDplus)
library(ungroup)
library(MortalitySmooth)
library(paletteer)

#Read in England & Wales data from
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset
ewfile <- tempfile()
ewurl <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/the21stcenturymortalityfilesdeathsdataset/current/21stcenturymortality2019final.xls"
ewfile <- curl_download(url=ewurl, destfile=ewfile, quiet=FALSE, mode="wb")

ewdata.01 <- read_excel(ewfile, sheet="2001", range="A2:E21262")
ewdata.02 <- read_excel(ewfile, sheet="2002", range="A2:E20877")
ewdata.03 <- read_excel(ewfile, sheet="2003", range="A2:E21248")
ewdata.04 <- read_excel(ewfile, sheet="2004", range="A2:E20956")
ewdata.05 <- read_excel(ewfile, sheet="2005", range="A2:E20925")
ewdata.06 <- read_excel(ewfile, sheet="2006", range="A2:E20863")
ewdata.07 <- read_excel(ewfile, sheet="2007", range="A2:E20654")
ewdata.08 <- read_excel(ewfile, sheet="2008", range="A2:E20657")
ewdata.09 <- read_excel(ewfile, sheet="2009", range="A2:E20789")
ewdata.10 <- read_excel(ewfile, sheet="2010", range="A2:E20781")
ewdata.11 <- read_excel(ewfile, sheet="2011", range="A2:E20377")
ewdata.12 <- read_excel(ewfile, sheet="2012", range="A2:E20208")
ewdata.13 <- read_excel(ewfile, sheet="2013", range="A2:E20436")
ewdata.14 <- read_excel(ewfile, sheet="2014", range="A2:E20423")
ewdata.15 <- read_excel(ewfile, sheet="2015", range="A2:E20195")
ewdata.16 <- read_excel(ewfile, sheet="2016", range="A2:E20277")
ewdata.17 <- read_excel(ewfile, sheet="2017", range="A2:E20190")
ewdata.18 <- read_excel(ewfile, sheet="2018", range="A2:E20478")
ewdata.19 <- read_excel(ewfile, sheet="2019", range="A2:E20302")

ewdata <- bind_rows(ewdata.01, ewdata.02, ewdata.03, ewdata.04, ewdata.05, ewdata.06, ewdata.07,
                    ewdata.08, ewdata.09, ewdata.10, ewdata.11, ewdata.12, ewdata.13, ewdata.14, 
                    ewdata.15, ewdata.16, ewdata.17, ewdata.18, ewdata.19) %>% 
  mutate(Year=coalesce(Year, YR),
         Age=coalesce(Age, AGE),
         Sex=coalesce(Sex, SEX),
         ICD10=coalesce(ICD10, `ICD-10`)) %>% 
  select(-c(YR, AGE, SEX, `ICD-10`)) %>% 
  #Allocate causes to code groups
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)), 
         code3=as.numeric(substr(ICD10,4,4)),
         Cause=case_when(
           code1=="K" & code2 %in% c(70, 73, 74) ~ "Alcohol",
           code1=="F" & code2==10 ~ "Alcohol",
           code1=="X" & code2==45 ~ "Alcohol", #Difference from the Masters defns as X45 is clearly alcohol-related
           code1=="X" & code2 %in% c(40:44, 85) ~ "Drugs",           
           code1=="Y" & code2 %in% c(10:15) ~ "Drugs",
           code1=="F" & code2 %in% c(11:16, 18, 19) ~ "Drugs", #Including F18 here to align with Scottish data
           code1=="U" & code2==3 ~ "Suicide",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide",
           code1=="Y" & code2 ==87 ~ "Suicide",
           code1=="C" ~ "Cancer",
           code1=="E" & code2 %in% c(10:14, 65:67) ~ "Metabolic",
           code1=="I" & code2 %in% c(0:13, 20:51) ~ "Metabolic",
           TRUE ~ "Other"),
         Age=case_when(
           Age %in% c("neonates", "neonatal", "Neonates", "<1") ~ "<1",
           TRUE ~ Age)) %>% 
  #Collapse into cause groups
  group_by(Year, Age, Sex, Cause) %>% 
  summarise(Dx=sum(NDTHS)) %>% 
  ungroup()

years <- length(unique(ewdata$Year))
ages <- length(unique(ewdata$Age))
causes <- length(unique(ewdata$Cause))
frame <- data.frame(Year=rep(2001:(2001+years-1), times=1, each=2*ages*causes),
                    Sex=rep(1:2, times=years, each=ages*causes),
                    Age=rep(unique(ewdata$Age), times=2*years, each=causes),
                    Cause=rep(unique(ewdata$Cause), times=2*years*ages, each=1))

#Widen for smoothing
ewdata.wide <- ewdata %>% 
  merge(frame, all.y=TRUE) %>% 
  mutate(Dx=replace_na(Dx, 0)) %>% 
  spread(Year, Dx) %>% 
  filter(Cause=="Drugs" & Sex==1 ) %>% 
  mutate(agestart=case_when(
    Age %in% c("neonatal", "neonates", "Neonates", "<1") ~ 0,
    Age=="01-04" ~ 1, Age=="05-09" ~ 5, Age=="10-14" ~ 10, Age=="15-19" ~ 15,
    Age=="20-24" ~ 20, Age=="25-29" ~ 25, Age=="30-34" ~ 30, Age=="35-39" ~ 35,
    Age=="40-44" ~ 40, Age=="45-49" ~ 45, Age=="50-54" ~ 50, Age=="55-59" ~ 55,
    Age=="60-64" ~ 60, Age=="65-69" ~ 65, Age=="70-74" ~ 70, Age=="75-79" ~ 75,
    Age=="80-84" ~ 80, TRUE ~ 85)) %>% 
  arrange(agestart) %>% 
  select(-c(Sex, Cause, Age, `2019`)) %>% 
  relocate(agestart)

#Download populations/exposures from HMD
username <- "c.r.angus@sheffield.ac.uk" 
password <- "1574553541"

ewpop <- readHMDweb(CNTRY="GBRTENW", "Exposures_1x1", username, password) %>% 
  filter(Year>=2001) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  select(c("Age", "Sex", "Year", "Ex")) %>% 
  spread(Year, Ex) %>% 
  filter(Sex=="Male") %>% 
  select(-Sex)

#Group populations to match deaths age groups
ewpop.grouped <- ewpop %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(agestart) %>%
  summarise(across(`2001`:`2018`, sum)) %>% 
  ungroup()

save(ewdata.wide, ewpop, ewpop.grouped, file="Data/Smoothing.RData")
load("Data/Smoothing.RData")
