rm(list=ls())

library(tidyverse)
library(paletteer)
library(extrafont)
library(scales)
library(geomtextpath)
library(ragg)
library(curl)
library(readxl)
library(keyring)
library(HMDHFDplus)
library(patchwork)
library(ggtext)

#Set common font for all plots
font <- "Lato"

theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          strip.clip="off",
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"),
          plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
          plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
          axis.text=element_text(colour="Grey40"),
          axis.title=element_text(colour="Grey20"),
          legend.text=element_text(colour="Grey40"),
          legend.title=element_text(colour="Grey20"),
          axis.line.x=element_blank(),
          panel.grid.major.y=element_line(colour="grey95"))
}

options(scipen=9999999)

#Start with ONS 20th Century mortality data
#2001-2022
#Read in data from ONS website
#https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1355deathsbysexsingleyearofageunderlyingcauseicd10codeanddeprivationdecileengland2001to2022
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/1355deathsbysexsingleyearofageunderlyingcauseicd10codeanddeprivationdecileengland2001to2022/deathsbyimd20012022final.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

rawpersons <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                         sheet="1", range=cell_limits(c(6,1), c(567283, 25))) %>% 
  mutate(Sex="Total")

rawmale <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                      sheet="2", range=cell_limits(c(6,1), c(415178, 25))) %>% 
  mutate(Sex="Male")

rawfemale <- read_excel(file.path(temp2, "Deaths by IMD 2001-2022 FINAL.xlsx"),
                        sheet="3", range=cell_limits(c(6,1), c(379521, 25))) %>% 
  mutate(Sex="Female")

rawdata <- bind_rows(rawpersons, rawmale, rawfemale) %>% 
  gather(Year, Deaths, c(4:25)) %>% 
  mutate(Year=as.numeric(Year))

#Collapse to 5-year age bands and separate out causes
working1 <- rawdata %>% 
  mutate(Cause=case_when(
    substr(`ICD-10 code`,1,1)== "K" & 
      as.numeric(substr(`ICD-10 code`,2,3)) %in% c(70:76) ~ "Liver disease",
    substr(`ICD-10 code`,1,1)== "C" | 
      (substr(`ICD-10 code`,1,1)== "D" & 
         as.numeric(substr(`ICD-10 code`,2,3)) %in% c(0:48)) ~ "Cancers",
    substr(`ICD-10 code`,1,1)=="E" & 
      as.numeric(substr(`ICD-10 code`,2,3)) %in% c(10:14) ~ "Diabetes",
    substr(`ICD-10 code`,1,1)=="E" & 
      as.numeric(substr(`ICD-10 code`,2,3)) %in% c(0:7, 15:88) ~ "Endocrine or metabolic",
    substr(`ICD-10 code`,1,1)=="I" & 
      as.numeric(substr(`ICD-10 code`,2,3)) %in% c(10:13) ~ "Circulatory",
    substr(`ICD-10 code`,1,1)=="I" & 
      as.numeric(substr(`ICD-10 code`,2,3)) %in% c(20:25) ~ "Ischaemic heart",
    substr(`ICD-10 code`,1,1)=="I" & 
      as.numeric(substr(`ICD-10 code`,2,3)) %in% c(60:69) ~ "Cerebrovascular",
    substr(`ICD-10 code`,1,3)=="G45" ~ "Cerebrovascular",
    substr(`ICD-10 code`,1,1)=="J" & 
      as.numeric(substr(`ICD-10 code`,2,3)) %in% c(0:22, 30:98) ~ "Respiratory"),
    Age5=case_when(
      Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
      Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
      Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
      Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  filter(!is.na(Cause)) %>% 
  group_by(Year, Age5, Sex, Cause) %>% 
  summarise(Deaths=sum(Deaths), .groups="drop") %>% 
  spread(Age5, Deaths) %>% 
  mutate(across(.cols=c(4:23), ~ if_else(is.na(.x), 0, .x))) %>% 
  gather(Age5, Deaths, c(4:23))

#Bring in population data
ewpop <- readHMDweb(CNTRY="GBRTENW", "Population", key_list("mortality.org")[1,2], 
                    key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=TRUE) %>% 
  mutate(Age=as.numeric(Age), Age=if_else(is.na(Age), 110, Age)) 

ewpop <- bind_rows(ewpop %>% filter(Year==2021) %>% 
                     select("Year", "Age", "Male2", "Female2") %>% 
                     mutate(Year=2022) %>% 
                     set_names(c("Year", "Age", "Male", "Female")),
                   ewpop %>% select(c("Year", "Age", "Male1", "Female1")) %>% 
                     set_names(c("Year", "Age", "Male", "Female"))) %>% 
  gather(Sex, Ex, c("Male", "Female")) %>% 
  mutate(Age5=case_when(
    Age<1 ~ "Under 1", Age<5 ~ "1-4", Age<10 ~ "5-9", Age<15 ~ "10-14", Age<20 ~ "15-19", Age<25 ~ "20-24",
    Age<30 ~ "25-29", Age<35 ~ "30-34", Age<40 ~ "35-39", Age<45 ~ "40-44", Age<50 ~ "45-49", Age<55 ~ "50-54",
    Age<60 ~ "55-59", Age<65 ~ "60-64", Age<70 ~ "65-69", Age<75 ~ "70-74", Age<80 ~ "75-79", Age<85 ~ "80-84",
    Age<90 ~ "85-89", Age>=90 ~ "90+")) %>% 
  group_by(Year, Sex, Age5) %>% 
  summarise(Pop=sum(Ex), .groups="drop")

#Combine and age-standardise
ONS0122 <- working1 %>% 
  merge(ewpop %>% bind_rows(ewpop %>% group_by(Age5, Year) %>% 
                              summarise(Pop=sum(Pop), .groups="drop") %>% 
                              mutate(Sex="Total")), all.x=TRUE) %>% 
  mutate(mx=Deaths*100000/Pop) %>% 
  select(-c(Deaths, Pop)) %>% 
  spread(Age5, mx) %>% 
  mutate(ASMR=`Under 1`*0.01+`1-4`*0.04+`5-9`*0.055+`10-14`*0.055+`15-19`*0.055+
           `20-24`*0.06+`25-29`*0.06+`30-34`*0.065+`35-39`*0.07+`40-44`*0.07+
           `45-49`*0.07+`50-54`*0.07+`55-59`*0.065+`60-64`*0.06+`65-69`*0.055+
           `70-74`*0.05+`75-79`*0.04+`80-84`*0.025+`85-89`*0.015+`90+`*0.01) %>% 
  select(Year, Sex, Cause, ASMR) %>% 
  mutate(ASMR_adj=ASMR)

#Plot
ggplot(ONS0122, aes(x=Year, y=ASMR, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

#Download older data from 20th Century Mortality Files
#https://webarchive.nationalarchives.gov.uk/ukgwa/20160111174808/http://www.ons.gov.uk/ons/publications/re-reference-tables.html?edition=tcm%3A77-215593

#Populations
temp <- tempfile()
source <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20160111174808mp_/http://www.ons.gov.uk/ons/rel/subnational-health1/the-20th-century-mortality-files/20th-century-deaths/populations-1901-2000.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

ONSpop19012000 <- read_excel(temp, sheet="POPLNS") %>% 
  set_names("Year", "Sex", "Age", "Pop") %>% 
  mutate(Sex=if_else(Sex==1, "Male", "Female"))

#1994-2000
temp <- tempfile()
url <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20160111174808mp_/http://www.ons.gov.uk/ons/rel/subnational-health1/the-20th-century-mortality-files/20th-century-deaths/1994-2000-icd9c.zip"
temp <- unzip(curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb"))

raw9900 <- read_excel(temp, sheet="icd9_8")
raw9798 <- read_excel(temp, sheet="icd9_7")
raw9496 <- read_excel(temp, sheet="icd9_6")

Years910Adj <- c(1993:1998, 2000)

data9400 <- bind_rows(raw9496, raw9798, raw9900) %>% 
  mutate(ICD_9.3=as.integer(substr(ICD_9,1,3)),
         ICD_9.4=as.integer(substr(ICD_9,4,4)),
         Year=as.integer(yr),
         Sex=if_else(sex==1, "Male", "Female"),
         Cause=case_when(
           ICD_9.3 %in% c(140:239) ~ "Cancers",
           ICD_9.3 %in% c(570:573) ~ "Liver disease",
           ICD_9.3==250 ~ "Diabetes",
           ICD_9.3 %in% c(401:404) ~ "Circulatory",
           ICD_9.3 %in% c(410:414, 429) ~ "Ischaemic heart",
           ICD_9.3 %in% c(430:438, 46, 348) ~ "Cerebrovascular",
           ICD_9.3 %in% c(460:466, 477:517) ~ "Respiratory",
           ICD_9.3 %in% c(240:246, 251:279) ~ "Endocrine or metabolic")) %>% 
  filter(!is.na(Cause)) %>% 
  #Apply adjustment factors to allow for alignment of ICD-9 and ICD-10 codes
  #Adjustment factors taken from https://files.digital.nhs.uk/24/7085FD/Compendium%20User%20Guide%202009%20%28November%29%20Annex%202%20V1.pdf
  #Available from https://digital.nhs.uk/data-and-information/publications/ci-hub/compendium-indicators
  mutate(ndths_adj=case_when(
    ICD_9.3==493 & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.056,
    ICD_9.3==188 & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.016,
    ICD_9.3 %in% c(490:492) & Sex=="Male" & Year %in% Years910Adj ~ ndths*0.660,
    ICD_9.3 %in% c(490:492) & Sex=="Female" & Year %in% Years910Adj ~ ndths*0.787,
    ICD_9.3==571 & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.03,
    ICD_9.3 %in% c(410:414) & Sex=="Male" & Year %in% Years910Adj ~ ndths*1.005,
    ICD_9.3 %in% c(410:414) & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.007,
    ICD_9.3 %in% c(401:405) & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.014,
    ICD_9.3==250 & Sex=="Male" & Year %in% Years910Adj ~ ndths*1.044,
    ICD_9.3==250 & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.042,
    ICD_9.3==201 & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.079,
    ICD_9.3 %in% c(204:208) & Sex=="Male" & Year %in% Years910Adj ~ ndths*1.061,
    ICD_9.3 %in% c(204:208) & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.049,
    ICD_9.3==162 & Year %in% Years910Adj ~ ndths*0.996,
    ICD_9.3==172 & Sex=="Male" & Year %in% Years910Adj ~ ndths*0.966,
    ICD_9.3==172 & Sex=="Female" & Year %in% Years910Adj ~ ndths*0.955,
    ICD_9.3==173 & Sex=="Female" & Year %in% Years910Adj ~ ndths*1.140,
    ICD_9.3==151 & Sex=="Male" & Year %in% Years910Adj ~ ndths*1.019,
    ICD_9.3==151 & Sex=="Male" & Year %in% Years910Adj ~ ndths*1.019,
    ICD_9.3 %in% c(430:438) & Sex=="Male" & age %in% c("75-79", "80-84") ~ ndths*1.147,
    ICD_9.3 %in% c(430:438) & Sex=="Male" & age=="85+" ~ ndths*1.176,
    ICD_9.3 %in% c(430:438) & Sex=="Male" ~ ndths*1.073,
    ICD_9.3 %in% c(430:438) & Sex=="Female" & age %in% c("75-79", "80-84") ~ ndths*1.097,
    ICD_9.3 %in% c(430:438) & Sex=="Female" & age=="85+" ~ ndths*1.1,
    ICD_9.3 %in% c(430:438) & Sex=="Female" ~ ndths*1.046,
    TRUE ~ ndths)) %>% 
  group_by(age, Sex, Year, Cause) %>% 
  summarise(Deaths=sum(ndths), Deaths_adj=sum(ndths_adj), .groups="drop")

ONS9400 <- data9400 %>% 
  #Fill in missing rows
  select(-Deaths_adj) %>% 
  spread(age, Deaths) %>% 
  mutate(across(.cols=c(4:22), ~ if_else(is.na(.x), 0, .x))) %>% 
  gather(age, Deaths, c(4:22)) %>% 
  merge(data9400 %>% select(-Deaths), all.x=TRUE) %>% 
  mutate(Deaths_adj=if_else(is.na(Deaths_adj), 0, Deaths_adj)) %>% 
  rename(Age="age") %>% 
  merge(ONSpop19012000, all.x=TRUE) 

ONS9400 <- ONS9400 %>%
  bind_rows(ONS9400 %>% 
              group_by(Age, Year, Cause) %>% 
              summarise(Deaths=sum(Deaths), Deaths_adj=sum(Deaths_adj), Pop=sum(Pop), .groups="drop") %>% 
              mutate(Sex="Total")) %>% 
  mutate(mx=Deaths*100000/Pop, mx_adj=Deaths_adj*100000/Pop) %>% 
  select(-c(Deaths, Deaths_adj, Pop)) %>% 
  group_by(Sex, Year, Cause) %>% 
  summarise(ASMR=mx[Age=="<1"]*0.01+mx[Age=="01-04"]*0.04+mx[Age=="05-09"]*0.055+
              mx[Age=="10-14"]*0.055+mx[Age=="15-19"]*0.055+mx[Age=="20-24"]*0.06+
              mx[Age=="25-29"]*0.06+mx[Age=="30-34"]*0.065+mx[Age=="35-39"]*0.07+
              mx[Age=="40-44"]*0.07+mx[Age=="45-49"]*0.07+mx[Age=="50-54"]*0.07+
              mx[Age=="55-59"]*0.065+mx[Age=="60-64"]*0.06+mx[Age=="65-69"]*0.055+
              mx[Age=="70-74"]*0.05+mx[Age=="75-79"]*0.04+mx[Age=="80-84"]*0.025+
              mx[Age=="85+"]*0.015,
            ASMR_adj=mx_adj[Age=="<1"]*0.01+mx_adj[Age=="01-04"]*0.04+mx_adj[Age=="05-09"]*0.055+
              mx_adj[Age=="10-14"]*0.055+mx_adj[Age=="15-19"]*0.055+mx_adj[Age=="20-24"]*0.06+
              mx_adj[Age=="25-29"]*0.06+mx_adj[Age=="30-34"]*0.065+mx_adj[Age=="35-39"]*0.07+
              mx_adj[Age=="40-44"]*0.07+mx_adj[Age=="45-49"]*0.07+mx_adj[Age=="50-54"]*0.07+
              mx_adj[Age=="55-59"]*0.065+mx_adj[Age=="60-64"]*0.06+mx_adj[Age=="65-69"]*0.055+
              mx_adj[Age=="70-74"]*0.05+mx_adj[Age=="75-79"]*0.04+mx_adj[Age=="80-84"]*0.025+
              mx_adj[Age=="85+"]*0.015, .groups="drop")

#Unadjusted
bind_rows(ONS9400, ONS0122) %>% 
  ggplot(aes(x=Year, y=ASMR, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_vline(xintercept=2000.5, colour="grey60")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

#Adjusted
bind_rows(ONS9400, ONS0122) %>% 
  ggplot(aes(x=Year, y=ASMR_adj, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_vline(xintercept=2000.5, colour="grey60")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

#1985-1993
temp <- tempfile()
url <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20160111174808mp_/http://www.ons.gov.uk/ons/rel/subnational-health1/the-20th-century-mortality-files/20th-century-deaths/1985-1993-icd9b.zip"
temp <- unzip(curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb"))

raw8587 <- read_excel(temp, sheet="icd9_3")
raw8890 <- read_excel(temp, sheet="icd9_4")
raw9193 <- read_excel(temp, sheet="icd9_5")

YearsICD9Adj <- c(1984:1992)

data8593 <- bind_rows(raw8587, raw8890, raw9193) %>% 
  mutate(ICD_9.3=as.integer(substr(ICD_9,1,3)),
         ICD_9.4=as.integer(substr(ICD_9,4,4)),
         Year=as.integer(yr),
         Sex=if_else(sex==1, "Male", "Female"),
         Cause=case_when(
           ICD_9.3 %in% c(140:239) ~ "Cancers",
           ICD_9.3 %in% c(570:573) ~ "Liver disease",
           ICD_9.3==250 ~ "Diabetes",
           ICD_9.3 %in% c(401:404) ~ "Circulatory",
           ICD_9.3 %in% c(410:414, 429) ~ "Ischaemic heart",
           ICD_9.3 %in% c(430:438, 46, 348) ~ "Cerebrovascular",
           ICD_9.3 %in% c(460:466, 477:517) ~ "Respiratory",
           ICD_9.3 %in% c(240:246, 251:279) ~ "Endocrine or metabolic")) %>% 
  filter(!is.na(Cause)) %>% 
  #Apply adjustment factors to account for changes in the way rules were applied around 
  #underlying causes of death, that affected some conditions between 1983 and 1994.
  #This is reported in Rooney and Devis 1996, Population Trends 86 p29-35
  #but actual adjustment factors come from Mortality Statistics: Cause 1984, Series DH2 no 11.
  #HMSO 1985, which is impossible to find, but here's a picture of the tables:
  #https://bsky.app/profile/statsgeekclare.bsky.social/post/3l6zsc332e72z
  #(I know, I know)
  mutate(ndths_adj=case_when(
    Cause=="Cancers" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.953,
    Cause=="Cancers" & Year %in% YearsICD9Adj ~ ndths*0.984,
    Cause=="Diabetes" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.622,
    Cause=="Diabetes" & Year %in% YearsICD9Adj ~ ndths*0.806,
    Cause=="Endocrine or metabolic" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.615,
    Cause=="Endocrine or metabolic" & Year %in% YearsICD9Adj ~ ndths*0.826,
    Cause=="Circulatory" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.909,
    Cause=="Circulatory" & Year %in% YearsICD9Adj ~ ndths*0.952,
    Cause=="Ischaemic heart" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.971,
    Cause=="Ischaemic heart" & Year %in% YearsICD9Adj ~ ndths*0.991,
    Cause=="Cerebrovascular" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.915,
    Cause=="Cerebrovascular" & Year %in% YearsICD9Adj ~ ndths*0.947,
    Cause=="Respiratory" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*1.508,
    Cause=="Respiratory" & Year %in% YearsICD9Adj ~ ndths*1.264,
    Cause=="Liver disease" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.873,
    Cause=="Liver disease" & Year %in% YearsICD9Adj ~ ndths*0.966,
    TRUE ~ ndths)) %>% 
  group_by(age, Sex, Year, Cause) %>% 
  summarise(Deaths=sum(ndths), Deaths_adj=sum(ndths_adj), .groups="drop")
  
ONS8593 <- data8593 %>% 
  #Fill in missing rows
  select(-Deaths_adj) %>% 
  spread(age, Deaths) %>% 
  mutate(across(.cols=c(4:22), ~ if_else(is.na(.x), 0, .x))) %>% 
  gather(age, Deaths, c(4:22)) %>% 
  merge(data8593 %>% select(-Deaths), all.x=TRUE) %>% 
  mutate(Deaths_adj=if_else(is.na(Deaths_adj), 0, Deaths_adj)) %>% 
  rename(Age="age") %>% 
  merge(ONSpop19012000, all.x=TRUE) 

ONS8593 <- ONS8593 %>%
  bind_rows(ONS8593 %>% 
              group_by(Age, Year, Cause) %>% 
              summarise(Deaths=sum(Deaths), Deaths_adj=sum(Deaths_adj), Pop=sum(Pop), .groups="drop") %>% 
              mutate(Sex="Total")) %>% 
  mutate(mx=Deaths*100000/Pop, mx_adj=Deaths_adj*100000/Pop) %>% 
  select(-c(Deaths, Deaths_adj, Pop)) %>% 
  group_by(Sex, Year, Cause) %>% 
  summarise(ASMR=mx[Age=="<1"]*0.01+mx[Age=="01-04"]*0.04+mx[Age=="05-09"]*0.055+
              mx[Age=="10-14"]*0.055+mx[Age=="15-19"]*0.055+mx[Age=="20-24"]*0.06+
              mx[Age=="25-29"]*0.06+mx[Age=="30-34"]*0.065+mx[Age=="35-39"]*0.07+
              mx[Age=="40-44"]*0.07+mx[Age=="45-49"]*0.07+mx[Age=="50-54"]*0.07+
              mx[Age=="55-59"]*0.065+mx[Age=="60-64"]*0.06+mx[Age=="65-69"]*0.055+
              mx[Age=="70-74"]*0.05+mx[Age=="75-79"]*0.04+mx[Age=="80-84"]*0.025+
              mx[Age=="85+"]*0.015,
            ASMR_adj=mx_adj[Age=="<1"]*0.01+mx_adj[Age=="01-04"]*0.04+mx_adj[Age=="05-09"]*0.055+
              mx_adj[Age=="10-14"]*0.055+mx_adj[Age=="15-19"]*0.055+mx_adj[Age=="20-24"]*0.06+
              mx_adj[Age=="25-29"]*0.06+mx_adj[Age=="30-34"]*0.065+mx_adj[Age=="35-39"]*0.07+
              mx_adj[Age=="40-44"]*0.07+mx_adj[Age=="45-49"]*0.07+mx_adj[Age=="50-54"]*0.07+
              mx_adj[Age=="55-59"]*0.065+mx_adj[Age=="60-64"]*0.06+mx_adj[Age=="65-69"]*0.055+
              mx_adj[Age=="70-74"]*0.05+mx_adj[Age=="75-79"]*0.04+mx_adj[Age=="80-84"]*0.025+
              mx_adj[Age=="85+"]*0.015, .groups="drop")

#Unadjusted
bind_rows(ONS8593, ONS9400, ONS0122) %>% 
  ggplot(aes(x=Year, y=ASMR, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_vline(xintercept=2000.5, colour="grey60")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

#Adjusted
bind_rows(ONS8593, ONS9400, ONS0122) %>% 
  ggplot(aes(x=Year, y=ASMR_adj, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  #geom_vline(xintercept=2000.5, colour="grey60")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

#1979-1984
temp <- tempfile()
url <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20160111174808mp_/http://www.ons.gov.uk/ons/rel/subnational-health1/the-20th-century-mortality-files/20th-century-deaths/1979-1984-icd9a.zip"
temp <- unzip(curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb"))

raw7981 <- read_excel(temp, sheet="icd9_1")
raw8284 <- read_excel(temp, sheet="icd9_2")

data7984 <- bind_rows(raw7981, raw8284) %>% 
  mutate(ICD_9.3=as.integer(substr(ICD_9,1,3)),
         ICD_9.4=as.integer(substr(ICD_9,4,4)),
         Year=as.integer(yr),
         Sex=if_else(sex==1, "Male", "Female"),
         Cause=case_when(
           ICD_9.3 %in% c(140:239) ~ "Cancers",
           ICD_9.3 %in% c(570:573) ~ "Liver disease",
           ICD_9.3==250 ~ "Diabetes",
           ICD_9.3 %in% c(401:404) ~ "Circulatory",
           ICD_9.3 %in% c(410:414, 429) ~ "Ischaemic heart",
           ICD_9.3 %in% c(430:438, 46, 348) ~ "Cerebrovascular",
           ICD_9.3 %in% c(460:466, 477:517) ~ "Respiratory",
           ICD_9.3 %in% c(240:246, 251:279) ~ "Endocrine or metabolic")) %>% 
  filter(!is.na(Cause)) %>% 
  #Apply adjustment factors to account for changes in the way rules were applied around 
  #underlying causes of death, that affected some conditions between 1983 and 1994.
  #This is reported in Rooney and Devis 1996, Population Trends 86 p29-35
  #but actual adjustment factors come from Mortality Statistics: Cause 1984, Series DH2 no 11.
  #HMSO 1985, which is impossible to find, but here's a picture of the tables:
  #https://bsky.app/profile/statsgeekclare.bsky.social/post/3l6zsc332e72z
  #(I know, I know)
  mutate(ndths_adj=case_when(
    Cause=="Cancers" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.953,
    Cause=="Cancers" & Year %in% YearsICD9Adj ~ ndths*0.984,
    Cause=="Diabetes" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.622,
    Cause=="Diabetes" & Year %in% YearsICD9Adj ~ ndths*0.806,
    Cause=="Endocrine or metabolic" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.615,
    Cause=="Endocrine or metabolic" & Year %in% YearsICD9Adj ~ ndths*0.826,
    Cause=="Circulatory" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.909,
    Cause=="Circulatory" & Year %in% YearsICD9Adj ~ ndths*0.952,
    Cause=="Ischaemic heart" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.971,
    Cause=="Ischaemic heart" & Year %in% YearsICD9Adj ~ ndths*0.991,
    Cause=="Cerebrovascular" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.915,
    Cause=="Cerebrovascular" & Year %in% YearsICD9Adj ~ ndths*0.947,
    Cause=="Respiratory" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*1.508,
    Cause=="Respiratory" & Year %in% YearsICD9Adj ~ ndths*1.264,
    Cause=="Liver disease" & age %in% c("75-79", "80-84", "85+") & Year %in% YearsICD9Adj ~ ndths*0.873,
    Cause=="Liver disease" & Year %in% YearsICD9Adj ~ ndths*0.966,
    TRUE ~ ndths)) %>% 
  group_by(age, Sex, Year, Cause) %>% 
  summarise(Deaths=sum(ndths), Deaths_adj=sum(ndths_adj), .groups="drop")

ONS7984 <- data7984 %>% 
  #Fill in missing rows
  select(-Deaths_adj) %>% 
  spread(age, Deaths) %>% 
  mutate(across(.cols=c(4:22), ~ if_else(is.na(.x), 0, .x))) %>% 
  gather(age, Deaths, c(4:22)) %>% 
  merge(data7984 %>% select(-Deaths), all.x=TRUE) %>% 
  mutate(Deaths_adj=if_else(is.na(Deaths_adj), 0, Deaths_adj)) %>% 
  rename(Age="age") %>% 
  merge(ONSpop19012000, all.x=TRUE) 

ONS7984 <- ONS7984 %>%
  bind_rows(ONS7984 %>% 
              group_by(Age, Year, Cause) %>% 
              summarise(Deaths=sum(Deaths), Deaths_adj=sum(Deaths_adj), Pop=sum(Pop), .groups="drop") %>% 
              mutate(Sex="Total")) %>% 
  mutate(mx=Deaths*100000/Pop, mx_adj=Deaths_adj*100000/Pop) %>% 
  select(-c(Deaths, Deaths_adj, Pop)) %>% 
  group_by(Sex, Year, Cause) %>% 
  summarise(ASMR=mx[Age=="<1"]*0.01+mx[Age=="01-04"]*0.04+mx[Age=="05-09"]*0.055+
              mx[Age=="10-14"]*0.055+mx[Age=="15-19"]*0.055+mx[Age=="20-24"]*0.06+
              mx[Age=="25-29"]*0.06+mx[Age=="30-34"]*0.065+mx[Age=="35-39"]*0.07+
              mx[Age=="40-44"]*0.07+mx[Age=="45-49"]*0.07+mx[Age=="50-54"]*0.07+
              mx[Age=="55-59"]*0.065+mx[Age=="60-64"]*0.06+mx[Age=="65-69"]*0.055+
              mx[Age=="70-74"]*0.05+mx[Age=="75-79"]*0.04+mx[Age=="80-84"]*0.025+
              mx[Age=="85+"]*0.015,
            ASMR_adj=mx_adj[Age=="<1"]*0.01+mx_adj[Age=="01-04"]*0.04+mx_adj[Age=="05-09"]*0.055+
              mx_adj[Age=="10-14"]*0.055+mx_adj[Age=="15-19"]*0.055+mx_adj[Age=="20-24"]*0.06+
              mx_adj[Age=="25-29"]*0.06+mx_adj[Age=="30-34"]*0.065+mx_adj[Age=="35-39"]*0.07+
              mx_adj[Age=="40-44"]*0.07+mx_adj[Age=="45-49"]*0.07+mx_adj[Age=="50-54"]*0.07+
              mx_adj[Age=="55-59"]*0.065+mx_adj[Age=="60-64"]*0.06+mx_adj[Age=="65-69"]*0.055+
              mx_adj[Age=="70-74"]*0.05+mx_adj[Age=="75-79"]*0.04+mx_adj[Age=="80-84"]*0.025+
              mx_adj[Age=="85+"]*0.015, .groups="drop")

ggplot(ONS7984, aes(x=Year, y=ASMR, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

#1968-1978
temp <- tempfile()
url <- "https://webarchive.nationalarchives.gov.uk/ukgwa/20160111174808mp_/http://www.ons.gov.uk/ons/rel/subnational-health1/the-20th-century-mortality-files/20th-century-deaths/1968-1978-icd8.zip"
temp <- unzip(curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb"))

raw6871 <- read_excel(temp, sheet="icd8_1")
raw7275 <- read_excel(temp, sheet="icd8_2")
raw7678 <- read_excel(temp, sheet="icd8_3")

data6878 <- bind_rows(raw6871, raw7275, raw7678) %>% 
  mutate(ICD_8.3=as.integer(substr(ICD_8,1,3)),
         ICD_8.4=as.integer(substr(ICD_8,4,4)),
         Year=as.integer(yr),
         Sex=if_else(sex==1, "Male", "Female"),
         Cause=case_when(
           ICD_8.3 %in% c(140:239) ~ "Cancers",
           ICD_8.3 %in% c(570:573) ~ "Liver disease",
           ICD_8.3==250 ~ "Diabetes",
           ICD_8.3 %in% c(401:404) ~ "Circulatory",
           ICD_8.3 %in% c(410:414, 429) ~ "Ischaemic heart",
           ICD_8.3 %in% c(430:438, 46, 348) ~ "Cerebrovascular",
           ICD_8.3 %in% c(460:466, 477:517) ~ "Respiratory",
           ICD_8.3 %in% c(240:246, 251:279) ~ "Endocrine or metabolic")) %>% 
  filter(!is.na(Cause)) %>% 
  group_by(age, Sex, Year, Cause) %>% 
  summarise(Deaths=sum(ndths), .groups="drop")

ONS6878 <- data6878 %>% 
  rename(Age="age") %>% 
  merge(ONSpop19012000, all.x=TRUE) 

ONS6878 <- ONS6878 %>%
  bind_rows(ONS6878 %>% 
              group_by(Age, Year, Cause) %>% 
              summarise(Deaths=sum(Deaths), Pop=sum(Pop), .groups="drop") %>% 
              mutate(Sex="Total")) %>% 
  mutate(mx=Deaths*100000/Pop) %>% 
  select(-c(Deaths, Pop)) %>% 
  spread(Age, mx) %>% 
  mutate(across(.cols=c(4:22), ~ if_else(is.na(.x), 0, .x))) %>% 
  mutate(ASMR=`<1`*0.01+`01-04`*0.04+`05-09`*0.055+`10-14`*0.055+`15-19`*0.055+
           `20-24`*0.06+`25-29`*0.06+`30-34`*0.065+`35-39`*0.07+`40-44`*0.07+
           `45-49`*0.07+`50-54`*0.07+`55-59`*0.065+`60-64`*0.06+`65-69`*0.055+
           `70-74`*0.05+`75-79`*0.04+`80-84`*0.025+`85+`*0.015) %>% 
  select(Year, Sex, Cause, ASMR) %>% 
  mutate(ASMR_adj=ASMR)

#Plot
ggplot(ONS6878, aes(x=Year, y=ASMR, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

#Full, unadjusted time series
Fulldata <- bind_rows(ONS6878, ONS7984, ONS8593, ONS9400, ONS0122)

ggplot(Fulldata, aes(x=Year, y=ASMR, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_vline(xintercept=2000.5, colour="grey60")+
  geom_vline(xintercept=1993.5, colour="grey60")+
  geom_vline(xintercept=1983.5, colour="grey60")+
  geom_vline(xintercept=1978.5, colour="grey60")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

ggplot(Fulldata, aes(x=Year, y=ASMR_adj, colour=Sex, linetype=Sex))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_vline(xintercept=2000.5, colour="grey60")+
  geom_vline(xintercept=1993.5, colour="grey60")+
  geom_vline(xintercept=1983.5, colour="grey60")+
  geom_vline(xintercept=1978.5, colour="grey60")+
  geom_line()+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  scale_colour_manual(values=c("#6600cc", "#00cc99", "black"))+
  scale_linetype_manual(values=c(1,1,2))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()

Fulldata %>% filter(Sex=="Total" & Cause=="Liver disease") %>% 
  ggplot(aes(x=Year))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_vline(xintercept=1983, colour="grey60")+
  geom_line(aes(y=ASMR), colour="tomato")+
  geom_line(aes(y=ASMR_adj), colour="royalblue")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate\nper 100,000")+
  theme_custom()

agg_png("Outputs/LongTermDeathsxCauseIndexedAdjusted.png", units="in", width=9, height=6, res=800)
Fulldata %>% 
  group_by(Sex, Cause) %>% 
  mutate(index=ASMR_adj/ASMR_adj[Year==1968]) %>% 
  ungroup() %>% 
  filter(!Cause %in% c("Endocrine or metabolic", "Circulatory") & Sex=="Total") %>% 
  mutate(Cause=factor(Cause, levels=c("Liver disease", "Diabetes", "Cancers",
                                      "Respiratory", "Ischaemic heart", "Cerebrovascular"))) %>% 
  ggplot(aes(x=Year, y=index, colour=Cause))+
  geom_hline(yintercept=1, colour="grey20")+
  geom_line(linewidth=1)+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Change in age-standardised mortality rate\nsince 1968", 
                     breaks=c(0.25, 0.5, 1, 2,3, 4), trans="log",
                     labels=c("Quartered", "Halved", "No change", "Doubled","Tripled",  "Quadrupled"))+
  scale_colour_manual(values=c("#436cab", "#00a9e2", "#00a7c9",  "#ca9c54", "#9b4494", "#b11048"))+
  theme_custom()

dev.off()