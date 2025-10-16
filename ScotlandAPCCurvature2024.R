rm(list=ls())

#Mortality smooth is currently off CRAN, so take mirrored version from Tim Riffe's GitHub
#remotes::install_github("timriffe/MortalitySmooth")

library(curl)
library(readxl)
library(keyring)
library(tidyverse)
library(HMDHFDplus)
library(paletteer)
library(ragg)
library(extrafont)
library(patchwork)
library(scales)
library(ggtext)
library(gt)
library(MortalitySmooth)
library(splines2)
library(epiR)
library(MASS)
library(modelr)
library(Epi)

#Set common font for all plots
font <- "Lato"

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


options(scipen=10000)

##########
#Scotland#
##########
#Get Scottish data
scotfile.2024 <- tempfile()
scoturl.2024 <- "https://www.nrscotland.gov.uk/media/ckqgj5fs/vital-events-reference-tables-all-chapters.xlsx"
scotfile.2024 <- curl_download(url=scoturl.2024, destfile=scotfile.2024, quiet=FALSE, mode="wb")

scotdata.2024 <- read_excel(scotfile.2024, sheet="Table_6.04", range=c("B10:Y1755"), col_names=FALSE) %>% 
  mutate(Year=2024)

scotfile.2023 <- tempfile()
scoturl.2023 <- "https://www.nrscotland.gov.uk/media/swuny3ez/vital-events-refernce-tables-2023-all-tables.xlsx"
scotfile.2023 <- curl_download(url=scoturl.2023, destfile=scotfile.2023, quiet=FALSE, mode="wb")

scotdata.2023 <- read_excel(scotfile.2023, sheet="6.04", range=c("B11:Y1729"), col_names=FALSE) %>% 
  mutate(Year=2023)

##############
#Following code works fine on some computers, but not mine :(
#So instead have to download files manually. Sigh.
scotfile.2022 <- tempfile()
scoturl.2022 <- "https://webarchive.nrscotland.gov.uk/20241128122608mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2022/vital-events-22-ref-tabs-6.xlsx"
scotfile.2022 <- curl_download(url=scoturl.2022, destfile=scotfile.2022, quiet=FALSE, mode="wb")

scotdata.2022 <- read_excel(scotfile.2022, sheet="6.04", range=c("B11:Y1730"), col_names=FALSE) %>%
  mutate(Year=2022)

scotfile.2021 <- tempfile()
scoturl.2021 <- "https://webarchive.nrscotland.gov.uk/20240326182051mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2021/vital-events-21-ref-tabs-6.xlsx"
scotfile.2021 <- curl_download(url=scoturl.2021, destfile=scotfile.2021, quiet=FALSE, mode="wb")

scotdata.2021 <- read_excel(scotfile.2021, sheet="6.04", range=c("B10:Y1726"), col_names=FALSE) %>%
  mutate(Year=2021)

scotfile.2020 <- tempfile()
scoturl.2020 <- "https://webarchive.nrscotland.gov.uk/20220315040830mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2020/vital-events-20-ref-tabs-6.xlsx"
scotfile.2020 <- curl_download(url=scoturl.2020, destfile=scotfile.2020, quiet=FALSE, mode="wb")

scotdata.2020 <- read_excel(scotfile.2020, sheet="6.04", range=c("A9:X1789"), col_names=FALSE) %>%
  mutate(Year=2020)

scotfile.2019 <- tempfile()
scoturl.2019 <- "https://webarchive.nrscotland.gov.uk/20210314054057mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2019/vital-events-19-ref-tabs-6.xlsx"
scotfile.2019 <- curl_download(url=scoturl.2019, destfile=scotfile.2019, quiet=FALSE, mode="wb")

scotdata.2019 <- read_excel(scotfile.2019, sheet="6.04", range=c("A9:X1739"), col_names=FALSE) %>%
  mutate(Year=2019)

scotfile.2018 <- tempfile()
scoturl.2018 <- "https://webarchive.nrscotland.gov.uk/20210314055026mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2018/vital-events-18-ref-tabs-6.xlsx"
scotfile.2018 <- curl_download(url=scoturl.2018, destfile=scotfile.2018, quiet=FALSE, mode="wb")

scotdata.2018 <- read_excel(scotfile.2018, sheet="6.04", range=c("A9:X1698"), col_names=FALSE) %>%
  mutate(Year=2018)

scotfile.2017 <- tempfile()
scoturl.2017 <- "https://webarchive.nrscotland.gov.uk/20210314060156mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2017/vital-events-17-ref-tabs-6-corrected.xlsx"
scotfile.2017 <- curl_download(url=scoturl.2017, destfile=scotfile.2017, quiet=FALSE, mode="wb")

scotdata.2017 <- read_excel(scotfile.2017, sheet="6.04", range=c("A9:X1767"), col_names=FALSE) %>%
  mutate(Year=2017)

scotfile.2016 <- tempfile()
scoturl.2016 <- "https://webarchive.nrscotland.gov.uk/20210314061109mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/16/6-d-cause/ve-ref-tabs-16-tab6.04.xlsx"
scotfile.2016 <- curl_download(url=scoturl.2016, destfile=scotfile.2016, quiet=FALSE, mode="wb")

scotdata.2016 <- read_excel(scotfile.2016, sheet="6.04", range=c("A9:X1776"), col_names=FALSE) %>%
  mutate(Year=2016)

scotfile.2015 <- tempfile()
scoturl.2015 <- "https://webarchive.nrscotland.gov.uk/20210314062647mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2015/section6/15-vital-events-ref-tabs-6-4.xlsx"
scotfile.2015 <- curl_download(url=scoturl.2015, destfile=scotfile.2015, quiet=FALSE, mode="wb")

scotdata.2015 <- read_excel(scotfile.2015, sheet="6.4", range=c("A9:W1784"), col_names=FALSE) %>%
  mutate(Year=2015)

scotfile.2014 <- tempfile()
scoturl.2014 <- "https://webarchive.nrscotland.gov.uk/20210314063758mp_/https://www.nrscotland.gov.uk/files//statistics/vital-events-ref-tables/2014/section-6/14-vital-events-ref-tabs-6-4.xlsx"
scotfile.2014 <- curl_download(url=scoturl.2014, destfile=scotfile.2014, quiet=FALSE, mode="wb")

scotdata.2014 <- read_excel(scotfile.2014, sheet="6.4", range=c("A9:W1683"), col_names=FALSE) %>%
  mutate(Year=2014)

scotfile.2013 <- tempfile()
scoturl.2013 <- "https://webarchive.nrscotland.gov.uk/20210314064649mp_/https://www.nrscotland.gov.uk/files//statistics/ve-ref-tables-2013/2013-ref-tabs-6-4.xls"
scotfile.2013 <- curl_download(url=scoturl.2013, destfile=scotfile.2013, quiet=FALSE, mode="wb")

scotdata.2013 <- read_excel(scotfile.2013, sheet="6.4", range=c("A9:W1741"), col_names=FALSE) %>%
  mutate(Year=2013)

scotfile.2012 <- tempfile()
scoturl.2012 <- "https://webarchive.nrscotland.gov.uk/20210314065708mp_/https://www.nrscotland.gov.uk/files/statistics/ve-ref-tables-2012/ve-12-t6-4.xls"
scotfile.2012 <- curl_download(url=scoturl.2012, destfile=scotfile.2012, quiet=FALSE, mode="wb")

scotdata.2012 <- read_excel(scotfile.2012, sheet="6.4", range=c("A9:W1736"), col_names=FALSE) %>%
  mutate(Year=2012)

scotfile.2011 <- tempfile()
scoturl.2011 <- "https://webarchive.nrscotland.gov.uk/20210314070639mp_/https://www.nrscotland.gov.uk/files/statistics/ve-reftables-2011/ve-2011-t6.4.xls"
scotfile.2011 <- curl_download(url=scoturl.2011, destfile=scotfile.2011, quiet=FALSE, mode="wb")

scotdata.2011 <- read_excel(scotfile.2011, sheet="6.4", range=c("A9:W1791"), col_names=FALSE) %>%
  mutate(Year=2011)

scotfile.2010 <- tempfile()
scoturl.2010 <- "https://webarchive.nrscotland.gov.uk/20210314071625mp_/https://www.nrscotland.gov.uk/files/statistics/ve-reftables-2010/ve10-t6-4.xls"
scotfile.2010 <- curl_download(url=scoturl.2010, destfile=scotfile.2010, quiet=FALSE, mode="wb")

scotdata.2010 <- read_excel(scotfile.2010, range=c("A9:W1779"), col_names=FALSE) %>%
  mutate(Year=2010)

scotfile.2009 <- tempfile()
scoturl.2009 <- "https://webarchive.nrscotland.gov.uk/20210314072703mp_/https://www.nrscotland.gov.uk/files/statistics/ve-reftables-09/ve09-t6-4.xls"
scotfile.2009 <- curl_download(url=scoturl.2009, destfile=scotfile.2009, quiet=FALSE, mode="wb")

scotdata.2009 <- read_excel(scotfile.2009, range=c("A9:W1794"), col_names=FALSE) %>%
  mutate(Year=2009)

scotfile.2008 <- tempfile()
scoturl.2008 <- "https://webarchive.nrscotland.gov.uk/20210314073648mp_/https://www.nrscotland.gov.uk/files/statistics/vital-events-ref-tables-2008/ve-2008-t6-4.xls"
scotfile.2008 <- curl_download(url=scoturl.2008, destfile=scotfile.2008, quiet=FALSE, mode="wb")

scotdata.2008 <- read_excel(scotfile.2008, range=c("A9:W1811"), col_names=FALSE) %>%
  mutate(Year=2008)

scotfile.2007 <- tempfile()
scoturl.2007 <- "https://webarchive.nrscotland.gov.uk/20210314075202mp_/https://www.nrscotland.gov.uk/files/statistics/07t6-4.xls"
scotfile.2007 <- curl_download(url=scoturl.2007, destfile=scotfile.2007, quiet=FALSE, mode="wb")

scotdata.2007 <- read_excel(scotfile.2007, range=c("A9:W1867"), col_names=FALSE) %>%
  mutate(Year=2007)

scotfile.2006 <- tempfile()
scoturl.2006 <- "https://webarchive.nrscotland.gov.uk/20210314080331mp_/https://www.nrscotland.gov.uk/files/statistics/06t6-4%20rev.xls"
scotfile.2006 <- curl_download(url=scoturl.2006, destfile=scotfile.2006, quiet=FALSE, mode="wb")

scotdata.2006 <- read_excel(scotfile.2006, range=c("A8:W1984"), col_names=FALSE) %>%
  mutate(Year=2006)

scotfile.2005 <- tempfile()
scoturl.2005 <- "https://webarchive.nrscotland.gov.uk/20210314081420mp_/https://www.nrscotland.gov.uk/files/statistics/old/05t6-4.xls"
scotfile.2005 <- curl_download(url=scoturl.2005, destfile=scotfile.2005, quiet=FALSE, mode="wb")

scotdata.2005 <- read_excel(scotfile.2005, range=c("A9:X2017"), col_names=FALSE) %>%
  mutate(Year=2005)

scotfile.2004 <- tempfile()
scoturl.2004 <- "https://webarchive.nrscotland.gov.uk/20210314082843mp_/https://www.nrscotland.gov.uk/files/statistics/old/04t6-4.xls"
scotfile.2004 <- curl_download(url=scoturl.2004, destfile=scotfile.2004, quiet=FALSE, mode="wb")

scotdata.2004 <- read_excel(scotfile.2004, range=c("A9:X2033"), col_names=FALSE) %>%
  mutate(Year=2004)

scotfile.2003 <- tempfile()
scoturl.2003 <- "https://webarchive.nrscotland.gov.uk/20210314084050mp_/https://www.nrscotland.gov.uk/files/statistics/old/03t6-4.xls"
scotfile.2003 <- curl_download(url=scoturl.2003, destfile=scotfile.2003, quiet=FALSE, mode="wb")

scotdata.2003 <- read_excel(scotfile.2003, range=c("A9:X2081"), col_names=FALSE) %>%
  mutate(Year=2003)

scotfile.2002 <- tempfile()
scoturl.2002 <- "https://webarchive.nrscotland.gov.uk/20210314085332mp_/https://www.nrscotland.gov.uk/files/statistics/old/02t6-4.xls"
scotfile.2002 <- curl_download(url=scoturl.2002, destfile=scotfile.2002, quiet=FALSE, mode="wb")

scotdata.2002 <- read_excel(scotfile.2002, range=c("A9:X2067"), col_names=FALSE) %>%
  mutate(Year=2002)

scotfile.2001 <- tempfile()
scoturl.2001 <- "https://webarchive.nrscotland.gov.uk/20210314090752mp_/https://www.nrscotland.gov.uk/files/statistics/old/01t6_4.xls"
scotfile.2001 <- curl_download(url=scoturl.2001, destfile=scotfile.2001, quiet=FALSE, mode="wb")

scotdata.2001 <- read_excel(scotfile.2001, range=c("A9:X2056"), col_names=FALSE) %>%
  mutate(Year=2001)

#Alternative code to bring in locally downloaded versions
#https://webarchive.nrscotland.gov.uk/20241128122543/https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/vital-events-reference-tables/archive
# NRSpath <- "C:/Users/cm1cra/Data_projects/colin_misc/Data/NRS Vital Events/"
# 
# scotdata.2022 <- read_excel(paste0(NRSpath, "vital-events-22-ref-tabs-6.xlsx"), 
#                             sheet="6.04", range=c("B11:Y1730"), col_names=FALSE) %>% 
#   mutate(Year=2022)
# 
# scotdata.2021 <- read_excel(paste0(NRSpath, "vital-events-21-ref-tabs-6.xlsx"), 
#                             sheet="6.04", range=c("B10:Y1726"), col_names=FALSE) %>% 
#   mutate(Year=2021)
# 
# scotdata.2020 <- read_excel(paste0(NRSpath, "vital-events-20-ref-tabs-6.xlsx"), 
#                             sheet="6.04", range=c("A9:X1789"), col_names=FALSE) %>% 
#   mutate(Year=2020)
# 
# scotdata.2019 <- read_excel(paste0(NRSpath, "vital-events-19-ref-tabs-6.xlsx"), 
#                             sheet="6.04", range=c("A9:X1739"), col_names=FALSE) %>% 
#   mutate(Year=2019)
# 
# scotdata.2018 <- read_excel(paste0(NRSpath, "vital-events-18-ref-tabs-6.xlsx"), 
#                             sheet="6.04", range=c("A9:X1698"), col_names=FALSE) %>% 
#   mutate(Year=2018)
# 
# scotdata.2017 <- read_excel(paste0(NRSpath, "vital-events-17-ref-tabs-6-corrected.xlsx"), 
#                             sheet="6.04", range=c("A9:X1767"), col_names=FALSE) %>% 
#   mutate(Year=2017)
# 
# scotdata.2016 <- read_excel(paste0(NRSpath, "ve-ref-tabs-16-tab6.04.xlsx"), 
#                             sheet="6.04", range=c("A9:X1776"), col_names=FALSE) %>% 
#   mutate(Year=2016)
# 
# scotdata.2015 <- read_excel(paste0(NRSpath, "15-vital-events-ref-tabs-6-4.xlsx"), 
#                             sheet="6.4", range=c("A9:W1784"), col_names=FALSE) %>% 
#   mutate(Year=2015)
# 
# scotdata.2014 <- read_excel(paste0(NRSpath, "14-vital-events-ref-tabs-6-4.xlsx"), 
#                             sheet="6.4", range=c("A9:W1683"), col_names=FALSE) %>% 
#   mutate(Year=2014)
# 
# scotdata.2013 <- read_excel(paste0(NRSpath, "2013-ref-tabs-6-4.xls"), 
#                             sheet="6.4", range=c("A9:W1741"), col_names=FALSE) %>% 
#   mutate(Year=2013)
# 
# scotdata.2012 <- read_excel(paste0(NRSpath, "ve-12-t6-4.xls"), 
#                             sheet="6.4", range=c("A9:W1736"), col_names=FALSE) %>% 
#   mutate(Year=2012)
# 
# scotdata.2011 <- read_excel(paste0(NRSpath, "ve-2011-t6.4.xls"), 
#                             sheet="6.4", range=c("A9:W1791"), col_names=FALSE) %>% 
#   mutate(Year=2011)
# 
# scotdata.2010 <- read_excel(paste0(NRSpath, "ve10-t6-4.xls"), 
#                             range=c("A9:W1779"), col_names=FALSE) %>% 
#   mutate(Year=2010)
# 
# scotdata.2009 <- read_excel(paste0(NRSpath, "ve09-t6-4.xls"), 
#                             range=c("A9:W1794"), col_names=FALSE) %>% 
#   mutate(Year=2009)
# 
# scotdata.2008 <- read_excel(paste0(NRSpath, "ve-2008-t6-4.xls"), 
#                             range=c("A9:W1811"), col_names=FALSE) %>% 
#   mutate(Year=2008)
# 
# scotdata.2007 <- read_excel(paste0(NRSpath, "07t6-4.xls"), 
#                             range=c("A9:W1867"), col_names=FALSE) %>% 
#   mutate(Year=2007)
# 
# scotdata.2006 <- read_excel(paste0(NRSpath, "06t6-4 rev.xls"), 
#                             range=c("A8:W1984"), col_names=FALSE) %>% 
#   mutate(Year=2006)
# 
# scotdata.2005 <- read_excel(paste0(NRSpath, "05t6-4.xls"), 
#                             range=c("A9:X2017"), col_names=FALSE) %>% 
#   mutate(Year=2005)
# 
# scotdata.2004 <- read_excel(paste0(NRSpath, "04t6-4.xls"), 
#                             range=c("A9:X2033"), col_names=FALSE) %>% 
#   mutate(Year=2004)
# 
# scotdata.2003 <- read_excel(paste0(NRSpath, "03t6-4.xls"), 
#                             range=c("A9:X2081"), col_names=FALSE) %>% 
#   mutate(Year=2003)
# 
# scotdata.2002 <- read_excel(paste0(NRSpath, "02t6-4.xls"), 
#                             range=c("A9:X2067"), col_names=FALSE) %>% 
#   mutate(Year=2002)
# 
# scotdata.2001 <- read_excel(paste0(NRSpath, "01t6_4.xls"), 
#                             range=c("A9:X2056"), col_names=FALSE) %>% 
#   mutate(Year=2001)
#########################

#Bring together older data, with infuriatingly slightly different formatting
scotdata <- bind_rows(scotdata.2001, scotdata.2002, scotdata.2003, scotdata.2004, scotdata.2005) %>% 
  filter(!is.na(`...4`)) %>% 
  rename(ICD10=`...1`, Sex=`...4`) %>% 
  gather(Age, Dx, c(6:24)) %>% 
  fill(ICD10) %>% 
  filter(nchar(ICD10)<=3) %>% 
  #Stupid faff because of *horrible* formatting choices in the data
  mutate(`...5`=as.numeric(`...5`)) %>% 
  arrange(Year, Sex, Age, ICD10, `...5`) %>% 
  distinct(Year, Sex, Age, ICD10, .keep_all=TRUE) %>% 
  dplyr::select(-c(`...2`, `...3`, `...5`)) %>% 
  mutate(Dx=as.numeric(if_else(Dx %in% c("-", ".", NA), "0", Dx)),
         Age=case_when(Age=="...6" ~ "0", Age=="...7" ~ "1-4",Age=="...8" ~ "5-9",
                       Age=="...9" ~ "10-14", Age=="...10" ~ "15-19", Age=="...11" ~ "20-24",
                       Age=="...12" ~ "25-29", Age=="...13" ~ "30-34", Age=="...14" ~ "35-39",
                       Age=="...15" ~ "40-44", Age=="...16" ~ "45-49", Age=="...17" ~ "50-54",
                       Age=="...18" ~ "55-59", Age=="...19" ~ "60-64", Age=="...20" ~ "65-69",
                       Age=="...21" ~ "70-74", Age=="...22" ~ "75-79", Age=="...23" ~ "80-84",
                       Age=="...24" ~ "85+"),
         Sex=if_else(Sex=="M", 1, 2))

#Age groups are 90+ in 2016 onwards, but 85+ before then
scotdata <- bind_rows(scotdata.2006, scotdata.2007, scotdata.2008, scotdata.2009, scotdata.2010, 
                      scotdata.2011, scotdata.2012, scotdata.2013, scotdata.2014, scotdata.2015, 
                      scotdata.2016, scotdata.2017, scotdata.2018, scotdata.2019, scotdata.2020) %>% 
  filter(!is.na(`...3`)) %>% 
  rename(ICD10=`...1`, Sex=`...3`) %>% 
  gather(Age, Dx, c(5:23, 25)) %>% 
  fill(ICD10) %>% 
  filter(nchar(ICD10)<=3) %>% 
  #Stupid faff because of *horrible* formatting choices in the data
  mutate(`...4`=as.numeric(`...4`)) %>% 
  arrange(Year, Sex, Age, ICD10, `...4`) %>% 
  distinct(Year, Sex, Age, ICD10, .keep_all=TRUE) %>% 
  dplyr::select(-c(`...2`, `...4`)) %>% 
  mutate(Dx=as.numeric(if_else(Dx %in% c("-", ".", NA), "0", Dx)),
         Age=case_when(Age=="...5" ~ "0", Age=="...6" ~ "1-4",Age=="...7" ~ "5-9",
                       Age=="...8" ~ "10-14", Age=="...9" ~ "15-19", Age=="...10" ~ "20-24",
                       Age=="...11" ~ "25-29", Age=="...12" ~ "30-34", Age=="...13" ~ "35-39",
                       Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                       Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69",
                       Age=="...20" ~ "70-74", Age=="...21" ~ "75-79", Age=="...22" ~ "80-84",
                       TRUE ~ "85+"),
         Sex=if_else(Sex=="M", 1, 2)) %>% 
  #Add in 2021 data, which was formatted differently (yay)
  bind_rows(scotdata.2021 %>% 
              #Sort out first 26 rows which are extra borked for some reason
              slice_head(n=26) %>% 
              mutate(lag1=lag(`...1`, n=1), lag2=lag(`...2`, n=1),
                     `...1`=coalesce(`...1`, lag1), `...2`=coalesce(`...2`, lag2)) %>% 
              bind_rows(scotdata.2021 %>% slice_tail(n=(nrow(.)-26))) %>% 
              filter(!is.na(`...2`)) %>% 
              mutate(ICD10=substr(`...2`, 1, 3)) %>% 
              dplyr::select(-`...4`) %>% 
              gather(Age, Dx, c(4:23)) %>% 
              dplyr::select(Year, `...3`, Age, ICD10, Dx) %>% 
              rename(Sex=`...3`) %>% 
              mutate(Dx=replace_na(as.numeric(Dx),0),
                     Age=case_when(Age=="...5" ~ "0", Age=="...6" ~ "1-4",Age=="...7" ~ "5-9",
                                   Age=="...8" ~ "10-14", Age=="...9" ~ "15-19", Age=="...10" ~ "20-24",
                                   Age=="...11" ~ "25-29", Age=="...12" ~ "30-34", Age=="...13" ~ "35-39",
                                   Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                                   Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69",
                                   Age=="...20" ~ "70-74", Age=="...21" ~ "75-79", Age=="...22" ~ "80-84",
                                   TRUE ~ "85+"),
                     Sex=if_else(Sex=="M", 1, 2))) %>% 
  #Add in 2022-24 data, which if formatted differently again
  bind_rows(bind_rows(scotdata.2022, scotdata.2023, scotdata.2024) %>% 
              filter(`...2`!="Total") %>% 
              mutate(ICD10=substr(`...2`, 1, 3)) %>% 
              dplyr::select(-`...4`) %>% 
              gather(Age, Dx, c(4:23)) %>% 
              dplyr::select(Year, `...3`, Age, ICD10, Dx) %>% 
              rename(Sex=`...3`) %>% 
              mutate(Dx=replace_na(as.numeric(Dx),0),
                     Age=case_when(Age=="...5" ~ "0", Age=="...6" ~ "1-4",Age=="...7" ~ "5-9",
                                   Age=="...8" ~ "10-14", Age=="...9" ~ "15-19", Age=="...10" ~ "20-24",
                                   Age=="...11" ~ "25-29", Age=="...12" ~ "30-34", Age=="...13" ~ "35-39",
                                   Age=="...14" ~ "40-44", Age=="...15" ~ "45-49", Age=="...16" ~ "50-54",
                                   Age=="...17" ~ "55-59", Age=="...18" ~ "60-64", Age=="...19" ~ "65-69",
                                   Age=="...20" ~ "70-74", Age=="...21" ~ "75-79", Age=="...22" ~ "80-84",
                                   TRUE ~ "85+"),
                     Sex=if_else(Sex %in% c("M", "Males"), 1, 2))) %>% 
  #Collapse into age groups we actually want
  group_by(Age, Sex, Year, ICD10) %>% 
  summarise(Dx=sum(Dx), .groups="drop") %>% 
  bind_rows(scotdata) %>% 
  #Remove stuborn codes that remain due to bad formatting in the data
  filter(!ICD10 %in% c("E90", "F99", "G99", "J99", "K93", "L99", "M99", "N99", "Y98")) %>% 
  filter(!(ICD10=="I99" & Year==2006)) %>% 
  mutate(code1=substr(ICD10, 1, 1), code2=as.numeric(substr(ICD10,2,3)),
         Cause=case_when(
           code1=="K" & code2 %in% c(70, 73, 74) ~ "Alcohol",
           code1=="F" & code2==10 ~ "Alcohol",
           code1=="X" & code2==45 ~ "Alcohol", #Difference from the Masters defns as X45 is clearly alcohol-related
           code1=="Y" & code2==15 ~ "Alcohol", #Difference from the Masters defns as Y15 is clearly alcohol-related
           code1=="X" & code2 %in% c(40:44, 85) ~ "Drugs",
           code1=="Y" & code2 %in% c(10:14) ~ "Drugs",
           code1=="F" & code2 %in% c(11:16, 18, 19) ~ "Drugs", #Including F18 here to align with Scottish data
           code1=="U" & code2==3 ~ "Suicide",
           code1=="X" & code2 %in% c(60:84) ~ "Suicide",
           code1=="Y" & code2 ==87 ~ "Suicide",
           code1=="C" ~ "Cancer",
           code1=="I" ~ "Cardiovascular disease",
           code1 %in% c("A", "B") ~ "Infectious diseases",
           code1=="J" ~ "Respiratory diseases",
           code1=="V" ~ "Transport accidents",
           code1 %in% c("W", "X", "Y") ~ "Other external causes",
           (code1=="F" & code2 %in% c(1:3)) | ICD10=="G30" ~ "Dementia/Alzheimers",
           TRUE ~ "Other")) %>% 
  group_by(Sex, Age, Cause, Year) %>% 
  summarise(Dx=sum(Dx)) %>% 
  ungroup()

#Add all-cause deaths group
scotdata.wide <- scotdata %>% 
  spread(Year, Dx) %>% 
  group_by(Age, Sex) %>% 
  summarise(across(c(`2001`:`2024`), sum)) %>% 
  ungroup() %>% 
  mutate(Cause="Total") %>% 
  bind_rows(scotdata %>% spread(Year, Dx) )%>% 
  #Initiate start of age groups
  mutate(agestart=case_when(
    Age=="0" ~ 0,
    Age=="1-4" ~ 1, Age=="5-9" ~ 5, Age=="10-14" ~ 10, Age=="15-19" ~ 15,
    Age=="20-24" ~ 20, Age=="25-29" ~ 25, Age=="30-34" ~ 30, Age=="35-39" ~ 35,
    Age=="40-44" ~ 40, Age=="45-49" ~ 45, Age=="50-54" ~ 50, Age=="55-59" ~ 55,
    Age=="60-64" ~ 60, Age=="65-69" ~ 65, Age=="70-74" ~ 70, Age=="75-79" ~ 75,
    Age=="80-84" ~ 80, TRUE ~ 85)) %>% 
  arrange(agestart) %>% 
  relocate(Cause, agestart)

#Download populations
scotpop <- readHMDweb(CNTRY="GBR_SCO", "Population",  key_list("mortality.org")[1,2], 
                      key_get("mortality.org", key_list("mortality.org")[1,2]), fixup=FALSE) %>% 
  mutate(Age=as.numeric(Age), Age=if_else(is.na(Age), 110, Age)) %>% 
  filter(Year>=2000) %>% 
  gather(Sex, Ex, c("Male2", "Female2")) %>% 
  dplyr::select(c("Age", "Sex", "Year", "Ex")) %>% 
  mutate(Year=Year+1) %>% 
  spread(Year, Ex) %>% 
  mutate(Sex=if_else(Sex=="Male2", 1, 2))

#Add in 2024 population estimates from NRS
temp <- tempfile()
scotpop24url <- "https://www.nrscotland.gov.uk/media/txvdnee4/data-mid-year-population-estimates-2024.xlsx"
temp <- curl_download(url=scotpop24url, destfile=temp, quiet=FALSE, mode="wb")

scotpop24 <- read_excel(temp, sheet="Table 1", range="D6:CR7", col_names=FALSE) %>% 
  set_names(c("Sex", "All ages", as.character(c(0:90)))) %>% 
  dplyr::select(-`All ages`) %>% 
  gather(Age, Ex, c(`0`:`90`)) %>% 
  mutate(Year=2024, Sex=if_else(Sex=="Males", 1, 2))

scotpop_full <- scotpop %>% 
  merge(scotpop24 %>% dplyr::select(-Year) %>% 
          rename(`2024`=Ex), all.x=TRUE)

#Group populations to match deaths age groups
scotpop.grouped <- scotpop_full %>% 
  mutate(agestart=case_when(
    Age==0 ~ 0, Age<5 ~ 1, Age<10 ~ 5, Age<15 ~ 10, Age<20 ~ 15,
    Age<25 ~ 20, Age<30 ~ 25, Age<35 ~ 30, Age<40 ~ 35, Age<45 ~ 40, Age<50 ~ 45,
    Age<55 ~ 50, Age<60 ~ 55, Age<65 ~ 60, Age<70 ~ 65, Age<75 ~ 70, Age<80 ~ 75,
    Age<85 ~ 80, TRUE ~ 85)) %>% 
  group_by(Sex, agestart) %>%
  summarise(across(`2001`:`2024`, ~sum(.x, na.rm=TRUE))) %>% 
  ungroup()

rm(list=setdiff(ls(), c("scotdata.wide", "scotpop_full", "scotpop.grouped", "username", "password", 
                        "font", "theme_custom")))



###########
#Analysis#
##########

Raw_long <- scotdata.wide %>% gather(Year, Dx, c(`2001`:`2024`)) %>% 
  merge(scotpop.grouped %>% gather(Year, Ex, c(`2001`:`2024`))) %>% 
  mutate(Year=as.numeric(Year),
         Sex=if_else(Sex==1, "Male", "Female"),
         mx=Dx*100000/Ex)

#Derive age-standardised deaths for each cause/country
ASdata <- Raw_long %>% 
  mutate(stdpop=case_when(
    agestart==10 ~ 5500, agestart==15 ~ 5500, agestart==20 ~ 6000, agestart==25 ~ 6000, 
    agestart==30 ~ 6500, agestart==35 ~ 7000, agestart==40 ~ 7000, agestart==45 ~ 7000, 
    agestart==50 ~ 7000, agestart==55 ~ 6500, agestart==60 ~ 6000, agestart==65 ~ 5500, 
    agestart==70 ~ 5000, agestart==75 ~ 4000, agestart==80 ~ 2500, agestart==85 ~ 1500, 
    TRUE ~ 1000)) %>% 
  group_by(Cause, Sex, Year) %>% 
  summarise(Dx=sum(Dx), Ex=sum(Ex), 
            mx_std=weighted.mean(mx, stdpop), .groups="drop") 

ggplot(ASdata, aes(x=Year, y=mx_std, colour=Sex))+
  geom_line()+
  geom_hline(yintercept=0, colour="grey20")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate")+
  scale_colour_manual(values=c("#6600cc", "#00cc99"))+
  facet_wrap(~Cause, scales="free_y")+
  theme_custom()+
  theme(axis.line.x=element_blank(), panel.grid.major.y=element_line(colour="grey95"))

ggplot(ASdata %>% filter(Cause!="Total"), aes(x=Year, y=mx_std, colour=Cause))+
  geom_line()+
  geom_hline(yintercept=0, colour="grey20")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="Age-standardised mortality rate")+
  scale_colour_paletteer_d("LaCroixColoR::paired")+
  facet_wrap(~Sex)+
  theme_custom()+
  theme(axis.line.x=element_blank(), panel.grid.major.y=element_line(colour="grey95"))


#Apply smoothing based approach suggested by Tim Riffe
#Prediction models fall over if you include <10 year olds, so exclude them as not relevant to analysis
x <- seq(10,85, by=5)
mortdata <- scotdata.wide %>% filter(agestart>=10 & !Cause %in% c("Total", "Other external causes"))
y <- 2001:2024
z <- mortdata %>% 
  filter(agestart>=10) %>% 
  arrange(agestart)

#Fit smoothing models within years only
mx_smoothed1D <- data.frame(Cause=character(), Sex=integer(), Age=integer(),
                            Year=integer(), mx_smt1D=double())

for(i in unique(mortdata$Cause)){
  for(j in 1:2){
    for(k in 2001:2024){
      y <- z %>% filter(Cause==i & Sex==j) %>% 
        dplyr::select(-c(agestart, Sex, Cause, Age)) %>% 
        dplyr::select(c(k-2000)) %>% 
        unlist() %>% 
        as.vector()
      
      offset_i <- scotpop.grouped %>% filter(Sex==j & agestart>=10) %>% 
        dplyr::select(-c(agestart, Sex)) %>% 
        dplyr::select(c(k-2000)) %>% 
        log() %>% 
        unlist() %>% 
        as.vector()
      
      mod <- Mort1Dsmooth(x, y, offset=offset_i)
      
      mx_smoothed1D <- predict(mod, newdata=c(10:85)) %>% 
        exp() %>% 
        as.data.frame() %>% 
        rename(mx_smt1D=1) %>% 
        mutate(Age=c(10:85), Cause=i, Sex=j, Year=k) %>% 
        bind_rows(mx_smoothed1D)
    }
  }
}


Smoothed <- mx_smoothed1D %>% 
  merge(scotpop_full %>% gather(Year, pop, c(`2001`:`2024`)), all.x=TRUE) %>% 
  mutate(Dx_smt1D=mx_smt1D*pop, Sex=if_else(Sex==1, "Male", "Female")) %>% 
  rename(Pop=pop)

#Lexis surface
Smoothed %>% 
  filter(Age<80 & Cause %in% c("Alcohol", "Drugs", "Suicide", "Transport accidents")) %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~Cause)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()

Lex_Alc <- Smoothed %>% 
  filter(Age<80 & Cause=="Alcohol") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_Can <- Smoothed %>% 
  filter(Age<80 & Cause=="Cancer") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_CVD <- Smoothed %>% 
  filter(Age<80 & Cause=="Cardiovascular disease") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")


Lex_Dem <- Smoothed %>% 
  filter(Age<80 & Cause=="Dementia/Alzheimers") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_Drg <- Smoothed %>% 
  filter(Age<80 & Cause=="Drugs") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_Inf <- Smoothed %>% 
  filter(Age<80 & Cause=="Infectious diseases") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_Oth <- Smoothed %>% 
  filter(Age<80 & Cause=="Other") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_Res <- Smoothed %>% 
  filter(Age<80 & Cause=="Respiratory diseases") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_Scd <- Smoothed %>% 
  filter(Age<80 & Cause=="Suicide") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_RTA <- Smoothed %>% 
  filter(Age<80 & Cause=="Transport accidents") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(Sex~.)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  guides(fill="none")

Lex_Alc + Lex_Can + Lex_CVD + Lex_Dem + Lex_Drg + Lex_Inf + Lex_Oth + Lex_Res + Lex_RTA + Lex_Scd
plot_grid(Lex_Alc,Lex_Can, Lex_CVD, Lex_Dem, Lex_Drg, Lex_Inf, Lex_Oth, Lex_Res, Lex_RTA, Lex_Scd, nrow=1)
plot_grid(Lex_Alc, Lex_Drg, Lex_RTA, Lex_Scd, nrow=1)

agg_png("Outputs/TransportLexisScotland.png", units="in", width=7, height=6, res=800)
Smoothed %>% 
  filter(Age<80 & Cause=="Transport accidents") %>% 
  ggplot()+
  geom_raster(aes(x=Year, y=Age, fill=mx_smt1D*100000))+
  scale_x_continuous(breaks=c(2000,2010,2020))+
  scale_y_continuous(name="Age", breaks=c(10, 20, 30, 40, 50, 60, 70, 80))+
  scale_fill_paletteer_c("viridis::turbo", name="Deaths\nper 100,000")+
  facet_grid(~Sex)+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  coord_equal()+
  labs(title="Young men don't die in transport accidents any more",
       subtitle="Age-specific rates of deaths in transport accidents in Scotland 2001-2024",
       caption="Data from National Records of Scotland\nPlot by @VictimOfMaths")

dev.off()

###############################################
#Fit APC model using code from Acosta & van Raalte paper
#https://www.demographic-research.org/volumes/vol41/42/default.htm

### Most of this function is an adaptation of the apc.fit function from the Epi package (Carstensen 2019, https://CRAN.R-project.org/package=Epi)
curvature_dAPC <- function(amin=10, amax=80, pmin=1981, pmax=2023, gr=1, 
                           C = "Alcohol", S="Males"){
  amax <- amin - 1 + (floor((amax-amin+1)/gr))*gr
  pmax <- min(pmax, 2023)
  pmax <- pmin - 1 + (floor((pmax-pmin+1)/gr))*gr
  
  data0 <- Smoothed %>% 
    filter(Year>=pmin, Year<=pmax, Age>=amin, Age<=amax, Cause==C, Sex==S) %>% 
    dplyr::select(Age, Year, Pop, Dx_smt1D)
  
  data1 <- data0 %>% 
    mutate(a_gr=as.integer(amin+floor((Age-amin)/gr)*gr), 
           p_gr=as.integer(pmin+floor((Year-pmin)/gr)*gr))
  
  data2 <- data1 %>% 
    group_by(a_gr, p_gr) %>% 
    summarise(D=sum(Dx_smt1D), 
              Y=sum(Pop)) %>% 
    rename(A=a_gr, P=p_gr) 
  
  data <- data2 %>% 
    mutate(C = as.integer(P - A),
           D = round(D),
           D = ifelse(D < 1, 1, D),
           D = as.integer(D),
           Y = as.integer(Y))
  
  # rm(list=setdiff(ls(), "data"))
  # dplyr::select <- dplyr::select
  
  # ref.c <- NA
  # ref.p <- NA
  dist <- "poisson" 
  model<- "bSpline" 
  dr.extr <- "weighted"
  parm = "APC"
  npar=c(10,7,15)
  scale = 1
  alpha = 0.05 
  print.AOV = TRUE
  
  
  # dist <- match.arg(dist)
  # model <- match.arg(model)
  drtyp <- deparse(substitute(dr.extr))
  # parm <- toupper(match.arg(parm))
  if (!missing(data)) {
    if (length(match(c("A", "P", "D", "Y"), names(data))) != 
        4) 
      stop("Data frame ", deparse(substitute(data)), " has columns:\n", 
           names(data), "\nmust have variables:\n", "A (age), P (period), D (cases) and Y (person-time)")
    data <- data[, c("A", "P", "D", "Y")]
    data <- data[complete.cases(data), ]
    A <- data$A
    P <- data$P
    D <- data$D
    Y <- data$Y
  } else {
    nm <- logical(4)
    nm[1] <- missing(A)
    nm[2] <- missing(P)
    nm[3] <- missing(D)
    nm[4] <- missing(Y)
    if (any(nm)) 
      stop("Variable", if (sum(nm) > 1) 
        "s", paste(c(" A", " P", " D", " Y")[nm], collapse = ","), 
        " missing from input")
    if (diff(range(lv <- sapply(list(A = A, P = P, D = D, 
                                     Y = Y), length))) != 0) 
      stop("\nLengths of variables (", paste(paste(names(lv), 
                                                   lv, sep = ":"), collapse = ", "), ") are not the same.")
  }
  med <- function(x, y) {
    o <- order(x)
    a <- y[o]
    names(a) <- x[o]
    return(as.numeric(names(a[cumsum(a)/sum(a) > 0.5][1])))
  }
  p0 <- med(P, D)
  c0 <- med(P - A, D)
  ref.p <- F
  ref.c <- F
  if (is.list(npar) & length(npar) < 3) 
    stop("npar as a list should have length 3! \n")
  if (!is.list(npar) & length(npar) != 3) {
    npar <- rep(npar, 3)[1:3]
    names(npar) = c("A", "P", "C")
    cat("NOTE: npar is specified as:")
    print(npar)
  }
  if (is.null(names(npar))) 
    names(npar) <- c("A", "P", "C")
  lu <- paste(formatC(c(alpha/2, 1 - alpha/2) * 100, format = "f", 
                      digits = 1), "%", sep = "")
  proj.ip <- function(X, M, orth = FALSE, weight = rep(1, nrow(X))) {
    if (nrow(X) != length(weight)) 
      stop("Dimension of space and length of i.p. weights differ!")
    if (nrow(X) != nrow(M)) 
      stop("Dimension of space and rownumber of model matrix differ!")
    Pp <- solve(crossprod(X * sqrt(weight)), t(X * weight)) %*% 
      M
    PM <- X %*% Pp
    if (orth) 
      PM <- M - PM
    else PM
  }
  Thin.col <- function(X, tol = 0.000001) {
    QR <- qr(X, tol = tol, LAPACK = FALSE)
    X[, QR$pivot[seq(length = QR$rank)], drop = FALSE]
  }
  detrend <- function(M, t, weight = rep(1, nrow(M))) {
    Thin.col(proj.ip(cbind(1, t), M, orth = TRUE, weight = weight))
  }
  if (is.list(model)) {
    if (!all(sapply(model, is.function))) 
      stop("'model' is a list, but not all elements are functions as they should be.")
    if ((lmod <- length(model)) < 3) 
      stop("'model' is a list, with", lmod, "elements, it should have three.")
    if (is.null(names(model))) 
      names(model) <- c("A", "P", "C")
    MA <- model[["A"]](A)
    MP <- model[["P"]](P)
    MC <- model[["C"]](P - A)
    Rp <- model[["P"]](p0)
    Rc <- model[["C"]](c0)
  } else {
    if (model == "factor") {
      MA <- model.matrix(~factor(A) - 1)
      MP <- model.matrix(~factor(P) - 1)
      MC <- model.matrix(~factor(P - A) - 1)
      Rp <- MP[abs(P - p0) == min(abs(P - p0)), , drop = FALSE][1, 
      ]
      Rc <- MC[abs(P - A - c0) == min(abs(P - A - c0)), 
               , drop = FALSE][1, ]
    }
    if (model == "ns") {
      knl <- is.list(npar)
      if (!knl & length(npar) == 1) 
        npar <- rep(npar, 3)
      if (is.null(names(npar))) 
        names(npar) <- c("A", "P", "C")
      names(npar) <- toupper(substr(names(npar), 1, 1))
      MA <- if (knl) 
        Ns(A, knots = npar[["A"]])
      else Ns(A, knots = quantile(rep(A, D), probs = (1:npar["A"] - 
                                                        0.5)/npar["A"]))
      MP <- if (knl) 
        Ns(P, knots = npar[["P"]])
      else Ns(P, knots = quantile(rep(P, D), probs = (1:npar["P"] - 
                                                        0.5)/npar["P"]))
      MC <- if (knl) 
        Ns(P - A, knots = npar[["C"]])
      else Ns(P - A, knots = quantile(rep(P - A, D), probs = (1:npar["C"] - 
                                                                0.5)/npar["C"]))
      Rp <- ns(p0, knots = attr(MP, "knots"), Boundary.knots = attr(MP, 
                                                                    "Boundary.knots"))
      Rc <- ns(c0, knots = attr(MC, "knots"), Boundary.knots = attr(MC, 
                                                                    "Boundary.knots"))
      Knots <- list(Age = sort(c(attr(MA, "knots"), attr(MA, 
                                                         "Boundary.knots"))), Per = sort(c(attr(MP, "knots"), 
                                                                                           attr(MP, "Boundary.knots"))), Coh = sort(c(attr(MC, 
                                                                                                                                           "knots"), attr(MC, "Boundary.knots"))))
    }
    if (model %in% c("bSpline", "ls")) {
      deg <- switch(model, ls = 1, bSpline = 3)
      knl <- is.list(npar)
      if (knl) 
        nk <- sapply(npar, length)
      MA <- if (knl) 
        bSpline(A, knots = npar[["A"]][-c(1, nk[1])], Boundary.knots = npar[["A"]][c(1, 
                                                                                     nk[1])], degree = deg)
      else bSpline(A, df = npar[["A"]], degree = deg)
      MP <- if (knl) 
        bSpline(P, knots = npar[["P"]][-c(1, nk[2])], Boundary.knots = npar[["P"]][c(1, 
                                                                                     nk[2])], degree = deg)
      else bSpline(P, df = npar[["P"]], degree = deg)
      MC <- if (knl) 
        bSpline(P - A, knots = npar[["C"]][-c(1, nk[3])], 
                Boundary.knots = npar[["C"]][c(1, nk[3])], 
                degree = deg)
      else bSpline(P - A, df = npar[["C"]], degree = deg)
      Rp <- bSpline(p0, knots = attr(MP, "knots"), Boundary.knots = attr(MP, 
                                                                         "Boundary.knots"), degree = attr(MP, "degree"))
      Rc <- bSpline(c0, knots = attr(MC, "knots"), Boundary.knots = attr(MC, 
                                                                         "Boundary.knots"), degree = attr(MC, "degree"))
      Knots <- list(Age = sort(c(attr(MA, "knots"), attr(MA, 
                                                         "Boundary.knots"))), Per = sort(c(attr(MP, "knots"), 
                                                                                           attr(MP, "Boundary.knots"))), Coh = sort(c(attr(MC, 
                                                                                                                                           "knots"), attr(MC, "Boundary.knots"))))
    }
  }
  if (tolower(substr(dist, 1, 2)) == "po") {
    m.APC <- glm(D ~ MA + I(P - p0) + MP + MC, offset = log(Y), 
                 family = poisson)
    Dist <- "Poisson with log(Y) offset"
  }
  if (tolower(substr(dist, 1, 3)) %in% c("bin")) {
    m.APC <- glm(cbind(D, Y - D) ~ MA + I(P - p0) + MP + 
                   MC, family = binomial)
    Dist <- "Binomial regression (logistic) of D/Y"
  }
  m.AP <- update(m.APC, . ~ . - MC)
  m.AC <- update(m.APC, . ~ . - MP)
  m.Ad <- update(m.AP, . ~ . - MP)
  m.A <- update(m.Ad, . ~ . - I(P - p0))
  m.0 <- update(m.A, . ~ . - MA)
  AOV <- anova(m.A, m.Ad, m.AC, m.APC, m.AP, m.Ad, test = "Chisq")
  attr(AOV, "heading") <- "\nAnalysis of deviance for Age-Period-Cohort model\n"
  attr(AOV, "row.names") <- c("Age", "Age-drift", "Age-Cohort", 
                              "Age-Period-Cohort", "Age-Period", "Age-drift")
  A.pt <- unique(A)
  A.pos <- match(A.pt, A)
  P.pt <- unique(P)
  P.pos <- match(P.pt, P)
  C.pt <- unique(P - A)
  C.pos <- match(C.pt, P - A)
  MA <- cbind(1, MA)
  if (!mode(dr.extr) %in% c("character", "numeric")) 
    stop("\"dr.extr\" must be of mode \"character\" or \"numeric\".\n")
  if (is.character(dr.extr)) {
    wt <- rep(1, length(D))
    drtyp <- "1-weights"
    if (toupper(substr(dr.extr, 1, 1)) %in% c("T", "D")) {
      wt <- D
      drtyp <- "D-weights"
    }
    else if (toupper(substr(dr.extr, 1, 1)) %in% c("L", "R")) {
      wt <- (Y^2)/D
      drtyp <- "Y^2/D-weights"
    }
    else if (toupper(substr(dr.extr, 1, 1)) %in% c("Y")) {
      wt <- Y
      drtyp <- "Y-weights"
    }
    else if (dr.extr %in% names(data)) {
      wt <- data[, dr.extr]
      drtyp <- paste(dr.extr, "weights")
    }
  }
  if (is.numeric(dr.extr)) 
    wt <- dr.extr
  Rp <- matrix(Rp, nrow = 1)
  Rc <- matrix(Rc, nrow = 1)
  xP <- detrend(rbind(Rp, MP), c(p0, P), weight = c(0, wt))
  xC <- detrend(rbind(Rc, MC), c(c0, P - A), weight = c(0, 
                                                        wt))
  MPr <- xP[-1, , drop = FALSE] - ref.p * xP[rep(1, nrow(MP)), 
                                             , drop = FALSE]
  MCr <- xC[-1, , drop = FALSE] - ref.c * xC[rep(1, nrow(MC)), 
                                             , drop = FALSE]
  if (length(grep("-", parm)) == 0) {
    if (parm %in% c("ADPC", "ADCP", "APC", "ACP")) 
      m.APC <- update(m.0, . ~ . - 1 + MA + I(P - p0) + 
                        MPr + MCr)
    drift <- rbind(ci.exp(m.APC, subset = "I\\(", alpha = alpha), 
                   ci.exp(m.Ad, subset = "I\\(", alpha = alpha))
    rownames(drift) <- c(paste("APC (", drtyp, ")", sep = ""), 
                         "A-d")
    if (parm == "ADCP") 
      m.APC <- update(m.0, . ~ . - 1 + MA + I(P - A - c0) + 
                        MPr + MCr)
    if (parm == "APC") {
      MPr <- cbind(P - p0, MPr)
      m.APC <- update(m.0, . ~ . - 1 + MA + MPr + MCr)
    }
    if (parm == "ACP") {
      MCr <- cbind(P - A - c0, MCr)
      m.APC <- update(m.0, . ~ . - 1 + MA + MPr + MCr)
    }
    Age <- cbind(Age = A.pt, ci.exp(m.APC, subset = "MA", 
                                    ctr.mat = MA[A.pos, , drop = FALSE], alpha = alpha))[order(A.pt), 
                                    ]
    Per <- cbind(Per = P.pt, ci.exp(m.APC, subset = "MPr", 
                                    ctr.mat = MPr[P.pos, , drop = FALSE], alpha = alpha))[order(P.pt), 
                                    ]
    Coh <- cbind(Coh = C.pt, ci.exp(m.APC, subset = "MCr", 
                                    ctr.mat = MCr[C.pos, , drop = FALSE], alpha = alpha))[order(C.pt), 
                                    ]
    colnames(Age)[-1] <- c("Rate", lu)
    colnames(Per)[-1] <- c("P-RR", lu)
    colnames(Coh)[-1] <- c("C-RR", lu)
    Type <- paste("ML of APC-model", Dist, ": (", parm, "):\n")
    Model <- m.APC
  } else {
    adc <- update(m.0, . ~ . - 1 + MA + I(P - A - c0))
    adp <- update(m.0, . ~ . - 1 + MA + I(P - p0))
    drift <- ci.exp(adc, subset = "I\\(")
    rownames(drift) <- "A-d"
    xP <- cbind(1, P - p0, MPr)
    xC <- cbind(1, P - A - c0, MCr)
    lP <- cbind(P - p0, MPr)
    lC <- cbind(P - A - c0, MCr)
    if (parm == "AD-C-P") {
      rc <- update(m.0, . ~ . - 1 + xC, offset = predict(adc, 
                                                         type = "link"))
      rp <- update(m.0, . ~ . - 1 + xP, offset = predict(adc, 
                                                         type = "link"))
      A.eff <- ci.exp(adc, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(rc, subset = "xC", ctr.mat = xC[C.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(rp, subset = "xP", ctr.mat = xP[P.pos, 
      ], alpha = alpha)
      Model <- list(adc, rc, rp)
    } else if (parm == "AD-P-C") {
      rp <- update(m.0, . ~ . - 1 + xP, offset = predict(adp, 
                                                         type = "link"))
      rc <- update(m.0, . ~ . - 1 + xC, offset = predict(rp, 
                                                         type = "link"))
      A.eff <- ci.exp(adp, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(rp, subset = "xP", ctr.mat = xP[P.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(rc, subset = "xC", ctr.mat = xC[C.pos, 
      ], alpha = alpha)
      Model <- list(adp, rp, rc)
    } else if (parm == "AC-P") {
      ac <- update(m.0, . ~ . - 1 + MA + lC)
      rp <- update(m.0, . ~ . - 1 + xP, offset = predict(ac, 
                                                         type = "link"))
      A.eff <- ci.exp(ac, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(ac, subset = "lC", ctr.mat = lC[C.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(rp, subset = "xP", ctr.mat = xP[P.pos, 
      ], alpha = alpha)
      Model <- list(ac, rp)
    } else if (parm == "AP-C") {
      ap <- update(m.0, . ~ . - 1 + MA + lP)
      rc <- update(m.0, . ~ . - 1 + xC, offset = predict(ap, 
                                                         type = "link"))
      A.eff <- ci.exp(ap, subset = "MA", ctr.mat = MA[A.pos, 
      ], alpha = alpha)
      P.eff <- ci.exp(ap, subset = "lP", ctr.mat = lP[P.pos, 
      ], alpha = alpha)
      C.eff <- ci.exp(rc, subset = "xC", ctr.mat = xC[C.pos, 
      ], alpha = alpha)
      Model <- list(ap, rc)
    }
    Age <- cbind(Age = A.pt, A.eff)[order(A.pt), ]
    Per <- cbind(Per = P.pt, P.eff)[order(P.pt), ]
    Coh <- cbind(Cph = C.pt, C.eff)[order(C.pt), ]
    colnames(Age)[-1] <- c("A.eff", lu)
    colnames(Per)[-1] <- c("P.eff", lu)
    colnames(Coh)[-1] <- c("C.eff", lu)
    Type <- paste("Sequential modelling", Dist, ": (", parm, 
                  "):\n")
  }
  res <- list(Type = Type, Model = Model, Age = Age, Per = Per, 
              Coh = Coh, Drift = drift, Ref = c(Per = if (parm %in% 
                                                          c("APC", "ADPC", "Ad-P-C", "AP-C")) p0 else NA, Coh = if (parm %in% 
                                                                                                                    c("ACP", "ADCP", "Ad-C-P", "AC-P")) c0 else NA), 
              Anova = AOV)
  if (model %in% c("ns", "bSpline")) 
    res <- c(res, list(Knots = Knots))
  res$Age[, -1] <- res$Age[, -1] * scale
  if (print.AOV) {
    print(res$Type)
    print(res$Anova)
  }
  if (!ref.p & parm %in% c("APC", "ADPC")) 
    cat("No reference period given:\n", "Reference period for age-effects is chosen as\n", 
        "the median date of event: ", p0, ".\n")
  if (!ref.c & parm %in% c("ACP", "ADCP")) 
    cat("No reference period given:\n", "Reference period for age-effects is chosen as\n", 
        "the median date of birth for persons  with event: ", 
        c0, ".\n")
  class(res) <- "apc"
  invisible(res)
  
  # MCr backup
  MCr2 <- MCr
  MCr <- MCr2
  data2 <- data %>%
    add_predictions(m.APC, var = "log_d_spl") %>% 
    mutate(d_spl = exp(log_d_spl))
  
  # Fiting with cohort effects set to zero
  MCr <- MCr * 0
  data3 <- data2 %>% 
    add_predictions(m.APC, var = "log_d_ap_spl") %>% 
    mutate(d_ap_spl = exp(log_d_ap_spl))
  
  ####
  #### (smoothed observed mortality) - (AP fitting from a full APC model)
  ####
  
  mtx <- data %>% 
    arrange(P, A)
  ylist <- unique(mtx$P) %>% sort()
  alist <- unique(mtx$A) %>% sort()
  deaths <- matrix(mtx$D, nrow=length(alist), ncol=length(ylist), byrow=F)
  colnames(deaths) <- ylist
  rownames(deaths) <- alist
  # population at risk (population/exposure)
  exposure <- matrix(mtx$Y, nrow=length(alist), ncol=length(ylist), byrow=F)
  colnames(exposure) <- ylist
  rownames(exposure) <- alist
  # smoothing mortality
  fit <- Mort2Dsmooth(x = alist, y = ylist, Z = deaths, offset = log(exposure),
                      overdispersion = TRUE, method = 2)
  summary(fit)
  mx.smooth <- (exp(fit[26]$logmortality))*100000
  d_smth <- (exp(fit[26]$logmortality))*exposure
  
  
  smt <- d_smth %>%
    as_tibble() %>% 
    mutate(A = alist) %>% 
    gather(-A, key = P, value = d_sth) %>% 
    mutate(A = as.integer(A),
           P = as.integer(P))
  
  data4 <- data3 %>% 
    left_join(smt) %>% 
    dplyr::select(-log_d_spl, -log_d_ap_spl) %>% 
    rename(Age = A,
           Year = P,
           Dx_smt = D,
           Pop = Y) %>% 
    mutate(Cause = C, Sex=S)
  
}

#################
#Fit models
curv_drg_m <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Drugs", S="Male")
curv_drg_f <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Drugs", S="Female")
curv_scd_m <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Suicide", S="Male")
curv_scd_f <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Suicide", S="Female")
curv_alc_m <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Alcohol", S="Male")
curv_alc_f <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Alcohol", S="Female")
curv_rta_m <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Transport accidents", S="Male")
curv_rta_f <- curvature_dAPC(amin=10, amax=85, pmin=2001, pmax=2024, gr=1, 
                             C = "Transport accidents", S="Female")

curvs <- bind_rows(curv_drg_m, curv_drg_f, curv_scd_m,
                   curv_scd_f, curv_alc_m, curv_alc_f,
                   curv_rta_m, curv_rta_f)

db <- curvs %>% 
  mutate(mx = 100000 * Dx_smt / Pop,
         mx_sth = 100000 * d_sth / Pop,
         mx_spl = 100000 * d_spl / Pop,
         mx_ap_spl = 100000 * d_ap_spl / Pop,
         cohort = Year - Age,
         mx_exc = mx_sth - mx_ap_spl,
         rr = mx_sth / mx_ap_spl) 

agg_png("Outputs/APCCurvatureExcess.png", units="in", width=6, height=6, res=800)
ggplot(db, aes(x=Year, y=Age, fill=mx_exc))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70))+
  scale_fill_paletteer_c("scico::roma", direction=-1,
                         limits=c(-max(abs(db$mx_exc)), max(abs(db$mx_exc))),
                         name="Excess mortality\nPer 100,000")+
  facet_grid(Sex~Cause)+
  theme_custom()+
  coord_equal()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

dev.off()

agg_png("Outputs/APCCurvatureExcess.png", units="in", width=6, height=6, res=800)
ggplot(db %>% filter(Cause=="Transport accidents"), aes(x=Year, y=Age, fill=mx_exc))+
  geom_tile()+
  scale_x_continuous(name="")+
  scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70))+
  scale_fill_paletteer_c("scico::roma", direction=-1,
                         limits=c(-12, 12),
                         name="Excess mortality\nPer 100,000")+
  facet_grid(~Sex)+
  theme_custom()+
  coord_equal()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

dev.off()


APCcurvs <- db %>% 
  filter(Age<=75) %>% 
  group_by(Year, Cause, Sex) %>% 
  summarise(Age=Age[mx_exc==max(mx_exc)], mx_exc=max(mx_exc), 
            rr=max(rr), .groups="drop")

ann_text <- data.frame(Age=seq(20, 77, by=5), Year=rep(2027.5, times=12), 
                       label=as.character(seq(2005, 1950, by=-5)))

agg_png("Outputs/APCCurvaturePlotFull.png", units="in", width=9, height=7, res=700)
ggplot(APCcurvs, aes(x=Year, y=Age))+
  geom_point(aes(fill=Cause, size=rr), shape=21, colour="black", alpha=0.7)+
  scale_x_continuous(name="")+
  scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70))+
  scale_fill_manual(values=c("#00A1FF", "#E69F00", "#CC5395", "#a1d99b"), name="Cause")+
  scale_size_continuous(name="Relative Risk")+
  geom_vline(xintercept = seq(2000, 2026, by=5), linetype="dashed", 
             color="grey30", linewidth=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(15, 75, by=5), linetype="dashed", color="grey30", 
             linewidth=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-2010, -1860, by=5), slope = 1, linetype="dashed", 
              color="grey30", linewidth=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2026, y = 13.5, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  facet_grid(~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="APC Curvature plot of alcohol, suicide and drug deaths",
       subtitle="Modal age of greatest excess mortality vs. a counterfactual assuming no cohort trends\n",
       caption="Data from National Records of Scotland\nPlot concept by Kike Acosta & Alyson van Raalte\nCreated by @VictimOfMaths")

dev.off()

agg_png("Outputs/APCCurvaturePlot2000.png", units="in", width=8, height=7, res=700)
ggplot(APCcurvs %>% filter(Year>=2000), aes(x=Year, y=Age))+
  geom_point(aes(fill=Cause, size=rr), shape=21, colour="black", alpha=0.7)+
  scale_x_continuous(name="", limits=c(2000, 2028), breaks=c(2000, 2010, 2020))+
  scale_y_continuous(breaks=c(20, 30, 40, 50, 60, 70))+
  scale_fill_manual(values=c("#00A1FF", "#E69F00", "#CC5395", "#a1d99b"), name="Cause")+
  scale_size_continuous(name="Relative Risk")+
  geom_vline(xintercept = seq(2000, 2025, by=5), linetype="dashed", 
             color="grey30", linewidth=.10, alpha = 0.8) +
  geom_hline(yintercept = seq(15, 75, by=5), linetype="dashed", color="grey30", 
             linewidth=.10, alpha = 0.8) +
  geom_abline(intercept = seq(-2010, -1860, by=5), slope = 1, linetype="dashed", 
              color="grey30", linewidth=.10, alpha = 0.8)+
  geom_text(data = ann_text,
            aes(label = label), size=3, angle=45, alpha=0.7, family=font)+
  annotate("text", x = 2025, y = 13.5, label = "Cohort", size=3.2, angle = 45, color="black",
           family=font)+
  facet_grid(~Sex)+
  coord_equal()+
  theme_custom()+
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))+
  labs(title="APC Curvature plot of alcohol, suicide and drug deaths",
       subtitle="Modal age of greatest excess mortality vs. a counterfactual assuming no cohort trends\n",
       caption="Data from National Records of Scotland\nPlot concept by Kike Acosta & Alyson van Raalte\nCreated by @VictimOfMaths")

dev.off()

