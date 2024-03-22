rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)
library(extrafont)
library(ragg)
library(DescTools)

#read in transaction-level LCFS data inflated to 2022 prices
data<-read.csv("X:/HAR_PR/PR/LCFS/Data/DEFRA/Processing/LCFS_transactions_processed.csv")

#read in and tidy up off-trade price distributions from MESAS/Nielsen
temp <- tempfile()
url <- "https://www.publichealthscotland.scot/media/13690/mesas-monitoring-report-2022-alcohol-price-and-affordability.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

MESAS <- read_excel(temp, sheet="Scotland 2021", range="A3:R54") %>% 
  gather(priceband, vol, c(2:18)) %>% 
  spread(`Pence per unit`, vol) %>% 
  mutate(Wine=`WINE (all)`+`FORTIFIED WINES`) %>% 
  select(priceband, "BEERS (all)*", "CIDER (all)", "Wine", "SPIRITS (all)", "RTDs") %>% 
  set_names("priceband", "Beer", "Cider", "Wine", "Spirits", "RTDs") %>% 
  mutate(priceband=factor(priceband, levels=c("up to 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29", "30 - 34",
                                              "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
                                              "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85up"))) %>% 
  arrange(priceband) %>% 
  mutate(across(c(2:6), ~cumsum(.x/sum(.x)))) %>% 
  gather(bevname, cumperc, c(2:6))

Caldata <- data %>% 
  #filter(gor_name=="Scotland") %>% 
  filter(channel=="Off-trade") %>% 
  group_by(bevname) %>% 
  arrange(ppu_inf) %>% 
  mutate(cumperc=cumsum(wgt/sum(wgt))) %>% 
  ungroup()

minprice <- min(Caldata$ppu_inf)
maxprice <- max(Caldata$ppu_inf)

Merged <- Caldata %>% 
  #generate transaction level if
  group_by(uniqueid) %>% 
  mutate(n=row_number()) %>% 
  ungroup() %>% 
  mutate(newid=paste0(uniqueid, n)) %>% 
  merge(MESAS, all.x=TRUE, by="bevname") %>%
  mutate(diff=abs(cumperc.x-cumperc.y)) %>% 
  group_by(newid) %>% 
  arrange(diff) %>% 
  slice(1) %>% 
  ungroup()
  
  
  
  left_join(MESAS, by=join_by(bevname, closest(cumperc<=cumperc))) %>% 
  mutate(pricebandval=case_when(
    priceband=="up to 9" ~ 7.5, priceband=="10 - 14" ~ 12.5,
    priceband=="15 - 19" ~ 17.5, priceband=="20 - 24" ~ 22.5,
    priceband=="25 - 29" ~ 27.5, priceband=="30 - 34" ~ 32.5,
    priceband=="35 - 39" ~ 37.5, priceband=="40 - 44" ~ 42.5,
    priceband=="45 - 49" ~ 47.5, priceband=="50 - 54" ~ 52.5,
    priceband=="55 - 59" ~ 57.5, priceband=="60 - 64" ~ 62.5,
    priceband=="65 - 69" ~ 67.5, priceband=="70 - 74" ~ 72.5,
    priceband=="75 - 79" ~ 77.5, priceband=="80 - 84" ~ 82.5,
    TRUE ~ 87.5)) %>% 
  mutate(ppu_adj=case_when(
    pricebandval==7.5 ~ minprice+(12.5-minprice)*ppu_inf*
  ))


