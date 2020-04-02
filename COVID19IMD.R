rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(paletteer)

#To do:
#sort out dates
#make code dynamic to handle new days data automatically
#tidy up deciles graph
#sort out log scale for y-axes

#Read in COVID dase data
temp <- tempfile()
source <- "https://fingertips.phe.org.uk/documents/Historic%20COVID-19%20Dashboard%20Data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- read_excel(temp, sheet="UTLAs", range="A9:Z160", col_names=TRUE)
#colnames(data)[c(3:26)] <- as.Date(seq(43899:43922), origin="1899-12-30")
colnames(data) <- c("code", "name", 1:24)

data_long <- gather(data, date, cases, c(3:26))
data_long$date <- as.integer(data_long$date) 

#Read in IMD data
temp <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/834001/File_11_-_IoD2019_Local_Authority_District_Summaries__upper-tier__.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
IMDdata <- read_excel(temp, sheet="IMD", range="A1:D152", col_names=TRUE)
colnames(IMDdata) <-  c("code", "name", "IMDrank", "IMDorder")
IMDdata$decile <- ntile(IMDdata$IMDrank, 10)
IMDdata$quintile <- ntile(IMDdata$IMDrank, 5)

#Combine
data_long <- merge(data_long, IMDdata, by="code")

#Collapse to quintiles
quintiles <- data_long %>%
  group_by(quintile, date) %>%
  summarise(cases=sum(cases))

tiff("Outputs/COVIDQuintiles.tiff", units="in", res=300, width=8, height=6)
ggplot(quintiles, aes(x=date, y=cases, colour=as.factor(quintile)))+
  geom_line()+
  theme_classic()+
  scale_colour_manual(values=c("#fcc5c0", "#fa9fb5", "#f768a1", "#c51b8a", "#7a0177"), name="IMD quintile",
                      labels=c("Q1 (least deprived)", "Q2", "Q3", "Q4", "Q5 (most deprived)"))+
  scale_x_continuous(name="Date", breaks=c(1,6,11,16,21), labels=c("9th Mar", "14th Mar", "19th Mar", "24th Mar", "29th Mar"))+
  scale_y_continuous(name="Cumulative known cases")+
  labs(title="Cumulative COVID-19 cases by deprivation quintile", subtitle="Deprivation estimated using mean IMD level across each UTLA",
       caption="Case data from PHE | IMD2019 data from DHCLG | Plot by @VictimOfMaths")
dev.off()

#Collapse to deciles
deciles <- data_long %>%
  group_by(decile, date) %>%
  summarise(cases=sum(cases))

ggplot(deciles, aes(x=date, y=cases, colour=as.factor(decile)))+
  geom_line()+
  theme_classic()+
  scale_colour_paletteer_d("RColorBrewer::Spectral")
