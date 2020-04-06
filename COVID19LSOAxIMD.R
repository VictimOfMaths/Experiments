rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(sf)
library(gtools)

#Read in 2018 mid-year population estimates at LSOA level by sex and single year of age
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimates%2fmid2018sape21dt1a/sape21dt1amid2018on2019lalsoasyoaestimatesformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
data_m <- read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                   sheet="Mid-2018 Males", range="A5:CQ35097", col_names=TRUE)
data_f <- read_excel(file.path(temp2, "SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 
                     sheet="Mid-2018 Females", range="A5:CQ35097", col_names=TRUE)

#Merge sex-specific data
data_m$sex <- "Male"
data_f$sex <- "Female"
data <- rbind(data_m, data_f)

#Collapse into age bands matching CFR data
data$`0-9` <- rowSums(data[,c(5:14)])
data$`10-19` <- rowSums(data[,c(15:24)])
data$`20-29` <- rowSums(data[,c(25:34)])
data$`30-39` <- rowSums(data[,c(35:44)])
data$`40-49` <- rowSums(data[,c(45:54)])
data$`50-59` <- rowSums(data[,c(55:64)])
data$`60-69` <- rowSums(data[,c(65:74)])
data$`70-79` <- rowSums(data[,c(75:84)])
data$`80-89` <- rowSums(data[,c(85:94)])

data <- data[,c(1:3, 96:105, 95)]

data_long <- gather(data, age, pop, c(5:14))

# CFR Italian 26 March
# https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_26-marzo%202020.pdf
cfr <-  tibble::tribble(
  ~age, ~b, ~m, ~f,
  "0-9",      0.0001,     0.0001,       0.0001,
  "10-19",      0.0001,     0.0001,       0.0001,
  "20-29",      0.0001,     0.0001,       0.0001,
  "30-39",    0.3,   0.6,     0.1,
  "40-49",    0.7,   1.1,     0.4,
  "50-59",    1.7,   2.4,     0.8,
  "60-69",    5.7,   6.9,     3.5,
  "70-79",   16.9,  19.8,    11.6,
  "80-89",   24.6,  29.2,    18.9,
  "90+",     24,  30.8,    20.4
) 

#Merge into population data
data_long <- merge(data_long,cfr, all.x=TRUE)

#Calculate expected deaths with 100% inflection by age group
data_long$ex_deaths <- case_when(
  data_long$sex=="Male" ~ data_long$pop*data_long$m/100,
  data_long$sex=="Female" ~ data_long$pop*data_long$f/100
)

#Summarise by LSOA
data_LSOA <- data_long %>% 
  group_by(`Area Codes`) %>% 
  summarise(name=unique(LSOA), pop=sum(pop), ex_deaths=sum(ex_deaths))

data_LSOA$mortrate <- data_LSOA$ex_deaths*100000/data_LSOA$pop

#Separate out LA-level data
data_LA <- subset(data_LSOA, is.na(name))

#Remove from LSOA-level data
data_LSOA <- subset(data_LSOA, !is.na(name))

#Bring in 2019 IMD data (England only)
temp <- tempfile()
source <- "https://opendatacommunities.org/downloads/cube-table?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
IMD <- read.csv(temp)
#IMD <- subset(IMD, (Measurement=="Decile " | Measurement=="Rank") & Indices.of.Deprivation=="a. Index of Multiple Deprivation (IMD)")
IMD <- subset(IMD, (Measurement=="Decile " | Measurement=="Rank") & Indices.of.Deprivation=="e. Health Deprivation and Disability Domain")
IMD_wide <- spread(IMD, Measurement, Value)
data_LSOA <- merge(data_LSOA, IMD_wide[,c(1,5,6)], by.x="Area Codes", by.y="FeatureCode", all.x=TRUE )
colnames(data_LSOA) <- c("code", "name", "pop", "ex_deaths", "mortrate", "decile", "rank")

#Rank LSOAs within each decile
data_LSOA <- data_LSOA %>%
  group_by(decile) %>%
  mutate(decile_rank = order(order(mortrate, decreasing=FALSE)))

tiff("Outputs/COVIDMortDepGrid.tiff", units="in", width=15, height=5, res=300)
ggplot(subset(data_LSOA, !is.na(decile)), aes(y=as.factor(decile), x=decile_rank, fill=mortrate))+
  geom_tile()+
  theme_classic()+
  scale_fill_paletteer_c("viridis::magma", direction=-1,name="Potential deaths\nper 100,000")+
  scale_y_discrete(name="Health deprivation & disability", labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", 
                                               "8", "9", "10 - least deprived"))+
  scale_x_continuous(name="")+
  theme(axis.text.x=element_blank(), axis.line.x=element_blank(), axis.ticks.x=element_blank())+
  labs(title="Maximum potential exposure to COVID-19 mortality by health deprivation",
       subtitle="Calculated using LSOA-level population age/sex distribution and observed Case Fatality Rates from Italy, assuming 100% COVID-19 prevalence",
       caption="Population data from ONS, CFRs from Istituto Superiore di Sanità\nPlot by @VictimOfMaths")
dev.off()

#calculate mean mortality rates by decile and overall (population weighted)
data_LSOA <- data_LSOA %>%
  group_by(decile) %>%
  mutate(decilemean = weighted.mean(mortrate, pop))

data_LSOA <- ungroup(data_LSOA)

data_LSOA <- data_LSOA %>%
  mutate(popmean = weighted.mean(mortrate, pop))


  tiff("Outputs/COVIDMortDepScatter.tiff", units="in", width=12, height=8, res=300)
  ggplot(subset(data_LSOA, !is.na(decile)), aes(x=mortrate, y=as.factor(decile), colour=mortrate))+
    geom_jitter(shape=21, alpha=0.6, show.legend=FALSE)+
    geom_segment(aes(x=popmean, xend=popmean, y=Inf, yend=-Inf), colour="Grey20")+
    geom_point(aes(x=decilemean, y=as.factor(decile)), colour="Grey20", fill="Cyan", shape=23, size=2)+
    scale_colour_paletteer_c("viridis::magma", direction=-1)+
    scale_x_continuous(name="Potential deaths per 100,000")+
    scale_y_discrete(name="Health deprivation & disability", labels=c("1 - most deprived", "2", "3", "4", "5", "6", "7", 
                                                                      "8", "9", "10 - least deprived"))+  
    theme_classic()+
    labs(title="Maximum potential exposure to COVID-19 mortality by health deprivation",
         subtitle="Calculated using LSOA-level population age/sex distribution and observed Case Fatality Rates from Italy, assuming 100% COVID-19 prevalence",
         caption="Population data from ONS, CFRs from Istituto Superiore di Sanità\nPlot by @VictimOfMaths")+
    annotate("text", x=8000, y=8.51, label="Each circle = 1 LSOA", size=3)+
    annotate("text", x=4700, y=6.5, label="Population average", size=3)+
    annotate("text", x=2500, y=3.5, label="Decile average", size=3)+
    geom_segment(aes(x=3370, y=6.5,  xend=4050, yend=6.5), colour="Grey20")+
    geom_segment(aes(x=2600, y=3.55,  xend=3070, yend=3.91), colour="Grey20")
  dev.off()
