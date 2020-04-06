rm(list=ls())

library(tidyverse)
library(paletteer)
library(curl)
library(readxl)
library(sf)
library(gtools)
library(cowplot)

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

#calculate mean mortality rates by decile and overall (population weighted)
data_LSOA <- data_LSOA %>%
  group_by(decile) %>%
  mutate(decilemean = weighted.mean(mortrate, pop))

data_LSOA <- ungroup(data_LSOA)

data_LSOA <- data_LSOA %>%
  mutate(popmean = weighted.mean(mortrate, pop))

#Download shapefile of LSOA boundaries
temp <- tempfile()
temp2 <- tempfile()
source <- "https://opendata.arcgis.com/datasets/e886f1cd40654e6b94d970ecf437b7b5_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)
shapefile <- st_read(file.path(temp2,"Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC.shp"))
names(shapefile)[names(shapefile) == "LSOA11CD"] <- "code"

map.data <- full_join(shapefile, data_LSOA, by="code")

#Bring in Local Authorities (LADs)
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LSOAtoLAD <- read.csv(temp)[,c(4,10,11)]
colnames(LSOAtoLAD) <- c("code", "LAcode", "LAname")
  
map.data <- full_join(map.data, LSOAtoLAD, by="code")

#Bring in Regions
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("LAcode", "Region")

map.data <- full_join(map.data, LADtoRegion, by="LAcode")

#Remove Scottish Data Zones which have snuck into the data
map.data <- subset(map.data, substr(code, 1,1)=="E")

#Bivariate map
#tertile the IMD and mortrate variables
#generate tertiles
map.data$IMDtert <- quantcut(-map.data$rank, q=3, labels=FALSE)
map.data$morttert <- quantcut(map.data$mortrate, q=3, labels=FALSE)

#Generate key
keydata <- data.frame(IMDtert=c(1,1,1,2,2,2,3,3,3), morttert=c(1,2,3,1,2,3,1,2,3),
                      RGB=c("#e8e8e8","#ace4e4","#5ac8c8","#dfb0d6","#a5add3",
                               "#5698b9","#be64ac","#8c62aa","#3b4994"))

#Bring colours into main data for plotting
map.data <- left_join(map.data, keydata, by=c("IMDtert", "morttert"))

#strip out a few unnecessary columns for tidiness
map.data <- map.data[,-c(1,3:6)]

#Plot for Sheffield 
plot <- ggplot(subset(map.data, LAname=="Sheffield"), aes(fill=RGB, geometry=geometry))+
  geom_sf()+
  theme_classic()+
  scale_fill_identity()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title="Mapping potential COVID-19 risk across Sheffield",
       subtitle="LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption="Population data from ONS, CFRs from Istituto Superiore di Sanità\nPlot by @VictimOfMaths")+
  annotate("text", x=-1.38, y=53.45, label="High deprivation,\nyoung population", size=3)+
  annotate("text", x=-1.34, y=53.38, label="High deprivation,\nold population", size=3)+
  annotate("text", x=-1.75, y=53.4, label="Low deprivation,\nold population", size=3)+
  geom_curve(aes(x=-1.38, y=53.44, xend=-1.4, yend=53.42), curvature=-0.15)+
  geom_curve(aes(x=-1.345, y=53.37, xend=-1.36, yend=53.355), curvature=-0.15)+
  geom_curve(aes(x=-1.725, y=53.4, xend=-1.62, yend=53.36), curvature=0.15)

key <- ggplot(keydata)+
  geom_tile(aes(x=morttert, y=IMDtert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater age-based COVID-19 risk" %->%  ""),
       y = expression("Greter health deprivation" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 8),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank())+
  # quadratic tiles
  coord_fixed()

tiff("Outputs/COVIDBivariateSheff.tiff", units="in", width=12, height=8, res=300)
ggdraw()+
  draw_plot(plot, 0,0,1,1)+
  draw_plot(key, 0.03,0.03,0.3,0.3)
dev.off()

#Editable plot: just change the LA name for any other English Lower Tier LA, 
#or select a group of LAs with LAname %in% c("x", "y"...) 
#Don't forget to update the title though
ggplot(subset(map.data, LAname=="Sheffield"), aes(fill=RGB, geometry=geometry))+
  geom_sf()+
  theme_classic()+
  scale_fill_identity()+
  theme(axis.line=element_blank(), axis.ticks=element_blank(), axis.text=element_blank(),
        axis.title=element_blank(),  plot.title=element_text(face="bold"))+
  labs(title="Mapping potential COVID-19 risk across Sheffield",
       subtitle="LSOA-level health deprivation and potential COVID-19 mortality risk based on age-sex structure of population",
       caption="Population data from ONS, CFRs from Istituto Superiore di Sanità\nPlot by @VictimOfMaths")
