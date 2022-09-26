rm(list=ls())

library(curl)
library(readxl)
library(readODS)
library(tidyverse)
library(paletteer)
library(ragg)
library(extrafont)
library(sf)
library(forcats)
library(scales)

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

#Download alcohol license data from DfC website
temp <- tempfile()
url <- "https://www.communities-ni.gov.uk/system/files/publications/communities/dfc-liquor-licences-northern-ireland-2021-tables.xlsx"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Exctract DEA-level data on pub licenses
raw1 <- read_excel(temp, sheet="Table 2", range="A4:D11")
raw2 <- read_excel(temp, sheet="Table 3", range="A4:D11")
raw3 <- read_excel(temp, sheet="Table 4", range="A4:D11")
raw4 <- read_excel(temp, sheet="Table 5", range="A4:D14")
raw5 <- read_excel(temp, sheet="Table 6", range="A4:D11")
raw6 <- read_excel(temp, sheet="Table 7", range="A4:D11")
raw7 <- read_excel(temp, sheet="Table 8", range="A4:D11")
raw8 <- read_excel(temp, sheet="Table 9", range="A4:D11")
raw9 <- read_excel(temp, sheet="Table 10", range="A4:D11")
raw10 <- read_excel(temp, sheet="Table 11", range="A4:D11")
raw11 <- read_excel(temp, sheet="Table 12", range="A4:D11")

pubdata <- bind_rows(raw1, raw2, raw3, raw4, raw5, raw6, raw7, raw8, raw9, raw10, raw11) %>% 
  set_names("DEA", "Licenses", "Pop", "LicenseDensity")

#DOwnload 2011 census data on religiousness
url2 <- "https://www.ninis2.nisra.gov.uk/Download/Census%202011/CT0328NI.ods"
temp <- curl_download(url=url2, destfile=temp, quiet=FALSE, mode="wb")

#Tidy up (headers get all wonky)
reldata <- read_ods(temp, sheet="DEA2014", range="A7:AG87") %>% 
  set_names("Name", "Code", "Pop", paste(rep(c("White", "Traveller", "Asian", "Black",
                                               "Mixed", "Other"), each=5), 
                                         rep(c("Total", "Catholic", "Protestant", "Other", 
                                               "None"), times=6), sep="_")) %>% 
  
  mutate(Catholic=rowSums(across(ends_with("Catholic"))),
         Protestant=rowSums(across(ends_with("Protestant"))),
         Cathprop=Catholic/Pop, Protprop=Protestant/Pop) %>% 
  select(Name, Code, Cathprop, Protprop)

data <- pubdata %>% 
  mutate(Name=toupper(DEA)) %>% 
  merge(reldata, all=T)

agg_tiff("Outputs/NIPubsxReligionScatter.tiff", units="in", width=9, height=6, res=500)
ggplot(data, aes(x=LicenseDensity, y=Cathprop))+
  geom_point(shape=21, colour="Purple")+
  scale_x_continuous(name="Pub licenses per 10,000 population\n")+
  scale_y_continuous(name="Proportion of population identifying as Catholic",
                     limits=c(0,1), labels=label_percent(accuracy=1))+
  theme_custom()+
  labs(title="Majority Catholic areas in Northern Ireland seem to have more pubs",
       subtitle="Number of liquor licenses in place in 2021 for public houses vs. proportion of population identifying as Catholic\nin the 2011 census by District Electoral Area",
       caption="Data from NI Department for Communities | Plot by @VictimOfMaths")
dev.off()

#Download shapefile
mapurl <- "https://osni-spatialni.opendata.arcgis.com/datasets/spatialni::osni-open-data-largescale-boundaries-district-electoral-areas-2012.zip?outSR=%7B%22latestWkid%22%3A29902%2C%22wkid%22%3A29900%7D"
tempshape3 <- tempfile()
tempshape4 <- tempfile()
tempshape3 <- curl_download(url=mapurl, destfile=tempshape3, quiet=FALSE, mode="wb")
unzip(zipfile=tempshape3, exdir=tempshape4)

#The actual shapefile has a different name each time you download it, so need to fish the name out of the unzipped file
name2 <- list.files(tempshape4, pattern=".shp")[1]
shapefile <- st_read(file.path(tempshape4, name2))
  
mapdata <- left_join(shapefile, data, by=c("FinalR_DEA"="DEA"))

agg_tiff("Outputs/NIPubsMap.tiff", units="in", width=9, height=6, res=500)
ggplot(mapdata, aes(geometry=geometry, fill=LicenseDensity))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("viridis::mako", direction=-1, name="Pubs\nPer 10,000")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                margin=margin(0,0,5.5,0)),
        text=element_text(family="Lato"),
        plot.subtitle=element_text(colour="Grey40", hjust=0, vjust=1),
        plot.caption=element_text(colour="Grey40", hjust=1, vjust=1, size=rel(0.8)),
        legend.text=element_text(colour="Grey40"),
        legend.title=element_text(colour="Grey20"))+
  labs(title="\nDerry/Londonderry has *a lot* of pubs",
       subtitle="Density of liquor licenses for public houses in 2021 by District Electoral Area",
       caption="Data from NI Department of COmmunities | Plot by @VictimOfMaths")
dev.off()

ggplot(mapdata, aes(geometry=geometry, fill=Cathprop))+
  geom_sf(colour=NA)+
  scale_fill_paletteer_c("pals::ocean.algae", direction=-1, 
                         name="Population\nidentifying\nas Catholic",
                         labels=label_percent(accuracy=1), limits=c(0,1))+
  theme_void()+
  theme(text=element_text(family="Lato"))


  