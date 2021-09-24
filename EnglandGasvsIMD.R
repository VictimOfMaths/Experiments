rm(list=ls())

library(tidyverse)
library(extrafont)
library(ggtext)
library(readxl)
library(ragg)
library(sf)
library(ggridges)
library(curl)
library(gtools)
library(cowplot)


theme_custom <- function() {
  theme_classic() %+replace%
    theme(plot.title.position="plot", plot.caption.position="plot",
          strip.background=element_blank(), strip.text=element_text(face="bold", size=rel(1)),
          plot.title=element_text(face="bold", size=rel(1.5), hjust=0,
                                  margin=margin(0,0,5.5,0)),
          text=element_text(family="Lato"))
}

#Download LSOA level gas consumption for England from gov.uk
#https://www.gov.uk/government/statistics/lower-and-middle-super-output-areas-gas-consumption
temp <- tempfile()
temp2 <- tempfile()
source <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/946694/LSOA_Gas_csv.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

gasdata <- read.csv(file.path(temp2, "LSOA Gas csv/LSOA_GAS_2019.csv")) %>% 
  set_names(c("LAname", "LAcode", "MSOAname", "MSOAcode", "LSOAname", "LSOAcode", "ConsMeters", "Consumption",
              "MeanCons", "MedCons", "NonConsMeters"))

#Download IMD data
IMDurl <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833970/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx"
temp <- curl_download(url=IMDurl, destfile=temp, quiet=FALSE, mode="wb")

IMDdata <- read_excel(temp, sheet="IMD2019", range="A1:F32845") %>% 
  set_names(c("LSOAcode", "LSOAname", "LAcode", "LAname", "IMDrank", "IMDdecile"))

#Combine
data <- merge(gasdata, IMDdata, by="LSOAcode")

#Plot correlation at LSOA level
ggplot(data, aes(x=MedCons, y=IMDrank))+
  geom_point(shape=21)+
  theme_classic()

agg_tiff("Outputs/EngGasxIMD.tiff", units="in", width=9, height=6, res=800)
ggplot(data, aes(x=MedCons, y=as.factor(-IMDdecile), fill=stat(x)))+
  geom_density_ridges_gradient(rel_min_height=0.01, show.legend = FALSE)+
  scale_x_continuous(limits=c(0,35000), name="Median annual domestic gas consumption (kWh)")+
  scale_y_discrete(name="Deprivation decile", labels=c("1 (least\ndeprived)", "2", "3", "4", "5", "6", "7",
                                                       "8", "9", "10 (most\ndeprived)"))+
  scale_fill_viridis_c(option="mako", direction=-1)+
  theme_custom()+
  labs(title="Homes in more deprived areas use less gas on average",
       subtitle="Median domestic gas consumption in 2019 in English Lower Super Output Areas by decile of the Index of Multiple Deprivation (IMD)",
       caption="Data from Department of Business, Energy & Industrial Strategy and Ministry of Housing, Communities & Local Government\nCartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")
dev.off()

#Aggregate everything up to MSOA level
#Bring in population data for LSOAs
temp <- tempfile()
temp2 <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2flowersuperoutputareamidyearpopulationestimatesnationalstatistics%2fmid2019sape22dt13/sape22dt13mid2019lsoabroadagesestimatesunformatted.zip"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
unzip(zipfile=temp, exdir=temp2)

pop <- read_excel(file.path(temp2, "SAPE22DT13-mid-2019-lsoa-Broad_ages-estimates-unformatted.xlsx"),
                  sheet="Mid-2019 Persons", range="A6:G34758", col_names=FALSE)[,c(1,7)]
colnames(pop) <- c("LSOAcode", "pop")

#Merge into IMD data
MSOAdata <- merge(data, pop, by="LSOAcode") %>% 
  group_by(MSOAcode) %>% 
  summarise(IMDrank=weighted.mean(IMDrank, pop),
            MeanCons=sum(Consumption)/sum(ConsMeters),
            MedCons=weighted.mean(MedCons, pop)) %>% 
  ungroup() %>% 
  rename(msoa11cd=MSOAcode)

#Download Carl Baker's lovely cartogram
msoa <- tempfile()
source <- ("https://github.com/houseofcommonslibrary/uk-hex-cartograms-noncontiguous/raw/main/geopackages/MSOA.gpkg")
msoa <- curl_download(url=source, destfile=msoa, quiet=FALSE, mode="wb")

BackgroundMSOA <- st_read(msoa, layer="5 Background")

MSOA <- st_read(msoa, layer="4 MSOA hex") %>% 
  left_join(MSOAdata, by="msoa11cd")

GroupsMSOA <- st_read(msoa, layer="2 Groups")

Group_labelsMSOA <- st_read(msoa, layer="1 Group labels") %>% 
  mutate(just=if_else(LabelPosit=="Left", 0, 1))

LAsMSOA <- st_read(msoa, layer="3 Local authority outlines (2019)")

plot1 <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom))+
  geom_sf(data=MSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom, fill=MedCons), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="White", size=0.1)+
  geom_sf(data=GroupsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black")+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="Black")+
  scale_fill_viridis_c(option="mako", direction=-1,
                       name="")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato"), legend.position="top",
        plot.title.position="plot")+
  guides(fill = guide_colorbar(title.position = 'top', title.hjust = .5,
                               barwidth = unit(20, 'lines'), barheight = unit(.5, 'lines')))+
  labs(title="Domestic gas consumption varies a lot across the country",
       subtitle="Median domestic gas consumption in 2019 in English neighbourhoods (MSOAs) in kWh",    
       caption="Data from Department of Business, Energy & Industrial Strategy\nCartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

agg_tiff("Outputs/EngGasCartogram.tiff", units="in", width=10, height=8, res=800)
plot1
dev.off()

#Bivariate map
#Remove missing MSOAs and calculate tertiles
MSOAv1 <- MSOA %>% 
  filter(RegionNation!="Wales") %>% 
  mutate(gastert=quantcut(MedCons, q=3, labels=FALSE),
         IMDtert=quantcut(-IMDrank, q=3, labels=FALSE),
         key=case_when(
           IMDtert==1 & gastert==1 ~ 1,
           IMDtert==1 & gastert==2 ~ 2,
           IMDtert==1 & gastert==3 ~ 3,
           IMDtert==2 & gastert==1 ~ 4,
           IMDtert==2 & gastert==2 ~ 5,
           IMDtert==2 & gastert==3 ~ 6,
           IMDtert==3 & gastert==1 ~ 7,
           IMDtert==3 & gastert==2 ~ 8,
           TRUE ~ 9),
         fillcolour=case_when(
           key==1 ~ "#f0f0f0", key==2 ~ "#a0dcdd", key==3 ~ "#00cfc1",
           key==4 ~ "#ffa2aa", key==5 ~ "#afa7b7", key==6 ~ "#44b4cb",
           key==7 ~ "#ff3968", key==8 ~ "#c066b2", TRUE ~ "#6d87cc"))

#generate dataframe for key
keydata <- MSOAv1 %>%
  filter(!is.na(fillcolour)) %>%
  group_by(gastert, IMDtert) %>%
  summarise(RGB=unique(fillcolour))

key <- ggplot(keydata)+
  geom_tile(aes(x=IMDtert, y=gastert, fill=RGB))+
  scale_fill_identity()+
  labs(x = expression("Greater deprivation" %->%  ""),
       y = expression("Higher gas consumption" %->%  "")) +
  theme_classic() +
  # make font small enough
  theme(
    axis.title = element_text(size = 9),axis.line=element_blank(), 
    axis.ticks=element_blank(), axis.text=element_blank(),
    text=element_text(family="Lato", colour="White"),
    panel.background=element_rect(fill="Grey10"),
    plot.background=element_rect(fill="Grey10", colour="Grey10"))+
  # quadratic tiles
  coord_fixed()

#Set up annotations
labels <- data.frame(x=c(12, 12, 50, 53.5), y=c(11, 25, 35, 15.5),
                     label=c("<span style='color:#ff3968;'>Red<span style='color:white;'> shows deprived areas with low gas consumption",
                             "<span style='color:#6d87cc;'>Blue<span style='color:white;'> shows deprived areas with high gas consumption",
                             "<span style='color:#f0f0f0;'>White<span style='color:white;'> shows affluent areas with low gas consumption",
                             "<span style='color:#00cfc1;'>Turquoise<span style='color:white;'> shows affluent areas with high gas consumption"))


plot <- ggplot()+
  geom_sf(data=BackgroundMSOA, aes(geometry=geom), fill="Grey10")+
  geom_sf(data=MSOAv1, 
          aes(geometry=geom, fill=fillcolour), colour=NA)+
  geom_sf(data=LAsMSOA %>% filter(RegionNation!="Wales"), 
          aes(geometry=geom), fill=NA, colour="Black", size=0.1)+
  geom_sf_text(data=Group_labelsMSOA %>% filter(RegionNation!="Wales"), 
               aes(geometry=geom, label=Group.labe,
                   hjust=just), size=rel(2.4), colour="White", family="Lato")+
  scale_fill_identity(na.value="Black")+
  theme_void()+
  theme(plot.title=element_text(face="bold", size=rel(1.6)),
        text=element_text(family="Lato", colour="White"), plot.title.position = "panel",
        plot.background=element_rect(fill="Grey10", colour="Grey10"),
        plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),)+
  geom_textbox(data=labels, aes(x=x, y=y, label=label), 
               width=grid::unit(0.1, "npc"), hjust=0, vjust=1, halign=0.5, size=rel(2.7),
               fill="Grey10", colour="Grey10", box.padding = grid::unit(rep(0, 4), "pt"))+
  labs(title="Birmingham & Bradford seem most exposed to rising gas prices",
       subtitle="Median 2019 domestic gas consumption and deprivation by neighbourhood (MSOA) in England",       
       caption="Data from Department of Business, Energy & Industrial Strategy and Ministry of Housing, Communities & Local Government\n cartogram from @carlbaker/House of Commons Library\nPlot by @VictimOfMaths")

#agg_png("Outputs/COVIDBivariateCasesVax.png", units="in", width=8, height=10, res=800)
agg_tiff("Outputs/BivariateGasxIMD.tiff", units="in", width=8, height=10, res=800, background="Grey10")
ggdraw()+
  draw_plot(plot, 0,0,1,1)+
  draw_plot(key, 0.66,0.68,0.28,0.28)
dev.off()
