rm(list=ls())

library(tidyverse)
library(scales)
library(extrafont)
library(ragg)
library(readxl)

#Data from Anderson & Pernilla
#Wine production per capita in litres of wine
#Beer procuction total in kilolitres of beer
#Population data from Eurostat
#Price data from EU files
#https://ec.europa.eu/taxation_customs/sites/taxation/files/resources/documents/taxation/excise_duties/alcoholic_beverages/rates/excise_duties-part_i_alcohol_en.pdf

data <- data.frame(country=c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark",
                             "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
                             "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
                             "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
                             "United Kingdom"),
                   wineprod=c(27.9, 1.3, 12.9, 10.9, NA, NA, 0, 0, 0, 62.4, 9.8, 19.1, 24.8, 0, 55.7,
                              0, 0, 1.3, NA, 0, 0, 63.6, 19.6, NA, NA, 73.4, 0, 0),
                   beerprod=c(920627, 2061600, 489000, 341667, NA, NA, 610700, NA, 401000, 2468000,
                              9495700, 380000, 623900, 800000, 1296800, NA, NA, NA, NA, 2455900,
                              NA, 729000, 1658060, NA, NA, 3620000, 461400, 4373400),
                   pop=c(8.9, 11.5+0.6, 7.0, 4.1, 0.9, 10.7, 5.8, 1.3, 5.5, 67.3, 83.2, 10.7, 9.8, 5.0,
                         59.6, 1.9, 2.8, 0.6, 0.5, 17.4, 38.0, 10.3, 19.3, 5.5, 2.1, 47.3, 10.3, 67.0),
                   wineprice=c(0, 0.08, 0, 0, 0, 0, 0.21, 0.15, 0.4, 0, 0, 0, 0, 0.43, 0, 0.11, 0.17, 
                               0, 0.02, 0.09, 0.04, 0, 0, 0, 0, 0, 0.25, 0.34),
                   beerprice=c(0.06, 0.06, 0.02, 0.07, 0.08, 0.04, 0.08, 0.16, 0.46, 0.1, 0.02, 0.16,
                               0.06, 0.29, 0.09, 0.1, 0.09, 0.03, 0.06, 0.1, 0.06, 0.05, 0.02, 0.05,
                               0.15, 0.03, 0.24, 0.27)) %>% 
  mutate(beerpercap=beerprod/(pop*1000000))

#Fit models to test significant of associations
beermodel <- lm(beerprice ~ beerpercap, data)
summary.lm(beermodel)

winemodel <- lm(wineprice ~ wineprod, data)
summary.lm(winemodel)

#agg_png("Outputs/BeerProdxDuty.png", units="in", width=8, height=6, res=500)
agg_tiff("Outputs/BeerProdxDuty.tiff", units="in", width=8, height=6, res=500)
ggplot(data, aes(x=beerpercap*1000, y=beerprice, size=pop))+
  geom_point(fill="#ffc000", shape=21)+
  scale_x_continuous(name="Beer production\n(litres per person per year)", limits=c(0,NA))+
  scale_y_continuous(name="Excise duty payable per 10grams of ethanol",
                     labels = dollar_format(suffix = "", prefix = "\u20AC"))+
  scale_size_continuous(name="Population\n(millions)")+
  theme_classic()+
  theme(text=element_text(family="Roboto"),
        plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="...but beer producing countries are happy to tax beer",
       subtitle="Per capita beer production compared to duty payable on 5% beer",
       caption="Production data from Anderson & Pinilla, tax data from the EU\nPlot by @VictimOfMaths")
dev.off()

#agg_png("Outputs/WineProdxDuty.png", units="in", width=8, height=6, res=500)
agg_tiff("Outputs/WineProdxDuty.tiff", units="in", width=8, height=6, res=500)
ggplot(data, aes(x=wineprod, y=wineprice, size=pop))+
  geom_point(fill="#7030a0", shape=21, alpha=0.7)+
  scale_x_continuous(name="Wine production\n(litres per person per year)", limits=c(0,NA))+
  scale_y_continuous(name="Excise duty payable per 10grams of ethanol",
                     labels = dollar_format(suffix = "", prefix = "\u20AC"))+
  scale_size_continuous(name="Population\n(millions)")+
  theme_classic()+
  theme(text=element_text(family="Roboto"),
        plot.title=element_text(face="bold", size=rel(1.5)))+
  labs(title="Wine producing nations in the EU don't tax wine...",
       subtitle="Per capita wine production compared to duty payable on 12.5% ABV wine",
       caption="Production data from Anderson & Pinilla, tax data from the EU\nPlot by @VictimOfMaths")
dev.off()
