rm(list=ls())

library(tidyverse)
library(curl)
library(forcats)
library(RcppRoll)

options(scipen = 999)

#Read in COVID dase data
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- fread(temp)[,c(1,2,3,4,5,8)]
colnames(data) <- c("name", "code", "type", "date", "cases", "cumul_cases")
data$date <- as.Date(data$date)
data <- subset(data, type=="Upper tier local authority")

temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D367", col_names=TRUE)
colnames(LApop) <- c("code2", "name", "geography", "pop")

LApop_long <- gather(LApop, age, pop, pop)

#deal with Cornwall & Scilly which are merged in the PHE data
LApop_long$code <- case_when(
  LApop_long$code2=="E06000052" ~ "E06000052",
  TRUE ~ LApop_long$code2
)

LApop_long <- LApop_long %>%
  group_by(code) %>%
  summarise(pop=sum(pop))

#Combine
data <- merge(data, LApop_long, all.x=TRUE)

#Set up skepeton dataframe with dates
LAcodes <- unique(data$code)
min <- min(data$date)
max <- max(data$date)

skeleton <- data.frame(code=rep(LAcodes, each=(max-min+1), times=1), date=rep(seq.Date(from=min, to=max, by="day"), each=1, times=length(LAcodes)))

#Map data onto skeleton
fulldata <- merge(skeleton, data[,c(1,4:6)], by=c("code", "date"), all.x=TRUE, all.y=TRUE)

#Fill in blank days
fulldata$cases <- ifelse(is.na(fulldata$cases), 0, fulldata$cases)

#Calculate cumulative sums so far
fulldata <- fulldata %>%
  group_by(code) %>%
  mutate(cumul_cases=cumsum(cases))

#Bring in LA factors
LAdata <- unique(data[,c(1,2,7)])
fulldata <- merge(fulldata, LAdata)

heatmap <- fulldata %>%
  group_by(code) %>%
  mutate(caserate=cases*100000/pop) %>%
  mutate(roll_avg=roll_mean(caserate, 5, align="right", fill=0)) %>%
  mutate(total=max(cumul_cases), maxrate=max(roll_avg),maxday=date[which(roll_avg==maxrate)][1])

heatmap$maxprop <- heatmap$roll_avg/heatmap$maxrate

tiles <- ggplot(heatmap, aes(x=date, y=fct_reorder(name, maxday), fill=maxprop))+
  geom_tile(colour="White", show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_y_discrete(name="")+
  scale_x_date(name="Date", limits=as.Date(c("2020-03-01", "2020-04-24")), expand=c(0,0))+
  labs(title="Timelines for COVID-19 cases in English Local Authorities",
       subtitle="The heatmap represents the 5-day rolling average of the number of new confirmed cases, normalised to the maximum value within the Local Authority.\nLAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each LA.",
       caption="Data from Public Health England | Plot by @VictimOfMaths")+
  theme(axis.line.y=element_blank(), plot.subtitle=element_text(size=rel(0.78)), plot.title.position="plot")

bars <- ggplot(subset(heatmap, date==maxday), aes(x=total, y=fct_reorder(name, maxday), fill=total))+
  geom_col(show.legend=FALSE)+
  theme_classic()+
  scale_fill_distiller(palette="Spectral")+
  scale_x_continuous(name="Total confirmed cases", breaks=c(0,1000,2000))+
  theme(axis.title.y=element_blank(), axis.line.y=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

png("Outputs/COVIDLAHeatmap.png", units="in", width=10, height=16, res=500)
plot_grid(tiles, bars, align="h", rel_widths=c(1,0.2))
dev.off()


