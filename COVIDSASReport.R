rm(list=ls())

library(tidyverse)
library(rJava)
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), 
                        INSTALL_opts = "--no-multiarch")
library(tabulizer)
library(curl)
library(extrafont)
library(forcats)
library(scales)
library(paletteer)
library(ragg)

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

#Download Senseaboutscience report
temp <- tempfile()
url <- "https://senseaboutscience.org/wp-content/uploads/2022/05/What-Counts-Scoping-Inquiry-Report-2022.pdf"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

#Scrape data from pdf
tables_scraped <- extract_tables(file=temp, pages=84, method="stream", output="data.frame") 

#Sort out the mess
table1 <- tables_scraped %>% 
  pluck(1) %>% 
  as_tibble() %>% 
  filter(!row_number() %in% c(3,6)) %>% 
  mutate(Total=gsub("2 30-391 ", "1", Total)) %>% 
  separate(Total, into=c("A", "B"), sep=" ") %>% 
  set_names("freq", "Total_Total", "Age_18-29", "Age_30-39", "Age_40-49", "Age_50-59",
            "Age_60-69", "Age_70+", "Education_Degree", "Education_A-level", "Education_GCSE", 
            "Education_Other", "Education_None", "Employment_Full Time Education", "Employment_Employed", 
            "Employment_Unemployed", "Employment_Retired", "Employment_Other",
            "Class_Middle", "Class_Working", "Class_Other") %>% 
  mutate(Class_Other=case_when(
    freq=="18% less than once a day" ~ "18%",
    freq=="2% do this" ~ "2%",
    TRUE ~ Class_Other),
    freq=c("Several times a day", "Once a day", "Once a week or more", "Less than once a week",
           "Never")) %>% 
  pivot_longer(c(2:21), names_to=c("Cat", "Val"), names_sep="_", values_to="Prop") %>% 
  mutate(Prop=as.numeric(gsub("%", "", Prop))/100,
         Val=factor(Val, levels=c("18-29", "30-39", "40-49", "50-59", "60-69", "70+",
                                  "Degree", "A-level", "GCSE", "None",
                                  "Middle", "Working", "Full Time Education", "Employed", "Unemployed",
                                  "Retired", "Other")),
         freq=factor(freq, levels=c("Several times a day", "Once a day", "Once a week or more", 
                                    "Less than once a week", "Never")))

agg_png("Outputs/COVIDSAS1st6mths.png", units="in", width=9, height=7, res=500)
ggplot(table1 %>% filter(Cat!="Total"), aes(x=Prop, y=fct_rev(Val), fill=fct_rev(freq)))+
  geom_col(position="fill")+
  scale_x_continuous(name="Proportion of respondents", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_manual(name="", values=c("#D5A370", "#B9DBF1", "#8EC1E7", "#5991C7", "#0F2B5F"),
                    guide = guide_legend(reverse = TRUE))+
  facet_wrap(~Cat, scales="free_y")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Older people and the unemployed checked the COVID numbers most regularly",
       subtitle="Self-reported frequency of 'finding out the latest information related to COVID-19' during the first 6 months of the pandemic",
       caption="Data from NatCen/Sense About Science | Plot by @VictimOfMaths")
dev.off()

#Repeat for 2nd table
table2 <- tables_scraped %>% 
  pluck(2) %>% 
  as_tibble() %>% 
  filter(!row_number() %in% c(1,3,6)) %>%
  bind_rows(as.data.frame(t(colnames(tables_scraped %>% pluck(2) %>% as_tibble()))) %>% 
            set_names(colnames(tables_scraped %>% pluck(2) %>% as_tibble())) %>% 
            mutate(across(c(2:20), ~sub("\\..*", "", gsub("X", "", .))))) %>% 
  set_names("freq", "Age_18-29", "Age_30-39", "Age_40-49", "Age_50-59",
            "Age_60-69", "Age_70+", "Education_Degree", "Education_A-level", "Education_GCSE", 
            "Education_Other", "Education_None", "Employment_Full Time Education", "Employment_Employed", 
            "Employment_Unemployed", "Employment_Retired", "Employment_Other",
            "Class_Middle", "Class_Working", "Class_Other") %>% 
  mutate(Class_Other=case_when(
    freq=="2% do this 2%" ~ "2%",
    freq=="38% less than once a day 39%" ~ "38%",
    TRUE ~ Class_Other),
    Education_GCSE=if_else(freq=="2% do this 2%", "3%", Education_GCSE),
    Education_Other=if_else(freq=="2% do this 2%", "5%", Education_Other),
    freq=c("Once a day", "Never", "Once a week or more", "Less than once a week", "Several times a day")) %>% 
  pivot_longer(c(2:20), names_to=c("Cat", "Val"), names_sep="_", values_to="Prop") %>% 
  mutate(Prop=as.numeric(gsub("%", "", Prop))/100,
         Val=factor(Val, levels=c("18-29", "30-39", "40-49", "50-59", "60-69", "70+",
                                  "Degree", "A-level", "GCSE", "None",
                                  "Middle", "Working", "Full Time Education", "Employed", "Unemployed",
                                  "Retired", "Other")),
         freq=factor(freq, levels=c("Several times a day", "Once a day", "Once a week or more", 
                                    "Less than once a week", "Never")))

agg_png("Outputs/COVIDSASlastmths.png", units="in", width=9, height=7, res=500)
ggplot(table2, aes(x=Prop, y=fct_rev(Val), fill=fct_rev(freq)))+
  geom_col(position="fill")+
  scale_x_continuous(name="Proportion of respondents", labels=label_percent(accuracy=1))+
  scale_y_discrete(name="")+
  scale_fill_manual(name="", values=c("#D5A370", "#B9DBF1", "#8EC1E7", "#5991C7", "#0F2B5F"),
                    guide = guide_legend(reverse = TRUE))+
  facet_wrap(~Cat, scales="free_y")+
  theme_custom()+
  theme(legend.position="top")+
  labs(title="Older people and the unemployed still check the COVID numbers most regularly",
       subtitle="Self-reported frequency of 'finding out the latest information related to COVID-19' since March 2021",
       caption="Data from NatCen/Sense About Science | Plot by @VictimOfMaths")
dev.off()
