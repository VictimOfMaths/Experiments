rm(list=ls())

library(tidyverse)
library(rJava)
remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), 
                        INSTALL_opts = "--no-multiarch")
library(tabulizer)
library(curl)
library(extrafont)

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

tables_scraped <- extract_tables(file=temp, pages=84, method="stream", output="data.frame") 

table1 <- tables_scraped %>% 
  pluck(1) %>% 
  as_tibble() %>% 
  filter(!row_number() %in% c(3,6)) %>% 
  mutate(Total=gsub("2 30-391 ", "1", Total)) %>% 
  separate(Total, into=c("A", "B"), sep=" ") %>% 
  set_names("freq", "Total_Total", "Age_18-29", "Age_30-39", "Age_40-49", "Age_50-59",
            "Age_60-69", "Age_70+", "Educ_Degree", "Educ_A-level", "Educ_GCSE", "Educ_Other", 
            "Educ_None", "Emp_FTE", "Emp_Employed", "Emp_Unemployed", "Emp_Retired", "Emp_Other",
            "Class_Middle", "Class_Working", "Class_Other") %>% 
  mutate(Class_Other=case_when(
    freq=="18% less than once a day" ~ "18%",
    freq=="2% do this" ~ "2%",
    TRUE ~ Class_Other),
    freq=c("Several times a day", "Once a day", "Once a week or more", "Less than once a week",
           "N/A")) %>% 
  pivot_longer(c(2:21), names_to=c("Cat", "Val"), names_sep="_", values_to="Prop") %>% 
  mutate(Prop=as.numeric(gsub("%", "", Prop))/100)

ggplot(table1 %>% filter(Cat!="Total"), aes(x=Prop, y=Val, fill=freq))+
  geom_col(position="fill")+
  facet_wrap(~Cat, scales="free_y")+
  theme_custom()




