# Data wrangling
library(tidyverse)
library(dplyr)
library(reshape2)
library(readxl)
library(easyPubMed)


# Plotting (general)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(patchwork)
library(ggExtra)
library(pals) # so we can use longer colour palette "alphabet" for south asia lab plot

# Plotting (maps)
library(sf)
library(rvest)
library(maps)
library(scatterpie)
library(ggnewscale) # to allow us to specify different scales for countries and pies
library(janitor)

data <- read_csv("E:/Github/World_Map/global_for_figure.csv")
View(data)


##attach(data)
#head(data)
#top_10_ST<-c("2062","320","199","12888","172","695","6121","276","2464","12473")
top_10_gpsc<-c("17","1","4","53","5","10","84","27","9","121")
#data<- data%>% filter(ST != "-")%>%
#  mutate(ST = if_else(ST %in% top_20_ST, ST, "Other"))


data <- data %>%
  #filter(GPSC != "-") %>%
  mutate(
    GPSC = if_else(
      GPSC %in% top_10_gpsc,
      as.character(GPSC),  # Convert to character if necessary
      "Other"
    )
  )


gps <- read.csv("D:/SPN/GLOBAL_DISTRIBUTION/gps_Country.csv")%>%
  select(Country, Latitude, Longitude)
#gps


#region_N <- data %>%
# group_by(Region)%>% 
#summarize(region_total =n())

country_N <- data %>%
  group_by(Country)%>% 
  summarize(country_total =n())%>%
  mutate(pie_size=ifelse(country_total>100, 5, ifelse(country_total>35, 4, ifelse(country_total>15, 3, 2)))) %>%
  mutate(pie_cat=ifelse(pie_size==2, "1-15", ifelse(pie_size==3, "16-35", ifelse(pie_size==4, "36-100", ">100"))))
#1-15: 2, 16-35:3, 36-100: 4, >100: 5
#view(country_N)


country_GPSC <- data %>%
  group_by(Country, GPSC)%>% 
  summarize(n =n()) %>%
  left_join(country_N) %>% # country counts
  mutate(GPSC_percentage_country = round(n/country_total*100,0)) # percent per region

#view(country_ST )
GPSC_name <- names(table(country_GPSC$GPSC))

#values = c("#000000", "#BD7CB4", "#7F2470", "#7D6AAF", "#492A75", "#AEC4E3", "#0966B1", "#58C3C3", "#008988", "#00A45D", "#569842", "#ACD589", "#FAED21", "#FCC777", "#F7A51C", "#FF6D00", "#EC403B", "#B72025")
#ST_colours=c("#BD7CB4","#7F2470", "#7D6AAF","#492A75","#0966B1","#58C3C3","#008988","#00A45D","#569842","#ACD589","#000000" )
GPSC_colours=c("#a50026","#d73027","#f46d43","#fdae61","#fee08b","#ffffbf","#d9ef8b","#a6d96a","#66bd63","#1a9850","#006837")

GPSC_colours<-c(GPSC_colours)
names(GPSC_colours) <- GPSC_name
GPSC_colours <- GPSC_colours[order(names(GPSC_colours))] # sort for legend



data_Map_countryGPSC <-  data %>%
  mutate(map_GPSCtype = GPSC)%>%
  select(Country, map_GPSCtype)%>%
  dcast(Country ~ map_GPSCtype)%>%
  left_join(gps)%>%
  left_join(country_N)%>%
  mutate(lat = as.numeric(Latitude), 
         long = as.numeric(Longitude),
         mytext=paste(
           Country, "\n", 
           "N= ", country_total, sep="")
  )

spn_countryN <- data %>%
  group_by(Country)%>%
  summarize(CountryCount=n())


spn_Nmap <- map_data('world') %>% 
  filter(region != "Antarctica") %>%
  group_by(region) %>%
  # Merge in spn counts
  left_join(spn_countryN, by = c('region' = 'Country'))

spn_Nmap$CountryData <- spn_Nmap$CountryCount > 0


GPSCtype_pie_map_country_legend <- ggplot(data = spn_Nmap, aes(long, lat)) +
  # Draw world map
  geom_map(map = spn_Nmap, aes(map_id = region), fill = "white", color = "gray", lwd = 0.2) +
  coord_quickmap()+
  # Add highlighted countries with light grey shade
  geom_polygon(data = spn_Nmap %>% filter(CountryData), aes(x = long, y = lat, group = group), fill = "#f2e6d9", color = "grey") + 
  geom_scatterpie(data = data_Map_countryGPSC, aes(x= long, y=lat , group = Country, r = pie_size*1.5),
                  cols= GPSC_name, color="black", alpha=1, lwd=0.2) +
  scale_fill_manual(values = GPSC_colours)+
  geom_scatterpie_legend(data_Map_countryGPSC$pie_size, x = -150, y = -40, n = 4 ) +
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw(base_size = 45)+
  theme_minimal()+
  theme(panel.background = element_rect(),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(title = "GPSC", keywidth = 0.5, keyheight = 0.5))
#+
# ggtitle("ST prevalence by world countries")

GPSCtype_pie_map_country_legend
ggsave("GPSC.svg", plot =  GPSCtype_pie_map_country_legend, width = 14, height = 8, dpi = 600)
