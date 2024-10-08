library(svglite)
# Data wrangling
library(tidyverse)
library(dplyr)
library(reshape2)
library(readxl)
library(easyPubMed)

# Plotting (general)
library(ggplot2)
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

#attach(data)
#head(data)


gps <- read.csv("D:/SPN/GLOBAL_DISTRIBUTION/gps_Country.csv")%>%
  select(Country, Latitude, Longitude)
#gps


#####region_N <- data %>%
  #group_by(Region)%>% 
  #summarize(region_total =n())

country_N <- data %>%
  group_by(Country)%>%
  summarize(country_total =n())%>%
  mutate(pie_size=ifelse(country_total>100, 5, ifelse(country_total>35, 4, ifelse(country_total>15, 3, 2)))) %>%
  mutate(pie_cat=ifelse(pie_size==2, "1-15", ifelse(pie_size==3, "16-35", ifelse(pie_size==4, "36-100", ">100"))))
 

#view(country_N)

country_sero <- data %>%
  group_by(Country, Subtype)%>% 
  summarize(n =n()) %>%
  left_join(country_N) %>% # country counts
  mutate(sero_percentage_country = round(n/country_total*100,0)) # percent per region

sero_name <- names(table(country_sero$Subtype))


sero_colours <- c( "#e66101", "#b2abd2", "#fdb863", "#5e3c99", "#008837")
names(sero_colours) <- sero_name
sero_colours <- sero_colours[order(names(sero_colours))] # sort for legend




# order by name
sero_colours <- sero_colours[sort(names(sero_colours))]

data_Map_countrySero <-  data %>%
  mutate(map_serotype = Subtype)%>%
  select(Country, map_serotype)%>%
  dcast(Country ~ map_serotype)%>%
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


serotype_pie_map_country_legend <- ggplot(data = spn_Nmap, aes(long, lat)) +
  # Draw world map
  geom_map(map = spn_Nmap, aes(map_id = region), fill = "white", color = "gray", lwd = 0.2) +
  coord_quickmap()+
  # Add highlighted countries with light grey shade
  geom_polygon(data = spn_Nmap %>% filter(CountryData), aes(x = long, y = lat, group = group), fill = "#f2e6d9", color = "grey") +
  geom_scatterpie(data = data_Map_countrySero, aes(x= long, y=lat , group = Country, r = pie_size),
                  cols= sero_name, color="black", alpha=1, lwd=0.2) +
  scale_fill_manual(values = sero_colours)+
  geom_scatterpie_legend(data_Map_countrySero$pie_size, x = -150, y = -40, n = 5 ) +
  ylab("Latitude")+
  xlab("Longitude")+
  theme_bw(base_size = 45)+
  theme_minimal()+
  theme(panel.background = element_rect(),
        panel.grid = element_blank(), 
        legend.position="bottom") +
  guides(fill = guide_legend(title = "Subtype", keywidth = 0.5, keyheight = 0.5))#+ ggtitle("Serotype prevalence by world countries")

serotype_pie_map_country_legend
ggsave("Subtype_R.svg", plot =  serotype_pie_map_country_legend, width = 14, height = 8, dpi = 600)
