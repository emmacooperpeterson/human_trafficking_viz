library(sp)
library(rgdal)
library(maptools)
library(broom)
library(dplyr)
library(ggplot2)
library(ggmap)
library(viridis)
library(tidyverse)

setwd('~/Desktop/repos/dataviz/project/data/')

countries <- readOGR(dsn = "ne_50m_admin_0_countries",
                  layer = "ne_50m_admin_0_countries")

countries.points <- fortify(countries, region="sov_a3")

countries.df <- left_join(countries.points, countries@data, 
                          by = c("id" = "sov_a3"))

victim_countries <- read_csv("htd/victim_countries.csv")
victim_countries <- victim_countries[c("case_id", "victimcountry")]
country_counts <- summarize(group_by(victim_countries, victimcountry), count=n())

country_codes <- read_csv("country-codes_csv.csv")
country_codes <- country_codes[c('name', 'ISO3166-1-Alpha-3')]
country_codes <- filter(country_codes, !is.na(name))

cases <- read_csv("htd/cases.csv")
num_victims <- inner_join(cases, victim_countries, by='case_id')
num_victims <- num_victims[c('case_id', 'number_victims_foreign', 'victimcountry')]

num_victims['number_victims_foreign'][is.na(num_victims['number_victims_foreign'])] <- 0
num_victims <- aggregate(num_victims$number_victims_foreign, 
                         by=list(num_victims$victimcountry), 
                         FUN=sum)

country_victim_counts <- inner_join(num_victims, country_codes, by=c('Group.1' = 'name'))
colnames(country_victim_counts) <- c('country', 'num_vics', 'id')

countries.df <- left_join(countries.df, country_victim_counts, by='id')
countries.df['num_vics'][is.na(countries.df['num_vics'])] <- 0

pops <- read_csv('country_pops.csv')
colnames(pops) <- c('country', 'id', 'pop')

countries.df <- inner_join(countries.df, pops, by='id')
countries.df$vics_per_pop <- (countries.df$num_vics / countries.df$pop) * 1000000

countries.df <- filter(countries.df, region_wb=="Latin America & Caribbean" &
                         id != "US1")

#buckets
countries.df$pop_bucket <- NA
countries.df$pop_bucket[countries.df$vics_per_pop == 0] <- "a"
countries.df$pop_bucket[countries.df$vics_per_pop > 0 & 
                      countries.df$vics_per_pop <= 1] <- "b"
countries.df$pop_bucket[countries.df$vics_per_pop > 1 & 
                      countries.df$vics_per_pop <= 9] <- "c"
countries.df$pop_bucket[countries.df$vics_per_pop > 9 & 
                          countries.df$vics_per_pop <= 26] <- "d"
countries.df$pop_bucket[countries.df$vics_per_pop > 26 & 
                      countries.df$vics_per_pop <= 57] <- "e"
countries.df$pop_bucket[countries.df$vics_per_pop > 57] <- "f"

my_theme <- theme(plot.background = element_rect(fill="#F4F4F4"),
                  plot.margin=unit(c(1,1,1,1),"cm"),
                  plot.caption = element_text(family="Montserrat Light", size=6,
                                              margin=margin(t=20)),
                  plot.title = element_text(family="Montserrat", face="bold", size=15),
                  plot.subtitle = element_text(family="Courier New"),
                  
                  panel.background = element_rect(fill = "#F4F4F4"),
                  panel.grid.major.y = element_line(color="black", size=0.25),
                  panel.grid.minor.y = element_line(color="black", size=0.25),
                  panel.grid.minor.x=element_blank(),
                  panel.grid.major.x=element_blank(),
                  
                  legend.background = element_rect(fill="#F4F4F4"),
                  legend.key = element_blank(),
                  legend.title = element_text(family="Montserrat", size=10),
                  legend.text = element_text(family="Courier New", size=8),
                  
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text = element_text(family="Montserrat Light", size=7),
                  axis.text.x = element_text(angle = 45, hjust = 1, margin=margin(t=-10)),
                  axis.title = element_text(family="Montserrat", size=8),
                  axis.title.x = element_text(margin=margin(t=15)),
                  axis.title.y = element_text(margin=margin(r=15)))

ggplot(data = countries.df, aes(x = long, 
                                y = lat, 
                                group = group, 
                                fill=pop_bucket)) +
  
  geom_polygon(color = "white", size=0.2) +
  
  coord_quickmap(xlim=c(-125, -30), ylim=(c(-60, 35))) +
  
  scale_fill_manual(values = c('#f6c90e','#d5b337','#b39e4b',
                               '#908859','#6c7464','#40616d'),
                    labels = c('least (0)', '', '', '', '', 'most \n(0.000057+)')) +
  
  labs(fill = "Victims per \ncapita to the \nUnited States", 
       title = paste("Latin American & Caribbean victims of \nhuman trafficking in the",
                      "United States \nare most likely to come from",
                      "Jamaica, \nDominican Republic, and Honduras"), 
       x="", 
       y="",
       subtitle = "Number of Latin American & Caribbean \nvictims per capita",
       caption="Source: www.HumanTraffickingData.org") +
  
  my_theme +
  
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c())

ggsave("../plots/latin_am.pdf")


#zoom - add this part to full graph in illustrator
ggplot(data = countries.df, aes(x = long, 
                                y = lat, 
                                group = group, 
                                fill=pop_bucket)) +
  geom_polygon(color = "white", size=0.2) +
  coord_quickmap(xlim=c(-90, -70), ylim=(c(10, 30))) +
  scale_fill_manual(
    values = c('#f6c90e','#d5b337','#b39e4b',
               '#908859','#6c7464','#40616d'),
    labels = c('least (0)', '', '', '', '', 'most \n(0.000057+)')) +
  
  labs(fill = "Victims per \ncapita to the \nUnited States", 
       title = paste("Latin American & Caribbean victims of \nhuman trafficking in the",
                     "United States \nare most likely to come from",
                     "Jamaica, \nDominican Republic, and Honduras"), 
       x="", 
       y="",
       subtitle = "Number of Latin American & Caribbean \nvictims per capita",
       caption="Source: www.HumanTraffickingData.org") +
  
  my_theme +
  
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c())

ggsave("../plots/latin_am_zoom.pdf")

