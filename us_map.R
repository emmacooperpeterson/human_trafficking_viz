library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(extrafont)
loadfonts()

setwd('~/Desktop/Repositories/dataviz/project/data/')

#################
### LOAD DATA ###
#################

#run this line before loading tidyverse bc of an error
us <- ggplot2::map_data("state")

library(tidyverse)
cases <- read_csv("htd/cases.csv")
crime_locations <- read_csv("htd/crime_locations.csv")

crime_locations$state <- tolower(crime_locations$state)

states_list <- list(c("ak", "alaska"), c("al", "alabama"), c("ar", "arkansas"),
                    c("az", "arizona"), c("ca", "california"), c("co", "colorado"),
                    c("ct", "connecticut"), c("dc", "district of columbia"),
                    c("d.c.", "district of columbia"), c("de", "delaware"),
                    c("fl", "florida"), c("ga", "georgia"), c("hi", "hawaii"),
                    c("ia", "iowa"), c("id", "idaho"), c("in", "indiana"),
                    c("il", "illinois"), c("ks", "kansas"), c("ky", "kentucky"),
                    c("la", "louisiana"), c("ma", "massachusetts"), c("md", "maryland"),
                    c("me", "maine"), c("mi", "michigan"), c("mn", "minnesota"),
                    c("mo", "missouri"), c("ms", "mississippi"), c("mt", "montana"),
                    c("nc", "north carolina"), c("nd", "north dakota"),
                    c("ne", "nebraska"), c("nh", "new hampshire"),
                    c("nj", "new jersey"), c("nm", "new mexico"), c("nv", "nevada"),
                    c("ny", "new york"), c("oh", "ohio"), c("ok", "oklahoma"),
                    c("or", "oregon"), c("pa", "pennsylvania"), c("pr", "puerto rico"),
                    c("ri", "rhode island"), c("sc", "south carolina"),
                    c("sd", "south dakota"), c("tn", "tennessee"), c("tx", "texas"),
                    c("ut", "utah"), c("va", "virginia"), c("vt", "vermont"),
                    c("wa", "washington"), c("wi", "wisconsin"), 
                    c("wv", "west virginia"), c("wy", "wyoming"),
                    c("district of colombia", "district of columbia"),
                    c("washington d.c.", "district of columbia"))
    
for (pair in states_list) {
  crime_locations$state[crime_locations$state == pair[1]] <- pair[2]
}

state_counts <- summarize(group_by(crime_locations, state), count = n())
state_counts <- filter(state_counts, !is.na(state))

us_counts <- inner_join(us, state_counts, by = c("region" = "state"))

population <- read_csv("nst-est2016-alldata.csv")
population <- population[6:57,c("NAME", "CENSUS2010POP")]
population$NAME <- tolower(population$NAME)

us_counts <- inner_join(us_counts, population, by = c("region" = "NAME"))
us_counts$hun_thou_residents <- us_counts$CENSUS2010POP / 100000
us_counts$relative_count <- us_counts$count / us_counts$hun_thou_residents

#city counts
cities <- crime_locations[c("city", "state")]
cities <- filter(crime_locations, !is.na(cities$city) & !is.na(cities$state)
                 & cities$state != "alaska" & cities$state != "hawaii"
                 & cities$state != "puerto rico" & cities$state != "guam")
cities$full_name <- paste(cities$city, ", ", cities$state, sep="")
city_counts <- dplyr::summarize(group_by(cities, full_name), cases = n())
#geocodes <- geocode(as.character(city_counts$full_name))
#write_csv(geocodes, "city_geocodes.csv")
#only run above lines if necessary: takes 5-10 minutes
geocodes <- read_csv("city_geocodes.csv")

city_counts$id <- seq(1,638)
geocodes$id <- seq(1,638)

city_counts_geocodes <- inner_join(city_counts, geocodes, by="id")

entries <- read_csv("htd/entry_ports.csv")
entry_points <- unique(entries$name)
entry_points <- entry_points[seq(2,27,1)]
#geos <- geocode(entry_points)
#write_csv(geos, "entry_geocodes.csv")
#only run above lines if necessary: takes 5-10 minutes
geos <- read_csv("entry_geocodes.csv")

entry_points <- data.frame(entry_points)
geos <- data.frame(geos)

entry_points$id <- seq(1,26)
geos$id <- seq(1,26)

entry_points <- inner_join(entry_points, geos, by="id")
entry_points <- entry_points[c('lon', 'lat')]
entry_points <- entry_points[!duplicated(entry_points), ]
entry_points <- filter(entry_points, lat > 20)
entry_points$num <- 1

############
### PLOT ###
############

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

ggplot() + geom_polygon(data=us_counts, aes(x=long, y=lat, 
                         group = group, fill=us_counts$relative_count)) + 
  my_theme + 
  
  labs(fill = "HT prosecutions \nper 100,000 residents", 
       size = "Entry points for foreign victims", 
       title = "South Dakota and D.C. lead U.S. in human \ntrafficking prosecutions per capita", x="", y="",
       subtitle = "Human trafficking prosecutions by city and state, 2000-2016",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_y_continuous(breaks=c()) + 
  
  scale_x_continuous(breaks=c()) + 
  
  theme(panel.border =  element_blank(),
        legend.position = "bottom") +
  
  scale_fill_gradient(low="#E3E3E3", high = "#141C25", 
                      guide = guide_colorbar(direction = "horizontal", 
                                             ticks=FALSE, barwidth = 8, 
                                             barheight = 0.5,
                                             title.position="top")) +
  
  scale_size_continuous(guide = guide_legend(title.position="top"),
                        range = c(2)) +
  
  geom_jitter(data=entry_points, aes(x=lon, y=lat, size=num), fill='#F6C90E', 
              color="black", shape=21, alpha=0.6) +
  
  coord_quickmap()

ggsave("../plots/us_map.pdf")



#zoom on d.c. - add to full map in illustrator
ggplot() + geom_polygon(data=us_counts, aes(x=long, y=lat, 
                                   group = group, fill=us_counts$relative_count)) + 
  my_theme + 
  
  labs(fill = "HT prosecutions \nper 100,000 residents", 
       size = "Entry points for foreign victims", 
       title = "South Dakota and D.C. lead U.S. in human \ntrafficking prosecutions per capita", x="", y="",
       subtitle = "Human trafficking prosecutions by city and state, 2000-2016",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_y_continuous(breaks=c()) + 
  
  scale_x_continuous(breaks=c()) + 
  
  theme(panel.border =  element_blank(),
        legend.position = "bottom") +
  
  scale_fill_gradient(low="#E3E3E3", high = "#141C25", 
                      guide = guide_colorbar(direction = "horizontal", 
                                             ticks=FALSE, barwidth = 8, 
                                             barheight = 0.5,
                                             title.position="top")) +
  
  scale_size_continuous(guide = guide_legend(title.position="top"),
                        range = c(2)) +
  
  geom_jitter(data=entry_points, aes(x=lon, y=lat, size=num), fill='#F6C90E', 
              color="black", shape=21, alpha=0.6) +
  
  coord_quickmap(xlim=c(-79, -76), ylim=(c(37, 40)))

ggsave("../plots/us_map_zoom.pdf")


