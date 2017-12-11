library(tidyverse)
library(extrafont)
library(waffle)
loadfonts()

setwd('~/Desktop/repos/dataviz/project/data/')

#################
### LOAD DATA ###
#################

cases <- read_csv("htd/cases.csv")
base_locations <- read_csv("htd/base_locations.csv")
crime_locations <- read_csv("htd/crime_locations.csv")
criminal_methods <- read_csv("htd/criminal_methods.csv")
defendants <- read_csv("htd/defendants.csv")
entry_ports <- read_csv("htd/entry_ports.csv")
judges <- read_csv("htd/judges.csv")
victim_countries <- read_csv("htd/victim_countries.csv")

################
### CLEANING ###
################

#remove outliers
defendants <- filter(defendants, arrest_age < 150 & felonies_sentenced < 999
                     & birth_year > 1900)

#create factors
defendants$gender <- factor(defendants$gender)
levels(defendants$gender) <- c("male", "female", "unknown")

defendants$race <- factor(defendants$race)
levels(defendants$race) <- c("white", "black", "hispanic", "asian", "indian", "other")

judges$appointed_by <- factor(judges$appointed_by)

#remove unwanted columns (rewrite to avoid repetition)
drop <- c("created_at", "updated_at", "id")

cases_drop <- c("created_at", "updated_at", "id", "assigned_user_id", "user_id",
                "dropbox_url", "case_number", "case_name", "case_status")

entry_ports <- entry_ports[ , !(names(entry_ports) %in% drop)]
victim_countries <- victim_countries[, !(names(victim_countries) %in% drop)]
criminal_methods <- criminal_methods[, !(names(criminal_methods) %in% drop)]
crime_locations <- crime_locations[, !(names(crime_locations) %in% drop)]
base_locations <- base_locations[, !(names(base_locations) %in% drop)]
cases <- cases[, !(names(cases) %in% cases_drop)]

defendants <- defendants[c("case_id", "judge_id", "number_of_defendants",
                           "first_name", "last_name", "alias", "gender", "race",
                           "country_of_origin", "birth_year", "arrest_age",
                           "arrest_date", "detained", "bail_type", "bail_amount",
                           "felonies_charged", "felonies_sentenced", "date_terminated",
                           "sentenced_date", "total_sentence", "restitution",
                           "charged_with_forfeiture", "sentenced_with_forfeiture",
                           "appeal", "sup_release", "probation")]

#######################
### CREATE VARIABES ###
#######################

#u.s. census regions
northeast <- c("Connecticut", "CT", "Maine", "ME", "Massachusetts", "MA", 
               "New Hampshire", "NH", "Rhode Island", "RI", "Vermont", 
               "VT", "New Jersey", "NJ", "New York", "NY", "Pennsylvania", "PA", "US-Canada Border",
               "John F. Kennedy International Airport", "John F Kennedy Airport",
               "John F Kennedy International Airport", "Newark Airport", 
               "Philadelphia International Airport")

midwest <- c("Illinois", "IL", "Indiana", "IN", "Michigan", "MI", "Ohio", "OH",
             "Wisconsin", "WI", "Iowa", "IA", "Kansas", "KS", "Minnesota", 
             "MN", "Missouri", "MO", "Nebraska", "NE", "ND", "SD", 
             "North Dakota", "South Dakota", "John Glenn Columbus International Airport", 
             "Kansas City International Airport", "Minneapolis-St. Paul Airport", 
             "O'Hare International Airport")

south <- c("Delaware", "DE", "Florida", "FL", "Georgia", "GA", "Maryland", "MD",
           "North Carolina", "NC", "South Carolina", "SC", "Virginia", "VA", 
           "District of Columbia", "DC", "West Virginia", "WV", "Alabama", 
           "AL", "Kentucky", "KY", "Mississippi", "MS", "Tennessee", "TN",
           "Arkansas", "AR", "Louisiana", "LA", "Oklahoma", "OK", "Texas", "TX",
           "Dulles International Airport", "US-Mexico Border", 
           "United States-Mexico Border", "DFW Airport", "Rio Grande River")

west <- c("Arizona", "AZ", "Colorado", "CO", "Idaho", "ID", "Montana", "MT", 
          "Nevada", "NV", "New Mexico", "NM", "Utah", "UT", "Wyoming", "WY", 
          "Alaska", "AK", "California", "CA", "Hawaii", "HI", "Oregon", 
          "OR", "Washington", "WA", "San Francisco International Airport", 
          "Antonio B. Won Pat International Airport", "King County", 
          "Los Angeles Cruise Ship Docks", "Los Angeles International Airport",
          "Oakland International Airport", "Oroville, Okanogan County",
          "Pago Pago International Airport", "Sumas County", "Whatcom County")

#world bank international regions
#international victims - using world bank regions
east_asia_pacific <- c("Korea, Republic of", "Indonesia", "China", "Vietnam",
                       "Taiwan, Province of China", "Micronesia, Federated States of",
                       "Philippines", "Fiji", "Tonga", "American Samoa", "Guam",
                       "Thailand")
europe_cent_asia <- c("Azerbaijan", "Ukraine", "Czech Republic", "Hungary",
                      "Denmark", "Belarus", "Russia", "France", "Russian Federation",
                      "Estonia", "Germany", "Lithuania", "Bosnia and Herzegowina",
                      "Greece", "Romania", "Uzbekistan", "Moldova, Republic of",
                      "Kazakhstan", "Slovakia", "United Kingdom", "Aland Islands")
latin_am_carib <- c("Colombia", "Columbia", "Puerto Rico", "Nicaragua", "Peru",
                    "Honduras", "Mexico", "Guatemala", "El Salvador",
                    "Brazil", "Dominican Republic", "Jamaica", "Haiti", "Cuba")
middle_east_n_af <- c("Israel", "Saudi Arabia", "Egypt", "Morocco", "Jordan",
                      "Pakistan")
north_am <- c("Canada", "United States")
south_asia <- c("India")
sub_sah_africa <- c("Togo", "Ghana", "Cameroon", "Nigeria", "Somalia")

#region of crime location
crime_locations$region <- NA
crime_locations$region[crime_locations$state %in% south] <- "south"
crime_locations$region[crime_locations$state %in% northeast] <- "northeast"
crime_locations$region[crime_locations$state %in% west] <- "west"
crime_locations$region[crime_locations$state %in% midwest] <- "midwest"
crime_locations$region <- factor(crime_locations$region)
crime_locations$region <- factor(crime_locations$region, 
                                 levels(crime_locations$region)[c(1,2,4,3)])

#region of entry point to u.s.
entry_ports$region <- NA
entry_ports$region[entry_ports$name %in% south] <- "south"
entry_ports$region[entry_ports$name %in% northeast] <- "northeast"
entry_ports$region[entry_ports$name %in% west] <- "west"
entry_ports$region[entry_ports$name %in% midwest] <- "midwest"
entry_ports$region[entry_ports$state %in% south] <- "south"
entry_ports$region[entry_ports$state %in% northeast] <- "northeast"
entry_ports$region[entry_ports$state %in% west] <- "west"
entry_ports$region[entry_ports$state %in% midwest] <- "midwest"

#defendant region of origin
defendants$def_origin_region <- NA
defendants$def_origin_region[defendants$country_of_origin %in% east_asia_pacific] <- "east asia and pacific"
defendants$def_origin_region[defendants$country_of_origin %in% europe_cent_asia] <- "europe and central asia"
defendants$def_origin_region[defendants$country_of_origin %in% latin_am_carib] <- "latin america and caribbean"
defendants$def_origin_region[defendants$country_of_origin %in% north_am] <- "north america"
defendants$def_origin_region[defendants$country_of_origin %in% sub_sah_africa] <- "sub-saharan africa"
defendants$def_origin_region[defendants$country_of_origin %in% south_asia] <- "south_asia"
defendants$def_origin_region[defendants$country_of_origin %in% middle_east_n_af] <- "middle east and north africa"

#victim region of origin
victim_countries$origin_region <- NA
victim_countries$origin_region[victim_countries$victimcountry %in% east_asia_pacific] <- "east asia and pacific"
victim_countries$origin_region[victim_countries$victimcountry %in% europe_cent_asia] <- "europe and central asia"
victim_countries$origin_region[victim_countries$victimcountry %in% latin_am_carib] <- "latin america and caribbean"
victim_countries$origin_region[victim_countries$victimcountry %in% north_am] <- "north america"
victim_countries$origin_region[victim_countries$victimcountry %in% sub_sah_africa] <- "sub-saharan africa"

#type of trafficking
cases$trafficking_type <- NA
cases$trafficking_type[cases$adult_sex == "true"] <- "sex"
cases$trafficking_type[cases$minor_sex == "true"] <- "minor sex"
cases$trafficking_type[cases$labor == "true"] <- "labor"
cases$trafficking_type <- factor(cases$trafficking_type)
cases$trafficking_type <- factor(cases$trafficking_type, 
                                    levels(cases$trafficking_type)[c(1,3,2)])


#############
### PLOTS ###
#############

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


#international trafficking flows to the united states
int_vics <- filter(victim_countries, victimcountry != "United States")
origin_entry <- inner_join(int_vics, entry_ports)
origin_entry <- origin_entry[c("victimcountry", "name", "region", "origin_region")]
origin_entry <- filter(origin_entry, !is.na(name) & !is.na(region) & name != "Greyhound bus stop")

ggplot(origin_entry, aes(x=region, fill=origin_region)) +
  
  geom_bar(aes(y = (..count..) / sum(..count..))) +
  
  labs(y = "Percentage of Cases", x = "Point of Entry to the United States", 
       title=paste("Majority of international U.S. cases involve \nLatin American", 
                   "and Caribbean victims \ntransported to the U.S. via the", 
                   "\nsouthern border", sep=""),
       subtitle="International trafficking flows to the United States",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_fill_manual(name="Country of Origin",
                    values=c("#303841", "#F6C90E", "#40616d", "#9A9B94", "#EA9215"), 
                    labels=c("East Asia & Pacific", "Europe & Central Asia",
                             "Latin America & Caribbean", "North America",
                             "Sub-Saharan Africa")) +
  
  my_theme +
  
  theme(panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(margin=margin(t=-2))) +
  
  scale_x_discrete(labels=c("Midwest", "Northeast", "South", "West")) +
  scale_y_continuous(labels = c("0%", "20%", "40%", "60%"),
                     breaks = c(0, 0.2, 0.4, 0.6))

#save plot
ggsave("../plots/international_flows.pdf")




#cases by us region and trafficking type
case_loc <- inner_join(cases, crime_locations, by="case_id")

ggplot(filter(case_loc, !is.na(region)), aes(x=region, fill=trafficking_type)) +
  geom_bar(aes(y = (..count..)), position="dodge") + 
  coord_flip() +
  
  labs(y = "Number of Cases (2000-2016)", x = "U.S. Region", 
       title=paste("Human Trafficking prosecutions \nmost common in the South ",
                   "and \nfor sex trafficking of minors", sep=""),
       subtitle="Human trafficking cases by region and type",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_fill_manual(name="Type of Trafficking",
                    values=c("#9A9B94", "#F6C90E", "#303841"), 
                    labels=c("Labor", "Sex: \nadult victim(s)",
                             "Sex: \nminor victim(s)")) +
  
  my_theme +
  
  theme(panel.grid.major.x = element_line(color="black", size=0.25),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        axis.text.x = element_text(margin=margin(t=-2))) +
  
  guides(fill=guide_legend(keywidth=0.5, keyheight=0.5, default.unit="cm")) +
  scale_x_discrete(labels=c("Midwest", "Northeast", "West", "South"))

#save plot
ggsave("../plots/cases_by_region_by_type.pdf")



#waffle plot

#convert NA to 0
to_convert <- c("number_victims", 
                "number_victims_minor",
                "number_victims_foreign",
                "number_victims_female",
                "number_victims_male",
                "number_victims_unknown")

cases[to_convert][is.na(cases[to_convert])] <- 0

age <- c(`Adult (78%)` = sum(cases$number_victims) - 
                            sum(cases$number_victims_minor),
         `Minor (22%)` = sum(cases$number_victims_minor))

gender <- c(`Female (93%)`= 6537, 
            `Male (7%)` = 513)

origin <- c(`Non-U.S. Citizen (66%)` = sum(cases$number_victims_foreign),
           `U.S. Citizen (34%)` = sum(cases$number_victims) - 
                                    sum(cases$number_victims_foreign))

age_waffle <- waffle(age/50, rows=4, size=0.25, 
                      colors=c("#303841", "#F6C90E")) + 
  
                    labs(title="Victims in U.S. human trafficking prosecutions \noverwhelmingly adult, female, non-citizens",
                         subtitle="Age") +
                    
                    my_theme +
                    
                    theme(panel.background = element_blank(),
                          legend.key.size = unit(.32, "cm"),
                          axis.ticks.y = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text = element_blank(),
                          axis.title = element_text(margin=margin(t=15)),
                          plot.background = element_rect(size=0))

ggsave('../plots/age_waffle.pdf')


gender_waffle <- waffle(gender/50, rows=4, size=0.25, 
                  colors=c("#40616d", "#9A9B94")) + 
  
                  labs(subtitle="Gender") +
                  
                  my_theme +
                  
                  theme(panel.background = element_blank(),
                        legend.key.size = unit(.32, "cm"),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_text(margin=margin(t=15)),
                        plot.background = element_rect(size=0),
                        plot.margin=unit(c(0,1,1,1),"cm"))

ggsave('../plots/gender_waffle.pdf')

origin_waffle <- waffle(origin/50, rows=4, size=0.25, 
                  colors=c("#7d8b2e", "#EA9215")) + 
  
                  labs(subtitle="Nationality",
                       caption="Source: www.HumanTraffickingData.org",
                       x = "1 square = 50 victims") +
                  
                  my_theme +
                  
                  theme(panel.background = element_blank(),
                        legend.key.size = unit(.32, "cm"),
                        axis.ticks.y = element_blank(),
                        axis.ticks.x = element_blank(),
                        axis.text = element_blank(),
                        axis.title = element_text(margin=margin(t=15)),
                        plot.background = element_rect(size=0),
                        plot.margin=unit(c(0,1,1,1),"cm"))

ggsave('../plots/origin_waffle.pdf')

#it's called iron when you put them together how cute is that
iron(age_waffle, gender_waffle, origin_waffle)

