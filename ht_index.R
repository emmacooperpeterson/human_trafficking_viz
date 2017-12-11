library(tidyverse)
library(extrafont)
library(haven)
loadfonts()

setwd('~/Desktop/repos/dataviz/project/data/')

##############################
### LOAD + MANIPULATE DATA ###
##############################

#human development index
hdi <- read_csv("hdi/2016_Statistical_Annex_Table_1.csv")
hdi <- hdi[c("X2", "HDI")]
colnames(hdi) <- c("country", "hdi_score")
hdi <- filter(hdi, !is.na(hdi_score))

#human trafficking index
three_p <- read_csv("3p/3P_Index_2015.csv")
three_p <- three_p[c("Country", "Overall 3P")]
colnames(three_p) <- c("country", "ht_index")
three_p <- filter(three_p, !is.na(ht_index))

#number of prosecutions by country 2000-2011
hti <- read_dta("hti/hti_data.dta")
hti <- hti[c("country", "prosnum")]
hti <- filter(hti, !is.na(prosnum))
hti <- dplyr::summarise(group_by(hti, country), prosecutions = sum(prosnum))

#combine all three
combined <- inner_join(hdi, three_p, by="country")
combined <- inner_join(combined, hti, by="country")

#world bank regions
east_asia_pacific <- c("Korea, Republic of", "Indonesia", "China", "Vietnam",
                       "Taiwan, Province of China", "Micronesia, Federated States of",
                       "Philippines", "Fiji", "Tonga", "American Samoa", "Guam",
                       "Thailand", "New Zealand", "Japan", "Malaysia", "Mongolia",
                       "Kiribati", "Papua New Guinea", "Australia", "Palau", 
                       "Timor-Leste", "Cambodia", "Singapore")

europe_cent_asia <- c("Azerbaijan", "Ukraine", "Czech Republic", "Hungary",
                      "Denmark", "Belarus", "Russia", "France", "Russian Federation",
                      "Estonia", "Germany", "Lithuania", "Bosnia and Herzegowina",
                      "Greece", "Romania", "Uzbekistan", "Moldova, Republic of",
                      "Kazakhstan", "Slovakia", "United Kingdom", "Aland Islands", "Norway",
                      "Ireland", "Austria", "Spain", "Montenegro", "Iceland", "Sweden",
                      "Belgium", "Slovenia", "Cyprus", "Poland", "Croatia",
                      "Bulgaria", "Georgia", "Albania", "Armenia", "Tajikistan",
                      "Switzerland", "Luxembourg", "Finland", "Italy", "Malta",
                      "Portugal", "Latvia", "Serbia", "Turkey", "Turkmenistan",
                      "Afghanistan")

latin_am_carib <- c("Colombia", "Columbia", "Puerto Rico", "Nicaragua", "Peru",
                    "Honduras", "Mexico", "Guatemala", "El Salvador", "Nigeria",
                    "Brazil", "Dominican Republic", "Jamaica", "Haiti", "Cuba",
                    "Chile", "Argentina", "Uruguay", "Antigua and Barbuda", 
                    "Trinidad and Tobago", "Ecuador", "Suriname", "Paraguay",
                    "Nicaragua", "Guyana", "Costa Rica", "Belize", "Barbados",
                    "Panama")

middle_east_n_af <- c("Israel", "Saudi Arabia", "Egypt", "Morocco", "Jordan",
                      "Pakistan", "Qatar", "United Arab Emirates", "Algeria",
                      "Oman", "Tunisia", "Iraq", "Yemen", "Djibouti", "Bahrain",
                      "Kuwait", "Lebanon")

north_am <- c("Canada", "United States")

south_asia <- c("India", "Sri Lanka", "Bangladesh", "Maldives", "Nepal")

sub_sah_africa <- c("Togo", "Ghana", "Cameroon", "Nigeria", "Somalia",
                    "South Africa", "Zambia", "Kenya", "Angola",
                    "Madagascar", "Lesotho", "Uganda", "Benin", "Malawi", "Mali",
                    "Eritrea", "Guinea", "Chad", "Seychelles", "Botswana",
                    "Zimbabwe", "Rwanda", "Senegal", "Sudan", "Liberia",
                    "Sierra Leone", "Burundi", "Niger", "Mauritius", "Gabon",
                    "Namibia", "Equatorial Guinea", "Swaziland", "Comoros",
                    "Ethiopia", "Guinea-Bissau", "Mozambique", "Burkina Faso",
                    "Central African Republic", "Mauritania")

#add region column
combined$region <- NA
combined$region[combined$country %in% east_asia_pacific] <- "east asia and pacific"
combined$region[combined$country %in% europe_cent_asia] <- "europe and central asia"
combined$region[combined$country %in% latin_am_carib] <- "latin america and caribbean"
combined$region[combined$country %in% north_am] <- "north america"
combined$region[combined$country %in% sub_sah_africa] <- "sub-saharan africa"
combined$region[combined$country %in% south_asia] <- "south_asia"
combined$region[combined$country %in% middle_east_n_af] <- "middle east and north africa"

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

ggplot(combined, aes(x=hdi_score, y=ht_index)) +
  geom_jitter(aes(color=region, size=prosecutions), alpha=0.75) +
  geom_smooth(se=FALSE, color="black") +
  
  labs(y = "Human Trafficking Index (15 = best score)", x = "Human Development Index (1=best score)", 
       title="Local quality of life associated with stronger \nanti-trafficking policies",
       subtitle="Human trafficking & human development",
       caption="Source: '3P' Anti-trafficking Policy Index & U.N. Human Development Index") +
  
  my_theme +
  
  theme(plot.subtitle = element_text(family="Courier New", margin=margin(b=10)),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, hjust = 1, margin=margin(t=-10)),
        axis.title = element_text(family="Montserrat", size=8)) +
  
  scale_color_manual(name="Region",
                     values=c("#303841", "#F6C90E", "#40616d", "#141607", 
                              "#9A9B94", "#7d8b2e", "#EA9215"), 
                     labels=c("East Asia & Pacific", "Europe & Central Asia",
                              "Latin America & Caribbean", "Middle East & North Africa", 
                              "North America", "South Asia", "Sub-Saharan Africa")) +
  scale_size_continuous(range=c(1,10), name="Number of Prosecutions \n(2000-2011)") +
  
  scale_y_continuous(breaks=c(4,6,8,10,12,14),
                     labels=c(4,6,8,10,12,14)) +
  
  scale_x_continuous(breaks=seq(0,1,by=0.2),
                     labels=seq(0,1,by=0.2))

#save plot
ggsave("../plots/human_dev_human_traf.pdf")



#larger plot for illustrator
ggplot(combined, aes(x=hdi_score, y=ht_index)) +
  geom_jitter(aes(color=region, size=prosecutions), alpha=0.75) +
  geom_smooth(se=FALSE, color="black") +

  my_theme +
  
  theme(plot.subtitle = element_text(family="Courier New", margin=margin(b=10)),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, hjust = 1, margin=margin(t=-10)),
        axis.title = element_text(family="Montserrat", size=8)) +
  
  scale_color_manual(name="Region",
                     values=c("#303841", "#F6C90E", "#40616d", "#141607", 
                              "#9A9B94", "#7d8b2e", "#EA9215"), 
                     labels=c("East Asia & Pacific", "Europe & Central Asia",
                              "Latin America & Caribbean", "Middle East & North Africa", 
                              "North America", "South Asia", "Sub-Saharan Africa")) +
  scale_size_continuous(range=c(1,10), name="Number of Prosecutions \n(2000-2011)") +
  
  scale_y_continuous(breaks=c(4,6,8,10,12,14),
                     labels=c(4,6,8,10,12,14)) +
  
  scale_x_continuous(breaks=seq(0,1,by=0.2),
                     labels=seq(0,1,by=0.2))

#save plot
ggsave("../plots/human_dev_human_traf_big.pdf", width=10, height=7, units='in')
