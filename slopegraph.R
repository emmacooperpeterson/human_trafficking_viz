library(tidyverse)
library(extrafont)
library(lubridate)
library(slopegraph)
library(reshape2)
loadfonts()

setwd('~/Desktop/Repositories/dataviz/project/data/')

#################
### LOAD DATA ###
#################

cases <- read_csv("htd/cases.csv")

cases$year <- substr(cases$start_date, 1, 4)
state_by_year <- summarize(group_by(cases, state, year), num_cases = n())
state_by_year <- filter(state_by_year, !is.na(state) & !is.na(year))
state_by_year_wide <- dcast(state_by_year, state ~ year)
state_by_year_wide[is.na(state_by_year_wide)] <- 0

#remove 2015 and 2016 because their data isn't complete
state_by_year_wide <- state_by_year_wide[, 1:16]

#add ranks
rank <- seq(1,50)

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2000', decreasing = TRUE),]
state_by_year_wide$'2000' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2001', decreasing = TRUE),]
state_by_year_wide$'2001' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2002', decreasing = TRUE),]
state_by_year_wide$'2002' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2003', decreasing = TRUE),]
state_by_year_wide$'2003' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2004', decreasing = TRUE),]
state_by_year_wide$'2004' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2005', decreasing = TRUE),]
state_by_year_wide$'2005' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2006', decreasing = TRUE),]
state_by_year_wide$'2006' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2007', decreasing = TRUE),]
state_by_year_wide$'2007' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2008', decreasing = TRUE),]
state_by_year_wide$'2008' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2009', decreasing = TRUE),]
state_by_year_wide$'2009' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2010', decreasing = TRUE),]
state_by_year_wide$'2010' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2011', decreasing = TRUE),]
state_by_year_wide$'2011' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2012', decreasing = TRUE),]
state_by_year_wide$'2012' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2013', decreasing = TRUE),]
state_by_year_wide$'2013' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2014', decreasing = TRUE),]
state_by_year_wide$'2014' <- rank

state_by_year_wide <- state_by_year_wide[order(state_by_year_wide$'2000', decreasing = TRUE),]

rownames(state_by_year_wide) <- state_by_year_wide[,1]
state_by_year_wide <- state_by_year_wide[,-1]
state_by_year_wide <- state_by_year_wide[c('2000', '2002', '2004', '2006', '2008', '2010', '2012', '2014')]

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

cols <- rep("#9A9B94", 50)
cols[6] <- "#303841"
cols[44] <- "#EA9215"

widths <- rep(0.15, 50)
widths[c(6,44)] <- 1

ggslopegraph(state_by_year_wide,  offset.x = 0.13, 
             yrev = TRUE, col.lines = cols, col.lab = cols, 
             col.num=cols, lwd=widths,
             xlab = "Year", 
             ylab = "Rank in HT Prosecutions (1=most prosecutions)") +
  
  labs(title="Trafficking prosecutions have increased \nthe most in South Dakota, decreased \nthe most in Alaska",
       subtitle="Relative rank in number of trafficking prosecutions per year",
       caption="Source: www.HumanTraffickingData.org") +
  
  theme(axis.title.y = element_text(margin=margin(r=0))) +
  
    my_theme

ggsave("../plots/slopegraph.pdf", height=12)
