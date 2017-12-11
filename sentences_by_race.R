library("tidyverse")
library(extrafont)
loadfonts()

setwd('~/Desktop/repos/dataviz/project/data/')

#################
### LOAD DATA ###
#################

defendants <- read_csv("htd/defendants.csv")
judges <- read_csv("htd/judges.csv")

################
### CLEANING ###
################

#remove unwanted columns (rewrite to avoid repetition)
judges <- judges[c("id", "race")]
defendants <- defendants[c("judge_id", "race", "total_sentence")]

#create factors
defendants$race <- factor(defendants$race)
levels(defendants$race) <- c("white", "black", "hispanic", "asian", "indian", "other")

#defendant
defendants <- filter(defendants, total_sentence < 999)
defendants$total_sentence <- defendants$total_sentence / 12

#rename
colnames(judges) <- c("judge_id", "judge_race")
colnames(defendants) <- c("judge_id", "defendant_race", "total_sentence")

judges$judge_race <- factor(judges$judge_race)

#join
traf <- inner_join(defendants, judges, by='judge_id')

max <- mean(traf$total_sentence) + 2 * sd(traf$total_sentence)
traf <- filter(traf, total_sentence < max)
traf <- filter(traf, !is.na(defendant_race) & !is.na(total_sentence) & !is.na(judge_race))

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


ggplot(filter(traf, defendant_race != "indian" & defendant_race != "other" & judge_race != 'Other')) +
  
  geom_boxplot(aes(x=judge_race, y=total_sentence, fill=judge_race), 
               width=0.75, outlier.shape=NA) +
  
  geom_boxplot(aes(x=defendant_race, y=total_sentence, fill=defendant_race), 
               width=0.75, outlier.shape=NA) +
  
  labs(y = "Length of Prison Sentence (years)", 
       title=paste("Black defendants receive the longest", 
                   "\nsentences for human trafficking, while",
                   "\nAsian judges deliver the shortest sentences", sep=""),
       subtitle="Varying prison stays for perpetrators of human trafficking",
       caption="Source: www.HumanTraffickingData.org") +
  
  scale_fill_manual(name="Judge Race",
                    values=c("#262c33", "#546373", "#b3920a", "#f2c60e", 
                             "#7f807a", "#bebfb6", "#bf7711", "#ff9f17"),
                    labels=c("Asian Defendant", "Asian Judge", "Black Defendant",
                             "Black Judge", "Hispanic Defendant", "Hispanic Judge",
                             "White Defendant", "White Judge")) +
  
  my_theme +
  
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  
  scale_y_continuous(limits=c(0, 25)) +
  
  guides(fill=guide_legend(keywidth=0.5, keyheight=1, default.unit="cm")) +
  
  scale_x_discrete(labels=c("Asian \ndefendant", "Asian \njudge", 
                            "Black \ndefendant", "Black \njudge",
                            "Hispanic \ndefendant", "Hispanic \njudge",
                            "White \ndefendant", "White \njudge"))


#ggsave('../plots/by_race.pdf')