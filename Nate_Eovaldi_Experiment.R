library(tidyverse)
setwd("~/Desktop/Main/Eovaldi")
league<- read_csv("Savant_stats.csv")


#Download my baseball - savant custom leaderboard with 2021 league wide stats
View(league)
sum(is.na(league))

#Renaming my usage columns early for readability later on
colnames(league)[which(names(league) == "n_ff_formatted")] <- "4seam_%age"
colnames(league)[which(names(league) == "ff_avg_speed")] <- "Avg_FB_speed"
colnames(league)[which(names(league) == "n_sl_formatted")] <- "Slider_%age"
colnames(league)[which(names(league) == "sl_avg_spin")] <- "Avg_sl_spin"
colnames(league)[which(names(league) == "sl_avg_break")] <- "Avg_sl_break"
colnames(league)[which(names(league) == "n_ch_formatted")] <- "Changeup_%age"
colnames(league)[which(names(league) == "n_cukc_formatted")] <- "Curve_%age"
colnames(league)[which(names(league) == "cu_avg_spin")] <- "Avg_curve_spin"
colnames(league)[which(names(league) == "cu_avg_break")] <- "Avg_curve_break"
colnames(league)[which(names(league) == "n_sift_formatted")] <- "Sinker_%age"
colnames(league)[which(names(league) == "si_avg_spin")] <- "Avg_sinker_spin"
colnames(league)[which(names(league) == "si_avg_break")] <- "Avg_sinker_break"
colnames(league)[which(names(league) == "n_fc_formatted")] <- "Cutter_%age"
colnames(league)[which(names(league) == "n_fs_formatted")] <- "Splitter_%age"
colnames(league)[which(names(league) == "n_fastball_formatted")] <- "Fastball_%age"
colnames(league)[which(names(league) == "n_breaking_formatted")] <- "Breakingball_%age"
colnames(league)[which(names(league) == "n_offspeed_formatted")] <- "Offspeed_%age"


E_usage <- league %>%
  select(c(,1:2, 16:32))

E_usage <- E_usage%>%
  filter(last_name == "Eovaldi" & first_name == "Nathan")
sum(is.na(E_usage))

#Lets look at how Eovaldi has attacked the strikezone in the last 6 years... 
Zone_changes <- read_csv("inZ_percentage.csv")


Zone_changes <- Zone_changes%>%
  select(c(1:4,6,8,10,12,14))
Eovaldi_changes <-Zone_changes%>%
  filter(Rk. == "90")

# Turning my yearly values into numeric to be able to plot
Eovaldi_changes <- transform(Eovaldi_changes, Eovaldi_changes$`2015` == as.numeric(Eovaldi_changes$`2015`),
                             Eovaldi_changes$`2016` == as.numeric(Eovaldi_changes$`2016`),
                             Eovaldi_changes$`2017` == as.numeric(Eovaldi_changes$`2017`),
                             Eovaldi_changes$`2018` == as.numeric(Eovaldi_changes$`2018`),
                             Eovaldi_changes$`2019` == as.numeric(Eovaldi_changes$`2019`),
                             Eovaldi_changes$`2020` == as.numeric(Eovaldi_changes$`2020`),
                             Eovaldi_changes$`2021` == as.numeric(Eovaldi_changes$`2021`))
sapply(Eovaldi_changes, class)
sapply(Eovaldi_changes, mode)

# I cant graph the Eovaldi_changes d.f as its constructed bc the years are columns, gonna take the
# data in that table and construct a new data frame that will be more suitable for graphing.

better_table <-data.frame("Player"= c("Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi"),
                          "Year" = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
                          "InZone" = c(47.6, 52.6, 53.7, 54.2, 51, 52.2, NA, 55, 48.3, 53.6, 55.6))



# Lets create a plot to display the yearly changes in Eovaldi's zone usage.
# We can see up and down performance from Eovaldi but there is one good take away..
# 2021 Eovaldi has had a career-high in pounding the zone... at the same time having a 3.52 ERA
# the second lowest of his carreer (3.39 in '13 with MIA when he had 60 less IP)
# 2019 Eovaldi had his worst career ERA.. the same year he had his worse INZone% since his rookie year
library(ggplot2)
ggplot(better_table, aes(Year, InZone))+
  geom_line(color = "red")+
  labs(y= "In-Zone Percentage",
       title = "Yearly changes in in-zone usage for Nate Eovaldi",
       subtitle = "Gap exists due to Eovaldi missing the 2017 season")+
  annotate(geom = "text", x = 2019, y = 48, label ="5.99 ERA")+
  annotate(geom = "text", x = 2021, y = 56, label = "3.52 ERA")+
  scale_y_continuous(limits = c(20, 60), breaks = c(20, 30, 40, 50, 60))+
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021))

# What has Eovaldi done with his Walks?
# Using data found on Baseball Savant, create a new df containing Eovaldi's yearly walk rate.

BB_percentage <- data.frame("Player"= c("Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi","Nathan Eovaldi"),
                            "Year" = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
                            "BB_percentage" = c(13.7, 8.9, 8.9, 5.0, 7.3, 7.6, NA, 4.4, 11.6, 3.5, 4.4))
league_avg<- mean(league$p_bb_percent)


with(BB_percentage, plot(Year, BB_percentage, type = 'l', main = "Eovaldi Year-to-Year Walk Percentage", ylab = "BB%"))
with(BB_percentage, lines(Year, BB_percentage, col = "red"))
with(BB_percentage, identify(Year, BB_percentage, BB_percentage, n = 3, pos = 4))
abline(h=league_avg)
text(2016, 9.7, "2021 League Walk-Ratio AVG")

### My hypothesis for this previous set of code was that Eovaldi sort of struggled in the 
# begging of his career because he was sort of a wild pitcher... suprisingly enough
# Eovaldi was actually consistently below league average Walk Rate after his rookie year
# until he having to get Tommy John in '17. 
# '18 when he was crucial to the Sox championship, Eovaldi was back to form serving a career best
# 3.5 BB%.

### ONE THING I WOULD DO DIFFERENTLY LOOKING BACK...
# Make the League walk ratio avg change from year to year to find that
# year's respective BB% rather than just show the 2021 one, every year is different.

##Let's take a look at Eovaldi's fastball usage... 
Eovaldi_pitchTrack <- read.csv("Eovaldi_pitch_track.csv")

# For the time being we'll only use columns related to Eovaldi's pitches splits...
# When we want to look at statcast we'll have to refer to the original eovaldi_pitchtrack
Eovaldi_arsenal_split <- Eovaldi_pitchTrack %>%
  select(c(, 1:7))
glimpse(Eovaldi_arsenal_split)

# What is Eovaldi's fastball usage look like??
# Because cutters and split fingers are still considered fastballs, we will look at those too
Eovaldi_fastballs <- subset(Eovaldi_arsenal_split, Pitch.Type == "4-Seam Fastball" | Pitch.Type == "Cutter" | Pitch.Type == "Split Finger" | Pitch.Type == "Sinker")
glimpse(Eovaldi_fastballs) 

## Using ddply we are able to group the pitches by year and then average the usage percentage
# for each of the pitches
Eovaldi_Usage <- ddply(Eovaldi_fastballs, .(Year), summarize,
                           total_usage = round(mean(PERCENTAGE), 2))
glimpse(Eovaldi_Usage)

ggplot(Eovaldi_Usage, aes(Year, total_usage))+
  geom_line()+
  labs(title = "Nathan Eovaldi's Fastball Usage Throughout Career",
       subtitle = "This graph doesn't contain a gap because the source completely removed 2017 from the databse",
       y = "Fastball Usage %")+
  scale_y_continuous(limits = c(10, 40), breaks = c(10, 15, 20, 25, 30, 35, 40))+
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2012, 2014, 2016, 2018, 2020))+
  geom_text(x = 2013, y = 37.5, label = "3.59 FIP")+
  geom_text(x = 2016, y = 17.5, label = "4.97 FIP")+
  geom_text(x = 2021, y= 21, label = "2.79 FIP")

#So ... suprisingly enough in 2021 Eovaldi has actually found a middle sweet spot between being over reliant on the fastballs (2013)
# and not using enough.. Resulting in the first time Eovaldi has had a Fielding Independent Pitching under 3
# Now lets look at how each one of the fastball types has been used individually. 
FB_type_usage <- ddply(Eovaldi_fastballs, .(Year, Pitch.Type),  summarise,
                       total_usage = round(mean(PERCENTAGE),2))

ggplot(FB_type_usage, aes(Year, total_usage, color= Pitch.Type))+
  geom_line()+
  labs(title = "Eovaldi's Fastball Type Usage Throughout Career",
       y = "Fastball Usage")+
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2012, 2014, 2016, 2018, 2020))

### It seems like as Eovaldi's career was developing he was begging to rely on his cutter jsut as much
### as his 4-seam, as one constantly rose the other began to decline. 
# In '21 he has established his 4-seam (96.8 MPH) as his main pitch and used his cutter to work 
# along his split finger as a secondary 50-50 fastball that a hitter must keep in the back of their head.
# Also worth noting, Eovaldi completely dropped his Sinker after Tommy John

#So how is Eovaldi mixing in the rest of his arsenal away from fastballs??
#Lets look at his entire repetoire over the years
arsenal_usage <- ddply(Eovaldi_arsenal_split, .(Year, Pitch.Type),  summarise,
                       total_usage = round(mean(PERCENTAGE),2))

ggplot(arsenal_usage, aes(Year, total_usage, color = Pitch.Type))+
  geom_line()+
  geom_vline(aes(xintercept = 2018))+
  annotate(geom = "text", x = 2018, y = 60, label = "Traded to Red Sox")+
  labs(title = "Nathan Eovaldi's Individual Pitch Usage Throughout Career",
       subtitle = "No exisiting gap in the graph because Baseball-Savant completely removed his missed 2017 season",
       y = "Pitch Percentage")+
  scale_x_continuous(limits = c(2011, 2021), breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2018, 2019, 2020, 2021))


## How good has Eovaldi been at making bats miss ?
IP<- read_csv("2021_IP.csv")

IP <- IP%>%
  select(c(1:5))

Whiff <- league[c(1:2,15)]

C_Whiff <-merge(Whiff, IP)

#Lets look at qualified pitchers... according to FanGraphs, as of Sept. 17,2021, Yu Darvish has the 
# least amount of innings by a qualified pitcher at 147 IP 

Q_Whiff <- subset(C_Whiff, IP >= 147)
arrange(Q_Whiff, desc(whiff_percent))

#Looking at his Whiff%... nothing really impresses which is shocking.








 












 