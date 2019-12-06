#Exam 4
install.packages("tidyverse")
install.packages("ggmap")
install.packages("osmdata")
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(plyr)
library(ggmap)
library(osmdata)
getwd()

# Spectral colour map from ColorBrewer
spectral <- function(n=6) {
  library("RColorBrewer")
  rev(brewer.pal(name="Spectral", n=n))
}


scale_fill_spectral <- function(...) {
  scale_fill_gradientn(colours=spectral(...))
}

scale_colour_spectral <- function(...) {
  scale_colour_gradientn(colours=spectral(...))
}

##Section 1, Bar Plot:

load("sec_stad.Rdata")
a=sec_stad

#Generate a bar plot showing the capacity of each football stadium. (4 points)

ggplot(data = a) +
  geom_bar(aes(x=Name, y=Capacity),
             position="dodge",
           stat="Identity")

ggplot(data = a) +
  geom_bar(aes(x=Name, y=Capacity, fill=Capacity),
           position="dodge",
           stat="Identity")


# Flip the axis so that the stadium names are arranged along the left side of the plot and the
#values are on the bottom of the plot. (2 points)
ggplot(data = a) +
  geom_bar(aes(x=Capacity, y=Name),
           position="dodge",
           stat="Identity") 

ggplot(data = a) +
  geom_bar(aes(x=Capacity, y=Name, fill=Capacity),
           position="dodge",
           stat="Identity") 


# Set the x-axis text to be a 90-degree angle. (2 points)

ggplot(data = a) +
  geom_bar(aes(x=Name, y=Capacity),
           position="dodge",
           stat="Identity")+
  theme(axis.text.x=element_text(angle=90))

ggplot(data = a) +
  geom_bar(aes(x=Capacity, y=Name),
           position="dodge",
           stat="Identity")+
  theme(axis.text.x=element_text(angle=90))


#Section 2. Multiple Points plot ____ / 5 points
load("team_statistics.Rdata")
b=ts

# Select teams belonging to the Sun Belt Conference from the data set. Generate a point plot using
#‘ggplot’ to illustrate if there is a relationship between the number of passing yards and the number
#of rushing yard for each team

b[b$Conference=="Sun Belt Conference",]
c=b[b$Conference=="Sun Belt Conference",]
c
ggplot(data=c, aes(x=Rush.Yard, y=Pass.Yard, colour=Team)) +
  geom_point()


##Section 3: Box-and-whisker plot
# For teams in the Big 10 Conference, generate a box-and-whisker plot for rushing yards. (6 points)

unique(b$Conference)
# Big Ten Conference Data frame
d=b[b$Conference=="Big Ten Conference",]

ggplot(data=d, aes(x=Team, y=Rush.Yard, fill=Team)) +
  geom_boxplot() 
  


# Make the panel background dark blue and the fill of the box-and-whiskers bright yellow (2 points)

ggplot(data=d, aes(x=Team, y=Rush.Yard)) +
  geom_boxplot(fill="yellow", colour="yellow")+
  theme_classic()+
  theme(panel.background = element_rect(fill='darkblue'))
  

# Using this subset of data, tell me which team had the most rushing yards, on average? (2 points)
# Minesotta had the highest rushing yards
team_max=max(d$Rush.Yard)

# Minesotta had the highest rushing yards
team_max_order=d[order(-d$Rush.Yard),]
team_with_highest_rushing_yard=team_max_order[1,]
team_with_highest_rushing_yard


##Section:4 Heat Map _____
load("football_stats.Rdata")
e=football.stats

# For teams in the Southeastern conference, generate a heat map with ‘ggplot’ showing each team’s
# performance for each statistical variable. (6 points)

h=e[e$Conference=="Southeastern Conference",]


heatmap=ggplot(data=h, aes(x=variable, y=stat, colour=Team))+
  geom_tile()
heatmap

#Plot the log-10 transformed values for the color scale. (2 points)
ggplot(data=h, aes(x=variable, y=stat, colour=Team))+
  geom_tile()+
  scale_y_log10()


#Section 5: Mapping _____ / 20 points
load("sec_stad.Rdata")
a=sec_stad
#Generate a ‘toner’ map figure showing the central and southeastern United States. (5 points)


bb=c(left=min(a$lng),bottom=min(a$lat),
     right=max(a$lng),top=max(a$lat))
SEC.map=get_stamenmap(bbox=bb,zoom=6,
                      map='toner')
ggmap(SEC.map)
ggsave('.png')
# Generate a second ‘toner’ map figure showing the central and southeastern United States, plus the
#geographic location of all the SEC stadiums (5 points).


ggmap(SEC.map)+
 geom_point(data=a,aes(x=lng,y=lat))
ggsave('map.png')

# Generate a third ‘toner’ map figure showing the geographic location of all the SEC stadiums, this
#time represent these locations using the following aesthetics:
# Size of the points scale with the ‘Capacity’ of each stadium (1 points)
# The colors of the points show degree of gradation in ‘Capacity’ among stadiums (3 points)


ggmap(SEC.map)+
  geom_point(data=a,aes(x=lng,y=lat,color=Capacity,size=Capacity))
ggsave('point.png')

# Export and save these three maps as “.png” files. Please include them when you upload to Moodle
#(or email) your exam. (6 points total; i.e., 2 points per map file)

getwd()
dir.create("C:/Users/anons/OneDrive/Desktop/R programming/Exam-4-R-")
ggsave(".tiff")


##Section 6: Analysis
#In which state is the largest capacity stadium located? (2 points)
L_cap=max(a$Capacity)
L_cap


L_cap_order=a[order(-a$Capacity),]
L_cap_order
State_with_largest_capacity=L_cap_order[1,]
State_with_largest_capacity
# state with largest capacity stadium is TN


# What is the mean and standard deviation capacity of the stadiums in each state? 
#(Hint: you may need to use detach(package:plyr) ) (2 points)

library(plyr)
detach(package:plyr)

mean.SD.capacity = a %>%  group_by(State) %>% summarize (Mean.Cap = mean(Capacity),SD.Cap=sd(Capacity))
mean.SD.capacity
