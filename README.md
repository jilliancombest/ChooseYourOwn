# Title: HarvardX Captsone: ChooseYourOwn
# Subtitle: Most Popular Video Game Console: 1980-2017
# Author: Jillian Combest


1. Introduction

# import of several libraries that will be used in project
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)

options(warn = -1)

# Import and read dataset
Video_Games_Sales_as_at_22_Dec_2016 <- read.csv("~/Downloads/Video_Games_Sales_as_at_22_Dec_2016.csv")
View(Video_Games_Sales_as_at_22_Dec_2016)

# Examine the data types of every variable
 str(Video_Games_Sales_as_at_22_Dec_2016)

 dim(Video_Games_Sales_as_at_22_Dec_2016)

 # Examine statistical details of non-categorical variables
summary(Video_Games_Sales_as_at_22_Dec_2016)

# Examine the details of categorical variables
categorical <- names(Video_Games_Sales_as_at_22_Dec_2016)[sapply(Video_Games_Sales_as_at_22_Dec_2016, is.character)]
print(categorical)

summary(Video_Games_Sales_as_at_22_Dec_2016[categorical])



2. Data Cleaning 


 # Count of NA values
sum(is.na(Video_Games_Sales_as_at_22_Dec_2016))

# Sum of NA values per column
colSums(is.na(Video_Games_Sales_as_at_22_Dec_2016))

url <- na.omit(Video_Games_Sales_as_at_22_Dec_2016)

# Convert "Year of Release" variable to an object
url$Year_of_Release <- as.character(url$Year_of_Release)
head(url$Year_of_Release)

# Check unique values of Platform
unique(url$Platform)

# Copy and add one more platform column to the end of dataset
url$Platform_General <- url$Platform

#Convert console subnames to the general names
url$Platform_General[url$Platform == 'PS3'] <- 'Sony_Playstation'
url$Platform_General[url$Platform == 'PS'] <- 'Sony_Playstation'
url$Platform_General[url$Platform == 'PS2'] <- 'Sony_Playstation'
url$Platform_General[url$Platform == 'PS4'] <- 'Sony_Playstation'
url$Platform_General[url$Platform == 'PSP'] <- 'Sony_Playstation'
url$Platform_General[url$Platform == 'PSV'] <- 'Sony_Playstation'
url$Platform_General[url$Platform == 'Wii'] <- 'Nintendo'
url$Platform_General[url$Platform == 'DS'] <- 'Nintendo'
url$Platform_General[url$Platform == 'GBA'] <- 'Nintendo'
url$Platform_General[url$Platform == '3DS'] <- 'Nintendo'
url$Platform_General[url$Platform == 'WiiU'] <- 'Nintendo'
url$Platform_General[url$Platform == 'NES'] <- 'Nintendo'
url$Platform_General[url$Platform == 'SNES'] <- 'Nintendo'
url$Platform_General[url$Platform == 'N64'] <- 'Nintendo'
url$Platform_General[url$Platform == 'GB'] <- 'Nintendo'
url$Platform_General[url$Platform == 'GC'] <- 'Nintendo'
url$Platform_General[url$Platform == 'X360'] <- 'Microsoft_Xbox'
url$Platform_General[url$Platform == 'XB'] <- 'Microsoft_Xbox'
url$Platform_General[url$Platform == 'XOne'] <- 'Microsoft_Xbox'
url$Platform_General[url$Platform == '2600'] <- 'Atari'
url$Platform_General[url$Platform == 'DC'] <- 'Sega'
url$Platform_General[url$Platform == 'SAT'] <- 'Sega'
url$Platform_General[url$Platform == 'GG'] <- 'Sega'
url$Platform_General[url$Platform == 'WS'] <- 'Bandal'
url$Platform_General[url$Platform == 'TG16'] <- 'Nec'
url$Platform_General[url$Platform == 'PCFX'] <- 'Nec'
url$Platform_General[url$Platform == '3DO'] <- 'Panasonic'


# Check uniq values of Platform_General
unique(url$Platform_General)



3. Generic Data Analysis

c(1980:2017)

ggplot(url, aes(x = Year_of_Release)) +
  geom_bar(fill = BuPu) +
  scale_x_discrete(breaks = seq.default(min(url$Year_of_Release), max(url$Year_of_Release), by = 2)+
  labs(x = Year_of_Release, y = Count_of_Games, title = Total_Count_of_Release_Games_per_Year)
