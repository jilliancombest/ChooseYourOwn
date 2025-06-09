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


4. Statistical Analysis

url_platform <- url %>%
  filter(Platform_General %in% c('Sony_Playstation', 'Microsoft_Xbox', 'Nintendo')) %>%
  mutate(dummy_count = as.numeric(1),
         User_Score = as.numeric(User_Score)) %>%
  summary()


url_platform$`Global_Sales`[c(0, 0.01, 0.25, 0.5, 0.75, 0.95, 1)]

url_platform <- url_platform[url_platform$Global_Sales < quantile(url_platform$Global_Sales, 0.95)]

quantile(url_platform$Global_Sales, c(0, 0.01, 0.25, 0.5, 0.75, 0.95, 1))


# First group by the data

urlgc <- urla_platform %>%
  group_by(Year_of_Release, Platform_General, Platform)

# Second add counts according to group_by info and then join with the statistical information required

counts <- vdpg %>%
  summarise(counts = n()) %>%
  as.data.frame()

global_sales <- vdpg %>%
  summarise(Global_Sales_M = sum(Global_Sales)) %>%
  as.data.frame()

url_df_pf_grouped <- merge(counts, global_sales, by = c("Year_of_Release", "Platform_General", "Platform"))

url_df_pf_grouped$GS_Amount_per_Game <- url_df_pf_grouped$Global_Sales_M / url_df_pf_grouped$counts
url_df_pf_grouped[] <- lapply(url_df_pf_grouped, function(x) as.numeric(as.character(x)))


url_df_pf_grouped %>%
  filter(Platform_General == 'Sony_Playstation') %>%
  summary()

url_df_pf_grouped %>%
  filter(Platform_General == 'Microsoft_Xbox') %>%
  summary()

url_df_pf_grouped %>%
  filter(Platform_General == 'Nintendo') %>%
  summary()


# Make a custom palette with platform colors
pal <- list(Sony_Playstation = "#6495ED", Microsoft_Xbox = "#F08080", Nintendo = "Green")

# Show the survival proability as a function of platforms
g <- ggplot(url_df_pf_grouped, aes(x = Year_of_Release, y = GS_Amount_per_Game, color = Platform_General)) +
  geom_point(position = position_jitter(height = 0.05)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ Platform_General) +
  scale_color_manual(values = pal)

# Use more informative axis labels than are provided by default
g <- g + labs(x = "Year of Release", y = "Global Sales per Game Released (M#)")
g <- g + ggtitle("GLOBAL SALES PER GAME FOR EACH PLATFORM") + theme(plot.title = element_text(size = 15, vjust = 1.5))


# Make a custom palette with platform colors
pal <- list(Sony_Playstation = "#6495ED", Microsoft_Xbox = "#F08080", Nintendo = "Green")

# Show the survival proability as a function of platforms
g <- ggplot(url_df_pf_grouped, aes(x = Year_of_Release, y = counts, color = Platform_General)) +
  geom_point(position = position_jitter(height = 0.05)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ Platform_General) +
  scale_color_manual(values = pal)


# Use more informative axis labels than are provided by default
ggplot2::ggtitle("GAMES RELEASED PER YEAR FOR EACH PLATFORM", size = 15) +
  xlab("Year of Release") +
  ylab("Games Released per Year")

# Make a custom palette with platform colors
pal <- c(Sony_Playstation = "#6495ED", Microsoft_Xbox = "#F08080", Nintendo = "Green")

# Show the survival proability as a function of platforms
ggplot(vgs_df_pf_grouped, aes(x = Year_of_Release, y = Global_Sales.M., color = Platform_General, group = Platform_General)) +
  geom_point(position = position_jitter(width = 0, height = 0.05)) +
  geom_smooth(method = "lm") +
  facet_wrap(~ Platform_General) +
  scale_color_manual(values = pal)

# Use more informative axis labels than are provided by default
ggplot(data, aes(x = Year_of_Release, y = Global_Sales_per_Year)) +
  geom_line() +
  labs(x = "Year of Release", y = "Global Sales per Year (M#)") +
  ggtitle("GLOBAL GAME SALES PER YEAR FOR EACH PLATFORM") +
  theme(plot.title = element_text(size = 15, vjust = 1.05))

url_platform$Platform_General <- as.factor(url_data_platform$Platform_General)
url_data_platform$User_Score <- as.numeric(url_data_platform$User_Score)

ratingSales <- url_platform %>%
  group_by(User_Score, Platform_General) %>%
  summarize(Global_Sales = sum(Global_Sales))

ratingSales_wide <- ratingSales %>%
  pivot_wider(names_from = Platform_General, values_from = Global_Sales)

ggplot(ratingSales_wide, aes(x = User_Score)) +
  geom_bar(aes(y = ``, fill = `Platform_General`), stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Greens") +
  theme_dark() +
  labs(title = "Stacked Barplot of Sales per Critic Score type of top 3 Platforms",
       x = "User Score",
       y = "Sales") +
  xlim(50, 96) +
  theme(legend.position = "top", legend.text = element_text(size = 13))

colors <- c('#008DB8', '#00AAAA', '#00C69C')

# Sony Playstation
sony_data <- aggregate(dummy_count ~ Genre, data = url_data_platform[url_data_platform$Platform_General == 'Sony_Playstation', ], sum)
par(mfrow=c(1, 2), mar=c(5, 5, 4, 2))
pie(sony_data$dummy_count,
    labels = sony_data$Genre,
    col = colors,
    main = 'Pie Chart of Genre Distribution of Playstation',
    init.angle = 90,
    radius = 1)

# Microsoft Xbox
xbox_data <- aggregate(dummy_count ~ Genre, data = url_data_platform[url_data_platform$Platform_General == 'Microsoft_Xbox', ], sum)
pie(xbox_data$dummy_count,
    labels = xbox_data$Genre,
    col = colors,
    main = 'Pie Chart of Genre Distribution of Xbox',
    init.angle = 90,
    radius = 1)

# Nintendo
nintendo_data <- aggregate(dummy_count ~ Genre, data = url_data_platform[url_data_platform$Platform_General == 'Nintendo', ], sum)
pie(nintendo_data$dummy_count,
    labels = nintendo_data$Genre,
    col = colors,
    main = 'Pie Chart of Genre Distribution of Nintendo',
    init.angle = 90,
    radius = 1)
