install.packages("readxl")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("viridisLite")
install.packages("hrbrthemes")
install.packages("viridis")
install.packages("lessR")
install.packages("RColorBrewer")
install.packages("Polychrome")
install.packages("plyr")
install.packages("ggpubr")

library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridisLite)
library(hrbrthemes)
library(viridis)
library(lessR)
library(RColorBrewer)
library(Polychrome)
library(plyr)
library(ggpubr)

#Importing data
data <- read_excel("C:\\Users\\HP\\OneDrive\\Documents\\Datasets for Projects\\Billionaires.xlsx", sheet = "cleaned")
glimpse(data)

data_2022 <- read_excel("C:\\Users\\HP\\OneDrive\\Documents\\Datasets for Projects\\Billionaires.xlsx", sheet = "2022")

data_merged <- rbind(data, data_2022)

glimpse(data_merged)

#Grouping by year and calculating changes.

yearwise <- data_merged %>% group_by(year) %>% filter(rank <= 20) %>% arrange(rank)

yearwise_plot <- boxplot(worth ~ year, data = yearwise,
                         xlab = "Year",
                         ylab = "Net Worth (Billions)",
                         main = "Yearwise change in Net Worth of Top 20 Billionaires",
                         col = c("lightblue", "tomato", "darkmagenta", "darkgreen"))
yearwise_plot

#Now let's see the the industry wise distribution of billionaires

glimpse(data_merged)

unique(data_merged$industry)

#so there are 19 industries and some NA's.
#let's first remove NA's

industry_na <- subset(data_merged, !is.na(industry))
glimpse(industry_na)

unique(industry_na$industry)
#Now there is one industry called '0' which doesn't provide any information. Let's remove it too.

industry_wise <-  industry_na[!(industry_na$industry == "0"),]

glimpse(industry_wise)

unique(industry_wise$industry)

factors_industry <- as.factor(industry_wise$industry)

levels_industry <- levels(factors_industry)

#Let's find out which industry has what percent of Billionaires.

display.brewer.pal(n = 18, name = 'Spectral')

color <- brewer.pal(length(levels_industry), "Spectral")

industry_plot <- PieChart(industry, hole = 0, values = "%", data = industry_wise, col = color, main = "Industry wise distribution")

#We can see that two industries have less than 1% people. Let's remove these to make our data more clearly visible.

industry_filtered_1 <- industry_wise[!(industry_wise$industry == "banking"),]


unique(industry_filtered_1$industry)

industry_filtered_2 <- industry_filtered_1[!(industry_filtered_1$industry == "services"),]

unique(industry_filtered_2$industry)

industry_filtered_3 <- industry_filtered_2[!(industry_filtered_2$industry == "Venture Capital"),]

unique(industry_filtered_3$industry)


industry_plot_final <- PieChart(industry, hole = 0, values = "%", data = industry_filtered_3, fill = viridis(20), main = "Industry wise distribution")

#So we can see that the top 6 industries are: Consumer, Retail, Real Estate, Money Management, Computer Technology & Media.

#top 6 industries.
industry_filtered_4 <- industry_filtered_3 %>% filter(industry == c("Consumer", "Retail, Restaurant", "Money Management", "Real Estate", "Technology-Computer", "Media"))

unique(industry_filtered_4$industry)

top_6_industry_plot <- PieChart(industry, hole = 0.7, values = "%", data = industry_filtered_4, fill = viridis(6), main = "Top 6 Industries among Billionaires")

#top 5 countries with most number of Billionaires.

unique(data_merged$location.citizenship)

#So there are 73 countries in the data-set.
country_plot <- PieChart(location.citizenship, hole = 0, values = "%", data = data_merged, col = viridis(73), main = "Country wise distribution")

#So we can see that United States has more than 1/3rd of all the billionaires.

#Let's find the top 10 countries.
country_variable <- factor(data_merged$location.citizenship)

t <- count(data_merged, 'location.citizenship')

t

names(t)[2] = 'Number_of_Billionaires'
names(t)[1] = 'Country'
t


top_20 <- top_n(t[order(t$Number_of_Billionaires, na.last=TRUE, decreasing = TRUE),], 20)
top_20


as.data.frame(top_20)

#Plotting it.
top_20_countries <- ggplot(top_20, aes(x = reorder(Country, Number_of_Billionaires), y = Number_of_Billionaires, fill = viridis(20))) + geom_bar(stat='identity', position = 'dodge') + geom_text(aes(label = Number_of_Billionaires), position = position_dodge(width=0.9), vjust=-0.25) + labs(x ="Country", y = "No. of Billionaires")
top_20_countries

#Top 20 each year.
data_1996 <- data_merged %>% filter(year == 1996)

view(data_1996)

data_1996$rank

data_top_20_96 <- data_1996 %>% filter(rank == c(1,2,4,5,6,7,8,9,10,11,13,14,15,16,18,19,20)) %>% group_by(year)

data_2001 <- data_merged %>% filter(year == 2001)

view(data_2001)

data_2001$rank

data_top_20_01 <- data_2001 %>% filter(rank == c(1,2,3,4,5,6,7,8,9,10,10,12,13,14,15,16,17,18,18,20)) %>% group_by(year)

data_2014 <- data_merged %>% filter(year == 2014)

view(data_2014)

data_2014$rank

data_top_20_14 <- data_2014 %>% filter(rank == c(1,2,3,4,5,6,6,8,9,10,11,12,13,14,15,16,17,18,19,20)) %>% group_by(year)


data_2022 <- data_merged %>% filter(year == 2022)

view(data_2022)

data_2022$rank

data_top_20_96
data_top_20_01
data_top_20_14
data_2022

#Let's see the change in sum of top 20 
data_top_20_merged <- rbind(data_top_20_96, data_top_20_01, data_top_20_14, data_2022)
as.data.frame(data_top_20_merged)
glimpse(data_top_20_merged)

sum <- aggregate(data_top_20_merged$worth, list(data_top_20_merged$year), FUN=sum)

glimpse(sum)
as.data.frame(sum)

names(sum)[1] = 'Year'
names(sum)[2] = 'Total_Worth'
sum

sum_top_20_plot <- ggplot(sum, aes(x= Year, y = Total_Worth, fill = viridis(4))) + geom_bar(stat='identity') + geom_text(aes(label = Total_Worth), position = position_dodge(width=0.9), vjust=-0.25) + scale_x_discrete(labels = c("1" = "1996", "2" = "2001","3" = "2014","4" = "2022"))
sum_top_20_plot

#So we can see that there's a huge leap in the total worth, and thus the individual worth from 2014 to 2022.

#Let's see which countries most billionaires each year.
g <- ggplot(data=data_top_20_merged, aes(x=" ", y=worth, group=location.citizenship, colour=location.citizenship, fill=location.citizenship)) +
  geom_bar(width = 1, stat = "identity") +
  facet_grid(.~ year) +theme_void()
g

#let's see the percentage change in each country's share.

p1 <- PieChart(location.citizenship, hole = 0.7, values = "%", data = data_top_20_96, col = viridis(20), main = "Country wise distribution of Top 20 Billionaires in 1996")
p2 <- PieChart(location.citizenship, hole = 0.7, values = "%", data = data_top_20_01, col = viridis(20), main = "Country wise distribution of Top 20 Billionaires in 2001")
p3 <- PieChart(location.citizenship, hole = 0.7, values = "%", data = data_top_20_14, col = viridis(20), main = "Country wise distribution of Top 20 Billionaires in 2014")
p4 <- PieChart(location.citizenship, hole = 0.7, values = "%", data = data_2022, col = viridis(20), main = "Country wise distribution of Top 20 Billionaires in 2022")

#let's see the percentage change in number of women each year.
glimpse(data_top_20_merged)

q1 <- PieChart(demographics.gender, hole = 0.7, values = "%", data = data_top_20_96, fill = c("Pale Violet Red 2 ", "Light Sky Blue 2 "), main = "Male - Female Ratio of Top 20 Billionaires in 1996")
q2 <- PieChart(demographics.gender, hole = 0.7, values = "%", data = data_top_20_01, fill = c("Pale Violet Red 2 ", "Light Sky Blue 2"), main = "Male - Female Ratio of Top 20 Billionaires in 2001")
q3 <- PieChart(demographics.gender, hole = 0.7, values = "%", data = data_top_20_14, fill = c("Pale Violet Red 2 ", "Light Sky Blue 2"), main = "Male - Female Ratio of Top 20 Billionaires in 2014")
q4 <- PieChart(demographics.gender, hole = 0.7, values = "%", data = data_2022, fill = c("Pale Violet Red 2 ", "Light Sky Blue 2"), main = "Male - Female Ratio of Top 20 Billionaires in 2022")

#let's see the percentage change in industry of Billionaires each year.

i1 <- PieChart(industry, hole = 0.7, values = "%", data = data_top_20_96, col = viridis(20), main = "Industry wise distribution of Top 20 Billionaires in 1996")
i2 <- PieChart(industry, hole = 0.7, values = "%", data = data_top_20_01, col = viridis(20), main = "Industry wise distribution of Top 20 Billionaires in 2001")
i3 <- PieChart(industry, hole = 0.7, values = "%", data = data_top_20_14, col = viridis(20), main = "Industry wise distribution of Top 20 Billionaires in 2014")
i4 <- PieChart(industry, hole = 0.7, values = "%", data = data_2022, col = viridis(20), main = "Industry wise distribution of Top 20 Billionaires in 2022")

#wealth-type: Inherited or Not
glimpse(data_top_20_merged)
dim(data_top_20_merged)

Inherited <- data_top_20_merged %>% filter(wealth.type == "inherited")
dim(Inherited)

self_made <- data_top_20_merged %>% filter(wealth.type != "inherited")
dim(self_made)
class(self_made)

as.data.frame(self_made)
class(self_made)

self_made$wealth_type <- paste0("Self Made", self_made$wealth_type)
glimpse(self_made)

self_made_1 <- self_made[,-c(10)]
glimpse(self_made_1)

names(self_made_1)[11] = 'wealth.type'
glimpse(self_made_1)
glimpse(inherited)

self_made_2 <- self_made_1[,c(1,2,3,4,5,6,7,8,9,11,10)]
glimpse(self_made_2)

wealth_type <- rbind(self_made_2, Inherited)
glimpse(wealth_type)

wealth_type_96 <- wealth_type %>% filter(year == 1996)
wealth_type_01 <- wealth_type %>% filter(year == 2001)
wealth_type_14 <- wealth_type %>% filter(year == 2014)
wealth_type_22 <- wealth_type %>% filter(year == 2022)

wealth_type_chart <- PieChart(wealth.type, hole = 0.7, values = "%", data = wealth_type, fill = c("Gold", "Dark Red"), main = "Wealth Type of Top 20 Billionaires Since 1996 - 2022")

w1 <- PieChart(wealth.type, hole = 0.7, values = "%", data = wealth_type_96, fill = c("Gold", "Dark Red"), main = "Wealth Type of Top 20 Billionaires in 1996")
w2 <- PieChart(wealth.type, hole = 0.7, values = "%", data = wealth_type_01, fill = c("Gold", "Dark Red"), main = "Wealth Type of Top 20 Billionaires in 2001")
w3 <- PieChart(wealth.type, hole = 0.7, values = "%", data = wealth_type_14, fill = c("Gold", "Dark Red"), main = "Wealth Type of Top 20 Billionaires in 2014")
w4 <- PieChart(wealth.type, hole = 0.7, values = "%", data = wealth_type_22, fill = c("Gold", "Dark Red"), main = "Wealth Type of Top 20 Billionaires in 2022")

#finally let's see which billionaires have been in the top 20 in all these years.
glimpse(data_top_20_merged)

unique(data_top_20_merged$name)

#So there are 51 unique people.
name_variable <- factor(data_top_20_merged$name)

n <- count(data_top_20_merged, 'name')
n

names(n)[1] = 'Name'
names(n)[2] = 'Frequency'
n

as.data.frame(n)

names_freq <- ggplot(n, aes(y = reorder(Name, Frequency), x = Frequency, fill = viridis(51))) + geom_bar(stat='identity') + labs(x ="Name", y = "Frequency")
names_freq

