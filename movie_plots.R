library(tidyverse)
library(dslabs)
library(ggthemes)
library(ggrepel)
library(dplyr)
library(ggplot2)
library(scales)

#import data from csv and make any last-minute changes to variable for better visualization
table <- read.csv("movies.csv")
table <- mutate(table, Earnings = round(Earnings/1000000),1)
#create array of all associated color values
color_values <- table$Color

#plot 1 (1) - year (x) and earnings in millions of dollars (y), color taken from poster, size/shape constant
plot1 <- ggplot(data = table, aes(Year, Earnings))        #specify what data should be used
plot1 <- plot1 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)    #create dot plot
plot1 <- plot1 + scale_color_manual(values = color_values)    #set the colors to the array created previously
plot1 <- plot1 + ggtitle("\nWorldwide Lifetime Gross of Top 1000 Movies by Year\n")
plot1 <- plot1 + xlab("\nYear\n")
plot1 <- plot1 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1 <- plot1 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1 <- plot1 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))  #log scale on y-axis to better show all points
plot1 <- plot1 + theme_tufte()                                                                              #y-axis lower limit of -1 creates warning but correctly places x-axis
plot1 <- plot1 + theme(text = element_text(size = 15))
plot1 <- plot1 + theme(plot.title = element_text(hjust = 0.5))  #center title
plot(plot1)

#plot 2 (2) - runtime in minutes (x) and earnings in dollars(y), color taken from poster, size/shape constant
plot2 <- ggplot(data = table, aes(Runtime, Earnings))
plot2 <- plot2 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot2 <- plot2 + scale_color_manual(values = color_values)
plot2 <- plot2 + ggtitle("\nWorldwide Lifetime Gross of Top 1000 Movies by Runtime\n")
plot2 <- plot2 + xlab("\nRuntime (Minutes)\n")
plot2 <- plot2 + scale_x_continuous(n.breaks = 10)
plot2 <- plot2 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot2 <- plot2 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot2 <- plot2 + theme_tufte()
plot2 <- plot2 + theme(text = element_text(size = 15))
plot2 <- plot2 + theme(plot.title = element_text(hjust = 0.5))
plot(plot2)

#plot 1.1 (3) - plot 1, size and color as rating, shape as audience
table1.1 <- filter(table, (Audience == "G" | Audience == "PG" | Audience == "PG-13" | Audience == "R") & Rating >= 6) 
plot1.1 <- ggplot(data = table1.1, aes(Year, Earnings))
plot1.1 <- plot1.1 + geom_point(aes(color = Rating, shape = Audience, size = Rating))
plot1.1 <- plot1.1 + scale_shape_manual(values = c(15, 16, 17, 18))     #shapes of a similar visual size chosen
plot1.1 <- plot1.1 + scale_color_gradient(low = "orange", high = "purple")  #color gradient to emphasize ratings
plot1.1 <- plot1.1 + ggtitle("\nWorldwide Lifetime Gross of Top 1000 Movies by Year, Extended Data\n")
plot1.1 <- plot1.1 + xlab("\nYear\n")
plot1.1 <- plot1.1 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.1 <- plot1.1 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.1 <- plot1.1 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.1 <- plot1.1 + theme_tufte()
plot1.1 <- plot1.1 + theme(text = element_text(size = 15))
plot1.1 <- plot1.1 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.1)

#plot 1.2.1 (4) - plot 1, filtered by genre - action
table1.2.1 = filter(table, G_Action == "True")
color_values1.2.1 <- table1.2.1$Color       #new color array for filtered data
plot1.2.1 <- ggplot(data = table1.2.1, aes(Year, Earnings))
plot1.2.1 <- plot1.2.1 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.1 <- plot1.2.1 + scale_color_manual(values = color_values1.2.1)
plot1.2.1 <- plot1.2.1 + ggtitle("\nWorldwide Lifetime Gross of Action Movies by Year\n")
plot1.2.1 <- plot1.2.1 + xlab("\nYear\n")
plot1.2.1 <- plot1.2.1 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.1 <- plot1.2.1 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.1 <- plot1.2.1 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.1 <- plot1.2.1 + theme_tufte()
plot1.2.1 <- plot1.2.1 + theme(text = element_text(size = 15))
plot1.2.1 <- plot1.2.1 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.1)

#plot 1.2.2 (5) - plot 1, filtered by genre - adventure
table1.2.2 = filter(table, G_Adventure == "True")
color_values1.2.2 <- table1.2.2$Color
plot1.2.2 <- ggplot(data = table1.2.2, aes(Year, Earnings))
plot1.2.2 <- plot1.2.2 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.2 <- plot1.2.2 + scale_color_manual(values = color_values1.2.2)
plot1.2.2 <- plot1.2.2 + ggtitle("\nWorldwide Lifetime Gross of Adventure Movies by Year\n")
plot1.2.2 <- plot1.2.2 + xlab("\nYear\n")
plot1.2.2 <- plot1.2.2 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.2 <- plot1.2.2 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.2 <- plot1.2.2 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.2 <- plot1.2.2 + theme_tufte()
plot1.2.2 <- plot1.2.2 + theme(text = element_text(size = 15))
plot1.2.2 <- plot1.2.2 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.2)

#plot 1.2.3 (6) - plot 1, filtered by genre - animation
table1.2.3 = filter(table, G_Animation == "True")
color_values1.2.3 <- table1.2.3$Color
plot1.2.3 <- ggplot(data = table1.2.3, aes(Year, Earnings))
plot1.2.3 <- plot1.2.3 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.3 <- plot1.2.3 + scale_color_manual(values = color_values1.2.3)
plot1.2.3 <- plot1.2.3 + ggtitle("\nWorldwide Lifetime Gross of Animated Movies by Year\n")
plot1.2.3 <- plot1.2.3 + xlab("\nYear\n")
plot1.2.3 <- plot1.2.3 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.3 <- plot1.2.3 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.3 <- plot1.2.3 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.3 <- plot1.2.3 + theme_tufte()
plot1.2.3 <- plot1.2.3 + theme(text = element_text(size = 15))
plot1.2.3 <- plot1.2.3 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.3)

#plot 1.2.4 (7) - plot 1, filtered by genre - biography
table1.2.4 = filter(table, G_Biography == "True")
color_values1.2.4 <- table1.2.4$Color
plot1.2.4 <- ggplot(data = table1.2.4, aes(Year, Earnings))
plot1.2.4 <- plot1.2.4 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.4 <- plot1.2.4 + scale_color_manual(values = color_values1.2.4)
plot1.2.4 <- plot1.2.4 + ggtitle("\nWorldwide Lifetime Gross of Biography Movies by Year\n")
plot1.2.4 <- plot1.2.4 + xlab("\nYear\n")
plot1.2.4 <- plot1.2.4 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.4 <- plot1.2.4 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.4 <- plot1.2.4 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.4 <- plot1.2.4 + theme_tufte()
plot1.2.4 <- plot1.2.4 + theme(text = element_text(size = 15))
plot1.2.4 <- plot1.2.4 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.4)

#plot 1.2.5 (8) - plot 1, filtered by genre - comedy
table1.2.5 = filter(table, G_Comedy == "True")
color_values1.2.5 <- table1.2.5$Color
plot1.2.5 <- ggplot(data = table1.2.5, aes(Year, Earnings))
plot1.2.5 <- plot1.2.5 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.5 <- plot1.2.5 + scale_color_manual(values = color_values1.2.5)
plot1.2.5 <- plot1.2.5 + ggtitle("\nWorldwide Lifetime Gross of Comedy Movies by Year\n")
plot1.2.5 <- plot1.2.5 + xlab("\nYear\n")
plot1.2.5 <- plot1.2.5 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.5 <- plot1.2.5 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.5 <- plot1.2.5 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.5 <- plot1.2.5 + theme_tufte()
plot1.2.5 <- plot1.2.5 + theme(text = element_text(size = 15))
plot1.2.5 <- plot1.2.5 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.5)

#plot 1.2.6 (9) - plot 1, filtered by genre - crime
table1.2.6 = filter(table, G_Crime == "True")
color_values1.2.6 <- table1.2.6$Color
plot1.2.6 <- ggplot(data = table1.2.6, aes(Year, Earnings))
plot1.2.6 <- plot1.2.6 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.6 <- plot1.2.6 + scale_color_manual(values = color_values1.2.6)
plot1.2.6 <- plot1.2.6 + ggtitle("\nWorldwide Lifetime Gross of Crime Movies by Year\n")
plot1.2.6 <- plot1.2.6 + xlab("\nYear\n")
plot1.2.6 <- plot1.2.6 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.6 <- plot1.2.6 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.6 <- plot1.2.6 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.6 <- plot1.2.6 + theme_tufte()
plot1.2.6 <- plot1.2.6 + theme(text = element_text(size = 15))
plot1.2.6 <- plot1.2.6 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.6)

#plot 1.2.7 (10) - plot 1, filtered by genre - drama
table1.2.7 = filter(table, G_Drama == "True")
color_values1.2.7 <- table1.2.7$Color
plot1.2.7 <- ggplot(data = table1.2.7, aes(Year, Earnings))
plot1.2.7 <- plot1.2.7 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.7 <- plot1.2.7 + scale_color_manual(values = color_values1.2.7)
plot1.2.7 <- plot1.2.7 + ggtitle("\nWorldwide Lifetime Gross of Drama Movies by Year\n")
plot1.2.7 <- plot1.2.7 + xlab("\nYear\n")
plot1.2.7 <- plot1.2.7 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.7 <- plot1.2.7 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.7 <- plot1.2.7 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.7 <- plot1.2.7 + theme_tufte()
plot1.2.7 <- plot1.2.7 + theme(text = element_text(size = 15))
plot1.2.7 <- plot1.2.7 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.7)

#plot 1.2.8 (11) - plot 1, filtered by genre - family
table1.2.8 = filter(table, G_Family == "True")
color_values1.2.8 <- table1.2.8$Color
plot1.2.8 <- ggplot(data = table1.2.8, aes(Year, Earnings))
plot1.2.8 <- plot1.2.8 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.8 <- plot1.2.8 + scale_color_manual(values = color_values1.2.8)
plot1.2.8 <- plot1.2.8 + ggtitle("\nWorldwide Lifetime Gross of Family Movies by Year\n")
plot1.2.8 <- plot1.2.8 + xlab("\nYear\n")
plot1.2.8 <- plot1.2.8 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.8 <- plot1.2.8 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.8 <- plot1.2.8 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.8 <- plot1.2.8 + theme_tufte()
plot1.2.8 <- plot1.2.8 + theme(text = element_text(size = 15))
plot1.2.8 <- plot1.2.8 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.8)

#plot 1.2.9 (12) - plot 1, filtered by genre - fantasy
table1.2.9 = filter(table, G_Fantasy == "True")
color_values1.2.9 <- table1.2.9$Color
plot1.2.9 <- ggplot(data = table1.2.9, aes(Year, Earnings))
plot1.2.9 <- plot1.2.9 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.9 <- plot1.2.9 + scale_color_manual(values = color_values1.2.9)
plot1.2.9 <- plot1.2.9 + ggtitle("\nWorldwide Lifetime Gross of Fantasy Movies by Year\n")
plot1.2.9 <- plot1.2.9 + xlab("\nYear\n")
plot1.2.9 <- plot1.2.9 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.9 <- plot1.2.9 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.9 <- plot1.2.9 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.9 <- plot1.2.9 + theme_tufte()
plot1.2.9 <- plot1.2.9 + theme(text = element_text(size = 15))
plot1.2.9 <- plot1.2.9 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.9)

#plot 1.2.10 (13) - plot 1, filtered by genre - history
table1.2.10 = filter(table, G_History == "True")
color_values1.2.10 <- table1.2.10$Color
plot1.2.10 <- ggplot(data = table1.2.10, aes(Year, Earnings))
plot1.2.10 <- plot1.2.10 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.10 <- plot1.2.10 + scale_color_manual(values = color_values1.2.10)
plot1.2.10 <- plot1.2.10 + ggtitle("\nWorldwide Lifetime Gross of History Movies by Year\n")
plot1.2.10 <- plot1.2.10 + xlab("\nYear\n")
plot1.2.10 <- plot1.2.10 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.10 <- plot1.2.10 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.10 <- plot1.2.10 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.10 <- plot1.2.10 + theme_tufte()
plot1.2.10 <- plot1.2.10 + theme(text = element_text(size = 15))
plot1.2.10 <- plot1.2.10 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.10)

#plot 1.2.11 (14) - plot 1, filtered by genre - horror
table1.2.11 = filter(table, G_Horror == "True")
color_values1.2.11 <- table1.2.11$Color
plot1.2.11 <- ggplot(data = table1.2.11, aes(Year, Earnings))
plot1.2.11 <- plot1.2.11 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.11 <- plot1.2.11 + scale_color_manual(values = color_values1.2.11)
plot1.2.11 <- plot1.2.11 + ggtitle("\nWorldwide Lifetime Gross of Horror Movies by Year\n")
plot1.2.11 <- plot1.2.11 + xlab("\nYear\n")
plot1.2.11 <- plot1.2.11 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.11 <- plot1.2.11 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.11 <- plot1.2.11 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.11 <- plot1.2.11 + theme_tufte()
plot1.2.11 <- plot1.2.11 + theme(text = element_text(size = 15))
plot1.2.11 <- plot1.2.11 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.11)

#plot 1.2.12 (15) - plot 1, filtered by genre - music
table1.2.12 = filter(table, G_Music == "True")
color_values1.2.12 <- table1.2.12$Color
plot1.2.12 <- ggplot(data = table1.2.12, aes(Year, Earnings))
plot1.2.12 <- plot1.2.12 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.12 <- plot1.2.12 + scale_color_manual(values = color_values1.2.12)
plot1.2.12 <- plot1.2.12 + ggtitle("\nWorldwide Lifetime Gross of Music Movies by Year\n")
plot1.2.12 <- plot1.2.12 + xlab("\nYear\n")
plot1.2.12 <- plot1.2.12 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.12 <- plot1.2.12 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.12 <- plot1.2.12 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.12 <- plot1.2.12 + theme_tufte()
plot1.2.12 <- plot1.2.12 + theme(text = element_text(size = 15))
plot1.2.12 <- plot1.2.12 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.12)

#plot 1.2.13 (16) - plot 1, filtered by genre - musical
table1.2.13 = filter(table, G_Musical == "True")
color_values1.2.13 <- table1.2.13$Color
plot1.2.13 <- ggplot(data = table1.2.13, aes(Year, Earnings))
plot1.2.13 <- plot1.2.13 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.13 <- plot1.2.13 + scale_color_manual(values = color_values1.2.13)
plot1.2.13 <- plot1.2.13 + ggtitle("\nWorldwide Lifetime Gross of Musical Movies by Year\n")
plot1.2.13 <- plot1.2.13 + xlab("\nYear\n")
plot1.2.13 <- plot1.2.13 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.13 <- plot1.2.13 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.13 <- plot1.2.13 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.13 <- plot1.2.13 + theme_tufte()
plot1.2.13 <- plot1.2.13 + theme(text = element_text(size = 15))
plot1.2.13 <- plot1.2.13 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.13)

#plot 1.2.14 (17) - plot 1, filtered by genre - mystery
table1.2.14 = filter(table, G_Mystery == "True")
color_values1.2.14 <- table1.2.14$Color
plot1.2.14 <- ggplot(data = table1.2.14, aes(Year, Earnings))
plot1.2.14 <- plot1.2.14 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.14 <- plot1.2.14 + scale_color_manual(values = color_values1.2.14)
plot1.2.14 <- plot1.2.14 + ggtitle("\nWorldwide Lifetime Gross of Mystery Movies by Year\n")
plot1.2.14 <- plot1.2.14 + xlab("\nYear\n")
plot1.2.14 <- plot1.2.14 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.14 <- plot1.2.14 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.14 <- plot1.2.14 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.14 <- plot1.2.14 + theme_tufte()
plot1.2.14 <- plot1.2.14 + theme(text = element_text(size = 15))
plot1.2.14 <- plot1.2.14 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.14)

#plot 1.2.15 (18) - plot 1, filtered by genre - romance
table1.2.15 = filter(table, G_Romance == "True")
color_values1.2.15 <- table1.2.15$Color
plot1.2.15 <- ggplot(data = table1.2.15, aes(Year, Earnings))
plot1.2.15 <- plot1.2.15 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.15 <- plot1.2.15 + scale_color_manual(values = color_values1.2.15)
plot1.2.15 <- plot1.2.15 + ggtitle("\nWorldwide Lifetime Gross of Romance Movies by Year\n")
plot1.2.15 <- plot1.2.15 + xlab("\nYear\n")
plot1.2.15 <- plot1.2.15 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.15 <- plot1.2.15 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.15 <- plot1.2.15 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.15 <- plot1.2.15 + theme_tufte()
plot1.2.15 <- plot1.2.15 + theme(text = element_text(size = 15))
plot1.2.15 <- plot1.2.15 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.15)

#plot 1.2.16 (19) - plot 1, filtered by genre - sci-fi
table1.2.16 = filter(table, G_SciFi == "True")
color_values1.2.16 <- table1.2.16$Color
plot1.2.16 <- ggplot(data = table1.2.16, aes(Year, Earnings))
plot1.2.16 <- plot1.2.16 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.16 <- plot1.2.16 + scale_color_manual(values = color_values1.2.16)
plot1.2.16 <- plot1.2.16 + ggtitle("\nWorldwide Lifetime Gross of Sci-Fi Movies by Year\n")
plot1.2.16 <- plot1.2.16 + xlab("\nYear\n")
plot1.2.16 <- plot1.2.16 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.16 <- plot1.2.16 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.16 <- plot1.2.16 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.16 <- plot1.2.16 + theme_tufte()
plot1.2.16 <- plot1.2.16 + theme(text = element_text(size = 15))
plot1.2.16 <- plot1.2.16 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.16)

#plot 1.2.17 (20) - plot 1, filtered by genre - thriller
table1.2.17 = filter(table, G_Thriller == "True")
color_values1.2.17 <- table1.2.17$Color
plot1.2.17 <- ggplot(data = table1.2.17, aes(Year, Earnings))
plot1.2.17 <- plot1.2.17 + geom_point(aes(color = Title), size = 4, show.legend = FALSE)
plot1.2.17 <- plot1.2.17 + scale_color_manual(values = color_values1.2.17)
plot1.2.17 <- plot1.2.17 + ggtitle("\nWorldwide Lifetime Gross of Thriller Movies by Year\n")
plot1.2.17 <- plot1.2.17 + xlab("\nYear\n")
plot1.2.17 <- plot1.2.17 + scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), limits = c(1937, 2021))
plot1.2.17 <- plot1.2.17 + ylab("\nWorldwide Lifetime Gross (Millions of Dollars) - Ln Scale\n")
plot1.2.17 <- plot1.2.17 + scale_y_continuous(trans = "log", breaks = c(250, 500, 1000, 2000), limits = c(-1, 3000))
plot1.2.17 <- plot1.2.17 + theme_tufte()
plot1.2.17 <- plot1.2.17 + theme(text = element_text(size = 15))
plot1.2.17 <- plot1.2.17 + theme(plot.title = element_text(hjust = 0.5))
plot(plot1.2.17)
