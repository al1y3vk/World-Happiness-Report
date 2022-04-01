library(DT)
library(shiny)
library(plot3D)
library(ggpubr)
library(caTools)
library(ggplot2)
library(reshape)
library(leaflet)
library(ggthemes)
library(corrplot)
library(corrgram)
library(htmltools)
library(tidyverse)
library(data.table)
library(countrycode)
library(shinyWidgets)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(tidyr)

X2015 <- read.csv("~/FinalProject/Data/2015.csv")
X2016 <- read.csv("~/FinalProject/Data/2016.csv")
X2017 <- read.csv("~/FinalProject/Data/2017.csv")
World_Coordinates <- read.csv("~/FinalProject/Data/World Coordinates.csv")


New_2015 <- left_join(x = X2015, y = World_Coordinates, by = "Country")
New_2016 <- left_join(x = X2016, y = World_Coordinates, by = "Country")
New_2017 <- left_join(x = X2017, y = World_Coordinates, by = "Country")

New_2015 <- mutate(New_2015, popupText = 
                     paste0("<center>", 
                            "<b>", Country, "</b></br>",
                            "<b>Happiness Rank: </b>", Happiness.Rank, "</br>",
                            "<b>Economy (GDP per Capita): </b>", round(Economy..GDP.per.Capita., digits = 3), "</br>",
                            "<b>Health (Life Expectancy): </b>", round(Health..Life.Expectancy., digits = 3), "</br>",
                            "<b>Dystopia Residual: </b>", round(Dystopia.Residual, digits = 3), "</br>"))

New_2016 <- mutate(New_2016, popupText = 
                     paste0("<center>", 
                            "<b>", Country, "</b></br>",
                            "<b>Happiness Rank: </b>", Happiness.Rank, "</br>",
                            "<b>Economy (GDP per Capita): </b>", round(Economy..GDP.per.Capita., digits = 3), "</br>",
                            "<b>Health (Life Expectancy): </b>", round(Health..Life.Expectancy., digits = 3), "</br>",
                            "<b>Dystopia Residual: </b>", round(Dystopia.Residual, digits = 3), "</br>"))

New_2017 <- mutate(New_2017, popupText = 
                     paste0("<center>", 
                            "<b>", Country, "</b></br>",
                            "<b>Happiness Rank: </b>", Happiness.Rank, "</br>",
                            "<b>Economy (GDP per Capita): </b>", round(Economy..GDP.per.Capita., digits = 3), "</br>",
                            "<b>Health (Life Expectancy): </b>", round(Health..Life.Expectancy., digits = 3), "</br>",
                            "<b>Dystopia Residual: </b>", round(Dystopia.Residual, digits = 3), "</br>"))

C2015 <- c(New_2015$Country)
C2016 <- c(New_2016$Country)
C2017 <- c(New_2017$Country)

New_2015$Continent <- countrycode(sourcevar = C2015,
                                  origin = "country.name",
                                  destination = "continent")

New_2016$Continent <- countrycode(sourcevar = C2016,
                                  origin = "country.name",
                                  destination = "continent")

New_2017$Continent <- countrycode(sourcevar = C2017,
                                  origin = "country.name",
                                  destination = "continent")

Analysis_2015 <- New_2015 %>% select(Country, Continent, Happiness.Rank, Happiness.Score,
                                     Economy..GDP.per.Capita., Family, Health..Life.Expectancy.,
                                     Freedom, Generosity, Trust..Government.Corruption., Dystopia.Residual)

Analysis_2016 <- New_2016 %>% select(Country, Continent, Happiness.Rank, Happiness.Score,
                                     Economy..GDP.per.Capita., Family, Health..Life.Expectancy.,
                                     Freedom, Generosity, Trust..Government.Corruption., Dystopia.Residual)

Analysis_2017 <- New_2017 %>% select(Country, Continent, Happiness.Rank, Happiness.Score,
                                     Economy..GDP.per.Capita., Family, Health..Life.Expectancy.,
                                     Freedom, Generosity, Trust..Government.Corruption., Dystopia.Residual)

# Correlation Plots
ColCount15 <- sapply(Analysis_2015, is.numeric)
CorData15 <- cor(Analysis_2015[c(4:11)])
colnames(CorData15) <- c("Happiness Score", "GDP per Capita", "Family", "Life Expectancy",
                         "Freedom", "Generosity", "Government Corruption", "Dystopia Residual")

ColCount16 <- sapply(Analysis_2016, is.numeric)
CorData16 <- cor(Analysis_2016[c(4:11)])
colnames(CorData16) <- c("Happiness Score", "GDP per Capita", "Family", "Life Expectancy",
                         "Freedom", "Generosity", "Government Corruption", "Dystopia Residual")


ColCount17 <- sapply(Analysis_2017, is.numeric)
CorData17 <- cor(Analysis_2017[c(4:11)])
colnames(CorData17) <- c("Happiness Score", "GDP per Capita", "Family", "Life Expectancy",
                         "Freedom", "Generosity", "Government Corruption", "Dystopia Residual")


# Bargraphs
Aggr15 <- aggregate(Analysis_2015[, 4:11], list(Analysis_2015$Continent), mean)
MeltedAggr15 <- melt(Aggr15)

Aggr16 <- aggregate(Analysis_2016[, 4:11], list(Analysis_2016$Continent), mean)
MeltedAggr16 <- melt(Aggr16)

Aggr17 <- aggregate(Analysis_2017[, 4:11], list(Analysis_2017$Continent), mean)
MeltedAggr17 <- melt(Aggr17)

colnames(MeltedAggr15) <- c("Continent", "Variable", "Value")
colnames(MeltedAggr16) <- c("Continent", "Variable", "Value")
colnames(MeltedAggr17) <- c("Continent", "Variable", "Value")

# Data
Main15 <- Analysis_2015[4:11]
Main16 <- Analysis_2016[4:11]
Main17 <- Analysis_2017[4:11]

# Predictions
set.seed(123)

split15 <- sample.split(Main15$Happiness.Score, SplitRatio = 0.8)
training15 <- subset(Main15, split15 == TRUE)
test15 <- subset(Main15, split15 == FALSE)

LM15 = lm(data = training15, formula = Happiness.Score ~ .)
summary(LM15)


split16 <- sample.split(Main16$Happiness.Score, SplitRatio = 0.8)
training16 <- subset(Main16, split16 == TRUE)
test16 <- subset(Main16, split16 == FALSE)

LM16 = lm(data = training16, formula = Happiness.Score ~ .)
summary(LM16)


split17 <- sample.split(Main17$Happiness.Score, SplitRatio = 0.8)
training17 <- subset(Main17, split17 == TRUE)
test17 <- subset(Main17, split17 == FALSE)

LM17 = lm(data = training17, formula = Happiness.Score ~ .)
summary(LM17)


pred15 = predict(LM15, newdata = test15)
actual15 <- as.data.frame(cbind(Prediction = pred15, Actual = test15$Happiness.Score))

pred16 = predict(LM16, newdata = test16)
actual16 <- as.data.frame(cbind(Prediction = pred16, Actual = test16$Happiness.Score))

pred17 = predict(LM17, newdata = test17)
actual17 <- as.data.frame(cbind(Prediction = pred17, Actual = test17$Happiness.Score))

# Popup Text
