#setwd
setwd("/Volumes/My Passport/Workspace/R_Files/gender-gap-mathematics")

#install.packages("ggplot2")

#Load packages
library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

#round all the variables to 3 decimal places. Avoid putting the values in scientific notation, strongly avoid it = 999
#options(scipen=99, digits = 3)

#Import the dataset
file <- "ECLSK_ass6.csv"
data <- fread(file, data.table = FALSE, na.strings = "NULL")

#Class Declarations
class(data$C1R4RSCL) <- "numeric"
class(data$C1R4MSCL) <- "numeric"
class(data$C5R4RSCL) <- "numeric"
class(data$C5R4MSCL) <- "numeric"


#Dimensions
dim(data)

#Data Frame
str(data, max.level = 1)

#Question No.2
data <- data %>% filter(C1R4RSCL > 0, C1R4MSCL > 0, C5R4RSCL > 0, C5R4MSCL > 0, GENDER > 0, RACE > 0, P1HMAFB > 0)

#Question No.3
data <- data %>% mutate(GENDER = plyr::mapvalues(data$GENDER, from=c(1,2), to=c("Male", "Female")),
                        RACE = plyr::mapvalues(data$RACE, from=c(1,2,3,4,5,6,7,8), 
                                               to=c("White", "Non-His Black or Afn Amn, Non-His", "His, Race Sepific",
                                                    "His, Race Non-Specific", "Asian", "Native Hawaiian, other PI",
                                                    "Amn Indian or ALSK Native", "More than one Race, Non-His")))

#Data Reshape:
dataS <- gather(data, Score_Type, Score, C1R4RSCL:C5R4MSCL)
head(dataS,5)

#Question No.4
data.AvgScore <- dataS %>% group_by(GENDER, Score_Type) %>% summarize(avgScore = round(mean(Score, na.rm=TRUE),2)) %>% data.table()
data.AvgScore <- spread(data.AvgScore, Score_Type, avgScore)
data.AvgScore

#Question No.5
data.AvgScore.Race <- dataS %>% group_by(RACE, Score_Type) %>% summarize(avgScore = round(mean(Score, na.rm=TRUE),2)) %>% data.table()
data.AvgScore.Race <- spread(data.AvgScore.Race, Score_Type, avgScore)
data.AvgScore.Race

#Question No.6
data.quantile <- data %>% group_by(GENDER) %>% summarize(C1Read10 = quantile(C1R4RSCL, .1, na.rm = TRUE),
                                    C1Read90 = quantile(C1R4RSCL, .9, na.rm = TRUE),
                                    C1Math10 = quantile(C1R4MSCL, .1, na.rm = TRUE),
                                    C1Math90 = quantile(C1R4MSCL, .9, na.rm = TRUE),
                                    C5Read10 = quantile(C5R4RSCL, .1, na.rm = TRUE),
                                    C5Read90 = quantile(C5R4RSCL, .9, na.rm = TRUE),
                                    C5Math10 = quantile(C5R4MSCL, .1, na.rm = TRUE),
                                    C5Math90 = quantile(C5R4MSCL, .9, na.rm = TRUE)) %>% data.table()
data.quantile

#Question No.7
data.Score <- dataS %>% select(GENDER, Score_Type, Score) %>% group_by(GENDER) %>% data.frame()
head(data.Score, 5)

data.dens <- ggplot(data = data.Score, aes(x=Score, ..density..)) +
  labs(x="Score", y="Density", title="Density of Scores by Gender") +
  geom_density(aes(color=factor(GENDER), fill=factor(Score_Type)), alpha=0.1)
data.dens

#Question No.8
data.Race <- data %>% select(C5R4RSCL, C5R4MSCL, RACE) %>% group_by(RACE) %>% data.frame()
data.Race <- gather(data.Race, RACE, Score, C5R4RSCL:C5R4MSCL)
head(data.Race,5)

data.RaceDens <- ggplot(data = data.Race, aes(x=Score, ..density..)) +
  labs(x="Score", y="Density", title="Density of 5th Grade Scores Categorized by Race") +
  geom_density(aes(color=factor(RACE), fill=factor(RACE)), alpha=0.1) +
  facet_wrap(~RACE, ncol=2)

data.RaceDens

#Question No.9
data.Mothers <- dataS %>% select(P1HMAFB, Score_Type, Score) %>% group_by(P1HMAFB) %>% data.frame()
head(data.Mothers,5)

data.MothersPlot <- ggplot(na.omit(data.Mothers), aes(x=P1HMAFB, y=Score)) + 
  labs(x = "Mothers' Age", y = "Score", title = "Mothers' Age at Birth Against Score") +
  geom_point(color="blue") +
  geom_smooth(method=lm, color="orange", se = FALSE) +
  facet_wrap(~Score_Type)

data.MothersPlot

#Question No.10
data.MothersByRacePlot <- data %>% ggplot(aes(x=P1HMAFB, y=C5R4MSCL)) +
  geom_point(color="lightgreen") +
  labs(x="Mothers' Age", y="Score", title = "Race, Mothers' Age, and Score") +
  geom_smooth(method=lm, color="black", se=FALSE) +
  facet_wrap(~RACE, ncol = 2)

data.MothersByRacePlot