#importing packages we might need.
library(GGally)
library(tidyverse)
library(janitor)
library(here)

#importing our Red Wine dataset
Winedata <- read.csv(here("/Users/SulMac/Desktop/MiskDSI/individual-assignment-2-SulAbid/data/winequality-red.csv"))

#Taking a look on our data
summary(Winedata)

names(Winedata)[9] <- "ph"

Winedf <- Winedata %>%
  clean_names()

#removing duplicated rows.

Winedf %>% get_dupes()
Winedf %>% get_dupes() %>% filter(dupe_count > 2)
Winedf <- distinct(Winedata)
#Taking a look on our data after removing duplicates.
glimpse(Winedf)
summary(Winedf)


#Create a variable tells us if the wine has high or med or low quality based on quality column
#Set boundaries for intervals
breaks <- c(0, 5, 7, 10)

# Bucket data points into intervals
Winedf$quality.category <- cut(Winedf$quality, breaks, include.lowest = TRUE, right = FALSE)
labels <- c("Low", "Medium", "High")
Winedf$quality.category <- cut(Winedf$quality, breaks, include.lowest = TRUE, right = FALSE, labels=labels)


#Fixed Acidity and Wine Quality
# as we can see Red line = mean of Low quality wine distribution (red shape)
# Blue line is for the High quality wine acidity level distribution (blue shape)
# Green line is for the Med quality wine acidity level distribution (Green shape)
# Red line is for the Low quality wine acidity level distribution (Red shape)


ggplot(Winedf,aes(x=fixed.acidity,fill=factor(quality.category)))+geom_density(alpha=0.60)+
  
  geom_vline(aes(xintercept=mean(fixed.acidity[quality.category=="High"],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(fixed.acidity[quality.category=="Medium"],na.rm=T)),color="green",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(fixed.acidity[quality.category=="Low"],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  
  scale_x_continuous(breaks = seq(4,16,1))+
  xlab(label = "Fixed Acidity Level")+
  ggtitle("Distribution of Fixed Acidity Levels")+
  theme_classic()

#Checking the summary of each wine category acidity level
by(Winedf$fixed.acidity, Winedf$quality.category, summary)
# The acidity level mean for High quality wine = 8.82
# The acidity level mean for Med quality wine = 8.25
# The acidity level mean for Low quality wine = 7.87



#PH and Wine Quality plot

#The Mean of pH is 3.31

summary(Winedf$ph)

#Plotting pH 

ggplot(Winedf,aes(x=ph,fill=factor(quality.category)))+geom_density(alpha=0.55)+
  
  geom_vline(aes(xintercept=mean(ph[quality.category=="High"],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(ph[quality.category=="Medium"],na.rm=T)),color="green",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(ph[quality.category=="Low"],na.rm=T)),color="blue",linetype="dashed",lwd=1)+
  
  scale_x_continuous(breaks = seq(2.5,5,0.5))+
  xlab(label = "Red Wine PH Level")+
  ggtitle("Distribution of Red Wine PH Levels")+
  theme_classic()

#Checking the summary of each wine category pH level
by(Winedf$ph, Winedf$quality.category, summary)
# The pH level mean for High quality wine = 3.27
# The pH level mean for Med quality wine = 3.31
# The pH level mean for Low quality wine = 3.38



#Alcohol and Wine Quality
ggplot(Winedf,aes(x=alcohol,fill=factor(quality.category)))+geom_density(alpha=0.55)+
  geom_vline(aes(xintercept=mean(alcohol[quality.category=="High"],na.rm=T)),color="red",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[quality.category=="Medium"],na.rm=T)),color="Green",linetype="dashed",lwd=1)+
  geom_vline(aes(xintercept=mean(alcohol[quality.category=="Low"],na.rm=T)),color="Blue",linetype="dashed",lwd=1)+
  
  
   scale_x_continuous(breaks = seq(8,15,1))+
  xlab(label = "Alcohol Level")+
  ggtitle("Distribution of Alcohol Levels")+
  theme_classic()

#Checking the summary of each wine category Alchohol level
by(Winedf$alcohol, Winedf$quality.category, summary)
# The Alchohol level mean for High quality wine = 11.55
# The Alcohol level mean for Med quality wine = 10.26
# The Alchohol level mean for Low quality wine = 10.22



