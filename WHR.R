#Share & Tell Presentation
#Exploratory Analysis on World Health Report
#Progrmmer: Yagna Venkitasamy


rm(list=ls())
library(rio)
library(moments)
library(tidyverse)
library(corrplot)
library(car)
library(plotly)
library(ggplot2)
library(dplyr)
library(lattice)
library(wildcard)
library(stargazer)
#Data Pre-processing and cleaning

setwd("C:/Users/yagna/Documents/R/R workings/world-happiness")
wh15 <- fread("2015.csv", data.table = FALSE)
wh16 <- fread("2016.csv", data.table = FALSE)
wh17 <- fread("2017.csv", data.table = FALSE)
wh18 <- fread("2018.csv", data.table = FALSE)
wh19 <- fread("2019.csv", data.table = FALSE)

df15 <- wh15[c(-2,-5,-12)]
df16 <- wh16[c(-2,-5,-6,-13)]
df17 <- wh17[c(-4,-5,-12)]

names(df17)[2] <- "Happiness Rank"
names(df17)[3] <- "Happiness Score"
names(df17)[4] <- "Economy (GDP per Capita)"
names(df17)[6] <- "Health (Life Expectancy)"
names(df17)[9] <- "Trust (Government Corruption)"

names(wh18)
names(wh19)

df15 <- df15[c("Country","Happiness Rank", "Happiness Score", "Economy (GDP per Capita)","Family", "Health (Life Expectancy)", "Freedom", "Generosity",  "Trust (Government Corruption)")]
df16 <- df16[c("Country","Happiness Rank", "Happiness Score", "Economy (GDP per Capita)","Family", "Health (Life Expectancy)", "Freedom", "Generosity",  "Trust (Government Corruption)")]

names(wh18)[1]="Happiness Rank"
names(wh18)[2]="Country"
names(wh18)[3]="Happiness Score" 
names(wh18)[4]="Economy (GDP per Capita)"
names(wh18)[5]="Family" 
names(wh18)[6]="Health (Life Expectancy)"
names(wh18)[7]="Freedom"
names(wh18)[9]="Trust (Government Corruption)"

names(wh19)[1]="Happiness Rank"
names(wh19)[2]="Country"
names(wh19)[3]="Happiness Score" 
names(wh19)[4]="Economy (GDP per Capita)"
names(wh19)[5]="Family" 
names(wh19)[6]="Health (Life Expectancy)"
names(wh19)[7]="Freedom"
names(wh19)[9]="Trust (Government Corruption)"

wh18 <- wh18[c("Country","Happiness Rank", "Happiness Score", "Economy (GDP per Capita)","Family", "Health (Life Expectancy)", "Freedom", "Generosity",  "Trust (Government Corruption)")]
wh19 <- wh19[c("Country","Happiness Rank", "Happiness Score", "Economy (GDP per Capita)","Family", "Health (Life Expectancy)", "Freedom", "Generosity",  "Trust (Government Corruption)")]

df15$year = 2015
df16$year = 2016
df17$year = 2017
wh18$year = 2018
wh19$year = 2019

df <- rbind(df15,df16,df17,wh18,wh19)
df <- subset(df,(!is.na(df$Trust)))
names(df) <- c("Country","Happiness_Rank","Happiness_Score","Economy_GDP",
                    "Family","Health","Freedom",
                    "Generosity","Trust","year")
names(df)
head(df)
str(df)
df$Trust = as.numeric(df$Trust)
str(df)
summary(df)


#Correlation matrix plot for all variables
corrplot(cor(df %>% 
               select(Happiness_Score:Trust)), 
         method="color",  
         sig.level = 0.01, insig = "blank",
         addCoef.col = "black", 
         tl.srt=45, 
         type="upper"
    
)


# Happiness score in world map across the countries

world <- map_data('world')

world <- world %>% filter(region != "Antarctica")
world <- fortify(world)
happiness.score17 <- df %>% select("Country", "Happiness Score", "year") %>% filter(year == 2017)
happiness.score17 <- wildcard(df = happiness.score17, wildcard = "United States", values = "USA",
                              expand = TRUE, rules = NULL)
happiness.score17 <- wildcard(df = happiness.score17, wildcard = "United Kingdom", values = "UK",
                              expand = TRUE, rules = NULL)

happiness.score17 <- wildcard(df = happiness.score17, wildcard = "Democratic Republic of the Congo", values = "Congo (Kinshasa)",
                              expand = TRUE, rules = NULL)
names(happiness.score17)[2] <- "Happiness.Score"
ggplot()+ 
  geom_map(data=world, map=world,
           aes(x=long, y=lat, group=group, map_id=region),
           fill="white", colour="black") + 
  geom_map(data=happiness.score17, map=world,
           aes(fill= Happiness.Score, map_id= Country),
           colour="black") + 
  scale_fill_continuous(low="red", high="blue",
                        guide="colorbar") + 
  labs(title = "World Happiness Score in 2017")


names(df)[3] <- "Happiness.Score"
names(df)[4] <- "Economy"
names(df)[6] <- "Health"
names(df)[9] <- "Trust"

#Scatterplot for the variables with positive correlation

plot_ly(data = df, 
        x=~Economy, 
        y=~Happiness.Score, 
        color=~Health, 
        type = "scatter",
        text = ~paste("Country:", Country)) %>% 
  layout(title = "Happiness, GDP and Health relationship", 
         xaxis = list(title = "GDP per Capita"),
         yaxis = list(title = "Happiness Score"))
# This interactive scatterplot shows that there is a strong positive correlation 
# between GDP and Happiness Also points are coloured by the Health score, 
# which also suggeests that Health tends to have big impact to happiness.

plot_ly(data = df, 
        x=~Family, 
        y=~Happiness.Score, 
        color=~Freedom, 
        type = "scatter",
        text = ~paste("Country:", Country)) %>% 
  layout(title = "Happiness, Family and Freedom relationship", 
         xaxis = list(title = "Family"),
         yaxis = list(title = "Happiness Score"))
# This interactive scatterplot shows that there is a positive correlation 
# between Family and Happiness Also points are coloured by the Freedom score, 
# which also suggeests that Freedom of life choices tends to have an impact to happiness.

#Linear Regression Models
m1 <- lm(df$Happiness.Score ~ df$Economy+df$Health)
summary(m1)
par(mfrow=c(2,2))
plot(m1)

plot(df$Happiness.Score~df$Economy)

m2 <- lm(df$Happiness.Score ~ df$Family+df$Freedom)
summary(m2)
par(mfrow=c(2,2))
plot(m2)
plot(df$Happiness.Score~df$`Trust (Government Corruption)`)

m3<- lm(Happiness.Score ~ Economy + Health +as.factor(year), data = df)
summary(m3)

m4 <- lm(Happiness.Score ~ Economy + Family + Health + Freedom + Generosity + Trust, data = df)
summary(m4)
par(mfrow=c(2,2))
plot(m4)

m5 <- lm(Happiness.Score ~ Economy + Family + Health + Freedom + Generosity + Trust 
         + as.factor(year), data = df)
summary(m5)
par(mfrow=c(2,2))
plot(m5)

#Model Comparison
d=data.frame("Model 1",summary(m4)$r.squared,summary(m4)$adj.r.squared,AIC(m4),BIC(m4))
names(d)=c("Model number","R squared","Adj. Rsquared","AIC","BIC")

d1=data.frame("Model 2",summary(m5)$r.squared,summary(m5)$adj.r.squared,AIC(m5),BIC(m5))
names(d1)=c("Model number","R squared","Adj. Rsquared","AIC","BIC")

comparison = rbind(d,d1)
comparison

stargazer(comparison,type="text",summary=FALSE,digits=2)
anova(m4,m5, test = "F")


#xy plot to track the trends in the correlation between the variables over the years.
xyplot(Happiness.Score ~ Economy | factor(year), data=df)
xyplot(Happiness.Score ~ Family | factor(year), data=df)
xyplot(Happiness.Score ~ Health | factor(year), data=df)
xyplot(Happiness.Score ~ Freedom | factor(year), data=df)
xyplot(Happiness.Score ~ Trust | factor(year), data=df)
xyplot(Happiness.Score ~ Generosity | factor(year), data=df)
