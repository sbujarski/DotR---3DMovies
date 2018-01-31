#Data on the Rocks
#January 2018

#3D Movies

#REQUIRED PACKAGES
library(SpPack)
library(xlsx) #package to import xls files directly
library(scales) #for percent axis
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3
library(lubridate) #data manipulation with date
library(boxoffice) #importing movie data from boxoffice mojo


#Utilizing package 'boxoffice'
#import all box office mojo data for all movies in the box office top ten from Jan 1 06 through Dec 31 2016
movies <- boxoffice(start_date="06-01-01", end_date="16-12-31", number_of_results=10, verbose=T)

#View(movies)
summary(movies)
SpDesc(movies)

#Calculate summary statistics per movie
#movie name
#total box office
#year released

movies.sum <- movies %>% group_by(movie_name) %>% summarise(boxtot=max(gross_to_date)/1000000, 
                                                   year=min(year(date)), month=min(month(date)))

View(movies.sum)
summary(movies.sum)


#whole year
movies.year <- movies.sum %>% group_by(year) %>% summarise(boxoffice=sum(boxtot))
summary(movies.year)

Movies3D <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/3D/List of 3D Movies.xlsx", sheetName="Sheet1", colIndex=1)

movies.sum$ThreeD <- ifelse(movies.sum$movie_name %in% Movies3D$name, 1, 0)

table(movies.sum$ThreeD)


movies.sum.3DYear <- movies.sum %>% group_by(year, ThreeD) %>% summarise(num=n(), sumboxtot=sum(boxtot)) %>% 
  mutate(numprop = num/sum(num), boxprop = sumboxtot/sum(sumboxtot))

View(movies.sum.3DYear)

movies.sum.3DYear$ThreeDstr <- ifelse(movies.sum.3DYear$ThreeD==1,"3D","2D")

colours <- c("#090ea3", "#089126")#number of movies-deep blue, proportion of money-green
ThreeDPlot <- ggplot(subset(movies.sum.3DYear, ThreeD==1), aes(x=year, y=numprop, colour=ThreeDstr)) + 
  geom_vline(xintercept=2009.92, linetype="11", colour="royalblue", size=2) + 
  annotate("text", label="Avatar\nReleased", x=2009.5, y=.42, colour="royalblue", size=6, fontface="bold", hjust=1, vjust=0) +
  geom_line(size=2, colour=colours[1]) + 
  annotate("text", label="Movies Released in 3D", x=2011.5, y=.11, colour=colours[1], size=6, fontface="bold", hjust=0, vjust=0) +
  geom_line(aes(x=year, y=boxprop), size=2, colour=colours[2])+
  annotate("text", label="Revenue from 3D Movies", x=2016.5, y=.53, colour=colours[2], size=6, fontface="bold", hjust=1, vjust=0) +
  scale_x_continuous("Year", limits=c(2005.5,2017), breaks=c(seq(2006,2016,2)), expand = c(0,0)) +
  scale_y_continuous("Proportion", labels=percent, limits=c(0,.6), breaks=seq(0,.6,.1), expand = c(0,0)) +
  ggtitle("3D Movies") +
  DoR.Theme()
ThreeDPlot

ggsave(ThreeDPlot, filename="ThreeDPlot.png", width = 8, height=7, dpi=500)




#Values for simile
movies.sum.3DYear[21:22,]




#subset summer movies
movies.sum$Summer <- ifelse(movies.sum$month >=4 & movies.sum$month <=8, "Summer","Not Summer")

movies.sum.S <- subset(movies.sum, Summer=="Summer")

movies.sum.S$ThreeD <- ifelse(movies.sum.S$movie_name %in% Movies3D$name, 1, 0)

table(movies.sum.S$ThreeD)

View(subset(movies.sum.S, year==2008))

movies.sum.S.3DYear <- movies.sum.S %>% group_by(year, ThreeD) %>% summarise(num=n(), sumboxtot=sum(boxtot)) %>% 
  mutate(numprop = num/sum(num), boxprop = sumboxtot/sum(sumboxtot))

View(movies.sum.S.3DYear)

movies.sum.S.3DYear$ThreeDstr <- ifelse(movies.sum.S.3DYear$ThreeD==1,"3D","2D")

colours <- c("#090ea3", "#089126")#number of movies-deep blue, proportion of money-green
ThreeDPlot <- ggplot(subset(movies.sum.S.3DYear, ThreeD==1), aes(x=year, y=numprop, colour=ThreeDstr)) + 
  geom_vline(xintercept=2009.92, linetype="11", colour="royalblue", size=2) + 
  annotate("text", label="Avater\nReleased", x=2008.5, y=.42, colour="royalblue", size=6, fontface="bold", hjust=0.5, vjust=0) +
  geom_line(size=2, colour=colours[1]) + 
  annotate("text", label="Number of\n3D Movies", x=2013.5, y=.08, colour=colours[1], size=6, fontface="bold", hjust=0.5, vjust=0) +
  geom_line(aes(x=year, y=boxprop), size=2, colour=colours[2])+
  annotate("text", label="Money from\n3D Movies", x=2012.6, y=.54, colour=colours[2], size=6, fontface="bold", hjust=0.5, vjust=0) +
  scale_x_continuous("Year", limits=c(2005,2017), breaks=c(seq(2006,2016,2)), expand = c(0,0)) +
  scale_y_continuous("Proportion", labels=percent, limits=c(0,.65), breaks=seq(0,.6,.1), expand = c(0,0)) +
  ggtitle("Summer Movies in 3D") +
  DoR.Theme()
ThreeDPlot

ggsave(ThreeDPlot, filename="ThreeDPlot.png", width = 8, height=7, dpi=500)






#OLD APPROACH----

#DATA IMPORT----
Movies <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/3D/3D Movies Data.xlsx", sheetName="Raw")
View(Movies)
Sp.Desc(Movies)

#Adjust for inflation
Movies$Gross.Inf <- NA

for(i in 1:dim(Movies)[1]){
  Movies$Gross.Inf[i] <- Inf.Adj.Yr(Yr = Movies$Year[i], 
                                    In.Yr = 2016, 
                                    Value = Movies$Gross[i])
}
View(Movies)

#Calculate means split by 3D
Movies.3D <- Movies %>% group_by(Series, ThreeD) %>% summarize(Gross.Inf = mean(Gross.Inf),
                                                       RT = mean(RT),
                                                       RT.User = mean(RT.User),
                                                       Gross = mean(Gross))

Movies.3D$Gross.Inf.Mil <- Movies.3D$Gross.Inf/1000000
Movies.3D$Gross.Mil <- Movies.3D$Gross/1000000
Movies.3D$ThreeD.Str <- ifelse(Movies.3D$ThreeD==0,"2D","3D")

#GRAPHING DATA----
#colours <- c("#bf0d0d", "#090ea3", "#089126") #need to pick colors for each series
ThreeD.plot <- ggplot(Movies.3D, aes(x=RT, y=Gross.Inf.Mil, colour=Series)) + 
  #geom_point( size=5) + #aes(shape=ThreeD.Str),
  geom_path(aes(x=RT, y=Gross.Inf.Mil), arrow=arrow(length=unit(0.4,"cm")), size=2) +
  geom_text(data=subset(Movies.3D, ThreeD==0), aes(x=RT+1, y=Gross.Inf.Mil, label=Series), hjust=0) + 
  scale_x_continuous("Rotten Tomatoes User Score", limits=c(25,110), breaks=seq(20,100,20), expand = c(0,0)) +
  scale_y_continuous("Gross Revenue Adjsted for Inflation (Millions)", limits=c(100, 800), expand = c(0,0)) +
  ggtitle("3D Movies") +
  DoR.Theme()
ThreeD.plot

ggsave(Crime.plot, filename="Crime.plot.png", width = 8, height=7, dpi=500)

#Data for Simile
MaxData.R <- data.frame(Outcome=c("Murder.R", "Violent.R", "Property.R"), Max=rep(NA,3), Max.Year=rep(NA,3))
#Historical
MaxData.R$Max[1] <- max(Crime$Murder.R)
MaxData.R$Max.Year[1] <- Crime$Year[which.max(Crime$Murder.R)]
MaxData.R$Max[2] <- max(Crime$Violent.R)
MaxData.R$Max.Year[2] <- Crime$Year[which.max(Crime$Violent.R)]
MaxData.R$Max[3] <- max(Crime$Property.R)
MaxData.R$Max.Year[3] <- Crime$Year[which.max(Crime$Property.R)]
#Current
MaxData.R$Current[1] <- Crime$Murder.R[46]
MaxData.R$Current[2] <- Crime$Violent.R[46]
MaxData.R$Current[3] <- Crime$Property.R[46]

MaxData.R$Fraction <- MaxData.R$Current / MaxData.R$Max

MaxData.R

#geom_curve(aes(x=2016, xend=2016, y=MaxData2[3,2], yend=Crime$Property.C[46]), colour=colours[3], size=1, curvature=-.3) +


#Data to export
write.csv(Crime, file="Crime.csv", row.names=F)
