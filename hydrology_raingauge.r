## A start at cleaning rainfall data but only partially done, and not sure how 
## useful this is - if its not helpful this whole script can be deleted!

library(tidyverse)
rm(list=ls())

rain <- read.csv("C:/Data/Hydrology/Edinglassie_rain.csv")
str(rain)
summary(rain)
head(rain)



## need to reformat date in Excel as some in ddmmyyyy, ddmmyy, mmddyy etc. 
rain$Date_time <- gsub("/15 ","/2015 ",rain$Date_time)
rain$Date_time <- gsub("/16 ","/2016 ",rain$Date_time)
rain$Date_time <- gsub("/17 ","/2017 ",rain$Date_time)
rain$Date_time <- gsub("/18 ","/2018 ",rain$Date_time)
rain$Date_time <- gsub("/19 ","/2019 ",rain$Date_time)
rain <- separate(rain,2,c("Date","Time"),sep=" ",remove=T)
rain$Date <- as.Date(as.character(rain$Date),format="%m/%d/%Y")
rain$Event <- as.numeric(as.character(rain$Event))


max_event <- group_by(rain,Date) %>%
  summarise(max(Event,na.rm=TRUE)) 


names(max_event) <- c("Date","Event")
i=1
rainfall <- rep(0,length(max_event$Date))
for(i in 1:length(max_event$Date)){
  rainfall[i+1] <- (max_event$Event[i+1]-max_event$Event[i])*2
}
rainfall[1] <- max_event$Event[1]*2

write.csv(rainfall,"Z:/PA_hydrology/Hydrology_all_data/edinglassie_rainfall.csv")
