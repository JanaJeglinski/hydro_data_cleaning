## Hydrology data cleaning and correcting
## 08 July 2019
## Laura Allen
#///////////////////////////////////

##! issues abotu data etc. flagged with '#!' for easy searching

# Summary plan for all data
# 1. site level logger data needs to be put into right format, check units, correct for any drift over time and then adjsut the water level to correct for height of well and ground level.
# 2. once all files have been cleaned, multiple loggers per site should be combined into one doc, and daily averages calculated for each logger.
# 3. Daily averages of all loggers across sites can then be combined into one spreadsheet. 

# Stage 1 - individual site-logger files ----
# Need to process each logger file for each site, pre-2016 and post 2016, individually. Each has different correction factors.
# 1. Format data into separate columns and correct headings
# 2. check same units used (metres)
# 3. check metadata to see if drift has occurred - if yes, plot data to see if this is gradual or distict jump.
# 4. apply correction for drift to data if needed.
# 5. get distance to top of well from metadata, and subtract this to correct water level.
# 6. get ground height and adjust water level to mAOS (m above sea level OS ref)
# 7. export cleaned file, ready for combining with other loggers and year for same site.

## File structure to use ----

## dataframe with 1 row per logger reading. 
# Column headings: 
# date = date format yyyy-mm-dd 
# time = hh:mm 
# level_raw = reading from raw data (in metres)
# datetime = date and time combined, required for plotting data
# level_drift = water level corrected for any drift/jump (copy raw if no drift)
# level_corr = water level corrected for drift and then corrected for height to top of well
# level_os = water level corrected for drift and then corrected for ground elevation
# site = site name 
# logger = logger reference (site_logger no.)
# 



## useful code for extra corrections ----
# if you need to do extra data processing, these migth be useful for some cases:

##for applyng correction between date range 
# for date range need to subset data frame 
# ab02b[ab02b$date >= "2016-01-22" & ab02b$date <= "2018-02-09",5] <- ab02b[ab02b$date >= "2016-01-22" & ab02b$date <= "2018-02-09",5] - 0.005


rm(list=ls())
## Libraries ----
library(tidyverse)

# data for corrections ----
corr<- read.csv("C:/Data/Hydrology/hydrology_corrections.csv")
head(corr)
corr$top_of_well <- as.numeric(as.character(corr$top_of_well))
corr$logger_drift_corr <- as.numeric(as.character(corr$logger_drift_corr))
corr$elevation <- as.numeric(as.character(corr$elevation))

# Functions ----
## for checking, correcting and writing the hydrology summary datasets

# function to check hydrology data once formatted
check <- function(logid){
  #remove rows with NA readings from data
  df <- logid[-c(which(is.na(logid$level_raw))),]
  par(mfrow=c(1,1))
  plot(df$level_raw~df$datetime,type="l",xlab=c("date"),ylab=c("water level below surface (m)"))
  return(df)
}

#function to apply corrections to hydro data
hydrocorr  <- function(logid,drifttype,stepfrom){
  df <- merge(logid,corr,by=c("site","logger","period"),all.x=T)
  df$level_drift <- df$level_raw # copy raw data into column for corrected data
  if(missing(stepfrom)){
    stepfrom <- as.POSIXct("1990-01-01 00:00")}
  if(drifttype=="step"){
    st <- which(df$datetime > stepfrom)# when did step change in lgoger readings occur?
    df$level_drift[st]<- df$level_drift[st]+df$logger_drift_corr[st]} ## if it is a sudden step in logger values use this correction
  else if(drifttype=="gradual"){ #if logger drift is gradual use this correction
    driftval <- df$logger_drift_corr[1] ## value of total drift correction
    dr <- c(1:length(df[,1]))*(driftval/length(df[,1])) # incremental drift to be applied
    df$level_drift<- df$level_drift+dr
  }
  df$level_corr <- df$level_drift-df$top_of_well # subtract distance to top of well
  df$level_os <- df$elevation-df$level_corr # get elevation of water table (mASL) (ground level at xy coords - distance from surface)
  
  par(mfrow=c(3,1),mar=c(4,4,2,1))
  plot(df$level_raw~df$datetime,type="l",ylim=c(0,5),xlab=c("date"),ylab=c("water level below surface (m)"))#plots to check data after applying corrections
  mtext("raw readings",side=3)
  plot(df$level_drift~df$datetime,type="l",ylim=c(0,5),xlab=c("date"),ylab=c("water level below surface (m)"))#plots to check data after applying corrections
  mtext("logger drift corrected",side=3)
  plot(df$level_corr~df$datetime,type="l",ylim=c(0,5),xlab=c("date"),ylab=c("water level below surface (m)"))
  abline(0,0,lty="dashed",col="red")
  mtext("distance to top of well subtracted",side=3)
  
  #calculate daily means
  df_mean_levels <- group_by(df,date) %>%
    summarise_at(c("level_raw","level_drift","level_corr","level_os"),mean,na.rm=TRUE) 
  df_mean_levels$site <- rep(df$site[1],length(df_mean_levels[,1]))
  df_mean_levels$logger <- rep(df$logger[1],length(df_mean_levels[,1]))
  df_mean_levels$period <- rep(df$period [1],length(df_mean_levels[,1]))
  return(df_mean_levels)
}

#function to write file once you have checked output
hydrowrite <- function(mean_data){
  filepath <- paste0("C:/Data/Hydrology/cleaned_site_logger/",mean_data$site[1],"_",mean_data$logger[1],"_",mean_data$period[1],".csv")
  write.csv(mean_data,filepath)
  
  pngpath <- paste0("C:/Data/Hydrology/cleaned_site_logger/plots/",mean_data$site[1],"_",mean_data$logger[1],"_",mean_data$period[1],".png")
  png(pngpath,res=300, width=20, height=17, units="cm",pointsize=12)       
  par(mfrow=c(1,1),mar=c(4,4,2,1))
  plot(mean_data$level_corr~mean_data$date,type="l",ylim=c(-1,3),xlab=c("Date"),ylab=c("Water level below surface (m)"))
  abline(0,0,lty="dashed",col="red")
  labpaste <- paste0("Corrected hydrology data for"," ",mean_data$site[1]," ",mean_data$logger[1]," ",mean_data$period[1])
  mtext(labpaste,side=3)
  dev.off()
}



### Abernethy ----
## pre 2016
# logger 1
## reshaping data to standard format to be done manually, as lots of small differences between files
ab01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Abernethy/Aberneth01/ABERNETH01 0001 20150317.csv")
str(ab01a)
names(ab01a) <- c("date","time","level_raw") #rename columns
ab01a$date <- as.Date(as.character(ab01a$date),format="%d.%m.%Y") ## change dates to date format
ab01a$level_raw <- as.numeric(as.character(ab01a$level_raw)) ## format water level as numeric
ab01a$datetime <- paste(ab01a$date,ab01a$time)
ab01a$datetime <- as.POSIXct(ab01a$datetime, format="%Y-%m-%d %H:%M") # posixct format includes both date and time 
ab01a$site <- c(rep("Abernethy", length(ab01a[,1])))
ab01a$logger <- c(rep("ABERNETH01", length(ab01a[,1])))
ab01a$period <- c(rep("pre2016", length(ab01a[,1])))
head(ab01a)

ab01a <- check(ab01a) #check data
meandf_a <- hydrocorr(logid=ab01a,drifttype = "step") # calculate corrected daily means
hydrowrite(meandf_a)   # write csv file and plot
## used 'step' correction across whole datset, based on plotting data and comparing with post2016

### NEXT STEP - decide if driftenddate is required, and compare pre-post 2016 for ab01.. then go on to do next logger

## Post-2016
# Logger 1 
ab01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Abernethy/Data/Groundwater/ABERNETH01 20180209131537.csv")
str(ab01b)
ab01b<- separate(ab01b,1,c("Date","Time","Level","ch2","ch3"),sep=";",remove=T)
ab01b <- ab01b[,c(1:3)]
names(ab01b) <- c("date","time","level_raw") #rename columns
ab01b$date <- as.Date(as.character(ab01b$date),format="%d.%m.%Y") ## change dates to date format
ab01b$level_raw <- as.numeric(as.character(ab01b$level_raw)) ## format water level as numeric
ab01b$datetime <- paste(ab01b$date,ab01b$time)
ab01b$datetime <- as.POSIXct(ab01b$datetime, format="%Y-%m-%d %H:%M")
ab01b$site <- c(rep("Abernethy", length(ab01b[,1])))
ab01b$logger <- c(rep("ABERNETH01", length(ab01b[,1])))
ab01b$period <- c(rep("post2016", length(ab01b[,1])))
head(ab01b)

ab01b <- check(ab01b) #check data
meandf_b <- hydrocorr(logid=ab01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

##/////////////////////////
## logger 2
#pre2016
ab02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Abernethy/Aberneth02/ABERNETH02 0001 20150317.csv")
str(ab02a)
names(ab02a) <- c("date","time","level_raw") #rename columns
ab02a$date <- as.Date(as.character(ab02a$date),format="%d.%m.%Y") ## change dates to date format
ab02a$level_raw <- as.numeric(as.character(ab02a$level_raw)) ## format water level as numeric
ab02a$datetime <- paste(ab02a$date,ab02a$time)
ab02a$datetime <- as.POSIXct(ab02a$datetime, format="%Y-%m-%d %H:%M")
ab02a$site <- c(rep("Abernethy", length(ab02a[,1])))
ab02a$logger <- c(rep("ABERNETH02", length(ab02a[,1])))
ab02a$period <- c(rep("pre2016", length(ab02a[,1])))
head(ab02a)

ab02a <- check(ab02a) #check data
meandf_a <- hydrocorr(logid=ab02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post 2016
# Logger 2 
ab02b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Abernethy/Data/Groundwater/ABERNETH02 20180209120557.csv")
str(ab02b)
ab02b<- separate(ab02b,1,c("Date","Time","Level","ch2","ch3"),sep=";",remove=T)
ab02b <- ab02b[,c(1:3)]
names(ab02b) <- c("date","time","level_raw") #rename columns
ab02b$date <- as.Date(as.character(ab02b$date),format="%d.%m.%Y") ## change dates to date format
ab02b$level_raw <- as.numeric(as.character(ab02b$level_raw)) ## format water level as numeric
ab02b$datetime <- paste(ab02b$date,ab02b$time)
ab02b$datetime <- as.POSIXct(ab02b$datetime, format="%Y-%m-%d %H:%M")
ab02b$site <- c(rep("Abernethy", length(ab02b[,1])))
ab02b$logger <- c(rep("ABERNETH02", length(ab02b[,1])))
ab02b$period <- c(rep("post2016", length(ab02b[,1])))
head(ab02b)


ab02b <- check(ab02b) #check data
meandf_b <- hydrocorr(logid=ab02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


##///////////////////////////
## logger 3
#pre 2016

ab03a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Abernethy/Aberneth03/ABERNETH03 0001 20150317.csv")
str(ab03a)
names(ab03a) <- c("date","time","level_raw") #rename columns
ab03a$date <- as.Date(as.character(ab03a$date),format="%d.%m.%Y") ## change dates to date format
ab03a$level_raw <- as.numeric(as.character(ab03a$level_raw)) ## format water level as numeric
ab03a$datetime <- paste(ab03a$date,ab03a$time)
ab03a$datetime <- as.POSIXct(ab03a$datetime, format="%Y-%m-%d %H:%M")
ab03a$site <- c(rep("Abernethy", length(ab03a[,1])))
ab03a$logger <- c(rep("ABERNETH03", length(ab03a[,1])))
ab03a$period <- c(rep("pre2016", length(ab03a[,1])))
head(ab03a)


ab03a <- check(ab03a) #check data
# ab03a <- ab03a[-1,] 
# plot(ab03a$level_raw~ab03a$datetime,type="l") # first few data points look dodgy - probably logger still adjusting
meandf_a <- hydrocorr(logid=ab03a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


#post 2016
# Logger 3 
ab03b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Abernethy/Data/Groundwater/ABERNETH02 20180209120557.csv")
str(ab03b)
ab03b<- separate(ab03b,1,c("Date","Time","Level","ch2","ch3"),sep=";",remove=T)
ab03b <- ab03b[,c(1:3)]
names(ab03b) <- c("date","time","level_raw") #rename columns
ab03b$date <- as.Date(as.character(ab03b$date),format="%d.%m.%Y") ## change dates to date format
ab03b$level_raw <- as.numeric(as.character(ab03b$level_raw)) ## format water level as numeric
ab03b$datetime <- paste(ab03b$date,ab03b$time)
ab03b$datetime <- as.POSIXct(ab03b$datetime, format="%Y-%m-%d %H:%M")
ab03b$site <- c(rep("Abernethy", length(ab03b[,1])))
ab03b$logger <- c(rep("ABERNETH03", length(ab03b[,1])))
ab03b$period <- c(rep("post2016", length(ab03b[,1])))
head(ab03b)

ab03b <- check(ab03b) #check data
meandf_b <- hydrocorr(logid=ab03b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

## *!* I have assumed a typo in the distance to top of well: changed from 1.62 to 0.62 for this logger.
# Logger was changed at some point, so this could explain the differences between years, need to check.
# 


##///////////////////////////
### Glenmullie ----
## site 1 
# pre 2016
gm01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Glenmullie/Site 1/0GLENMULL1 0001 20150318.csv")
str(gm01a)
gm01a <- separate(gm01a,1,c("date","time","level_raw"),sep=";",remove=T)
gm01a$date <- as.Date(as.character(gm01a$date),format="%d.%m.%Y") ## change dates to date format
gm01a$level_raw <- as.numeric(as.character(gm01a$level_raw)) ## format water level as numeric
gm01a$datetime <- paste(gm01a$date,gm01a$time)
gm01a$datetime <- as.POSIXct(gm01a$datetime, format="%Y-%m-%d %H:%M")
gm01a$site <- c(rep("Glenmullie", length(gm01a[,1])))
gm01a$logger <- c(rep("0GLENMULL1", length(gm01a[,1])))
gm01a$period <- c(rep("pre2016", length(gm01a[,1])))
head(gm01a)

gm01a <- check(gm01a) #check data
tail(gm01a)
gm01a <- gm01a[-length(gm01a[,1]),] #removed last reading (probably when checking logger
meandf_a <- hydrocorr(logid=gm01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
gm01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Glenmullie/Data/Groundwater/0GLENMULL1 20180412125459.csv")
str(gm01b)
gm01b <- separate(gm01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gm01b <- gm01b[,c(1:3)]
gm01b$date <- as.Date(as.character(gm01b$date),format="%d.%m.%Y") ## change dates to date format
gm01b$level_raw <- as.numeric(as.character(gm01b$level_raw)) ## format water level as numeric
gm01b$datetime <- paste(gm01b$date,gm01b$time)
gm01b$datetime <- as.POSIXct(gm01b$datetime, format="%Y-%m-%d %H:%M")
gm01b$site <- c(rep("Glenmullie", length(gm01b[,1])))
gm01b$logger <- c(rep("0GLENMULL1", length(gm01b[,1])))
gm01b$period <- c(rep("post2016", length(gm01b[,1])))
head(gm01b)

gm01b <- check(gm01b) #check data
meandf_b <- hydrocorr(logid=gm01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

## check proportions of days waterlavel <15cm
length(which(meandf_a$level_corr > 0.15))/length(meandf_a$date)
length(which(meandf_b$level_corr > 0.15))/length(meandf_b$date)

###////////////
## site 2 
# pre 2016
gm02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Glenmullie/Site 2/0GLENMULL2 0001 20150318.csv")
str(gm02a)
gm02a <- separate(gm02a,1,c("date","time","level_raw"),sep=";",remove=T)
gm02a$date <- as.Date(as.character(gm02a$date),format="%d.%m.%Y") ## change dates to date format
gm02a$level_raw <- as.numeric(as.character(gm02a$level_raw)) ## format water level as numeric
gm02a$datetime <- paste(gm02a$date,gm02a$time)
gm02a$datetime <- as.POSIXct(gm02a$datetime, format="%Y-%m-%d %H:%M")
gm02a$site <- c(rep("Glenmullie", length(gm02a[,1])))
gm02a$logger <- c(rep("0GLENMULL2", length(gm02a[,1])))
gm02a$period <- c(rep("pre2016", length(gm02a[,1])))
head(gm02a)

gm02a <- check(gm02a) #check data
meandf_a <- hydrocorr(logid=gm02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
gm02b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Glenmullie/Data/Groundwater/0GLENMULL2 20180412112419.csv")
str(gm02b)
gm02b <- separate(gm02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gm02b <- gm02b[,c(1:3)]
gm02b$date <- as.Date(as.character(gm02b$date),format="%d.%m.%Y") ## change dates to date format
gm02b$level_raw <- as.numeric(as.character(gm02b$level_raw)) ## format water level as numeric
gm02b$datetime <- paste(gm02b$date,gm02b$time)
gm02b$datetime <- as.POSIXct(gm02b$datetime, format="%Y-%m-%d %H:%M")
gm02b$site <- c(rep("Glenmullie", length(gm02b[,1])))
gm02b$logger <- c(rep("0GLENMULL2", length(gm02b[,1])))
gm02b$period <- c(rep("post2016", length(gm02b[,1])))
head(gm02b)

gm02b <- check(gm02b) #check data
meandf_b <- hydrocorr(logid=gm02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


###////////////
## site 3 
# pre 2016 - no data

# post2016
gm03b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Glenmullie/Data/Groundwater/0GLENMULL3 20180412105228.csv")
str(gm03b)
gm03b <- separate(gm03b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gm03b <- gm03b[,c(1:3)]
gm03b$date <- as.Date(as.character(gm03b$date),format="%d.%m.%Y") ## change dates to date format
gm03b$level_raw <- as.numeric(as.character(gm03b$level_raw)) ## format water level as numeric
gm03b$datetime <- paste(gm03b$date,gm03b$time)
gm03b$datetime <- as.POSIXct(gm03b$datetime, format="%Y-%m-%d %H:%M")
gm03b$site <- c(rep("Glenmullie", length(gm03b[,1])))
gm03b$logger <- c(rep("0GLENMULL3", length(gm03b[,1])))
gm03b$period <- c(rep("post2016", length(gm03b[,1])))
head(gm03b)

gm03b <- check(gm03b) #check data
meandf_b <- hydrocorr(logid=gm03b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file


###////////////
## site 4 
# pre 2016 - no data

# post2016
gm04b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Glenmullie/Data/Groundwater/0GLENMULL4 20180412120351.csv")
str(gm04b)
gm04b <- separate(gm04b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gm04b <- gm04b[,c(1:3)]
gm04b$date <- as.Date(as.character(gm04b$date),format="%d.%m.%Y") ## change dates to date format
gm04b$level_raw <- as.numeric(as.character(gm04b$level_raw)) ## format water level as numeric
gm04b$datetime <- paste(gm04b$date,gm04b$time)
gm04b$datetime <- as.POSIXct(gm04b$datetime, format="%Y-%m-%d %H:%M")
gm04b$site <- c(rep("Glenmullie", length(gm04b[,1])))
gm04b$logger <- c(rep("0GLENMULL4", length(gm04b[,1])))
gm04b$period <- c(rep("post2016", length(gm04b[,1])))
head(gm04b)

gm04b <- check(gm04b) #check data
gm04b <- gm04b[-1,] #removed first row as date seems incorrect
meandf_b <- hydrocorr(logid=gm04b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file


## ///////////////////////////////
## Auchnafree -----
### Site 1
# pre 2016
au01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Auchnafree/Site 1/0ACHNAFR01 0001 20150316.csv")
str(au01a)
au01a <- separate(au01a,1,c("date","time","level_raw"),sep=";",remove=T)
au01a$date <- as.Date(as.character(au01a$date),format="%d.%m.%Y") ## change dates to date format
au01a$level_raw <- as.numeric(as.character(au01a$level_raw)) ## format water level as numeric
au01a$datetime <- paste(au01a$date,au01a$time)
au01a$datetime <- as.POSIXct(au01a$datetime, format="%Y-%m-%d %H:%M")
au01a$site <- c(rep("Auchnafree", length(au01a[,1])))
au01a$logger <- c(rep("0ACHNAFR01", length(au01a[,1])))
au01a$period <- c(rep("pre2016", length(au01a[,1])))
head(au01a)

au01a <- check(au01a) #check data
meandf_a <- hydrocorr(logid=au01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
au01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Auchnafree/Data/Groundwater/0ACHNAFR01 20180808112700.csv")
str(au01b)
au01b <- separate(au01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
au01b <- au01b[,c(1:3)]
au01b$date <- as.Date(as.character(au01b$date),format="%d.%m.%Y") ## change dates to date format
au01b$level_raw <- as.numeric(as.character(au01b$level_raw)) ## format water level as numeric
au01b$datetime <- paste(au01b$date,au01b$time)
au01b$datetime <- as.POSIXct(au01b$datetime, format="%Y-%m-%d %H:%M")
au01b$site <- c(rep("Auchnafree", length(au01b[,1])))
au01b$logger <- c(rep("0ACHNAFR01", length(au01b[,1])))
au01b$period <- c(rep("post2016", length(au01b[,1])))
head(au01b)

au01b <- check(au01b) #check data
meandf_b <- hydrocorr(logid=au01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

##///////////
### Site 2
# pre 2016
au02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Auchnafree/Site 2/0ACHNAFR02 0001 20150316.csv")
str(au02a)
au02a <- separate(au02a,1,c("date","time","level_raw"),sep=";",remove=T)
au02a$date <- as.Date(as.character(au02a$date),format="%d.%m.%Y") ## change dates to date format
au02a$level_raw <- as.numeric(as.character(au02a$level_raw)) ## format water level as numeric
au02a$datetime <- paste(au02a$date,au02a$time)
au02a$datetime <- as.POSIXct(au02a$datetime, format="%Y-%m-%d %H:%M")
au02a$site <- c(rep("Auchnafree", length(au02a[,1])))
au02a$logger <- c(rep("0ACHNAFR02", length(au02a[,1])))
au02a$period <- c(rep("pre2016", length(au02a[,1])))
head(au02a)

au02a <- check(au02a) #check data
meandf_a <- hydrocorr(logid=au02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
au02b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Auchnafree/Data/Groundwater/0ACHNAFR02 20180808131334.csv")
str(au02b)
au02b <- separate(au02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
au02b <- au02b[,c(1:3)]
au02b$date <- as.Date(as.character(au02b$date),format="%d.%m.%Y") ## change dates to date format
au02b$level_raw <- as.numeric(as.character(au02b$level_raw)) ## format water level as numeric
au02b$datetime <- paste(au02b$date,au02b$time)
au02b$datetime <- as.POSIXct(au02b$datetime, format="%Y-%m-%d %H:%M")
au02b$site <- c(rep("Auchnafree", length(au02b[,1])))
au02b$logger <- c(rep("0ACHNAFR02", length(au02b[,1])))
au02b$period <- c(rep("post2016", length(au02b[,1])))
head(au02b)

au02b <- check(au02b) #check data
meandf_b <- hydrocorr(logid=au02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

## ///////////////////////////////
## Barlosh Moss  -----
# pre 2016 - no data

# post2016
ba01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Barlosh Moss/Data/BARLOSHM01 20180319134147.csv")
str(ba01b)
ba01b <- separate(ba01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
ba01b <- ba01b[,c(1:3)]
ba01b$date <- as.Date(as.character(ba01b$date),format="%d.%m.%Y") ## change dates to date format
ba01b$level_raw <- as.numeric(as.character(ba01b$level_raw)) ## format water level as numeric
ba01b$datetime <- paste(ba01b$date,ba01b$time)
ba01b$datetime <- as.POSIXct(ba01b$datetime, format="%Y-%m-%d %H:%M")
ba01b$site <- c(rep("Barlosh Moss", length(ba01b[,1])))
ba01b$logger <- c(rep("BARLOSHM01", length(ba01b[,1])))
ba01b$period <- c(rep("post2016", length(ba01b[,1])))
head(ba01b)

ba01b <- check(ba01b) #check data
meandf_b <- hydrocorr(logid=ba01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

## ///////////////////////////////
## Ben Lawers -----
### Site 1
# pre 2016
bl01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Ben Lawers/Site 1/BENLAWERS1 0001 20150612.csv")
str(bl01a)
bl01a <- separate(bl01a,1,c("date","time","level_raw"),sep=";",remove=T)
bl01a$date <- as.Date(as.character(bl01a$date),format="%d.%m.%Y") ## change dates to date format
bl01a$level_raw <- as.numeric(as.character(bl01a$level_raw)) ## format water level as numeric
bl01a$datetime <- paste(bl01a$date,bl01a$time)
bl01a$datetime <- as.POSIXct(bl01a$datetime, format="%Y-%m-%d %H:%M")
bl01a$site <- c(rep("Ben Lawers", length(bl01a[,1])))
bl01a$logger <- c(rep("BENLAWERS1", length(bl01a[,1])))
bl01a$period <- c(rep("pre2016", length(bl01a[,1])))
head(bl01a)

bl01a <- check(bl01a) #check data
meandf_a <- hydrocorr(logid=bl01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
bl01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Ben Lawers/Data/Groundwater/BENLAWERS1 20180427121627.csv")
str(bl01b)
bl01b <- separate(bl01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
bl01b <- bl01b[,c(1:3)]
bl01b$date <- as.Date(as.character(bl01b$date),format="%d.%m.%Y") ## change dates to date format
bl01b$level_raw <- as.numeric(as.character(bl01b$level_raw)) ## format water level as numeric
bl01b$datetime <- paste(bl01b$date,bl01b$time)
bl01b$datetime <- as.POSIXct(bl01b$datetime, format="%Y-%m-%d %H:%M")
bl01b$site <- c(rep("Ben Lawers", length(bl01b[,1])))
bl01b$logger <- c(rep("BENLAWERS1", length(bl01b[,1])))
bl01b$period <- c(rep("post2016", length(bl01b[,1])))
head(bl01b)

bl01b <- check(bl01b) #check data
meandf_b <- hydrocorr(logid=bl01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


### Site 2
# pre 2016
bl02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Ben Lawers/Site 2/BENLAWERS2 0001 20150612.csv")
str(bl02a)
bl02a <- separate(bl02a,1,c("date","time","level_raw"),sep=";",remove=T)
bl02a$date <- as.Date(as.character(bl02a$date),format="%d.%m.%Y") ## change dates to date format
bl02a$level_raw <- as.numeric(as.character(bl02a$level_raw)) ## format water level as numeric
bl02a$datetime <- paste(bl02a$date,bl02a$time)
bl02a$datetime <- as.POSIXct(bl02a$datetime, format="%Y-%m-%d %H:%M")
bl02a$site <- c(rep("Ben Lawers", length(bl02a[,1])))
bl02a$logger <- c(rep("BENLAWERS2", length(bl02a[,1])))
bl02a$period <- c(rep("pre2016", length(bl02a[,1])))
head(bl02a)

bl02a <- check(bl02a) #check data
meandf_a <- hydrocorr(logid=bl02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
bl02b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Ben Lawers/Data/Groundwater/BENLAWERS2 20180427141847.csv")
str(bl02b)
bl02b <- separate(bl02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
bl02b <- bl02b[,c(1:3)]
bl02b$date <- as.Date(as.character(bl02b$date),format="%d.%m.%Y") ## change dates to date format
bl02b$level_raw <- as.numeric(as.character(bl02b$level_raw)) ## format water level as numeric
bl02b$datetime <- paste(bl02b$date,bl02b$time)
bl02b$datetime <- as.POSIXct(bl02b$datetime, format="%Y-%m-%d %H:%M")
bl02b$site <- c(rep("Ben Lawers", length(bl02b[,1])))
bl02b$logger <- c(rep("BENLAWERS2", length(bl02b[,1])))
bl02b$period <- c(rep("post2016", length(bl02b[,1])))
head(bl02b)

bl02b <- check(bl02b) #check data
meandf_b <- hydrocorr(logid=bl02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

## ///////////////////////////////
## Ben Lawers -----
### Site 1
# pre 2016
blo01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Ben Lomond/Ben Lomond 01/BENLOMON01 0001 20141106.csv")
str(blo01a)
blo01a <- separate(blo01a,1,c("date","time","level_raw"),sep=";",remove=T)
blo01a$date <- as.Date(as.character(blo01a$date),format="%d.%m.%Y") ## change dates to date format
blo01a$level_raw <- as.numeric(as.character(blo01a$level_raw)) ## format water level as numeric
blo01a$datetime <- paste(blo01a$date,blo01a$time)
blo01a$datetime <- as.POSIXct(blo01a$datetime, format="%Y-%m-%d %H:%M")
blo01a$site <- c(rep("Ben Lomond", length(blo01a[,1])))
blo01a$logger <- c(rep("BENLOMON01", length(blo01a[,1])))
blo01a$period <- c(rep("pre2016", length(blo01a[,1])))
head(blo01a)

blo01a <- check(blo01a) #check data
meandf_a <- hydrocorr(logid=blo01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
blo01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Ben Lomond/Data/Groundwater/BENLOMON01 20180329120901.csv")
str(blo01b)
blo01b <- separate(blo01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
blo01b <- blo01b[,c(1:3)]
blo01b$date <- as.Date(as.character(blo01b$date),format="%d.%m.%Y") ## change dates to date format
blo01b$level_raw <- as.numeric(as.character(blo01b$level_raw)) ## format water level as numeric
blo01b$datetime <- paste(blo01b$date,blo01b$time)
blo01b$datetime <- as.POSIXct(blo01b$datetime, format="%Y-%m-%d %H:%M")
blo01b$site <- c(rep("Ben Lomond", length(blo01b[,1])))
blo01b$logger <- c(rep("BENLOMON01", length(blo01b[,1])))
blo01b$period <- c(rep("post2016", length(blo01b[,1])))
head(blo01b)

blo01b <- check(blo01b) #check data
meandf_b <- hydrocorr(logid=blo01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


### Site 2 //////////
# pre 2016
blo02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Ben Lomond/Ben Lomond 02/BENLOMON02 0001 20141106.csv")
str(blo02a)
blo02a <- separate(blo02a,1,c("date","time","level_raw"),sep=";",remove=T)
blo02a$date <- as.Date(as.character(blo02a$date),format="%d.%m.%Y") ## change dates to date format
blo02a$level_raw <- as.numeric(as.character(blo02a$level_raw)) ## format water level as numeric
blo02a$datetime <- paste(blo02a$date,blo02a$time)
blo02a$datetime <- as.POSIXct(blo02a$datetime, format="%Y-%m-%d %H:%M")
blo02a$site <- c(rep("Ben Lomond", length(blo02a[,1])))
blo02a$logger <- c(rep("BENLOMON02", length(blo02a[,1])))
blo02a$period <- c(rep("pre2016", length(blo02a[,1])))
head(blo02a)

blo02a <- check(blo02a) #check data
meandf_a <- hydrocorr(logid=blo02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
blo02b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Ben Lomond/Data/Groundwater/BENLOMON02 20180329130400.csv")
str(blo02b)
blo02b <- separate(blo02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
blo02b <- blo02b[,c(1:3)]
blo02b$date <- as.Date(as.character(blo02b$date),format="%d.%m.%Y") ## change dates to date format
blo02b$level_raw <- as.numeric(as.character(blo02b$level_raw)) ## format water level as numeric
blo02b$datetime <- paste(blo02b$date,blo02b$time)
blo02b$datetime <- as.POSIXct(blo02b$datetime, format="%Y-%m-%d %H:%M")
blo02b$site <- c(rep("Ben Lomond", length(blo02b[,1])))
blo02b$logger <- c(rep("BENLOMON02", length(blo02b[,1])))
blo02b$period <- c(rep("post2016", length(blo02b[,1])))
head(blo02b)

blo02b <- check(blo02b) #check data
meandf_b <- hydrocorr(logid=blo02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


## ///////////////////////////////
## Black Moss Muir of Dinnet-----
### Site 1
# pre 2016
bd01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Black Moss Muir of Dinnet/Site 1/BLACKMOSS1 0001 20150304.csv")
str(bd01a)
names(bd01a) <- c("date","time","level_raw")
bd01a$date <- as.Date(as.character(bd01a$date),format="%d.%m.%Y") ## change dates to date format
bd01a$level_raw <- as.numeric(as.character(bd01a$level_raw)) ## format water level as numeric
bd01a$datetime <- paste(bd01a$date,bd01a$time)
bd01a$datetime <- as.POSIXct(bd01a$datetime, format="%Y-%m-%d %H:%M")
bd01a$site <- c(rep("Black Moss Muir of Dinnet", length(bd01a[,1])))
bd01a$logger <- c(rep("BLACKMOSS1", length(bd01a[,1])))
bd01a$period <- c(rep("pre2016", length(bd01a[,1])))
head(bd01a)

bd01a <- check(bd01a) #check data
meandf_a <- hydrocorr(logid=bd01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
bd01b_1 <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Black Moss Muir of Dinnet/Data/BLACKMOSS1 20180119133316.csv")
bd01b_2 <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Black Moss Muir of Dinnet/Data/BLACKMOSS1 20180410111123.csv")
bd01b_3 <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Black Moss Muir of Dinnet/Data/BLACKMOSS1 20180410114913.csv")
bd01b_4 <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Black Moss Muir of Dinnet/Data/BLACKMOSS1 20180410123439.csv")

bd01b <- rbind(bd01b_1,bd01b_2,bd01b_3,bd01b_4)

str(bd01b)
bd01b <- separate(bd01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
bd01b <- bd01b[,c(1:3)]
bd01b$date <- as.Date(as.character(bd01b$date),format="%d.%m.%Y") ## change dates to date format
bd01b$level_raw <- as.numeric(as.character(bd01b$level_raw)) ## format water level as numeric
bd01b$datetime <- paste(bd01b$date,bd01b$time)
bd01b$datetime <- as.POSIXct(bd01b$datetime, format="%Y-%m-%d %H:%M")
bd01b$site <- c(rep("Black Moss Muir of Dinnet", length(bd01b[,1])))
bd01b$logger <- c(rep("BLACKMOSS1", length(bd01b[,1])))
bd01b$period <- c(rep("post2016", length(bd01b[,1])))
head(bd01b)

bd01b <- check(bd01b) #check data
meandf_b <- hydrocorr(logid=bd01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 2 ////////////
# pre 2016
bd02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Black Moss Muir of Dinnet/Site 2/BLACKMOSS2 0001 20150304.csv")
str(bd02a)
names(bd02a) <- c("date","time","level_raw")
bd02a$date <- as.Date(as.character(bd02a$date),format="%d.%m.%Y") ## change dates to date format
bd02a$level_raw <- as.numeric(as.character(bd02a$level_raw)) ## format water level as numeric
bd02a$datetime <- paste(bd02a$date,bd02a$time)
bd02a$datetime <- as.POSIXct(bd02a$datetime, format="%Y-%m-%d %H:%M")
bd02a$site <- c(rep("Black Moss Muir of Dinnet", length(bd02a[,1])))
bd02a$logger <- c(rep("BLACKMOSS2", length(bd02a[,1])))
bd02a$period <- c(rep("pre2016", length(bd02a[,1])))
head(bd02a)

bd02a <- check(bd02a) #check data
meandf_a <- hydrocorr(logid=bd02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
bd02b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Black Moss Muir of Dinnet/Data/BLACKMOSS2 20180119142017.csv")

str(bd02b)
bd02b <- separate(bd02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
bd02b <- bd02b[,c(1:3)]
bd02b$date <- as.Date(as.character(bd02b$date),format="%d.%m.%Y") ## change dates to date format
bd02b$level_raw <- as.numeric(as.character(bd02b$level_raw)) ## format water level as numeric
bd02b$datetime <- paste(bd02b$date,bd02b$time)
bd02b$datetime <- as.POSIXct(bd02b$datetime, format="%Y-%m-%d %H:%M")
bd02b$site <- c(rep("Black Moss Muir of Dinnet", length(bd02b[,1])))
bd02b$logger <- c(rep("BLACKMOSS2", length(bd02b[,1])))
bd02b$period <- c(rep("post2016", length(bd02b[,1])))
head(bd02b)

bd02b <- check(bd02b) #check data
meandf_b <- hydrocorr(logid=bd02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


## Blawhorn Moss - #!# No correction data available 

## ///////////////////////////////
## Cander Moss -----
### Site 1
# pre 2016
cm01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Cander Moss/Site 1/0CANDERMO1 0001 20141029.csv")
str(cm01a)
cm01a <- separate(cm01a,1,c("date","time","level_raw"),sep=";",remove=T)
cm01a$date <- as.Date(as.character(cm01a$date),format="%d.%m.%Y") ## change dates to date format
cm01a$level_raw <- as.numeric(as.character(cm01a$level_raw)) ## format water level as numeric
cm01a$datetime <- paste(cm01a$date,cm01a$time)
cm01a$datetime <- as.POSIXct(cm01a$datetime, format="%Y-%m-%d %H:%M")
cm01a$site <- c(rep("Cander Moss", length(cm01a[,1])))
cm01a$logger <- c(rep("0CANDERMO1", length(cm01a[,1])))
cm01a$period <- c(rep("pre2016", length(cm01a[,1])))
head(cm01a)

cm01a <- check(cm01a) #check data
meandf_a <- hydrocorr(logid=cm01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
cm01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Cander Moss/Data/Groundwater/0CANDERMO1 20180206155624.csv")

str(cm01b)
cm01b <- separate(cm01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
cm01b <- cm01b[,c(1:3)]
cm01b$date <- as.Date(as.character(cm01b$date),format="%d.%m.%Y") ## change dates to date format
cm01b$level_raw <- as.numeric(as.character(cm01b$level_raw)) ## format water level as numeric
cm01b$datetime <- paste(cm01b$date,cm01b$time)
cm01b$datetime <- as.POSIXct(cm01b$datetime, format="%Y-%m-%d %H:%M")
cm01b$site <- c(rep("Cander Moss", length(cm01b[,1])))
cm01b$logger <- c(rep("0CANDERMO1", length(cm01b[,1])))
cm01b$period <- c(rep("post2016", length(cm01b[,1])))
head(cm01b)

cm01b <- check(cm01b) #check data
meandf_b <- hydrocorr(logid=cm01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

## ///////////////////////////////
## Cardowan Moss -----
### Site 1
# pre 2016
cd01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Cardowan Moss/Site 1/CARDOWAN01 0001 20150327.csv")
str(cd01a)
cd01a <- separate(cd01a,1,c("date","time","level_raw"),sep=";",remove=T)
cd01a$date <- as.Date(as.character(cd01a$date),format="%d.%m.%Y") ## change dates to date format
cd01a$level_raw <- as.numeric(as.character(cd01a$level_raw)) ## format water level as numeric
cd01a$datetime <- paste(cd01a$date,cd01a$time)
cd01a$datetime <- as.POSIXct(cd01a$datetime, format="%Y-%m-%d %H:%M")
cd01a$site <- c(rep("Cardowan Moss", length(cd01a[,1])))
cd01a$logger <- c(rep("CARDOWAN01", length(cd01a[,1])))
cd01a$period <- c(rep("pre2016", length(cd01a[,1])))
head(cd01a)

cd01a <- check(cd01a) #check data
meandf_a <- hydrocorr(logid=cd01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

### Site 2
# pre 2016
cd02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Cardowan Moss/Site 2/0000357787 0001 20141022.csv")
str(cd02a)
cd02a <- separate(cd02a,1,c("date","time","level_raw"),sep=";",remove=T)
cd02a$date <- as.Date(as.character(cd02a$date),format="%d.%m.%Y") ## change dates to date format
cd02a$level_raw <- as.numeric(as.character(cd02a$level_raw)) ## format water level as numeric
cd02a$datetime <- paste(cd02a$date,cd02a$time)
cd02a$datetime <- as.POSIXct(cd02a$datetime, format="%Y-%m-%d %H:%M")
cd02a$site <- c(rep("Cardowan Moss", length(cd02a[,1])))
cd02a$logger <- c(rep("CARDOWAN02", length(cd02a[,1])))
cd02a$period <- c(rep("pre2016", length(cd02a[,1])))
head(cd02a)

cd02a <- check(cd02a) #check data
meandf_a <- hydrocorr(logid=cd02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
## loggers destroyed


## ///////////////////////////////
## Goatfell -----
### Site 1
# pre 2016
gf01a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Goatfell/Goatfell 01/GOATFELL01 0001 20141121.CSV")
str(gf01a)
gf01a <- separate(gf01a,1,c("date","time","level_raw"),sep=";",remove=T)
gf01a$date <- as.Date(as.character(gf01a$date),format="%d.%m.%Y") ## change dates to date format
gf01a$level_raw <- as.numeric(as.character(gf01a$level_raw)) ## format water level as numeric
gf01a$datetime <- paste(gf01a$date,gf01a$time)
gf01a$datetime <- as.POSIXct(gf01a$datetime, format="%Y-%m-%d %H:%M")
gf01a$site <- c(rep("Goatfell", length(gf01a[,1])))
gf01a$logger <- c(rep("GOATFELL01", length(gf01a[,1])))
gf01a$period <- c(rep("pre2016", length(gf01a[,1])))
head(gf01a)

gf01a <- check(gf01a) #check data
meandf_a <- hydrocorr(logid=gf01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
gf01b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Goatfell/Data/Groundwater/GOATFELL01 20180306130545.csv")

str(gf01b)
gf01b <- separate(gf01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gf01b <- gf01b[,c(1:3)]
gf01b$date <- as.Date(as.character(gf01b$date),format="%d.%m.%Y") ## change dates to date format
gf01b$level_raw <- as.numeric(as.character(gf01b$level_raw)) ## format water level as numeric
gf01b$datetime <- paste(gf01b$date,gf01b$time)
gf01b$datetime <- as.POSIXct(gf01b$datetime, format="%Y-%m-%d %H:%M")
gf01b$site <- c(rep("Goatfell", length(gf01b[,1])))
gf01b$logger <- c(rep("GOATFELL01", length(gf01b[,1])))
gf01b$period <- c(rep("post2016", length(gf01b[,1])))
head(gf01b)

gf01b <- check(gf01b) #check data
meandf_b <- hydrocorr(logid=gf01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


### Site 2
# pre 2016
gf02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Goatfell/Goatfell 02/GOATFELL02 0001 20141121.CSV")
str(gf02a)
gf02a <- separate(gf02a,1,c("date","time","level_raw"),sep=";",remove=T)
gf02a$date <- as.Date(as.character(gf02a$date),format="%d.%m.%Y") ## change dates to date format
gf02a$level_raw <- as.numeric(as.character(gf02a$level_raw)) ## format water level as numeric
gf02a$datetime <- paste(gf02a$date,gf02a$time)
gf02a$datetime <- as.POSIXct(gf02a$datetime, format="%Y-%m-%d %H:%M")
gf02a$site <- c(rep("Goatfell", length(gf02a[,1])))
gf02a$logger <- c(rep("GOATFELL02", length(gf02a[,1])))
gf02a$period <- c(rep("pre2016", length(gf02a[,1])))
head(gf02a)

gf02a <- check(gf02a) #check data
meandf_a <- hydrocorr(logid=gf02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
gf02b <- read.csv("C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Goatfell/Data/Groundwater/GOATFELL02 20180306120750.csv")

str(gf02b)
gf02b <- separate(gf02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gf02b <- gf02b[,c(1:3)]
gf02b$date <- as.Date(as.character(gf02b$date),format="%d.%m.%Y") ## change dates to date format
gf02b$level_raw <- as.numeric(as.character(gf02b$level_raw)) ## format water level as numeric
gf02b$datetime <- paste(gf02b$date,gf02b$time)
gf02b$datetime <- as.POSIXct(gf02b$datetime, format="%Y-%m-%d %H:%M")
gf02b$site <- c(rep("Goatfell", length(gf02b[,1])))
gf02b$logger <- c(rep("GOATFELL02", length(gf02b[,1])))
gf02b$period <- c(rep("post2016", length(gf02b[,1])))
head(gf02b)

gf02b <- check(gf02b) #check data
meandf_b <- hydrocorr(logid=gf02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


## ///////////////////////////////
## Red Moss of Netherley -----
### Site 1 - logger submerged so results wrong
# pre 2016
rn01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Red Moss of Netherly/Site 1/REDMOSSN01 0001 20150303.CSV")
str(rn01a)
names(rn01a) <- c("date","time","level_raw")
rn01a$date <- as.Date(as.character(rn01a$date),format="%d.%m.%Y") ## change dates to date format
rn01a$level_raw <- as.numeric(as.character(rn01a$level_raw)) ## format water level as numeric
rn01a$datetime <- paste(rn01a$date,rn01a$time)
rn01a$datetime <- as.POSIXct(rn01a$datetime, format="%Y-%m-%d %H:%M")
rn01a$site <- c(rep("Red Moss of Netherley", length(rn01a[,1])))
rn01a$logger <- c(rep("REDMOSSN01", length(rn01a[,1])))
rn01a$period <- c(rep("pre2016", length(rn01a[,1])))
head(rn01a)

rn01a <- check(rn01a) #check data
meandf_a <- hydrocorr(logid=rn01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
rn01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss of Netherly/Data/Groundwater/REDMOSSN01 20180115133728.CSV")

str(rn01b)
rn01b <- separate(rn01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
rn01b <- rn01b[,c(1:3)]
rn01b$date <- as.Date(as.character(rn01b$date),format="%d.%m.%Y") ## change dates to date format
rn01b$level_raw <- as.numeric(as.character(rn01b$level_raw)) ## format water level as numeric
rn01b$datetime <- paste(rn01b$date,rn01b$time)
rn01b$datetime <- as.POSIXct(rn01b$datetime, format="%Y-%m-%d %H:%M")
rn01b$site <- c(rep("Red Moss of Netherley", length(rn01b[,1])))
rn01b$logger <- c(rep("REDMOSSN01", length(rn01b[,1])))
rn01b$period <- c(rep("post2016", length(rn01b[,1])))
head(rn01b)

rn01b <- check(rn01b) #check data
meandf_b <- hydrocorr(logid=rn01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 2 
# pre 2016
rn02a <- read.csv("C:/Data/Hydrology/Hydrology_data_pre2016/Red Moss of Netherly/Site 2/REDMOSSN02 0001 20150303.CSV")
str(rn02a)
names(rn02a) <- c("date","time","level_raw")
rn02a$date <- as.Date(as.character(rn02a$date),format="%d.%m.%Y") ## change dates to date format
rn02a$level_raw <- as.numeric(as.character(rn02a$level_raw)) ## format water level as numeric
rn02a$datetime <- paste(rn02a$date,rn02a$time)
rn02a$datetime <- as.POSIXct(rn02a$datetime, format="%Y-%m-%d %H:%M")
rn02a$site <- c(rep("Red Moss of Netherley", length(rn02a[,1])))
rn02a$logger <- c(rep("REDMOSSN02", length(rn02a[,1])))
rn02a$period <- c(rep("pre2016", length(rn02a[,1])))
head(rn02a)

rn02a <- check(rn02a) #check data
meandf_a <- hydrocorr(logid=rn02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
rn02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss of Netherly/Data/Groundwater/REDMOSSN02 20180115144633.CSV")

str(rn02b)
rn02b <- separate(rn02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
rn02b <- rn02b[,c(1:3)]
rn02b$date <- as.Date(as.character(rn02b$date),format="%d.%m.%Y") ## change dates to date format
rn02b$level_raw <- as.numeric(as.character(rn02b$level_raw)) ## format water level as numeric
rn02b$datetime <- paste(rn02b$date,rn02b$time)
rn02b$datetime <- as.POSIXct(rn02b$datetime, format="%Y-%m-%d %H:%M")
rn02b$site <- c(rep("Red Moss of Netherley", length(rn02b[,1])))
rn02b$logger <- c(rep("REDMOSSN02", length(rn02b[,1])))
rn02b$period <- c(rep("post2016", length(rn02b[,1])))
head(rn02b)

rn02b <- check(rn02b) #check data
meandf_b <- hydrocorr(logid=rn02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


## ///////////////////////////////
## Carrifran -----
### Site 1 
# pre 2016
cf01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Carrifran/Carrifran 1/CARRIFRAN1 0001 20150520.CSV")
str(cf01a)
cf01a <- separate(cf01a,1,c("date","time","level_raw"),sep=";",remove=T)
cf01a$date <- as.Date(as.character(cf01a$date),format="%d.%m.%Y") ## change dates to date format
cf01a$level_raw <- as.numeric(as.character(cf01a$level_raw)) ## format water level as numeric
cf01a$datetime <- paste(cf01a$date,cf01a$time)
cf01a$datetime <- as.POSIXct(cf01a$datetime, format="%Y-%m-%d %H:%M")
cf01a$site <- c(rep("Carrifran", length(cf01a[,1])))
cf01a$logger <- c(rep("CARRIFRAN1", length(cf01a[,1])))
cf01a$period <- c(rep("pre2016", length(cf01a[,1])))
head(cf01a)

cf01a <- check(cf01a) #check data
meandf_a <- hydrocorr(logid=cf01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016 - excel file missing

# /////////////
## Site 2 
# pre 2016
cf02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Carrifran/Carrifran 2/CARRIFRAN2 0001 20150520.CSV")
str(cf02a)
cf02a <- separate(cf02a,1,c("date","time","level_raw"),sep=";",remove=T)
cf02a$date <- as.Date(as.character(cf02a$date),format="%d.%m.%Y") ## change dates to date format
cf02a$level_raw <- as.numeric(as.character(cf02a$level_raw)) ## format water level as numeric
cf02a$datetime <- paste(cf02a$date,cf02a$time)
cf02a$datetime <- as.POSIXct(cf02a$datetime, format="%Y-%m-%d %H:%M")
cf02a$site <- c(rep("Carrifran", length(cf02a[,1])))
cf02a$logger <- c(rep("CARRIFRAN2", length(cf02a[,1])))
cf02a$period <- c(rep("pre2016", length(cf02a[,1])))
head(cf02a)

cf02a <- check(cf02a) #check data
meandf_a <- hydrocorr(logid=cf02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016 - excel file missing
cf02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Carrifran/Data/Groundwater/CARRIFRAN2 20180816225859.CSV")
head(cf02b)
str(cf02b)
cf02b <- separate(cf02b,1,c("date","time","level_raw"),sep=";",remove=T)
cf02b$date <- as.Date(as.character(cf02b$date),format="%d.%m.%Y") ## change dates to date format
cf02b$level_raw <- as.numeric(as.character(cf02b$level_raw)) ## format water level as numeric
cf02b$datetime <- paste(cf02b$date,cf02b$time)
cf02b$datetime <- as.POSIXct(cf02b$datetime, format="%Y-%m-%d %H:%M")
cf02b$site <- c(rep("Carrifran", length(cf02b[,1])))
cf02b$logger <- c(rep("CARRIFRAN2", length(cf02b[,1])))
cf02b$period <- c(rep("post2016", length(cf02b[,1])))
head(cf02b)

cf02b <- check(cf02b) #check data
meandf_b <- hydrocorr(logid=cf02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


## ///////////////////////////////
## Coalburn Moss -----
### # pre 2016 - all excel files missing #!

# site 1 post2016
cb01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Coalburn Moss/Data/Groundwater/COALBURN01 20180201144012.CSV")

str(cb01b)
cb01b <- separate(cb01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
cb01b <- cb01b[,c(1:3)]
cb01b$date <- as.Date(as.character(cb01b$date),format="%d.%m.%Y") ## change dates to date format
cb01b$level_raw <- as.numeric(as.character(cb01b$level_raw)) ## format water level as numeric
cb01b$datetime <- paste(cb01b$date,cb01b$time)
cb01b$datetime <- as.POSIXct(cb01b$datetime, format="%Y-%m-%d %H:%M")
cb01b$site <- c(rep("Coalburn Moss", length(cb01b[,1])))
cb01b$logger <- c(rep("COALBURN01", length(cb01b[,1])))
cb01b$period <- c(rep("post2016", length(cb01b[,1])))
head(cb01b)

cb01b <- check(cb01b) #check data
meandf_b <- hydrocorr(logid=cb01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

# site 2 post2016
cb02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Coalburn Moss/Data/Groundwater/COALBURN02 20180201152524.CSV")

str(cb02b)
cb02b <- separate(cb02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
cb02b <- cb02b[,c(1:3)]
cb02b$date <- as.Date(as.character(cb02b$date),format="%d.%m.%Y") ## change dates to date format
cb02b$level_raw <- as.numeric(as.character(cb02b$level_raw)) ## format water level as numeric
cb02b$datetime <- paste(cb02b$date,cb02b$time)
cb02b$datetime <- as.POSIXct(cb02b$datetime, format="%Y-%m-%d %H:%M")
cb02b$site <- c(rep("Coalburn Moss", length(cb02b[,1])))
cb02b$logger <- c(rep("COALBURN02", length(cb02b[,1])))
cb02b$period <- c(rep("post2016", length(cb02b[,1])))
head(cb02b)

cb02b <- check(cb02b) #check data
meandf_b <- hydrocorr(logid=cb02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

# site 3 post 2016
cb03b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Coalburn Moss/Data/Groundwater/COALBURN03 20180201135438.CSV")

str(cb03b)
cb03b <- separate(cb03b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
cb03b <- cb03b[,c(1:3)]
cb03b$date <- as.Date(as.character(cb03b$date),format="%d.%m.%Y") ## change dates to date format
cb03b$level_raw <- as.numeric(as.character(cb03b$level_raw)) ## format water level as numeric
cb03b$datetime <- paste(cb03b$date,cb03b$time)
cb03b$datetime <- as.POSIXct(cb03b$datetime, format="%Y-%m-%d %H:%M")
cb03b$site <- c(rep("Coalburn Moss", length(cb03b[,1])))
cb03b$logger <- c(rep("COALBURN03", length(cb03b[,1])))
cb03b$period <- c(rep("post2016", length(cb03b[,1])))
head(cb03b)

cb03b <- check(cb03b) #check data
meandf_b <- hydrocorr(logid=cb03b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

# site 4 post2016
cb04b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Coalburn Moss/Data/Groundwater/COALBURN04 20180201130903.CSV")

str(cb04b)
cb04b <- separate(cb04b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
cb04b <- cb04b[,c(1:3)]
cb04b$date <- as.Date(as.character(cb04b$date),format="%d.%m.%Y") ## change dates to date format
cb04b$level_raw <- as.numeric(as.character(cb04b$level_raw)) ## format water level as numeric
cb04b$datetime <- paste(cb04b$date,cb04b$time)
cb04b$datetime <- as.POSIXct(cb04b$datetime, format="%Y-%m-%d %H:%M")
cb04b$site <- c(rep("Coalburn Moss", length(cb04b[,1])))
cb04b$logger <- c(rep("COALBURN04", length(cb04b[,1])))
cb04b$period <- c(rep("post2016", length(cb04b[,1])))
head(cb04b)

cb04b <- check(cb04b) #check data
meandf_b <- hydrocorr(logid=cb04b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

## ///////////////////////////////
## Commonhead moss  -----
### Site 1 
# pre 2016
ch01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Commonhead Moss/Site 1/COMMHEAD01 0001 20150327.CSV")
str(ch01a)
ch01a <- separate(ch01a,1,c("date","time","level_raw"),sep=";",remove=T)
ch01a$date <- as.Date(as.character(ch01a$date),format="%d.%m.%Y") ## change dates to date format
ch01a$level_raw <- as.numeric(as.character(ch01a$level_raw)) ## format water level as numeric
ch01a$datetime <- paste(ch01a$date,ch01a$time)
ch01a$datetime <- as.POSIXct(ch01a$datetime, format="%Y-%m-%d %H:%M")
ch01a$site <- c(rep("Commonhead Moss", length(ch01a[,1])))
ch01a$logger <- c(rep("COMMHEAD01", length(ch01a[,1])))
ch01a$period <- c(rep("pre2016", length(ch01a[,1])))
head(ch01a)

ch01a <- check(ch01a) #check data
meandf_a <- hydrocorr(logid=ch01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post 2016
ch01b_1 <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Commonhead Moss/Data/COMMHEAD01 20180314133224.CSV")
str(ch01b_1)
ch01b_1 <- separate(ch01b_1,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
ch01b_1 <- ch01b_1[,c(1:3)]
ch01b_1$date <- as.Date(as.character(ch01b_1$date),format="%d.%m.%Y") ## change dates to date format
ch01b_1$level_raw <- as.numeric(as.character(ch01b_1$level_raw)) ## format water level as numeric
ch01b_1$datetime <- paste(ch01b_1$date,ch01b_1$time)
ch01b_1$datetime <- as.POSIXct(ch01b_1$datetime, format="%Y-%m-%d %H:%M")
ch01b_1$site <- c(rep("Commonhead Moss", length(ch01b_1[,1])))
ch01b_1$logger <- c(rep("COMMHEAD01", length(ch01b_1[,1])))
ch01b_1$period <- c(rep("post2016_1", length(ch01b_1[,1])))
head(ch01b_1)
ch01b_1 <- check(ch01b_1) #check data
meandf_b1 <- hydrocorr(logid=ch01b_1,drifttype = "gradual") # calculate corrected daily means

ch01b_2 <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Commonhead Moss/Data/COMMHEAD01 20180413105108.CSV")
str(ch01b_2)
ch01b_2 <- separate(ch01b_2,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
ch01b_2 <- ch01b_2[,c(1:3)]
ch01b_2$date <- as.Date(as.character(ch01b_2$date),format="%d.%m.%Y") ## change dates to date format
ch01b_2$level_raw <- as.numeric(as.character(ch01b_2$level_raw)) ## format water level as numeric
ch01b_2$datetime <- paste(ch01b_2$date,ch01b_2$time)
ch01b_2$datetime <- as.POSIXct(ch01b_2$datetime, format="%Y-%m-%d %H:%M")
ch01b_2$site <- c(rep("Commonhead Moss", length(ch01b_2[,1])))
ch01b_2$logger <- c(rep("COMMHEAD01", length(ch01b_2[,1])))
ch01b_2$period <- c(rep("post2016_2", length(ch01b_2[,1])))
head(ch01b_2)
ch01b_2 <- check(ch01b_2) #check data
meandf_b2 <- hydrocorr(logid=ch01b_2,drifttype = "gradual") # calculate corrected daily means

meandf_b <- rbind(meandf_b1,meandf_b2)
meandf_b$period <- rep("post2016",length(meandf_b$period))
head(meandf_b)
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine
#!# post 2016 plot doesn't look very good.

### Site 2 ////////////
# pre 2016
ch02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Commonhead Moss/Site 2/COMMHEAD02 0001 20150327.CSV")
str(ch02a)
ch02a <- separate(ch02a,1,c("date","time","level_raw"),sep=";",remove=T)
ch02a$date <- as.Date(as.character(ch02a$date),format="%d.%m.%Y") ## change dates to date format
ch02a$level_raw <- as.numeric(as.character(ch02a$level_raw)) ## format water level as numeric
ch02a$datetime <- paste(ch02a$date,ch02a$time)
ch02a$datetime <- as.POSIXct(ch02a$datetime, format="%Y-%m-%d %H:%M")
ch02a$site <- c(rep("Commonhead Moss", length(ch02a[,1])))
ch02a$logger <- c(rep("COMMHEAD02", length(ch02a[,1])))
ch02a$period <- c(rep("pre2016", length(ch02a[,1])))
head(ch02a)

ch02a <- check(ch02a) #check data
meandf_a <- hydrocorr(logid=ch02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post 2016
ch02b_1 <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Commonhead Moss/Data/COMMHEAD02 20180314125329.CSV")
str(ch02b_1)
ch02b_1 <- separate(ch02b_1,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
ch02b_1 <- ch02b_1[,c(1:3)]
ch02b_1$date <- as.Date(as.character(ch02b_1$date),format="%d.%m.%Y") ## change dates to date format
ch02b_1$level_raw <- as.numeric(as.character(ch02b_1$level_raw)) ## format water level as numeric
ch02b_1$datetime <- paste(ch02b_1$date,ch02b_1$time)
ch02b_1$datetime <- as.POSIXct(ch02b_1$datetime, format="%Y-%m-%d %H:%M")
ch02b_1$site <- c(rep("Commonhead Moss", length(ch02b_1[,1])))
ch02b_1$logger <- c(rep("COMMHEAD02", length(ch02b_1[,1])))
ch02b_1$period <- c(rep("post2016_1", length(ch02b_1[,1])))
head(ch02b_1)
ch02b_1 <- check(ch02b_1) #check data
meandf_b1 <- hydrocorr(logid=ch02b_1,drifttype = "gradual") # calculate corrected daily means

ch02b_2 <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Commonhead Moss/Data/COMMHEAD02 20180413101434.CSV")
str(ch02b_2)
ch02b_2 <- separate(ch02b_2,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
ch02b_2 <- ch02b_2[,c(1:3)]
ch02b_2$date <- as.Date(as.character(ch02b_2$date),format="%d.%m.%Y") ## change dates to date format
ch02b_2$level_raw <- as.numeric(as.character(ch02b_2$level_raw)) ## format water level as numeric
ch02b_2$datetime <- paste(ch02b_2$date,ch02b_2$time)
ch02b_2$datetime <- as.POSIXct(ch02b_2$datetime, format="%Y-%m-%d %H:%M")
ch02b_2$site <- c(rep("Commonhead Moss", length(ch02b_2[,1])))
ch02b_2$logger <- c(rep("COMMHEAD02", length(ch02b_2[,1])))
ch02b_2$period <- c(rep("post2016_2", length(ch02b_2[,1])))
head(ch02b_2)
ch02b_2 <- check(ch02b_2) #check data
meandf_b2 <- hydrocorr(logid=ch02b_2,drifttype = "gradual") # calculate corrected daily means

meandf_b <- rbind(meandf_b1,meandf_b2)
meandf_b$period <- rep("post2016",length(meandf_b$period))
head(meandf_b)
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine
##!# post 2016 definitely not very good! 

###///////
# Corrimony ----
### Site 1 
# pre 2016
cr01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Corrimony/Site 1/CORRIMONY1 0001 20151026.CSV")
str(cr01a) ## empty data #!#

# post2016
cr01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Corrimony/Data/Groundwater/CORRIMONY1 20180322153829.CSV")
str(cr01b)
cr01b <- separate(cr01b,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
cr01b <- cr01b[,c(1:3)]
cr01b$date <- as.Date(as.character(cr01b$date),format="%d.%m.%Y") ## change dates to date format
cr01b$level_raw <- as.numeric(as.character(cr01b$level_raw)) ## format water level as numeric
cr01b$datetime <- paste(cr01b$date,cr01b$time)
cr01b$datetime <- as.POSIXct(cr01b$datetime, format="%Y-%m-%d %H:%M")
cr01b$site <- c(rep("Corrimony", length(cr01b[,1])))
cr01b$logger <- c(rep("CORRIMONY1", length(cr01b[,1])))
cr01b$period <- c(rep("post2016", length(cr01b[,1])))
head(cr01b)

cr01b <- check(cr01b) #check data
meandf_b <- hydrocorr(logid=cr01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file


### Site 2 ///////// 
# pre 2016
cr02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Corrimony/Site 2/CORRIMONY2 0001 20150312.CSV")
str(cr02a) 
names(cr02a) <- c("date","time","level_raw")
cr02a$date <- as.Date(as.character(cr02a$date),format="%d.%m.%Y") ## change dates to date format
cr02a$level_raw <- as.numeric(as.character(cr02a$level_raw)) ## format water level as numeric
cr02a$datetime <- paste(cr02a$date,cr02a$time)
cr02a$datetime <- as.POSIXct(cr02a$datetime, format="%Y-%m-%d %H:%M")
cr02a$site <- c(rep("Corrimony", length(cr02a[,1])))
cr02a$logger <- c(rep("CORRIMONY2", length(cr02a[,1])))
cr02a$period <- c(rep("pre2016", length(cr02a[,1])))
head(cr02a)

cr02a <- check(cr02a) #check data
meandf_a <- hydrocorr(logid=cr02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
cr02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Corrimony/Data/Groundwater/CORRIMONY2 20180322162514.CSV")
str(cr02b)
cr02b <- separate(cr02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
cr02b <- cr02b[,c(1:3)]
cr02b$date <- as.Date(as.character(cr02b$date),format="%d.%m.%Y") ## change dates to date format
cr02b$level_raw <- as.numeric(as.character(cr02b$level_raw)) ## format water level as numeric
cr02b$datetime <- paste(cr02b$date,cr02b$time)
cr02b$datetime <- as.POSIXct(cr02b$datetime, format="%Y-%m-%d %H:%M")
cr02b$site <- c(rep("Corrimony", length(cr02b[,1])))
cr02b$logger <- c(rep("CORRIMONY2", length(cr02b[,1])))
cr02b$period <- c(rep("post2016", length(cr02b[,1])))
head(cr02b)

cr02b <- check(cr02b) #check data
meandf_b <- hydrocorr(logid=cr02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


###///////
# Corrour Forest ----
### Site 1 
# pre 2016
cf01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Corrour Forest/Site 1/CORROURF01 0001 20150310.CSV")
str(cf01a) 
names(cf01a) <- c("date","time","level_raw")
cf01a$date <- as.Date(as.character(cf01a$date),format="%d.%m.%Y") ## change dates to date format
cf01a$level_raw <- as.numeric(as.character(cf01a$level_raw)) ## format water level as numeric
cf01a$datetime <- paste(cf01a$date,cf01a$time)
cf01a$datetime <- as.POSIXct(cf01a$datetime, format="%Y-%m-%d %H:%M")
cf01a$site <- c(rep("Corrour Forest", length(cf01a[,1])))
cf01a$logger <- c(rep("CORROURF01", length(cf01a[,1])))
cf01a$period <- c(rep("pre2016", length(cf01a[,1])))
head(cf01a)

cf01a <- check(cf01a) #check data
meandf_a <- hydrocorr(logid=cf01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
cf01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Corrimony/Data/Groundwater/CORRIMONY1 20180322153829.CSV")
str(cf01b)
cf01b <- separate(cf01b,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
cf01b <- cf01b[,c(1:3)]
cf01b$date <- as.Date(as.character(cf01b$date),format="%d.%m.%Y") ## change dates to date format
cf01b$level_raw <- as.numeric(as.character(cf01b$level_raw)) ## format water level as numeric
cf01b$datetime <- paste(cf01b$date,cf01b$time)
cf01b$datetime <- as.POSIXct(cf01b$datetime, format="%Y-%m-%d %H:%M")
cf01b$site <- c(rep("Corrour Forest", length(cf01b[,1])))
cf01b$logger <- c(rep("CORROURF01", length(cf01b[,1])))
cf01b$period <- c(rep("post2016", length(cf01b[,1])))
head(cf01b)

cf01b <- check(cf01b) #check data
meandf_b <- hydrocorr(logid=cf01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine
#!# weird jump in wwater level between pre and post 2016, but corrections have been checked


### Site 2 ///////// 
# pre 2016
cf02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Corrour Forest/Site 2/CORROURF02 0001 20150310.CSV")
str(cf02a) 
names(cf02a) <- c("date","time","level_raw")
cf02a$date <- as.Date(as.character(cf02a$date),format="%d.%m.%Y") ## change dates to date format
cf02a$level_raw <- as.numeric(as.character(cf02a$level_raw)) ## format water level as numeric
cf02a$datetime <- paste(cf02a$date,cf02a$time)
cf02a$datetime <- as.POSIXct(cf02a$datetime, format="%Y-%m-%d %H:%M")
cf02a$site <- c(rep("Corrour Forest", length(cf02a[,1])))
cf02a$logger <- c(rep("CORROURF02", length(cf02a[,1])))
cf02a$period <- c(rep("pre2016", length(cf02a[,1])))
head(cf02a)

cf02a <- check(cf02a) #check data
meandf_a <- hydrocorr(logid=cf02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
cf02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Corrour Forrest/Data/Groundwater/CORROURF02 20180321140348.CSV")
str(cf02b)
cf02b <- separate(cf02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
cf02b <- cf02b[,c(1:3)]
cf02b$date <- as.Date(as.character(cf02b$date),format="%d.%m.%Y") ## change dates to date format
cf02b$level_raw <- as.numeric(as.character(cf02b$level_raw)) ## format water level as numeric
cf02b$datetime <- paste(cf02b$date,cf02b$time)
cf02b$datetime <- as.POSIXct(cf02b$datetime, format="%Y-%m-%d %H:%M")
cf02b$site <- c(rep("Corrour Forest", length(cf02b[,1])))
cf02b$logger <- c(rep("CORROURF02", length(cf02b[,1])))
cf02b$period <- c(rep("post2016", length(cf02b[,1])))
head(cf02b)

cf02b <- check(cf02b) #check data
meandf_b <- hydrocorr(logid=cf02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine



###///////
# Dal na copaig ----
# monthly files need to be combined into one sheet some data saved under 'Dalchork'

###///////
# Edinglassie ----
### Site 1 
# pre 2016
ed01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Edinglassie/Edinglassie 2016/Site 1/EDINGLASS1 0001 20150317.CSV")
str(ed01a) 
names(ed01a) <- c("date","time","level_raw")
ed01a$date <- as.Date(as.character(ed01a$date),format="%d.%m.%Y") ## change dates to date format
ed01a$level_raw <- as.numeric(as.character(ed01a$level_raw)) ## format water level as numeric
ed01a$datetime <- paste(ed01a$date,ed01a$time)
ed01a$datetime <- as.POSIXct(ed01a$datetime, format="%Y-%m-%d %H:%M")
ed01a$site <- c(rep("Edinglassie", length(ed01a[,1])))
ed01a$logger <- c(rep("EDINGLASS1", length(ed01a[,1])))
ed01a$period <- c(rep("pre2016", length(ed01a[,1])))
head(ed01a)

ed01a <- check(ed01a) #check data
meandf_a <- hydrocorr(logid=ed01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
ed01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Edinglassie/Edinglassie 2018/Data/Groundwater/EDINGLASS1 20180326122421.CSV")
str(ed01b)
ed01b <- separate(ed01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
ed01b <- ed01b[,c(1:3)]
ed01b$date <- as.Date(as.character(ed01b$date),format="%d.%m.%Y") ## change dates to date format
ed01b$level_raw <- as.numeric(as.character(ed01b$level_raw)) ## format water level as numeric
ed01b$datetime <- paste(ed01b$date,ed01b$time)
ed01b$datetime <- as.POSIXct(ed01b$datetime, format="%Y-%m-%d %H:%M")
ed01b$site <- c(rep("Edinglassie", length(ed01b[,1])))
ed01b$logger <- c(rep("EDINGLASS1", length(ed01b[,1])))
ed01b$period <- c(rep("post2016", length(ed01b[,1])))
head(ed01b)

ed01b <- check(ed01b) #check data
meandf_b <- hydrocorr(logid=ed01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


### Site 2 ///////////////
# pre 2016
ed02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Edinglassie/Edinglassie 2016/Site 2/EDINGLASS2 0001 20150318.CSV")
str(ed02a) 
names(ed02a) <- c("date","time","level_raw")
ed02a$date <- as.Date(as.character(ed02a$date),format="%d.%m.%Y") ## change dates to date format
ed02a$level_raw <- as.numeric(as.character(ed02a$level_raw)) ## format water level as numeric
ed02a$datetime <- paste(ed02a$date,ed02a$time)
ed02a$datetime <- as.POSIXct(ed02a$datetime, format="%Y-%m-%d %H:%M")
ed02a$site <- c(rep("Edinglassie", length(ed02a[,1])))
ed02a$logger <- c(rep("EDINGLASS2", length(ed02a[,1])))
ed02a$period <- c(rep("pre2016", length(ed02a[,1])))
head(ed02a)

ed02a <- check(ed02a) #check data
meandf_a <- hydrocorr(logid=ed02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
ed02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Edinglassie/Edinglassie 2018/Data/Groundwater/EDINGLASS2 20180326115822.CSV")
str(ed02b)
ed02b <- separate(ed02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
ed02b <- ed02b[,c(1:3)]
ed02b$date <- as.Date(as.character(ed02b$date),format="%d.%m.%Y") ## change dates to date format
ed02b$level_raw <- as.numeric(as.character(ed02b$level_raw)) ## format water level as numeric
ed02b$datetime <- paste(ed02b$date,ed02b$time)
ed02b$datetime <- as.POSIXct(ed02b$datetime, format="%Y-%m-%d %H:%M")
ed02b$site <- c(rep("Edinglassie", length(ed02b[,1])))
ed02b$logger <- c(rep("EDINGLASS2", length(ed02b[,1])))
ed02b$period <- c(rep("post2016", length(ed02b[,1])))
head(ed02b)

ed02b <- check(ed02b) #check data
meandf_b <- hydrocorr(logid=ed02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 3 ////////////// 
# pre 2016
ed02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Edinglassie/Edinglassie 2016/Site 3/EDINGLASS3 0001 20150318.CSV",header=F)
str(ed02a) 
ed02a <- separate(ed02a,1,c("date","time","level_raw"),sep=";",remove=T)
ed02a <- ed02a[,c(1:3)]
ed02a$date <- as.Date(as.character(ed02a$date),format="%d.%m.%Y") ## change dates to date format
ed02a$level_raw <- as.numeric(as.character(ed02a$level_raw)) ## format water level as numeric
ed02a$datetime <- paste(ed02a$date,ed02a$time)
ed02a$datetime <- as.POSIXct(ed02a$datetime, format="%Y-%m-%d %H:%M")
ed02a$site <- c(rep("Edinglassie", length(ed02a[,1])))
ed02a$logger <- c(rep("EDINGLASS3", length(ed02a[,1])))
ed02a$period <- c(rep("pre2016", length(ed02a[,1])))
head(ed02a)

ed02a <- check(ed02a) #check data
meandf_a <- hydrocorr(logid=ed02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
ed02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Edinglassie/Edinglassie 2018/Data/Groundwater/EDINGLASS3 20180326111846.CSV")
str(ed02b)
ed02b <- separate(ed02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
ed02b <- ed02b[,c(1:3)]
ed02b$date <- as.Date(as.character(ed02b$date),format="%d.%m.%Y") ## change dates to date format
ed02b$level_raw <- as.numeric(as.character(ed02b$level_raw)) ## format water level as numeric
ed02b$datetime <- paste(ed02b$date,ed02b$time)
ed02b$datetime <- as.POSIXct(ed02b$datetime, format="%Y-%m-%d %H:%M")
ed02b$site <- c(rep("Edinglassie", length(ed02b[,1])))
ed02b$logger <- c(rep("EDINGLASS3", length(ed02b[,1])))
ed02b$period <- c(rep("post2016", length(ed02b[,1])))
head(ed02b)

ed02b <- check(ed02b) #check data
meandf_b <- hydrocorr(logid=ed02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


###///////
# Goatfell ----
### Site 1 
# pre 2016
gf01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Goatfell/Goatfell 01/GOATFELL01 0001 20141121.CSV",header=F)
str(gf01a) 
gf01a <- separate(gf01a,1,c("date","time","level_raw"),sep=";",remove=T)
gf01a$date <- as.Date(as.character(gf01a$date),format="%d.%m.%Y") ## change dates to date format
gf01a$level_raw <- as.numeric(as.character(gf01a$level_raw)) ## format water level as numeric
gf01a$datetime <- paste(gf01a$date,gf01a$time)
gf01a$datetime <- as.POSIXct(gf01a$datetime, format="%Y-%m-%d %H:%M")
gf01a$site <- c(rep("Goatfell", length(gf01a[,1])))
gf01a$logger <- c(rep("GOATFELL01", length(gf01a[,1])))
gf01a$period <- c(rep("pre2016", length(gf01a[,1])))
head(gf01a)

gf01a <- check(gf01a) #check data
meandf_a <- hydrocorr(logid=gf01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
gf01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Goatfell/Data/Groundwater/GOATFELL01 20180306130545.CSV")
str(gf01b)
gf01b <- separate(gf01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gf01b <- gf01b[,c(1:3)]
gf01b$date <- as.Date(as.character(gf01b$date),format="%d.%m.%Y") ## change dates to date format
gf01b$level_raw <- as.numeric(as.character(gf01b$level_raw)) ## format water level as numeric
gf01b$datetime <- paste(gf01b$date,gf01b$time)
gf01b$datetime <- as.POSIXct(gf01b$datetime, format="%Y-%m-%d %H:%M")
gf01b$site <- c(rep("Goatfell", length(gf01b[,1])))
gf01b$logger <- c(rep("GOATFELL01", length(gf01b[,1])))
gf01b$period <- c(rep("post2016", length(gf01b[,1])))
head(gf01b)

gf01b <- check(gf01b) #check data
meandf_b <- hydrocorr(logid=gf01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

#///////////
### Site 2 ///////////////
# pre 2016
gf02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Goatfell/Goatfell 02/GOATFELL02 0001 20141121.CSV",header=F)
str(gf02a) 
gf02a <- separate(gf02a,1,c("date","time","level_raw"),sep=";",remove=T)
gf02a$date <- as.Date(as.character(gf02a$date),format="%d.%m.%Y") ## change dates to date format
gf02a$level_raw <- as.numeric(as.character(gf02a$level_raw)) ## format water level as numeric
gf02a$datetime <- paste(gf02a$date,gf02a$time)
gf02a$datetime <- as.POSIXct(gf02a$datetime, format="%Y-%m-%d %H:%M")
gf02a$site <- c(rep("Goatfell", length(gf02a[,1])))
gf02a$logger <- c(rep("GOATFELL02", length(gf02a[,1])))
gf02a$period <- c(rep("pre2016", length(gf02a[,1])))
head(gf02a)

gf02a <- check(gf02a) #check data
meandf_a <- hydrocorr(logid=gf02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
gf02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Goatfell/Data/Groundwater/GOATFELL02 20180306120750.CSV")
str(gf02b)
gf02b <- separate(gf02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gf02b <- gf02b[,c(1:3)]
gf02b$date <- as.Date(as.character(gf02b$date),format="%d.%m.%Y") ## change dates to date format
gf02b$level_raw <- as.numeric(as.character(gf02b$level_raw)) ## format water level as numeric
gf02b$datetime <- paste(gf02b$date,gf02b$time)
gf02b$datetime <- as.POSIXct(gf02b$datetime, format="%Y-%m-%d %H:%M")
gf02b$site <- c(rep("Goatfell", length(gf02b[,1])))
gf02b$logger <- c(rep("GOATFELL02", length(gf02b[,1])))
gf02b$period <- c(rep("post2016", length(gf02b[,1])))
head(gf02b)

gf02b <- check(gf02b) #check data
meandf_b <- hydrocorr(logid=gf02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


###///////
# Greenhead Moss ----
### Site 1 
# pre 2016
gh01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Greenhead Moss/Site 1/0GREENHM01 0001 20141030.CSV",header=F)
str(gh01a) 
gh01a <- separate(gh01a,1,c("date","time","level_raw"),sep=";",remove=T)
gh01a$date <- as.Date(as.character(gh01a$date),format="%d.%m.%Y") ## change dates to date format
gh01a$level_raw <- as.numeric(as.character(gh01a$level_raw)) ## format water level as numeric
gh01a$datetime <- paste(gh01a$date,gh01a$time)
gh01a$datetime <- as.POSIXct(gh01a$datetime, format="%Y-%m-%d %H:%M")
gh01a$site <- c(rep("Greenhead Moss", length(gh01a[,1])))
gh01a$logger <- c(rep("0GREENHM01", length(gh01a[,1])))
gh01a$period <- c(rep("pre2016", length(gh01a[,1])))
tail(gh01a)

gh01a <- check(gh01a) #check data
meandf_a <- hydrocorr(logid=gh01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

# post2016
gh01b_1 <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Greenhead Moss/Data/Groundwater/0GREENHM01 20180215133132.CSV")
str(gh01b_1)
gh01b_1 <- separate(gh01b_1,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gh01b_1 <- gh01b_1[,c(1:3)]
gh01b_1$date <- as.Date(as.character(gh01b_1$date),format="%d.%m.%Y") ## change dates to date format
gh01b_1$level_raw <- as.numeric(as.character(gh01b_1$level_raw)) ## format water level as numeric
gh01b_1$datetime <- paste(gh01b_1$date,gh01b_1$time)
gh01b_1$datetime <- as.POSIXct(gh01b_1$datetime, format="%Y-%m-%d %H:%M")
gh01b_1$site <- c(rep("Greenhead Moss", length(gh01b_1[,1])))
gh01b_1$logger <- c(rep("0GREENHM01", length(gh01b_1[,1])))
gh01b_1$period <- c(rep("post2016", length(gh01b_1[,1])))
head(gh01b_1)

gh01b_1 <- check(gh01b_1) #check data
meandf_b1 <- hydrocorr(logid=gh01b_1,drifttype = "gradual") # calculate corrected daily means
## logger was underwater so this explains why water level data are 'above ground'


gh01b_2 <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Greenhead Moss/Data/Groundwater/0GREENHM01 20180330125617.CSV")
str(gh01b_2)
gh01b_2 <- separate(gh01b_2,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
gh01b_2 <- gh01b_2[,c(1:3)]
gh01b_2$date <- as.Date(as.character(gh01b_2$date),format="%d.%m.%Y") ## change dates to date format
gh01b_2$level_raw <- as.numeric(as.character(gh01b_2$level_raw)) ## format water level as numeric
gh01b_2$datetime <- paste(gh01b_2$date,gh01b_2$time)
gh01b_2$datetime <- as.POSIXct(gh01b_2$datetime, format="%Y-%m-%d %H:%M")
gh01b_2$site <- c(rep("Greenhead Moss", length(gh01b_2[,1])))
gh01b_2$logger <- c(rep("0GREENHM01", length(gh01b_2[,1])))
gh01b_2$period <- c(rep("post2016_2", length(gh01b_2[,1])))
head(gh01b_2)

gh01b_2 <- check(gh01b_2) #check data
meandf_b2 <- hydrocorr(logid=gh01b_2,drifttype = "gradual") # calculate corrected daily means

meandf_b <- rbind(meandf_b1,meandf_b2)
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


###///////
# Longbridgemuir ----
### Site 1 
# pre 2016
lb01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 1/LONGBRIDM1 0001 20141118.CSV",header=F)
str(lb01a) 
names(lb01a) <- c("date","time","level_raw")
lb01a$date <- as.Date(as.character(lb01a$date),format="%d.%m.%Y") ## change dates to date format
lb01a$level_raw <- as.numeric(as.character(lb01a$level_raw)) ## format water level as numeric
lb01a$datetime <- paste(lb01a$date,lb01a$time)
lb01a$datetime <- as.POSIXct(lb01a$datetime, format="%Y-%m-%d %H:%M")
lb01a$site <- c(rep("Longbridge Muir", length(lb01a[,1])))
lb01a$logger <- c(rep("LONGBRIDM1", length(lb01a[,1])))
lb01a$period <- c(rep("pre2016", length(lb01a[,1])))
head(lb01a)

lb01a <- check(lb01a) #check data
meandf_a <- hydrocorr(logid=lb01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM1 20180207140428.CSV")
str(lb01b)
lb01b <- separate(lb01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb01b <- lb01b[,c(1:3)]
lb01b$date <- as.Date(as.character(lb01b$date),format="%d.%m.%Y") ## change dates to date format
lb01b$level_raw <- as.numeric(as.character(lb01b$level_raw)) ## format water level as numeric
lb01b$datetime <- paste(lb01b$date,lb01b$time)
lb01b$datetime <- as.POSIXct(lb01b$datetime, format="%Y-%m-%d %H:%M")
lb01b$site <- c(rep("Longbridge Muir", length(lb01b[,1])))
lb01b$logger <- c(rep("LONGBRIDM1", length(lb01b[,1])))
lb01b$period <- c(rep("post2016", length(lb01b[,1])))
head(lb01b)

lb01b <- check(lb01b) #check data
meandf_b <- hydrocorr(logid=lb01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


### Site 2 
# pre 2016
lb02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 2/LONGBRIDM2 0001 20150113.CSV",header=F)
str(lb02a) 
names(lb02a) <- c("date","time","level_raw")
lb02a$date <- as.Date(as.character(lb02a$date),format="%d.%m.%Y") ## change dates to date format
lb02a$level_raw <- as.numeric(as.character(lb02a$level_raw)) ## format water level as numeric
lb02a$datetime <- paste(lb02a$date,lb02a$time)
lb02a$datetime <- as.POSIXct(lb02a$datetime, format="%Y-%m-%d %H:%M")
lb02a$site <- c(rep("Longbridge Muir", length(lb02a[,1])))
lb02a$logger <- c(rep("LONGBRIDM2", length(lb02a[,1])))
lb02a$period <- c(rep("pre2016", length(lb02a[,1])))
head(lb02a)

lb02a <- check(lb02a) #check data
meandf_a <- hydrocorr(logid=lb02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM2 20180207145410.CSV")
str(lb02b)
lb02b <- separate(lb02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb02b <- lb02b[,c(1:3)]
lb02b$date <- as.Date(as.character(lb02b$date),format="%d.%m.%Y") ## change dates to date format
lb02b$level_raw <- as.numeric(as.character(lb02b$level_raw)) ## format water level as numeric
lb02b$datetime <- paste(lb02b$date,lb02b$time)
lb02b$datetime <- as.POSIXct(lb02b$datetime, format="%Y-%m-%d %H:%M")
lb02b$site <- c(rep("Longbridge Muir", length(lb02b[,1])))
lb02b$logger <- c(rep("LONGBRIDM2", length(lb02b[,1])))
lb02b$period <- c(rep("post2016", length(lb02b[,1])))
head(lb02b)

lb02b <- check(lb02b) #check data
meandf_b <- hydrocorr(logid=lb02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 3 
# pre 2016
lb03a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 3/LONGBRIDM3 0001 20150113.CSV",header=F)
str(lb03a) 
names(lb03a) <- c("date","time","level_raw")
lb03a$date <- as.Date(as.character(lb03a$date),format="%d.%m.%Y") ## change dates to date format
lb03a$level_raw <- as.numeric(as.character(lb03a$level_raw)) ## format water level as numeric
lb03a$datetime <- paste(lb03a$date,lb03a$time)
lb03a$datetime <- as.POSIXct(lb03a$datetime, format="%Y-%m-%d %H:%M")
lb03a$site <- c(rep("Longbridge Muir", length(lb03a[,1])))
lb03a$logger <- c(rep("LONGBRIDM3", length(lb03a[,1])))
lb03a$period <- c(rep("pre2016", length(lb03a[,1])))
head(lb03a)

lb03a <- check(lb03a) #check data
meandf_a <- hydrocorr(logid=lb03a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb03b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM3 20180207153052.CSV")
str(lb03b)
lb03b <- separate(lb03b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb03b <- lb03b[,c(1:3)]
lb03b$date <- as.Date(as.character(lb03b$date),format="%d.%m.%Y") ## change dates to date format
lb03b$level_raw <- as.numeric(as.character(lb03b$level_raw)) ## format water level as numeric
lb03b$datetime <- paste(lb03b$date,lb03b$time)
lb03b$datetime <- as.POSIXct(lb03b$datetime, format="%Y-%m-%d %H:%M")
lb03b$site <- c(rep("Longbridge Muir", length(lb03b[,1])))
lb03b$logger <- c(rep("LONGBRIDM3", length(lb03b[,1])))
lb03b$period <- c(rep("post2016", length(lb03b[,1])))
head(lb03b)

lb03b <- check(lb03b) #check data
meandf_b <- hydrocorr(logid=lb03b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 4 
# pre 2016
lb04a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 4/LONGBRIDM4 0001 20141118.CSV",header=F)
str(lb04a) 
names(lb04a) <- c("date","time","level_raw")
lb04a$date <- as.Date(as.character(lb04a$date),format="%d.%m.%Y") ## change dates to date format
lb04a$level_raw <- as.numeric(as.character(lb04a$level_raw)) ## format water level as numeric
lb04a$datetime <- paste(lb04a$date,lb04a$time)
lb04a$datetime <- as.POSIXct(lb04a$datetime, format="%Y-%m-%d %H:%M")
lb04a$site <- c(rep("Longbridge Muir", length(lb04a[,1])))
lb04a$logger <- c(rep("LONGBRIDM4", length(lb04a[,1])))
lb04a$period <- c(rep("pre2016", length(lb04a[,1])))
head(lb04a)

lb04a <- check(lb04a) #check data
meandf_a <- hydrocorr(logid=lb04a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb04b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM4 20180208121516.CSV")
str(lb04b)
lb04b <- separate(lb04b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb04b <- lb04b[,c(1:3)]
lb04b$date <- as.Date(as.character(lb04b$date),format="%d.%m.%Y") ## change dates to date format
lb04b$level_raw <- as.numeric(as.character(lb04b$level_raw)) ## format water level as numeric
lb04b$datetime <- paste(lb04b$date,lb04b$time)
lb04b$datetime <- as.POSIXct(lb04b$datetime, format="%Y-%m-%d %H:%M")
lb04b$site <- c(rep("Longbridge Muir", length(lb04b[,1])))
lb04b$logger <- c(rep("LONGBRIDM4", length(lb04b[,1])))
lb04b$period <- c(rep("post2016", length(lb04b[,1])))
head(lb04b)

lb04b <- check(lb04b) #check data
meandf_b <- hydrocorr(logid=lb04b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 5 
# pre 2016
lb05a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 5/LONGBRIDM5 0001 20141118.CSV",header=F)
str(lb05a) 
names(lb05a) <- c("date","time","level_raw")
lb05a$date <- as.Date(as.character(lb05a$date),format="%d.%m.%Y") ## change dates to date format
lb05a$level_raw <- as.numeric(as.character(lb05a$level_raw)) ## format water level as numeric
lb05a$datetime <- paste(lb05a$date,lb05a$time)
lb05a$datetime <- as.POSIXct(lb05a$datetime, format="%Y-%m-%d %H:%M")
lb05a$site <- c(rep("Longbridge Muir", length(lb05a[,1])))
lb05a$logger <- c(rep("LONGBRIDM5", length(lb05a[,1])))
lb05a$period <- c(rep("pre2016", length(lb05a[,1])))
head(lb05a)

lb05a <- check(lb05a) #check data
meandf_a <- hydrocorr(logid=lb05a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb05b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM5 20180208152106.CSV")
str(lb05b)
lb05b <- separate(lb05b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb05b <- lb05b[,c(1:3)]
lb05b$date <- as.Date(as.character(lb05b$date),format="%d.%m.%Y") ## change dates to date format
lb05b$level_raw <- as.numeric(as.character(lb05b$level_raw)) ## format water level as numeric
lb05b$datetime <- paste(lb05b$date,lb05b$time)
lb05b$datetime <- as.POSIXct(lb05b$datetime, format="%Y-%m-%d %H:%M")
lb05b$site <- c(rep("Longbridge Muir", length(lb05b[,1])))
lb05b$logger <- c(rep("LONGBRIDM5", length(lb05b[,1])))
lb05b$period <- c(rep("post2016", length(lb05b[,1])))
head(lb05b)

lb05b <- check(lb05b) #check data
meandf_b <- hydrocorr(logid=lb05b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 6 
# pre 2016
lb06a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 6/LONGBRIDM6 0001 20141118.CSV",header=F)
str(lb06a) 
names(lb06a) <- c("date","time","level_raw")
lb06a$date <- as.Date(as.character(lb06a$date),format="%d.%m.%Y") ## change dates to date format
lb06a$level_raw <- as.numeric(as.character(lb06a$level_raw)) ## format water level as numeric
lb06a$datetime <- paste(lb06a$date,lb06a$time)
lb06a$datetime <- as.POSIXct(lb06a$datetime, format="%Y-%m-%d %H:%M")
lb06a$site <- c(rep("Longbridge Muir", length(lb06a[,1])))
lb06a$logger <- c(rep("LONGBRIDM6", length(lb06a[,1])))
lb06a$period <- c(rep("pre2016", length(lb06a[,1])))
head(lb06a)

lb06a <- check(lb06a) #check data
meandf_a <- hydrocorr(logid=lb06a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb06b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM6 20180208141305.CSV")
str(lb06b)
lb06b <- separate(lb06b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb06b <- lb06b[,c(1:3)]
lb06b$date <- as.Date(as.character(lb06b$date),format="%d.%m.%Y") ## change dates to date format
lb06b$level_raw <- as.numeric(as.character(lb06b$level_raw)) ## format water level as numeric
lb06b$datetime <- paste(lb06b$date,lb06b$time)
lb06b$datetime <- as.POSIXct(lb06b$datetime, format="%Y-%m-%d %H:%M")
lb06b$site <- c(rep("Longbridge Muir", length(lb06b[,1])))
lb06b$logger <- c(rep("LONGBRIDM6", length(lb06b[,1])))
lb06b$period <- c(rep("post2016", length(lb06b[,1])))
head(lb06b)

lb06b <- check(lb06b) #check data
meandf_b <- hydrocorr(logid=lb06b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 7 
# pre 2016
lb07a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 7/LONGBRIDM7 0001 20141118.CSV",header=F)
str(lb07a) 
names(lb07a) <- c("date","time","level_raw")
lb07a$date <- as.Date(as.character(lb07a$date),format="%d.%m.%Y") ## change dates to date format
lb07a$level_raw <- as.numeric(as.character(lb07a$level_raw)) ## format water level as numeric
lb07a$datetime <- paste(lb07a$date,lb07a$time)
lb07a$datetime <- as.POSIXct(lb07a$datetime, format="%Y-%m-%d %H:%M")
lb07a$site <- c(rep("Longbridge Muir", length(lb07a[,1])))
lb07a$logger <- c(rep("LONGBRIDM7", length(lb07a[,1])))
lb07a$period <- c(rep("pre2016", length(lb07a[,1])))
head(lb07a)

lb07a <- check(lb07a) #check data
meandf_a <- hydrocorr(logid=lb07a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb07b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM7 20180208131105.CSV")
str(lb07b)
lb07b <- separate(lb07b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb07b <- lb07b[,c(1:3)]
lb07b$date <- as.Date(as.character(lb07b$date),format="%d.%m.%Y") ## change dates to date format
lb07b$level_raw <- as.numeric(as.character(lb07b$level_raw)) ## format water level as numeric
lb07b$datetime <- paste(lb07b$date,lb07b$time)
lb07b$datetime <- as.POSIXct(lb07b$datetime, format="%Y-%m-%d %H:%M")
lb07b$site <- c(rep("Longbridge Muir", length(lb07b[,1])))
lb07b$logger <- c(rep("LONGBRIDM7", length(lb07b[,1])))
lb07b$period <- c(rep("post2016", length(lb07b[,1])))
head(lb07b)

lb07b <- check(lb07b) #check data
meandf_b <- hydrocorr(logid=lb07b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 8 
# pre 2016
lb08a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Longbridgemuir/Site 8/LONGBRIDM8 0001 20150113.CSV",header=F)
str(lb08a) 
names(lb08a) <- c("date","time","level_raw")
lb08a$date <- as.Date(as.character(lb08a$date),format="%d.%m.%Y") ## change dates to date format
lb08a$level_raw <- as.numeric(as.character(lb08a$level_raw)) ## format water level as numeric
lb08a$datetime <- paste(lb08a$date,lb08a$time)
lb08a$datetime <- as.POSIXct(lb08a$datetime, format="%Y-%m-%d %H:%M")
lb08a$site <- c(rep("Longbridge Muir", length(lb08a[,1])))
lb08a$logger <- c(rep("LONGBRIDM8", length(lb08a[,1])))
lb08a$period <- c(rep("pre2016", length(lb08a[,1])))
head(lb08a)

lb08a <- check(lb08a) #check data
meandf_a <- hydrocorr(logid=lb08a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
lb08b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Longbridgemuir/Data/Groundwater/LONGBRIDM8 20180208155831.CSV")
str(lb08b)
lb08b <- separate(lb08b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
lb08b <- lb08b[,c(1:3)]
lb08b$date <- as.Date(as.character(lb08b$date),format="%d.%m.%Y") ## change dates to date format
lb08b$level_raw <- as.numeric(as.character(lb08b$level_raw)) ## format water level as numeric
lb08b$datetime <- paste(lb08b$date,lb08b$time)
lb08b$datetime <- as.POSIXct(lb08b$datetime, format="%Y-%m-%d %H:%M")
lb08b$site <- c(rep("Longbridge Muir", length(lb08b[,1])))
lb08b$logger <- c(rep("LONGBRIDM8", length(lb08b[,1])))
lb08b$period <- c(rep("post2016", length(lb08b[,1])))
head(lb08b)

lb08b <- check(lb08b) #check data
meandf_b <- hydrocorr(logid=lb08b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine



###///////
# Low Moss ----
### Site 1 
# pre 2016
lm01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Low Moss/Data/000LOWMOSS 20180216125317.CSV",header=F)
str(lm01b)
lm01b <- separate(lm01b,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
lm01b <- lm01b[,c(1:3)]
lm01b$date <- as.Date(as.character(lm01b$date),format="%d.%m.%Y") ## change dates to date format
lm01b$level_raw <- as.numeric(as.character(lm01b$level_raw)) ## format water level as numeric
## in cm not m! - convert to m
lm01b$level_raw <- lm01b$level_raw/100
lm01b$datetime <- paste(lm01b$date,lm01b$time)
lm01b$datetime <- as.POSIXct(lm01b$datetime, format="%Y-%m-%d %H:%M")
lm01b$site <- c(rep("Low Moss", length(lm01b[,1])))
lm01b$logger <- c(rep("000LOWMOSS", length(lm01b[,1])))
lm01b$period <- c(rep("post2016", length(lm01b[,1])))
head(lm01b)

lm01b <- check(lm01b) #check data
meandf_b <- hydrocorr(logid=lm01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file


###///////
# Lenzie ----
### Site 1 
# pre 2016
lz01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Lenzie Moss/Data/Groundwater/0000LENZIE 20180215113858.CSV",header=F)
str(lz01b) 
lz01b <- separate(lz01b,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
lz01b <- lz01b[,c(1:3)]
lz01b$date <- as.Date(as.character(lz01b$date),format="%d.%m.%Y") ## change dates to date format
lz01b$level_raw <- as.numeric(as.character(lz01b$level_raw)) ## format water level as numeric
lz01b$level_raw <- lz01b$level_raw/100 # convert to m
lz01b$datetime <- paste(lz01b$date,lz01b$time)
lz01b$datetime <- as.POSIXct(lz01b$datetime, format="%Y-%m-%d %H:%M")
lz01b$site <- c(rep("Lenzie Moss", length(lz01b[,1])))
lz01b$logger <- c(rep("0000LENZIE", length(lz01b[,1])))
lz01b$period <- c(rep("post2016", length(lz01b[,1])))
head(lz01b)

lz01b <- check(lz01b) #check data
meandf_b <- hydrocorr(logid=lz01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

###///////
# Moine Mhor----
### Site 1 
# pre 2016

#! different data format (WALRAG) and corrections not found, needs to be processed separately



###///////
# Nairnside ----
### Site 1 
# pre 2016
ns01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Nairnside/Site 1/NAIRNSIDE1 0001 20150312.CSV",header=F)
str(ns01a) 
ns01a <- separate(ns01a,1,c("date","time","level_raw"),sep=";",remove=T)
ns01a$date <- as.Date(as.character(ns01a$date),format="%d.%m.%Y") ## change dates to date format
ns01a$level_raw <- as.numeric(as.character(ns01a$level_raw)) ## format water level as numeric
ns01a$datetime <- paste(ns01a$date,ns01a$time)
ns01a$datetime <- as.POSIXct(ns01a$datetime, format="%Y-%m-%d %H:%M")
ns01a$site <- c(rep("Nairnside", length(ns01a[,1])))
ns01a$logger <- c(rep("NAIRNSIDE1", length(ns01a[,1])))
ns01a$period <- c(rep("pre2016", length(ns01a[,1])))
head(ns01a)

ns01a <- check(ns01a) #check data
meandf_a <- hydrocorr(logid=ns01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
ns01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Nairnside/Data/Groundwater/NAIRNSIDE1 20180323142317.CSV")
str(ns01b)
ns01b <- separate(ns01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
ns01b <- ns01b[,c(1:3)]
ns01b$date <- as.Date(as.character(ns01b$date),format="%d.%m.%Y") ## change dates to date format
ns01b$level_raw <- as.numeric(as.character(ns01b$level_raw)) ## format water level as numeric
ns01b$datetime <- paste(ns01b$date,ns01b$time)
ns01b$datetime <- as.POSIXct(ns01b$datetime, format="%Y-%m-%d %H:%M")
ns01b$site <- c(rep("Nairnside", length(ns01b[,1])))
ns01b$logger <- c(rep("NAIRNSIDE1", length(ns01b[,1])))
ns01b$period <- c(rep("post2016", length(ns01b[,1])))
head(ns01b)

ns01b <- check(ns01b) #check data
meandf_b <- hydrocorr(logid=ns01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 2 
# pre 2016
## data missing #!#

# post2016
ns02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Nairnside/Data/Groundwater/NAIRNSIDE2 20180323135306.CSV")
str(ns02b)
ns02b <- separate(ns02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
ns02b <- ns02b[,c(1:3)]
ns02b$date <- as.Date(as.character(ns02b$date),format="%d.%m.%Y") ## change dates to date format
ns02b$level_raw <- as.numeric(as.character(ns02b$level_raw)) ## format water level as numeric
ns02b$datetime <- paste(ns02b$date,ns02b$time)
ns02b$datetime <- as.POSIXct(ns02b$datetime, format="%Y-%m-%d %H:%M")
ns02b$site <- c(rep("Nairnside", length(ns02b[,1])))
ns02b$logger <- c(rep("NAIRNSIDE2", length(ns02b[,1])))
ns02b$period <- c(rep("post2016", length(ns02b[,1])))
head(ns02b)

ns02b <- check(ns02b) #check data
meandf_b <- hydrocorr(logid=ns02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

###///////
# Portmoak ----
### Site 1 
# 2014-2018
pm01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Portmoak/Data/PORTMOAK01 0001 20141103.CSV",header=F)
str(pm01b)
pm01b <- separate(pm01b,1,c("date","time","level_raw"),sep=";",remove=T)
pm01b$date <- as.Date(as.character(pm01b$date),format="%d.%m.%Y") ## change dates to date format
pm01b$level_raw <- as.numeric(as.character(pm01b$level_raw)) ## format water level as numeric
pm01b$datetime <- paste(pm01b$date,pm01b$time)
pm01b$datetime <- as.POSIXct(pm01b$datetime, format="%Y-%m-%d %H:%M")
pm01b$site <- c(rep("Portmoak", length(pm01b[,1])))
pm01b$logger <- c(rep("PORTMOAK01", length(pm01b[,1])))
pm01b$period <- c(rep("2014to2018", length(pm01b[,1])))
head(pm01b)

pm01b <- check(pm01b) #check data
meandf_b <- hydrocorr(logid=pm01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

### Site 2 
# 2014-2018
pm02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Portmoak/Data/00PORTMOAK 0001 20131120.CSV",header=F)
str(pm02b)
pm02b <- separate(pm02b,1,c("date","time","level_raw"),sep=";",remove=T)
pm02b$date <- as.Date(as.character(pm02b$date),format="%d.%m.%Y") ## change dates to date format
pm02b$level_raw <- as.numeric(as.character(pm02b$level_raw)) ## format water level as numeric
pm02b$datetime <- paste(pm02b$date,pm02b$time)
pm02b$datetime <- as.POSIXct(pm02b$datetime, format="%Y-%m-%d %H:%M")
pm02b$site <- c(rep("Portmoak", length(pm02b[,1])))
pm02b$logger <- c(rep("00PORTMOAK", length(pm02b[,1])))
pm02b$period <- c(rep("2014to2018", length(pm02b[,1])))
head(pm02b)

pm02b <- check(pm02b) #check data
meandf_b <- hydrocorr(logid=pm02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

###///////
# Red Moss of Balerno ----
### Site 1 
# pre 2016
##! data in unreadable format
# post2016
rb01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss Of Balerno/Data/Groundwater/RMOBALER01 20180109105813.CSV",header=F)
str(rb01b) 
rb01b <- separate(rb01b,1,c("date","time","level_raw","Ch2","ch3"),sep=";",remove=T)
rb01b <- rb01b[,c(1:3)]
rb01b$date <- as.Date(as.character(rb01b$date),format="%d.%m.%Y") ## change dates to date format
rb01b$level_raw <- as.numeric(as.character(rb01b$level_raw)) ## format water level as numeric
rb01b$datetime <- paste(rb01b$date,rb01b$time)
rb01b$datetime <- as.POSIXct(rb01b$datetime, format="%Y-%m-%d %H:%M")
rb01b$site <- c(rep("Red Moss of Balerno", length(rb01b[,1])))
rb01b$logger <- c(rep("RMOBALER01", length(rb01b[,1])))
rb01b$period <- c(rep("post2016", length(rb01b[,1])))
head(rb01b)

rb01b <- check(rb01b) #check data
meandf_a <- hydrocorr(logid=rb01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

## site 2
# post2016
rb02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss Of Balerno/Data/Groundwater/RMOBALER02 20180109122146.CSV",header=F)
str(rb02b) 
rb02b <- separate(rb02b,1,c("date","time","level_raw","Ch2","ch3"),sep=";",remove=T)
rb02b <- rb02b[,c(1:3)]
rb02b$date <- as.Date(as.character(rb02b$date),format="%d.%m.%Y") ## change dates to date format
rb02b$level_raw <- as.numeric(as.character(rb02b$level_raw)) ## format water level as numeric
rb02b$datetime <- paste(rb02b$date,rb02b$time)
rb02b$datetime <- as.POSIXct(rb02b$datetime, format="%Y-%m-%d %H:%M")
rb02b$site <- c(rep("Red Moss of Balerno", length(rb02b[,1])))
rb02b$logger <- c(rep("RMOBALER02", length(rb02b[,1])))
rb02b$period <- c(rep("post2016", length(rb02b[,1])))
head(rb02b)

rb02b <- check(rb02b) #check data
meandf_a <- hydrocorr(logid=rb02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


## site 3
# post2016
rb03b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss Of Balerno/Data/Groundwater/RMOBALER03 0001 20170713.CSV",header=F)
str(rb03b) 
rb03b <- separate(rb03b,1,c("date","time","level_raw","Ch2","ch3"),sep=";",remove=T)
rb03b <- rb03b[,c(1:3)]
rb03b$date <- as.Date(as.character(rb03b$date),format="%d.%m.%Y") ## change dates to date format
rb03b$level_raw <- as.numeric(as.character(rb03b$level_raw)) ## format water level as numeric
rb03b$datetime <- paste(rb03b$date,rb03b$time)
rb03b$datetime <- as.POSIXct(rb03b$datetime, format="%Y-%m-%d %H:%M")
rb03b$site <- c(rep("Red Moss of Balerno", length(rb03b[,1])))
rb03b$logger <- c(rep("RMOBALER03", length(rb03b[,1])))
rb03b$period <- c(rep("post2016", length(rb03b[,1])))
head(rb03b)

rb03b <- check(rb03b) #check data
meandf_a <- hydrocorr(logid=rb03b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

## site 4
# post2016
rb04b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss Of Balerno/Data/Groundwater/RMOBALER04 20180109115146.CSV",header=F)
str(rb04b) 
rb04b <- separate(rb04b,1,c("date","time","level_raw","Ch2","ch3"),sep=";",remove=T)
rb04b <- rb04b[,c(1:3)]
rb04b$date <- as.Date(as.character(rb04b$date),format="%d.%m.%Y") ## change dates to date format
rb04b$level_raw <- as.numeric(as.character(rb04b$level_raw)) ## format water level as numeric
rb04b$datetime <- paste(rb04b$date,rb04b$time)
rb04b$datetime <- as.POSIXct(rb04b$datetime, format="%Y-%m-%d %H:%M")
rb04b$site <- c(rep("Red Moss of Balerno", length(rb04b[,1])))
rb04b$logger <- c(rep("RMOBALER04", length(rb04b[,1])))
rb04b$period <- c(rep("post2016", length(rb04b[,1])))
head(rb04b)

rb04b <- check(rb04b) #check data
meandf_a <- hydrocorr(logid=rb04b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

## site 5
# post2016
rb05b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss Of Balerno/Data/Groundwater/RMOBALER05 20180109113302.CSV",header=F)
str(rb05b) 
rb05b <- separate(rb05b,1,c("date","time","level_raw","Ch2","ch3"),sep=";",remove=T)
rb05b <- rb05b[,c(1:3)]
rb05b$date <- as.Date(as.character(rb05b$date),format="%d.%m.%Y") ## change dates to date format
rb05b$level_raw <- as.numeric(as.character(rb05b$level_raw)) ## format water level as numeric
rb05b$datetime <- paste(rb05b$date,rb05b$time)
rb05b$datetime <- as.POSIXct(rb05b$datetime, format="%Y-%m-%d %H:%M")
rb05b$site <- c(rep("Red Moss of Balerno", length(rb05b[,1])))
rb05b$logger <- c(rep("RMOBALER05", length(rb05b[,1])))
rb05b$period <- c(rep("post2016", length(rb05b[,1])))
head(rb05b)

rb05b <- check(rb05b) #check data
meandf_a <- hydrocorr(logid=rb05b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file

###///////
# Red Moss of Leys ----
### Site 1 
# pre 2016
rl01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Red Moss of Leys/Site 1/REDMLEYS01 0001 20150304.CSV",header=F)
str(rl01a) 
names(rl01a) <- c("date","time","level_raw")
rl01a$date <- as.Date(as.character(rl01a$date),format="%d.%m.%Y") ## change dates to date format
rl01a$level_raw <- as.numeric(as.character(rl01a$level_raw)) ## format water level as numeric
rl01a$datetime <- paste(rl01a$date,rl01a$time)
rl01a$datetime <- as.POSIXct(rl01a$datetime, format="%Y-%m-%d %H:%M")
rl01a$site <- c(rep("Red Moss of Leys", length(rl01a[,1])))
rl01a$logger <- c(rep("REDMLEYS01", length(rl01a[,1])))
rl01a$period <- c(rep("pre2016", length(rl01a[,1])))
head(rl01a)

rl01a <- check(rl01a) #check data
meandf_a <- hydrocorr(logid=rl01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
rl01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss of Leys/Data/Groundwater/REDMLEYS01 20180117135235.CSV")
str(rl01b)
rl01b <- separate(rl01b,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
rl01b <- rl01b[,c(1:3)]
rl01b$date <- as.Date(as.character(rl01b$date),format="%d.%m.%Y") ## change dates to date format
rl01b$level_raw <- as.numeric(as.character(rl01b$level_raw)) ## format water level as numeric
rl01b$datetime <- paste(rl01b$date,rl01b$time)
rl01b$datetime <- as.POSIXct(rl01b$datetime, format="%Y-%m-%d %H:%M")
rl01b$site <- c(rep("Red Moss of Leys", length(rl01b[,1])))
rl01b$logger <- c(rep("REDMLEYS01", length(rl01b[,1])))
rl01b$period <- c(rep("post2016", length(rl01b[,1])))
head(rl01b)

rl01b <- check(rl01b) #check data
meandf_b <- hydrocorr(logid=rl01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 2 
# pre 2016
rl02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Red Moss of Leys/Site 2/REDMLEYS02 0001 20150304.CSV",header=F)
str(rl02a) 
names(rl02a) <- c("date","time","level_raw")
rl02a$date <- as.Date(as.character(rl02a$date),format="%d.%m.%Y") ## change dates to date format
rl02a$level_raw <- as.numeric(as.character(rl02a$level_raw)) ## format water level as numeric
rl02a$datetime <- paste(rl02a$date,rl02a$time)
rl02a$datetime <- as.POSIXct(rl02a$datetime, format="%Y-%m-%d %H:%M")
rl02a$site <- c(rep("Red Moss of Leys", length(rl02a[,1])))
rl02a$logger <- c(rep("REDMLEYS02", length(rl02a[,1])))
rl02a$period <- c(rep("pre2016", length(rl02a[,1])))
head(rl02a)

rl02a <- check(rl02a) #check data
meandf_a <- hydrocorr(logid=rl02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016 - #! ther's only abotu a week of data for this bit
rl02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss of Leys/Data/Groundwater/REDMLEYS02 20180117145216.CSV")
str(rl02b)
rl02b <- separate(rl02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
rl02b <- rl02b[,c(1:3)]
rl02b$date <- as.Date(as.character(rl02b$date),format="%d.%m.%Y") ## change dates to date format
rl02b$level_raw <- as.numeric(as.character(rl02b$level_raw)) ## format water level as numeric
rl02b$datetime <- paste(rl02b$date,rl02b$time)
rl02b$datetime <- as.POSIXct(rl02b$datetime, format="%Y-%m-%d %H:%M")
rl02b$site <- c(rep("Red Moss of Leys", length(rl02b[,1])))
rl02b$logger <- c(rep("REDMLEYS02", length(rl02b[,1])))
rl02b$period <- c(rep("post2016", length(rl02b[,1])))
head(rl02b)

rl02b <- check(rl02b) #check data
meandf_b <- hydrocorr(logid=rl02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

###///////
# Red Moss of Netherley ----
### Site 1 
# pre 2016
rn01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Red Moss of Netherly/Site 1/REDMOSSN01 0001 20150303.CSV",header=F)
str(rn01a) 
names(rn01a) <- c("date","time","level_raw")
rn01a$date <- as.Date(as.character(rn01a$date),format="%d.%m.%Y") ## change dates to date format
rn01a$level_raw <- as.numeric(as.character(rn01a$level_raw)) ## format water level as numeric
rn01a$datetime <- paste(rn01a$date,rn01a$time)
rn01a$datetime <- as.POSIXct(rn01a$datetime, format="%Y-%m-%d %H:%M")
rn01a$site <- c(rep("Red Moss of Netherley", length(rn01a[,1])))
rn01a$logger <- c(rep("REDMOSSN01", length(rn01a[,1])))
rn01a$period <- c(rep("pre2016", length(rn01a[,1])))
head(rn01a)

rn01a <- check(rn01a) #check data
meandf_a <- hydrocorr(logid=rn01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
rn01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss of Netherly/Data/Groundwater/REDMOSSN01 20180115133728.CSV")
str(rn01b)
rn01b <- separate(rn01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
rn01b <- rn01b[,c(1:3)]
rn01b$date <- as.Date(as.character(rn01b$date),format="%d.%m.%Y") ## change dates to date format
rn01b$level_raw <- as.numeric(as.character(rn01b$level_raw)) ## format water level as numeric
rn01b$datetime <- paste(rn01b$date,rn01b$time)
rn01b$datetime <- as.POSIXct(rn01b$datetime, format="%Y-%m-%d %H:%M")
rn01b$site <- c(rep("Red Moss of Netherley", length(rn01b[,1])))
rn01b$logger <- c(rep("REDMOSSN01", length(rn01b[,1])))
rn01b$period <- c(rep("post2016", length(rn01b[,1])))
head(rn01b)

rn01b <- check(rn01b) #check data
meandf_b <- hydrocorr(logid=rn01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 2 
# pre 2016
rn02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Red Moss of Netherly/Site 2/REDMOSSN02 0001 20150303.CSV",header=F)
str(rn02a) 
names(rn02a) <- c("date","time","level_raw")
rn02a$date <- as.Date(as.character(rn02a$date),format="%d.%m.%Y") ## change dates to date format
rn02a$level_raw <- as.numeric(as.character(rn02a$level_raw)) ## format water level as numeric
rn02a$datetime <- paste(rn02a$date,rn02a$time)
rn02a$datetime <- as.POSIXct(rn02a$datetime, format="%Y-%m-%d %H:%M")
rn02a$site <- c(rep("Red Moss of Netherley", length(rn02a[,1])))
rn02a$logger <- c(rep("REDMOSSN02", length(rn02a[,1])))
rn02a$period <- c(rep("pre2016", length(rn02a[,1])))
head(rn02a)

rn02a <- check(rn02a) #check data
meandf_a <- hydrocorr(logid=rn02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016 - #! ther's only abotu a week of data for this bit
rn02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Red Moss of Netherly/Data/Groundwater/REDMOSSN02 20180115144633.CSV")
str(rn02b)
rn02b <- separate(rn02b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
rn02b <- rn02b[,c(1:3)]
rn02b$date <- as.Date(as.character(rn02b$date),format="%d.%m.%Y") ## change dates to date format
rn02b$level_raw <- as.numeric(as.character(rn02b$level_raw)) ## format water level as numeric
rn02b$datetime <- paste(rn02b$date,rn02b$time)
rn02b$datetime <- as.POSIXct(rn02b$datetime, format="%Y-%m-%d %H:%M")
rn02b$site <- c(rep("Red Moss of Netherley", length(rn02b[,1])))
rn02b$logger <- c(rep("REDMOSSN02", length(rn02b[,1])))
rn02b$period <- c(rep("post2016", length(rn02b[,1])))
head(rn02b)

rn02b <- check(rn02b) #check data
meandf_b <- hydrocorr(logid=rn02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine


###///////
# Salsburgh Moss ----
# #! no data in folder

###///////
# Slamannan Bog --------
### Site 1 
# pre 2016
sb01a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Slamannan Bog/Site 1/SLAMBOGM01 0001 20141031.CSV",header=F)
str(sb01a) 
names(sb01a) <- c("date","time","level_raw")
sb01a$date <- as.Date(as.character(sb01a$date),format="%d.%m.%Y") ## change dates to date format
sb01a$level_raw <- as.numeric(as.character(sb01a$level_raw)) ## format water level as numeric
sb01a$datetime <- paste(sb01a$date,sb01a$time)
sb01a$datetime <- as.POSIXct(sb01a$datetime, format="%Y-%m-%d %H:%M")
sb01a$site <- c(rep("Slamannan Bog", length(sb01a[,1])))
sb01a$logger <- c(rep("SLAMBOGM01", length(sb01a[,1])))
sb01a$period <- c(rep("pre2016", length(sb01a[,1])))
head(sb01a)

sb01a <- check(sb01a) #check data
meandf_a <- hydrocorr(logid=sb01a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
sb01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Slamannan/Data/SLAMBOGM01 0001 20160708.CSV",header=F)
str(sb01b)
sb01b <- separate(sb01b,1,c("date","time","level_raw"),sep=";",remove=T)
sb01b$date <- as.Date(as.character(sb01b$date),format="%d.%m.%Y") ## change dates to date format
sb01b$level_raw <- as.numeric(as.character(sb01b$level_raw)) ## format water level as numeric
sb01b$datetime <- paste(sb01b$date,sb01b$time)
sb01b$datetime <- as.POSIXct(sb01b$datetime, format="%Y-%m-%d %H:%M")
sb01b$site <- c(rep("Slamannan Bog", length(sb01b[,1])))
sb01b$logger <- c(rep("SLAMBOGM01", length(sb01b[,1])))
sb01b$period <- c(rep("post2016", length(sb01b[,1])))
head(sb01b)

sb01b <- check(sb01b) #check data
meandf_b <- hydrocorr(logid=sb01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 2 
# pre 2016
sb02a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Slamannan Bog/Site 2/SLAMBOGM02 0001 20141031.CSV",header=F)
str(sb02a) 
names(sb02a) <- c("date","time","level_raw")
sb02a$date <- as.Date(as.character(sb02a$date),format="%d.%m.%Y") ## change dates to date format
sb02a$level_raw <- as.numeric(as.character(sb02a$level_raw)) ## format water level as numeric
sb02a$datetime <- paste(sb02a$date,sb02a$time)
sb02a$datetime <- as.POSIXct(sb02a$datetime, format="%Y-%m-%d %H:%M")
sb02a$site <- c(rep("Slamannan Bog", length(sb02a[,1])))
sb02a$logger <- c(rep("SLAMBOGM02", length(sb02a[,1])))
sb02a$period <- c(rep("pre2016", length(sb02a[,1])))
head(sb02a)

sb02a <- check(sb02a) #check data
meandf_a <- hydrocorr(logid=sb02a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
sb02b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Slamannan/Data/SLAMBOGM02 0001 20160708.CSV",header=F)
str(sb02b)
sb02b <- separate(sb02b,1,c("date","time","level_raw"),sep=";",remove=T)
sb02b$date <- as.Date(as.character(sb02b$date),format="%d.%m.%Y") ## change dates to date format
sb02b$level_raw <- as.numeric(as.character(sb02b$level_raw)) ## format water level as numeric
sb02b$datetime <- paste(sb02b$date,sb02b$time)
sb02b$datetime <- as.POSIXct(sb02b$datetime, format="%Y-%m-%d %H:%M")
sb02b$site <- c(rep("Slamannan Bog", length(sb02b[,1])))
sb02b$logger <- c(rep("SLAMBOGM02", length(sb02b[,1])))
sb02b$period <- c(rep("post2016", length(sb02b[,1])))
head(sb02b)

sb02b <- check(sb02b) #check data
meandf_b <- hydrocorr(logid=sb02b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

### Site 3 
# pre 2016
sb03a <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_pre2016/Slamannan Bog/Site 3/SLAMBOGM03 0001 20141031.CSV",header=F)
str(sb03a) 
names(sb03a) <- c("date","time","level_raw")
sb03a$date <- as.Date(as.character(sb03a$date),format="%d.%m.%Y") ## change dates to date format
sb03a$level_raw <- as.numeric(as.character(sb03a$level_raw)) ## format water level as numeric
sb03a$datetime <- paste(sb03a$date,sb03a$time)
sb03a$datetime <- as.POSIXct(sb03a$datetime, format="%Y-%m-%d %H:%M")
sb03a$site <- c(rep("Slamannan Bog", length(sb03a[,1])))
sb03a$logger <- c(rep("SLAMBOGM03", length(sb03a[,1])))
sb03a$period <- c(rep("pre2016", length(sb03a[,1])))
head(sb03a)

sb03a <- check(sb03a) #check data
meandf_a <- hydrocorr(logid=sb03a,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_a) # write file


# post2016
sb03b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Slamannan/Data/SLAMBOGM03 0001 20160708.CSV",header=F)
str(sb03b)
sb03b <- separate(sb03b,1,c("date","time","level_raw"),sep=";",remove=T)
sb03b$date <- as.Date(as.character(sb03b$date),format="%d.%m.%Y") ## change dates to date format
sb03b$level_raw <- as.numeric(as.character(sb03b$level_raw)) ## format water level as numeric
sb03b$datetime <- paste(sb03b$date,sb03b$time)
sb03b$datetime <- as.POSIXct(sb03b$datetime, format="%Y-%m-%d %H:%M")
sb03b$site <- c(rep("Slamannan Bog", length(sb03b[,1])))
sb03b$logger <- c(rep("SLAMBOGM03", length(sb03b[,1])))
sb03b$period <- c(rep("post2016", length(sb03b[,1])))
head(sb03b)

sb03b <- check(sb03b) #check data
meandf_b <- hydrocorr(logid=sb03b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

#compare pre and post 2016
prepost2016 <- rbind(meandf_a,meandf_b) # compare pre and post 2016 data 
par(mfrow=c(1,1))
plot(prepost2016$level_corr~prepost2016$date,type="l",xlab=c("date"),ylab=c("corrected water level below surface (m)"),main=paste(meandf_a$site[1],meandf_a$logger[1])) # match fine

###///////
# Threepwood ----
# pre 2016 #! no data
# post2016
tw01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Threepwood/Data/Groundwater/THRPWOOD01 20180202130754.CSV",header=F)
str(tw01b)
tw01b <- separate(tw01b,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
tw01b <- tw01b[,c(1:3)]
tw01b$date <- as.Date(as.character(tw01b$date),format="%d.%m.%Y") ## change dates to date format
tw01b$level_raw <- as.numeric(as.character(tw01b$level_raw)) ## format water level as numeric
tw01b$datetime <- paste(tw01b$date,tw01b$time)
tw01b$datetime <- as.POSIXct(tw01b$datetime, format="%Y-%m-%d %H:%M")
tw01b$site <- c(rep("Threepwood", length(tw01b[,1])))
tw01b$logger <- c(rep("THRPWOOD01", length(tw01b[,1])))
tw01b$period <- c(rep("post2016", length(tw01b[,1])))
head(tw01b)

tw01b <- check(tw01b) #check data
meandf_b <- hydrocorr(logid=tw01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file


###///////
# Wester Moss ----
# pre 2016 #! no data
# post2016
wm01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Wester Moss/Data/0000000005 20180216131648.CSV",header=F)
str(wm01b)
wm01b <- separate(wm01b,1,c("date","time","level_raw","ch2","ch3"),sep=";",remove=T)
wm01b <- wm01b[,c(1:3)]
wm01b$date <- as.Date(as.character(wm01b$date),format="%d.%m.%Y") ## change dates to date format
wm01b$level_raw <- as.numeric(as.character(wm01b$level_raw)) ## format water level as numeric
wm01b$level_raw <- wm01b$level_raw/100 #convert to m
wm01b$datetime <- paste(wm01b$date,wm01b$time)
wm01b$datetime <- as.POSIXct(wm01b$datetime, format="%Y-%m-%d %H:%M")
wm01b$site <- c(rep("Wester Moss", length(wm01b[,1])))
wm01b$logger <- c(rep("5", length(wm01b[,1])))
wm01b$period <- c(rep("post2016", length(wm01b[,1])))
head(wm01b)

wm01b <- check(wm01b) #check data
meandf_b <- hydrocorr(logid=wm01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file

###///////
# Whim Bog ----
# site 1
# pre 2016 #! no data
# post2016
wb01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Whim Bog/Data/Groundwater/WHIMBOGM01 20180112135122.CSV",header=F)
str(wb01b)
wb01b <- separate(wb01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
wb01b <- wb01b[,c(1:3)]
wb01b$date <- as.Date(as.character(wb01b$date),format="%d.%m.%Y") ## change dates to date format
wb01b$level_raw <- as.numeric(as.character(wb01b$level_raw)) ## format water level as numeric
wb01b$datetime <- paste(wb01b$date,wb01b$time)
wb01b$datetime <- as.POSIXct(wb01b$datetime, format="%Y-%m-%d %H:%M")
wb01b$site <- c(rep("Whim Bog", length(wb01b[,1])))
wb01b$logger <- c(rep("WHIMBOGM01", length(wb01b[,1])))
wb01b$period <- c(rep("post2016", length(wb01b[,1])))
head(wb01b)

wb01b <- check(wb01b) #check data
meandf_b <- hydrocorr(logid=wb01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file


### site 2
# pre 2016 #! no data
# post2016
wb01b <- read.csv("file:///C:/Data/Hydrology/Hydrology_data_2016-2018_HydroSol/Whim Bog/Data/Groundwater/WHIMBOGM02 20180112125102.CSV",header=F)
str(wb01b)
wb01b <- separate(wb01b,1,c("date","time","level_raw","ch2","ch3","ch7"),sep=";",remove=T)
wb01b <- wb01b[,c(1:3)]
wb01b$date <- as.Date(as.character(wb01b$date),format="%d.%m.%Y") ## change dates to date format
wb01b$level_raw <- as.numeric(as.character(wb01b$level_raw)) ## format water level as numeric
wb01b$datetime <- paste(wb01b$date,wb01b$time)
wb01b$datetime <- as.POSIXct(wb01b$datetime, format="%Y-%m-%d %H:%M")
wb01b$site <- c(rep("Whim Bog", length(wb01b[,1])))
wb01b$logger <- c(rep("WHIMBOGM02", length(wb01b[,1])))
wb01b$period <- c(rep("post2016", length(wb01b[,1])))
head(wb01b)

wb01b <- check(wb01b) #check data
meandf_b <- hydrocorr(logid=wb01b,drifttype = "gradual") # calculate corrected daily means
hydrowrite(meandf_b) # write file


