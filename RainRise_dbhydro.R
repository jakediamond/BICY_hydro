#
#Purpose: To calculate rain:rise from 15 minute rainfall and water table data
#Coder: Jake Diamond
#Date: February 13, 2018
#

#Set Working Directory
setwd("C:/Users/diamo/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

# Load data
df_r <- read.csv("rain_15min.csv")
df <- read.csv("dbhydro.csv")

# Clean data
df$X <- NULL
df_r$X <- NULL
df$date <- as.POSIXct(df$date,
                      format = "%Y-%m-%d %H:%M:%S",
                      tz = "UTC")
colnames(df)[1] <- "datetime"
colnames(df)[2:10] <- sapply(strsplit(
  colnames(df)[2:10],
  split = "_"), `[`, 1)
df <- df[-1, ]
df_r$interval <- as.POSIXct(df_r$interval,
                            format = "%Y-%m-%d %H:%M:%S",
                            tz = "UTC")
colnames(df_r) <- c("site", "datetime", "rain")

# Only do RR for three sites with rain
sites <- c("BCA15", "BCA16", "BCA18")
df_r <- subset(df_r, site %in% (sites))
df <- subset(df, datetime > min(df_r$datetime))
df <- df[, 1:4]

# Stage data into long format
df <- gather(df,
             key = site,
             value = stage,
             -datetime)

# Combine rain data into hourly format...less calculations. Same answer?
df_r2 <- df %>%
  select(site, datetime) %>%
  left_join(df_r)

df_r2 <- df_r2 %>% 
  mutate(datetime = floor_date(datetime, unit = "day") +
           hours(hour(ceiling_date(datetime, 
                                   unit = "hour")))) %>% 
  group_by(site, datetime) %>% 
  summarize(rain = sum(rain, na.rm = TRUE))

# Data combination
df2 <- left_join(df_r2, df)

# Get data in same units, cm
df2$rain <- df2$rain * 2.54
df2$stage <- df2$stage * 30.48

# Calculate rain sums by event
rainsum_fun <- function(data){
  # Bound the rainy days
  nrows <- nrow(data)
  firstrain <- min(which(!data$rain == 0))
  lastrain <- max(which(!data$rain == 0))
  
  # Adds columns to combined water table and rain dataframe for counts 
  # and cumulative depths of rain events
  data$count <- rep(NA, nrow(data))
  data$rainsum <- rep(NA, nrow(data))
  
  # Just in case first data point is a rain event
  data$rain[1] <- 0
  
  # Loops through the combined data set and calculates cumulative 
  # rain lengths and cumulative rain depths per event
  # This loop does not separate a rain event even if it has a 1 hour break in rainfall
  for(i in firstrain:lastrain){
    if(data$rain[i] != 0 && 
       is.na(data$count[i - 1])){
      data$count[i] <- 1
    } else{
      if(data$rain[i] != 0){
        data$count[i] <- data$count[i - 1] + 1
      } else{
        if(data$rain[i - 1] != 0 && 
           data$rain[i + 1] != 0){
          data$count[i] <- data$count[i - 1] + 1
        }
      }
    }
    if(!is.na(data$count[i])){
      data$rainsum[i] <- sum(data$rain[(i - 
                                          data$count[i] + 
                                          1):i])
    }
  }
  data
}

# Calculate rain sum by event
r <- df2 %>%
  group_by(site) %>%
  do(rainsum_fun(.))
  
#Rain:Rise calculations
rainrise <- function(data){
  #Create empty matrix to fill
  nrows <- nrow(data)
  firstrain <- min(which(!data$rain == 0))
  lastrain <- max(which(!data$rain == 0))
  rr_mat <- as.data.frame(matrix(NA, 
                                 nrow = (lastrain -
                                           firstrain + 1), 
                                 ncol = 11))
  colnames(rr_mat) <- c("datetime", "Stage", 
                        "AverageStage", "InitialStage",
                        "Rain", "Rise", "RainRise",
                        "RiseLag15", "RainRiseLag15",
                        "RiseLag30", "RainRiseLag30")
  rr_mat$datetime <- as.POSIXct(rr_mat$datetime,
                                tz = "UTC")
  
  #This takes out interception from the rainsum column
  # combine$Month <- format(index(combine), "%m")
  # LeafData <- data.frame(Month = seq(1, 12, by = 1), Leaf = rep(NA, 12))
  # LeafData$Leaf <- ifelse(LeafData$Month == c(1,2,3) | LeafData$Month == c(11,12), "LeafOff", "LeafOn")
  # combine2 <- data.frame(DateTime = index(combine), coredata(combine))
  # combine2 <- merge(LeafData, combine2, id = "Month")
  # combine2$Site <- rep(site_code, nrow(combine))
  # combine3 <- merge(interception, combine2)
  # combine3 <- combine3[with(combine3, order(DateTime)), ]
  # require(data.table)
  # combine4 <- as.data.table(combine3)
  # combine4[, LessInterception := (rainsum[rainsum != 0] - Interception)]
  # combine4[, RainKeep := ifelse(LessInterception > 0, LessInterception, NA)]
  # 
  # data <- as.data.frame(combine4)
  
  #Calculate rain:rise
  for(i in (firstrain - 1):(lastrain + 3)){
    if(!is.na(data$rainsum[i])){
      event_length <- data$count[i]
      rise <- data$stage[i] - 
        data$stage[i - event_length]
      rain <- data$rainsum[i]
      rr = rain / rise
      lag15_rise = data$stage[i + 1] - 
        data$stage[i - event_length + 1]
      lag30_rise = data$stage[i + 2] - 
        data$stage[i - event_length + 2]
      lag15_rr = rain/lag15_rise
      lag30_rr = rain/lag30_rise
      stage <- data$stage[i]
      avg_stage <- mean(data$stage[(i - event_length):i])
      initial_stage <- data$stage[i - event_length] 
      stage_date <- data$datetime[i]
      calc_data <- c(stage, avg_stage, 
                     initial_stage, rain, 
                     rise, rr, lag15_rise, 
                     lag15_rr, lag30_rise, lag30_rr)
      
      rr_mat[(i - (firstrain - 1)), 2:11] <- calc_data
      rr_mat[(i - (firstrain - 1)), 1] <- stage_date
    }
  }
  result <- as.data.frame(rr_mat)
}

#Clean rain:rise data by removing NAs
cleandata <- function(data){
  clean <- na.omit(data)
  attr(clean$datetime, "tzone") <- "UTC"
  
  #Adds EventID column
  clean$EventID <- rep(NA, nrow(clean))
  for(i in 1:nrow(clean)){
    if(i == 1){
      clean$EventID[i] <- 1
    } else{
      if(clean$InitialStage[i] == 
         clean$InitialStage[i - 1]){
        clean$EventID[i] = clean$EventID[i - 1]
      } else{
        clean$EventID[i] = clean$EventID[i - 1] + 1
      }
    }
  }
  result <- as.data.frame(clean)
}

#Further cleans the dataset by removing bad events
keep <- function(clean){
  #Adds a column to dataframe for the total rainfall for each event. 
  # This is total rainfall - interception
  maxeventrain <- aggregate(Rain ~ EventID, 
                            clean, 
                            FUN = max)
  maxeventrain$MaxRain <- maxeventrain$Rain
  maxeventrain$Rain <- NULL
  clean <- merge(maxeventrain, 
                 clean, 
                 by = "EventID")
  
  flowrate <- 3.1e-7 #m/s, equal to 1 inch per day - assumed to be the rate at which water 
  #drains from site; used to estimate when site has fully "recovered" from previous rain
  
  #Adds a "keep" column to cleaned dataset to determine if the datapoint will be kept or discarded
  #based on whether or not each event is far enough removed in time from the previous event
  #based on the assumed flowrate, or drainage rate. This was more effective at keeping "good" storms than a simple,
  #say, 24 hour separation criterion.
  clean$keep <- rep(NA, nrow(clean))
  
  for(i in 1:nrow(clean)){
    if(i == 1){
      clean$keep[i] <- 1
    } else{
      newevent <- diff(clean$EventID)[i - 1] #Check to see if it is a new storm event
      if(newevent == 1){
        if(clean$datetime[i] < (clean$MaxRain[i - 1] / 
                                flowrate + clean$datetime[i-1])){ #"Recovery" test
          clean$keep[i] <- 0
        } else{
          clean$keep[i] <- 1
        }
      } else{
        if(clean$keep[i-1] == 0){
          clean$keep[i] <- 0
        } else{
          clean$keep[i] <- 1
        }
      }
    }
  }
  
  final <- subset(clean, keep == 1) # & RainRise >= 0 & RainRise <= 1)
}

# Calculate rain:rise
rr <- r %>%
  group_by(site) %>%
  do(rainrise(.))

# Clean data
rr2 <- rr %>%
  group_by(site) %>%
  do(cleandata(.))

# Final data clean
rr3 <- rr2 %>%
  group_by(site) %>%
  do(keep(.))

#Uses lags in case of negative RR
library(data.table)
setDT(rr3)[, clean15lag := ifelse(RainRise < 0, RainRiseLag15, RainRise), by = EventID]
setDT(rr3)[, clean30lag := ifelse(clean15lag < 0, RainRiseLag30, clean15lag), by = EventID]
setDT(rr3)[, clean30lag := ifelse(clean15lag < 0, RainRiseLag30, clean15lag), by = EventID]

#Write to .csv
date_str = Sys.Date()
file_string = paste("RainRise_AllSites", date_str, ".csv", sep = "")
write.table(rr3, file = file_string, sep = ",", row.names = F)

#Considering rain:rise by event instead of individual 15-minute increments
byevent <- setDT(rr3)[, .SD[which.max(datetime)], by = c("EventID", "site")]
byevent <- as.data.frame(byevent)

byevent2 <- byevent %>%
  group_by(site, EventID) %>%
  mutate(count = n(),
         avgrr = mean(clean30lag, na.rm = TRUE))

count <- setDT(rr3)[, .N, by = c("EventID", "site")]
byevent$Count <- count$N

mean <- setDT(rr3)[, mean(clean30lag), by = c("EventID", "site")]
byevent$avgRR <- mean$V1

#Write to .csv
date_str = Sys.Date()
file_string2 = paste("RainRise_byEvent", date_str, ".csv", sep = "")
write.table(byevent, file = file_string2, sep = ",", row.names = F)

# Plotting
p <- ggplot(data = byevent %>%
              filter(MaxRain > 1),
            aes(x = AverageStage,
                y = avgRR)) +
  geom_point() +
  scale_y_continuous(limits = c(0, 2)) +
  facet_wrap(~site)
p
