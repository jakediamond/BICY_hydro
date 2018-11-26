# 
# Coder: Jake Diamond
# Date: May 9, 2017
# Purpose: To retrieve and compile all dbhydro data for cypress domes
# 

# Set Working Directory
setwd("C:/Users/diamo/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dbhydroR)
library(dplyr)
library(data.table)
library(tidyr)

# List of stations
stations <- c("BCNPA4", "BCNPA5", "BCA16", "BCNPA14", 
              "BCNPA3", "BCA15", "BCNPA12", 
              "BCA18", "BCNPA13")

# Meta data
meta_sw <- get_dbkey(stationid = stations, 
                     category = "SW", 
                     freq = "BK", 
                     detail.level = "full")

meta_w <- get_dbkey(stationid = stations, 
                    category = "WEATHER", 
                    freq = "BK", 
                    detail.level = "full")

## Had to fix get_dbykey function then did this to get latlong
# coords <- data.frame(Latitude = meta$Latitude, 
#                      Longitude = meta$Longitude)
# degrees <- as.numeric(apply(coords, 2, function(x)substring(x, 1, 2)))
# minutes <- as.numeric(apply(coords, 2, function(x)substring(x, 3, 4)))
# seconds <- as.numeric(apply(coords, 2, function(x)substring(x, 5, 10)))
# coords2 <- degrees + minutes / 60 + seconds / 3600
# coords <- data.frame(Latitude = coords2[1:19], Longitude = coords2[20:38])
# coords$Longitude <- coords$Longitude *  -1
# meta$Latitude <- coords$Latitude
# meta$Longitude <- coords$Longitude

meta <- rbind(meta_sw, meta_w)

write.csv(meta, "meta_data.csv")

# Retrieve surface water data
dbkeys <- meta_sw$Dbkey

for(i in 1:length(dbkeys)){
  # First check for error in retrieving data
  check <- tryCatch(
    get_hydro(dbkey = dbkeys[i],
              category = "SW",
              freq = "BK",
              date_min = "1990-01-01",
              date_max = "2017-05-09"),
    error = function(e) e)
  # If there is no error, go on
  if(!inherits(check, "error")){
    rm(check)
    x <- get_hydro(dbkey = dbkeys[i], 
                   category = "SW", 
                   freq = "BK", 
                   date_min = "1990-01-01", 
                   date_max = "2017-05-09")
    x <- as.data.table(x)
    if(i == 1){
      df <- x
    } else {
      df <- merge(df, x, by = "date", all = TRUE)
      rm(x)
    }
  }
}

df2 <- df %>%
  transmute(`BCNPA12_STG_ft NGVD29` = rowSums(cbind(`BCNPA12_STG_ft NGVD29.x`, 
                                        `BCNPA12_STG_ft NGVD29.y`),
                                        na.rm = TRUE),
            `BCNPA3_STG_ft NGVD29` = rowSums(cbind(`BCNPA3_STG_ft NGVD29.x`,
                                         `BCNPA3_STG_ft NGVD29.y`),
                                         na.rm = TRUE),
            `BCNPA4_STG_ft NGVD29` = rowSums(cbind(`BCNPA4_STG_ft NGVD29.x`,
                                         `BCNPA4_STG_ft NGVD29.y`),
                                         na.rm = TRUE),
            `BCNPA5_STG_ft NGVD29` = rowSums(cbind(`BCNPA5_STG_ft NGVD29.x`,
                                         `BCNPA5_STG_ft NGVD29.y`),
                                         na.rm = TRUE),
            date = date
  )
df2[df2 == 0] <- NA
df2 <- df %>%
  select(-c(`BCNPA12_STG_ft NGVD29.x`,
            `BCNPA12_STG_ft NGVD29.y`,
            `BCNPA3_STG_ft NGVD29.x`,
            `BCNPA3_STG_ft NGVD29.y`,
            `BCNPA4_STG_ft NGVD29.x`,
            `BCNPA4_STG_ft NGVD29.y`,
            `BCNPA5_STG_ft NGVD29.x`,
            `BCNPA5_STG_ft NGVD29.y`)) %>%
  left_join(df2)
  

write.csv(df2, "dbhydro.csv")

df2$rounded <- as.POSIXlt(round(as.numeric(df$date) / 
                                          (5 * 60)) * (5 * 60),
                                  origin = '1970-01-01')
df3 <- df2 %>%
  select(-date) %>%
  gather(site, stage, 2:10)



# Retrieve surface water data
dbkeys2 <- meta_w$Dbkey


for(i in 1:length(dbkeys2)){
  # First check for error in retrieving data
  # check <- tryCatch(
  #   get_hydro(dbkey = dbkeys2[i],
  #             category = "WEATHER",
  #             freq = "BK",
  #             date_min = "1990-01-01",
  #             date_max = "2017-05-09"),
  #   error = function(e) e)
  # # If there is no error, go on
  # if(!inherits(check, "error")){
    # rm(check)
    x <- get_hydro(dbkey = dbkeys2[i], 
                   category = "WEATHER", 
                   freq = "BK", 
                   date_min = "1990-01-01", 
                   date_max = "2017-05-09")
    x <- as.data.table(x)
    if(i == 1){
      df <- x
    } else {
      df <- merge(df, x, by = "date", all = TRUE)
      rm(x)
    }
  # }
}

colnames(df) <- c("datetime", "BCA15_radar", "BCA15",
                  "BCA16_radar", "BCA16",
                  "BCA18_radar", "BCA18")

write.csv(df, "dbhydro_rain.csv")

