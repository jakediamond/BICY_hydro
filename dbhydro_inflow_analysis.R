# 
# Coder: Jake Diamond
# Date: February 7, 2018
# Purpose: To calculate inflows from dbhydro data
# 

# Set Working Directory
setwd("C:/Users/diamo/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Read in data, clean up column names
df_s <-  read.csv("midnight_long.csv")
df_s$X <- NULL
df_s$date <- as.Date(df_s$date, format = "%Y-%m-%d")
df <- read.csv("average_long.csv")
df$X <- NULL
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df_et <- read.csv("pet.csv")
df_et$Date <- as.Date(df_et$Date, format = "%m/%d/%Y")
colnames(df_et) <- tolower(colnames(df_et))
df_r <- read.csv("rain.csv")
df_r$date <- as.Date(df_r$date, format = "%Y-%m-%d")
df_r2 <- read.csv("raccoon_rain.csv")
df_r2$date <- as.Date(df_r2$date, format = "%m/%d/%Y")
df_sy <- read.csv("rr_eqn_dbhydro.csv")

# Clean data for PET = 0
df_et <- df_et[!(df_et$pet_cm_d == 0), ]

# Make Change in storage negative (have it backwards in orig. calc)
df_s$S <- -df_s$S

# Combine data
df <- df %>%
  left_join(df_et) %>%
  left_join(df_r) %>%
  left_join(df_s) %>%
  left_join(df_r2)

# Remove days where no PET
df <- df[!is.na(df$pet_cm_d), ]

# Get all data into cm
df$avg <- df$avg * 30.48
df$rain_in <- df$rain_in * 2.54
colnames(df)[5] <- "site_rain"
df$stage <- df$stage * 30.48
df$S <- df$S * 30.48
df$precip_mm <- df$precip_mm / 10
colnames(df)[11] <- "rac_rain"

# Get rid of data when bottom of well or bad days
df <- df %>%
  filter(!(stage == avg))
  
# Apply specific yield to three sites
df <- df %>%
  left_join(df_sy) %>%
  mutate(sy = ifelse(stage < split,
                     0.15,
                     int2 + slope2 * stage)) %>%
  mutate(sy = ifelse(sy > 0.95,
                     0.95,
                     sy)) %>%
  mutate(Ssy = ifelse(is.na(sy),
                      S,
                      S * sy))

# Get rid of days where S is not from midnight to midnight
df <- df[df$midnight == 0, ]

# Calculate inflow
df <- df %>%
  rowwise() %>% 
  mutate(inflow = sum(-site_rain, 
                      pet_cm_d, 
                      S, 
                      na.rm = TRUE)) %>%
  mutate(inflow_sy = sum(-site_rain, 
                      pet_cm_d, 
                      Ssy, 
                      na.rm = TRUE)) %>%
  mutate(rain = ifelse(site_rain > 0, 
                       "yes", 
                       "no")) %>%
  mutate(rain = ifelse(is.na(rain),
                       ifelse(rac_rain > 0,
                              "yes",
                              "no"),
                       rain))

write.csv(df, "dbhydro_inflow_sy.csv")
  
# Plot data
p <- ggplot(data = filter(df, 
                          rain == "no"), 
            aes(x = stage,
                y = inflow)) +
  geom_point() + theme_bw() +
  xlab("Absolute stage ASL (cm)") +
  ylab("inflow (cm/d)") +
  facet_wrap(~site) + 
  geom_hline(yintercept = 0, 
             size = 1, 
             linetype = "dashed") +
  scale_y_continuous(limits = c(-10, 5))

p

ggsave(plot = p, 
       filename = "inflow_vs_abs_zoom.tiff",
       device = "tiff")
  
  