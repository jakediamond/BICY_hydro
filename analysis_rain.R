# 
# Coder: Jake Diamond
# Date: May 14, 2017
# Purpose: To analyze dbhydro cypress dome rain data
# 

# Set Working Directory
setwd("C:/Users/Jake/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dbhydroR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

# Read in data, clean up column names
df <- read.csv("dbhydro_rain.csv")
df$X <- NULL
df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M", 
                      tz = "America/New_York")
colnames(df) <- sapply(strsplit(colnames(df), "_"), function(x) x[1])
colnames(df)[1] <- "datetime"

# # Round datetime to nearest 5 min
# df$datetime <- as.POSIXct(
#   as.POSIXlt(round(
#     as.numeric(df$date) / (5 * 60)) * (5 * 60),
#     origin = '1970-01-01')
# )

# Get data in long format
df2 <- df %>%
  gather(site, rain, 2:7) %>%
  mutate(date = as.Date(strftime(datetime, format = "%Y-%m-%d")))

# Summarize data by daily rainfall sum
df_sum <- df2 %>%
  group_by(site, date) %>%
  summarize(sum = sum(rain, na.rm = TRUE))

# Write daily rainfall to to disc
# write.csv(df_sum, "rain.csv")

# Color blind palette
cbPalette <- c("#000000", "grey", "dark blue",
               "blue", "dark red", "red")

# Plot time series for rain
ts_plot <- ggplot(data = df_sum, 
                    aes(x = date, y = sum, colour = site)) + 
  geom_line() +
  theme_bw() +
  theme(
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title = element_text(face = "bold", size = 18),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18, colour = "black"),
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.background = element_rect(
      fill = "gray90",
      size = 1,
      linetype = "solid",
      colour = "black"
    )
  ) +
  scale_x_date(date_breaks = "5 years", date_minor_breaks = "1 year",
               date_labels = (date_format = "%Y")) +
  scale_colour_manual(name = "Site", values = cbPalette) +
  ylab("Rain (in/d)")

# Save rain time series
ggsave(ts_plot, filename = "ts_plot_rain.png")

# Clean data
df2["rain"][is.na(df2["rain"])] <- 0
df2 <- df2[complete.cases(df2), ]

# Get data into 12 hr format
rain <- df2 %>% 
  mutate(interval = floor_date(datetime, unit = "day") +
           hours(floor(hour(datetime) / 12) * 12)) %>% 
  group_by(site, interval) %>% 
  dplyr::summarize(sum = sum(rain)) 

# Function to get spectrum
spec_fun <- function(data)
{
  ts <- ts(data$sum) # Turn into R timeseries
  sp <- spectrum(ts, span = c(3))
  spec <- sp$spec # power spectra
  wl <- (1 / sp$freq) / (2) # wavelength (days)
  results <- data.frame(spec, wl)
}

# Spectral data
spect <- df3 %>% 
  group_by(site) %>% 
  do(spec_fun(.))

# Plot spectral data
# Create data frame for slope example
x = 10 ^ seq(0.5, 1.5, 0.2) 
y_fit = exp(-9) * x ^ 2
dat = data.frame(x, y_fit)

spec_plot <- ggplot(data = spect, 
                    aes(x = wl, y = spec, colour = site)) + 
  geom_line() +
  scale_x_log10(
    labels = trans_format('log10', math_format(10 ^ .x)),
    breaks = c(10 ^ -1, 10 ^ -0, 10 ^ 1, 10 ^ 2, 10 ^ 3)
    ) +
  scale_y_log10(
    labels = trans_format('log10', math_format(10 ^ .x)),
    breaks = c(10 ^ -3, 10 ^ -2, 10 ^ -1, 10 ^ 0, 10 ^ 1)
    ) +
  theme_bw() +
  theme(
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title = element_text(face = "bold", size = 18),
    plot.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18, colour = "black"),
    panel.background = element_blank(),
    legend.position = c(0.85, 0.22),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.background = element_rect(
      fill = "gray90",
      size = 1,
      linetype = "solid",
      colour = "black"
    )
  ) +
  scale_colour_manual(name = "Site", values = cbPalette) +
  xlab("Wavelength (days)") +
  ylab("Spectral Power") 
# Save spectral graph
ggsave(spec_plot, filename = "spectrum_rain.png")

# Get ready to save data as an .xls
colnames(df_sum)[3] <- "rain_in"
split <- split(df_sum, df_sum$site)

# Write the data so that each site is its own sheet
library(WriteXLS)
WriteXLS("split", "rain.xls", AdjWidth = TRUE, BoldHeaderRow = TRUE)

