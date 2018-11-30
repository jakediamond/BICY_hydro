# 
# Coder: Jake Diamond
# Date: October 31, 2018
# Purpose: To plot wetland stage time series
# 

# Set Working Directory
# setwd("C:/Users/diamo/Dropbox/Projects/Big Cypress ET/HydroData")
setwd("E:/Dropbox/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(readxl)

# Read in stage data
df <- read_excel("BICY Stage_Summary_JD.xlsx")
df$date <- as.Date(df$date)

# Get h_crit values
h_crit <- data.frame(site = c("RP1", "RP2", "TR1", "TR2", "TR3"),
                     h_crit = c(0.48, 0.48, 0.66, 0.80, 0.96))

# Determine if daily stage is above h_crit
# also calculate "periods" of connectivity
df <- df %>%
  left_join(h_crit) %>%
  na.omit() %>%
  mutate(above = ifelse(wl_m >= h_crit, 1, 0)) %>%
  group_by(site) %>%
  mutate(period = cumsum(c(0, (diff(above) != 0 | diff(date) != 1)))) %>%
  ungroup()

# Calculate number of wetlands connected over time
wet_conn_count <- df %>%
  group_by(date) %>%
  summarize(wet.no = sum(above))

# Calculate number of periods of connectivity, by year
connects <- df %>%
  mutate(year = year(date)) %>%
  group_by(site, year) %>%
  summarize(pds = max(period))
            
connects <- df %>%
  mutate(year = year(date)) %>%
  group_by(site, year, period) %>%
  summarize(length = n()) %>%
  summarize(mean_length = mean(length)) %>%
  right_join(connects)

# Calculate frequency of connectivity
h_crit_sum <- df %>%
  na.omit() %>%
  group_by(site) %>%
  summarize(freq = sum(above) / n())

# Reorder levels so that TR sites come first
df$site <- factor(df$site,
                  levels = c("RP1", "RP2", "TR1", 
                             "TR2", "TR3"))

cbbPalette <- c("#000000", "#E69F00",
                "#56B4E9", "#009E73", 
                "#0072B2"
                )

# Plot the stage data
p <- ggplot() +
  geom_line(data = filter(df, above == 0),
            aes(x = date,
                y = wl_m,
                color = site,
                group = interaction(site, above, period)),
            linetype = "solid") +
  geom_line(data = filter(df, above == 1),
            aes(x = date,
                y = wl_m,
                color = site,
                group = interaction(site, above, period)),
            linetype = "solid",
            size = 1.2) +
  # geom_text(data = h_crit_sum,
  #           aes(x = as.Date("01-01-2016", format = "%m-%d-%Y"),
  #               y = c(-0.22, -0.40, -0.58, -0.76, -0.94),
  #               color = site,
  #               label = paste("h >= h[crit]:", 
  #                             round(freq, digits = 3)*100,
  #                             "*\'%\'")),
  #           parse = TRUE,
  #           show.legend = FALSE) +
  theme_classic() +
  xlab("") +
  ylab("Wetland stage (m)") +
  scale_colour_manual(name = "Site",
                      values = cbbPalette) +
  scale_y_continuous(limits = c(-1, 1.2),
                     breaks = seq(-1, 1, 0.5)) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%b-%Y") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    legend.position = c(0.09, 0.27),
    legend.background = element_rect(colour = "black",
                                     fill = "gray90"),
    legend.key = element_rect(fill = "white",
                              colour = "black")
  )

p

ggsave(plot = p, 
       "stage_timeseries_bp_for_hcrit.tiff",
       device = "tiff",
       dpi = 600)

# Read in canal discharge data
df_canal <- read_excel("hydrologic data_canals.xlsx", sheet = 2,
                       col_types = c("text", "date", "numeric", "numeric"))
df_canal <- read_excel("hydrologic data_canals.xlsx",
                       col_types = c("text", "date", "numeric", "numeric")) %>%
  bind_rows(df_canal)

# Clean data
df_canal <- df_canal %>%
  rename(site = StationID, date = Date, flow.cfs = `Discharge (cfs)`) %>%
  transmute(site, date = as.Date(date), flow = flow.cfs * 0.028)

# Calculate the total canal flow when wetlands are connected
canal_connect <- wet_conn_count %>%
  filter(wet.no > 0) %>%
  left_join(df_canal, by = "date") %>%
  summarize(no.days = n(),
            flowsum = sum(flow, na.rm = TRUE))

canal_summary <- df_canal %>%
  filter(date >= min(df$date), date <= max(df$date)) %>%
  summarize(day.frac = canal_connect$no.days / n(),
            flow.frac = canal_connect$flowsum / sum(flow, na.rm = TRUE))

# Plot data for same time as wetland stage data
date_range <- c(min(df$date), max(df$date))

p2 <- ggplot() +
  geom_line(data = df_canal,
            aes(x = date,
                y = flow,
                color = site),
             size = 1.5,
            alpha = 0.7) +
  theme_classic() +
  xlab("") +
  ylab(expression("Canal discharge ("*m^3*d^-1*")")) +
  scale_colour_manual(name = "USGS Station",
                      values = c("blue", "royal blue")) +
  # scale_y_continuous(limits = c(-1, 1.2),
  #                    breaks = seq(-1, 1, 0.5)) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%b-%Y",
               limits = date_range) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    legend.position = c(0.2, 0.6),
    legend.background = element_rect(colour = "black",
                                     fill = "gray90"),
    legend.key = element_rect(fill = "white",
                              colour = "black")
  )

p2

# Plot wetland connections over time
p3 <- ggplot() +
  geom_line(data = wet_conn_count,
            aes(x = date,
                y = wet.no)) +
  theme_classic() +
  xlab("") +
  ylab("Count of wetland\nsurface connects") +
  # scale_colour_manual(name = "Site",
  #                     values = cbbPalette) +
  # scale_y_continuous(limits = c(-1, 1.2),
  #                    breaks = seq(-1, 1, 0.5)) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%b-%Y",
               limits = date_range) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  theme(
    legend.position = c(0.09, 0.25),
    legend.background = element_rect(colour = "black",
                                     fill = "gray90"),
    legend.key = element_rect(fill = "white",
                              colour = "black"),
    axis.title.y = element_text(hjust = 0.5)
  )
p3


p4 <- ggarrange(p3 + rremove("xlab") + rremove("x.axis") +
                  rremove("x.text") + rremove("x.ticks"), 
                p2,
                heights = 1:2,
                ncol = 1, nrow = 2, 
                # common.legend = TRUE,
                # legend = "right",
                align = "v")
p4


ggsave(plot = p4, 
       "canal_discharge_with_wetland_connections.tiff",
       device = "tiff",
       dpi = 600)
