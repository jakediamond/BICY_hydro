# 
# Coder: Jake Diamond
# Date: May 10, 2017
# Purpose: To analyze dbhydro cypress dome data
# 

# Set Working Directory
setwd("C:/Users/diamo/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dbhydroR)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Read in data, clean up column names
df <- read.csv("dbhydro.csv")
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
  gather(site, stage, 2:10)

# Get rid of NAs
df2 <- df2[complete.cases(df2), ]

# Read in reference elevation data
ref <- read.csv("elev_refs.csv")
colnames(ref)[2] <- "rel_elev"

# Calculate relative stages
df2 <- df2 %>%
  left_join(ref) %>%
  mutate(rel = stage - rel_elev) %>%
  dplyr::select(-rel_elev)

# Color blind palette
cbPalette <- c("#000000", "999999", "#E69F00",
               "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7")

# Plot time series for abs elev
ts_plot <- ggplot(data = df2, 
                    aes(x = datetime, y = stage, colour = site)) + 
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
  scale_x_datetime(date_breaks = "5 years", date_minor_breaks = "1 year",
               date_labels = (date_format = "%Y")) +
  scale_colour_manual(name = "Site", values = cbPalette) +
  ylab("Elevation Above NGVD29 (ft)")

# ggsave(ts_plot,"ts_plot.png")

# Plot time series for rel elev
ts_plot2 <- ggplot(data = df2 %>% 
                     dplyr::filter(site %in% c("BCA15", "BCA16", "BCA18")), 
                  aes(x = datetime, y = rel, colour = site)) + 
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
    legend.position = c(0.1, 0.14),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    legend.background = element_rect(
      fill = "gray90",
      size = 1,
      linetype = "solid",
      colour = "black"
    )
  ) +
  scale_x_datetime(date_breaks = "5 years", date_minor_breaks = "1 year",
                   date_labels = (date_format = "%Y")) +
  scale_colour_manual(name = "Site", values = c("black", "red", "blue")) +
  geom_hline(yintercept = 0, size = 1.2, linetype = "dashed") +
  ylab("Relative Elevation (ft)")

ggsave(ts_plot2, filename = "ts_rel.png")

# Get data into 4-hour format
wt <- df2 %>% 
  mutate(interval = floor_date(datetime, unit = "day") +
           hours(floor(hour(datetime) / 4) * 4)) %>% 
  group_by(site, interval) %>% 
  dplyr::summarize(sum = sum(stage)) 

# Function to get spectrum
spec_fun <- function(data)
  {
    ts <- ts(data$sum) # Turn into R timeseries
    sp <- spectrum(ts, span = c(3, 3))
    spec <- sp$spec # power spectra
    wl <- (1 / sp$freq) / (6) # wavelength (days)
    results <- data.frame(spec, wl)
  }

# Spectral data
spect <- wt %>% 
  group_by(site) %>% 
  do(spec_fun(.))

# Function to calculate slopes
piecewise_fun <- function(data, p_value) {
  library(segmented)
  library(strucchange)
  library(MASS)
  data$spec <- log(data$spec)
  data$wl <- log(data$wl)
  lin.mod <- lm(spec ~ wl, data = data) #linear model with IRLS for data
  d <- davies.test(lin.mod, seg.Z = ~ wl) # test if difference in slopes
  if(d$p.value < p_value){ #only does piecewise regression if null hypothesis of same slopes is rejected
    segmented.mod <- segmented(lin.mod,
                               seg.Z = ~ wl,
                               psi = NA,
                               control = seg.control(
                                 stop.if.error = FALSE,
                                 it.max = 5000,
                                 K = 1,
                                 n.boot = 0,
                                 h = 0.15)) #create piecewise regression
    if(is.null(summary(segmented.mod)$psi[1, 2])){
      slope1 <- coef(lin.mod)[[2]]
      slope2 <- slope1
      intercept1 <- coef(lin.mod)[[1]]
      intercept2 <- intercept1
      wlsplit <- NA
    } else {
      slope1 <- slope(segmented.mod)$wl[[1]]
      intercept1 <- intercept(segmented.mod)$wl[[1]]
      slope2 <- slope(segmented.mod)$wl[[2]]
      intercept2 <- intercept(segmented.mod)$wl[[2]]
      wlsplit <- exp(summary(segmented.mod)$psi[1, 2])
    }
  } else{
    slope1 <- coef(lin.mod)[[2]]
    slope2 <- slope1
    intercept1 <- coef(lin.mod)[[1]]
    intercept2 <- intercept1
    wlsplit <- NA
  }
  
  results <- data.frame(wlsplit, slope1, intercept1, slope2, intercept2)
}

# Calculate slopes
slopes <- spect %>%
  group_by(site) %>%
  do(piecewise_fun(., 0.05))

# Plot spectral data
# Create data frame for slope example
x = 10 ^ seq(1.4, 2.2, 0.2) 
y_fit = exp(-4) * x ^ 2
dat = data.frame(x, y_fit)

spec_plot <- ggplot(data = spect, 
                    aes(x = wl, y = spec)) + 
  geom_line() +
  geom_line(data = dat, aes(x = x, y = y_fit), colour = "red", size = 1) +
  scale_x_log10(
    labels = trans_format('log10', math_format(10 ^ .x)),
    breaks = c(10 ^ -1, 10 ^ -0, 10 ^ 1, 10 ^ 2, 10 ^ 3)) +
  scale_y_log10(
    labels = trans_format('log10', math_format(10 ^ .x)),
    breaks = c(10 ^ 0, 10 ^ 1, 10 ^ 2, 10 ^ 3, 10 ^ 4, 10 ^ 5, 10 ^
                 6)) +
  theme_bw() +
  theme(
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text = element_text(size = 18, colour = "black"),
    panel.background = element_blank(),
    legend.position = "none"
  ) +
  xlab("Wavelength (days)") +
  ylab("Spectral power") +
  annotate("text", x = 50, y = 10^1, label = "1/f ^ 2", parse = TRUE,
           colour = "red") +
  facet_wrap(~site)

ggsave(spec_plot, filename = "spectrum_facet.png")

# Function to get midnight values
mid_fun <- function(data) {
  index <- which(abs(data$hourmin - 0) == min(abs(data$hourmin - 0)))
  midnight <- data$hourmin[index]
  stage <- data$stage[index]
  data.frame(midnight, stage)
}

# Add date and hourmin
df2 <- df2 %>% 
  mutate(date = as.Date(datetime, tz = "America/New_York"),
         hourmin = as.numeric(strftime(datetime, 
                                        format = "%H%M", 
                                        tz = "America/New_York")))


# Calculate S
S_results <- df2 %>% 
  group_by(site, date) %>% 
  do(mid_fun(.)) %>% 
  as.data.frame() %>% 
  mutate(S = stage - lead(stage)) %>% 
  as.data.frame()

midnight <- S_results %>%
  spread(site, S)

write.csv(S_results, "midnight_long.csv")
write.csv(midnight, "midnight_wide.csv")

avg <- df2 %>%
  group_by(site, date) %>%
  summarise(avg = mean(stage, na.rm = TRUE))

write.csv(avg, "average_long.csv")
S_results <- read.csv("midnight_long.csv")
avg_wide <- avg %>%
  spread(site, avg)

avg <- read.csv("average_long.csv")
avg$X <- NULL

write.csv(avg_wide, "average_wide.csv")
library(WriteXLS)

S_split <- split(S_results, S_results$site)
avg_split <- split(avg, avg$site)

WriteXLS("S_split", "S.xls", AdjWidth = TRUE, BoldHeaderRow = TRUE)
WriteXLS("avg_split", "average.xls", AdjWidth = TRUE, BoldHeaderRow = TRUE)


WriteXLS("S_split", "S.xls", AdjWidth = TRUE, BoldHeaderRow = TRUE)
