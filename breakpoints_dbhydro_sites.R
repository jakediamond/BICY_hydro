# 
# Coder: Jake Diamond
# Date: February 13, 2018
# Purpose: To calculate breakpoints in inflow-stage rlshp
# 
# Want to look at distribution of near breakpoint
# Want to look at sensitivity of breakpoint

# Set Working Directory
setwd("C:/Users/diamo/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(purrr)
library(broom)
library(viridis)

# Read in data, clean up column names
df <- read.csv("dbhydro_inflow_sy.csv")
df$date <- as.Date(df$date, format = "%Y-%m-%d")
df$X <- NULL

# Clean data a bit
# df <- df[abs(df$inflow_sy) < 5, ]

# Tidy data
df_t <- df %>%
  dplyr::select(site, stage, inflow, inflow_sy, rain) %>%
  gather(key = "in_type", 
         value = "inflow",
         -site,
         -stage,
         -rain)

# Breakpoint function
bp_fun <- function(data, k, p_value) {
  library(segmented)
  library(MASS)

  lin.mod <- glm(inflow ~ stage, data = data)
  d <- davies.test(lin.mod, seg.Z =  ~ stage)
  if(d$p.value < p_value){
    segmented.mod <- segmented(lin.mod,
                               seg.Z = ~ stage,
                               psi = NA,
                               control = seg.control(
                                 stop.if.error = FALSE,
                                 it.max = 5000,
                                 K = k,
                                 n.boot = 0,
                                 h = 0.05))
  }
}

# Set p-value for breakpoint analysis
p_value <- 0.05

# Get data in list format for purrr
bp_df <- df_t %>%
  dplyr::filter(rain == "no") %>%
  dplyr::select(-rain) %>%
  group_by(site, in_type) %>%
  nest()

# Apply breakpoint analysis to data 
bp <- bp_df %>%
  mutate(model = map(data, 
                     bp_fun, 
                     p_value = p_value,
                     k = 1))

# Function to tidy model results
tidy_fun <- function(mod){
  if(length(mod$coefficients) < 3){
    tidy(mod)
  } else{list(m = tidy(mod),
              psi = data.frame(
                term = "breakpoint",
                estimate = unlist(
                  tail(mod$psi.history, n = 1)))
  ) %>%
      bind_rows()
  }
}

# Final data frame with breakpoint model results
bp2 <- bp %>%
  transmute(site, in_type,
            beta = map(model, 
                       tidy_fun)) %>%
  unnest()

# Plot
p_bp <- ggplot(data = filter(df, rain == "no"), 
             aes(x = stage,
                 y = inflow_sy)) +
  geom_point(shape = 1) + 
  theme_bw() +
  xlab("Absolute stage ASL (cm)") +
  ylab("Inflow (cm/d)") +
  facet_wrap(~site) + 
  scale_linetype_discrete(name = "Inflow Type",
                          breaks = c("inflow",
                                     "inflow_sy"),
                          labels = c("No Sy",
                                     "Sy")) +
  theme(legend.position = c(0.0711, 0.78),
        legend.background = element_rect(
          colour = "black",
          fill = "gray90"),
          legend.title = element_blank(),
        legend.margin = margin(1, 1.5, 1.5, 1.5),
        legend.key = element_rect(fill = "white", 
                                  colour = "black")) + 
  geom_hline(yintercept = 0, 
             size = 1, 
             linetype = "dashed") +
  geom_vline(data = dplyr::filter(bp2,
                                  term == "breakpoint"), 
             aes(xintercept = estimate,
                 linetype = in_type)) + 
  scale_y_continuous(limits = c(-10, 10))

p_bp

ggsave(plot = p_bp, "breakpoints_sy_1bp_noclean.tiff",
       device = "tiff",
       width = 8,
       height = 6,
       units = "in")

# Calculate percentage of time above breakpoint
df_brk <- bp2 %>%
  dplyr::filter(term == "breakpoint",
                in_type == "inflow_sy") %>%
  dplyr::select(site, estimate) %>%
  right_join(df) %>%
  mutate(month = month(date)) %>%
  group_by(site, year, month) %>%
  summarize(days_above = sum(stage > estimate,
                             na.rm = TRUE))

# Remove years without full annual data
df_brk <- df_brk %>%
  group_by(site, year) %>%
  summarize(n = n()) %>%
  dplyr::filter(n == 12) %>%
  semi_join(df_brk, .)

df <- df_brk %>%
  group_by(site, year) %>%
  summarize(n = n()) %>%
  dplyr::filter(n == 12) %>%
  semi_join(df, .)
  
# Summarize data by month
df_brk_s <- df_brk %>%
  group_by(site, month) %>%
  summarize(avg_above = mean(days_above, 
                             na.rm = TRUE),
            se = sd(days_above, 
                    na.rm = TRUE) / 
              sqrt(n())) %>%
  mutate(d = days_in_month(month)) %>%
  mutate(per_above = avg_above / d,
         per_se = se / d)
  
# Summarize data by year
df_brk_s_y <- df_brk %>%
  group_by(site, year) %>%
  summarize(days_above = sum(days_above, 
                             na.rm = TRUE)) %>%
  filter(days_above > 0) %>%
  group_by(site) %>%
  summarize_at(vars(days_above), 
                funs(mean,
                     sd, 
                     se = sd(.) / 
                       n())) %>%
  mutate(per_above = 
           round(100 * (mean / 365), 
                 digits = 1))

# Data frame for x,y position of text on graph
dat <- df_brk_s_y %>%
  mutate(x = 4,
         y = 0.9)

# Plotting distributions of time over brkpt
month_p <- ggplot(data = df_brk_s, 
                  aes(x = month,
                      y = per_above)) +
  geom_point() + 
  geom_line() + 
  geom_errorbar(aes(ymin = per_above - per_se, 
                    ymax = per_above + per_se), 
                width = .1) +
  theme_bw() +
  xlab("Month") +
  ylab("Fraction of days above breakpoint") +
  geom_text(data = dat,
            aes(x = x,
                y = y,
                label = paste(per_above,
                              "\u00B1",
                              round(se, digits = 1),
                               "%",
                              sep = ""))) +
  facet_wrap(~site)

month_p

ggsave(plot = month_p, "percent_by_month_rain_sy_noclean.tiff",
       device = "tiff",
       width = 8,
       height = 6,
       units = "in")
  
# Name sites that don't have RR
sites <- c("BCNPA12", "BCNPA13", "BCNPA14",
           "BCNPA3", "BCNPA4", "BCNPA5")

# Calculate relative magnitude of inflow/outlfow, need to not include
# rainy days for sites that don't have rain gages (or use racoon rain gage)

# Use this to remove days when "sites" have rain
df_sitenorain <- df %>%
  dplyr::filter(rain == "yes", site %in% sites) %>%
  anti_join(df, .)

# Use this to estimate inflow using raccoon rain gage
df_rainest <- df %>%
  mutate(inflow_sy = ifelse(rain == "yes" & site %in% sites,
         inflow_sy - rac_rain,
         inflow_sy))

# Summarize inflow above breakpoint
df_above_bp <- bp2 %>%
  dplyr::filter(term == "breakpoint",
                in_type == "inflow_sy") %>%
  dplyr::select(site, estimate) %>%
  right_join(df_rainest) %>%
  dplyr::filter(stage > estimate) %>%
  group_by(site, year) %>%
  summarize(cum_in = sum(inflow_sy[inflow_sy > 0],
                         na.rm = TRUE),
            cum_out = sum(inflow_sy[inflow_sy <= 0],
                          na.rm = TRUE)) %>%
  gather(key = direction,
         value = inflow,
         -site,
         -year)

# Summarize inflow below breakpoint
df_below_bp <- bp2 %>%
  dplyr::filter(term == "breakpoint",
                in_type == "inflow_sy") %>%
  dplyr::select(site, estimate) %>%
  right_join(df_rainest) %>%
  dplyr::filter(stage <= estimate) %>%
  group_by(site, year) %>%
  summarize(cum_in = sum(inflow_sy[inflow_sy > 0],
                         na.rm = TRUE),
            cum_out = sum(inflow_sy[inflow_sy <= 0],
                          na.rm = TRUE)) %>%
  gather(key = direction,
         value = inflow,
         -site,
         -year)

# Calculate balance of inflows and outflows for plotting
df_balance_above <- df_above_bp %>%
  group_by(site, direction) %>%
  summarize(avg = mean(abs(inflow))) %>%
  group_by(site) %>%
  mutate(tot = sum(avg)) %>%
  mutate(percentage = 100 * (avg / tot)) %>%
  mutate(pos = ifelse(direction == "cum_in",
                      avg - (0.5 * avg),
                      -(avg - (0.5 * avg))))

df_balance_below <- df_below_bp %>%
  group_by(site, direction) %>%
  summarize(avg = mean(abs(inflow))) %>%
  group_by(site) %>%
  mutate(tot = sum(avg)) %>%
  mutate(percentage = 100 * (avg / tot)) %>%
  mutate(pos = ifelse(direction == "cum_in",
                      avg - (0.5 * avg),
                      -(avg - (0.5 * avg))))

# Plot
p_above <- ggplot(data = df_above_bp,
                  aes(x = site,
                      y = inflow,
                      fill = direction)) +
  geom_bar(stat = "summary", 
           fun.y = "mean",
           colour = "black") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.4) +
  geom_text(data = df_balance_above, 
            aes(x = site, 
                y = pos, 
                label = paste0(round(percentage, 1),
                               "%")),
            size = 3) + 
  xlab("Site") +
  ylab("Net Annual Inflow (cm/yr)") +
  scale_fill_manual(values = c("white",
                                 "grey"),
                      guide = FALSE) +
  # scale_y_continuous(limits = c(-100, 100),
  #                    breaks = seq(-100, 100, 25)) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 0.5))

p_below <- ggplot(data = df_below_bp %>%
                    filter(site %in% c("BCA15",
                                       "BCA16",
                                       "BCA18")),
                  aes(x = site,
                      y = inflow,
                      fill = direction)) +
  geom_bar(stat = "summary", 
           fun.y = "mean",
           colour = "black") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.4) +
  geom_text(data = filter(df_balance_below,
                          site %in% c("BCA15",
                                      "BCA16",
                                      "BCA18")), 
            aes(x = site, 
                y = pos, 
                label = paste0(round(percentage, 1),
                               "%")),
            size = 3) + 
  xlab("Site") +
  scale_fill_manual(values = c("white",
                               "grey"),
                    guide = FALSE) +
  # scale_y_continuous(limits = c(-100, 100),
  #                    breaks = seq(-100, 100, 25)) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1),
        axis.title.y = element_blank())


plot2 <- plot_grid(p_above, p_below,
          labels = c("Above Breakpoint",
                     "Below Breakpoint"),
          align = "h",
          label_x = .3, 
          hjust = 0
)
plot2

save_plot("cumulative_inflows_v2_estimate_rain.tiff", plot2,
          ncol = 2)

# Plotting for inset map
p_above2 <- ggplot(data = df_below_bp %>%
                    filter(site %in% c("BCA15",
                                       "BCA16",
                                       "BCA18")),
                  aes(x = site,
                      y = inflow,
                      fill = direction)) +
  geom_bar(stat = "summary", 
           fun.y = "mean",
           colour = "black") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.4) +
  geom_text(data = filter(df_balance_below,
                          site %in% c("BCA15",
                                      "BCA16",
                                      "BCA18")), 
            aes(x = site, 
                y = pos, 
                label = paste0(round(percentage, 1),
                               "%")),
            size = 3) + 
  xlab("Site") +
  scale_fill_manual(values = c("white",
                               "grey"),
                    guide = FALSE) +
  # scale_y_continuous(limits = c(-100, 100),
  #                    breaks = seq(-100, 100, 25)) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 1),
        axis.title.y = element_blank())




ggdraw() +
  draw_plot(plot.diamonds + theme(legend.justification = "bottom"), 0, 0, 1, 1) +
  draw_plot(plot.mpg + scale_color_viridis(discrete = TRUE) + 
              theme(legend.justification = "top"), 0.5, 0.52, 0.5, 0.4) +
  draw_plot_label(c("A", "B"), c(0, 0.5), c(1, 0.92), size = 15)
