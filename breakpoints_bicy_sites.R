# 
# Coder: Jake Diamond
# Date: October 19, 2018
# Purpose: To do hydro analysis of BICY data, with updated data from daniel
# 

# Set Working Directory
# setwd("C:/Users/diamo/Dropbox/Projects/Big Cypress ET/HydroData")
setwd("E:/Dropbox/Dropbox/Projects/Big Cypress ET/HydroData")

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(purrr)
library(broom)
library(readxl)
library(lazyeval)
library(grid)
library(gridExtra)
library(ggpubr)

# Read in data, clean up column names
df <- read_excel("BICY Inflow Rates_new_jd.xlsx")
df$site <- toupper(df$site)

# Correct inflow in m3/d when 0 to NA
df$inflow_m3.d <- ifelse(df$inflow_m3.d == 0,
                         NA,
                         df$inflow_m3.d)

# Get water years
df$year <- year(df$date)
df$wy <- ifelse(month(df$date) >= 10,
                df$year + 1,
                df$year)

# Tidy data
df_t <- df %>%
  gather(key = "in_type", 
         value = "inflow",
         -site,
         -stage_m,
         -rain,
         -date,
         -year,
         -wy)

# Breakpoint function
bp_fun <- function(data, k, p_value) {
  library(segmented)
  library(MASS)

  lin.mod <- glm(inflow ~ stage_m, data = data)
  d <- davies.test(lin.mod, seg.Z =  ~ stage_m)
  if(d$p.value < p_value){
    segmented.mod <- segmented(lin.mod,
                               seg.Z = ~ stage_m,
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
  dplyr::filter(rain == "0") %>%
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

# Get order of factors for plotting 
df_t$in_type <- factor(df_t$in_type, 
                       levels = c("inflow_uncor_cm.d"
                                  , "inflow_sy_pet_cm.d"
                                  , "inflow_m3.d"))
levels(df_t$in_type) <- c("Uncorrected", 
                          "Corrected", 
                          "Volumetric")
bp2$in_type <- factor(bp2$in_type, 
                      levels = c("inflow_uncor_cm.d"
                                 , "inflow_sy_pet_cm.d"
                                 , "inflow_m3.d"))
levels(bp2$in_type) <- c("Uncorrected", 
                         "Corrected", 
                         "Volumetric")

# Get lidar spill elevations
lid <- read.csv("lidar_spill_elev.csv")

# Pick site for the progression
sit <- "TR2"

# Read in stage-area relationships for inset plot
sa <- read.csv("stage_area.csv")

# Get rain:rise data for inset plot
rr <- read.csv("sites_rr_plotting.csv")
rr$Site <- toupper(rr$Site)

#Set Rain event cutoff value
cutoff <- 0.015 #meters

# clean data
rr <- rr %>%
  filter(!is.na(RainRise), 
         RainRise < 1.1,
         Rain >= cutoff)

# Remove known outliers
rr <- rr %>%
  filter(!(RainRise > 0.3 & AverageStage < 0)) %>%
  filter(!(RainRise > 0.4 & AverageStage < 0.6 & Site == "TR2"))

# Curve fitting
mods <- rr %>%
  filter(AverageStage > 0) %>%
  group_by(Site) %>%
  do(tidy(nls(RainRise ~ a * exp(b * AverageStage), 
              data = .,
              start = list(a = 0.4, 
                           b = 0.4))))

# function to get equations 
text_fun <- function(data){
  a <- format(data$estimate[1], digits = 3)
  b <- format(data$estimate[2], digits = 3)
  eqn <- paste("y = ", a, "*exp(", b, "*x)", sep = "")
  data.frame(eqn = eqn)
}

# get equations
eqns <- mods %>%
  group_by(Site) %>%
  do(text_fun(.))

# Plot rain rise inset
inset_sy <- ggplot(dplyr::filter(rr, Site == sit), 
            aes(x = AverageStage, y = RainRise)) +
  geom_point(shape = 16,
             size = 0.7) + 
  theme_bw() + 
  geom_line(stat = "smooth",
            method = "nls", 
            formula = (y ~ a * exp(b * x)), 
            method.args = list(start = c(a = 0.4,
                                         b = 0.4)),
            se = FALSE, 
            linetype = 1,
            alpha = 0.8,
            color = "blue") +
  xlab("") + 
  ylab("Rain:Rise") +  
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, 1)) +
  theme(axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) 

inset_sy

# Plot stage-area inset
inset_sa <- ggplot(dplyr::filter(sa, site == sit), 
                   aes(x = stage_m, y = area_m2 / 10000)) +
  geom_point(shape = 16,
             size = 0.7) + 
  theme_bw() + 
  xlab("") + 
  ylab(expression("Area ("*10^4~m^2*")")) +  
  scale_x_continuous(limits = c(0, 1),
                     breaks = c(0, 1)) +
  scale_y_continuous(label = comma) +
  theme(axis.line = element_line(colour = "black"), 
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA)) 

inset_sa

# Get rid of some outliers to make plotting cleaner for TR2
df_t.c <- df_t %>%
  dplyr::filter(((in_type == "Uncorrected" & 
                      inflow < 2) |
                   (in_type == "Corrected" & 
                   between(inflow, -3, 2)) |
                   (in_type == "Volumetric" &
                      between(inflow, -500, 200))))

# Plot progression for chosen site (TR2)
prog <- ggplot(data = dplyr::filter(df_t.c, 
                                    rain == 0,
                                    site == sit), 
               aes(x = stage_m,
                   y = inflow)) +
  geom_point(shape = 1) + 
  geom_smooth(method = "loess") +
  theme_bw() +
  xlab("Stage (m)") +
  facet_wrap(~in_type,
             scales = "free_y") +
  ylab(expression("Net flow ("*cm~d^-1~italic("or")~~m^3*d^-1*")")) +
  geom_vline(data = dplyr::filter(bp2,
                                  term == "breakpoint",
                                  site == sit),
             aes(xintercept = estimate,
                 linetype = in_type)) +
  geom_vline(data = dplyr::filter(lid,
                                   site == sit),
             aes(xintercept = spill_elev_lidar_m),
             color = "grey") +
  scale_linetype_manual(name = "Connectivity threshold",
                        breaks = c("Uncorrected",
                                   "Corrected",
                                   "Volumetric"),
                        values = c("solid",
                                   "dashed",
                                   "dotted")) +
  theme(legend.position = "bottom",
        legend.background = element_rect(
          colour = "black",
          fill = "gray90"),
        legend.title = element_text(face = "bold"),
        legend.margin = margin(1, 1.5, 1.5, 1.5),
        legend.key = element_rect(fill = "white", 
                                  colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1) + 
  geom_hline(yintercept = 0,
             size = 1,
             linetype = "dashed") 

prog

prog_full <- ggdraw() +
  draw_plot(prog) +
  draw_plot(inset_sy, 
            x = 0.37,
            y = 0.295,
            width = 0.2,
            height = 0.3,
            scale = 0.8) + 
  draw_plot(inset_sa, 
            x = 0.70,
            y = 0.295,
            width = 0.2,
            height = 0.3,
            scale = 0.8)
# prog_full

# Save plot
ggsave(plot = prog_full, 
       paste("progression_", sit, "_", 
             Sys.Date(), ".tiff" ,sep = ""),
       device = "tiff",
       width = 6,
       height = 4,
       units = "in",
       dpi = 600)

# Set colorblind scale
cbbPalette <- c("#56B4E9", 
                "#009E73", "#0072B2",
                "#000000", "#E69F00")

# Reorder levels so that TR sites come first
df$site <- factor(df$site,
                  levels = c("TR1", "TR2", "TR3", 
                     "RP1", "RP2", "RP3"))

# Plot the loess of stage-volume
p_loess_vol <- ggplot(data = dplyr::filter(df, 
                                           rain == 0,
                                           site != "RP3"), 
               aes(x = stage_m,
                   y = inflow_m3.d),
               alpha = 0.75) +
  theme_classic() +
  xlab("Stage (m)") +
  ylab(expression("Net flow ("*m^3~d^-1*")")) +
  scale_colour_manual(name = "Site",
                      values = cbbPalette) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(0, 1.5),
                     expand = c(0,0)) +
  coord_cartesian(ylim = c(-250, 80)) +
  geom_smooth(aes(color = site),
              method = "loess",
              se = FALSE) +
  geom_vline(data = dplyr::filter(bp2,
                                  term == "breakpoint",
                                  in_type == "Volumetric",
                                  site != "RP3"),
             aes(xintercept = estimate,
                 colour = site),
             linetype = "dashed") +
  theme(legend.position = "none",
        legend.background = element_rect(
          colour = "black",
          fill = "gray90"),
        legend.margin = margin(1, 1.5, 1.5, 1.5),
        legend.key = element_rect(fill = "white", 
                                  colour = "black"),
        legend.title = element_text(face = "bold")) + 
  geom_hline(yintercept = 0, 
             size = 1, 
             linetype = "solid") 

p_loess_vol

# Same plot but for corrected, non-volume inflow
p_loess_cor <- ggplot(data = dplyr::filter(df, 
                                           rain == 0,
                                           site != "RP3"), 
                      aes(x = stage_m,
                          y = inflow_sy_pet_cm.d)) +
  theme_classic() +
  xlab("Stage (m)") +
  ylab(expression("Net flow ("*cm~d^-1*")")) +
  scale_colour_manual(name = "Site",
                      values = cbbPalette) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(limits = c(-0.75, 1.5),
                     expand = c(0,0)) +
  coord_cartesian(ylim = c(-1, 1)) +
  geom_smooth(aes(color = site),
              method = "loess",
              se = FALSE) +
  geom_vline(data = dplyr::filter(bp2,
                                  term == "breakpoint",
                                  in_type == "Corrected",
                                  site != "RP3"),
             aes(xintercept = estimate,
                 colour = site),
             linetype = "dashed") +
  theme(legend.position = c(0.2, 0.25),
        legend.background = element_rect(
          colour = "black",
          fill = "gray90"),
        legend.margin = margin(1, 1.5, 1.5, 1.5),
        legend.key = element_rect(fill = "white", 
                                  colour = "black"),
        legend.title = element_text(face = "bold")) + 
  geom_hline(yintercept = 0, 
             size = 1, 
             linetype = "solid") 

p_loess_cor

p_loess_full <- plot_grid(p_loess_cor,
                          p_loess_vol,
                          labels = c("A", "B"))

ggsave(plot = p_loess_full, 
       paste("loess_full_limit_", 
             Sys.Date(), 
             ".tiff", 
             sep = ""),
       device = "tiff",
       width = 8,
       height = 6,
       units = "in",
       dpi = 600)

# Calculate cumulative inflows
df_cume_stage_all <- df %>%
  mutate(flow_type = ifelse(inflow_m3.d > 0, "Inflow", "Outflow")) %>%
  group_by(site, flow_type) %>%
  arrange(stage_m) %>%
  dplyr::filter(!(is.na(inflow_m3.d))) %>%
  nest() %>%
  mutate(rain_type = "all")

df_cume_stage_norain <- df %>%
  mutate(flow_type = ifelse(inflow_m3.d > 0, "Inflow", "Outflow")) %>%
  dplyr::filter(rain == 0) %>%
  group_by(site, flow_type) %>%
  arrange(stage_m) %>%
  dplyr::filter(!(is.na(inflow_m3.d))) %>%
  nest() %>%
  mutate(rain_type = "no rain")

df_cume_stage <- bind_rows(df_cume_stage_all, df_cume_stage_norain) %>%
  mutate(cume_flow = map(data, 
                         ~cumsum(.$inflow_m3.d))) %>%
  unnest()

# Determine ecdf of stage by site
df_ecdf <- df %>%
  group_by(site) %>%
  arrange(stage_m) %>%
  mutate(cumestage = cume_dist(stage_m))

# Quick calculation of cumulative flows
cume_stages <- df_cume_stage %>%
  group_by(site, flow_type) %>%
  summarize(maxcume = max(abs(cume_flow)))

cume_stages <- bp2 %>%
  dplyr::filter(term == "breakpoint", in_type == "Volumetric") %>%
  dplyr::select(site, estimate) %>%
  right_join(df_cume_stage) %>%
  group_by(site, flow_type) %>%
  summarize(cume_flow_bp = abs(nth(cume_flow, which.min(abs(stage_m - estimate))))) %>%
  right_join(cume_stages) %>%
  mutate(percent_above = 1 - cume_flow_bp / maxcume)

cume_stages_summary <- cume_stages %>%
  ungroup() %>%
  group_by(flow_type) %>%
  summarize(avg = mean(percent_above),
            std = sd(percent_above))

# Vector of sites, arranged by graphing order
sites <- c("TR1", "TR2", "TR3", "RP1", "RP2")

# For loop to get plots for each site
for(i in 1:length(sites)){
s <- sites[i]

# Get breakpoint data for that site
bp_p <- as.numeric(bp2[bp2$site == s &
                         bp2$term == "breakpoint" &
                         bp2$in_type == "Volumetric", "estimate"])
ec_p <- df_ecdf[df_ecdf$site == s, ]
ec_p <- as.numeric(ec_p[which.min(abs(ec_p$stage_m - bp_p)), "cumestage"])
maxs_p <- max(df[df$site == s, "stage_m"])
mins_p <- min(df[df$site == s, "stage_m"])

# Plot for cumulative flow vs stage and stage exceedance
if(i < 4){
  lims = c(-50000, 30000)
  brks = seq(-50000, 25000, 25000)} else{
    lims = c(-25000, 25000)
    brks = seq(-20000, 20000, 10000)
  }

p1_top <- ggplot(data = dplyr::filter(df_cume_stage,
                                      site == s,
                                      rain_type == "all")) + 
  geom_line(aes(x = stage_m,
                y = cume_flow,
                colour = flow_type
                # linetype = rain_type
                )) + 
  theme_classic() + 
  scale_x_continuous(limits = c(0, maxs_p),
                     expand = c(0 ,0)) +
  scale_y_continuous(limits = lims,
                     breaks = brks,
                     label = comma) +
  ylab(expression("Cumulative flow ("*m^3*")")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = bp_p,
             linetype = "dotted") +
  theme(legend.position = "none",
        legend.background = element_rect(
          colour = "black",
          fill = "gray90"),
        legend.margin = margin(1, 1.5, 1.5, 1.5),
        legend.key = element_rect(fill = "white", 
                                  colour = "black"),
        legend.title = element_text(face = "bold"),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank()) +
  scale_colour_manual(name = "Flow Direction",
                      breaks = c("Inflow",
                                 "Outflow"),
                      labels = c("Inflow",
                                 "Outflow"),
                      values = c("black",
                                 "gray"),
                      guide = guide_legend(ncol = 1)) 
  # scale_linetype_manual(name = "Rain Days Included?",
  #                       breaks = c("all", "no rain"),
  #                       labels = c("Yes",
  #                                  "No"),
  #                       values = c("solid",
  #                                  "dashed"))

# Plot for stage exceedance
p1_bot <- ggplot(data = subset(df_ecdf,
                                   site == s)) + 
  geom_line(aes(x = stage_m,
                y = cumestage)) + 
  theme_classic() + 
  scale_y_continuous(limits = c(0, 1),
                     breaks = c(0, round(ec_p, 2), 1),
                     expand = c(0, 0)) +
  scale_x_continuous(limits = c(0, maxs_p),
                     expand = c(0, 0)) +
  xlab("Stage (m)") +
  ylab("P(S < s)") +
  geom_vline(xintercept = bp_p,
             linetype = "dotted") +
  geom_segment(aes(x = 0, xend = bp_p,
                              y = ec_p, yend = ec_p),
             colour = "dark grey") + 
  theme()

# Get the combined plot for the site
if(i == 1 || i == 2 || i == 3){
  p1_bot = p1_bot + rremove("xlab")}
if(i == 2 || i == 3 || i == 5){
  p1_top = p1_top + rremove("ylab")
  p1_bot = p1_bot + rremove("ylab")}
p1_s <- ggarrange(p1_top,
                  p1_bot,
                  heights = 2:1,
                  ncol = 1, nrow = 2, 
                  # common.legend = TRUE,
                  # legend = "right",
                  align = "v")

# Name the plot according to site
assign(paste0("p1_", s), p1_s)
}

# Plot for legend
p1_legend <- ggplot(data = df_cume_stage,
                    aes(x = stage_m,
                        y = cume_flow,
                        colour = flow_type
                        # linetype = rain_type
                        )) + 
  geom_line() + 
  theme(legend.position = c(0.2, 0.5),
        legend.background = element_rect(
          colour = "black",
          fill = "gray90",
          linetype = "solid"),
        # legend.margin = margin(1, 1.5, 1.5, 1.5),
        legend.key = element_rect(fill = "white", 
                                  colour = "black",
                                  linetype = "solid"),
        legend.title = element_text(face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()) + 
  scale_colour_manual(name = "Flow Direction",
                      breaks = c("Inflow",
                                 "Outflow"),
                      labels = c("Inflow",
                                 "Outflow"),
                      values = c("black",
                                 "gray"),
                      guide = guide_legend(ncol = 1))  
  # scale_linetype_manual(name = "Rain Days Included?",
  #                       breaks = c("all", "no rain"),
  #                       labels = c("Yes",
  #                                  "No"),
  #                       values = c("solid",
  #                                  "dashed"))
p1_legend

legend <- cowplot::get_legend(p1_legend)

# Plot all combined plots on one figure
p1_all <- ggarrange(p1_TR1, 
                    p1_TR2, 
                    p1_TR3, 
                    p1_RP1, 
                    p1_RP2,
                    legend,
                    ncol = 3, nrow = 2,
                    labels = c("TR1", "TR2", "TR3", "RP1", "RP2"),
                    label.x = c(0.35, 0.2, 0.2, 0.35, 0.2),
                    label.y = 1,
                    heights = c(1, 1))
p1_all

ggsave(plot = p1_all, "cumulative_flow_vs_stage_pooled_ecdf_legend2_alldays.tiff",
       device = "tiff",
       width = 8,
       height = 6,
       units = "in",
       dpi = 600)

# Add info for above breakpoint, above ground surface
# and if it's inflow or outflow
df2 <- bp2 %>%
  dplyr::filter(term == "breakpoint",
                in_type == "Volumetric") %>%
  dplyr::select(site, estimate) %>%
  right_join(df) %>%
  mutate(above_bp = ifelse(stage_m >= estimate,
                           1,
                           0)) %>%
  mutate(above_g = ifelse(stage_m >= 0,
                          1,
                          0)) %>%
  mutate(flow_type = ifelse(inflow_m3.d > 0,
                            "in",
                            "out"))

# Calculate average annual inflow and outflow under two scenarios:
# 1) days below the breakpoint (AKA GW),
# and 2) days above the breakpoint (AKA GW + SW)
# Get data in list format for purrr
df.a_l <- df2 %>%
  group_by(site, wy, flow_type, above_bp) %>%
  arrange(date) %>%
  nest() %>%
  na.omit()

# Function to get annual averages
sum_fun <- function(data){
  data %>%
    summarize(annual_sum = sum(inflow_m3.d))
}

# Apply function to data
df_avg <- df.a_l %>%
  transmute(site, wy, flow_type, above_bp,
            ann = map(data, 
                       sum_fun)) %>%
  unnest() %>%
  mutate(type = ifelse(above_bp == 1, 
                       "SW+GW",
                       "GW"))

# Calculate percent of time for each flow regime
df.flowtime_l <- df2 %>%
  mutate(type = ifelse(above_bp == 1, 
                       "SW+GW",
                       "GW")) %>%
  group_by(site, wy, type) %>%
  arrange(date) %>%
  nest() %>%
  transmute(site, wy, type,
            flow_time = map(data, 
                            ~nrow(.))) %>%
  unnest()

df.flowtime <- df.flowtime_l %>%
  group_by(site, wy) %>%
  summarize(tot = sum(flow_time)) %>%
  right_join(df.flowtime_l) %>%
  mutate(frac_flow = flow_time / tot)

df.flowtime2 <- df.flowtime_l %>%
  group_by(site, type) %>%
  summarize(tot = sum(flow_time))

df.flowtime2 <- df.flowtime2 %>%
  group_by(site) %>%
  summarize(total = sum(tot)) %>%
  right_join(df.flowtime2) %>%
  mutate(frac_flow = tot / total)
  
# Order the groups for faceting in graph
df_avg$type <- factor(df_avg$type, 
                      levels = c("GW", "SW+GW"))
df.flowtime2$type <- factor(df.flowtime2$type, 
                            levels = c("GW", "SW+GW"))
df_avg$site <- factor(df_avg$site, 
                      levels = c("TR1", "TR2", "TR3", "RP1", "RP2"))

# Plot data
p2_top <- ggplot(data = dplyr::filter(df_avg,
                                      site != "RP3"),
                  aes(x = site,
                      y = annual_sum,
                      fill = flow_type)) +
  geom_bar(stat = "summary", 
           fun.y = "mean",
           colour = "black") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.4) +
  xlab("Site") +
  ylab(expression("Net flow ("*m^3~y^-1*")")) +
  scale_fill_manual(values = c("black",
                               "gray"),
                    guide = FALSE) + 
  theme_classic() +
  scale_y_continuous(breaks = seq(-20000, 8000, 4000),
                     label = comma) +
  theme(axis.text.x = element_text(angle = 90,
                                   hjust = 0.5),
        panel.grid.major.y = element_line(linetype = "dashed",
                                          color = "light grey")) + 
  facet_wrap(~type)

p2_top

# Reorder
df.flowtime2$site <- factor(df.flowtime2$site, 
                      levels = c("TR1", "TR2", "TR3", "RP1", "RP2"))
df.flowtime$site <- factor(df.flowtime$site, 
                            levels = c("TR1", "TR2", "TR3", "RP1", "RP2"))

# Bottom plot for percentage of time
p2_bot <- ggplot(data = dplyr::filter(df.flowtime,
                                      site != "RP3"),
                 aes(x = site,
                     y = frac_flow)) + 
  geom_bar(stat = "summary", 
           fun.y = "mean",
           colour = "black") +
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               width = 0.4) +
  xlab("Site") +
  ylab("Fraction of Flow Regime") +
  theme_classic() +
  scale_y_continuous(limits = c(0, 0.7),
                     expand = c(0, 0)) +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed",
                                          color = "light grey")) + 
  facet_wrap(~type)

p2_bot

p2 <- ggarrange(p2_top + rremove("xlab") + rremove("x.axis") +
                  rremove("x.text") + rremove("x.ticks"), 
                p2_bot,
                heights = 2:1,
                ncol = 1, nrow = 2, 
                # common.legend = TRUE,
                # legend = "right",
                align = "v")
p2

# Save plot
ggsave(plot = p2, 
       "annual_inflow_fraction_of_flow_yaxis_change2_alldays.tiff",
       device = "tiff",
       width = 8,
       height = 6,
       units = "in")
