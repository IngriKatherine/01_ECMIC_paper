############################################################
# Project: Spring Paper 20260
# Author:  Ingri Katherine Quevedo Rocha
# Created: 2026-02-23
############################################################

############################
# Environment Setup
############################
# Set Project folder as the Working Directory
setwd("..")
getwd()

# Clear workspace
rm(list = ls())

# Set options
set.seed(123)

# Libraries
pacman::p_load(dplyr, tidyverse, ggplot2, haven, ezids, arrow, scales, tidyr, knitr, kableExtra, showtext, sysfonts)
font_add("cmunsi", "C:/Users/k_the/Downloads/computer-modern/cmunss.ttf")
showtext_auto()

#######################################
# Open EAM and BRING ELASTICITIES
#######################################

### EAM ALL
df <- read_parquet("data/proc/work_EAM.parquet")

#######################################
# Descriptive table
#######################################

df$w <- df$WB/df$L
df <- df %>% filter(!is.na(nu_l_inv))
df <- df %>% filter(L!=0)
df <- df %>% filter(GO!=0)
df <- df %>% filter(w!=0)
df <- df %>%mutate(female_dummy = ifelse(own_w_share > 0, 1, 0),)
df <- df %>% filter(L>=10)
df <- df %>%
  arrange(id_firm, year) %>%      # make sure data is sorted
  group_by(id_firm) %>%
  mutate(firm_age = row_number()) %>%
  ungroup()

summary(df$nu_l_inv)
summary(df$nu_l_inv[df$nu_l_inv < 5], rm.na=TRUE)
sum(df$nu_l_inv[df$nu_l_inv < 5], rm.na=TRUE)

# ============================================================
# SECTION 1: Overall
# ============================================================
overall <- data.frame(
  Variable = c(
    "Observations",
    "Years",
    "Firms",
    "Geographies",
    "Industries"
  ),
  Value = c(
    formatC(nrow(df),                        format = "d", big.mark = ","),
    paste0(min(df$year), "\u2013", max(df$year)),
    formatC(n_distinct(df$id_firm),          format = "d", big.mark = ","),
    formatC(n_distinct(df$GEO),              format = "d", big.mark = ","),
    formatC(n_distinct(df$isic4),            format = "d", big.mark = ",")
  ),
  stringsAsFactors = FALSE
)

overall

# ============================================================
# SECTION 2: By Firm
# ============================================================
firm_vars <- list(
  own_tot     = "Total owners (N)",
  own_w_share = "Female ownership share",
  female_dummy = "Dummy Female",
  L           = "Employees (N)",
  w           = "Wage",
  GO          = "Gross output",
  firm_age    = "Firm Age",
  nu_l_imputed = "Markdown"
)

sum_row <- function(data, var, label) {
  x <- data[[var]]
  x <- x[!is.na(x)]          # remove NA only, keep Inf
  data.frame(
    Variable = label,
    Mean     = mean(x),
    Min      = min(x),
    Max      = max(x),
    SD       = sd(x),
    stringsAsFactors = FALSE
  )
}

firm_stats <- bind_rows(
  lapply(names(firm_vars), function(v) sum_row(df, v, firm_vars[[v]]))
)

fmt <- function(x, digits = 3) formatC(as.numeric(x), format = "f", digits = digits, big.mark = ",")

firm_stats[, c("Mean","Min","Max","SD")] <-
  lapply(firm_stats[, c("Mean","Min","Max","SD")], fmt)

firm_stats
# ============================================================
# SECTION 3: By Firm - Estimation Parameters (winsorized)
# ============================================================
est_vars <- list(
  beta_l_DLW   = "Output elasticity of labor",
  beta_k_DLW   = "Output elasticity of capital",
  beta_m_DLW   = "Output elasticity of materials",
  tfp          = "Total Factor Productivity (TFP)",
  alpha_l      = "Labor output share",
  alpha_m      = "Materials output share",
  alpha_k      = "Capital output share",
  mu_m         = "Markup",
  nu_l_imputed = "Markdown",
  nu_l_inv     = "Inverse markdown"
)

# Section 3
est_stats <- map_dfr(names(est_vars), function(v) {
  x <- df[[v]]
  tibble(
    variable = v,
    label    = est_vars[[v]],
    mean   = mean(x[x <2], na.rm = TRUE),
    p25      = quantile(x, 0.25, na.rm = TRUE),
    median   = median(x, na.rm = TRUE),
    p75      = quantile(x, 0.75, na.rm = TRUE),
  )
})

est_stats[, c("mean","p25","median","p75")] <-
  lapply(est_stats[, c("mean","p25","median","p75")], fmt)

est_stats

#######################################
# DISRIBUTION
#######################################

# Create dummy + clean data
df_dist <- df %>%
  mutate(
    female_dummy = ifelse(own_w_share > 0, 1, 0)) %>%
  filter(
    is.finite(nu_l_imputed), nu_l_imputed < 10
  )

# Distribution plot
g_dist<-ggplot(df_dist, aes(x = nu_l_imputed, fill = factor(female_dummy))) +
  geom_density(alpha = 0.5) +
  
  scale_fill_manual(
    values = c("#C44E52", "#187864"),
    labels = c("No Female Ownership", "Positive Female Ownership")
  ) +
  
  labs(
    x = "Markdown (Productivity-wage gap)",
    y = "Density",
    fill = ""
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 12),
    axis.text          = element_text(color = "black", size = 12),
    axis.title         = element_text(size = 14, face = "bold"),
    axis.line          = element_line(color = "black", linewidth = 0.5),
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("latex/distribution_nu_female_dummy.png",
       plot = g_dist, width = 8, height = 4, dpi = 150)



#######################################
# Correlation plot by industry
#######################################

df_scatter <- df %>%
  filter(
    is.finite(nu_l_imputed),
    is.finite(own_w_share)
  ) %>%
  group_by(id_firm) %>%
  summarise(
    nu_l_imputed = mean(nu_l_imputed, na.rm = TRUE),
    own_w_share = mean(own_w_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(nu_l_imputed < 10)

# Correlation
ct <- cor.test(df_scatter$own_w_share, df_scatter$nu_l_imputed)

lab <- paste0("Corr = ", round(ct$estimate, 3),
              "\nP-value = ", format.pval(ct$p.value, digits = 3, eps = 0.001))

# Scatter plot
g_scatter<-ggplot(df_scatter, aes(x = own_w_share, y = nu_l_imputed)) +
  geom_point(shape = 16, size = 1.5, alpha = 0.3, color = "#187864") +
  geom_smooth(method = "lm", se = TRUE, color = "#781830", linewidth = 1.5) +  
  labs(
    x = "Female Ownership Share",
    y = "Markdown (Productivity-wage gap)"
  ) +
  annotate("label", x = Inf, y = Inf, label = lab,
           hjust = 1.1, vjust = 1.5,
           size = 5,
           fill = "white",
           color = "black",
           linewidth = 0.5,
           alpha = 0.8,
           label.padding = unit(0.5, "lines")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "none",
    axis.text          = element_text(color = "black", size = 14),
    axis.title         = element_text(size = 14, face = "bold"),
    axis.line          = element_line(color = "black", linewidth = 0.5),
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )
g_scatter
ggsave("latex/scatter_markdown_ownwshare.png",
       plot = g_scatter, width = 8, height = 4, dpi = 150)


#######################################
# Median inverse markdown overtime here
#######################################

# Ensure year is treated as a date (Jan 1 of each year)
df_plot <- df %>%
  group_by(year) %>%
  summarize(median_nu_l_inv = median(nu_l_inv[nu_l_inv < 2], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year_date = as.Date(paste0(year, "-01-01")))

# Your theme (unchanged)
ts_theme <- function() {
  list(
    scale_x_date(
      breaks       = seq(as.Date("2000-01-01"), as.Date("2019-01-01"), by = "1 year"),
      date_labels  = "%Y",
      expand       = expansion(mult = c(0.01, 0.01))
    ),
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.02))),
    theme_minimal(base_size = 14),
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 12),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text          = element_text(color = "black", size = 12),
      axis.title         = element_text(size = 14, face = "bold"),
      axis.line          = element_line(color = "black", linewidth = 0.5),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 12),
      plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle      = element_text(size = 12, hjust = 0.5, color = "grey40"),
      plot.caption       = element_text(size = 10, color = "grey50", hjust = 1)
    )
  )
}

# Plot
ggplot(df_plot, aes(x = year_date, y = median_nu_l_inv)) +
  geom_line(color = "#187864", linewidth = 1) +
  geom_point(color = "#187864", size = 2) +
  labs(
    x        = "Year",
    y        = "Take-Home Wage (in Cents) per Dollar Value Contributed
to Firms"
  ) +
  ts_theme()

ggsave("latex/median_nu_linv_overtime.png", plot = g1, width = 8, height = 4, dpi = 150)


#######################################
# Median markdown overtime and % female owner
#######################################

# Your theme (unchanged)
ts_theme <- function() {
  list(
    scale_x_date(
      breaks       = seq(as.Date("2000-01-01"), as.Date("2019-01-01"), by = "1 year"),
      date_labels  = "%Y",
      expand       = expansion(mult = c(0.01, 0.01))
    ),
    theme_minimal(base_size = 14),
    theme(
      axis.text.x        = element_text(angle = 45, hjust = 1, size = 12),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text          = element_text(color = "black", size = 12),
      axis.title         = element_text(size = 14, face = "bold"),
      axis.line          = element_line(color = "black", linewidth = 0.5),
      panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      legend.text        = element_text(size = 12),
      plot.title         = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle      = element_text(size = 12, hjust = 0.5, color = "grey40"),
      plot.caption       = element_text(size = 10, color = "grey50", hjust = 1)
    )
  )
}

df_plot <- df %>%
  filter(year != 2000) %>%
  group_by(year) %>%
  summarize(
    median_nu_l_inv = median(nu_l_inv[nu_l_inv < 2], na.rm = TRUE),
    mean_fem        = mean(own_w_share, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year_date = as.Date(paste0(year, "-01-01")),
    median_nu_l_inv = median_nu_l_inv * 100
  )

g2<-ggplot(df_plot, aes(x = year_date)) +
  geom_line(aes(y = median_nu_l_inv, color = "Median Inverse Markdown: Employees take home dollars per $100 contributed"), linewidth = 1) +
  geom_point(aes(y = median_nu_l_inv, color = "Median Inverse Markdown: Employees take home dollars per $100 contributed"), size = 2) +
  
  geom_line(aes(y = mean_fem, color = "Average Female Ownership Share %"), 
            linewidth = 1, linetype = "dashed") +
  geom_point(aes(y = mean_fem, color = "Average Female Ownership Share %"), size = 2) + 
  scale_color_manual(
    values = c("#187864", "#C44E52"),
    guide = guide_legend(ncol = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10)
  ) +
  labs(
    x = "Year",
    y = "",
    color = NULL
  ) +
  
  ts_theme()

ggsave("latex/median_nu_linv_overtime_femsh.png", plot = g2, width = 8, height = 4, dpi = 150)


#######################################
# DISRIBUTION
#######################################

# Create dummy + clean data
df_dist <- df %>%
  mutate(
    female_dummy = ifelse(own_w_share > 0, 1, 0),
    nu_l_inv = nu_l_inv * 100
  ) %>%
  filter(
    is.finite(nu_l_inv),
    nu_l_inv < 200
  )

# Distribution plot
g_dist <- ggplot(df_dist, aes(x = nu_l_inv, fill = factor(female_dummy))) +
  geom_density(alpha = 0.5) +
  
  scale_fill_manual(
    values = c("#C44E52", "#187864"),
    labels = c("No Female Ownership", "Positive Female Ownership")
  ) +
  
  labs(
    x = "Inverse Markdown \n (Employees take home dollars per $100 contributed)",
    y = "Density",
    fill = ""
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 12),
    axis.text          = element_text(color = "black", size = 12),
    axis.title         = element_text(size = 14, face = "bold"),
    axis.line          = element_line(color = "black", linewidth = 0.5),
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("latex/distribution_nu_linv_female_dummy.png",
       plot = g_dist, width = 8, height = 4, dpi = 150)

# Violin plot
g_violin <- ggplot(df_dist, aes(x = factor(female_dummy), y = nu_l_inv, fill = factor(female_dummy))) +
  geom_violin(trim = FALSE, alpha = 0.6, color = NA) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) + # show median and IQR
  scale_fill_manual(
    values = c("#C44E52", "#187864"),
    labels = c("No Female Ownership", "Positive Female Ownership")
  ) +
  labs(
    x = "",
    y = "Inverse Markdown (Employees take home dollars per $100 contributed)",
    fill = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.text     = element_text(size = 12),
    axis.text       = element_text(color = "black", size = 12),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.line       = element_line(color = "black", linewidth = 0.5),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# Save plot
ggsave("latex/violin_nu_linv_female_dummy.png",
       plot = g_violin, width = 6, height = 4, dpi = 150)

######### CORRELATION FIRM-LEVEL

df_scatter <- df %>%
  mutate(
    female_dummy = ifelse(own_w_share > 0, 1, 0),
    GO_mil = GO / 3670000
  ) %>%
  filter(
    is.finite(nu_l_inv),
    is.finite(own_w_share),
    nu_l_inv <= 2
  )

ct <- cor.test(df_scatter$own_w_share, df_scatter$nu_l_inv)
lab <- paste0("Corr = ", round(ct$estimate, 3),
              "\nP-value = ", format.pval(ct$p.value, digits = 3, eps = 0.001))

# Scatter plot
g_scatter<-ggplot(df_scatter, aes(x = nu_l_inv, y = own_w_share)) +
  geom_point(aes(size = GO_mil), alpha = 0.2, color = "#187864") +
  
  scale_size_continuous(range = c(0.5, 4)) +  # adjust as needed
  
  geom_smooth(method = "lm", se = TRUE, color = "#781830", linewidth = 1.5) +  
  
  labs(
    x = "Inverse Markdown",
    y = "Female Ownership Share",
    size = "Firm's Gross Output Millions USD"
  ) +
  
  annotate("label", x = Inf, y = Inf, label = lab,
           hjust = 1.1, vjust = 1.5,
           size = 4.5,
           fill = "white",      # background color
           color = "black",     # text color
           linewidth = 0.5,     # border thickness
           alpha = 0.8) +       # optional transparency
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position    = "bottom",
    legend.text        = element_text(size = 12),
    axis.text          = element_text(color = "black", size = 12),
    axis.title         = element_text(size = 14, face = "bold"),
    axis.line          = element_line(color = "black", linewidth = 0.5),
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ggsave("latex/scatter_nu_linv_ownwshare.png",
       plot = g_scatter, width = 8, height = 4, dpi = 150)


############## NOT USED!!!!!!!!!!!!!!!!!!!


#######################################
# Add graph of scatter plot median markdown 
# and the female ownership share by sector, 
# with size of points corresponding to the 
# gross output of the sector.
#######################################


library(dplyr)
library(ggplot2)

# Create ISIC2 variable and aggregate

df_sector <- df %>%
  mutate(
    isic2 = substr(as.character(isic4), 1, 2),   # first 2 digits
    nu_l_inv = nu_l_inv * 100                    # scale like before
  ) %>%
  filter(is.finite(nu_l_inv), is.finite(GO), nu_l_inv < 400, nu_l_inv>0) %>%
  group_by(isic2) %>%
  summarize(
    median_nu_l_inv = median(nu_l_inv, na.rm = TRUE),
    mean_fem       = mean(own_w_share, na.rm = TRUE),
    total_GO       = sum(GO, na.rm = TRUE)
  ) %>%
  mutate(total_GO=(total_GO/3670)/1000000)%>%
  ungroup()

# Scatter plot
ggplot(df_sector, aes(x = mean_fem, y = median_nu_l_inv, size = total_GO)) +
  geom_point(color = "#187864", alpha = 0.7) +
  geom_text(aes(label = isic2), vjust = -1.2, size = 3.5) + # label points by ISIC2
  scale_size_continuous(range = c(3, 12)) +
  labs(
    x = "Female Ownership Share",
    y = "Median Inverse Markdown (Employees take home dollars per $100 contributed)",
    size = "Gross Output",
    title = "Median Inverse Markdown vs Female Ownership by Sector (ISIC 2-digit)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text       = element_text(color = "black", size = 12),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.line       = element_line(color = "black", linewidth = 0.5),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.position = "bottom",
    legend.title    = element_text(size = 12),
    legend.text     = element_text(size = 12),
    plot.title      = element_text(size = 16, face = "bold", hjust = 0.5)
  )

# Save the plot
ggsave("latex/scatter_nu_linv_fem_ownership_sector.png",
       plot = g_sector, width = 8, height = 5, dpi = 150)









#######################################
# Plot distribution
#######################################


#### MARKDOWNS
sum(is.na(df$nu_l_inv))
summary(df$nu_l_inv)
pdat <- df %>% filter(!is.na(nu_l_inv))
summary(pdat$nu_l_inv)
pdat$nu_l_inv[pdat$nu_l_inv > 4] <- NA
summary(pdat$nu_l_inv)
pdat <- pdat %>% filter(!is.na(nu_l_inv))


# TOTAL Markdown distribution
ggplot(pdat, aes(nu_l_inv)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "#187864", color = "white", alpha = 0.1) +
  geom_density(color = "#187864", linewidth = 0.8) +
  labs(title = "Distribution of Inverse Markdown",
       x = "Share of marginal revenue product of labor that goes to workers",
       y = "Density") +
  theme_minimal(base_size = 16, base_family = "cmunsi") +
  theme(axis.text.x = element_text(hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"))

#### FEMALE OWNERSHIP
ggplot(pdat, aes(own_w_share)) +
  geom_histogram(aes(y = ..density..), bins = 40,
                 fill = "#187864", color = "white", alpha = 0.1) +
  geom_density(color = "#187864", linewidth = 0.8) +
  labs(title = "Distribution of Female Ownership Share",
       x = "Percentage (%)",
       y = "Density") +
  theme_minimal(base_size = 16, base_family = "cmunsi") +
  theme(axis.text.x = element_text(hjust = 1),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"))

# Step 1: compute density manually
dens <- density(pdat$own_w_share, bw = "nrd0")
dens_df <- data.frame(
  y    = dens$x,      # the % values
  dens = dens$y       # the density values
)
max_dens <- max(dens$y)   # to know the scale

median_val <- median(pdat$own_w_share, na.rm = TRUE)
mean_val <- mean(pdat$own_w_share, na.rm = TRUE)

# Step 2: plot
ggplot(dens_df, aes(y = y)) +
  geom_ribbon(
    aes(xmin = -dens, xmax = dens),
    fill = "#187864", color = "#187864", alpha = 0.2
  ) +
  geom_hline(
    yintercept = median_val,
    color = "#187864", linetype = "dashed", size = 1
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_x_continuous(
    breaks = pretty(c(-max(dens_df$dens), max(dens_df$dens))),
    labels = abs
  ) +
  labs(
    title = "Distribution of Female Ownership Share",
    x = "Density",
    y = "Percentage (%)"
  ) +
  annotate( "text", x = -0.03, y = median_val,
            label = paste0("Median: ", scales::percent(median_val/100)),
            vjust = -1, size = 5
  ) +
  theme_minimal(base_size = 16, base_family = "cmunsi") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black")
  )


######### CORRELATION SECTOR-LEVEL WEIGHTED
pdat_sector <- pdat %>%
  group_by(isic4, year) %>%
  summarise(
    own_w_share = weighted.mean(own_w_share, w = GO, na.rm = TRUE),
    nu_l_inv    = weighted.mean(nu_l_inv,    w = GO, na.rm = TRUE),
    n_firms     = n(),
    .groups = "drop"
  ) %>%
  filter(is.finite(own_w_share), is.finite(nu_l_inv))

ct <- cor.test(pdat_sector$own_w_share, pdat_sector$nu_l_inv)

lab <- paste0("Corr = ", round(ct$estimate, 3),
  "\nP-value = ", format.pval(ct$p.value, digits = 3, eps = 0.001))

# Plot
ggplot(pdat_sector, aes(own_w_share, nu_l_inv)) +
  geom_point(size = 2, alpha = 0.7, color = "#187864") +
  geom_smooth(method = "lm", se = TRUE, color = "#781830", linewidth = 1) +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    name = "Female Ownership (%)"
  ) +
  labs(y = "Markdown: marginal revenue product of labor / wage") +
  annotate("text", x = Inf, y = 3.8, label = lab,
           hjust = 1.1, vjust = 1.5, size = 4.5) +
  theme_minimal(base_size = 16, base_family = "cmunsi") +
  theme(
    axis.text.x = element_text(hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(color = "black"),
    axis.line = element_line(color = "black")
  )

#######################################
# BASIC STATS
#######################################

sample_stats <- tibble(
  Statistic = c(
    "Firm-year observations",
    "Unique firms",
    "Sectors",
    "Regions",
    "Years",
    "First year",
    "Last year"
  ),
  Value = c(
    nrow(df),
    n_distinct(df$id_firm),
    n_distinct(df$isic4),
    n_distinct(df$GEO),
    n_distinct(df$year),
    min(df$year, na.rm = TRUE),
    max(df$year, na.rm = TRUE)
  )
)

kable(sample_stats, digits = 0, caption = "Sample Composition") %>%
  kable_styling(full_width = FALSE)

#---------------------------------------------
# 1. Descriptive statistics for main variables
#---------------------------------------------
desc_vars <- c("L", "K", "WB", "M", "GO", "own_w_share")

desc_table <- df %>%
  dplyr::select(dplyr::all_of(desc_vars)) %>%
  summarise(across(
    everything(),
    list(
      N = ~sum(!is.na(.)),
      Mean = ~mean(., na.rm = TRUE),
      SD = ~sd(., na.rm = TRUE),
      P25 = ~quantile(., 0.25, na.rm = TRUE),
      Median = ~median(., na.rm = TRUE),
      P75 = ~quantile(., 0.75, na.rm = TRUE),
      Min = ~min(., na.rm = TRUE),
      Max = ~max(., na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "^(.*)_(N|Mean|SD|P25|Median|P75|Min|Max)$"
  ) %>%
  mutate(
    Variable = dplyr::recode(
      Variable,
      "L" = "Employees",
      "K" = "Fixed assets",
      "WB" = "Wage bill",
      "M" = "Materials expenditure",
      "GO" = "Gross output",
      "own_w_share" = "Share of Female Ownership"
    )
  )

#---------------------------------------------
# 3. Print tables
#---------------------------------------------
kable(desc_table, digits = 2, caption = "Descriptive Statistics") %>%
  kable_styling(full_width = FALSE)


latex_table <- kable(
  desc_table,
  format = "latex",
  digits = 2,
  caption = "Descriptive Statistics",
  booktabs = TRUE
)

cat(latex_table)


#######################################
# HOUSEKEEPING
#######################################
rm(list = ls())
