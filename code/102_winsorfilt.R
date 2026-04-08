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


# ============================================================
# PARAMETERS (set these to match your Stata globals)
# ============================================================
do_winsor_rawvars       <- 1        # 1 = trim, 2 = winsorize
rawvars_low             <- 1        # lower percentile cut
rawvars_upp             <- 99       # upper percentile cut
do_Yehetal_m_l_tc       <- 1        # 1 = apply median filter
m_tc_cut                <- 0.25     # fraction of median threshold for m_tc
l_tc_cut                <- 0.25     # fraction of median threshold for l_tc
min_firms               <- 10       # minimum firms per sector-year
min_consecutive_years   <- 5        # minimum consecutive years threshold
periodicity             <- 1        # 1 = consecutive-years logic, else per-year
sector_var              <- "isic4" # name of your sector column (e.g. "sector2")

# ============================================================
# HELPER: Winsorize / Trim a vector
# ============================================================
winsorize_vec <- function(x, low_pct, upp_pct, trim = FALSE) {
  lo <- quantile(x, low_pct / 100, na.rm = TRUE)
  hi <- quantile(x, upp_pct / 100, na.rm = TRUE)
  if (trim) {
    x[x < lo | x > hi] <- NA   # Stata "trim": sets to NA (drops)
  } else {
    x <- pmin(pmax(x, lo), hi) # Stata default: clamps to bounds
  }
  x
}

# ============================================================
# 1. Winsorize / Trim raw variables  (by year x sector)
# ============================================================
vars_to_winsor <- c("GO", "M", "KEXP", "K", "L", "WB")

if (do_winsor_rawvars %in% c(1, 2)) {
  trim_flag <- (do_winsor_rawvars == 1)
  df <- df %>%
    group_by(year, .data[[sector_var]]) %>%
    mutate(across(
      all_of(vars_to_winsor),
      ~ winsorize_vec(.x, rawvars_low, rawvars_upp, trim = trim_flag)
    )) %>%
    ungroup()
}

# ============================================================
# 2. Yeh et al. — Drop firms in the bottom quarter of m_tc / l_tc
#    relative to their sector-year median
# ============================================================
df <- df %>%
  mutate(
    TC   = WB + M + KEXP,
    l_tc = WB / TC,
    m_tc = M  / TC
  )

if (do_Yehetal_m_l_tc == 1) {
  df <- df %>%
    group_by(year, .data[[sector_var]]) %>%
    mutate(
      m_tc_median = median(m_tc, na.rm = TRUE),
      l_tc_median = median(l_tc, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(
      m_tc >= m_tc_cut * m_tc_median,
      l_tc >= l_tc_cut * l_tc_median
    )
}

# Drop auxiliary columns
df <- df %>% select(-TC, -l_tc, -m_tc, -matches("_median"), -matches("_tc"))

# ============================================================
# 3. Sector must have >= min_firms for >= min_consecutive_years
# ============================================================
if (periodicity == 1) {
  
  # --- Build sector-year firm counts ---
  sector_year_counts <- df %>%
    group_by(.data[[sector_var]], year) %>%
    summarise(firm_count = n(), .groups = "drop") %>%
    mutate(threshold_met = firm_count >= min_firms)
  
  # --- Count max consecutive years meeting threshold, per sector ---
  max_consec <- sector_year_counts %>%
    arrange(.data[[sector_var]], year) %>%
    group_by(.data[[sector_var]]) %>%
    summarise(
      max_consec_years = {
        # Run-length encoding on threshold_met
        rle_result  <- rle(threshold_met)
        qualifying  <- rle_result$lengths[rle_result$values == TRUE]
        if (length(qualifying) == 0) 0L else max(qualifying)
      },
      .groups = "drop"
    )
  
  # --- Filter sectors that meet the consecutive-years threshold ---
  qualifying_sectors <- max_consec %>%
    filter(max_consec_years >= min_consecutive_years) %>%
    pull(.data[[sector_var]])
  
  df <- df %>%
    filter(.data[[sector_var]] %in% qualifying_sectors)
  
} else {
  
  # --- Simpler: require min_firms in EVERY year (no consecutive logic) ---
  sector_year_counts <- df %>%
    group_by(.data[[sector_var]], year) %>%
    summarise(firm_count = n(), .groups = "drop")
  
  df <- df %>%
    left_join(sector_year_counts, by = c(sector_var, "year")) %>%
    filter(firm_count >= min_firms) %>%
    select(-firm_count)
  
}

# ============================================================
# Quick diagnostic tabs
# ============================================================
print(table(df[[sector_var]], df$year))
cat("Distinct sectors remaining:", n_distinct(df[[sector_var]]), "\n")

write_parquet(df, "data/proc/work_EAM_filt.parquet")

#######################################
# HOUSEKEEPING
#######################################
rm(list = ls())
