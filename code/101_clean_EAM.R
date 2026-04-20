############################################################
# Project: Spring Paper 20260
# Author:  Ingri Katherine Quevedo Rocha
# Created: 2026-02-23
############################################################

############################
# Environment Setup
############################
#Set Project folder as the Working Directory
#setwd("..")
setwd("C:/Users/k_the/main/01_ECMIC_paper")
getwd()

# Clear workspace
rm(list = ls())

# Set options
set.seed(123)

#Libraries
pacman::p_load(dplyr,tidyverse, ggplot2, haven, ezids, arrow)
select <- dplyr::select

#######################################
# Open Full EAM COMBINE ALL YEARS
#######################################
years <- 2000:2019

eam_list <- vector("list", length(years))

for (i in seq_along(years)) {
  
  yr <- years[i]
  
  # File path
  file_path <- paste0("data/raw/EAM_full/EAM_", yr, ".csv")
  
  # Conditional separator
  sep_used <- ifelse(yr == 2003, ",", ";")
  
  # Read data
  df <- read.csv(file_path, header = TRUE, sep = sep_used)  
  
  # Make all variable names lowercase
  names(df) <- tolower(names(df))
  
  # Identify which ciiu variable exists
  ciiu_var <- intersect(c("ciiu","ciiu2", "ciiu3", "ciiu4", "ciiu_4"), names(df))
  
  if (length(ciiu_var) == 0) {
    stop(paste("No CIIU variable found in year", yr))
  }
  
  # Keep only needed variables
  vars_to_keep <- c(
    "nordemp", "nordest", "dpto", "periodo",
    "c4r4c1t", "c4r4c2t",
    "pertotal", "activfi", "salpeyte", "valorcom", "valorven",
    ciiu_var, "c4r1c9n","c4r1c10n","c4r1c9e","c4r1c10n","c4r5c1",
    "c4r5c2", "c4r5c3", "c4r5c4", "porcvt", "valorcx", "c2r1c7", 
    "c2r1c8"
  )
  
  # Keep only variables that actually exist in df
  vars_to_keep <- intersect(vars_to_keep, names(df))
  
  df_clean <- df %>%
    select(all_of(vars_to_keep)) %>%
    rename(any_of(c(
      id_firm     = "nordemp",
      id_plant    = "nordest",
      GEO         = "dpto",
      L           = "pertotal",
      K           = "activfi",
      WB          = "salpeyte",
      M           = "valorcom",
      GO          = "valorven",
      ciiu        = ciiu_var,
      proftec_wn  = "c4r1c9n",
      proftec_mn  = "c4r1c10n",
      proftec_wf  = "c4r1c9e",
      proftec_mf  = "c4r1c10e",
      prod_w      = "c4r5c1",
      prod_m      = "c4r5c2",
      admin_w     = "c4r5c3",
      admin_m     = "c4r5c4",
      exports     = "porcvt",
      imports     = "valorcx"
    ))) %>%
    mutate(
      year = yr,
      across(
        any_of(c("id_firm", "id_plant", "GEO", "ciiu", "periodo")),
        as.character
      ),
      across(
        any_of(c("L", "K", "WB", "M", "GO",
                 "proftec_wn", "proftec_mn", "proftec_wf", "proftec_mf",
                 "prod_w", "prod_m", "admin_w", "admin_m",
                 "exports", "imports")),
        ~ suppressWarnings(as.numeric(gsub("[^0-9\\.\\-]", "", as.character(.x))))
      ),
      lproftec = rowSums(across(any_of(c("proftec_wn","proftec_mn","proftec_wf","proftec_mf"))), na.rm = TRUE),
      lprod    = rowSums(across(any_of(c("prod_w","prod_m"))), na.rm = TRUE),
      ladmin   = rowSums(across(any_of(c("admin_w","admin_m"))), na.rm = TRUE),
      lw       = rowSums(across(any_of(c("proftec_wn","proftec_wf","prod_w","admin_w"))), na.rm = TRUE),
      lm       = rowSums(across(any_of(c("proftec_mn","proftec_mf","prod_m","admin_m"))), na.rm = TRUE),
      plants   = 1
    )
  
  # --- Safely coerce ownership vars to numeric, create as 0 if absent ---
  own_w_vars <- c("c4r4c1t", "c2r1c7")
  own_m_vars <- c("c4r4c2t", "c2r1c8")
  
  for (v in c(own_w_vars, own_m_vars)) {
    if (v %in% names(df_clean)) {
      df_clean[[v]] <- suppressWarnings(
        as.numeric(gsub("[^0-9\\.\\-]", "", as.character(df_clean[[v]])))
      )
    } else {
      df_clean[[v]] <- 0L
    }
  }
  
  # --- Create own_w and own_m, then drop components ---
  df_clean <- df_clean %>%
    mutate(
      own_w = rowSums(across(all_of(own_w_vars)), na.rm = TRUE),
      own_m = rowSums(across(all_of(own_m_vars)), na.rm = TRUE),
      own_tot = own_w + own_m,
      own_zero = as.integer(ifelse(own_tot == 0, 1, 0))
    ) %>%
    select(-all_of(c(own_w_vars, own_m_vars)))
  
  eam_list[[i]] <- df_clean
}

eam_all <- bind_rows(eam_list)
rm(eam_list, df, df_clean)

own_zero_summary <- eam_all %>%
  group_by(own_zero) %>%
  summarise(
    n_obs = n(),
    obs_share = 100 * n() / nrow(eam_all),
    total_GO = sum(GO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    GO_share = 100 * total_GO / sum(total_GO, na.rm = TRUE)
  )

own_zero_summary

#######################################
# Aggregate from plant to firm level
#######################################
# Variables to sum across plants within firm-year
vars_to_sum <- c("own_w", "own_m", "own_tot","L", "K", "WB", "M", "GO", "lproftec", "lprod", "ladmin", "lw", "lm", "exports" , "imports", "plants")

# 1. Get GEO and ciiu from the plant with the largest GO in each firm-year
firm_geo_ciiu <- eam_all %>%
  group_by(id_firm, year) %>%
  slice_max(order_by = GO, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(id_firm, year, GEO, ciiu)

# 2. Aggregate summed variables to the firm-year level
firm_agg <- eam_all %>%
  group_by(id_firm, year) %>%
  summarise(
    across(all_of(vars_to_sum), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  left_join(firm_geo_ciiu, by = c("id_firm", "year"))

#######################################
# CREATE WOMEN OWNER SHARE
#######################################

firm_agg <- firm_agg %>%
  select(id_firm, year, GEO, ciiu, all_of(vars_to_sum)) %>%
  mutate(
    own_w_share = if_else(own_tot == 0, 0, 100 * own_w / own_tot)
  )

# View result
str(firm_agg)

rm(eam_all,firm_geo_ciiu)
#######################################
# BIG CLEAN: drop obs where own=0, L=0, or GO=0
#######################################

firm_agg <- firm_agg %>%
  mutate(
    L_zero   = as.integer(L == 0),
    GO_zero  = as.integer(GO == 0),
    any_zero = as.integer(L_zero == 1 | GO_zero == 1)
  )

to_drop <- firm_agg %>%
  group_by(any_zero) %>%
  summarise(
    n_obs = n(),
    obs_share = 100 * n_obs / nrow(firm_agg),
    
    total_GO = sum(GO, na.rm = TRUE),
    total_L  = sum(L, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    GO_share = 100 * total_GO / sum(total_GO, na.rm = TRUE),
    L_share  = 100 * total_L  / sum(total_L, na.rm = TRUE)
  )

to_drop

eam_f <- firm_agg %>%
  filter(any_zero == 0) %>%
  select(-any_zero)

rm(eam_all, own_zero_summary, own_zero_summary_year)

#######################################
# CLEAN UP CIIU VARIABLE 
#######################################

# Bring correlation CIIU to ISIC4
ciiuisic4 <- read_dta("data/input/ciiuCOL_to_isic4.dta")

eam_f <- eam_f %>%
  inner_join(ciiuisic4, by = c("year", "ciiu"))

rm(ciiuisic4)
#######################################
# BRING USER COST OF CAPITAL 
# r=(nomilar interest rate - inflation) + depreciation
#######################################

macrovars <- read_dta("data/input/macro_variables.dta")

eam_f <- eam_f %>%
  inner_join(macrovars, by = "year") %>%
  mutate(KEXP = K*rK_DLEU) %>%
  select(-rK)

rm(macrovars)

#######################################
# SAVE WORK DATASET
#######################################
write_parquet(eam_f, "data/proc/EAM.parquet")

#Clean-up
rm(list = ls())
