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
# Years to load
years <- 2000:2019

# Empty list to store each cleaned dataset
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
    ciiu_var
  )
  ## 1) Propietarios, Socios y familiares sin remuneración 
  # c4r4c1t women
  # c4r4c2t men
  ## 2) KEY VARS
  # PERTOTAL (L)
  # ACTIVFI (K)
  # SALPEYTE (WB)
  # VALORCOM (M) "Valor de la materia prima comprada, Mill. COP"
  # VALORVEN (GO)
  # Valor agregado: es el total de los ingresos recibidos por el uso de los factores productivos
  #participantes en el proceso de producción. Se calcula como la diferencia entre producción
  #bruta y consumo intermedio. NVM - NOT NEEDED
  
  df_clean <- df %>%
    select(all_of(vars_to_keep)) %>%
    rename(
      id_firm = nordemp,
      id_plant = nordest,
      GEO = dpto,
      L = pertotal,
      K = activfi,
      WB = salpeyte,
      M = valorcom,
      GO = valorven,
      own_w = c4r4c1t,
      own_m = c4r4c2t,
      ciiu = all_of(ciiu_var)
    ) %>%
    mutate(
      year = yr,
      id_firm = as.character(id_firm),
      id_plant = as.character(id_plant),
      GEO = as.character(GEO),
      ciiu = as.character(ciiu),
      periodo = as.character(periodo),
      own_w = as.numeric(own_w),
      own_m = as.numeric(own_m),
      L = as.numeric(L),
      K = as.numeric(K),
      WB = as.numeric(WB),
      M = as.numeric(M),
      GO = as.numeric(GO)
    )
  eam_list[[i]] <- df_clean
}

eam_all <- bind_rows(eam_list)
rm(eam_list,df, df_clean)

#######################################
# Aggregate from plant to firm level
#######################################
# Variables to sum across plants within firm-year
vars_to_sum <- c("own_w", "own_m","L", "K", "WB", "M", "GO")

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

firm_agg <- firm_agg %>%
  select(id_firm, year, GEO, ciiu, all_of(vars_to_sum))

# View result
str(firm_agg)

rm(eam_all,firm_geo_ciiu)

#######################################
# CREATE WOMEN OWNER SHARE
#######################################

eam_all <- firm_agg %>%
  mutate(
    own_tot = own_w + own_m,
    own_w_share = if_else(own_tot == 0, 0, 100 * own_w / own_tot),
    own_zero = as.integer(own_tot == 0)
  )

### SUMMARY FIRMS WITHOUT OWNER

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

own_zero_summary_year <- eam_all %>%
  group_by(year, own_zero) %>%
  summarise(
    n_obs = n(),
    total_GO = sum(GO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    obs_share = 100 * n_obs / sum(n_obs),
    GO_share  = 100 * total_GO / sum(total_GO),
    group = if_else(own_zero == 1, "own_tot = 0", "own_tot > 0")
  ) %>%
  ungroup() %>%
  select(year, group, own_zero, n_obs, obs_share, total_GO, GO_share)

print(own_zero_summary_year  %>%
        filter(own_zero == 0), n=50)

mean(own_zero_summary_year$n_obs[own_zero_summary_year$own_zero == 0], na.rm = TRUE)

############## FILTER TO ONLY OBS WHERE TOT OWNER IS NOT ZERO

eam_f <- eam_all %>%
  filter(own_zero == 0) %>%
  select(-own_zero)

rm(eam_all, firm_agg, own_zero_summary, own_zero_summary_year)

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

eam_f <- df %>%
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
