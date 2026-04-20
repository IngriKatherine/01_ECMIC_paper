#######################################
# CALCULATE MARKDOWNS
#######################################

#######################################
# Open EAM and BRING ELASTICITIES
#######################################

### EAM ALL
df <- read_parquet("data/proc/EAM.parquet")

### ELASTICITIES
e <- read_parquet("data/proc/elasticities_DLW_ISIC4.parquet")

#######################################
# Merge EAM with Elasticities
#######################################
final_df <- df %>%
  left_join(e, by = c("id_firm", "year", "isic4"))

#######################################
# Input expenditure shares
#######################################

cat("Input expenditure shares\n")

final_df <- final_df %>%
  mutate(
    # Capital input share
    alpha_k = KEXP / GO,
    
    # Labor input share
    alpha_l = WB / GO,
    
    # Intermediate input share
    alpha_m = M / GO
  )

#######################################
# Markups and Markdowns - Computation
#######################################

cat("Markups and Markdowns - Computation\n")

final_df <- final_df %>%
  mutate(
    # Markup calculation
    mu_m = beta_m_DLW / alpha_m,
    
    # Markdown calculation
    nu_l = (beta_l_DLW / alpha_l) * (1 / mu_m),
    
  )

#######################################
# IMPUTE nu_l — Forward/Backward Fill
# then ISIC-4 Median Fallback
#######################################

cat("Imputing nu_l — fill within firm, then ISIC-4 median fallback\n")

final_df <- final_df %>%
  arrange(id_firm, year) %>%
  group_by(id_firm) %>%
  mutate(
    # Step 1: Within-firm fill
    # Forward-fill (carry last observed value forward → fills trailing NAs)
    nu_l_filled = zoo::na.locf(nu_l, na.rm = FALSE),
    # Backward-fill (carry first observed value backward → fills leading NAs)
    nu_l_filled = zoo::na.locf(nu_l_filled, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  ungroup()

# Step 2: ISIC-4 median fallback for firms with NO nu_l in any year
isic4_median <- final_df %>%
  group_by(isic4) %>%
  summarise(
    nu_l_isic4_median = median(nu_l, na.rm = TRUE),
    .groups = "drop"
  )

final_df <- final_df %>%
  left_join(isic4_median, by = "isic4") %>%
  mutate(
    nu_l_imputed = case_when(
      !is.na(nu_l_filled)       ~ nu_l_filled,          # firm has at least one obs
      !is.na(nu_l_isic4_median) ~ nu_l_isic4_median,    # sector median fallback
      TRUE                      ~ NA_real_               # sector also empty (edge case)
    ),
    # Flag what was done
    nu_l_impute_method = case_when(
      !is.na(nu_l)              ~ "observed",
      !is.na(nu_l_filled)       ~ "firm_fill",           # leading/trailing filled
      !is.na(nu_l_isic4_median) ~ "isic4_median",
      TRUE                      ~ "missing"
    )
  )

# Diagnostics
cat("\n--- nu_l imputation summary ---\n")
final_df %>%
  count(nu_l_impute_method) %>%
  mutate(pct = round(100 * n / nrow(final_df), 1)) %>%
  arrange(desc(n)) %>%
  print()

cat("  Still missing after imputation:",
    sum(is.na(final_df$nu_l_imputed)), "\n")


#######################################
# SAVE FINAL
#######################################
### ADDITIONAL VARIABLES
final_df <- final_df %>%
  arrange(id_firm, year) %>%      # make sure data is sorted
  group_by(id_firm) %>%
  mutate(firm_age = row_number()) %>%
  ungroup() %>%
  mutate(female_dummy = ifelse(own_w_share > 0, 1, 0),
         w=WB/L)

write_parquet(final_df, "data/proc/work_EAM.parquet")

#Clean-up
rm(list = ls())