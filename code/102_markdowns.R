################################################################################
# DLW Translog Elasticities — 4-Digit Sector Level
# Method:  De Loecker & Warzynski (2012) — Translog Production Function (GMM)
#
# TWO-TIER APPROACH per sector:
#
#  Tier 1 — DLW Translog GMM (10-parameter translog)
#            Full DLW with Nelder-Mead + Rvmmin fallback.
#            The Markov-projection step uses MASS::ginv() instead of solve()
#            so near-singular omega systems never crash the optimizer.
#
#  Tier 2 — ACF Cobb-Douglas GMM (4-parameter model)
#            Fewer parameters → far less likely to be singular.
#            Gives sector-constant (not firm-varying) elasticities,
#            but still GMM-identified from panel variation.
#
# Output column  elasticity_method  records which tier was used per obs:
#   "DLW_translog"  — Tier 1 (firm-varying, GMM translog)
#   "ACF_cd"        — Tier 2 (sector-constant, GMM Cobb-Douglas)
#   NA              — not covered by any tier
################################################################################

pacman::p_load(arrow, haven, tidyr, DescTools, optimx, MASS, dplyr, Hmisc)
# NOTE: dplyr is loaded LAST so dplyr::select masks MASS::select cleanly.

################################################################################
# SECTION 0 — USER SETTINGS
################################################################################
# setwd("..")
getwd()

df <- read_parquet("data/proc/EAM.parquet")

labor_input <- "L"
ID_col      <- "id_firm"
countryISO  <- "COL"
CountryName <- "Country"

rawvars_low <- 0.01
rawvars_upp <- 0.99
m_tc_cut    <- 0.25
l_tc_cut    <- 0.25

cat("Raw data:", nrow(df), "rows |",
    length(unique(df$isic4)), "ISIC-4 sectors\n")

################################################################################
# SECTION 0B — WINSORIZATION
################################################################################
cat("\n--- Winsorizing inputs before estimation ---\n")

# Variables to winsorize (in levels, before log transformation)
wins_vars <- c("GO", "K", "L", "M")

# Winsorize within ISIC-4 sector × year cells to respect
# sector-specific distributions; fall back to sector-level
# if a cell is too thin (< 10 obs)
df <- df %>%
  group_by(isic4, year) %>%
  mutate(cell_n = n()) %>%
  ungroup()

for (v in wins_vars) {
  if (!v %in% names(df)) {
    warning("Winsorization: variable '", v, "' not found — skipping.")
    next
  }
  
  # Cell-level winsorization where cell is large enough
  df <- df %>%
    group_by(isic4, year) %>%
    mutate(
      !!paste0(v, "_w") := if_else(
        cell_n >= 10,
        DescTools::Winsorize(!!sym(v),
                             quantile(!!sym(v),
                                      probs = c(rawvars_low, rawvars_upp),
                                      na.rm = TRUE)),
        !!sym(v)
      )
    ) %>%
    ungroup()
  
  # Sector-level fallback for thin cells
  df <- df %>%
    group_by(isic4) %>%
    mutate(
      !!paste0(v, "_w") := if_else(
        cell_n < 10,
        DescTools::Winsorize(!!sym(v),
                             quantile(!!sym(v),
                                      probs = c(rawvars_low, rawvars_upp),
                                      na.rm = TRUE)),
        !!sym(paste0(v, "_w"))
      )
    ) %>%
    ungroup()
  
  cat("  Winsorized:", v, "→", paste0(v, "_w"), "\n")
}

df <- df %>% dplyr::select(-cell_n)

# Swap original variables for winsorized versions so the rest
# of the pipeline is unchanged
for (v in wins_vars) {
  if (paste0(v, "_w") %in% names(df)) {
    df[[v]] <- df[[paste0(v, "_w")]]
    df[[paste0(v, "_w")]] <- NULL
  }
}

cat("Winsorization complete.\n")

################################################################################
# SECTION 1 — HELPER FUNCTIONS
################################################################################

# ---- Safe Markov-projection step --------------------------------------------
safe_project <- function(OMEGA_lag_pol, OMEGA) {
  A <- t(OMEGA_lag_pol) %*% OMEGA_lag_pol
  g_b <- tryCatch(
    solve(A) %*% t(OMEGA_lag_pol) %*% OMEGA,
    error = function(e) MASS::ginv(A) %*% t(OMEGA_lag_pol) %*% OMEGA
  )
  return(g_b)
}

# ---- GMM criterion: DLW Translog (10 parameters) ----------------------------
gmm_dlw_criterion <- function(betas, data_gmm) {
  PHI     <- data_gmm$phi_wt
  PHI_LAG <- data_gmm$phi_lag_wt
  C       <- data_gmm$const_wt
  
  X <- as.matrix(data_gmm[, c("const_wt",
                              "k1_wt","l1_wt","m1_wt",
                              "k2_wt","l2_wt","m2_wt",
                              "k1l1_wt","k1m1_wt","l1m1_wt")])
  X_lag <- as.matrix(data_gmm[, c("const_wt",
                                  "k1_lag_wt","l1_lag_wt","m1_lag_wt",
                                  "k2_lag_wt","l2_lag_wt","m2_lag_wt",
                                  "k1_lag_l1_lag_wt","k1_lag_m1_lag_wt",
                                  "l1_lag_m1_lag_wt")])
  Z <- as.matrix(data_gmm[, c("const_wt","k1_wt",
                              "l1_lag_wt","m1_lag_wt",
                              "k2_wt","l2_lag_wt","m2_lag_wt",
                              "k1l1_lag_wt","k1m1_lag_wt",
                              "l1_lag_m1_lag_wt")])
  
  OMEGA     <- PHI     - X     %*% betas
  OMEGA_lag <- PHI_LAG - X_lag %*% betas
  OMEGA_lag_pol <- cbind(C, OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3)
  
  g_b <- safe_project(OMEGA_lag_pol, OMEGA)
  XI  <- OMEGA - OMEGA_lag_pol %*% g_b
  ZXI <- t(Z) %*% XI
  return(as.numeric(t(ZXI) %*% ZXI))
}

# ---- GMM criterion: ACF Cobb-Douglas (4 parameters) -------------------------
gmm_acf_criterion <- function(betas, data_gmm) {
  PHI     <- data_gmm$phi_wt
  PHI_LAG <- data_gmm$phi_lag_wt
  C       <- data_gmm$const_wt
  
  X     <- as.matrix(data_gmm[, c("const_wt","k1_wt","l1_wt","m1_wt")])
  X_lag <- as.matrix(data_gmm[, c("const_wt","k1_lag_wt","l1_lag_wt","m1_lag_wt")])
  Z     <- as.matrix(data_gmm[, c("const_wt","k1_wt","l1_lag_wt","m1_lag_wt")])
  
  OMEGA     <- PHI     - X     %*% betas
  OMEGA_lag <- PHI_LAG - X_lag %*% betas
  OMEGA_lag_pol <- cbind(C, OMEGA_lag, OMEGA_lag^2, OMEGA_lag^3)
  
  g_b <- safe_project(OMEGA_lag_pol, OMEGA)
  XI  <- OMEGA - OMEGA_lag_pol %*% g_b
  ZXI <- t(Z) %*% XI
  return(as.numeric(t(ZXI) %*% ZXI))
}

# ---- Generic optimizer: Nelder-Mead with Rvmmin fallback --------------------
run_gmm <- function(criterion_fn, data_gmm, beta_guess) {
  np <- length(beta_guess)
  
  result_nm <- tryCatch(
    optimx(par = beta_guess, fn = criterion_fn, method = "Nelder-Mead",
           control = list(maxit = 10000, maximize = FALSE),
           data_gmm = data_gmm),
    error = function(e) { message("  NM error: ", conditionMessage(e)); NULL }
  )
  
  nm_ok <- !is.null(result_nm) && (result_nm$convcode[1] %in% c(0, 1))
  
  if (nm_ok) {
    betas_out <- unlist(result_nm[1, 1:np])
    if (result_nm$convcode[1] == 0)
      return(list(converged = TRUE, betas = betas_out))
    fallback_guess <- betas_out
  } else {
    fallback_guess <- beta_guess
  }
  
  result_rv <- tryCatch(
    optimx(par = fallback_guess, fn = criterion_fn, method = "Rvmmin",
           control = list(maxit = 10000, maximize = FALSE),
           data_gmm = data_gmm),
    error = function(e) { message("  Rvmmin error: ", conditionMessage(e)); NULL }
  )
  
  if (!is.null(result_rv) && result_rv$convcode[1] == 0)
    return(list(converged = TRUE, betas = unlist(result_rv[1, 1:np])))
  
  if (nm_ok)
    return(list(converged = FALSE, betas = unlist(result_nm[1, 1:np])))
  
  return(list(converged = FALSE, betas = rep(NA_real_, np)))
}

# ---- Build weighted columns -------------------------------------------------
add_wt_cols <- function(d, cols) {
  for (v in cols)
    if (v %in% names(d)) d[[paste0(v, "_wt")]] <- d[[v]]
  d
}

# ---- First-stage OLS and lag attachment -------------------------------------
run_first_stage <- function(ds, poly_cols, tdum_cols, ID_col, use_full_poly = TRUE) {
  
  if (use_full_poly) {
    pcols <- intersect(
      c(paste0("l",1:3), paste0("k",1:3), paste0("m",1:3),
        paste0("k1l",1:3), paste0("k2l",1:3), paste0("k3l",1:3),
        paste0("k1m",1:3), paste0("k2m",1:3), paste0("k3m",1:3),
        paste0("l1m",1:3), paste0("l2m",1:3), paste0("l3m",1:3)),
      names(ds)
    )
  } else {
    pcols <- intersect(
      c(paste0("l",1:2), paste0("k",1:2), paste0("m",1:2),
        "k1l1","k1m1","l1m1"),
      names(ds)
    )
  }
  
  fs <- tryCatch(
    lm(as.formula(paste("y ~", paste(c(pcols, tdum_cols), collapse = "+"))),
       data = ds),
    error = function(e) NULL
  )
  if (is.null(fs)) return(NULL)
  
  ds$phi     <- predict(fs, newdata = ds)
  ds$epsilon <- ds$y - ds$phi
  
  ds <- ds %>%
    arrange(!!sym(ID_col), year) %>%
    group_by(!!sym(ID_col)) %>%
    mutate(
      phi_lag = lag(phi),
      k1_lag  = lag(k1), k2_lag = lag(k2), k3_lag = lag(k3),
      l1_lag  = lag(l1), l2_lag = lag(l2), l3_lag = lag(l3),
      m1_lag  = lag(m1), m2_lag = lag(m2), m3_lag = lag(m3)
    ) %>%
    ungroup() %>%
    mutate(
      k1l1_lag      = k1     * l1_lag,
      k1m1_lag      = k1     * m1_lag,
      l1m1_lag      = l1     * m1_lag,
      k1_lag_l1_lag = k1_lag * l1_lag,
      k1_lag_m1_lag = k1_lag * m1_lag,
      l1_lag_m1_lag = l1_lag * m1_lag
    )
  
  return(ds)
}

################################################################################
# SECTION 4 — SECTOR-BY-SECTOR CASCADE ESTIMATION
################################################################################

sectors <- sort(unique(df$isic4))
cat("\n--- Cascade estimation across", length(sectors), "sectors ---\n")

sector_results <- list()

for (sec in sectors) {
  
  sec_key <- as.character(sec)
  n_raw   <- sum(df$isic4 == sec)
  cat("\n  Sector:", sec, "(raw n =", n_raw, ")\n")
  
  ds <- df %>%
    filter(isic4 == sec, GO > 0, K > 0, L > 0, M > 0)
  
  if (nrow(ds) < 5) {
    cat("    Fewer than 5 usable rows — skipping entirely\n")
    next
  }
  
  # ---- Log inputs & shared polynomial terms ----------------------------------
  ds <- ds %>%
    mutate(
      ln_GO = log(GO), ln_L = log(L), ln_K = log(K), ln_M = log(M),
      y = ln_GO, const = 1
    )
  
  year_levels <- sort(unique(ds$year))
  ds <- ds %>%
    mutate(year_fac = factor(year, levels = year_levels)) %>%
    bind_cols(
      model.matrix(~ year_fac - 1, data = .) %>%
        as.data.frame() %>%
        setNames(paste0("tdum_", year_levels))
    ) %>%
    dplyr::select(-year_fac)
  
  tdum_cols <- grep("^tdum_", names(ds), value = TRUE)
  
  for (i in 1:3) {
    ds[[paste0("k", i)]] <- ds$ln_K^i
    ds[[paste0("l", i)]] <- ds$ln_L^i
    ds[[paste0("m", i)]] <- ds$ln_M^i
    for (j in 1:3) {
      ds[[paste0("k", i, "l", j)]] <- ds$ln_K^i * ds$ln_L^j
      ds[[paste0("k", i, "m", j)]] <- ds$ln_K^i * ds$ln_M^j
      ds[[paste0("l", i, "m", j)]] <- ds$ln_L^i * ds$ln_M^j
    }
  }
  
  tl_formula <- as.formula(paste(
    "y ~ k1+l1+m1+k2+l2+m2+k1l1+k1m1+l1m1 +",
    paste(tdum_cols, collapse = "+")
  ))
  
  ds$beta_k_DLW        <- NA_real_
  ds$beta_l_DLW        <- NA_real_
  ds$beta_m_DLW        <- NA_real_
  ds$elasticity_method <- NA_character_
  
  needs_elast <- function(d)
    is.na(d$beta_k_DLW) | is.na(d$beta_l_DLW) | is.na(d$beta_m_DLW)
  
  # =========================================================================
  # TIER 1 — DLW Translog GMM
  # =========================================================================
  cat("    --- Tier 1: DLW Translog GMM ---\n")
  
  ols_tl <- tryCatch(lm(tl_formula, data = ds), error = function(e) NULL)
  
  if (!is.null(ols_tl)) {
    
    bg_dlw <- c(coef(ols_tl)["(Intercept)"],
                coef(ols_tl)[c("k1","l1","m1","k2","l2","m2","k1l1","k1m1","l1m1")])
    bg_dlw[is.na(bg_dlw)] <- 0
    
    ds_fs <- run_first_stage(ds, NULL, tdum_cols, ID_col, use_full_poly = TRUE)
    if (is.null(ds_fs))
      ds_fs <- run_first_stage(ds, NULL, tdum_cols, ID_col, use_full_poly = FALSE)
    
    if (!is.null(ds_fs)) {
      ds <- ds_fs
      
      ds$touse <- complete.cases(ds[, c("y","l1_lag","k1_lag","phi","phi_lag")])
      ds_gmm   <- ds %>% filter(touse)
      cat("    GMM obs:", nrow(ds_gmm), "/ sector obs:", nrow(ds), "\n")
      
      if (nrow(ds_gmm) >= 20) {
        
        ds_gmm <- add_wt_cols(ds_gmm,
                              c("const","k1","l1","m1","k2","l2","m2","k1l1","k1m1","l1m1",
                                "k1_lag","l1_lag","m1_lag","k2_lag","l2_lag","m2_lag",
                                "k1_lag_l1_lag","k1_lag_m1_lag","l1_lag_m1_lag",
                                "k1l1_lag","k1m1_lag","l1m1_lag","phi","phi_lag"))
        
        gmm1 <- run_gmm(gmm_dlw_criterion, ds_gmm, bg_dlw)
        
        if (gmm1$converged) {
          b <- gmm1$betas
          bk <- b[2] + 2*b[5]*ds$k1 + b[8]*ds$l1 + b[9]*ds$m1
          bl <- b[3] + 2*b[6]*ds$l1 + b[8]*ds$k1 + b[10]*ds$m1
          bm <- b[4] + 2*b[7]*ds$m1 + b[9]*ds$k1 + b[10]*ds$l1
          
          tier1_ok <- bk > 0 & bl > 0 & bm > 0 & !is.na(bk) & !is.na(bl) & !is.na(bm)
          ds$beta_k_DLW[tier1_ok]        <- bk[tier1_ok]
          ds$beta_l_DLW[tier1_ok]        <- bl[tier1_ok]
          ds$beta_m_DLW[tier1_ok]        <- bm[tier1_ok]
          ds$elasticity_method[tier1_ok] <- "DLW_translog"
          
          cat("    Tier 1: CONVERGED —",
              sum(tier1_ok), "obs filled,",
              sum(!tier1_ok), "non-positive → fall to next tier\n")
        } else {
          cat("    Tier 1: did not converge\n")
        }
      } else {
        cat("    Tier 1: too few GMM obs (<20)\n")
      }
    } else {
      cat("    Tier 1: first-stage regression failed\n")
    }
  } else {
    cat("    Tier 1: OLS initial-guess failed\n")
  }
  
  # =========================================================================
  # TIER 2 — ACF Cobb-Douglas GMM
  # =========================================================================
  n_need2 <- sum(needs_elast(ds))
  cat("    --- Tier 2: ACF Cobb-Douglas GMM (need:", n_need2, ") ---\n")
  
  if (n_need2 > 0) {
    
    if (!"phi_lag" %in% names(ds) || all(is.na(ds$phi_lag))) {
      ds_fs2 <- run_first_stage(ds, NULL, tdum_cols, ID_col, use_full_poly = FALSE)
      if (!is.null(ds_fs2)) ds <- ds_fs2
    }
    
    if ("phi_lag" %in% names(ds) && !all(is.na(ds$phi_lag))) {
      
      ols_cd <- tryCatch(
        lm(as.formula(paste("y ~ k1+l1+m1 +", paste(tdum_cols, collapse = "+"))),
           data = ds),
        error = function(e) NULL
      )
      bg_acf <- if (!is.null(ols_cd)) {
        v <- c(coef(ols_cd)["(Intercept)"], coef(ols_cd)[c("k1","l1","m1")])
        v[is.na(v)] <- 0.25; v
      } else rep(0.25, 4)
      
      ds$touse_acf <- complete.cases(ds[, c("y","l1_lag","k1_lag","phi","phi_lag")])
      ds_gmm2 <- ds %>% filter(touse_acf)
      cat("    ACF GMM obs:", nrow(ds_gmm2), "\n")
      
      if (nrow(ds_gmm2) >= 10) {
        
        ds_gmm2 <- add_wt_cols(ds_gmm2,
                               c("const","k1","l1","m1","k1_lag","l1_lag","m1_lag","phi","phi_lag"))
        
        gmm2 <- run_gmm(gmm_acf_criterion, ds_gmm2, bg_acf)
        
        if (gmm2$converged) {
          b2 <- gmm2$betas
          if (b2[2] > 0 && b2[3] > 0 && b2[4] > 0) {
            fill2 <- needs_elast(ds)
            ds$beta_k_DLW[fill2]        <- b2[2]
            ds$beta_l_DLW[fill2]        <- b2[3]
            ds$beta_m_DLW[fill2]        <- b2[4]
            ds$elasticity_method[fill2] <- "ACF_cd"
            cat("    Tier 2: CONVERGED —", sum(fill2), "obs filled\n")
          } else {
            cat("    Tier 2: converged but betas non-positive — skipping\n")
          }
        } else {
          cat("    Tier 2: did not converge\n")
        }
      } else {
        cat("    Tier 2: too few GMM obs (<10)\n")
      }
    } else {
      cat("    Tier 2: first-stage unavailable\n")
    }
  }
  
  # =========================================================================
  # TFP RECOVERY — firm-year productivity residual
  # =========================================================================
  # TFP (Solow residual style, using GMM elasticities):
  #   tfp = ln_GO − β_k·ln_K − β_l·ln_L − β_m·ln_M
  #
  # For DLW_translog obs: β's are firm-varying (already in beta_*_DLW cols)
  # For ACF_cd obs:       β's are sector-constant scalars from Tier 2
  # For missing obs:      TFP is left as NA
  #
  # We also store omega (first-stage productivity proxy) as a by-product.
  # -------------------------------------------------------------------------
  
  cat("    --- TFP Recovery ---\n")
  
  ds <- ds %>%
    mutate(
      # Structural TFP: output net of all inputs weighted by GMM elasticities
      tfp = case_when(
        !is.na(beta_k_DLW) & !is.na(beta_l_DLW) & !is.na(beta_m_DLW) ~
          ln_GO - beta_k_DLW * ln_K
        - beta_l_DLW * ln_L
        - beta_m_DLW * ln_M,
        TRUE ~ NA_real_
      ),
      
      # Partial TFP: capital + intermediates only (useful robustness check)
      tfp_km = case_when(
        !is.na(beta_k_DLW) & !is.na(beta_m_DLW) ~
          ln_GO - beta_k_DLW * ln_K - beta_m_DLW * ln_M,
        TRUE ~ NA_real_
      ),
      
      # First-stage productivity proxy (phi − predicted input bundle)
      # phi is already in ds from run_first_stage(); carry it through if present
      omega = if ("phi" %in% names(.)) phi else NA_real_
    )
  
  n_tfp <- sum(!is.na(ds$tfp))
  cat("    TFP computed for", n_tfp, "/", nrow(ds), "obs\n")
  
  # ---- Save sector results --------------------------------------------------
  sector_results[[sec_key]] <- ds %>%
    dplyr::select(any_of(c(
      "year", "isic4", ID_col,
      "beta_k_DLW", "beta_l_DLW", "beta_m_DLW",
      "elasticity_method",
      "tfp", "tfp_km", "omega",
      "ln_GO", "ln_K", "ln_L", "ln_M"   # keep logs for downstream use
    )))
}

################################################################################
# SECTION 5 — ASSEMBLE, SUMMARISE & SAVE
################################################################################

cat("\n--- Assembling final results ---\n")

sector_results_clean <- Filter(Negate(is.null), sector_results)
if (length(sector_results_clean) == 0)
  stop("No sectors produced results. Check your data and settings.")

final_df <- bind_rows(sector_results_clean) %>%
  arrange(isic4, year, !!sym(ID_col))

still_na <- sum(
  is.na(final_df$beta_k_DLW) |
    is.na(final_df$beta_l_DLW) |
    is.na(final_df$beta_m_DLW)
)

cat("\n========================================\n")
cat("COVERAGE SUMMARY (", nrow(final_df), "total obs ):\n")

method_summary <- final_df %>%
  mutate(elasticity_method = replace_na(elasticity_method, "(missing)")) %>%
  count(elasticity_method) %>%
  mutate(pct = round(100 * n / nrow(final_df), 1)) %>%
  arrange(desc(n))
print(method_summary)

cat("  Still missing (elasticities):", still_na, "\n")
cat("  TFP computed:", sum(!is.na(final_df$tfp)), "\n")
cat("  TFP missing: ", sum( is.na(final_df$tfp)), "\n")
cat("========================================\n")

cat("\nSector-level breakdown:\n")
sec_summary <- final_df %>%
  group_by(isic4) %>%
  summarise(
    n_total   = n(),
    n_tier1   = sum(elasticity_method == "DLW_translog", na.rm = TRUE),
    n_tier2   = sum(elasticity_method == "ACF_cd",       na.rm = TRUE),
    n_missing = sum(is.na(beta_k_DLW)),
    .groups = "drop"
  )
print(sec_summary, n = Inf)

#######################################
# SAVE FINAL
#######################################

write_parquet(final_df, "data/proc/elasticities_DLW_ISIC4.parquet")
cat("\nDone. Saved to: data/proc/elasticities_DLW_ISIC4.parquet\n")
cat("\nelasticity_method values:\n")
cat("  'DLW_translog' — Tier 1: firm-varying, full GMM translog\n")
cat("  'ACF_cd'       — Tier 2: sector-constant, Cobb-Douglas GMM\n")
cat("  NA             — not covered by any tier\n")

final_df %>%
  count(elasticity_method)

rm(list = ls())
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
# Inverse Markdowns
#######################################

cat("Markups and Markdowns - Computation\n")

final_df <- final_df %>%
  mutate(
    nu_l_inv = (1 / nu_l_imputed)
  )

#######################################
# SAVE FINAL
#######################################
write_parquet(final_df, "data/proc/work_EAM.parquet")

#Clean-up
rm(list = ls())