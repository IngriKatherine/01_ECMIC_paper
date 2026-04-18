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
pacman::p_load(dplyr, tidyverse, ggplot2, haven, ezids, arrow, scales, tidyr, knitr, kableExtra, showtext, sysfonts, fixest, modelsummary)
font_add("cmunsi", "C:/Users/k_the/Downloads/computer-modern/cmunss.ttf")
showtext_auto()

#######################################
# Open EAM and BRING ELASTICITIES
#######################################

### EAM ALL
df <- read_parquet("data/proc/work_EAM.parquet")

#######################################
# BASIC
#######################################

# ── Models ───────────────────────────────────────────────────────────────────
fit0 <- lm(nu_l_imputed ~ own_w_share,
              data   = df)

fit1 <- feglm(nu_l_imputed ~ own_w_share | year,
              data   = df)

fit2 <- feglm(nu_l_inv ~ own_w_share | year + isic4,
              data   = df)

fit3 <- feglm(nu_l_inv ~ own_w_share | year + isic4 + GEO,
              data   = df)

# ── Extract coefficients & SEs ───────────────────────────────────────────────
extract_coef <- function(model) {
  ct    <- summary(model)$coeftable
  pcol  <- grep("Pr\\(>", colnames(ct), value = TRUE)
  
  list(
    coef = setNames(ct[, "Estimate"],   rownames(ct)),
    se   = setNames(ct[, "Std. Error"], rownames(ct)),
    pval = setNames(ct[, pcol],         rownames(ct))
  )
}
m0 <- extract_coef(fit0)
m1 <- extract_coef(fit1)
m2 <- extract_coef(fit2)
m3 <- extract_coef(fit3)

# ── All variables across models ───────────────────────────────────────────────
all_vars <- unique(c(names(m1$coef), names(m2$coef), names(m3$coef)))

# ── Stars helper ─────────────────────────────────────────────────────────────
stars <- function(p) {
  if (is.na(p))        return("")
  if (p < 0.01)        return("$^{***}$")
  if (p < 0.05)        return("$^{**}$")
  if (p < 0.10)        return("$^{*}$")
  return("")
}

fmt  <- function(x) formatC(round(x, 3), format = "f", digits = 3)

# ── Build LaTeX rows ─────────────────────────────────────────────────────────
models   <- list(m1, m2, m3)
coef_rows <- character(0)

for (v in all_vars) {
  coef_row <- paste0("\\texttt{", gsub("_", "\\\\_", v), "}")
  se_row   <- ""
  
  for (m in models) {
    if (v %in% names(m$coef)) {
      coef_row <- paste0(coef_row, " & ", fmt(m$coef[v]), stars(m$pval[v]))
      se_row   <- paste0(se_row,   " & (", fmt(m$se[v]), ")")
    } else {
      coef_row <- paste0(coef_row, " & ")
      se_row   <- paste0(se_row,   " & ")
    }
  }
  
  coef_row <- paste0(coef_row, " \\\\")
  se_row   <- paste0(se_row,   " \\\\")
  coef_rows <- c(coef_rows, coef_row, se_row)
}

# ── Observations row ─────────────────────────────────────────────────────────
n_row <- paste0("Observations",
                paste(sapply(models, function(m) paste0(" & ", length(m$coef))), collapse = ""),
                " \\\\")

# ── Assemble LaTeX table ──────────────────────────────────────────────────────
fe_row <- paste0(
  "Fixed Effects",
  " & Year",
  " & Year-Industry",
  " & Year-Industry \\& Geography",
  " \\\\"
)

latex_table <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Regression Results}",
  "\\label{tab:regression}",
  "\\begin{tabular}{l ccc}",
  "\\hline\\hline",
  " & (1) & (2) & (3) \\\\",
  "\\hline",
  coef_rows,
  "\\hline",
  fe_row,
  "\\hline\\hline",
  "\\multicolumn{4}{l}{\\textit{Note:} $^{*}$p$<$0.10; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\",
  "\\end{tabular}",
  "\\end{table}"
)

# ── Print & save ─────────────────────────────────────────────────────────────
cat(paste(latex_table, collapse = "\n"))
writeLines(latex_table, "regression_table.tex")





#######################################
# FILTERS
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

df_reg <- df %>%
  filter(
    !is.na(nu_l_imputed),
    !is.na(female_dummy),
    !is.na(own_w_share),
    !is.na(firm_age),
    !is.na(ln_GO),
    !is.na(ln_L),
    !is.na(isic4),
    !is.na(GEO),
    !is.na(year)
  ) 

cat("Regression sample:", nrow(df_reg), "obs |",
    n_distinct(df_reg$id_firm), "firms |",
    n_distinct(df_reg$year), "years\n")

