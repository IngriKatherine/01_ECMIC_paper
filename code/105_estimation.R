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


df_filt <- df %>%
  mutate(
    female_dummy = ifelse(own_w_share > 0, 1, 0),
    GO_mil = GO / 3670000,
    nu_l_inv = nu_l_inv*100
  ) %>%
  filter(
    is.finite(nu_l_inv),
    is.finite(own_w_share),
    nu_l_inv <= 200
  )

#######################################
# BASIC
#######################################

library(fixest)
library(dplyr)

# ── Models ───────────────────────────────────────────────────────────────────
fit1 <- feglm(nu_l_inv ~ own_w_share | year,
              data   = df_filt)

fit2 <- feglm(nu_l_inv ~ own_w_share | year + isic4,
              data   = df_filt)

fit3 <- feglm(nu_l_inv ~ own_w_share | year + isic4 + GEO,
              data   = df_filt)

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

