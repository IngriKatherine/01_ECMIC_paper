############################################################
# Project: Spring Paper 20260
# Author:  Ingri Katherine Quevedo Rocha
# Created: 2026-02-23
############################################################

############################
# Environment Setup
############################
# Set Project folder as the Working Directory
#setwd("..")
setwd("C:/Users/k_the/main/01_ECMIC_paper")
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
library(fixest)
library(dplyr)

### EAM ALL
df <- read_parquet("data/proc/work_EAM.parquet")

### Firm share in employment
df <- df %>%
  group_by(year, GEO) %>%
  mutate(L_share_geo = L / sum(L, na.rm = TRUE)) %>%
  mutate(GO_share_geo = GO / sum(GO, na.rm = TRUE)) %>%     # firm share in GO by industry
  ungroup() %>%
  group_by(year, isic4) %>%                          # group by year & industry
  mutate(L_share_ind = L / sum(L, na.rm = TRUE)) %>%
  mutate(GO_share_ind = GO / sum(GO, na.rm = TRUE)) %>%     # firm share in GO by industry
  ungroup() %>%
  mutate(
    year = as.factor(year),
    age = as.factor(firm_age),
    shareprod = (lprod / L) * 100,
    sharew = (lw / (lw + lm)) * 100,
    lnu=log(nu_l_imputed)
  )

hhi <- df %>%
  group_by(year, isic4) %>%
  summarise(HHI = sum((GO_share_ind*100)^2, na.rm = TRUE), .groups = "drop")

df <- df %>%
  left_join(hhi, by = c("year", "isic4"))

df <- df %>%
  group_by(year) %>%
  mutate(firm_decile = as.factor(ntile(GO, 10))) %>%
  ungroup()

df <- df %>%
  mutate(age_group = case_when(
    firm_age <= 1              ~ "1. Young (≤1 year)",
    firm_age >= 2  & firm_age <= 5  ~ "2. Early (2-5 years)",
    firm_age >= 6  & firm_age <= 10 ~ "3. Established (6-10 years)",
    firm_age >= 11 & firm_age <= 15 ~ "4. Mature (11-15 years)",
    firm_age >= 16 & firm_age <= 20 ~ "5. Old (16-20 years)",
    firm_age > 20                   ~ "6. Very Old (>20 years)",
    TRUE                            ~ NA_character_
  ) %>%
    as.factor())

### FILTERS
df_filt <- df %>%
  mutate( own_w = pmin(own_w, quantile(own_w, 0.70, na.rm = TRUE)),
    own_w_share=(own_w/own_tot)*100) %>%
  filter(nu_l_imputed!=0, nu_l_imputed<50)

#######################################
# BASIC
#######################################
# ── Models ───────────────────────────────────────────────────────────────────
fit0 <- lm(lnu ~ own_w_share + year + GEO + isic4, data   = df_filt)
summary(fit0)

fit1 <- lm(lnu ~ own_w_share + L_share_ind + shareprod + HHI + firm_decile + age_group + tfp + year, data  = df_filt)
summary(fit1)

fit2 <- lm(lnu ~ own_w_share+ L_share_ind + shareprod + HHI + firm_decile + age_group + tfp  + year + GEO, data  = df_filt)
summary(fit2)

fit3 <- lm(lnu ~ own_w_share + L_share_ind + shareprod + HHI + firm_decile + age_group + tfp + year + GEO + isic4, data  = df_filt)
summary(fit3)


get_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.1   ~ ".",
    TRUE       ~ ""
  )
}

# ── Variables of interest ─────────────────────────────────────────────────────
vars_of_interest <- c(
  "own_w_share",
  "L_share_ind",
  "shareprod",
  "HHI",
  "firm_decile2", "firm_decile3", "firm_decile4", "firm_decile5",
  "firm_decile6", "firm_decile7", "firm_decile8", "firm_decile9", "firm_decile10",
  "age_group2. Early (2-5 years)", "age_group3. Established (6-10 years)",
  "age_group4. Mature (11-15 years)", "age_group5. Old (16-20 years)",
  "tfp"
)

# ── Nice display names ────────────────────────────────────────────────────────
var_labels <- c(
  "own_w_share"                            = "Female Leadership",
  "L_share_ind"                            = "Labour share (industry)",
  "shareprod"                              = "Share of production",
  "HHI"                                    = "HHI",
  "firm_decile2"                           = "Firm decile 2",
  "firm_decile3"                           = "Firm decile 3",
  "firm_decile4"                           = "Firm decile 4",
  "firm_decile5"                           = "Firm decile 5",
  "firm_decile6"                           = "Firm decile 6",
  "firm_decile7"                           = "Firm decile 7",
  "firm_decile8"                           = "Firm decile 8",
  "firm_decile9"                           = "Firm decile 9",
  "firm_decile10"                          = "Firm decile 10",
  "age_group2. Early (2-5 years)"          = "Age: Early (2--5 yrs)",
  "age_group3. Established (6-10 years)"   = "Age: Established (6--10 yrs)",
  "age_group4. Mature (11-15 years)"       = "Age: Mature (11--15 yrs)",
  "age_group5. Old (16-20 years)"          = "Age: Old (16--20 yrs)",
  "tfp"                                    = "TFP"
)

# ── Extract one model into a tidy data frame ──────────────────────────────────
extract_model <- function(fit) {
  s   <- summary(fit)
  cf  <- as.data.frame(coef(s))
  cf$var <- rownames(cf)
  
  cf <- cf %>%
    filter(var %in% vars_of_interest) %>%
    mutate(
      stars  = get_stars(`Pr(>|t|)`),
      coef_s = paste0(formatC(Estimate,    digits = 4, format = "f"), stars),
      se_s   = paste0("(", formatC(`Std. Error`, digits = 4, format = "f"), ")")
    ) %>%
    select(var, coef_s, se_s)
  
  # bottom stats
  n   <- nobs(fit)
  r2  <- s$r.squared
  
  list(cf = cf, n = n, r2 = r2)
}

# ── Run models ────────────────────────────────────────────────────────────────
fit0 <- lm(lnu ~ own_w_share + year + GEO + isic4, data = df_filt)
fit1 <- lm(lnu ~ own_w_share + L_share_ind + shareprod + HHI + firm_decile + age_group + tfp + year, data = df_filt)
fit2 <- lm(lnu ~ own_w_share + L_share_ind + shareprod + HHI + firm_decile + age_group + tfp + year + GEO, data = df_filt)
fit3 <- lm(lnu ~ own_w_share + L_share_ind + shareprod + HHI + firm_decile + age_group + tfp + year + GEO + isic4, data = df_filt)

models      <- list(fit0, fit1, fit2, fit3)
model_names <- c("(1)", "(2)", "(3)", "(4)")

ex <- lapply(models, extract_model)

# ── Build wide table ──────────────────────────────────────────────────────────
# All variables that appear in at least one model
all_vars <- vars_of_interest   # preserves your preferred order

build_col <- function(e) {
  rows <- data.frame(var = character(), coef_s = character(), se_s = character(),
                     stringsAsFactors = FALSE)
  for (v in all_vars) {
    hit <- e$cf %>% filter(var == v)
    if (nrow(hit) == 1) {
      rows <- rbind(rows, data.frame(var = v, coef_s = hit$coef_s, se_s = hit$se_s,
                                     stringsAsFactors = FALSE))
    } else {
      rows <- rbind(rows, data.frame(var = v, coef_s = "", se_s = "",
                                     stringsAsFactors = FALSE))
    }
  }
  rows
}

cols <- lapply(ex, build_col)

# ── Build LaTeX ───────────────────────────────────────────────────────────────
cat_ln <- function(...) cat(..., "\n", sep = "")

sink("latex/regression_table.tex")   # writes to file; remove to print to console

cat_ln("\\begin{table}[htbp]")
cat_ln("\\centering")
cat_ln("\\caption{Regression Results}")
cat_ln("\\label{tab:regression}")
cat_ln(paste0("\\begin{tabular}{l", paste(rep("c", length(models)), collapse = ""), "}"))
cat_ln("\\hline\\hline")

# Header
cat_ln(paste0(
  " & ",
  paste(model_names, collapse = " & "),
  " \\\\"
))
cat_ln("\\hline")

# Rows: coefficient + SE
for (v in all_vars) {
  label <- ifelse(!is.na(var_labels[v]), var_labels[v], v)
  
  # coefficient row
  coef_vals <- sapply(cols, function(col) col$coef_s[col$var == v])
  cat_ln(paste0(label, " & ", paste(coef_vals, collapse = " & "), " \\\\"))
  
  # standard error row
  se_vals <- sapply(cols, function(col) col$se_s[col$var == v])
  cat_ln(paste0(" & ", paste(se_vals, collapse = " & "), " \\\\"))
}

cat_ln("\\hline")

# N and R2
n_row  <- sapply(ex, function(e) formatC(e$n,  format = "d", big.mark = ","))
r2_row <- sapply(ex, function(e) formatC(e$r2, digits = 4, format = "f"))

cat_ln(paste0("$N$ & ", paste(n_row,  collapse = " & "), " \\\\"))
cat_ln(paste0("$R^{2}$ & ", paste(r2_row, collapse = " & "), " \\\\"))

cat_ln("\\hline\\hline")
cat_ln(paste0("\\multicolumn{", length(models) + 1,
              "}{l}{\\textit{Note:} $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$; $^{.}p<0.1$} \\\\"))
cat_ln("\\end{tabular}")
cat_ln("\\end{table}")

sink()


par(mfrow = c(2, 2)) 
plot(fit1, main = "Diagnostic Plots Model") 



##############################################
### PLOTS AND TABLES FOR THE PAPER
####################################

df_scatter <- df_filt %>%
  filter(
    is.finite(nu_l_imputed),
    is.finite(own_w_share),
    nu_l_imputed < 10
  )

# Correlation
ct <- cor.test(df_scatter$own_w_share, df_scatter$nu_l_imputed)

lab <- paste0("Corr = ", round(ct$estimate, 3),
              "\nP-value = ", format.pval(ct$p.value, digits = 3, eps = 0.001))

# Scatter plot
g_scatter <- ggplot(df_scatter, aes(x = own_w_share, y = nu_l_imputed, weight = GO)) +
  geom_point(aes(size = GO), shape = 16, alpha = 0.3, color = "#187864") +
  geom_smooth(method = "lm", se = TRUE, color = "#781830", linewidth = 1.5) +  
  scale_size_continuous(range = c(0.5, 5)) +
  labs(
    x = "Female Leadership",
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

# ============================================================
# SECTION 2: By Firm
# ============================================================
firm_vars <- list(
  own_tot     = "Total owners (N)",
  own_w_share = "Female leadership",
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


firm_stats[, c("Mean","Min","Max","SD")] <-
  lapply(firm_stats[, c("Mean","Min","Max","SD")], fmt)

firm_stats




# ── Variables for descriptive statistics ──────────────────────────────────────
desc_vars <- c(
  "lnu",          # dependent variable
  "own_w_share",  # own wage share
  "L_share_ind",  # labour share industry
  "shareprod",    # share of production
  "HHI",          # Herfindahl-Hirschman Index
  "tfp",          # total factor productivity
  "GO",           # gross output
  "firm_age",     # firm age in years
  "L",            # number of employees
  "w"             # wage per worker
)

# ── Nice display labels ───────────────────────────────────────────────────────
var_labels <- c(
  "lnu"         = "Log wage bill (lnu)",
  "own_w_share" = "Own wage share",
  "L_share_ind" = "Labour share (industry)",
  "shareprod"   = "Share of production",
  "HHI"         = "HHI",
  "tfp"         = "TFP",
  "GO"          = "Gross output (GO)",
  "firm_age"    = "Firm age (years)",
  "L"           = "Employees (L)",
  "w"           = "Wage per worker (w)"
)

# ── Compute descriptive statistics ───────────────────────────────────────────
desc_stats <- df_filt %>% mutate (GO=GO/3600000000, w=w/3600000)

desc_stats <- desc_stats %>%
  select(all_of(desc_vars)) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~ mean(.x, na.rm = TRUE),
      SD   = ~ sd(.x,   na.rm = TRUE),
      Min  = ~ min(.x,  na.rm = TRUE),
      P50  = ~ median(.x, na.rm = TRUE),
      Max  = ~ max(.x,  na.rm = TRUE),
      N    = ~ sum(!is.na(.x))
    ),
    .names = "{.col}__{.fn}"
  )) %>%
  tidyr::pivot_longer(
    everything(),
    names_to  = c("variable", "stat"),
    names_sep = "__"
  ) %>%
  tidyr::pivot_wider(names_from = stat, values_from = value) %>%
  mutate(
    Label = var_labels[variable],
    across(c(Mean, SD, Min, P50, Max), ~ round(.x, 3)),
    N = as.integer(N)
  ) %>%
  select(Label, N, Mean, SD, Min, P50, Max)

# ── Print to console ──────────────────────────────────────────────────────────
print(desc_stats)

# ── Write LaTeX table ─────────────────────────────────────────────────────────
sink("latex/descriptive_stats.tex")

cat("\\begin{table}[htbp]\n")
cat("\\centering\n")
cat("\\footnotesize\n")
cat("\\caption{Descriptive Statistics}\n")
cat("\\label{tab:desc_stats}\n")
cat("\\begin{tabular}{lrrrrrr}\n")
cat("\\hline\\hline\n")
cat("Variable & $N$ & Mean & SD & Min & Median & Max \\\\\n")
cat("\\hline\n")

for (i in seq_len(nrow(desc_stats))) {
  row <- desc_stats[i, ]
  cat(sprintf(
    "%s & %s & %s & %s & %s & %s & %s \\\\\n",
    row$Label,
    formatC(row$N,    format = "d", big.mark = ","),
    formatC(row$Mean, format = "f", digits = 3),
    formatC(row$SD,   format = "f", digits = 3),
    formatC(row$Min,  format = "f", digits = 3),
    formatC(row$P50,  format = "f", digits = 3),
    formatC(row$Max,  format = "f", digits = 3)
  ))
}

cat("\\hline\\hline\n")
cat("\\end{tabular}\n")
cat("\\end{table}\n")

sink()
message("Table written to descriptive_stats.tex")