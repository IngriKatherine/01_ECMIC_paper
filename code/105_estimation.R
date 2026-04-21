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
    own_w_share=(own_w/own_tot)*100,
    own_w_share2=own_w_share*own_w_share) %>%
  filter(nu_l_imputed!=0, nu_l_imputed<50)

#######################################
# BASIC
#######################################
# ── Models ───────────────────────────────────────────────────────────────────
fit0 <- lm(lnu ~ own_w_share + year + GEO + isic4, data   = df_filt)
summary(fit0)

fit0 <- lm(lnu ~ own_w_share + own_w_share2+ year + GEO + isic4, data   = df_filt)
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

#######################################
# BEAUTIFUL PLOT
#######################################

# Create ownership share categories
df_violin <- df_filt %>%
  filter(
    is.finite(nu_l_imputed),
    is.finite(own_w_share),
    nu_l_imputed < 50
  ) %>%
  mutate(
    own_w_cat = cut(
      own_w_share,
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0–25%", "25–50%", "50–75%", "75–100%"),
      include.lowest = TRUE
    )
  ) %>%
  filter(!is.na(own_w_cat))

# Compute medians for annotation
medians <- df_violin %>%
  group_by(own_w_cat) %>%
  summarise(med = median(nu_l_imputed, na.rm = TRUE), .groups = "drop")

# Color palette (soft, matching your example)
cat_colors <- c(
  "0–25%"    = "#F4A8A8",   # soft red/pink
  "25–50%"   = "#A8D4F4",   # soft blue
  "50–75%"   = "#A8F4C0",   # soft green
  "75–100%"  = "#F4D4A8"    # soft orange
)

# Violin plot
g_violin <- ggplot(df_violin, aes(x = own_w_cat, y = nu_l_imputed, fill = own_w_cat)) +
  
  # Violin
  geom_violin(
    trim    = FALSE,
    alpha   = 0.4,
    color   = NA,
    linewidth = 0
  ) +
  
  # Jittered scatter points inside
  geom_jitter(
    aes(color = own_w_cat),
    width   = 0.08,
    size    = 1.5,
    alpha   = 0.35,
    shape   = 16
  ) +
  
  # Median dot
  geom_point(
    data  = medians,
    aes(x = own_w_cat, y = med),
    shape = 16,
    size  = 4,
    color = "black",
    inherit.aes = FALSE
  ) +
  
  # Median label
  geom_text(
    data  = medians,
    aes(x = own_w_cat, y = med, label = paste0("Median: ", round(med, 2))),
    vjust = -1,
    size  = 4.5,
    fontface = "bold",
    color = "black",
    inherit.aes = FALSE
  ) +
  
  scale_fill_manual(values = cat_colors) +
  scale_color_manual(values = c(
    "0–25%"   = "#C0504D",
    "25–50%"  = "#4472C4",
    "50–75%"  = "#217346",
    "75–100%" = "#C07830"
  )) +
  
  labs(
    x = "Female Executive Ownership Share",
    y = "Markdown (Productivity-wage gap)"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text       = element_text(color = "black", size = 14),
    axis.title      = element_text(size = 14, face = "bold"),
    axis.line       = element_line(color = "black", linewidth = 0.5),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

g_violin

ggsave("latex/violin_markdown_ownwshare.png",
       plot = g_violin, width = 8, height = 5, dpi = 150)

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


library(tidyverse)
library(ggplot2)
library(patchwork)
library(car)        # for vif(), ncvTest()
library(lmtest)     # for bptest()

diag_col   <- "#4472C4"   # soft blue — main points
line_col   <- "#C0504D"   # soft red  — reference / fit lines
band_col   <- "#A8D4F4"   # light blue — confidence bands
text_col   <- "black"

# Shared theme (mirrors your violin theme_minimal setup)
diag_theme <- theme_minimal(base_size = 14) +
  theme(
    axis.text    = element_text(color = text_col, size = 13),
    axis.title   = element_text(size = 13, face = "bold"),
    axis.line    = element_line(color = text_col, linewidth = 0.5),
    panel.border = element_rect(color = text_col, fill = NA, linewidth = 0.5),
    plot.title   = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
    legend.position = "none"
  )

# ── Fit your model (replace with your actual model) ─────────
model <- lm(lnu ~ own_w_share + L_share_ind + shareprod + HHI + firm_decile + age_group + tfp + year + GEO + isic4, data = df_filt)

# Extract diagnostic quantities
df_diag <- tibble(
  fitted    = fitted(model),
  residuals = residuals(model),
  std_resid = rstandard(model),
  sqrt_abs  = sqrt(abs(rstandard(model))),
  leverage  = hatvalues(model),
  cooksd    = cooks.distance(model)
)

n          <- nrow(df_diag)
top_idx    <- order(df_diag$cooksd, decreasing = TRUE)[1:3]   # label top outliers


# ── 1. LINEARITY — Residuals vs Fitted ──────────────────────
p1 <- ggplot(df_diag, aes(x = fitted, y = residuals)) +
  geom_hline(yintercept = 0, color = line_col,
             linetype = "dashed", linewidth = 0.7) +
  geom_point(color = diag_col, alpha = 0.35, size = 1.5, shape = 16) +
  geom_smooth(method  = "loess", se = TRUE,
              color   = line_col,
              fill    = band_col,
              linewidth = 1, alpha = 0.3) +
  geom_text(
    data = df_diag[top_idx, ] %>% mutate(idx = top_idx),
    aes(label = idx), size = 3.5, color = text_col, vjust = -0.8
  ) +
  labs(
    title    = "Linearity",
    subtitle = "Residuals vs Fitted",
    x = "Fitted values", y = "Residuals"
  ) +
  diag_theme

# ── 2. NORMALITY OF ERRORS — Q–Q plot ───────────────────────
qq_df <- df_diag %>%
  arrange(std_resid) %>%
  mutate(
    theoretical = qnorm(ppoints(n)),
    is_tail     = row_number() %in% c(1:3, (n-2):n)
  )

p2 <- ggplot(qq_df, aes(x = theoretical, y = std_resid)) +
  geom_abline(slope = 1, intercept = 0,
              color = line_col, linetype = "dashed", linewidth = 0.7) +
  geom_point(color = diag_col, alpha = 0.35, size = 1.5, shape = 16) +
  geom_text(
    data = filter(qq_df, is_tail),
    aes(label = round(std_resid, 1)),
    size = 3, color = text_col, vjust = -0.8
  ) +
  labs(
    title    = "Normality of Errors",
    subtitle = paste0("Q–Q Plot  |  SW p = ",
                      round(shapiro.test(
                        sample(df_diag$std_resid,
                               min(5000, n)))$p.value, 3)),
    x = "Theoretical quantiles", y = "Standardized residuals"
  ) +
  diag_theme

# ── 3. HOMOSKEDASTICITY — Scale–Location ────────────────────
bp   <- lmtest::bptest(model)
bp_p <- round(bp$p.value, 3)

p3 <- ggplot(df_diag, aes(x = fitted, y = sqrt_abs)) +
  geom_point(color = diag_col, alpha = 0.35, size = 1.5, shape = 16) +
  geom_smooth(method    = "loess", se = TRUE,
              color     = line_col,
              fill      = band_col,
              linewidth = 1, alpha = 0.3) +
  labs(
    title    = "Homoskedasticity",
    subtitle = paste0("Scale–Location  |  BP p = ", bp_p),
    x = "Fitted values", y = expression(sqrt("|Std. residuals|"))
  ) +
  diag_theme

# ── 4. COLLINEARITY — VIF bar chart ─────────────────────────
vif_vals <- car::vif(model)

# Handle GVIF (for categorical predictors) vs plain VIF
if (is.matrix(vif_vals)) {
  vif_df <- tibble(
    term = rownames(vif_vals),
    vif  = vif_vals[, "GVIF^(1/(2*Df))"]^2   # adjusted GVIF² ≈ VIF
  )
} else {
  vif_df <- tibble(term = names(vif_vals), vif = vif_vals)
}

# Color bars by severity
vif_df <- vif_df %>%
  mutate(
    severity = case_when(
      vif >= 10 ~ "High (≥10)",
      vif >=  5 ~ "Moderate (5–10)",
      TRUE      ~ "Low (<5)"
    ),
    severity = factor(severity,
                      levels = c("Low (<5)", "Moderate (5–10)", "High (≥10)"))
  )

vif_colors <- c(
  "Low (<5)"       = "#A8F4C0",   # soft green  (your violin palette)
  "Moderate (5–10)"= "#F4D4A8",   # soft orange
  "High (≥10)"     = "#F4A8A8"    # soft red/pink
)

p4 <- ggplot(vif_df, aes(x = reorder(term, vif), y = vif, fill = severity)) +
  geom_col(color = text_col, linewidth = 0.4, alpha = 0.75, width = 0.6) +
  geom_hline(yintercept = 5,  color = "#C07830",
             linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = 10, color = "#C0504D",
             linetype = "dashed", linewidth = 0.7) +
  geom_text(aes(label = round(vif, 2)),
            hjust = -0.15, size = 3.5, color = text_col, fontface = "bold") +
  coord_flip(clip = "off") +
  scale_fill_manual(values = vif_colors, name = "VIF severity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Collinearity",
    subtitle = "Variance Inflation Factors (VIF)",
    x = NULL, y = "VIF"
  ) +
  diag_theme +
  theme(
    legend.position = "bottom",
    legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 10)
  )

# ── Assemble with patchwork ──────────────────────────────────
diag_grid <- (p1 | p2) / (p3 | p4) +
  plot_annotation(
    title   = "Linear Model Diagnostics",
    theme   = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
    )
  )

diag_grid

ggsave("latex/lm_diagnostics.png",
       plot = diag_grid, width = 12, height = 9, dpi = 150)
