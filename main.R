# =========================================================
# Replication code for:
# 1918 Flu Flash Paper
# Yi Liu (2026)
# Protecting Jobs without Harming Productivity
# NPI Only on Employment and TFP Proxy
# Three NPI Measures:
#   1) high_npi
#   2) markel_days_npi
#   3) markel_speed_npi
# =========================================================

# -----------------------------
# 0. Packages
# -----------------------------
required_packages <- c(
  "haven", "dplyr", "fixest", "ggplot2",
  "stringr", "broom", "tibble", "readr", "purrr"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

library(haven)
library(dplyr)
library(fixest)
library(ggplot2)
library(stringr)
library(broom)
library(tibble)
library(readr)
library(purrr)

# -----------------------------
# 1. File paths
# -----------------------------
path_city <- "data/city.dta"
path_manu <- "data/city-manufacturing.dta"

out_dir <- "results_employment_tfp_npi_robustness"
if (!dir.exists(out_dir)) dir.create(out_dir)

# -----------------------------
# 2. Read data
# -----------------------------
city <- read_dta(path_city)
manu <- read_dta(path_manu)

cat("city dim:", dim(city), "\n")
cat("manu dim:", dim(manu), "\n")

# -----------------------------
# 3. Helper functions
# -----------------------------
require_vars <- function(df, vars, df_name = deparse(substitute(df))) {
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop(
      paste0(
        "Missing variables in ", df_name, ": ",
        paste(missing_vars, collapse = ", ")
      )
    )
  }
}

resolve_joined_var <- function(df, varname) {
  if (varname %in% names(df)) return(df)

  x_name <- paste0(varname, ".x")
  y_name <- paste0(varname, ".y")

  has_x <- x_name %in% names(df)
  has_y <- y_name %in% names(df)

  if (has_x && has_y) {
    df[[varname]] <- dplyr::coalesce(df[[x_name]], df[[y_name]])
  } else if (has_x) {
    df[[varname]] <- df[[x_name]]
  } else if (has_y) {
    df[[varname]] <- df[[y_name]]
  }

  df
}

extract_year_terms <- function(tidy_df, keyword, type_label) {
  out <- tidy_df %>%
    filter(str_detect(term, fixed(keyword))) %>%
    mutate(
      year_plot = as.numeric(str_extract(term, "\\d{4}")),
      type = type_label
    ) %>%
    filter(!is.na(year_plot))
  out
}

make_baseline_rows <- function(type_vec, baseline_year = 1914) {
  tibble(
    term = paste0("baseline_", seq_along(type_vec)),
    estimate = 0,
    std.error = 0,
    statistic = NA_real_,
    p.value = NA_real_,
    conf.low = NA_real_,
    conf.high = NA_real_,
    year_plot = baseline_year,
    type = type_vec
  )
}

get_post_coef_info <- function(model, keyword, outcome_label, spec_label) {
  td <- broom::tidy(model, conf.int = TRUE)

  row <- td %>%
    filter(str_detect(term, keyword), str_detect(term, "post")) %>%
    slice(1)

  if (nrow(row) == 0) {
    return(tibble(
      outcome = outcome_label,
      specification = spec_label,
      term = NA_character_,
      estimate = NA_real_,
      std.error = NA_real_,
      statistic = NA_real_,
      p.value = NA_real_,
      conf.low = NA_real_,
      conf.high = NA_real_
    ))
  }

  tibble(
    outcome = outcome_label,
    specification = spec_label,
    term = row$term,
    estimate = row$estimate,
    std.error = row$std.error,
    statistic = row$statistic,
    p.value = row$p.value,
    conf.low = row$conf.low,
    conf.high = row$conf.high
  )
}

run_dynamic_plot <- function(df_plot, plot_title, outfile) {
  p <- ggplot(
    df_plot,
    aes(x = year_plot, y = estimate, color = type, group = type)
  ) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(
        ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error
      ),
      width = 0.25
    ) +
    geom_vline(xintercept = 1914, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dotted") +
    scale_x_continuous(breaks = sort(unique(df_plot$year_plot))) +
    labs(
      x = "Year",
      y = "Coefficient relative to 1914",
      title = plot_title,
      color = NULL
    ) +
    theme_minimal()

  print(p)

  ggsave(
    filename = outfile,
    plot = p,
    width = 8.5,
    height = 5.5
  )

  return(p)
}

# -----------------------------
# 4. Inspect key variables
# -----------------------------
cat("\nCity variables relevant for NPI + controls:\n")
print(names(city)[str_detect(
  names(city),
  "high_npi|markel_days_npi|markel_speed_npi|pop|density|mortality"
)])

cat("\nManufacturing variables:\n")
print(names(manu)[str_detect(
  names(manu),
  "year|manuf_emp|manuf_value|manuf_capital|manuf_valadd|wages|hp"
)])

# =========================================================
# PART I. CITY-LEVEL NPI VARIABLES
# =========================================================

# -----------------------------
# 5. Check required variables
# -----------------------------
required_city_vars <- c(
  "city_id",
  "high_npi",
  "markel_days_npi",
  "markel_speed_npi",
  "log_pop1910",
  "density1910",
  "ip_mortality_rate1917"
)
require_vars(city, required_city_vars, "city")

# -----------------------------
# 6. Construct city_core
# -----------------------------
city_core <- city %>%
  transmute(
    city_id,
    high_npi,
    markel_days_npi,
    markel_speed_npi,
    log_pop1910,
    density1910,
    ip_mortality_rate1917
  ) %>%
  mutate(
    high_npi_z = as.numeric(scale(high_npi)),
    markel_days_npi_z = as.numeric(scale(markel_days_npi)),
    markel_speed_npi_z = as.numeric(scale(markel_speed_npi))
  )

cat("\nSummary of NPI variables:\n")
print(summary(city_core %>% select(
  high_npi, markel_days_npi, markel_speed_npi,
  high_npi_z, markel_days_npi_z, markel_speed_npi_z
)))

# =========================================================
# PART II. MANUFACTURING DATA + OUTCOMES
# =========================================================

# -----------------------------
# 7. Check required manufacturing vars
# -----------------------------
required_manu_vars <- c(
  "city_id",
  "year",
  "manuf_emp",
  "manuf_capital",
  "manuf_valadd"
)
require_vars(manu, required_manu_vars, "manu")

# -----------------------------
# 8. Merge city-level vars into manufacturing
# -----------------------------
df_manu <- manu %>%
  left_join(city_core, by = "city_id")

for (v in c(
  "high_npi", "markel_days_npi", "markel_speed_npi",
  "high_npi_z", "markel_days_npi_z", "markel_speed_npi_z",
  "log_pop1910", "density1910", "ip_mortality_rate1917"
)) {
  df_manu <- resolve_joined_var(df_manu, v)
}

cat("\nPotential joined names after merge:\n")
print(names(df_manu)[str_detect(
  names(df_manu),
  "high_npi|markel_days_npi|markel_speed_npi|log_pop1910|density1910|ip_mortality_rate1917"
)])

# -----------------------------
# 9. Construct outcomes
# -----------------------------
alpha <- 0.33

df_manu <- df_manu %>%
  mutate(
    post = ifelse(year >= 1919, 1, 0),

    manuf_emp_pos = ifelse(manuf_emp > 0, manuf_emp, NA_real_),
    manuf_capital_pos = ifelse(manuf_capital > 0, manuf_capital, NA_real_),
    manuf_valadd_pos = ifelse(manuf_valadd > 0, manuf_valadd, NA_real_),

    log_emp = log(manuf_emp_pos),
    log_capital = log(manuf_capital_pos),
    log_valadd = log(manuf_valadd_pos),

    log_tfp_proxy = log_valadd - alpha * log_capital - (1 - alpha) * log_emp,
    log_valadd_per_worker = log_valadd - log_emp
  )

cat("\nSummary of employment outcome:\n")
print(summary(df_manu$log_emp))

cat("\nSummary of TFP proxy outcome:\n")
print(summary(df_manu$log_tfp_proxy))

# =========================================================
# PART III. REGRESSION SAMPLES
# =========================================================

# Separate samples for each NPI measure and each outcome
df_emp_high <- df_manu %>%
  filter(
    !is.na(log_emp),
    !is.na(high_npi_z),
    !is.na(log_pop1910),
    !is.na(density1910),
    !is.na(ip_mortality_rate1917),
    !is.na(city_id),
    !is.na(year)
  )

df_emp_days <- df_manu %>%
  filter(
    !is.na(log_emp),
    !is.na(markel_days_npi_z),
    !is.na(log_pop1910),
    !is.na(density1910),
    !is.na(ip_mortality_rate1917),
    !is.na(city_id),
    !is.na(year)
  )

df_emp_speed <- df_manu %>%
  filter(
    !is.na(log_emp),
    !is.na(markel_speed_npi_z),
    !is.na(log_pop1910),
    !is.na(density1910),
    !is.na(ip_mortality_rate1917),
    !is.na(city_id),
    !is.na(year)
  )

df_tfp_high <- df_manu %>%
  filter(
    !is.na(log_tfp_proxy),
    !is.na(high_npi_z),
    !is.na(log_pop1910),
    !is.na(density1910),
    !is.na(ip_mortality_rate1917),
    !is.na(city_id),
    !is.na(year)
  )

df_tfp_days <- df_manu %>%
  filter(
    !is.na(log_tfp_proxy),
    !is.na(markel_days_npi_z),
    !is.na(log_pop1910),
    !is.na(density1910),
    !is.na(ip_mortality_rate1917),
    !is.na(city_id),
    !is.na(year)
  )

df_tfp_speed <- df_manu %>%
  filter(
    !is.na(log_tfp_proxy),
    !is.na(markel_speed_npi_z),
    !is.na(log_pop1910),
    !is.na(density1910),
    !is.na(ip_mortality_rate1917),
    !is.na(city_id),
    !is.na(year)
  )

cat("\nSample sizes:\n")
cat("Employment - high_npi:", nrow(df_emp_high), "\n")
cat("Employment - markel_days_npi:", nrow(df_emp_days), "\n")
cat("Employment - markel_speed_npi:", nrow(df_emp_speed), "\n")
cat("TFP - high_npi:", nrow(df_tfp_high), "\n")
cat("TFP - markel_days_npi:", nrow(df_tfp_days), "\n")
cat("TFP - markel_speed_npi:", nrow(df_tfp_speed), "\n")

# =========================================================
# PART IV. AVERAGE POST EFFECTS
# =========================================================

# -----------------------------
# 10. Employment DID
# -----------------------------
m_emp_high <- feols(
  log_emp ~ high_npi_z:post +
    log_pop1910:post + density1910:post + ip_mortality_rate1917:post |
    city_id + year,
  cluster = ~city_id,
  data = df_emp_high
)

m_emp_days <- feols(
  log_emp ~ markel_days_npi_z:post +
    log_pop1910:post + density1910:post + ip_mortality_rate1917:post |
    city_id + year,
  cluster = ~city_id,
  data = df_emp_days
)

m_emp_speed <- feols(
  log_emp ~ markel_speed_npi_z:post +
    log_pop1910:post + density1910:post + ip_mortality_rate1917:post |
    city_id + year,
  cluster = ~city_id,
  data = df_emp_speed
)

# -----------------------------
# 11. TFP DID
# -----------------------------
m_tfp_high <- feols(
  log_tfp_proxy ~ high_npi_z:post +
    log_pop1910:post + density1910:post + ip_mortality_rate1917:post |
    city_id + year,
  cluster = ~city_id,
  data = df_tfp_high
)

m_tfp_days <- feols(
  log_tfp_proxy ~ markel_days_npi_z:post +
    log_pop1910:post + density1910:post + ip_mortality_rate1917:post |
    city_id + year,
  cluster = ~city_id,
  data = df_tfp_days
)

m_tfp_speed <- feols(
  log_tfp_proxy ~ markel_speed_npi_z:post +
    log_pop1910:post + density1910:post + ip_mortality_rate1917:post |
    city_id + year,
  cluster = ~city_id,
  data = df_tfp_speed
)

# =========================================================
# PART V. DYNAMIC EFFECTS
# =========================================================

# -----------------------------
# 12. Employment dynamic
# -----------------------------
m_emp_dyn_high <- feols(
  log_emp ~ i(year, high_npi_z, ref = 1914) |
    city_id + year,
  cluster = ~city_id,
  data = df_emp_high
)

m_emp_dyn_days <- feols(
  log_emp ~ i(year, markel_days_npi_z, ref = 1914) |
    city_id + year,
  cluster = ~city_id,
  data = df_emp_days
)

m_emp_dyn_speed <- feols(
  log_emp ~ i(year, markel_speed_npi_z, ref = 1914) |
    city_id + year,
  cluster = ~city_id,
  data = df_emp_speed
)

# -----------------------------
# 13. TFP dynamic
# -----------------------------
m_tfp_dyn_high <- feols(
  log_tfp_proxy ~ i(year, high_npi_z, ref = 1914) |
    city_id + year,
  cluster = ~city_id,
  data = df_tfp_high
)

m_tfp_dyn_days <- feols(
  log_tfp_proxy ~ i(year, markel_days_npi_z, ref = 1914) |
    city_id + year,
  cluster = ~city_id,
  data = df_tfp_days
)

m_tfp_dyn_speed <- feols(
  log_tfp_proxy ~ i(year, markel_speed_npi_z, ref = 1914) |
    city_id + year,
  cluster = ~city_id,
  data = df_tfp_speed
)

# =========================================================
# PART VI. ROBUSTNESS TABLES
# =========================================================

# Average DID robustness table
etable(
  m_emp_high, m_emp_days, m_emp_speed,
  m_tfp_high, m_tfp_days, m_tfp_speed,
  file = file.path(out_dir, "etable_employment_tfp_npi_robustness.tex")
)

# Dynamic robustness table
etable(
  m_emp_dyn_high, m_emp_dyn_days, m_emp_dyn_speed,
  m_tfp_dyn_high, m_tfp_dyn_days, m_tfp_dyn_speed,
  file = file.path(out_dir, "etable_dynamic_employment_tfp_npi_robustness.tex")
)

# =========================================================
# PART VII. COLLECT AVERAGE POST COEFFICIENTS
# =========================================================

beta_emp_high <- get_post_coef_info(m_emp_high, "high_npi_z", "Employment", "High NPI")
beta_emp_days <- get_post_coef_info(m_emp_days, "markel_days_npi_z", "Employment", "NPI days")
beta_emp_speed <- get_post_coef_info(m_emp_speed, "markel_speed_npi_z", "Employment", "NPI speed")

beta_tfp_high <- get_post_coef_info(m_tfp_high, "high_npi_z", "TFP proxy", "High NPI")
beta_tfp_days <- get_post_coef_info(m_tfp_days, "markel_days_npi_z", "TFP proxy", "NPI days")
beta_tfp_speed <- get_post_coef_info(m_tfp_speed, "markel_speed_npi_z", "TFP proxy", "NPI speed")

beta_compare_all <- bind_rows(
  beta_emp_high, beta_emp_days, beta_emp_speed,
  beta_tfp_high, beta_tfp_days, beta_tfp_speed
)

write_csv(beta_compare_all, file.path(out_dir, "beta_compare_employment_tfp_npi_robustness.csv"))

# simple bar plot
p_beta_compare <- ggplot(
  beta_compare_all,
  aes(x = specification, y = estimate, fill = specification)
) +
  geom_col(width = 0.6) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.2
  ) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    x = NULL,
    y = "Estimated post coefficient",
    title = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

print(p_beta_compare)

ggsave(
  filename = file.path(out_dir, "beta_compare_employment_tfp_npi_robustness.png"),
  plot = p_beta_compare,
  width = 10,
  height = 5.5
)

# =========================================================
# PART VIII. TIDY DYNAMIC COEFFICIENTS
# =========================================================

tidy_emp_dyn_high <- broom::tidy(m_emp_dyn_high)
tidy_emp_dyn_days <- broom::tidy(m_emp_dyn_days)
tidy_emp_dyn_speed <- broom::tidy(m_emp_dyn_speed)

tidy_tfp_dyn_high <- broom::tidy(m_tfp_dyn_high)
tidy_tfp_dyn_days <- broom::tidy(m_tfp_dyn_days)
tidy_tfp_dyn_speed <- broom::tidy(m_tfp_dyn_speed)

cat("\nUnique dynamic terms in employment high NPI model:\n")
print(unique(tidy_emp_dyn_high$term))

cat("\nUnique dynamic terms in TFP high NPI model:\n")
print(unique(tidy_tfp_dyn_high$term))

# Employment dynamic plot data
emp_dyn_high_df <- extract_year_terms(tidy_emp_dyn_high, "high_npi_z", "High NPI")
emp_dyn_days_df <- extract_year_terms(tidy_emp_dyn_days, "markel_days_npi_z", "NPI days")
emp_dyn_speed_df <- extract_year_terms(tidy_emp_dyn_speed, "markel_speed_npi_z", "NPI speed")
emp_base_df <- make_baseline_rows(c("High NPI", "NPI days", "NPI speed"), 1914)

emp_dyn_all_df <- bind_rows(
  emp_dyn_high_df,
  emp_dyn_days_df,
  emp_dyn_speed_df,
  emp_base_df
) %>%
  arrange(type, year_plot)

# TFP dynamic plot data
tfp_dyn_high_df <- extract_year_terms(tidy_tfp_dyn_high, "high_npi_z", "High NPI")
tfp_dyn_days_df <- extract_year_terms(tidy_tfp_dyn_days, "markel_days_npi_z", "NPI days")
tfp_dyn_speed_df <- extract_year_terms(tidy_tfp_dyn_speed, "markel_speed_npi_z", "NPI speed")
tfp_base_df <- make_baseline_rows(c("High NPI", "NPI days", "NPI speed"), 1914)

tfp_dyn_all_df <- bind_rows(
  tfp_dyn_high_df,
  tfp_dyn_days_df,
  tfp_dyn_speed_df,
  tfp_base_df
) %>%
  arrange(type, year_plot)

write_csv(emp_dyn_all_df, file.path(out_dir, "employment_dynamic_npi_robustness_coefficients.csv"))
write_csv(tfp_dyn_all_df, file.path(out_dir, "tfp_dynamic_npi_robustness_coefficients.csv"))

# Also save separate dynamic coefficient files
write_csv(emp_dyn_high_df, file.path(out_dir, "employment_dynamic_high_npi_coefficients.csv"))
write_csv(emp_dyn_days_df, file.path(out_dir, "employment_dynamic_days_npi_coefficients.csv"))
write_csv(emp_dyn_speed_df, file.path(out_dir, "employment_dynamic_speed_npi_coefficients.csv"))

write_csv(tfp_dyn_high_df, file.path(out_dir, "tfp_dynamic_high_npi_coefficients.csv"))
write_csv(tfp_dyn_days_df, file.path(out_dir, "tfp_dynamic_days_npi_coefficients.csv"))
write_csv(tfp_dyn_speed_df, file.path(out_dir, "tfp_dynamic_speed_npi_coefficients.csv"))

# =========================================================
# PART IX. DYNAMIC PLOTS
# =========================================================

# Combined employment dynamic plot
p_emp_dyn_all <- run_dynamic_plot(
  df_plot = emp_dyn_all_df,
  plot_title = NULL,
  outfile = file.path(out_dir, "employment_dynamic_npi_robustness.png")
)

# Combined TFP dynamic plot
p_tfp_dyn_all <- run_dynamic_plot(
  df_plot = tfp_dyn_all_df,
  plot_title = NULL,
  outfile = file.path(out_dir, "tfp_dynamic_npi_robustness.png")
)

# Optional: separate employment dynamic plots
p_emp_dyn_high <- run_dynamic_plot(
  df_plot = bind_rows(emp_dyn_high_df, make_baseline_rows("High NPI", 1914)),
  plot_title = "Dynamic Effects of High NPI on Manufacturing Employment",
  outfile = file.path(out_dir, "employment_dynamic_high_npi.png")
)

p_emp_dyn_days <- run_dynamic_plot(
  df_plot = bind_rows(emp_dyn_days_df, make_baseline_rows("NPI days", 1914)),
  plot_title = "Dynamic Effects of NPI Days on Manufacturing Employment",
  outfile = file.path(out_dir, "employment_dynamic_days_npi.png")
)

p_emp_dyn_speed <- run_dynamic_plot(
  df_plot = bind_rows(emp_dyn_speed_df, make_baseline_rows("NPI speed", 1914)),
  plot_title = "Dynamic Effects of NPI Speed on Manufacturing Employment",
  outfile = file.path(out_dir, "employment_dynamic_speed_npi.png")
)

# Optional: separate TFP dynamic plots
p_tfp_dyn_high <- run_dynamic_plot(
  df_plot = bind_rows(tfp_dyn_high_df, make_baseline_rows("High NPI", 1914)),
  plot_title = "Dynamic Effects of High NPI on Manufacturing TFP Proxy",
  outfile = file.path(out_dir, "tfp_dynamic_high_npi.png")
)

p_tfp_dyn_days <- run_dynamic_plot(
  df_plot = bind_rows(tfp_dyn_days_df, make_baseline_rows("NPI days", 1914)),
  plot_title = "Dynamic Effects of NPI Days on Manufacturing TFP Proxy",
  outfile = file.path(out_dir, "tfp_dynamic_days_npi.png")
)

p_tfp_dyn_speed <- run_dynamic_plot(
  df_plot = bind_rows(tfp_dyn_speed_df, make_baseline_rows("NPI speed", 1914)),
  plot_title = "Dynamic Effects of NPI Speed on Manufacturing TFP Proxy",
  outfile = file.path(out_dir, "tfp_dynamic_speed_npi.png")
)

# =========================================================
# PART X. SAVE MODELS
# =========================================================

# Average models
saveRDS(m_emp_high, file.path(out_dir, "model_emp_high_npi.rds"))
saveRDS(m_emp_days, file.path(out_dir, "model_emp_days_npi.rds"))
saveRDS(m_emp_speed, file.path(out_dir, "model_emp_speed_npi.rds"))

saveRDS(m_tfp_high, file.path(out_dir, "model_tfp_high_npi.rds"))
saveRDS(m_tfp_days, file.path(out_dir, "model_tfp_days_npi.rds"))
saveRDS(m_tfp_speed, file.path(out_dir, "model_tfp_speed_npi.rds"))

# Dynamic models
saveRDS(m_emp_dyn_high, file.path(out_dir, "model_emp_dynamic_high_npi.rds"))
saveRDS(m_emp_dyn_days, file.path(out_dir, "model_emp_dynamic_days_npi.rds"))
saveRDS(m_emp_dyn_speed, file.path(out_dir, "model_emp_dynamic_speed_npi.rds"))

saveRDS(m_tfp_dyn_high, file.path(out_dir, "model_tfp_dynamic_high_npi.rds"))
saveRDS(m_tfp_dyn_days, file.path(out_dir, "model_tfp_dynamic_days_npi.rds"))
saveRDS(m_tfp_dyn_speed, file.path(out_dir, "model_tfp_dynamic_speed_npi.rds"))

# =========================================================
# PART XI. PRINT RESULTS
# =========================================================

cat("\n============================\n")
cat("Employment models\n")
cat("============================\n")
print(summary(m_emp_high))
print(summary(m_emp_days))
print(summary(m_emp_speed))

cat("\n============================\n")
cat("TFP models\n")
cat("============================\n")
print(summary(m_tfp_high))
print(summary(m_tfp_days))
print(summary(m_tfp_speed))

cat("\n============================\n")
cat("Average beta comparison\n")
cat("============================\n")
print(beta_compare_all)

cat("\nDone. Results saved in folder:", out_dir, "\n")