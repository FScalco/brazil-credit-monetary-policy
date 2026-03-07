# Pull and prepare monthly macro-financial data from BCB SGS API
# Series requested: Selic, free credit total, earmarked credit total, IPCA, industrial production.
#
# Notes:
# - This script uses the `rbcb` package to access SGS data.
# - Series codes can change or be revised; verify codes at:
#   https://dadosabertos.bcb.gov.br/dataset/11-taxa-de-juros---selic
#   and SGS search: https://www3.bcb.gov.br/sgspub/

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(lubridate)
  library(zoo)
  library(rbcb)
})

# -----------------------------
# 1) Define series and metadata
# -----------------------------
# IMPORTANT: Confirm these SGS codes in your project before final estimation.
series_map <- tibble::tribble(
  ~series_name,                ~sgs_code,
  "selic_rate",                    432,   # Selic target rate (% a.a.)
  "credit_free_total",           20539,   # Free credit total (verify)
  "credit_earmarked_total",      20540,   # Earmarked credit total (verify)
  "ipca_inflation",                433,   # IPCA monthly inflation (%)
  "industrial_production_index",  21859   # Industrial production / activity proxy (verify)
)

# Set sample window (monthly output in this range)
start_date <- as.Date("2003-01-01")
end_date   <- Sys.Date()

# -------------------------------------------------
# 2) Function to download one series using `rbcb`
# -------------------------------------------------
fetch_sgs_series <- function(sgs_code, series_name, start_date, end_date) {
  # Download from SGS through rbcb
  raw <- tryCatch(
    rbcb::get_series(
      code = sgs_code,
      start_date = start_date,
      end_date = end_date
    ),
    error = function(e) {
      warning(sprintf("Failed to download %s (SGS %s): %s", series_name, sgs_code, e$message))
      return(NULL)
    }
  )

  # Handle empty response safely
  if (is.null(raw) || length(raw) == 0 || (is.data.frame(raw) && nrow(raw) == 0)) {
    warning(sprintf("No data returned for %s (SGS %s)", series_name, sgs_code))
    return(tibble(date = as.Date(character()), value = numeric(), series = character()))
  }

  # Convert different possible rbcb return formats to a tidy tibble
  if (inherits(raw, "xts")) {
    out <- tibble(
      date = as.Date(zoo::index(raw)),
      value = as.numeric(raw[, 1]),
      series = series_name
    )
  } else {
    raw_tbl <- tibble::as_tibble(raw)
    col_names <- names(raw_tbl)

    date_col <- col_names[grepl("^date$|ref[._]?date|^data$", col_names, ignore.case = TRUE)][1]
    value_col <- col_names[grepl("^value$|^valor$", col_names, ignore.case = TRUE)][1]

    # Fallback: if value column is not explicitly named, use first non-date column.
    if (is.na(value_col) || length(value_col) == 0) {
      non_date_cols <- setdiff(col_names, date_col)
      value_col <- non_date_cols[1]
    }

    out <- raw_tbl |>
      transmute(
        date = as.Date(.data[[date_col]]),
        value = as.numeric(.data[[value_col]]),
        series = series_name
      )
  }

  out |>
    filter(!is.na(date))
}

# ------------------------------------------
# 3) Download all requested SGS time series
# ------------------------------------------
all_long <- pmap_dfr(
  list(series_map$sgs_code, series_map$series_name),
  ~ fetch_sgs_series(..1, ..2, start_date, end_date)
)

# -------------------------------------------------
# 4) Align to monthly frequency and keep one value
# -------------------------------------------------
# Rule: for each series and month, keep the last available observation in the month.
# This works well when some series are daily and others are monthly.
monthly_long <- all_long |>
  mutate(month = floor_date(date, unit = "month")) |>
  group_by(series, month) |>
  summarise(value = dplyr::last(value[!is.na(value)]), .groups = "drop")

# ------------------------------------------------------
# 5) Pivot to wide format and build full monthly calendar
# ------------------------------------------------------
monthly_wide <- monthly_long |>
  tidyr::pivot_wider(names_from = series, values_from = value)

full_calendar <- tibble(
  month = seq.Date(
    from = floor_date(start_date, "month"),
    to   = floor_date(end_date, "month"),
    by   = "month"
  )
)

panel_raw <- full_calendar |>
  left_join(monthly_wide, by = "month") |>
  arrange(month)

# -----------------------------------------
# 6) Handle missing values (transparent)
# -----------------------------------------
# Step 1: forward-fill each series (LOCF)
# Step 2: backward-fill remaining leading gaps
# This gives a complete balanced panel while preserving observed paths.
panel_clean <- panel_raw |>
  mutate(across(-month, ~ na.locf(.x, na.rm = FALSE))) |>
  mutate(across(-month, ~ na.locf(.x, fromLast = TRUE, na.rm = FALSE)))

# Optional: if you prefer interpolation for interior gaps instead of LOCF, use:
# panel_clean <- panel_raw |>
#   mutate(across(-month, ~ na.approx(.x, na.rm = FALSE))) |>
#   mutate(across(-month, ~ na.locf(.x, na.rm = FALSE))) |>
#   mutate(across(-month, ~ na.locf(.x, fromLast = TRUE, na.rm = FALSE)))

# -------------------------------------
# 7) Basic checks and export final data
# -------------------------------------
message("Rows in final monthly panel: ", nrow(panel_clean))
message("Date range: ", min(panel_clean$month), " to ", max(panel_clean$month))

# Inspect missingness after cleaning
na_count <- sapply(panel_clean, function(x) sum(is.na(x)))
print(na_count)

# Save merged dataset to CSV in working directory
output_file <- "brazil_credit_monetary_monthly_panel.csv"
write_csv(panel_clean, output_file)
message("Saved: ", normalizePath(output_file, winslash = "/", mustWork = FALSE))

# The final dataframe is `panel_clean`.
# Columns: month, selic_rate, credit_free_total, credit_earmarked_total,
#          ipca_inflation, industrial_production_index
