# ============================================================
# Brazil vs Germany: Directed Credit and Development-Bank Proxy
# ============================================================
# Research design implemented in this script:
# 1) Comparable institutional comparison: BNDES vs KfW (annual FLOW metrics)
# 2) Broader context: Brazil official directed credit vs Germany proxy channels
#
# Conceptual guardrails:
# - Brazil's official directed credit is broader than BNDES.
# - KfW is an important German promotional channel, not a full German equivalent
#   of Brazil's directed-credit aggregate.

# ----------------
# 1) User settings
# ----------------
start_date <- as.Date("2003-01-01")
end_date <- Sys.Date()
output_dir <- "output"
save_plots <- TRUE
save_data <- TRUE

# -----------------------------------
# 2) Package installation and loading
# -----------------------------------
required_packages <- c(
  "tidyverse",
  "lubridate",
  "rbcb",
  "bundesbank",
  "janitor",
  "scales",
  "patchwork",
  "zoo",
  "httr2",
  "jsonlite",
  "readr",
  "glue"
)

safe_install_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

invisible(lapply(required_packages, safe_install_package))
invisible(lapply(required_packages, library, character.only = TRUE))

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -------------------
# 3) Helper functions
# -------------------
safe_api_get <- function(url) {
  tryCatch(
    {
      httr2::request(url) |>
        httr2::req_perform()
    },
    error = function(e) {
      warning(glue::glue("Request failed for URL: {url}\n{e$message}"))
      return(NULL)
    }
  )
}

clean_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x_chr <- as.character(x)

  suppressWarnings({
    d1 <- lubridate::ymd(x_chr)
    d2 <- lubridate::dmy(x_chr)
    d3 <- zoo::as.Date.yearqtr(x_chr, format = "%Y-Q%q")
  })

  dplyr::coalesce(d1, d2, d3)
}

calc_ratio <- function(numerator, denominator) {
  ifelse(
    is.na(numerator) | is.na(denominator) | denominator == 0,
    NA_real_,
    numerator / denominator
  )
}

plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey25"),
      axis.title = ggplot2::element_text(face = "bold")
    )
}


latest_value_summary_table <- function(data, date_col, value_cols, group_cols = NULL) {
  if (is.null(group_cols) || length(group_cols) == 0) {
    data |>
      dplyr::filter(!is.na(.data[[date_col]])) |>
      dplyr::slice_max(order_by = .data[[date_col]], n = 1, with_ties = FALSE) |>
      dplyr::select(dplyr::all_of(c(date_col, value_cols)))
  } else {
    data |>
      dplyr::filter(!is.na(.data[[date_col]])) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::slice_max(order_by = .data[[date_col]], n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::select(dplyr::all_of(c(group_cols, date_col, value_cols)))
  }
}

safe_get_series_sgs <- function(code, label, start_date, end_date) {
  raw <- tryCatch(
    rbcb::get_series(code = code, start_date = start_date, end_date = end_date),
    error = function(e) {
      warning(glue::glue("SGS download failed for {label} ({code}): {e$message}"))
      return(NULL)
    }
  )

  if (is.null(raw)) {
    return(tibble::tibble(date = as.Date(character()), value = numeric(), series = character()))
  }

  # rbcb often returns data.frame/tibble with date + value
  if (is.data.frame(raw)) {
    out <- tibble::as_tibble(raw)
    if (ncol(out) < 2) {
      warning(glue::glue("Unexpected SGS structure for {label} ({code})."))
      return(tibble::tibble(date = as.Date(character()), value = numeric(), series = character()))
    }
    names(out)[1:2] <- c("date", "value")
    return(
      out |>
        dplyr::mutate(
          date = as.Date(date),
          value = as.numeric(value),
          series = label
        ) |>
        dplyr::select(date, value, series)
    )
  }

  # fallback for named numeric vector
  if (is.numeric(raw) && !is.null(names(raw))) {
    return(
      tibble::tibble(
        date = clean_date(names(raw)),
        value = as.numeric(raw),
        series = label
      ) |>
        dplyr::filter(!is.na(date))
    )
  }

  warning(glue::glue("Unhandled SGS type for {label} ({code})."))
  tibble::tibble(date = as.Date(character()), value = numeric(), series = character())
}

safe_world_bank_gdp <- function(country_iso2) {
  # World Bank indicator: GDP current LCU (NY.GDP.MKTP.CN)
  url <- glue::glue(
    "https://api.worldbank.org/v2/country/{country_iso2}/indicator/NY.GDP.MKTP.CN",
    "?format=json&per_page=20000"
  )
  resp <- safe_api_get(url)
  if (is.null(resp)) return(tibble::tibble())

  payload <- jsonlite::fromJSON(httr2::resp_body_string(resp))
  if (length(payload) < 2 || nrow(payload[[2]]) == 0) {
    warning(glue::glue("No World Bank GDP data for {country_iso2}."))
    return(tibble::tibble())
  }

  tibble::as_tibble(payload[[2]]) |>
    dplyr::transmute(
      year = as.integer(date),
      gdp_current_lcu = as.numeric(value)
    ) |>
    dplyr::filter(!is.na(year), !is.na(gdp_current_lcu))
}

safe_bundesbank_get <- function(series_id, start = NULL, end = NULL) {
  out <- tryCatch(
    bundesbank::getSeries(
      series = series_id,
      start = start,
      end = end,
      return.class = "data.frame",
      verbose = FALSE
    ),
    error = function(e) {
      warning(glue::glue("Bundesbank download failed for {series_id}: {e$message}"))
      return(NULL)
    }
  )

  if (is.null(out) || nrow(out) == 0) return(tibble::tibble())

  tibble::as_tibble(out) |>
    dplyr::rename(date = dates, value = values) |>
    dplyr::mutate(series_id = series_id)
}

parse_ptbr_number <- function(x) {
  readr::parse_number(
    as.character(x),
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  )
}


# -------------------------
# 4) Data retrieval: Brazil
# -------------------------
# Official BCB SGS monthly stock series:
# - 20539 total credit
# - 20540 directed/earmarked credit
brazil_sgs_codes <- tibble::tribble(
  ~series, ~sgs_code,
  "credit_total_brl", 20539,
  "credit_directed_brl", 20540
)

brazil_credit_monthly <- purrr::pmap_dfr(
  list(brazil_sgs_codes$sgs_code, brazil_sgs_codes$series),
  ~ safe_get_series_sgs(..1, ..2, start_date, end_date)
) |>
  dplyr::mutate(date = lubridate::floor_date(date, "month")) |>
  dplyr::group_by(series, date) |>
  dplyr::summarise(value = dplyr::last(value), .groups = "drop") |>
  tidyr::pivot_wider(names_from = series, values_from = value) |>
  dplyr::arrange(date)

brazil_gdp_annual <- safe_world_bank_gdp("BR") |>
  dplyr::rename(gdp_brl = gdp_current_lcu)

brazil_monthly <- brazil_credit_monthly |>
  dplyr::mutate(year = lubridate::year(date)) |>
  dplyr::left_join(brazil_gdp_annual, by = "year") |>
  dplyr::mutate(
    directed_credit_share_total = calc_ratio(credit_directed_brl, credit_total_brl),
    # If official monthly directed-credit-to-GDP is not directly used from SGS,
    # reconstruct as monthly stock / annual nominal GDP for that year.
    directed_credit_share_gdp = calc_ratio(credit_directed_brl, gdp_brl)
  )

# --------------------------
# 5) Data retrieval: Germany
# --------------------------
# Bundesbank total-credit proxy (quarterly STOCK):
# Sum of short + long-term loans to households and NFCs from S12K creditor sector.
de_credit_series <- tibble::tribble(
  ~series_id, ~component,
  "BBAF3.Q.F41.S12K.DE.S14.DE.LE.N._X.B", "households_short_term",
  "BBAF3.Q.F42.S12K.DE.S14.DE.LE.N._X.B", "households_long_term",
  "BBAF3.Q.F41.S12K.DE.S11.DE.LE.N._X.B", "nfc_short_term",
  "BBAF3.Q.F42.S12K.DE.S11.DE.LE.N._X.B", "nfc_long_term"
)

germany_credit_quarterly_long <- purrr::map2_dfr(
  de_credit_series$series_id,
  de_credit_series$component,
  ~ safe_bundesbank_get(
      .x,
      start = format(start_date, "%Y-%m"),
      end = format(end_date, "%Y-%m")
    ) |>
    dplyr::mutate(component = .y)
)

germany_credit_quarterly <- germany_credit_quarterly_long |>
  dplyr::mutate(date = clean_date(date)) |>
  dplyr::filter(!is.na(date)) |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    germany_total_credit_eur = sum(value, na.rm = TRUE) * 1e9, # Bundesbank unit is billions of EUR
    .groups = "drop"
  ) |>
  dplyr::mutate(
    year = lubridate::year(date),
    quarter = lubridate::quarter(date)
  )

germany_gdp_annual <- safe_world_bank_gdp("DE") |>
  dplyr::rename(gdp_eur = gdp_current_lcu)

# KfW annual promotional/public development proxy (FLOW).
# Values entered manually from official KfW annual reports / press releases.
# This is intentionally manual to avoid brittle HTML/PDF scraping.
kfw_annual_raw <- tibble::tribble(
  ~year, ~kfw_promotional_business_eur_bn, ~source_note,
  2020, NA_real_, "Fill from official KfW source",
  2021, NA_real_, "Fill from official KfW source",
  2022, NA_real_, "Fill from official KfW source",
  2023, NA_real_, "Fill from official KfW source",
  2024, 112.8,    "Official KfW promotional/business figure"
)




# BNDES annual disbursements (FLOW), read from a preprocessed local file.
bndes_annual_file <- "bndes_annual.csv"

if (!file.exists(bndes_annual_file)) {
  stop(
    paste(
      "Missing preprocessed BNDES file:", bndes_annual_file,
      "\nRun the preprocessing script first."
    )
  )
}

bndes_annual <- readr::read_csv(
  file = bndes_annual_file,
  show_col_types = FALSE
)



# --------------------------------------
# 6) Data cleaning and harmonization
# --------------------------------------
brazil_monthly_metrics <- brazil_monthly |>
  dplyr::transmute(
    date,
    year,
    credit_total_brl,
    credit_directed_brl,
    directed_credit_share_total,
    directed_credit_share_gdp
  )

brazil_annual_context <- brazil_monthly_metrics |>
  dplyr::group_by(year) |>
  dplyr::slice_max(order_by = date, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(
    year,
    brazil_directed_credit_share_total = directed_credit_share_total,
    brazil_directed_credit_share_gdp = directed_credit_share_gdp,
    brazil_directed_credit_brl = credit_directed_brl
  )

germany_credit_annual <- germany_credit_quarterly |>
  dplyr::group_by(year) |>
  dplyr::slice_max(order_by = quarter, n = 1, with_ties = FALSE) |>
  dplyr::ungroup() |>
  dplyr::select(year, germany_total_credit_eur)

# ----------------
# 7) Calculations
# ----------------
# Brazil
brazil_monthly_metrics <- brazil_monthly_metrics |>
  dplyr::mutate(
    directed_credit_share_total = calc_ratio(credit_directed_brl, credit_total_brl),
    directed_credit_share_gdp = directed_credit_share_gdp
  )

# BNDES vs KfW (comparable annual FLOW / FLOW-to-GDP)
bndes_vs_kfw_annual <- bndes_annual |>
  dplyr::left_join(brazil_gdp_annual, by = "year") |>
  dplyr::left_join(kfw_annual_raw, by = "year") |>
  dplyr::left_join(germany_gdp_annual, by = "year") |>
  dplyr::mutate(
    bndes_share_gdp = calc_ratio(bndes_disbursement_brl, gdp_brl),
    kfw_share_gdp = calc_ratio(kfw_promotional_business_eur_bn * 1e9, gdp_eur)
  )

context_annual <- brazil_annual_context |>
  dplyr::left_join(
    bndes_vs_kfw_annual |>
      dplyr::select(year, bndes_share_gdp, kfw_share_gdp),
    by = "year"
  )

# ----------------
# 8) Output tables
# ----------------
brazil_latest_table <- latest_value_summary_table(
  data = brazil_monthly_metrics,
  date_col = "date",
  value_cols = c("directed_credit_share_total", "directed_credit_share_gdp")
)

institution_latest_table <- latest_value_summary_table(
  data = bndes_vs_kfw_annual |>
    dplyr::filter(!is.na(bndes_share_gdp) | !is.na(kfw_share_gdp)) |>
    dplyr::mutate(date = as.Date(paste0(year, "-12-31"))),
  date_col = "date",
  value_cols = c("year", "bndes_share_gdp", "kfw_share_gdp")
)

metadata_table <- tibble::tribble(
  ~series_name, ~source, ~unit, ~frequency, ~stock_or_flow,
  "Brazil total credit outstanding", "BCB SGS via rbcb (20539)", "BRL", "Monthly", "Stock",
  "Brazil directed credit outstanding", "BCB SGS via rbcb (20540)", "BRL", "Monthly", "Stock",
  "Brazil GDP", "World Bank API NY.GDP.MKTP.CN", "BRL", "Annual", "Flow",
  "BNDES disbursements", "BNDES Open Data desembolsos-mensais (aggregated)", "BRL", "Monthly->Annual", "Flow",
  "Germany total credit proxy", "Bundesbank BBAF3 loan stock series via bundesbank package", "EUR", "Quarterly", "Stock",
  "Germany GDP", "World Bank API NY.GDP.MKTP.CN", "EUR", "Annual", "Flow",
  "KfW promotional/public development proxy", "Manual entry from official KfW annual reports / press releases", "EUR billion", "Annual", "Flow"
)

print("Latest-value summary table: Brazil directed credit metrics")
print(brazil_latest_table)

print("Latest-value summary table: BNDES vs KfW metrics")
print(institution_latest_table)

print("Series metadata table")
print(metadata_table)

# -------
# 9) Plots
# -------
country_colors <- c(
  "Brazil" = "#1b9e77",
  "Germany" = "#d95f02"
)

# Figure 1: Brazil directed credit
fig1_panel_a <- brazil_monthly_metrics |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = directed_credit_share_total)) +
  ggplot2::geom_line(color = country_colors[["Brazil"]], linewidth = 0.8) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  ggplot2::labs(
    title = "Figure 1A. Brazil Directed Credit as Share of Total Credit",
    subtitle = "Official BCB directed (earmarked) credit divided by total credit",
    x = NULL,
    y = "% of total credit",
    caption = "Source: BCB SGS via rbcb (20539, 20540)."
  ) +
  plot_theme()

fig1_panel_b <- brazil_monthly_metrics |>
  ggplot2::ggplot(ggplot2::aes(x = date, y = directed_credit_share_gdp)) +
  ggplot2::geom_line(color = country_colors[["Brazil"]], linewidth = 0.8) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  ggplot2::scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  ggplot2::labs(
    title = "Figure 1B. Brazil Directed Credit as Share of GDP",
    subtitle = "Monthly directed credit stock divided by annual nominal GDP",
    x = NULL,
    y = "% of GDP",
    caption = "Source: BCB SGS via rbcb; GDP from World Bank."
  ) +
  plot_theme()

figure_1 <- fig1_panel_a / fig1_panel_b +
  patchwork::plot_annotation(
    title = "Figure 1. Brazil Directed Credit",
    subtitle = "Official directed-credit concept is broader than BNDES"
  )

# Figure 2: BNDES vs KfW (comparable institutional metric as % GDP)
fig2_data <- bndes_vs_kfw_annual |>
  dplyr::select(year, bndes_share_gdp, kfw_share_gdp) |>
  tidyr::pivot_longer(
    cols = c(bndes_share_gdp, kfw_share_gdp),
    names_to = "series",
    values_to = "value"
  ) |>
  dplyr::mutate(
    series = dplyr::recode(
      series,
      bndes_share_gdp = "BNDES annual disbursements / Brazil GDP",
      kfw_share_gdp = "KfW annual promotional business / Germany GDP"
    )
  ) |>
  dplyr::filter(!is.na(value))

figure_2 <- fig2_data |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = value, color = series)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 1.8) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  ggplot2::scale_color_manual(values = c(
    "BNDES annual disbursements / Brazil GDP" = country_colors[["Brazil"]],
    "KfW annual promotional business / Germany GDP" = country_colors[["Germany"]]
  )) +
  ggplot2::labs(
    title = "Figure 2. Comparable Institutional Comparison: BNDES vs KfW",
    subtitle = "Both series are annual flow metrics scaled by annual GDP (no stock-flow mixing)",
    x = NULL,
    y = "% of GDP",
    color = NULL,
    caption = paste(
      "Sources: BNDES Open Data, KfW official annual reports / press releases,",
      "World Bank GDP."
    )
  ) +
  plot_theme()



print(figure_1)
print(figure_2)

if (save_plots) {
  ggplot2::ggsave(
    filename = file.path(output_dir, "figure_1_brazil_directed_credit.png"),
    plot = figure_1,
    width = 11, height = 8, dpi = 300
  )
  
  ggplot2::ggsave(
    filename = file.path(output_dir, "figure_2_bndes_vs_kfw.png"),
    plot = figure_2,
    width = 11, height = 6, dpi = 300
  )
}

# ---------------------
# 10) DATA CAVEATS
# ---------------------
# DATA CAVEATS
#
# - Brazil's directed credit (BCB official concept) is a broad aggregate that
#   includes more than BNDES, including housing, rural, and other earmarked credit.
#
# - KfW is NOT the full German equivalent of Brazil's directed credit.
#   In this script, KfW is explicitly treated as a proxy for one major
#   promotional/public development lending channel in Germany.
#
# - Any Germany-side comparison beyond KfW is suggestive. The German total-credit
#   context is proxied using selected Bundesbank loan-stock series for households
#   and non-financial corporations.
#
# - Stock vs flow:
#   * Stocks: Brazil total credit, Brazil directed credit, Germany total-credit proxy
#   * Flows: BNDES disbursements, KfW promotional business, GDP
#   * Figure 2 uses flow metrics for both institutions, divided by annual GDP.
#
# - Frequency:
#   * Brazil directed-credit analysis: monthly
#   * Germany total-credit proxy: quarterly (preserved, then annualized via year-end)
#   * BNDES vs KfW comparison: annual
#
# - Transformations:
#   * No fake monthly German directed-credit series was created.
#   * No interpolation was used to force KfW annual values to monthly.
#   * BNDES monthly disbursements are aggregated to annual sums.
#   * Brazil directed-credit/GDP monthly ratio uses annual GDP denominator by year.
#
# - Data limitations:
#   * KfW values are entered manually from official KfW annual reports or
#     press releases and should be updated manually as new releases appear.