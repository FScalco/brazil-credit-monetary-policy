library(tidyverse)
library(janitor)

parse_ptbr_number <- function(x) {
  readr::parse_number(
    as.character(x),
    locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
  )
}

bndes_raw_file <- "C:/Users/chico/Downloads/179950b8-b504-4cc7-b0db-9c9eed99e9ba.csv"

# BNDES monthly disbursements raw file
# Source: BNDES Open Data portal
# File: desembolsos-mensais.csv
#
# Raw variables available in the CSV according to the data dictionary:
#  1. ano                    (numeric)
#  2. mes                    (numeric)
#  3. forma_de_apoio         (text)
#  4. produto                (text)
#  5. instrumento_financeiro (text)
#  6. inovacao               (text)
#  7. porte_de_empresa       (text)
#  8. regiao                 (text)
#  9. uf                     (text)
# 10. municipio              (text)
# 11. municipio_codigo       (numeric)
# 12. setor_cnae             (text)
# 13. subsetor_cnae_agrupado (text)
# 14. setor_bndes            (text)
# 15. subsetor_bndes         (text)
# 16. desembolsos_reais      (numeric)
#
# In this preprocessing step, only:
# - ano
# - desembolsos_reais
# are used to build annual BNDES disbursements.

bndes_raw <- readr::read_delim(
  file = bndes_raw_file,
  delim = ",",
  locale = readr::locale(encoding = "Windows-1252"),
  show_col_types = FALSE,
  progress = TRUE
) |>
  janitor::clean_names()

bndes_annual <- bndes_raw |>
  dplyr::transmute(
    year = as.integer(ano),
    disbursement_brl = parse_ptbr_number(desembolsos_reais)
  ) |>
  dplyr::filter(!is.na(year), !is.na(disbursement_brl)) |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    bndes_disbursement_brl = sum(disbursement_brl, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(
  bndes_annual,
  "C:/Users/chico/brazil-credit-monetary-policy/bndes_annual.csv"
)