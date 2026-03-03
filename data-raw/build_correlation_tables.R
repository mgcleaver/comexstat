# Optional helper script:
# refresh local cache for dynamic correlation tables.

devtools::load_all()

invisible(get_country_table(refresh = TRUE))
invisible(get_ncm_table(refresh = TRUE))
invisible(get_isic_table(refresh = TRUE))
invisible(get_bec_table(refresh = TRUE))
invisible(get_cuci_table(refresh = TRUE))
