# Optional helper script:
# refresh local cache for dynamic correlation tables.

devtools::load_all()

invisible(refresh_cache())
