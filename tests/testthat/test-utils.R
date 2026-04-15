test_that("normalize_correlation_table_name validates and uppercases inputs", {
  expect_equal(comexstat:::normalize_correlation_table_name("pais"), "PAIS")
  expect_error(comexstat:::normalize_correlation_table_name(NA_character_), "single")
  expect_error(comexstat:::normalize_correlation_table_name("foo"), "must be one of")
})

test_that("resolve_correlation_table_url handles absolute and relative paths", {
  expect_equal(
    comexstat:::resolve_correlation_table_url("https://example.com/PAIS.csv"),
    "https://example.com/PAIS.csv"
  )
  expect_equal(
    comexstat:::resolve_correlation_table_url("/balanca/bd/tabelas/PAIS.csv"),
    "https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv"
  )
  expect_equal(
    comexstat:::resolve_correlation_table_url("PAIS.csv"),
    "https://balanca.economia.gov.br/balanca/bd/tabelas/PAIS.csv"
  )
})

test_that("rename_columns_if_present renames known aliases and preserves others", {
  input <- tibble::tibble(
    co_pais = 10L,
    no_pais_ing = "Brazil",
    untouched = "ok"
  )

  result <- comexstat:::rename_columns_if_present(input)

  expect_named(result, c("country_code", "country_name", "untouched"))
  expect_equal(result$country_name, "Brazil")
})

test_that("post_process_correlation_table pads NCM codes to 8 digits", {
  result <- comexstat:::post_process_correlation_table(
    tibble::tibble(ncm = c("1", "12345678"), value = c(1L, 2L)),
    "NCM"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(result$ncm, c("00000001", "12345678"))
})

test_that("cache time helpers detect stale data and format UTC timestamps", {
  fresh_meta <- list(downloaded_at = Sys.time() - 3600)
  stale_meta <- list(downloaded_at = Sys.time() - (10 * 24 * 3600))

  expect_false(comexstat:::is_correlation_cache_stale(fresh_meta, max_age_days = 1))
  expect_true(comexstat:::is_correlation_cache_stale(stale_meta, max_age_days = 1))
  expect_equal(
    comexstat:::format_cache_datetime(as.POSIXct("2024-01-02 03:04:05", tz = "UTC")),
    "2024-01-02 03:04:05 UTC"
  )
})

test_that("correlation tables are read with the best encoding and normalized to UTF-8", {
  path <- tempfile(fileext = ".csv")
  write_latin1_csv(
    path,
    c(
      "CO_PAIS;NO_PAIS",
      "105;São Tomé e Príncipe"
    )
  )

  selected <- comexstat:::read_correlation_table_with_best_encoding(
    path,
    encodings = c("UTF-8", "Latin1")
  )
  table <- comexstat:::read_correlation_table(path)

  expect_equal(selected$encoding, "Latin1")
  expect_equal(selected$data$NO_PAIS[[1]], "São Tomé e Príncipe")
  expect_equal(attr(table, "source_encoding", exact = TRUE), "Latin1")
  expect_named(table, c("co_pais", "no_pais"))
  expect_equal(table$no_pais[[1]], "São Tomé e Príncipe")
})
