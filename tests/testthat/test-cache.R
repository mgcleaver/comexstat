test_that("correlation table cache can be written and read back", {
  cache_root <- tempfile("cache-root-")
  dir.create(cache_root)
  withr::local_envvar(c(R_USER_CACHE_DIR = cache_root))

  table_data <- fixture_country_table()

  comexstat:::write_correlation_table_cache(
    table_code = "PAIS",
    table_data = table_data,
    source_url = "https://example.com/PAIS.csv",
    source_encoding = "Latin1"
  )

  cached <- comexstat:::read_correlation_table_cache("PAIS")

  expect_equal(cached$data, table_data)
  expect_equal(cached$meta$table_code, "PAIS")
  expect_equal(cached$meta$source_url, "https://example.com/PAIS.csv")
  expect_equal(cached$meta$source_encoding, "Latin1")
})

test_that("get_correlation_table_cached uses a fresh cache without downloading", {
  cached <- list(
    data = fixture_country_table(),
    meta = list(
      downloaded_at = Sys.time(),
      source_encoding = "UTF-8"
    )
  )

  result <- testthat::with_mocked_bindings(
    read_correlation_table_cache = function(table_code) cached,
    download_correlation_table_data = function(table_code) {
      stop("should not download")
    },
    code = comexstat:::get_correlation_table_cached("PAIS", verbose = FALSE),
    .package = "comexstat"
  )

  expect_equal(result, cached$data)
})

test_that("get_correlation_table_cached downloads and writes cache when needed", {
  written <- new.env(parent = emptyenv())
  written$value <- NULL

  download_result <- list(
    data = fixture_country_table(),
    source_url = "https://example.com/PAIS.csv",
    source_encoding = "Latin1"
  )

  result <- testthat::with_mocked_bindings(
    read_correlation_table_cache = function(table_code) NULL,
    download_correlation_table_data = function(table_code) download_result,
    write_correlation_table_cache = function(...) {
      written$value <- list(...)
      invisible(NULL)
    },
    code = comexstat:::get_correlation_table_cached("PAIS", verbose = FALSE),
    .package = "comexstat"
  )

  expect_equal(result, download_result$data)
  expect_equal(written$value$table_code, "PAIS")
  expect_equal(written$value$table_data, download_result$data)
})

test_that("get_correlation_table_cached falls back to cache when refresh fails", {
  cached <- list(
    data = fixture_country_table(),
    meta = list(
      downloaded_at = Sys.time() - (10 * 24 * 3600),
      source_encoding = "UTF-8"
    )
  )

  result <- testthat::with_mocked_bindings(
    read_correlation_table_cache = function(table_code) cached,
    download_correlation_table_data = function(table_code) {
      stop("download failed")
    },
    code = comexstat:::get_correlation_table_cached(
      "PAIS",
      refresh = TRUE,
      verbose = FALSE
    ),
    .package = "comexstat"
  )

  expect_equal(result, cached$data)
})

test_that("get_correlation_table_cached errors when download fails and no cache exists", {
  expect_error(
    testthat::with_mocked_bindings(
      read_correlation_table_cache = function(table_code) NULL,
      download_correlation_table_data = function(table_code) {
        stop("download failed")
      },
      code = comexstat:::get_correlation_table_cached("PAIS", verbose = FALSE),
      .package = "comexstat"
    ),
    "Failed to download 'PAIS' and no cache is available"
  )
})
