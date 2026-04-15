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

test_that("read_correlation_table_cache invalidates legacy dynamic cache files", {
  cache_root <- tempfile("cache-root-")
  dir.create(cache_root)
  withr::local_envvar(c(R_USER_CACHE_DIR = cache_root))

  table_code <- "NCM_CGCE"
  data_path <- comexstat:::correlation_cache_data_path(table_code)
  meta_path <- comexstat:::correlation_cache_meta_path(table_code)

  saveRDS(
    tibble::tibble(
      bec_n3_code = "111",
      bec_n3_desc = "Primary food"
    ),
    data_path
  )
  saveRDS(list(downloaded_at = Sys.time(), source_encoding = "UTF-8"), meta_path)

  cached <- comexstat:::read_correlation_table_cache(table_code)

  expect_null(cached)
  expect_false(file.exists(data_path))
  expect_false(file.exists(meta_path))
})

test_that("clear_cache removes only dynamic correlation table cache files", {
  cache_root <- tempfile("cache-root-")
  dir.create(cache_root)
  withr::local_envvar(c(R_USER_CACHE_DIR = cache_root))

  dynamic_codes <- c("PAIS", "NCM", "NCM_ISIC", "NCM_CGCE", "NCM_CUCI")
  preserved_codes <- c("UF", "NCM_UNIDADE")

  all_codes <- c(dynamic_codes, preserved_codes)

  purrr::walk(
    all_codes,
    \(table_code) {
      comexstat:::write_correlation_table_cache(
        table_code = table_code,
        table_data = fixture_country_table(),
        source_url = paste0("https://example.com/", table_code, ".csv"),
        source_encoding = "UTF-8"
      )
    }
  )

  removed_paths <- comexstat::clear_cache()

  dynamic_paths <- tibble::tibble(table_code = dynamic_codes) |>
    dplyr::mutate(
      data_path = purrr::map_chr(table_code, comexstat:::correlation_cache_data_path),
      meta_path = purrr::map_chr(table_code, comexstat:::correlation_cache_meta_path)
    ) |>
    tidyr::pivot_longer(
      cols = c(data_path, meta_path),
      names_to = "cache_type",
      values_to = "path"
    ) |>
    dplyr::pull(path)

  preserved_paths <- tibble::tibble(table_code = preserved_codes) |>
    dplyr::mutate(
      data_path = purrr::map_chr(table_code, comexstat:::correlation_cache_data_path),
      meta_path = purrr::map_chr(table_code, comexstat:::correlation_cache_meta_path)
    ) |>
    tidyr::pivot_longer(
      cols = c(data_path, meta_path),
      names_to = "cache_type",
      values_to = "path"
    ) |>
    dplyr::pull(path)

  expect_setequal(removed_paths, dynamic_paths)
  expect_false(any(file.exists(dynamic_paths)))
  expect_true(all(file.exists(preserved_paths)))
})

test_that("refresh_cache updates only dynamic correlation tables", {
  calls <- new.env(parent = emptyenv())
  calls$table_codes <- character(0)
  calls$refresh <- logical(0)

  refreshed <- testthat::with_mocked_bindings(
    get_correlation_table_cached = function(name, refresh = FALSE, ...) {
      calls$table_codes <- c(calls$table_codes, name)
      calls$refresh <- c(calls$refresh, refresh)

      tibble::tibble(table_code = name)
    },
    code = comexstat::refresh_cache(),
    .package = "comexstat"
  )

  expect_identical(
    calls$table_codes,
    c("PAIS", "NCM", "NCM_ISIC", "NCM_CGCE", "NCM_CUCI")
  )
  expect_true(all(calls$refresh))
  expect_named(
    refreshed,
    c("PAIS", "NCM", "NCM_ISIC", "NCM_CGCE", "NCM_CUCI")
  )
  expect_false(any(calls$table_codes %in% c("UF", "NCM_UNIDADE")))
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

test_that("get_correlation_table_cached redownloads after invalidating legacy cache", {
  cache_root <- tempfile("cache-root-")
  dir.create(cache_root)
  withr::local_envvar(c(R_USER_CACHE_DIR = cache_root))

  table_code <- "NCM_CGCE"
  data_path <- comexstat:::correlation_cache_data_path(table_code)
  meta_path <- comexstat:::correlation_cache_meta_path(table_code)
  download_result <- list(
    data = fixture_bec_table(),
    source_url = "https://example.com/NCM_CGCE.csv",
    source_encoding = "Latin1"
  )

  saveRDS(
    tibble::tibble(
      bec_n3_code = "111",
      bec_n3_desc = "Primary food"
    ),
    data_path
  )
  saveRDS(list(downloaded_at = Sys.time(), source_encoding = "UTF-8"), meta_path)

  result <- testthat::with_mocked_bindings(
    download_correlation_table_data = function(table_code) download_result,
    code = comexstat:::get_correlation_table_cached(table_code, verbose = FALSE),
    .package = "comexstat"
  )

  cached <- comexstat:::read_correlation_table_cache(table_code)

  expect_equal(result, download_result$data)
  expect_equal(cached$data, download_result$data)
  expect_equal(cached$meta$source_url, download_result$source_url)
  expect_equal(cached$meta$source_encoding, download_result$source_encoding)
  expect_true(file.exists(data_path))
  expect_true(file.exists(meta_path))
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

test_that("get_correlation_table_cached errors when only legacy cache exists", {
  cache_root <- tempfile("cache-root-")
  dir.create(cache_root)
  withr::local_envvar(c(R_USER_CACHE_DIR = cache_root))

  table_code <- "NCM_CGCE"
  data_path <- comexstat:::correlation_cache_data_path(table_code)
  meta_path <- comexstat:::correlation_cache_meta_path(table_code)

  saveRDS(
    tibble::tibble(
      bec_n3_code = "111",
      bec_n3_desc = "Primary food"
    ),
    data_path
  )
  saveRDS(list(downloaded_at = Sys.time(), source_encoding = "UTF-8"), meta_path)

  expect_error(
    testthat::with_mocked_bindings(
      download_correlation_table_data = function(table_code) {
        stop("download failed")
      },
      code = comexstat:::get_correlation_table_cached(table_code, verbose = FALSE),
      .package = "comexstat"
    ),
    "Failed to download 'NCM_CGCE' and no cache is available"
  )

  expect_false(file.exists(data_path))
  expect_false(file.exists(meta_path))
})
