test_that("read_exports aggregates duplicated rows and pads NCM codes", {
  path <- tempfile(fileext = ".csv")
  write_trade_csv(
    path,
    data.frame(
      CO_ANO = c(2024, 2024, 2024),
      CO_MES = c(1, 1, 2),
      CO_NCM = c(1234, 1234, 99),
      SG_UF_NCM = c("SP", "SP", "RJ"),
      CO_PAIS = c(10, 10, 20),
      KG_LIQUIDO = c(1, 2, 5),
      QT_ESTAT = c(2, 3, 7),
      VL_FOB = c(10, 20, 30)
    )
  )

  result <- comexstat:::read_exports(path)

  expect_equal(nrow(result), 2L)
  expect_equal(result$month, c(1, 2))
  expect_equal(result$ncm, c("00001234", "00000099"))
  expect_equal(result$fob_value, c(30, 30))
  expect_equal(result$kg, c(3, 5))
  expect_equal(result$qty, c(5, 7))
})

test_that("read_imports aggregates rows and computes cif_value", {
  path <- tempfile(fileext = ".csv")
  write_trade_csv(
    path,
    data.frame(
      CO_ANO = c(2024, 2024, 2024),
      CO_MES = c(1, 1, 2),
      CO_NCM = c(1234, 1234, 99),
      SG_UF_NCM = c("SP", "SP", "RJ"),
      CO_PAIS = c(10, 10, 20),
      KG_LIQUIDO = c(1, 2, 5),
      QT_ESTAT = c(2, 3, 7),
      VL_FOB = c(10, 20, 30),
      VL_FRETE = c(1, 2, 3),
      VL_SEGURO = c(4, 5, 6)
    )
  )

  result <- comexstat:::read_imports(path)

  expect_equal(nrow(result), 2L)
  expect_equal(result$month, c(1, 2))
  expect_equal(result$ncm, c("00001234", "00000099"))
  expect_equal(result$fob_value, c(30, 30))
  expect_equal(result$cif_value, c(42, 39))
  expect_equal(result$kg, c(3, 5))
  expect_equal(result$qty, c(5, 7))
})

test_that("download_cs_file reports a clear error after retry exhaustion", {
  expect_error(
    testthat::with_mocked_bindings(
      GET = function(...) {
        stop("network down")
      },
      code = testthat::with_mocked_bindings(
        Sys.sleep = function(...) NULL,
        code = comexstat:::download_cs_file(
          "https://example.com/ncm/IMP_2024.csv",
          tempfile(fileext = ".csv")
        ),
        .package = "base"
      ),
      .package = "httr"
    ),
    "Failed to download file for year 2024 after 3 attempts"
  )
})

test_that("build_db routes import downloads through the import pipeline", {
  tracker <- new.env(parent = emptyenv())
  tracker$download <- NULL
  tracker$read_path <- NULL
  tracker$write <- NULL

  testthat::with_mocked_bindings(
    download_cs_file = function(link_download, dir_file_download) {
      tracker$download <- list(
        link_download = link_download,
        dir_file_download = dir_file_download
      )
      writeLines("stub", dir_file_download)
      invisible(NULL)
    },
    read_imports = function(path) {
      tracker$read_path <- path
      tibble::tibble(year = 2024L, month = 1L)
    },
    write_cs_db = function(x, path, data_schema) {
      tracker$write <- list(x = x, path = path, data_schema = data_schema)
      invisible(NULL)
    },
    code = comexstat:::build_db(
      "https://example.com/ncm/IMP_2024.csv",
      db_dirs = c("tmp/export", "tmp/import"),
      schemas = list(imp = "imp_schema", exp = "exp_schema")
    ),
    .package = "comexstat"
  )

  expect_match(tracker$download$link_download, "IMP_2024")
  expect_equal(tracker$read_path, tracker$download$dir_file_download)
  expect_equal(tracker$write$path, "tmp/import")
  expect_equal(tracker$write$data_schema, "imp_schema")
})

test_that("build_db routes export downloads through the export pipeline", {
  tracker <- new.env(parent = emptyenv())
  tracker$write <- NULL

  testthat::with_mocked_bindings(
    download_cs_file = function(link_download, dir_file_download) {
      writeLines("stub", dir_file_download)
      invisible(NULL)
    },
    read_exports = function(path) {
      tibble::tibble(year = 2024L, month = 1L)
    },
    write_cs_db = function(x, path, data_schema) {
      tracker$write <- list(x = x, path = path, data_schema = data_schema)
      invisible(NULL)
    },
    code = comexstat:::build_db(
      "https://example.com/ncm/EXP_2024.csv",
      db_dirs = c("tmp/export", "tmp/import"),
      schemas = list(imp = "imp_schema", exp = "exp_schema")
    ),
    .package = "comexstat"
  )

  expect_equal(tracker$write$path, "tmp/export")
  expect_equal(tracker$write$data_schema, "exp_schema")
})

test_that("create_cs_db filters years and ignores unwanted download links", {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  start_year <- current_year - 1L
  dest_dir <- tempfile("comexstat-create-")

  links <- c(
    sprintf("/ncm/EXP_%d.csv", current_year - 2L),
    sprintf("/ncm/IMP_%d.csv", current_year - 2L),
    sprintf("/ncm/EXP_%d.csv", current_year - 1L),
    sprintf("/ncm/IMP_%d.csv", current_year - 1L),
    sprintf("/ncm/EXP_%d.csv", current_year),
    sprintf("/ncm/IMP_%d.csv", current_year),
    sprintf("/ncm/EXP_COMPLETA_%d.csv", current_year),
    sprintf("/ncm/IMP_CONFERENCIA_%d.csv", current_year)
  )
  html_path <- write_download_page(links)
  captured <- character()

  testthat::with_mocked_bindings(
    cs_base_url = html_path,
    build_db = function(link_download, db_dirs, schemas) {
      captured <<- c(captured, link_download)
      invisible(NULL)
    },
    code = comexstat::create_cs_db(
      dest_dir = dest_dir,
      start_year = start_year,
      timeout = 1
    ),
    .package = "comexstat"
  )

  expected <- c(
    sprintf("/ncm/EXP_%d.csv", current_year - 1L),
    sprintf("/ncm/IMP_%d.csv", current_year - 1L),
    sprintf("/ncm/EXP_%d.csv", current_year),
    sprintf("/ncm/IMP_%d.csv", current_year)
  )

  expect_true(dir.exists(file.path(dest_dir, "export")))
  expect_true(dir.exists(file.path(dest_dir, "import")))
  expect_equal(sort(captured), sort(expected))
})

test_that("update_cs_db errors when local datasets cannot be opened", {
  dest_dir <- tempfile("comexstat-invalid-")

  expect_error(
    comexstat::update_cs_db(dest_dir = dest_dir, timeout = 1),
    "cannot be updated"
  )
})

test_that("update_cs_db stops when the local database is already updated", {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  dest_dir <- tempfile("comexstat-updated-")

  write_year_month_dataset(
    file.path(dest_dir, "export"),
    tibble::tibble(year = c(current_year, current_year), month = c(1L, 3L))
  )
  write_year_month_dataset(
    file.path(dest_dir, "import"),
    tibble::tibble(year = c(current_year, current_year), month = c(2L, 3L))
  )

  expect_error(
    testthat::with_mocked_bindings(
      compare_local_db = function(file_dir) TRUE,
      get_last_update = function() sprintf("%d-03", current_year),
      code = comexstat::update_cs_db(dest_dir = dest_dir, timeout = 1),
      .package = "comexstat"
    ),
    "already updated"
  )
})

test_that("update_cs_db infers the earliest start_year across import and export", {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  earliest_year <- current_year - 4L
  middle_year <- current_year - 2L
  dest_dir <- tempfile("comexstat-update-")

  write_year_month_dataset(
    file.path(dest_dir, "export"),
    tibble::tibble(year = c(middle_year, current_year), month = c(12L, 1L))
  )
  write_year_month_dataset(
    file.path(dest_dir, "import"),
    tibble::tibble(year = c(earliest_year, current_year), month = c(12L, 1L))
  )

  years <- earliest_year:current_year
  links <- unlist(
    lapply(
      years,
      function(year) {
        c(
          sprintf("/ncm/EXP_%d.csv", year),
          sprintf("/ncm/IMP_%d.csv", year)
        )
      }
    ),
    use.names = FALSE
  )
  html_path <- write_download_page(links)
  captured <- character()

  testthat::with_mocked_bindings(
    compare_local_db = function(file_dir) FALSE,
    get_last_update = function() sprintf("%d-02", current_year),
    cs_base_url = html_path,
    build_db = function(link_download, db_dirs, schemas) {
      captured <<- c(captured, link_download)
      invisible(NULL)
    },
    code = comexstat::update_cs_db(dest_dir = dest_dir, timeout = 1),
    .package = "comexstat"
  )

  expect_equal(sort(captured), sort(links))
})
