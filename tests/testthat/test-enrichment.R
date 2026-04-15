test_that("filter_states keeps only valid Brazilian states", {
  data <- tibble::tibble(
    state = c("SP", "EX", "RJ", "ND"),
    value = c(1L, 2L, 3L, 4L)
  )

  result <- comexstat::filter_states(data)

  expect_equal(result$state, c("SP", "RJ"))
  expect_equal(result$value, c(1L, 3L))
})

test_that("filter_states fails when state column is missing", {
  expect_error(
    comexstat::filter_states(tibble::tibble(value = 1L)),
    "state"
  )
})

test_that("add_state_name joins state names and respects drop_code", {
  results <- testthat::with_mocked_bindings(
    get_state_table = function(...) fixture_state_table(),
    code = list(
      kept = comexstat::add_state_name(
        tibble::tibble(state = c("SP", "XX"), value = c(1L, 2L)),
        drop_code = FALSE
      ),
      dropped = comexstat::add_state_name(
        tibble::tibble(state = c("SP", "XX"), value = c(1L, 2L)),
        drop_code = TRUE
      )
    ),
    .package = "comexstat"
  )

  expect_named(results$kept, c("state", "state_name", "value"))
  expect_equal(results$kept$state_name, c("Sao Paulo", NA))
  expect_named(results$dropped, c("state_name", "value"))
})

test_that("add_country_name joins translated names and preserves rows", {
  results <- testthat::with_mocked_bindings(
    get_country_table = function(...) fixture_country_table(),
    code = list(
      kept = comexstat::add_country_name(
        tibble::tibble(country_code = c(10L, 99L), value = c(1L, 2L)),
        lang = "pt",
        drop_code = FALSE
      ),
      dropped = comexstat::add_country_name(
        tibble::tibble(country_code = c(10L, 99L), value = c(1L, 2L)),
        lang = "es",
        drop_code = TRUE
      )
    ),
    .package = "comexstat"
  )

  expect_named(results$kept, c("country_code", "country_name_pt", "value"))
  expect_equal(results$kept$country_name_pt, c("Pais A", NA))
  expect_named(results$dropped, c("country_name_es", "value"))
  expect_equal(results$dropped$country_name_es, c("Pais A ES", NA))
})

test_that("add_ncm_name adds translated names", {
  results <- testthat::with_mocked_bindings(
    get_ncm_table = function(...) fixture_ncm_table(),
    code = list(
      kept = comexstat::add_ncm_name(
        tibble::tibble(ncm = c("00000001", "99999999"), value = c(1L, 2L)),
        lang = "en",
        drop_code = FALSE
      ),
      dropped = comexstat::add_ncm_name(
        tibble::tibble(ncm = c("00000001", "99999999"), value = c(1L, 2L)),
        lang = "pt",
        drop_code = TRUE
      )
    ),
    .package = "comexstat"
  )

  expect_named(results$kept, c("ncm", "ncm_name", "value"))
  expect_equal(results$kept$ncm_name, c("Live horses", NA))
  expect_named(results$dropped, c("ncm_name_pt", "value"))
  expect_equal(results$dropped$ncm_name_pt, c("Cavalos vivos", NA))
})

test_that("add_units uses NCM to recover unit names", {
  results <- testthat::with_mocked_bindings(
    get_ncm_table = function(...) fixture_ncm_table(),
    unit_table = fixture_unit_table(),
    code = list(
      kept = comexstat::add_units(
        tibble::tibble(ncm = c("00000001", "99999999"), value = c(1L, 2L)),
        lang = "en",
        drop_code = FALSE
      ),
      dropped = comexstat::add_units(
        tibble::tibble(ncm = c("00000001", "99999999"), value = c(1L, 2L)),
        lang = "pt",
        drop_code = TRUE
      )
    ),
    .package = "comexstat"
  )

  expect_named(results$kept, c("ncm", "unit_code", "unit_name", "value"))
  expect_equal(results$kept$unit_name, c("Net kilogram", NA))
  expect_named(results$dropped, c("ncm", "unit_name_pt", "value"))
  expect_equal(results$dropped$unit_name_pt, c("Quilograma liquido", NA))
})

test_that("add_bec_name filters columns by requested level", {
  results <- testthat::with_mocked_bindings(
    get_ncm_table = function(...) fixture_ncm_table(),
    get_bec_table = function(...) fixture_bec_table(),
    code = list(
      level_n2 = comexstat::add_bec_name(
        tibble::tibble(ncm = c("00000001", "99999999")),
        lang = "pt",
        level = "n2",
        drop_code = FALSE
      ),
      all_levels = comexstat::add_bec_name(
        tibble::tibble(ncm = "00000001"),
        lang = "en",
        level = "all",
        drop_code = TRUE
      )
    ),
    .package = "comexstat"
  )

  expect_named(results$level_n2, c("ncm", "bec_n2_code", "bec_n2_name_pt"))
  expect_equal(results$level_n2$bec_n2_name_pt, c("Alimentos", NA))
  expect_false(any(stringr::str_detect(names(results$level_n2), "bec_n3")))

  expect_named(
    results$all_levels,
    c("ncm", "bec_n3_name", "bec_n2_name", "bec_n1_name")
  )
})

test_that("add_isic_name keeps only requested level and language", {
  results <- testthat::with_mocked_bindings(
    get_ncm_table = function(...) fixture_ncm_table(),
    get_isic_table = function(...) fixture_isic_table(),
    code = list(
      division = comexstat::add_isic_name(
        tibble::tibble(ncm = c("00000001", "99999999")),
        lang = "en",
        level = "division",
        drop_code = FALSE
      ),
      all_levels = comexstat::add_isic_name(
        tibble::tibble(ncm = "00000001"),
        lang = "es",
        level = "all",
        drop_code = TRUE
      )
    ),
    .package = "comexstat"
  )

  expect_named(
    results$division,
    c("ncm", "isic_division_code", "isic_division_name")
  )
  expect_equal(
    results$division$isic_division_name,
    c("Crop production", NA)
  )
  expect_false(any(stringr::str_detect(names(results$division), "class")))

  expect_named(
    results$all_levels,
    c(
      "ncm",
      "isic_class_name_es",
      "isic_group_name_es",
      "isic_division_name_es",
      "isic_section_name_es"
    )
  )
})

test_that("add_cuci_name keeps Portuguese names for requested level", {
  results <- testthat::with_mocked_bindings(
    get_ncm_table = function(...) fixture_ncm_table(),
    get_cuci_table = function(...) fixture_cuci_table(),
    code = list(
      group_level = comexstat::add_cuci_name(
        tibble::tibble(ncm = c("00000001", "99999999")),
        level = "group",
        drop_code = FALSE
      ),
      all_levels = comexstat::add_cuci_name(
        tibble::tibble(ncm = "00000001"),
        level = "all",
        drop_code = TRUE
      )
    ),
    .package = "comexstat"
  )

  expect_named(
    results$group_level,
    c("ncm", "cuci_group_code", "cuci_group_name_pt")
  )
  expect_equal(results$group_level$cuci_group_name_pt, c("Animais vivos", NA))
  expect_false(any(stringr::str_detect(names(results$group_level), "basic_heading")))

  expect_named(
    results$all_levels,
    c(
      "ncm",
      "cuci_basic_heading_name_pt",
      "cuci_subgroup_name_pt",
      "cuci_group_name_pt",
      "cuci_division_name_pt",
      "cuci_section_name_pt"
    )
  )
})
