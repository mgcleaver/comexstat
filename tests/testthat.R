library(testthat)

run_package_tests <- function() {
  library(comexstat)
  test_check("comexstat")
}

tryCatch(
  run_package_tests(),
  error = function(err) {
    if (!grepl("No test files found|there is no package called", conditionMessage(err))) {
      stop(err)
    }

    test_local()
  }
)
