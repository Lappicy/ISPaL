library(testthat)

if (dir.exists("tests/testthat")) {
  test_dir("tests/testthat", reporter = "summary")
} else {
  test_check("ISPaLShiny", reporter = "summary")
}
