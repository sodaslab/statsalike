test_that("describe returns one row per variable", {
  d <- s.describe(mtcars)
  expect_s3_class(d, "st_describe")
  expect_equal(nrow(d), ncol(mtcars))
  expect_equal(d$name, names(mtcars))
  expect_true(all(d$type == "dbl"))
  expect_output(print(d), "Observations:")
  expect_output(print(d), "Variables:")
})

test_that("variable selection and options", {
  d <- s.describe(iris, Sepal.Length:Petal.Length, numbers = TRUE)
  expect_equal(d$position, 1:3)
  expect_output(print(s.describe(iris, simple = TRUE)), "Species")
  out <- capture.output(print(s.describe(iris, short = TRUE)))
  expect_false(any(grepl("Sepal.Length", out)))
  expect_equal(s.describe(iris)$type[5], "fct")
  expect_equal(s.describe(iris)$vallab[5], "levels(3)")
})

test_that("strings with invalid encoding do not break printing", {
  bad <- c("Gr\xfc\xdfe", "ok", "Stra\xdfe")
  d <- tibble::tibble(x = bad, y = c("a", "b", "a"))
  # testthat's expect_output() cannot match invalid strings, so compare bytes
  has_total <- function(obj) {
    out <- suppressWarnings(utils::capture.output(print(obj)))
    any(grepl("Total", out, useBytes = TRUE))
  }
  expect_no_error(suppressWarnings(utils::capture.output(print(s.describe(d)))))
  expect_true(has_total(s.tab(d, x)))
  expect_true(has_total(s.tab(d, x, y)))
  expect_true(has_total(s.table(d, x, y)))
})

test_that("umlauts in native encoding (as from read.csv on Windows) work", {
  x <- c("äpfel", "birne", "äpfel")
  Encoding(x) <- "unknown"
  tab <- s.tab(tibble::tibble(x = x), x)
  expect_equal(tab$freq, c(1, 2))
})

test_that("variable labels", {
  d <- tibble::tibble(x = structure(1:3, label = "My label"),
                      s = c("a", "bb", "ccc"))
  res <- s.describe(d)
  expect_equal(res$varlab[1], "My label")
  expect_equal(res$format[2], "%-3s")
})
