d <- tibble::tibble(
  x = c(1, 2, 2, NA, 1, 2, NA, 1),
  g = c("a", "b", NA, "a", "b", "a", NA, "b"),
  y = c(3, 4, 5, 6, NA, 2, 1, 7)
)

test_that("missing values are shown as R does", {
  t1 <- s.tab(d, x, missing = TRUE)
  # the result holds a genuine NA, the output shows <NA>
  expect_equal(sum(is.na(t1$x)), 1)
  expect_equal(levels(t1$x), c("1", "2"))
  expect_equal(t1$freq, c(3, 3, 2))
  out <- capture.output(print(t1))
  expect_true(any(grepl("^ +<NA> \\|", out)))
  expect_false(any(grepl(".NA", out, fixed = TRUE)))

  t2 <- s.tab(d, x, g, missing = TRUE)
  expect_true(any(grepl("<NA>", capture.output(print(t2)), fixed = TRUE)))
  expect_equal(sum(is.na(t2$g)), 3)

  tb <- s.table(d, x, g, missing = TRUE)
  expect_true(sum(is.na(tb$g)) > 0)
  out <- capture.output(print(tb))
  expect_true(any(grepl("^ +<NA> +\\|", out)))
  expect_true(any(grepl("Total", out, fixed = TRUE)))

  expect_output(print(d |> dplyr::group_by(g) |> s.summarize(y)), "-> g = <NA>", fixed = TRUE)
  cb <- capture.output(print(s.codebook(d, g)))
  expect_true(any(grepl("Missing NA: 2/8", cb, fixed = TRUE)))
  expect_true(any(grepl("^ +2  <NA>$", cb)))
  # estimation results show missing statistics as Stata does (".")
  expect_output(print(s.regress(mtcars, mpg, wt, hp, cluster = am)), "=          .", fixed = TRUE)
  expect_output(print(s.logit(mtcars, vs, mpg, wt, cluster = am, nolog = TRUE)),
                "Wald chi2(0)      =          .", fixed = TRUE)
  l1 <- s.tab1(d, x, g, missing = TRUE)
  expect_equal(sum(is.na(l1$g$g)), 1)
  expect_true(any(grepl("^ +<NA> \\|", capture.output(print(l1)))))
  expect_true(any(grepl("<NA>", capture.output(print(s.tab2(d, x, g, missing = TRUE))), fixed = TRUE)))
  gen <- s.tabgen(d, x, missing = TRUE)
  expect_equal(attr(gen$x_3, "label"), "x==<NA>")

  f <- tempfile(fileext = ".html")
  suppressMessages(s.export(t1, f))
  expect_match(paste(readLines(f, warn = FALSE), collapse = "\n"), "&lt;NA&gt;", fixed = TRUE)
})

test_that("extended missing values are shown as NA(a)", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, haven::tagged_na("a"), NA), c(yes = 1, no = 2))
  r <- s.tab(tibble::tibble(x = x), x, missing = TRUE)$x
  expect_equal(as.character(r), c("yes", "no", NA, "NA(a)"))
})

test_that("the former r.na argument no longer exists", {
  expect_false("r.na" %in% names(formals(s.tabulate)))
  expect_false("r.na" %in% names(formals(s.regress)))
})
