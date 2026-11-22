df <- tibble::as_tibble(mtcars)

test_that("HTML export writes a table with the layout of the printed output", {
  f <- tempfile(fileext = ".html")
  tab <- s.tab(df, cyl, am, row = TRUE, chi2 = TRUE)
  expect_message(res <- s.export(tab, f), "Exported")
  expect_identical(res, tab)
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "<table class=\"st\">", fixed = TRUE)
  # the title is the equivalent Stata command, including the default `missing`
  expect_match(html, "tabulate cyl am, missing row chi2", fixed = TRUE)
  expect_match(html, "row percentage", fixed = TRUE)
  expect_match(html, "Pearson chi2(2)", fixed = TRUE)
  expect_match(html, ">Total<", fixed = TRUE)
})

test_that("export option of the table functions", {
  f <- tempfile(fileext = ".html")
  expect_message(tab <- s.table(df, cyl, am, export = f), "Exported")
  expect_s3_class(tab, "st_table")
  expect_true(file.exists(f))
  f2 <- tempfile(fileext = ".htm")
  expect_message(s.describe(df, export = f2))
  expect_match(paste(readLines(f2), collapse = ""), "Variable name", fixed = TRUE)
  f3 <- tempfile(fileext = ".html")
  expect_message(s.tab2(df, cyl, gear, am, chi2 = TRUE, export = f3))
  expect_match(paste(readLines(f3), collapse = ""), "tab2 cyl gear am, missing chi2", fixed = TRUE)
  f4 <- tempfile(fileext = ".html")
  expect_message(s.tab(df, cyl, am, missing = FALSE, chi2 = TRUE, export = f4))
  expect_match(paste(readLines(f4), collapse = ""), "tabulate cyl am, chi2<", fixed = TRUE)
})

test_that("several results and grouped results go into one file", {
  f <- tempfile(fileext = ".html")
  grouped <- df |> dplyr::group_by(vs) |> s.tab(gear, wt = carb)
  expect_message(s.export(list(s.describe(df), grouped, s.table(df, cyl, tabs = am)), f,
                          title = c("Data", "Gears", "Cylinders")))
  html <- paste(readLines(f), collapse = "\n")
  expect_equal(lengths(regmatches(html, gregexpr("<h2>", html))), 3L)
  expect_match(html, "-&gt; vs = 0", fixed = TRUE)
  expect_match(html, "-&gt; am = 1", fixed = TRUE)
})

test_that("HTML special characters and invalid encodings are handled", {
  d <- tibble::tibble(x = c("a<b", "R&D", "Gr\xfc\xdfe"))
  f <- tempfile(fileext = ".html")
  expect_message(s.export(s.tab(d, x), f))
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "a&lt;b", fixed = TRUE)
  expect_match(html, "R&amp;D", fixed = TRUE)
  expect_match(html, "Grüße", fixed = TRUE)
})

test_that("wrong input is rejected", {
  tab <- s.tab(df, cyl)
  expect_error(s.export(df, tempfile(fileext = ".html")), "must be a result")
  expect_error(s.export(tab, tempfile(fileext = ".pdf")), "Unknown export format")
  expect_error(s.export(tab, file.path(tempdir(), "no", "such", "dir", "x.html")), "does not exist")
  expect_error(s.export(list(tab, tab, tab), tempfile(fileext = ".html"), title = c("a", "b")),
               "length 1 or 3")
})

test_that("format can be given explicitly and extensions are case-insensitive", {
  f <- tempfile()
  expect_message(s.export(s.tab(df, cyl), f, format = "html"))
  expect_match(paste(readLines(f), collapse = ""), "<table", fixed = TRUE)
  expect_message(s.export(s.tab(df, cyl), tempfile(fileext = ".HTM")))
})

test_that("DOCX export", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  f <- tempfile(fileext = ".docx")
  tab <- s.tab(df, cyl, am, row = TRUE, column = TRUE, chi2 = TRUE)
  expect_message(s.export(list(tab, s.table(df, c(cyl, gear), am, statistic = c("frequency", "mean"), vars = mpg),
                               s.describe(df)), f))
  content <- officer::docx_summary(officer::read_docx(f))
  txt <- paste(content$text, collapse = " ")
  expect_match(txt, "tabulate cyl am, missing row column chi2", fixed = TRUE)
  expect_match(txt, "Pearson chi2(2)", fixed = TRUE)
  expect_match(txt, "Mean mpg", fixed = TRUE)
  expect_match(txt, "Variable name", fixed = TRUE)
  expect_true(any(content$content_type == "table cell"))
})
