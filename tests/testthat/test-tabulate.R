df <- tibble::as_tibble(mtcars)

test_that("one-way table matches table()", {
  tab <- s.tab(df, cyl)
  expect_s3_class(tab, "st_tabulate")
  expect_equal(as.character(tab$cyl), c("4", "6", "8"))
  expect_equal(tab$freq, as.numeric(table(mtcars$cyl)))
  expect_equal(sum(tab$percent), 100)
  expect_equal(tail(tab$cum, 1), 100)
  expect_output(print(tab), "Freq\\.")
  expect_output(print(tab), "Total")
})

test_that("sort and plot", {
  tab <- s.tab(df, cyl, sort = TRUE)
  expect_equal(as.character(tab$cyl)[1], "8")
  expect_output(print(s.tab(df, cyl, plot = TRUE)), "\\*")
})

test_that("NA is shown as a category by default, missing = FALSE excludes it", {
  d <- tibble::tibble(x = c(1, 1, 2, NA), y = c("a", "b", "a", "b"))
  tab <- s.tab(d, x)
  expect_equal(as.character(tab$x), c("1", "2", NA))
  expect_output(print(tab), "<NA> |", fixed = TRUE)
  expect_identical(tab, s.tab(d, x, missing = TRUE))
  expect_equal(nrow(s.tab(d, x, missing = FALSE)), 2)
  expect_equal(sum(s.tab(d, x, y)$freq), 4)
  expect_equal(sum(s.tab(d, x, y, missing = FALSE)$freq), 3)
  expect_equal(nrow(s.tab1(d, x)$x), 3)
  expect_equal(nrow(s.tab1(d, x, missing = FALSE)$x), 2)
  expect_equal(sum(s.tab2(d, x, y)[[1]]$freq), 4)
  expect_equal(sum(s.table(d, x)$value[s.table(d, x)$x != "Total" | is.na(s.table(d, x)$x)]), 4)
  expect_false(anyNA(s.table(d, x, missing = FALSE)$x))
  # s.tabgen() keeps NA in the indicator variables by default
  expect_true(is.na(s.tabgen(d, x)$x_1[4]))
})

test_that("two-way table and chi2 match chisq.test()", {
  tab <- s.tab(df, cyl, am, chi2 = TRUE, lrchi2 = TRUE, V = TRUE,
               row = TRUE, column = TRUE)
  O <- table(mtcars$cyl, mtcars$am)
  expect_equal(tab$freq, as.vector(t(O)))
  ref <- suppressWarnings(stats::chisq.test(O, correct = FALSE))
  t <- s.tests(tab)
  expect_equal(t$statistic[t$test == "Pearson chi2"], unname(ref$statistic))
  expect_equal(t$p[t$test == "Pearson chi2"], unname(ref$p.value))
  V <- sqrt(unname(ref$statistic) / (sum(O) * 1))
  expect_equal(t$statistic[grepl("V$", t$test)], V)
  expect_output(print(tab), "row percentage")
  expect_output(print(tab), "Pearson chi2\\(2\\)")
})

test_that("empty strings are missing, as in Stata", {
  d <- tibble::tibble(s = c("a", "", "b", NA, "a"))
  expect_equal(sum(s.tab(d, s, missing = FALSE)$freq), 3)
  expect_equal(as.character(s.tab(d, s)$s), c("a", "b", NA))
})

test_that("Stata extended missing values (.a, .b) are separate categories", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, haven::tagged_na("a"), haven::tagged_na("b"), NA, 1),
                       c(yes = 1, no = 2, "don't know" = haven::tagged_na("a")))
  d <- tibble::tibble(x = x)
  expect_equal(sum(s.tab(d, x, missing = FALSE)$freq), 3)
  tab <- s.tab(d, x)
  expect_equal(as.character(tab$x), c("yes", "no", NA, "don't know", "NA(b)"))
  expect_equal(tab$freq, c(2, 1, 1, 1, 1))
})

test_that("SPSS user-defined missing values are treated as missing", {
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(c(1, 2, -9, 2, -8), c(yes = 1, no = 2, refused = -9),
                            na_values = c(-9, -8))
  z <- haven::labelled_spss(c(10, 20, 30, -99, 40), na_values = -99)
  d <- tibble::tibble(x = x, z = z)
  expect_equal(sum(s.tab(d, x, missing = FALSE)$freq), 3)
  expect_equal(as.character(s.tab(d, x)$x), c("yes", "no", "refused", "-8"))
  s <- s.tab(d, x, summarize = z, missing = FALSE)
  expect_equal(s$mean, c(10, 20))
  tb <- s.table(d, statistic = "mean", vars = z)
  expect_equal(tb$value, 25)
})

test_that("entropy addon for one-way tables", {
  ent <- function(tab) stats::setNames(s.tests(tab)$statistic, s.tests(tab)$test)
  uni <- tibble::tibble(x = rep(letters[1:4], 5))
  e <- ent(s.tab(uni, x, entropy = TRUE))
  expect_equal(unname(e["entropy"]), log(4))
  expect_equal(unname(e["relative entropy"]), 1)

  d <- tibble::tibble(x = c("a", "a", "b", "c", NA), w = c(1, 1, 2, 4, 1))
  e <- ent(s.tab(d, x, entropy = TRUE, missing = FALSE))
  p <- c(.5, .25, .25)
  expect_equal(unname(e["entropy"]), -sum(p * log(p)))
  expect_equal(unname(e["relative entropy"]), -sum(p * log(p)) / log(3))

  # weights: entropy of the weighted distribution (2, 2, 4) / 8
  e <- ent(s.tab(d, x, wt = w, weight_type = "aweight", entropy = TRUE, missing = FALSE))
  p <- c(2, 2, 4) / 8
  expect_equal(unname(e["entropy"]), -sum(p * log(p)))

  # by default the category <NA> counts
  e <- ent(s.tab(d, x, entropy = TRUE))
  p <- c(2, 1, 1, 1) / 5
  expect_equal(unname(e["relative entropy"]), -sum(p * log(p)) / log(4))

  # a single category: entropy 0, relative entropy undefined
  e <- ent(s.tab(tibble::tibble(x = rep("a", 3)), x, entropy = TRUE))
  expect_equal(unname(e["entropy"]), 0)
  expect_true(is.na(e["relative entropy"]))

  tab <- s.tab(df, cyl, entropy = TRUE)
  expect_output(print(tab), "relative entropy")
  expect_false(any(grepl("Note|addon", capture.output(print(tab)))))
  expect_output(print(s.tab(df, cyl, plot = TRUE, entropy = TRUE)), "relative entropy")

  grouped <- s.tests(df |> dplyr::group_by(am) |> s.tab(gear, entropy = TRUE))
  expect_equal(nrow(grouped), 4)
  expect_true("am" %in% names(grouped))
  expect_length(s.tab1(df, cyl, gear, entropy = TRUE), 2)

  expect_error(s.tab(df, cyl, am, entropy = TRUE), "one-way")
  expect_error(s.tab(df, cyl, summarize = mpg, entropy = TRUE), "entropy")
})

test_that("Fisher's exact test matches fisher.test()", {
  tab <- s.tab(df, vs, am, exact = TRUE)
  t <- s.tests(tab)
  expect_equal(t$p[t$test == "Fisher's exact"],
               stats::fisher.test(table(mtcars$vs, mtcars$am))$p.value)
  expect_true("1-sided Fisher's exact" %in% t$test)
})

test_that("exact falls back to a fast simulation for large r x c tables, reproducible with set.seed()", {
  set.seed(42)
  d <- tibble::tibble(a = sample(paste0("x", 1:4), 300, TRUE), b = sample(1:5, 300, TRUE))
  set.seed(1)
  elapsed <- system.time(t1 <- s.tests(s.tab(d, a, b, exact = TRUE)))[["elapsed"]]
  expect_lt(elapsed, 10)
  expect_equal(t1$test, "Fisher's exact (simulated)")
  set.seed(1)
  t2 <- s.tests(s.tab(d, a, b, exact = TRUE))
  expect_equal(t1$p, t2$p)
  # the same as stats::fisher.test() with the same seed
  set.seed(1)
  p_r <- stats::fisher.test(table(d$a, d$b), simulate.p.value = TRUE, B = 1e5)$p.value
  expect_equal(t1$p, p_r)
})

test_that("gamma and tau-b", {
  tab <- s.tab(df, cyl, gear, gamma = TRUE, taub = TRUE)
  t <- s.tests(tab)
  taub <- stats::cor(mtcars$cyl, mtcars$gear, method = "kendall")
  expect_equal(t$statistic[t$test == "Kendall's tau-b"], taub)
  expect_true(all(t$ase > 0))
})

test_that("fweights match expanded data", {
  d <- tibble::tibble(x = c("a", "b", "c"), y = c(1, 1, 2), w = c(2, 3, 5))
  long <- d[rep(1:3, d$w), ]
  expect_equal(s.tab(d, x, wt = w)$freq, s.tab(long, x)$freq)
  expect_equal(s.tab(d, x, y, wt = w)$freq, s.tab(long, x, y)$freq)
  expect_error(s.tab(d, x, wt = c(1.5, 1, 1)), "integers")
})

test_that("aweights are rescaled to the number of observations", {
  d <- tibble::tibble(x = c("a", "b", "b"), w = c(2, 1, 1))
  expect_equal(s.tab(d, x, wt = w, weight_type = "aweight")$freq, c(1.5, 1.5))
})

test_that("group_by works like by:", {
  tab <- df |> dplyr::group_by(am) |> s.tab(gear)
  expect_equal(names(tab)[1:2], c("am", "gear"))
  expect_equal(sum(tab$freq), 32)
  expect_output(print(tab), "-> am = 0")
})

test_that("summarize()", {
  tab <- s.tab(df, cyl, summarize = mpg)
  expect_equal(tab$mean, as.numeric(tapply(mtcars$mpg, mtcars$cyl, mean)))
  expect_equal(tab$sd, as.numeric(tapply(mtcars$mpg, mtcars$cyl, sd)))
  expect_output(print(tab), "Summary of mpg")
  tab2 <- s.tab(df, cyl, am, summarize = mpg)
  expect_equal(nrow(tab2), 6)
  expect_output(print(tab2), "Means, Standard deviations and Frequencies")
})

test_that("options are checked as in Stata", {
  expect_error(s.tab(df, cyl, chi2 = TRUE), "two-way")
  expect_error(s.tab(df, cyl, am, sort = TRUE), "one-way")
  expect_error(s.tab(df, cyl, obs = TRUE), "summarize")
})

test_that("dplyr verbs return ordinary tibbles", {
  tab <- s.tab(df, cyl, am)
  f <- dplyr::filter(tab, freq > 2)
  expect_false(inherits(f, "st_output"))
  expect_s3_class(f, "tbl_df")
  expect_false(inherits(tab[1:2, ], "st_output"))
})

test_that("tab1, tab2 and tabgen", {
  l1 <- s.tab1(df, cyl, gear)
  expect_s3_class(l1, "st_tab_list")
  expect_named(l1, c("cyl", "gear"))
  l2 <- s.tab2(df, cyl, gear, am, chi2 = TRUE)
  expect_length(l2, 3)
  expect_length(s.tab2(df, cyl, gear, am, firstonly = TRUE), 2)
  expect_output(print(l2), "-> tabulation of cyl by gear")

  g <- s.tabgen(df, cyl, prefix = "c")
  expect_equal(g$c1, as.integer(mtcars$cyl == 4), ignore_attr = TRUE)
  expect_equal(attr(g$c3, "label"), "cyl==8")
})

test_that("haven value labels are used", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(sex = haven::labelled(c(1, 2, 2), c(male = 1, female = 2)))
  expect_equal(as.character(s.tab(d, sex)$sex), c("male", "female"))
  expect_equal(as.character(s.tab(d, sex, nolabel = TRUE)$sex), c("1", "2"))
})
