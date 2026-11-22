df <- tibble::as_tibble(mtcars, rownames = "model")

test_that("summarize matches base R and Stata's formulas", {
  s <- s.summarize(df, mpg, hp, detail = TRUE)
  expect_s3_class(s, "st_summarize")
  expect_equal(s$mean, c(mean(mtcars$mpg), mean(mtcars$hp)))
  expect_equal(s$sd, c(sd(mtcars$mpg), sd(mtcars$hp)))
  expect_equal(s$p25, unname(c(quantile(mtcars$mpg, .25, type = 2), quantile(mtcars$hp, .25, type = 2))))
  m <- mean(mtcars$mpg)
  m2 <- mean((mtcars$mpg - m)^2)
  expect_equal(s$skewness[1], mean((mtcars$mpg - m)^3) / m2^1.5)
  expect_equal(s$kurtosis[1], mean((mtcars$mpg - m)^4) / m2^2)
  expect_output(print(s), "Percentiles")
  expect_output(print(s), "Kurtosis")
})

test_that("summarize: non-numeric variables, weights, groups, options", {
  s <- s.summarize(df)
  expect_equal(nrow(s), ncol(df))
  expect_equal(s$obs[s$variable == "model"], 0)
  expect_output(print(s), "Std. dev.")

  d <- tibble::tibble(x = c(1, 2, 3, 10), f = c(3, 1, 2, 1))
  long <- d[rep(1:4, d$f), ]
  cols <- c("obs", "mean", "sd", "p50", "p90", "skewness")
  expect_equal(unlist(s.summarize(d, x, wt = f, detail = TRUE)[, cols]),
               unlist(s.summarize(long, x, detail = TRUE)[, cols]))
  a <- s.summarize(df, mpg, wt = wt, weight_type = "aweight")
  wn <- mtcars$wt * 32 / sum(mtcars$wt)
  mw <- sum(wn * mtcars$mpg) / 32
  expect_equal(a$mean, mw)
  expect_equal(a$sd, sqrt(sum(wn * (mtcars$mpg - mw)^2) / 31))
  expect_output(print(a), "Weight")
  expect_error(s.summarize(df, mpg, wt = wt, weight_type = "iweight", detail = TRUE), "detail")

  g <- df |> dplyr::group_by(am) |> s.summarize(mpg)
  expect_equal(g$mean, as.vector(tapply(mtcars$mpg, mtcars$am, mean)))
  expect_output(print(g), "-> am = 0")
  expect_false("am" %in% (df |> dplyr::group_by(am) |> s.summarize())$variable)

  out <- capture.output(print(s.summarize(df, mpg, hp, wt, separator = 2)))
  expect_equal(sum(grepl("^-+\\+-+$", out)), 2)
})

test_that("codebook describes variables", {
  d <- tibble::tibble(
    id = 1:6,
    sex = factor(c("male", "female", "female", NA, "male", "female")),
    income = c(1200, 2500, NA, 1800, 3100, 2200),
    city = c("Erlangen", "Nuernberg", "Fuerth", "Erlangen", " Erlangen", NA),
    const = 1
  )
  cb <- s.codebook(d)
  expect_s3_class(cb, "st_codebook")
  expect_equal(cb$missing, c(0, 1, 1, 1, 0))
  expect_equal(cb$unique, c(6, 2, 5, 4, 1))
  expect_equal(cb$range[3], "[1200,3100]")
  expect_equal(cb$units[3], 100)
  out <- capture.output(print(cb))
  expect_true(any(grepl("Tabulation:", out)))
  expect_true(any(grepl("leading and/or trailing blanks", out)))
  expect_output(print(s.codebook(d, compact = TRUE)), "Unique")
  p <- capture.output(print(s.codebook(d, problems = TRUE)))
  expect_true(any(grepl("constant", p)))
  expect_true(any(grepl("leading or trailing", p)))
  expect_output(print(s.codebook(d, header = TRUE)), "Number of observations")

  big <- tibble::tibble(x = seq(0.5, 50, by = 0.5))
  cbb <- s.codebook(big)
  expect_equal(cbb$p50, unname(quantile(big$x, .5, type = 2)))
  expect_output(print(cbb), "Percentiles")
  expect_equal(statsalike:::st_units(c(0.25, 0.5)), 0.01)
})

test_that("codebook output fits the console width", {
  d <- tibble::tibble(
    age = structure(c(18:90, -32), labels = c("keine Angabe" = -32), label = "Age"),
    txt = c(paste(rep("long text", 30), collapse = " "), rep("b", 73)),
    bad = rep(c("Gr\xfc\xdfe und noch viel mehr Text, der nicht passt, und noch mehr", "x"), 37),
    tiny = c(seq(0.533349986399573, 1.23382678100839, length.out = 73), NA),
    id = 1e15 + (1:74) * 1e9
  )
  for (w in c(60, 80)) {
    old <- options(width = w)
    out <- suppressWarnings(capture.output(print(s.codebook(d))))
    out_c <- suppressWarnings(capture.output(print(s.codebook(d, compact = TRUE))))
    options(old)
    expect_lte(max(statsalike:::st_width(out)), max(w, 79))
    expect_lte(max(statsalike:::st_width(out_c)), w)
  }
  expect_true(any(grepl("not labeled", capture.output(print(s.codebook(d, age))))))
})

test_that("codebook shows value labels and Stata's missing categories", {
  skip_if_not_installed("haven")
  x <- haven::labelled(c(1, 2, 2, 3, haven::tagged_na("a")), c(yes = 1, no = 2))
  cb <- s.codebook(tibble::tibble(x = x))
  out <- capture.output(print(cb))
  expect_true(any(grepl("1 nonmissing value is not labeled", out, fixed = TRUE)))
  expect_true(any(grepl("Numeric  Label", out)))
  expect_true(any(grepl("NA(a)", out, fixed = TRUE)))
  expect_match(cb$problems, "incompletely labeled")

  # missing values are displayed as in R
  m <- capture.output(print(s.codebook(tibble::tibble(s = c("a", "", NA, "b")))))
  expect_true(any(grepl("Missing NA: 2/4", m, fixed = TRUE)))
  expect_true(any(grepl("^ +2  <NA>$", m)))
  expect_output(print(s.codebook(tibble::tibble(s = "a"), compact = TRUE)), "NA", fixed = TRUE)
})
