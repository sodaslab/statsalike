df <- tibble::as_tibble(mtcars)

withr_width <- function(width, code) {
  old <- options(width = width)
  on.exit(options(old))
  code
}

test_that("frequencies with totals", {
  tab <- s.table(df, cyl, am)
  expect_s3_class(tab, "st_table")
  full <- dplyr::filter(tab, cyl != "Total", am != "Total")
  expect_equal(sum(full$value), 32)
  tot <- dplyr::filter(tab, cyl == "Total", am == "Total")
  expect_equal(tot$value, 32)
  expect_output(print(tab), "Total")
})

test_that("nototals", {
  tab <- s.table(df, cyl, am, totals = FALSE)
  expect_false("Total" %in% as.character(tab$cyl))
  expect_equal(nrow(tab), 6)
})

test_that("a column heading wider than its columns keeps the rules equal", {
  d <- df
  attr(d$am, "label") <- "Transmission type, automatic or manual"
  for (w in c(80L, 50L)) {
    out <- withr_width(w, capture.output(print(s.table(d, cyl, am, totals = FALSE))))
    rules <- out[grepl("^-+(\\+-+)?$", out)]
    expect_length(unique(nchar(rules)), 1L)
    expect_true(any(grepl("Transmission", out)))
    expect_lte(max(nchar(out)), w)
  }
})

test_that("summary statistics match dplyr::summarise()", {
  tab <- s.table(df, cyl, statistic = c("mean", "sd", "median"),
                 vars = c(mpg, hp), totals = FALSE)
  ref <- dplyr::summarise(dplyr::group_by(df, cyl),
                          m = mean(mpg), s = sd(hp), md = median(mpg))
  expect_equal(tab$value[tab$statistic == "mean" & tab$variable == "mpg"], ref$m)
  expect_equal(tab$value[tab$statistic == "sd" & tab$variable == "hp"], ref$s)
  expect_equal(tab$value[tab$statistic == "median" & tab$variable == "mpg"], ref$md)
})

test_that("percentages", {
  tab <- s.table(df, cyl, am, statistic = c("percent", "rowpercent"))
  pct <- dplyr::filter(tab, statistic == "percent", cyl != "Total", am != "Total")
  expect_equal(sum(pct$value), 100)
  rp <- dplyr::filter(tab, statistic == "rowpercent", cyl == "4", am != "Total")
  expect_equal(sum(rp$value), 100)
})

test_that("nested rows, tabs and layout", {
  tab <- s.table(df, c(cyl, gear), am, statistic = c("frequency", "mean"),
                 vars = mpg)
  expect_output(print(tab), "Mean mpg")
  tab2 <- s.table(df, cyl, tabs = am)
  expect_output(print(tab2), "-> am = 0")
  grouped <- df |> dplyr::group_by(vs) |> s.table(cyl)
  expect_true("vs" %in% names(grouped))
})

test_that("quantiles follow Stata's definition", {
  expect_equal(statsalike:::st_wquantile(1:4, rep(1, 4), 50), 2.5)
  expect_equal(statsalike:::st_wquantile(1:5, rep(1, 5), 50), 3)
  expect_equal(statsalike:::st_wquantile(1:4, rep(1, 4), 25), 1.5)
  expect_equal(statsalike:::st_wquantile(1:5, rep(1, 5), 25), 2)
})

test_that("error messages", {
  expect_error(s.table(df, cyl, statistic = "mean"), "vars")
  expect_error(s.table(df, cyl, statistic = "foo"), "Unknown statistic")
})

test_that("group_by works like by: no total across groups", {
  tab <- df |> dplyr::group_by(vs) |> s.table(cyl)
  expect_false("Total" %in% as.character(tab$vs))
  expect_true("Total" %in% as.character(tab$cyl))
  out <- capture.output(print(tab))
  expect_false(any(grepl("vs = Total", out)))
})

test_that("multi-level column header stays readable after wrapping", {
  d <- tibble::tibble(a = rep(c("x", "y"), 20),
                      b = rep(c("aaaaaaaaaaaa", "bbbbbbbbbbbb", "cccccccccccc", "dddddddddddd"), each = 10),
                      c = rep(c("a long value", "another long value"), 20))
  out <- withr_width(60, capture.output(print(s.table(d, a, c(c, b)))))
  blocks <- split(out, cumsum(out == ""))
  expect_gt(length(blocks), 1)
  # every block names a value of the first column variable in its header
  expect_true(all(vapply(blocks, function(b) any(grepl("long value|Total", b)), logical(1))))
})

test_that("aweights: statistics match weighted.mean and normalized variance", {
  set.seed(1)
  d <- tibble::tibble(g = sample(c("a", "b"), 200, TRUE), x = rnorm(200), w = runif(200, 0.3, 2))
  tab <- s.table(d, g, statistic = c("mean", "sd"), vars = x, wt = w,
                 weight_type = "aweight", totals = FALSE)
  for (lv in c("a", "b")) {
    s <- d$g == lv
    wn <- d$w[s] * sum(s) / sum(d$w[s])
    m <- weighted.mean(d$x[s], wn)
    expect_equal(tab$value[tab$g == lv & tab$statistic == "mean"], m)
    expect_equal(tab$value[tab$g == lv & tab$statistic == "sd"],
                 sqrt(sum(wn * (d$x[s] - m)^2) / (sum(s) - 1)))
  }
  s1 <- s.tab(d, g, summarize = x, wt = w, weight_type = "aweight")
  expect_equal(s1$mean, tab$value[tab$statistic == "mean"])
})

test_that("printed lines never exceed the console width", {
  d <- tibble::tibble(
    lab = rep(c("a very very long category label that goes on and on and on", "short"), 10),
    g = rep(c("first group with a long name", "second group with a long name"), each = 10),
    x = seq_len(20)
  )
  attr(d$g, "label") <- "A variable label that is much longer than the console is wide, really"
  outputs <- list(
    s.tab(d, lab, g, row = TRUE),
    s.tab(d, lab, plot = TRUE),
    s.tab(d, lab, g, summarize = x),
    s.table(d, lab, g, statistic = c("frequency", "mean"), vars = x),
    s.table(df, c(cyl, gear), c(am, vs), statistic = c("frequency", "p25"), vars = mpg)
  )
  for (w in c(60, 80)) {
    widths <- withr_width(w, unlist(lapply(outputs, function(o) nchar(capture.output(print(o))))))
    expect_lte(max(widths), w)
  }
})

test_that("nformat", {
  tab <- s.table(df, cyl, statistic = "mean", vars = mpg, nformat = "%9.1f")
  expect_output(print(tab), "26\\.7")
})
