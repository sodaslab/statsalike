m1 <- s.regress(mtcars, mpg, wt)
m2 <- s.regress(mtcars, mpg, wt, hp, cyl, categorical = cyl)

test_that("etable matches lm()", {
  e <- s.etable(m1, m2)
  expect_s3_class(e, "st_etable")
  expect_equal(unique(e$model), c("mpg (1)", "mpg (2)"))
  f <- summary(lm(mpg ~ wt + hp + factor(cyl), mtcars))
  e2 <- e[e$model == "mpg (2)", ]
  expect_equal(e2$term, c("wt", "hp", "cyl:6", "cyl:8", "_cons"))
  ref <- f$coefficients[c("wt", "hp", "factor(cyl)6", "factor(cyl)8", "(Intercept)"), ]
  expect_equal(e2$estimate, unname(ref[, 1]))
  expect_equal(e2$std_error, unname(ref[, 2]))
  expect_equal(e2$p_value, unname(ref[, 4]))
  # terms that a model does not contain have no row
  expect_equal(e$term[e$model == "mpg (1)"], c("wt", "_cons"))
  meta <- attr(e, "st_meta")
  expect_equal(meta$r2[2], f$r.squared)
  out <- capture.output(print(e))
  expect_true(any(grepl("-3.181***", out, fixed = TRUE)))
  expect_true(any(grepl("(0.720)", out, fixed = TRUE)))
  expect_true(any(grepl("^Intercept", out)))
  expect_true(any(grepl("* p < 0.05, ** p < 0.01, *** p < 0.001", out, fixed = TRUE)))
  # the base category is hidden by default
  expect_false(any(grepl("(base)", out, fixed = TRUE)))
  expect_true(any(grepl("(base)", capture.output(print(s.etable(m2, showbase = TRUE))), fixed = TRUE)))
})

test_that("own row and column labels with spaces", {
  e <- s.etable("Model 1" = m1, "Model 2" = m2,
                rowlabels = c(wt = "Weight in 1000 lbs", "cyl:8" = "Eight cylinders",
                           "_cons" = "Constant"),
                mstat = c("Cases" = "N", "R squared" = "r2"))
  out <- capture.output(print(e))
  expect_true(any(grepl("Model 1    Model 2", out, fixed = TRUE)))
  expect_true(any(grepl("^Weight in 1000 lbs", out)))
  expect_true(any(grepl("^  Eight cylinders", out)))
  expect_true(any(grepl("^Constant", out)))
  expect_true(any(grepl("^Cases", out)))
  expect_equal(unique(e$label[e$term == "wt"]), "Weight in 1000 lbs")
  expect_equal(unique(s.etable(m1, m2, collabels = c("A", "B"))$model), c("A", "B"))
  expect_warning(s.etable(m2, rowlabels = c("cyl:eight" = "x")), "not in the table")
  de <- capture.output(print(s.etable(m1, notes = "Standardfehler in Klammern.")))
  expect_true(any(de == "Standardfehler in Klammern."))
  expect_false(any(grepl("Standard errors", de)))
  expect_false(any(grepl("p < 0.05", capture.output(print(s.etable(m1, notes = FALSE))), fixed = TRUE)))
  expect_error(s.etable(m1, notes = 1), "`notes`")
})

test_that("variable labels are used by default", {
  skip_if_not_installed("haven")
  d <- tibble::tibble(
    y = haven::labelled(mtcars$mpg, label = "Miles per gallon"),
    x = haven::labelled(mtcars$wt, label = "Weight"),
    g = haven::labelled(mtcars$am, c(automatic = 0, manual = 1), label = "Transmission")
  )
  m <- s.regress(d, y, x, g, categorical = g)
  out <- capture.output(print(s.etable(m)))
  expect_true(any(grepl("Miles per gallon", out)))
  expect_true(any(grepl("^Weight", out)))
  expect_true(any(grepl("^Transmission", out)))
  expect_true(any(grepl("^  manual", out)))
  expect_true(any(grepl("^x ", capture.output(print(s.etable(m, varlabel = FALSE))))))
  expect_true(any(grepl("^  Manual gearbox",
                        capture.output(print(s.etable(m, rowlabels = c("g:manual" = "Manual gearbox")))))))
})

test_that("cell statistics, stars, keep and drop", {
  ci <- capture.output(print(s.etable(m1, cstat = "ci", stars = FALSE)))
  expect_true(any(grepl("[-6.486, -4.203]", ci, fixed = TRUE)))
  expect_false(any(grepl("*", ci, fixed = TRUE)))
  expect_true(any(grepl("95% confidence intervals in brackets.", ci, fixed = TRUE)))
  l90 <- s.regress(mtcars, mpg, wt, level = 90)
  ci2 <- capture.output(print(s.etable(m1, l90, cstat = "ci")))
  expect_true(any(grepl("Confidence intervals (95% in model(s) 1; 90% in model(s) 2) in brackets.",
                        ci2, fixed = TRUE)))
  expect_true(any(grepl("[-6.293, -4.396]", ci2, fixed = TRUE)))
  expect_true(any(grepl("(-9.56)", capture.output(print(s.etable(m1, cstat = "t"))), fixed = TRUE)))
  expect_true(any(grepl("(0.000)", capture.output(print(s.etable(m1, cstat = "p"))), fixed = TRUE)))
  expect_equal(nrow(s.etable(m1, cstat = "none")), 2L)
  expect_true(any(grepl("-5.344**", capture.output(print(s.etable(m1, stars = c(0.05, 0.01)))),
                        fixed = TRUE)))
  expect_equal(unique(s.etable(m2, keep = "cyl")$term), c("cyl:6", "cyl:8"))
  expect_false("_cons" %in% s.etable(m2, drop = "_cons")$term)
  expect_error(s.etable(m1, keep = "zzz"), "No coefficients")
  expect_equal(s.etable(m1, nformat = "%9.1f") |> capture.output() |> grepl(pattern = "-5.3***", fixed = TRUE) |> any(), TRUE)
})

test_that("logit models, odds ratios and mixed tables", {
  l1 <- s.logit(mtcars, am, wt, hp, nolog = TRUE)
  l2 <- s.logit(mtcars, am, wt, hp, or = TRUE, nolog = TRUE)
  g <- glm(am ~ wt + hp, binomial, mtcars)
  e <- s.etable(l1, eform = TRUE)
  expect_equal(e$estimate, unname(exp(coef(g))[c("wt", "hp", "(Intercept)")]), tolerance = 1e-6)
  expect_equal(e$std_error, unname((exp(coef(g)) * sqrt(diag(vcov(g))))[c("wt", "hp", "(Intercept)")]),
               tolerance = 1e-5)
  expect_equal(s.etable(l2)$estimate, e$estimate)
  out <- capture.output(print(s.etable(l1, l2, mstat = c("N", "r2_p"))))
  expect_true(any(grepl("Odds ratios in model(s) 2.", out, fixed = TRUE)))
  meta <- attr(s.etable(m1, l1, mstat = c("N", "r2", "r2_p")), "st_meta")
  expect_true(is.na(meta$r2[2]))
  expect_true(is.na(meta$r2_p[1]))
  expect_true(any(grepl("t/z statistics", capture.output(print(s.etable(m1, l1, cstat = "t"))))))
})

test_that("long column headings are wrapped, not truncated", {
  long <- dplyr::mutate(mtcars, y = mpg)
  attr(long$y, "label") <- "A VERY LONG VARIABLE LABEL OF THE DEPENDENT VARIABLE(WITH+NO+SPACES)"
  a <- s.regress(long, y, wt)
  b <- s.regress(long, y, wt, hp)
  out <- capture.output(print(s.etable(a, b)))
  # one block (no wrapping of columns) and the model numbers are visible
  expect_equal(sum(grepl("^Intercept", out)), 1L)
  expect_true(any(grepl("(1)", out, fixed = TRUE)))
  expect_true(any(grepl("(2)", out, fixed = TRUE)))
  expect_true(all(nchar(out) <= 80))
  expect_equal(st_wrap <- statsalike:::st_wrap_heading("ab cd efghijklmnop", 5), c("ab cd", "efghi", "jklmn", "op"))
})

test_that("omitted terms and different base categories", {
  mo <- s.regress(dplyr::mutate(mtcars, wt2 = 2 * wt), mpg, wt, wt2)
  e <- s.etable(mo)
  # as in Stata, regress keeps the variable with the larger scale (wt2)
  expect_equal(e$note[e$term == "wt"], "omitted")
  expect_true(any(grepl("(omitted)", capture.output(print(e)), fixed = TRUE)))
  a <- s.regress(mtcars, mpg, wt, cyl, categorical = cyl)
  b <- s.regress(dplyr::filter(mtcars, cyl != 4), mpg, wt, cyl, categorical = cyl)
  e <- s.etable(a, b)
  # cyl 6 is a coefficient in model 1 and the base in model 2
  expect_equal(e$note[e$model == "mpg (2)" & e$term == "cyl:6"], "base")
  expect_equal(e$estimate[e$model == "mpg (1)" & e$term == "cyl:6"],
               unname(coef(lm(mpg ~ wt + factor(cyl), mtcars))["factor(cyl)6"]))
})

test_that("robust models, groups, lists and errors", {
  r <- s.regress(mtcars, mpg, wt, vce = "robust")
  out <- capture.output(print(s.etable(m1, r)))
  expect_true(any(grepl("Robust standard errors in model(s) 2.", out, fixed = TRUE)))
  expect_true(any(grepl("Robust standard errors in parentheses.",
                        capture.output(print(s.etable(r))), fixed = TRUE)))
  cl <- s.regress(mtcars, mpg, wt, cluster = cyl)
  expect_true(any(grepl("Cluster-robust standard errors in parentheses.",
                        capture.output(print(s.etable(cl))), fixed = TRUE)))
  mixed <- capture.output(print(s.etable(m1, r, cl)))
  expect_true(any(grepl("Robust standard errors in model(s) 2.", mixed, fixed = TRUE)))
  expect_true(any(grepl("Cluster-robust standard errors in model(s) 3.", mixed, fixed = TRUE)))
  iw <- s.regress(mtcars, mpg, wt, wt = qsec / 10, weight_type = "iweight")
  # N is the sum of the importance weights, not rounded to a whole number
  expect_true(any(grepl(sprintf("%.2f", sum(mtcars$qsec / 10)),
                        capture.output(print(s.etable(iw))), fixed = TRUE)))
  g <- mtcars |> dplyr::group_by(am) |> s.regress(mpg, wt)
  expect_equal(unique(s.etable(g)$model), c("mpg am = 0", "mpg am = 1"))
  expect_equal(unique(s.etable(list(a = m1, b = m2))$model), c("a", "b"))
  expect_error(s.etable(mtcars), "must be estimation results")
  expect_error(s.etable(), "at least one")
  expect_error(s.etable(m1, rowlabels = "x"), "named character")
  expect_error(s.etable(m1, mstat = "aic"), "Unknown model statistic")
  expect_error(s.etable(m1, stars = c(0.01, 0.05)), "decreasing")
  expect_error(s.etable(m1, collabels = c("a", "b")), "one label per model")
})

test_that("etable exports", {
  f <- tempfile(fileext = ".html")
  expect_message(s.etable("Model 1" = m1, m2, title = "Table 3", export = f), "Exported")
  html <- paste(readLines(f, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "<h2>Table 3</h2>", fixed = TRUE)
  expect_match(html, "Model 1", fixed = TRUE)
  expect_match(html, "p &lt; 0.05", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("Table 3", html, fixed = TRUE))), 2L)
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  f2 <- tempfile(fileext = ".docx")
  expect_message(s.etable(m1, m2, export = f2), "Exported")
  expect_true(file.exists(f2))
})

test_that("a variable that is continuous in one model and a factor in another gets its own rows", {
  d <- tibble::as_tibble(mtcars)
  m_c <- s.regress(d, mpg, am, wt)
  m_f <- s.regress(d, mpg, wt, "i.am")
  for (e in list(s.etable(m_c, m_f), s.etable(m_f, m_c))) {
    out <- utils::capture.output(print(e))
    rows <- grep("^(am|  1) ", out)
    # the coefficient row of am and, separately, the heading of the factor
    # directly followed by its category
    expect_length(rows, 3L)
    hdr <- rows[grepl("^am +\\|$", out[rows])]
    expect_length(hdr, 1L)
    expect_match(out[hdr + 1L], "^  1 ")
    expect_equal(sum(!is.na(e$estimate[e$term == "am"])), 1L)
    expect_equal(sum(e$term == "am:1"), 1L)
  }
})