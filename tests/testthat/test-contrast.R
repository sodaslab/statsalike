# contrast, firstcell, dates and labelled regressors.

dc <- mtcars
dc$cyl <- factor(dc$cyl)
dc$am <- factor(dc$am, labels = c("automatic", "manual"))

test_that("joint tests of contrast are the Type III tests", {
  skip_if_not_installed("car")
  m <- s.regress(dc, mpg, wt, interact = "cyl##am")
  ct <- s.contrast(m, cyl, am, "cyl#am")
  a <- car::Anova(lm(mpg ~ wt + cyl * am, dc, contrasts = list(cyl = "contr.sum", am = "contr.sum")),
                  type = 3)
  expect_equal(ct$F, a[c("cyl", "am", "cyl:am"), "F value"], tolerance = 1e-8)
  expect_equal(ct$df, c(2, 1, 2))
  expect_equal(ct$p_value, a[c("cyl", "am", "cyl:am"), "Pr(>F)"], tolerance = 1e-8)
  out <- utils::capture.output(print(ct))
  expect_true(any(grepl("Contrasts of marginal linear predictions", out, fixed = TRUE)))
  expect_true(any(grepl("Denominator |         25", out, fixed = TRUE)))
})

test_that("r. contrasts without interactions are the coefficients", {
  m <- s.regress(dc, mpg, cyl, am, wt)
  ct <- s.contrast(m, r.cyl, effects = TRUE)
  b <- coef(lm(mpg ~ cyl + am + wt, dc))
  single <- ct[ct$contrast != "Joint", ]
  expect_equal(single$contrast, c("(6 vs 4)", "(8 vs 4)"))
  expect_equal(single$estimate, unname(b[c("cyl6", "cyl8")]), tolerance = 1e-8)
  expect_equal(single$F, single$statistic^2, tolerance = 1e-10)
  expect_equal(ct$F[ct$contrast == "Joint"], s.test(m, "cyl")$F, tolerance = 1e-8)
  out <- utils::capture.output(print(ct))
  expect_true(any(grepl("P>|t|", out, fixed = TRUE)))
})

test_that("contrast operators give the expected contrasts", {
  m <- s.regress(dc, mpg, cyl, wt)
  b <- coef(lm(mpg ~ cyl + wt, dc))
  mu <- c(0, b[["cyl6"]], b[["cyl8"]])
  est <- function(op) {
    ct <- s.contrast(m, paste0(op, ".cyl"))
    ct$estimate[ct$contrast != "Joint"]
  }
  expect_equal(est("a"), c(mu[1] - mu[2], mu[2] - mu[3]), tolerance = 1e-8)
  expect_equal(est("ar"), c(mu[2] - mu[1], mu[3] - mu[2]), tolerance = 1e-8)
  expect_equal(est("g"), mu - mean(mu), tolerance = 1e-8)
  expect_equal(est("h"), c(mu[1] - mean(mu[2:3]), mu[2] - mu[3]), tolerance = 1e-8)
  expect_equal(est("j"), c(mu[2] - mu[1], mu[3] - mean(mu[1:2])), tolerance = 1e-8)
  expect_equal(est("q"), unname(drop(t(contr.poly(3)) %*% mu)), tolerance = 1e-8)
  # all operators test the same joint hypothesis
  joint <- vapply(c("r", "a", "g", "h", "p"), function(op) {
    ct <- s.contrast(m, paste0(op, ".cyl"))
    ct$F[ct$contrast == "Joint"]
  }, numeric(1))
  expect_equal(unname(joint), rep(joint[[1]], 5), tolerance = 1e-8)
})

test_that("simple effects with @ and logit models", {
  m <- s.regress(dc, mpg, wt, interact = "cyl##am")
  ct <- s.contrast(m, "r.am@cyl")
  b <- coef(lm(mpg ~ wt + cyl * am, dc))
  expect_equal(ct$estimate[1:3], unname(b["ammanual"] + c(0, b["cyl6:ammanual"], b["cyl8:ammanual"])),
               tolerance = 1e-8)
  expect_equal(ct$contrast[1:3], paste("(manual vs automatic)", c(4, 6, 8)))
  expect_equal(ct$df[4], 3)
  l <- s.logit(dc, vs, am, wt, nolog = TRUE)
  cl <- s.contrast(l, am)
  expect_true("chi2" %in% names(cl))
  g <- glm(vs ~ am + wt, binomial, dc)
  expect_equal(cl$chi2, unname((coef(g)[2] / sqrt(vcov(g)[2, 2]))^2), tolerance = 1e-5)
})

test_that("contrast refuses continuous variables and unknown operators", {
  m <- s.regress(dc, mpg, cyl, wt)
  expect_error(s.contrast(m, wt), "not a factor variable")
  expect_error(s.contrast(m, "x.cyl"), "Unknown contrast operator")
  expect_error(s.contrast(m, gear), "not a variable of the model")
  expect_error(s.contrast(m), "Please name")
  # an interaction that is not in the model
  ct <- s.contrast(s.regress(dc, mpg, cyl, am), "cyl#am")
  expect_equal(ct$note, "omitted")
})

test_that("firstcell fills the top left cell", {
  m1 <- s.regress(mtcars, mpg, wt)
  out <- utils::capture.output(print(s.etable("M1" = m1, firstcell = "Model of mpg")))
  expect_true(any(grepl("^Model of mpg +\\|", out)))
  out <- utils::capture.output(print(s.table(mtcars, cyl, am, firstcell = "Count")))
  expect_true(any(grepl("^Count +\\|", out)))
  f <- tempfile(fileext = ".html")
  s.etable(m1, firstcell = "Model of mpg", export = f) |> expect_message()
  expect_true(any(grepl("Model of mpg", readLines(f, encoding = "UTF-8"), fixed = TRUE)))
  expect_error(s.etable(m1, firstcell = c("a", "b")), "single string")
})

test_that("dates enter as days since 1970 with a note", {
  set.seed(2)
  d <- data.frame(y = rnorm(40), day = as.Date("2020-01-01") + sample(0:400, 40))
  m <- s.regress(d, y, day)
  ref <- coef(lm(y ~ as.numeric(day), d))
  expect_equal(m$estimate, unname(ref[c(2, 1)]), tolerance = 1e-8)
  out <- utils::capture.output(print(m))
  expect_true(any(grepl("day enters as days since 1970-01-01", out, fixed = TRUE)))
})

test_that("labelled numeric regressors give a hint once", {
  skip_if_not_installed("haven")
  h <- mtcars
  h$carb_l <- haven::labelled(h$carb, c(one = 1, two = 2, four = 4))
  rlang::reset_message_verbosity("statsalike_labelled_hint_carb_l")
  expect_message(s.regress(h, mpg, carb_l), "has value labels")
  expect_no_message(s.regress(h, mpg, carb_l))
  withr::local_options(statsalike.labelled_hint = FALSE)
  rlang::reset_message_verbosity("statsalike_labelled_hint_carb_l")
  expect_no_message(s.regress(h, mpg, carb_l))
})
