df <- tibble::as_tibble(mtcars)
df$cyl_f <- factor(df$cyl)

test_that("regress matches lm()", {
  r <- s.regress(df, mpg, wt, cyl_f)
  expect_s3_class(r, "st_regress")
  fit <- summary(lm(mpg ~ wt + cyl_f, data = df))
  ref <- stats::coef(fit)[c(2:4, 1), ]
  coefs <- r[r$note == "", ]
  expect_equal(coefs$estimate, unname(ref[, 1]))
  expect_equal(coefs$std_error, unname(ref[, 2]))
  expect_equal(coefs$p_value, unname(ref[, 4]))
  e <- s.ereturn(r)
  expect_equal(e$F, unname(fit$fstatistic[1]))
  expect_equal(e$r2, fit$r.squared)
  expect_equal(e$r2_a, fit$adj.r.squared)
  expect_equal(e$rmse, fit$sigma)
  expect_equal(r$note[r$level %in% "4"], "base")
  out <- capture.output(print(r))
  expect_true(any(grepl("Source", out)))
  # as in Stata 15, the base category is not displayed
  expect_false(any(grepl("(base)", out, fixed = TRUE)))
})

test_that("regress: robust and cluster standard errors as in Stata", {
  X <- cbind(1, df$wt, df$hp)
  fit <- lm(mpg ~ wt + hp, df)
  e <- residuals(fit)
  bread <- solve(crossprod(X))
  n <- 32
  k <- 3
  hc1 <- bread %*% crossprod(X * e) %*% bread * n / (n - k)
  r <- s.regress(df, mpg, wt, hp, vce = "robust")
  expect_equal(r$std_error, sqrt(diag(hc1))[c(2, 3, 1)])
  expect_output(print(r), "Robust")

  U <- rowsum(X * e, df$gear)
  G <- nrow(U)
  cr1 <- bread %*% crossprod(U) %*% bread * (G / (G - 1)) * ((n - 1) / (n - k))
  rc <- s.regress(df, mpg, wt, hp, cluster = gear)
  expect_equal(rc$std_error, sqrt(diag(cr1))[c(2, 3, 1)])
  expect_output(print(rc), "adjusted for 3 clusters in gear")
})

test_that("regress: weights, beta, noconstant, collinearity", {
  ra <- s.regress(df, mpg, hp, wt = wt, weight_type = "aweight")
  la <- stats::coef(summary(lm(mpg ~ hp, df, weights = wt)))
  expect_equal(c(ra$estimate, ra$std_error), unname(c(la[c(2, 1), 1], la[c(2, 1), 2])))

  d <- tibble::tibble(x = c(1, 2, 3, 4, 5), y = c(2, 1, 4, 3, 6), f = c(2, 1, 3, 1, 2))
  long <- d[rep(1:5, d$f), ]
  expect_equal(s.regress(d, y, x, wt = f)$std_error, s.regress(long, y, x)$std_error)

  rb <- s.regress(df, mpg, wt, hp, beta = TRUE)
  expect_equal(rb$beta[1:2],
               unname(stats::coef(lm(scale(mpg) ~ scale(wt) + scale(hp), df))[2:3]))
  expect_output(print(rb), "Beta")

  rn <- s.regress(df, mpg, wt, noconstant = TRUE)
  expect_equal(rn$estimate, unname(stats::coef(lm(mpg ~ 0 + wt, df))))

  df$wt2 <- df$wt * 2
  ro <- s.regress(df, mpg, wt, wt2)
  # as Stata, regress drops the collinear variable with the smaller scale
  expect_equal(ro$note, c("omitted", "", ""))
  expect_equal(s.logit(df, am, wt, wt2, nolog = TRUE)$note, c("", "omitted", ""))
  expect_output(print(ro), "(omitted)", fixed = TRUE)
})

test_that("logit matches glm() and Stata's statistics", {
  l <- s.logit(df, am, mpg, wt)
  expect_s3_class(l, "st_logit")
  g <- glm(am ~ mpg + wt, df, family = binomial, control = glm.control(epsilon = 1e-14))
  expect_equal(l$estimate, unname(stats::coef(g)[c(2, 3, 1)]), tolerance = 1e-6)
  expect_equal(l$std_error, unname(sqrt(diag(stats::vcov(g)))[c(2, 3, 1)]), tolerance = 1e-6)
  e <- s.ereturn(l)
  expect_equal(e$ll, as.numeric(stats::logLik(g)), tolerance = 1e-8)
  expect_equal(e$chi2, g$null.deviance - g$deviance, tolerance = 1e-6)
  expect_equal(e$r2_p, 1 - as.numeric(stats::logLik(g)) / (-g$null.deviance / 2), tolerance = 1e-6)
  out <- capture.output(print(l))
  expect_true(any(grepl("^Iteration 0:", out)))
  expect_true(any(grepl("LR chi2(2)", out, fixed = TRUE)))
  expect_false(any(grepl("Iteration", capture.output(print(s.logit(df, am, mpg, nolog = TRUE))))))

  o <- s.logit(df, am, mpg, or = TRUE)
  g1 <- glm(am ~ mpg, df, family = binomial, control = glm.control(epsilon = 1e-14))
  expect_equal(o$estimate, unname(exp(stats::coef(g1))[c(2, 1)]), tolerance = 1e-6)
  expect_output(print(o), "Odds Ratio")
  expect_output(print(o), "baseline odds")

  # robust: HC0 * N / (N - 1)
  p <- stats::fitted(g1)
  X <- cbind(1, df$mpg)
  bread <- solve(crossprod(X * sqrt(p * (1 - p))))
  V <- bread %*% crossprod(X * (df$am - p)) %*% bread * 32 / 31
  expect_equal(s.logit(df, am, mpg, vce = "robust")$std_error, sqrt(diag(V))[c(2, 1)],
               tolerance = 1e-6)

  expect_error(s.logit(df, vs, mpg, wt = wt, weight_type = "aweight"), "should be one of")
  expect_error(s.logit(dplyr::mutate(df, one = 1), one, mpg), "does not vary")
})

test_that("numerically difficult and invalid models", {
  # regressors on very different scales (solve(crossprod(X)) would fail)
  set.seed(2)
  d <- tibble::tibble(x1 = rnorm(200) * 1e9, x2 = rnorm(200) * 1e-6, y = rnorm(200))
  d$b <- as.numeric(d$y + d$x2 * 1e6 > 0)
  r <- s.regress(d, y, x1, x2)
  expect_equal(r$estimate, unname(stats::coef(lm(y ~ x1 + x2, d))[c(2, 3, 1)]))
  l <- s.logit(d, b, x1, x2, nolog = TRUE)
  g <- glm(b ~ x1 + x2, d, family = binomial, control = glm.control(epsilon = 1e-14))
  expect_equal(l$estimate, unname(stats::coef(g)[c(2, 3, 1)]), tolerance = 1e-6)

  # complete separation: no estimates, as in Stata
  sep <- tibble::tibble(x = 1:20, y = as.numeric(1:20 > 10))
  expect_error(s.logit(sep, y, x), "predict the outcome perfectly")

  # negative weights
  expect_error(s.regress(df, mpg, wt, wt = qsec - 17, weight_type = "iweight"), "Negative weights")

  # fewer clusters than coefficients: F cannot be computed (shown as NA)
  rc <- s.regress(df, mpg, wt, hp, cluster = am)
  expect_true(is.na(s.ereturn(rc)$F))
  expect_output(print(rc), "F(0, 1)           =          .", fixed = TRUE)

  # extreme values are printed compactly
  huge <- tibble::tibble(x = 1:30, y = 1e15 * (1:30) + rnorm(30))
  out <- capture.output(print(s.regress(huge, y, x)))
  expect_lte(max(nchar(out)), 80)
})

test_that("grouped models, model statistics and export", {
  g <- df |> dplyr::group_by(am) |> s.regress(mpg, wt)
  expect_equal(nrow(s.ereturn(g)), 2)
  expect_output(print(g), "-> am = 1")
  f <- tempfile(fileext = ".html")
  expect_message(s.export(list(s.summarize(df, mpg), s.codebook(df, mpg), s.regress(df, mpg, wt),
                               s.logit(df, am, mpg)), f))
  html <- paste(readLines(f), collapse = "\n")
  expect_match(html, "regress mpg wt", fixed = TRUE)
  expect_match(html, "logit am mpg", fixed = TRUE)
  expect_match(html, "summarize mpg", fixed = TRUE)
  expect_error(s.ereturn(s.tab(df, cyl)), "s.regress")
})

test_that("output follows Stata's display rules", {
  # %9.0g keeps a column for the sign: Stata prints .0680008, not .06800083
  expect_equal(statsalike:::st_stata_g(c(0.068000829, -0.068000829, 824.1717612, 0), 9L),
               c(".0680008", "-.0680008", "824.1718", "0"))
  d <- mtcars
  d$wt2 <- 2 * d$wt
  out <- capture.output(print(s.regress(d, mpg, wt, wt2, hp)))
  expect_equal(out[1], "note: wt omitted because of collinearity")
  expect_true(any(grepl("^ +wt \\| +0  \\(omitted\\)$", out)))
  # factor levels end in column 11, as in Stata ("          6  |")
  out <- capture.output(print(s.regress(mtcars, mpg, wt, cyl, categorical = cyl)))
  expect_true(any(grepl("^          6  \\|", out)))
})

test_that("header and coefficient table end in the same column (78), as in Stata", {
  right_edges <- function(x) {
    out <- capture.output(print(x))
    out <- out[nzchar(out) & !grepl("^Iteration|^note:", out)]
    unique(nchar(out[grepl("=|-{20}|\\|", out)]))
  }
  d <- dplyr::mutate(mtcars, cyl = factor(cyl))
  expect_equal(max(right_edges(s.regress(d, mpg, wt, cyl))), 78)
  expect_equal(max(right_edges(s.regress(d, mpg, wt, cyl, vce = "robust"))), 78)
  expect_equal(max(right_edges(s.regress(d, mpg, wt, hp, beta = TRUE))), 78)
  expect_equal(max(right_edges(s.logit(d, am, wt, nolog = TRUE))), 78)
  expect_equal(max(right_edges(s.logit(d, am, wt, or = TRUE, nolog = TRUE))), 78)
  # the frame of the coefficient table is exactly 78 characters wide
  out <- capture.output(print(s.logit(d, am, wt, nolog = TRUE)))
  expect_true(all(nchar(out[grepl("^-+$", out)]) == 78))
})
