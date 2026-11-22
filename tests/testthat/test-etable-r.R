d <- mtcars
d$cylf <- factor(d$cyl)
d$auto <- d$am == 0

test_that("lm() is read directly and matches summary()", {
  fit <- lm(mpg ~ wt + cylf + auto + wt:hp, d)
  e <- s.etable(fit, mstat = c("N", "r2", "r2_a", "F", "rmse"))
  s <- summary(fit)
  expect_equal(e$term, c("wt", "cylf:6", "cylf:8", "auto:TRUE", "wt:hp", "_cons"))
  ct <- s$coefficients[c("wt", "cylf6", "cylf8", "autoTRUE", "wt:hp", "(Intercept)"), ]
  expect_equal(e$estimate, unname(ct[, 1]))
  expect_equal(e$std_error, unname(ct[, 2]))
  expect_equal(e$p_value, unname(ct[, 4]))
  ci <- confint(fit)[c("wt", "cylf6", "cylf8", "autoTRUE", "wt:hp", "(Intercept)"), ]
  expect_equal(e$conf_low, unname(ci[, 1]))
  expect_equal(e$conf_high, unname(ci[, 2]))
  meta <- attr(e, "st_meta")
  expect_equal(meta$r2, s$r.squared)
  expect_equal(meta$r2_a, s$adj.r.squared)
  expect_equal(meta$F, unname(s$fstatistic[1]))
  expect_equal(meta$rmse, s$sigma)
  expect_equal(meta$N, 32)
  out <- capture.output(print(e))
  expect_true(any(grepl("^cylf", out)))
  expect_true(any(grepl("^  8 ", out)))
  expect_false(any(grepl("(base)", out, fixed = TRUE)))
  expect_true(any(grepl("(base)", capture.output(print(s.etable(fit, showbase = TRUE))), fixed = TRUE)))
})

test_that("lm() equals s.regress() in one table", {
  fit <- lm(mpg ~ wt + cylf, d)
  sr <- s.regress(d, mpg, wt, cylf)
  e <- s.etable("lm" = fit, "s.regress" = sr, mstat = c("N", "r2", "ll"))
  expect_equal(attr(e, "st_meta")$ll[1], attr(e, "st_meta")$ll[2])
  expect_equal(s.ereturn(sr)$ll, as.numeric(logLik(fit)))
  a <- e[e$model == "lm", ]
  b <- e[e$model == "s.regress", ]
  expect_equal(a$term, b$term)
  expect_equal(a$estimate, b$estimate)
  expect_equal(a$std_error, b$std_error)
  expect_equal(a$conf_low, b$conf_low)
  expect_equal(attr(e, "st_meta")$r2[1], attr(e, "st_meta")$r2[2])
})

test_that("glm() is read directly with Wald intervals and Stata's statistics", {
  fit <- glm(am ~ wt + hp, binomial, d)
  e <- s.etable(fit, mstat = c("N", "r2_p", "r2_n", "ll", "chi2"))
  expect_equal(e$estimate, unname(coef(fit)[c("wt", "hp", "(Intercept)")]))
  ci <- confint.default(fit)[c("wt", "hp", "(Intercept)"), ]
  expect_equal(e$conf_low, unname(ci[, 1]))
  lg <- s.logit(d, am, wt, hp, nolog = TRUE)
  m_glm <- attr(e, "st_meta")
  m_st <- attr(s.etable(lg, mstat = c("N", "r2_p", "r2_n", "ll", "chi2")), "st_meta")
  expect_equal(m_glm$r2_p, m_st$r2_p, tolerance = 1e-6)
  expect_equal(m_glm$ll, m_st$ll, tolerance = 1e-6)
  expect_equal(m_glm$chi2, m_st$chi2, tolerance = 1e-6)
  expect_equal(m_glm$r2_n, m_st$r2_n, tolerance = 1e-6)
  skip_if_not_installed("performance")
  expect_equal(m_glm$r2_n, unname(performance::r2_nagelkerke(fit)), tolerance = 1e-6)
  # odds ratios
  eo <- s.etable(fit, eform = TRUE)
  expect_equal(eo$estimate, unname(exp(coef(fit))[c("wt", "hp", "(Intercept)")]))
})

test_that("glm() families and aliased coefficients", {
  g <- glm(mpg ~ wt, Gamma, d)
  e <- s.etable(g, mstat = c("N", "r2", "r2_p", "chi2", "ll"))
  meta <- attr(e, "st_meta")
  expect_true(is.na(meta$r2_p))
  expect_true(is.na(meta$chi2))
  expect_equal(meta$ll, as.numeric(logLik(g)))
  gg <- glm(mpg ~ wt, gaussian, d)
  expect_equal(attr(s.etable(gg), "st_meta")$r2, summary(lm(mpg ~ wt, d))$r.squared)
  qp <- glm(carb ~ wt, quasipoisson, d)
  expect_true(is.na(attr(s.etable(qp, mstat = c("N", "ll")), "st_meta")$ll))
  al <- lm(mpg ~ wt + I(2 * wt), d)
  ea <- s.etable(al)
  expect_equal(ea$note[ea$term == "I(2 * wt)"], "omitted")
  expect_true(any(grepl("(omitted)", capture.output(print(ea)), fixed = TRUE)))
})

test_that("variable labels of haven data in lm()", {
  skip_if_not_installed("haven")
  hd <- tibble::tibble(y = haven::labelled(d$mpg, label = "Miles per gallon"),
                       x = haven::labelled(d$wt, label = "Weight"))
  out <- capture.output(print(s.etable(lm(y ~ x, hd))))
  expect_true(any(grepl("Miles per gallon", out)))
  expect_true(any(grepl("^Weight", out)))
  # lm() drops unused levels (here the label of a missing-value code) and
  # loses the variable label; it is taken from the data instead
  skip_if_not_installed("insight")
  fd <- data.frame(y = d$mpg)
  fd$s <- haven::as_factor(haven::labelled(d$am, c(Refused = -9, Automatic = 0, Manual = 1),
                                           label = "Transmission"))
  out <- capture.output(print(s.etable(lm(y ~ s, fd))))
  expect_true(any(grepl("^Transmission", out)))
  expect_true(any(grepl("^  Manual", out)))
  expect_false(any(grepl("Refused", out)))
})

test_that("other model classes via parameters, as in sjPlot::tab_model()", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("MASS")
  nb <- suppressWarnings(MASS::glm.nb(carb ~ wt, d))
  mp <- as.data.frame(parameters::model_parameters(nb, verbose = FALSE))
  e <- s.etable(nb)
  expect_equal(sort(e$estimate), sort(mp$Coefficient))
  expect_equal(sort(e$conf_low), sort(mp$CI_low))

  d$gearo <- factor(d$gear, ordered = TRUE)
  po <- MASS::polr(gearo ~ wt, d, Hess = TRUE)
  out <- capture.output(print(s.etable(po)))
  expect_true(any(grepl("^Cut points", out)))
  expect_true(any(grepl("^3|4", out)))
  expect_equal(nrow(s.etable(po)), 3L)
})

test_that("further equations: multinom, zeroinfl, mixed models, coxph", {
  skip_if_not_installed("parameters")
  skip_if_not_installed("performance")
  if (requireNamespace("nnet", quietly = TRUE)) {
    mn <- nnet::multinom(factor(gear) ~ wt, d, trace = FALSE)
    out <- capture.output(print(s.etable(mn)))
    expect_true(any(grepl("^Outcome 4", out)))
    expect_true(any(grepl("^Outcome 5", out)))
    expect_equal(nrow(s.etable(mn)), 4L)
  }
  if (requireNamespace("pscl", quietly = TRUE)) {
    d$cz <- d$carb - 1
    zi <- pscl::zeroinfl(cz ~ wt | hp, d)
    e <- s.etable(zi, rowlabels = c("zero_inflated::hp" = "Horsepower (zero part)",
                                 "zero_inflated::" = "Zero part"))
    out <- capture.output(print(e))
    expect_true(any(grepl("^Zero part", out)))
    expect_true(any(grepl("^Horsepower \\(zero part\\)", out)))
    expect_equal(e$estimate[e$term == "zero_inflated::hp"], unname(coef(zi)["zero_hp"]))
    expect_equal(sum(e$term == "_cons"), 1L)
    expect_equal(sum(e$term == "zero_inflated::_cons"), 1L)
  }
  if (requireNamespace("lme4", quietly = TRUE)) {
    mm <- lme4::lmer(mpg ~ wt + (1 | cyl), d)
    e <- s.etable(mm, mstat = c("N", "r2_m", "r2_c"))
    expect_equal(e$estimate, unname(lme4::fixef(mm)[c("wt", "(Intercept)")]))
    r2 <- performance::r2(mm)
    expect_equal(attr(e, "st_meta")$r2_m, unname(r2$R2_marginal))
    expect_equal(attr(e, "st_meta")$r2_c, unname(r2$R2_conditional))
  }
  if (requireNamespace("survival", quietly = TRUE)) {
    cx <- survival::coxph(survival::Surv(mpg, am) ~ wt, d)
    e <- s.etable(cx, mstat = c("N", "r2", "r2_n"))
    expect_equal(e$term, "wt")
    expect_equal(e$estimate, unname(coef(cx)))
    # performance reports Nagelkerke's R2 for coxph; it must not appear as R2
    expect_true(is.na(attr(e, "st_meta")$r2))
  }
})

test_that("unsupported objects give a clear error", {
  expect_error(s.etable(structure(list(a = 1), class = "foo")), "not supported|parameters")
  expect_error(s.etable(mtcars), "estimation results")
})
