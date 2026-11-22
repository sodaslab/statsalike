# Further estimation commands and tests, compared with R's own functions.

d <- tibble::as_tibble(mtcars)

test_that("probit agrees with glm(binomial(probit))", {
  m <- s.probit(d, am, wt, hp, nolog = TRUE)
  # the reference fit with this strict tolerance warns that some fitted
  # probabilities of mtcars are numerically 0 or 1 (the warning comes from
  # glm.fit, not from s.probit())
  g <- suppressWarnings(glm(am ~ wt + hp, family = binomial("probit"), data = mtcars,
                            control = glm.control(epsilon = 1e-14, maxit = 100)))
  # both maximize the same likelihood; ml stops at Stata's tolerances
  expect_equal(m$estimate, unname(coef(g)[c(2, 3, 1)]), tolerance = 1e-5)
  expect_equal(s.ereturn(m)$ll, as.numeric(logLik(g)), tolerance = 1e-9)
  # the standard errors come from the observed information, as in Stata
  # (checked against Stata in test-stata-identity.R); glm() uses the expected
  # information, which differs for probit
})

test_that("oprobit agrees with MASS::polr(method = 'probit')", {
  skip_if_not_installed("MASS")
  m <- s.oprobit(d, gear, wt, hp, nolog = TRUE)
  p <- MASS::polr(factor(gear) ~ wt + hp, data = mtcars, method = "probit", Hess = TRUE)
  expect_equal(m$estimate, unname(c(coef(p), p$zeta)), tolerance = 1e-4)
  expect_equal(s.ereturn(m)$ll, as.numeric(logLik(p)), tolerance = 1e-6)
})

test_that("nbreg agrees with MASS::glm.nb", {
  skip_if_not_installed("MASS")
  m <- s.nbreg(warpbreaks, breaks, wool, tension, nolog = TRUE)
  g <- MASS::glm.nb(breaks ~ wool + tension, data = warpbreaks)
  est <- m$estimate[m$note == "" & is.na(m$equation)]
  expect_equal(est, unname(coef(g)[c(2, 3, 4, 1)]), tolerance = 1e-5)
  expect_equal(s.ereturn(m)$alpha, 1 / g$theta, tolerance = 1e-4)
  expect_equal(s.ereturn(m)$ll, as.numeric(logLik(g)), tolerance = 1e-6)
  out <- utils::capture.output(print(m))
  expect_true(any(grepl("LR test of alpha=0", out)))
})

test_that("tobit agrees with survival::survreg (left-censored normal model)", {
  skip_if_not_installed("survival")
  m <- s.tobit(d, mpg, wt, ll = 17, nolog = TRUE)
  y <- pmax(mtcars$mpg, 17)
  s <- survival::survreg(survival::Surv(y, y > 17, type = "left") ~ wt, data = mtcars, dist = "gaussian")
  expect_equal(m$estimate[1:2], unname(coef(s)[c(2, 1)]), tolerance = 1e-6)
  expect_equal(m$estimate[3], s$scale^2, tolerance = 1e-6)
  expect_equal(s.ereturn(m)$ll, as.numeric(logLik(s)), tolerance = 1e-8)
  expect_error(s.tobit(d, mpg, wt), "lower limit")
})

test_that("HC2 and HC3 agree with sandwich::vcovHC", {
  skip_if_not_installed("sandwich")
  f <- lm(mpg ~ wt + hp, data = mtcars)
  for (type in c("hc2", "hc3")) {
    m <- s.regress(d, mpg, wt, hp, vce = type)
    expect_equal(m$std_error, unname(sqrt(diag(sandwich::vcovHC(f, type = toupper(type))))[c(2, 3, 1)]),
                 tolerance = 1e-8)
  }
})

test_that("ttest agrees with t.test", {
  r <- s.ttest(d, mpg, mu = 20)
  t1 <- t.test(mtcars$mpg, mu = 20)
  expect_equal(r$t, unname(t1$statistic))
  expect_equal(r$p, t1$p.value)
  r <- s.ttest(d, mpg, by = am)
  t2 <- t.test(mpg ~ am, data = mtcars, var.equal = TRUE)
  expect_equal(r$t, unname(t2$statistic))
  r <- s.ttest(d, mpg, by = am, unequal = TRUE)
  t3 <- t.test(mpg ~ am, data = mtcars)
  expect_equal(r$df, unname(t3$parameter))
  expect_equal(r$p, t3$p.value)
  r <- s.ttest(d, drat, wt)
  t4 <- t.test(mtcars$drat, mtcars$wt, paired = TRUE)
  expect_equal(r$t, unname(t4$statistic))
  expect_error(s.ttest(d, mpg), "exactly one")
  expect_error(s.ttest(d, mpg, by = cyl), "two groups")
})

test_that("oneway and anova agree with R", {
  r <- s.oneway(d, mpg, cyl)
  a <- anova(lm(mpg ~ factor(cyl), data = mtcars))
  expect_equal(r$F, a$`F value`[1])
  b <- bartlett.test(mpg ~ factor(cyl), data = mtcars)
  expect_equal(r$chi2_bartlett, unname(b$statistic))
  # complete design: Stata's partial SS are type III sums of squares
  skip_if_not_installed("car")
  r <- s.anova(warpbreaks, breaks, "wool##tension")
  op <- options(contrasts = c("contr.sum", "contr.poly"))
  on.exit(options(op))
  c3 <- car::Anova(lm(breaks ~ wool * tension, data = warpbreaks), type = 3)
  expect_equal(r$ss[2:4], c3$`Sum Sq`[2:4], tolerance = 1e-8)
})

test_that("alpha agrees with the textbook formula", {
  X <- mtcars[c("disp", "hp", "wt")]
  k <- ncol(X)
  a <- k / (k - 1) * (1 - sum(apply(X, 2, var)) / var(rowSums(X)))
  r <- s.alpha(d, disp, hp, wt, asis = TRUE)
  expect_equal(r$alpha[nrow(r)], a)
  r2 <- s.alpha(d, mpg, disp, hp, wt, item = TRUE)
  expect_equal(r2$sign[r2$item == "mpg"], "-")
})

test_that("lincom, linktest and lroc", {
  m <- s.regress(d, mpg, wt, hp)
  l <- s.lincom(m, "wt + hp")
  V <- attr(m, "st_model")[[1]]$V
  expect_equal(l$estimate, sum(m$estimate[1:2]))
  expect_equal(l$std_error, sqrt(V[1, 1] + V[2, 2] + 2 * V[1, 2]))
  expect_error(s.lincom(m, "wt * hp"), "not linear")
  lk <- s.linktest(m)
  expect_equal(lk$variable, c("_hat", "_hatsq", "_cons"))
  g <- s.logit(d, am, wt, hp, nolog = TRUE)
  p <- fitted(glm(am ~ wt + hp, family = binomial, data = mtcars))
  w <- wilcox.test(p[mtcars$am == 1], p[mtcars$am == 0], exact = FALSE)
  auc <- unname(w$statistic) / (sum(mtcars$am == 1) * sum(mtcars$am == 0))
  expect_equal(s.lroc(g, graph = FALSE)$area, auc, tolerance = 1e-8)
})

test_that("margins pwcompare gives the differences of the margins", {
  m <- s.regress(d, mpg, "i.cyl", wt)
  mg <- s.margins(m, cyl)
  pw <- s.margins(m, cyl, pwcompare = TRUE)
  expect_equal(pw$contrast, c("6 vs 4", "8 vs 4", "8 vs 6"))
  expect_equal(pw$estimate, c(mg$estimate[2] - mg$estimate[1], mg$estimate[3] - mg$estimate[1],
                              mg$estimate[3] - mg$estimate[2]))
  expect_error(s.margins(m, dydx = wt, pwcompare = TRUE), "dydx")
})

test_that("ame = TRUE reports the average marginal effects", {
  l <- s.logit(d, am, wt, hp, nolog = TRUE)
  la <- s.logit(d, am, wt, hp, nolog = TRUE, ame = TRUE)
  mg <- s.margins(l, dydx = "*")
  expect_equal(la$estimate, mg$estimate)
  expect_equal(la$std_error, mg$std_error)
  out <- utils::capture.output(print(la))
  expect_true(any(grepl("Average marginal effects", out)))
  expect_error(s.logit(d, am, wt, or = TRUE, ame = TRUE), "cannot be combined")
  # an etable with log odds, odds ratios and average marginal effects
  lo <- s.logit(d, am, wt, hp, nolog = TRUE, or = TRUE)
  e <- s.etable("Log odds" = l, "Odds ratios" = lo, "AME" = la, mstat = c("N", "r2_p", "r2_t"))
  expect_equal(unique(e$model), c("Log odds", "Odds ratios", "AME"))
  expect_equal(e$estimate[e$model == "AME"], mg$estimate)
  out <- utils::capture.output(print(e))
  expect_true(any(grepl("Tjur's R-squared", out)))
  expect_true(any(grepl("Average marginal effects in model(s) 3.", out, fixed = TRUE)))
  # margins results directly
  e2 <- s.etable(l, mg, collabels = c("b", "dy/dx"), rowlabels = c(wt = "Weight"))
  expect_equal(unique(e2$model), c("b", "dy/dx"))
  expect_true(any(grepl("^Weight", utils::capture.output(print(e2)))))
  # ame also after the other models
  expect_s3_class(s.poisson(warpbreaks, breaks, wool, ame = TRUE, nolog = TRUE), "st_poisson")
  expect_s3_class(s.ologit(d, gear, wt, ame = TRUE, nolog = TRUE), "st_ologit")
})

test_that("Tjur's R-squared", {
  l <- s.logit(d, am, wt, hp, nolog = TRUE)
  p <- fitted(glm(am ~ wt + hp, family = binomial, data = mtcars))
  expect_equal(s.ereturn(l)$r2_t, mean(p[mtcars$am == 1]) - mean(p[mtcars$am == 0]), tolerance = 1e-8)
})

test_that("interactions are named without Stata's prefixes and noted", {
  dd <- d
  dd$cylf <- factor(dd$cyl)
  m <- s.regress(dd, mpg, "wt##cylf")
  out <- utils::capture.output(print(m))
  expect_true(any(grepl("cylf#wt", out, fixed = TRUE)))
  expect_false(any(grepl("c.wt", out, fixed = TRUE)))
  expect_true(any(grepl("Note: cylf#wt = cylf (factor) x wt (numeric).", out, fixed = TRUE)))
  withr::local_options(statsalike.stata_notation = TRUE)
  out <- utils::capture.output(print(m))
  expect_true(any(grepl("cylf#c.wt", out, fixed = TRUE)))
  expect_false(any(grepl("Note: cylf#wt", out, fixed = TRUE)))
})

test_that("plots use ggplot2's default design", {
  skip_if_not_installed("ggplot2")
  m <- s.regress(d, mpg, wt, hp)
  for (p in list(s.rvfplot(m), s.avplot(m, wt), s.avplots(m), s.cprplot(m, wt),
                 s.lsens(s.logit(d, am, wt, nolog = TRUE)),
                 s.marginsplot(s.margins(m, at = list(wt = 2:4))))) {
    expect_s3_class(p, "ggplot")
    expect_true(inherits(p$theme, "list") || length(p$theme) == 0L)
  }
})
