#' Probit regression (Stata: `probit`)
#'
#' Re-implementation of Stata's
#' `probit depvar [indepvars] [weight] [, vce(robust|cluster) level(#) nolog noconstant]`
#' with Stata's output. Iteration log, model statistics (likelihood-ratio
#' test, pseudo R-squared) and the coefficient table with z tests.
#'
#' The model is the same as [s.logit()] with the standard normal
#' distribution in place of the logistic one. As there, 0 is failure and
#' every other nonmissing value success, variables that predict the outcome
#' perfectly are dropped with their observations, and regressors, factor
#' variables, interactions, weights, standard errors and groups are handled
#' as in [s.regress()].
#'
#' @inheritParams s.logit
#' @return A tibble of class `st_probit` with one row per coefficient (with
#'   `ame = TRUE` one row per average marginal effect). Model statistics are
#'   returned by [s.ereturn()].
#' @examples
#' s.probit(mtcars, am, wt, hp)
#' s.probit(mtcars, am, wt, hp, nolog = TRUE, ame = TRUE)
#' @export
s.probit <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                     wt = NULL, weight_type = c("fweight", "iweight", "pweight"),
                     vce = c("ols", "robust", "cluster"), cluster = NULL,
                     level = 95, ame = FALSE, nolog = FALSE, noconstant = FALSE,
                     export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "probit", rlang::current_env())
  fits <- st_fit_groups(set, function(D) st_logit_fit(D, set, level, FALSE, noconstant, link = "probit"),
                        noconstant)
  st_reg_output(set, fits, "st_probit", opts = c(if (nolog) "nolog", if (noconstant) "noconstant"),
                export = export, nolog = nolog, ame = ame)
}

#' @export
print.st_probit <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

#' Ordered probit regression (Stata: `oprobit`)
#'
#' Re-implementation of Stata's
#' `oprobit depvar [indepvars] [weight] [, vce(robust|cluster) level(#) nolog]`
#' with Stata's output. Iteration log, model statistics, the coefficients and
#' the cut points (`/cut1`, `/cut2`, ...). The model is the one of
#' [s.ologit()] with the standard normal distribution in place of the
#' logistic one.
#'
#' @inheritParams s.ologit
#' @return A tibble of class `st_oprobit` with one row per coefficient and
#'   cut point. [s.margins()] gives the effects on the probabilities of the
#'   outcomes.
#' @examples
#' s.oprobit(mtcars, gear, wt, hp)
#' @export
s.oprobit <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                      wt = NULL, weight_type = c("fweight", "iweight", "pweight"),
                      vce = c("ols", "robust", "cluster"), cluster = NULL,
                      level = 95, ame = FALSE, nolog = FALSE, export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "oprobit", rlang::current_env())
  fits <- st_fit_groups(set, function(D) st_ologit_fit(D, set, level, FALSE, link = "probit"), TRUE)
  st_reg_output(set, fits, "st_oprobit", opts = if (nolog) "nolog",
                export = export, nolog = nolog, ame = ame)
}

#' @export
print.st_oprobit <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

#' Negative binomial regression (Stata: `nbreg`)
#'
#' Re-implementation of Stata's
#' `nbreg depvar [indepvars] [weight] [, exposure(varname) offset(varname) irr vce(robust|cluster) level(#) nolog]`
#' (mean dispersion, the NB2 model) with Stata's output. The iteration logs
#' of the Poisson model, the constant-only model and the full model, the
#' coefficient table with the overdispersion parameter (`/lnalpha` and
#' `alpha`) and the likelihood-ratio test of `alpha = 0`, i.e. of the Poisson
#' model against the negative binomial model.
#'
#' As in Stata, the Poisson model gives the starting values of the full
#' model (its constant shifted to the constant-only negative binomial model).
#' The test of `alpha = 0` is on the boundary of the parameter space, so its
#' p-value is half the chi-squared p-value (`chibar2(01)`). It is not reported
#' with robust or cluster-robust standard errors and with sampling weights.
#'
#' @inheritParams s.poisson
#' @return A tibble of class `st_nbreg` with one row per coefficient and for
#'   `lnalpha` and `alpha` (`equation` `"/"`). Model statistics, including
#'   `alpha`, the log likelihood of the Poisson model and the test of
#'   `alpha = 0`, are returned by [s.ereturn()].
#' @examples
#' s.nbreg(warpbreaks, breaks, wool, tension)
#' s.nbreg(warpbreaks, breaks, wool, irr = TRUE, nolog = TRUE)
#' @export
s.nbreg <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                    exposure = NULL, offset = NULL,
                    wt = NULL, weight_type = c("fweight", "iweight", "pweight"),
                    vce = c("ols", "robust", "cluster"), cluster = NULL,
                    level = 95, irr = FALSE, ame = FALSE, nolog = FALSE, noconstant = FALSE,
                    export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "nbreg", rlang::current_env(),
                      offset_quo = rlang::enquo(offset), exposure_quo = rlang::enquo(exposure))
  fits <- st_fit_groups(set, function(D) st_nbreg_fit(D, set, level, irr, noconstant), noconstant)
  st_reg_output(set, fits, "st_nbreg", opts = c(if (irr) "irr", if (nolog) "nolog",
                                                if (noconstant) "noconstant"),
                export = export, nolog = nolog, extra_spec = list(eform = irr, irr = irr), ame = ame)
}

#' @export
print.st_nbreg <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

#' Tobit regression (Stata: `tobit`)
#'
#' Re-implementation of Stata's
#' `tobit depvar [indepvars] [weight], ll[(#)] ul[(#)] [vce(robust|cluster) level(#) nolog]`
#' for a dependent variable that is censored from below (`ll`), from above
#' (`ul`) or both, with the output of Stata 15: the numbers of uncensored and
#' censored observations, the model statistics, the coefficients with t
#' tests and the variance of the error (`var(e.y)`).
#'
#' `ll = TRUE` (`ul = TRUE`) takes the smallest (largest) value of `y` as
#' the limit, as Stata's `ll` without a value. Observations at or below the
#' lower limit (at or above the upper limit) are censored. The coefficients
#' describe the latent variable. [s.margins()] and [s.predict()] give the
#' linear prediction. The estimates are identical to Stata's. The iteration
#' log can differ, because Stata 15 refines its starting values with a grid
#' search that statsalike does not reproduce.
#'
#' @inheritParams s.logit
#' @param ll,ul Lower and upper limit. A number, or `TRUE` for the minimum
#'   (maximum) of `y`. At least one is needed.
#' @return A tibble of class `st_tobit` with one row per coefficient and one
#'   for the variance of the error (`equation` `"/"`).
#' @examples
#' s.tobit(mtcars, mpg, wt, ll = 17)
#' s.tobit(mtcars, mpg, wt, am, ll = 17, ul = 30, nolog = TRUE)
#' @export
s.tobit <- function(.data, y, ..., ll = NULL, ul = NULL, categorical = NULL, interact = NULL,
                    base = NULL, wt = NULL, weight_type = c("fweight", "aweight", "iweight", "pweight"),
                    vce = c("ols", "robust", "cluster"), cluster = NULL,
                    level = 95, ame = FALSE, nolog = FALSE, noconstant = FALSE, export = NULL) {
  if (is.null(ll) && is.null(ul)) rlang::abort("Please give a lower limit `ll`, an upper limit `ul` or both.")
  for (a in list(list(v = ll, n = "ll"), list(v = ul, n = "ul"))) {
    if (!is.null(a$v) && !isTRUE(a$v) && !(is.numeric(a$v) && length(a$v) == 1L && !is.na(a$v))) {
      rlang::abort(sprintf("`%s` must be a number or TRUE.", a$n))
    }
  }
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "tobit", rlang::current_env())
  fits <- st_fit_groups(set, function(D) st_tobit_fit(D, set, level, ll, ul, noconstant), noconstant)
  lim <- function(v, n) if (is.null(v)) NULL else if (isTRUE(v)) n else sprintf("%s(%s)", n, format(v))
  st_reg_output(set, fits, "st_tobit", opts = c(lim(ll, "ll"), lim(ul, "ul"), if (nolog) "nolog",
                                                if (noconstant) "noconstant"),
                export = export, nolog = nolog, ame = ame)
}

#' @export
print.st_tobit <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

# Negative binomial ------------------------------------------------------------

# Log likelihood of the NB2 model with its derivatives with respect to the
# linear predictor (eta) and ln(alpha): ll, scores and second derivatives per
# observation.
st_nb_terms <- function(y, eta, la) {
  a <- exp(la)
  mu <- exp(eta)
  am <- a * mu
  r <- 1 / a
  ll <- lgamma(y + r) - lgamma(r) - lgamma(y + 1) - (r + y) * log1p(am) + y * (la + eta)
  g_eta <- (y - mu) / (1 + am)
  S <- digamma(r) - digamma(y + r) + log1p(am)
  Da <- S / a^2 - (r + y) * mu / (1 + am) + y / a
  g_la <- a * Da
  h_ee <- -mu * (1 + a * y) / (1 + am)^2
  h_el <- -am * (y - mu) / (1 + am)^2
  Daa <- -2 * S / a^3 + (-trigamma(r) / a^2 + trigamma(y + r) / a^2 + mu / (1 + am)) / a^2 +
    mu / (a^2 * (1 + am)) + (r + y) * mu^2 / (1 + am)^2 - y / a^2
  h_ll <- a * Da + a^2 * Daa
  list(ll = ll, g_eta = g_eta, g_la = g_la, h_ee = h_ee, h_el = h_el, h_ll = h_ll)
}

st_nbreg_fit <- function(D, set, level, irr, noconstant) {
  y <- D$y
  if (any(y < 0)) rlang::abort(sprintf("`%s` must be greater than or equal to zero.", set$y_name))
  w <- D$w
  off <- D$offset %||% rep(0, D$n)
  robust <- set$vce != "ols" || (set$weighted && set$weight_type == "pweight")
  # 1. Poisson model: comparison model for the test of alpha = 0 (not
  # reported with robust standard errors) and starting values
  pois <- st_poisson_fit(D, set, level, FALSE, noconstant)
  ok <- pois$ok
  Xk <- D$X[, ok, drop = FALSE]
  k <- sum(ok)
  cons_k <- st_cons_index(D, ok)
  fn_of <- function(X) {
    kx <- ncol(X)
    function(theta) {
      b <- theta[seq_len(kx)]
      la <- theta[kx + 1L]
      eta <- drop(X %*% b) + off
      tm <- st_nb_terms(y, eta, la)
      g <- c(as.vector(crossprod(X, w * tm$g_eta)), sum(w * tm$g_la))
      H <- matrix(0, kx + 1L, kx + 1L)
      H[seq_len(kx), seq_len(kx)] <- crossprod(X, X * (w * tm$h_ee))
      H[seq_len(kx), kx + 1L] <- H[kx + 1L, seq_len(kx)] <- as.vector(crossprod(X, w * tm$h_el))
      H[kx + 1L, kx + 1L] <- sum(w * tm$h_ll)
      list(ll = sum(w * tm$ll), g = g, H = H,
           S = cbind(X * (w * tm$g_eta), w * tm$g_la))
    }
  }
  # 2. constant-only model, starting from ln(mean) and ln(alpha) = 0
  ll0 <- NA_real_
  log0 <- NULL
  b0 <- NULL
  if (!noconstant) {
    c0 <- log(sum(w * y) / sum(w * exp(off)))
    one <- matrix(1, D$n, 1L)
    opt0 <- st_ml_max(fn_of(one), c(c0, 0))
    b0 <- opt0$theta
    ll0 <- opt0$ll
    log0 <- list(ll = opt0$log, flags = opt0$flags)
  }
  # 3. starting values of the full model: the Poisson coefficients with the
  # constant shifted so that the mean linear prediction equals the constant
  # of the constant-only model, and its ln(alpha)
  fn <- fn_of(Xk)
  start <- c(pois$b, 0)
  if (!noconstant && !is.null(cons_k) && k > 1L) {
    xb <- drop(Xk %*% pois$b)
    bp <- pois$b
    bp[cons_k] <- bp[cons_k] + b0[1] - sum(w * xb) / sum(w)
    cand <- c(bp, b0[2])
    start <- if (is.finite(fn(cand)$ll) && fn(cand)$ll > ll0) cand else {
      s <- rep(0, k + 1L)
      s[cons_k] <- b0[1]
      s[k + 1L] <- b0[2]
      s
    }
  } else if (!noconstant && !is.null(cons_k)) {
    start <- b0
  }
  opt <- st_ml_max(fn, start)
  fin <- fn(opt$theta)
  N <- st_ml_N(set, D)
  vc <- st_ml_vce(fin$H, fin$S, D, set, N, k + 1L)
  V <- vc$V
  noncons <- setdiff(seq_len(k), cons_k)
  df_m <- length(noncons)
  chi2 <- if (set$vce == "ols") 2 * (opt$ll - ll0) else
    if (anyNA(V)) NA_real_ else st_wald(opt$theta[noncons], V[noncons, noncons, drop = FALSE])
  wr <- st_wald_rank(V, k + 1L, df_m, set$vce)
  if (wr$deficient) chi2 <- NA_real_
  la <- opt$theta[k + 1L]
  chibar <- if (!robust) max(0, 2 * (opt$ll - pois$stats$ll)) else NA_real_
  stats <- list(N = N, df_m = df_m, df_test = wr$df %||% df_m, chi2 = chi2,
                p = if (df_m > 0) stats::pchisq(chi2, df_m, lower.tail = FALSE) else NA_real_,
                r2_p = 1 - opt$ll / ll0, ll = opt$ll, ll_0 = ll0, ll_c = pois$stats$ll,
                alpha = exp(la), chi2_c = chibar,
                p_c = if (is.na(chibar)) NA_real_ else 0.5 * stats::pchisq(chibar, 1, lower.tail = FALSE),
                n_clust = vc$G, iterations = length(opt$log) - 1L, converged = opt$converged)
  ncol_full <- length(D$cols)
  coefs <- st_ml_coefs(opt$theta, V, level, if (irr) "irr", cut = k + 1L)
  # alpha = exp(ln(alpha)) with the delta-method standard error
  se_la <- coefs$se[k + 1L]
  crit <- stats::qnorm(1 - (1 - level / 100) / 2)
  for (nm in names(coefs)) {
    coefs[[nm]] <- c(coefs[[nm]], switch(nm, estimate = exp(la), se = exp(la) * se_la, stat = NA_real_,
                                         p = NA_real_, low = exp(la - crit * se_la), high = exp(la + crit * se_la)))
  }
  rows <- st_design_rows(D)
  if (!is.null(set$off_type)) {
    lab <- if (set$off_type == "exposure") sprintf("ln(%s)", set$off_name) else set$off_name
    rows <- append(rows, list(list(variable = lab, level = NA_character_, type = set$off_type,
                                   col = NA_integer_, term = NA_integer_, label = lab)))
  }
  rows <- c(rows, list(
    list(variable = "/", level = NA_character_, type = "block", col = NA_integer_, block = "/", title = "/",
         term = NA_integer_),
    list(variable = "lnalpha", level = NA_character_, type = "coef", col = ncol_full + 1L, block = "/",
         label = "/lnalpha", term = NA_integer_),
    list(variable = "alpha", level = NA_character_, type = "coef", col = ncol_full + 2L, block = "alpha",
         label = "alpha", term = NA_integer_)))
  notes <- c(st_empty_notes(D), st_omit_notes(D, ok))
  logs <- list(
    list(title = "Fitting Poisson model:", ll = pois$log, flags = pois$flags),
    if (!is.null(log0)) list(title = "Fitting constant-only model:", ll = log0$ll, flags = log0$flags),
    list(title = "Fitting full model:", ll = opt$log, flags = opt$flags))
  list(D = D, ok = c(ok, TRUE, TRUE), ok_cols = ok, b = opt$theta, V = V, coefs = coefs,
       cut_cols = ncol_full + 1:2, stats = stats, log = opt$log, flags = opt$flags,
       logs = Filter(Negate(is.null), logs), df_t = Inf, rows = rows, notes = notes, k = k)
}

# Tobit ------------------------------------------------------------------------

# Log likelihood of the censored normal model with its derivatives with
# respect to the linear predictor (eta) and s = ln(sigma), per observation;
# `cens` is -1 (left-censored), 0 (uncensored) or 1 (right-censored).
st_tobit_terms <- function(y, eta, s, cens, lo, hi) {
  sig <- exp(s)
  n <- length(y)
  ll <- g_e <- g_s <- h_ee <- h_es <- h_ss <- numeric(n)
  u <- cens == 0
  z <- (y[u] - eta[u]) / sig
  ll[u] <- stats::dnorm(z, log = TRUE) - s
  g_e[u] <- z / sig
  g_s[u] <- z^2 - 1
  h_ee[u] <- -1 / sig^2
  h_es[u] <- -2 * z / sig
  h_ss[u] <- -2 * z^2
  for (side in c(-1, 1)) {
    i <- cens == side
    if (!any(i)) next
    # a = (lo - eta)/sigma on the left, (eta - hi)/sigma on the right
    a <- if (side < 0) (lo - eta[i]) / sig else (eta[i] - hi) / sig
    lam <- exp(stats::dnorm(a, log = TRUE) - stats::pnorm(a, log.p = TRUE))
    ll[i] <- stats::pnorm(a, log.p = TRUE)
    g_e[i] <- side * lam / sig
    g_s[i] <- -lam * a
    h_ee[i] <- -lam * (a + lam) / sig^2
    h_es[i] <- if (side < 0) (lam / sig) * (1 - a * (a + lam)) else (lam / sig) * (a * (a + lam) - 1)
    h_ss[i] <- lam * a - lam * a^2 * (a + lam)
  }
  list(ll = ll, g_e = g_e, g_s = g_s, h_ee = h_ee, h_es = h_es, h_ss = h_ss)
}

st_tobit_fit <- function(D, set, level, ll_lim, ul_lim, noconstant) {
  y <- D$y
  w <- D$w
  lo <- if (isTRUE(ll_lim)) min(y) else if (is.null(ll_lim)) -Inf else ll_lim
  hi <- if (isTRUE(ul_lim)) max(y) else if (is.null(ul_lim)) Inf else ul_lim
  if (lo >= hi) rlang::abort("The lower limit must be smaller than the upper limit.")
  cens <- ifelse(y <= lo, -1L, ifelse(y >= hi, 1L, 0L))
  if (!any(cens == 0)) rlang::abort("There are no uncensored observations.")
  ok <- st_estimable(D, w)
  Xk <- D$X[, ok, drop = FALSE]
  k <- sum(ok)
  cons_k <- st_cons_index(D, ok)
  # the model is maximized in ln(var(e)) = 2 ln(sigma), as Stata 15 does
  fn_of <- function(X) {
    kx <- ncol(X)
    function(theta) {
      b <- theta[seq_len(kx)]
      s <- theta[kx + 1L] / 2
      eta <- drop(X %*% b)
      tm <- st_tobit_terms(y, eta, s, cens, lo, hi)
      g <- c(as.vector(crossprod(X, w * tm$g_e)), sum(w * tm$g_s) / 2)
      H <- matrix(0, kx + 1L, kx + 1L)
      H[seq_len(kx), seq_len(kx)] <- crossprod(X, X * (w * tm$h_ee))
      H[seq_len(kx), kx + 1L] <- H[kx + 1L, seq_len(kx)] <- as.vector(crossprod(X, w * tm$h_es)) / 2
      H[kx + 1L, kx + 1L] <- sum(w * tm$h_ss) / 4
      list(ll = sum(w * tm$ll), g = g, H = H, S = cbind(X * (w * tm$g_e), w * tm$g_s / 2))
    }
  }
  # starting values: least squares on all observations
  ols <- function(X) {
    q <- qr(X * sqrt(w), tol = 1e-9)
    b <- qr.coef(q, y * sqrt(w))
    b[is.na(b)] <- 0
    e <- y - drop(X %*% b)
    c(b, log(sum(w * e^2) / sum(w)))
  }
  ll0 <- NA_real_
  if (!noconstant) {
    one <- matrix(1, D$n, 1L)
    ll0 <- st_ml_max(fn_of(one), ols(one))$ll
  }
  fn <- fn_of(Xk)
  opt <- st_ml_max(fn, ols(Xk))
  # Stata 15 maximizes the tobit model more tightly than ml's defaults:
  # further Newton steps up to the precision of the computer
  for (i in 1:5) {
    cur <- fn(opt$theta)
    step <- tryCatch(solve(-cur$H, cur$g), error = function(e) NULL)
    if (is.null(step) || !is.finite(fn(opt$theta + step)$ll) || fn(opt$theta + step)$ll < cur$ll - 1e-10) break
    opt$theta <- opt$theta + step
    opt$ll <- fn(opt$theta)$ll
  }
  fin <- fn(opt$theta)
  N <- st_ml_N(set, D)
  vc <- st_ml_vce(fin$H, fin$S, D, set, N, k + 1L)
  V <- vc$V
  noncons <- setdiff(seq_len(k), cons_k)
  df_m <- length(noncons)
  chi2 <- if (set$vce == "ols") 2 * (opt$ll - ll0) else
    if (anyNA(V)) NA_real_ else st_wald(opt$theta[noncons], V[noncons, noncons, drop = FALSE])
  wr <- st_wald_rank(V, k + 1L, df_m, set$vce)
  if (wr$deficient) chi2 <- NA_real_
  # with robust standard errors an F test, as Stata reports it
  Fs <- if (set$vce != "ols") chi2 / df_m else NA_real_
  # numbers of observations: sums of frequency weights, else counts
  cnt <- function(i) if (set$weighted && set$weight_type %in% c("fweight", "iweight")) sum(w[i]) else sum(i)
  # t and F tests with N minus the tested coefficients as degrees of freedom,
  # as Stata's tobit
  df_t <- N - df_m
  crit <- stats::qt(1 - (1 - level / 100) / 2, df_t)
  b <- opt$theta[seq_len(k)]
  se <- sqrt(pmax(diag(V)[seq_len(k)], 0))
  tval <- b / se
  lv <- opt$theta[k + 1L]
  se_lv <- sqrt(max(V[k + 1L, k + 1L], 0))
  v2 <- exp(lv)
  s <- lv / 2
  # the variance with its confidence limits from ln(var)
  coefs <- list(estimate = c(b, v2), se = c(se, v2 * se_lv), stat = c(tval, NA_real_),
                p = c(2 * stats::pt(-abs(tval), df_t), NA_real_),
                low = c(b - crit * se, exp(lv - crit * se_lv)),
                high = c(b + crit * se, exp(lv + crit * se_lv)))
  stats <- list(N = N, df_m = df_m, df_test = wr$df %||% df_m, df_r = df_t, chi2 = chi2, F = Fs,
                p = if (df_m == 0) NA_real_ else if (set$vce != "ols") stats::pf(Fs, df_m, df_t, lower.tail = FALSE) else
                  stats::pchisq(chi2, df_m, lower.tail = FALSE),
                r2_p = 1 - opt$ll / ll0, ll = opt$ll, ll_0 = ll0, sigma = exp(s),
                N_unc = cnt(cens == 0), N_lc = cnt(cens < 0), N_rc = cnt(cens > 0),
                llopt = lo, ulopt = hi,
                n_clust = vc$G, iterations = length(opt$log) - 1L, converged = opt$converged)
  ncol_full <- length(D$cols)
  rows <- c(st_design_rows(D), list(
    list(variable = "/", level = NA_character_, type = "block", col = NA_integer_, block = "/", title = "/",
         term = NA_integer_),
    list(variable = "var(e)", level = NA_character_, type = "coef", col = ncol_full + 1L, block = "/",
         label = sprintf("var(e.%s)", set$y_name), wide = TRUE, term = NA_integer_)))
  list(D = D, ok = c(ok, TRUE), ok_cols = ok, b = opt$theta, V = V, coefs = coefs,
       cut_cols = ncol_full + 1L, stats = stats, log = opt$log, flags = opt$flags, df_t = df_t,
       rows = rows, k = k, notes = c(st_empty_notes(D), st_omit_notes(D, ok)))
}

# Headers ----------------------------------------------------------------------

st_nbreg_header <- function(m, spec) {
  head <- st_ml_header("Negative binomial regression", m$stats, spec)
  # "Dispersion" is aligned with "Log likelihood" or "Log pseudolikelihood"
  lab <- st_pad("Dispersion", if (spec$vce == "ols") 14L else 20L, "left")
  head[3] <- paste0(st_pad(paste(lab, "= mean"), 48L, "left"), substring(head[3], 49L))
  head
}

st_tobit_header <- function(m, spec) {
  s <- m$stats
  n_txt <- function(v) if (v != round(v)) st_stata_g(v, 10L) else formatC(v, format = "d", big.mark = ",")
  lim <- function(v) if (is.infinite(v)) if (v > 0) "+inf" else "-inf" else st_stata_g(v, 10L)
  ml <- st_ml_header("Tobit regression", s, spec)
  if (spec$vce != "ols") {
    # robust: an F test with the residual degrees of freedom
    sp0 <- strrep(" ", 48L)
    ml[2] <- paste0(sp0, st_hdr_r(sprintf("F(%4d,%7d)", as.integer(s$df_test %||% s$df_m), as.integer(s$df_r)),
                                  st_fmt_2f(s$F)))
    ml[3] <- paste0(sp0, st_hdr_r("Prob > F", st_fmt_4f(s$p)))
  }
  sp <- strrep(" ", 48L)
  c(ml[1],
    paste0(sp, st_hdr_r("   Uncensored", n_txt(s$N_unc))),
    paste0(st_pad(paste("Limits: lower =", lim(s$llopt)), 48L, "left"), st_hdr_r("   Left-censored", n_txt(s$N_lc))),
    paste0(st_pad(paste("        upper =", lim(s$ulopt)), 48L, "left"), st_hdr_r("   Right-censored", n_txt(s$N_rc))),
    "", ml[2:4])
}

# The likelihood-ratio test of alpha = 0 below the table of nbreg.
st_nbreg_lr_line <- function(s) {
  if (is.na(s$chi2_c)) return(character())
  paste0(st_pad(paste0("LR test of alpha=0: chibar2(01) = ", sprintf("%-8.2f", s$chi2_c)), 55L, "left"),
         "Prob >= chibar2 = ", sprintf("%5.3f", s$p_c))
}
