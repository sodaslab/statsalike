#' Multinomial logistic regression (Stata: `mlogit`)
#'
#' Re-implementation of Stata's
#' `mlogit depvar [indepvars] [weight] [, baseoutcome(#) rrr vce(robust|cluster) level(#) nolog]`
#' with Stata's output. Iteration log, model statistics and one equation per
#' outcome, in the layout of Stata 15.
#'
#' * As in Stata, the most frequent outcome is the base outcome unless
#'   `baseoutcome` gives another one (value or value label).
#' * The model is estimated by Newton-Raphson, starting from the
#'   constant-only model (iteration 0).
#' * Regressors, categorical variables, interactions, missing values, weights
#'   (`fweight`, `iweight`, `pweight`), standard errors and groups are
#'   handled as in [s.regress()].
#'
#' @inheritParams s.logit
#' @param y Dependent variable with the outcomes (numeric, possibly with value
#'   labels, or a factor).
#' @param baseoutcome Base outcome (value or value label), by default the most
#'   frequent outcome.
#' @param rrr Report relative-risk ratios (`exp(b)`) instead of coefficients.
#'
#' @return A tibble of class `st_mlogit` with one row per equation and
#'   coefficient (`equation` is the outcome). Model statistics are returned by
#'   [s.ereturn()]. [s.margins()] gives marginal effects on the probabilities.
#' @examples
#' d <- transform(mtcars, gear = factor(gear))
#' s.mlogit(d, gear, mpg)
#' s.mlogit(d, gear, mpg, baseoutcome = 3, rrr = TRUE, nolog = TRUE)
#' @export
s.mlogit <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                     wt = NULL, weight_type = c("fweight", "iweight", "pweight"),
                     vce = c("ols", "robust", "cluster"), cluster = NULL,
                     baseoutcome = NULL, level = 95, rrr = FALSE, ame = FALSE, nolog = FALSE,
                     noconstant = FALSE, export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "mlogit", rlang::current_env())
  fits <- st_fit_groups(set, function(D) st_mlogit_fit(D, set, level, rrr, noconstant, baseoutcome),
                        noconstant)
  st_reg_output(set, fits, "st_mlogit",
                opts = c(if (!is.null(baseoutcome)) sprintf("baseoutcome(%s)", baseoutcome),
                         if (rrr) "rrr", if (nolog) "nolog", if (noconstant) "noconstant"),
                export = export, nolog = nolog, extra_spec = list(eform = rrr, rrr = rrr), ame = ame)
}

#' Ordered logistic regression (Stata: `ologit`)
#'
#' Re-implementation of Stata's
#' `ologit depvar [indepvars] [weight] [, or vce(robust|cluster) level(#) nolog]`
#' with Stata's output. Iteration log, model statistics, the coefficients and
#' the cut points (`/cut1`, `/cut2`, ...), in the layout of Stata 15.
#'
#' The outcomes are the ordered values of `y` (numeric, possibly with value
#' labels, or a factor in the order of its levels). As in Stata, the model
#' has no constant. The cut points take its place. It is estimated by
#' Newton-Raphson, starting from the constant-only model (iteration 0).
#' Regressors, categorical variables, interactions, missing values, weights
#' (`fweight`, `iweight`, `pweight`), standard errors and groups are handled
#' as in [s.regress()].
#'
#' @inheritParams s.logit
#' @param y Ordered dependent variable.
#' @param or Report odds ratios for the coefficients (the cut points stay on
#'   the log-odds scale).
#' @return A tibble of class `st_ologit` with one row per coefficient and cut
#'   point (`equation` is `"/"` for the cut points). Model statistics are
#'   returned by [s.ereturn()]. [s.margins()] gives marginal effects on the
#'   probabilities of the outcomes.
#' @examples
#' s.ologit(mtcars, gear, mpg, wt)
#' s.ologit(mtcars, carb, mpg, or = TRUE, nolog = TRUE)
#' @export
s.ologit <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                     wt = NULL, weight_type = c("fweight", "iweight", "pweight"),
                     vce = c("ols", "robust", "cluster"), cluster = NULL,
                     level = 95, or = FALSE, ame = FALSE, nolog = FALSE, export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "ologit", rlang::current_env())
  fits <- st_fit_groups(set, function(D) st_ologit_fit(D, set, level, or), TRUE)
  st_reg_output(set, fits, "st_ologit", opts = c(if (or) "or", if (nolog) "nolog"),
                export = export, nolog = nolog, or = or, ame = ame)
}

#' Poisson regression (Stata: `poisson`)
#'
#' Re-implementation of Stata's
#' `poisson depvar [indepvars] [weight] [, exposure(varname) offset(varname) irr vce(robust|cluster) level(#) nolog]`
#' with Stata's output. Iteration log, model statistics and the coefficient
#' table, in the layout of Stata 15.
#'
#' The dependent variable is a count (nonnegative). Starting values follow
#' Stata's approach (a linear regression rescaled to the Poisson model and
#' refined by one iteratively reweighted least-squares step). Regressors,
#' categorical variables, interactions, missing values, weights (`fweight`,
#' `iweight`, `pweight`), standard errors and groups are handled as in
#' [s.regress()]. Overdispersed counts are better analyzed with robust
#' standard errors (`vce = "robust"`) or a negative binomial model.
#'
#' @inheritParams s.logit
#' @param y Count variable.
#' @param exposure Exposure variable. Its logarithm enters with coefficient 1.
#' @param offset Offset variable (enters the linear predictor with
#'   coefficient 1).
#' @param irr Report incidence-rate ratios (`exp(b)`).
#' @return A tibble of class `st_poisson` with one row per coefficient. Model
#'   statistics are returned by [s.ereturn()]. [s.estat()] gives the
#'   goodness-of-fit test (`"gof"`).
#' @examples
#' s.poisson(warpbreaks, breaks, wool, tension)
#' s.poisson(warpbreaks, breaks, wool, irr = TRUE, nolog = TRUE)
#' @export
s.poisson <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                      exposure = NULL, offset = NULL,
                      wt = NULL, weight_type = c("fweight", "iweight", "pweight"),
                      vce = c("ols", "robust", "cluster"), cluster = NULL,
                      level = 95, irr = FALSE, ame = FALSE, nolog = FALSE, noconstant = FALSE,
                      export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "poisson", rlang::current_env(),
                      offset_quo = rlang::enquo(offset), exposure_quo = rlang::enquo(exposure))
  fits <- st_fit_groups(set, function(D) st_poisson_fit(D, set, level, irr, noconstant), noconstant)
  st_reg_output(set, fits, "st_poisson", opts = c(if (irr) "irr", if (nolog) "nolog",
                                                  if (noconstant) "noconstant"),
                export = export, nolog = nolog, extra_spec = list(eform = irr, irr = irr), ame = ame)
}

# Maximization -----------------------------------------------------------------

# Maximization as in Stata's ml (technique nr). Every iteration takes the
# Newton-Raphson step d = (-H)^-1 g. If it lowers the log likelihood, the
# step is halved (at most 60 times; "(backed up)" after 6 or more
# halvings). If it improves, ml keeps stepping forward by 1/8 of the step,
# doubling the increment, as long as the log likelihood rises. Where -H is not
# positive definite ("(not concave)"), its diagonal is inflated until it is,
# and the step is scaled to a small expected improvement before stepping
# forward. Convergence: the relative change of the coefficients below 1e-6
# (tolerance) or of the log likelihood below 1e-7 (ltolerance), and the
# scaled gradient g'(-H)^-1 g below 1e-5 (nrtolerance). `fn(theta)` returns
# list(ll, g, H).
st_ml_max <- function(fn, theta, maxit = 300L) {
  cur <- fn(theta)
  if (!is.finite(cur$ll)) rlang::abort("Initial values not feasible.")
  log_ll <- numeric()
  flags <- character()
  converged <- FALSE
  reldif <- function(a, b) abs(a - b) / (abs(b) + 1)
  kback <- 0L
  ok_ll <- function(f) is.finite(f$ll)
  for (it in seq_len(maxit)) {
    f0 <- cur$ll
    b0 <- theta
    inv <- st_ml_inverse(cur$H)
    log_ll <- c(log_ll, f0)
    flags <- c(flags, if (inv$notcc) "(not concave)" else if (kback >= 6L) "(backed up)" else "")
    d <- drop(inv$V %*% cur$g)
    kback <- 0L
    if (inv$notcc) {
      # step scaled to an expected change of eps * |f|
      eps <- if (it <= 2L) 1e-2 else {
        n <- length(log_ll)
        min(1e-2, max(1e-8, 0.1 * (log_ll[n] - log_ll[n - 1L]) / abs(log_ll[n - 1L])))
      }
      fctr <- eps * abs(f0) / sum(cur$g * d)
      if (!is.finite(fctr)) fctr <- 2
      d <- fctr * d
      start <- 1
    } else {
      start <- 0.125
    }
    new <- fn(theta + d)
    if (!ok_ll(new) || new$ll < f0) {
      th <- theta + d
      while (kback < 60L && (!ok_ll(new) || new$ll < f0)) {
        th <- (th + b0) / 2
        new <- fn(th)
        kback <- kback + 1L
      }
      if (!ok_ll(new) || new$ll < f0) {
        th <- b0
        new <- cur
      }
    } else {
      th <- theta + d
      inc <- start * d
      repeat {
        nxt <- fn(th + inc)
        if (!ok_ll(nxt) || nxt$ll <= new$ll) break
        th <- th + inc
        new <- nxt
        inc <- 2 * inc
      }
    }
    if (!ok_ll(new)) rlang::abort("Discontinuous region encountered; cannot compute an improvement.")
    theta <- th
    cur <- new
    conv <- max(reldif(theta, b0)) <= 1e-6 || reldif(cur$ll, f0) <= 1e-7
    if (conv) {
      inv1 <- st_ml_inverse(cur$H)
      nr <- sum(cur$g * drop(inv1$V %*% cur$g))
      conv <- is.finite(nr) && nr < 1e-5
    }
    if (conv) {
      converged <- TRUE
      break
    }
  }
  inv <- st_ml_inverse(cur$H)
  log_ll <- c(log_ll, cur$ll)
  flags <- c(flags, if (inv$notcc) "(not concave)" else if (kback >= 6L) "(backed up)" else "")
  if (!converged) warning("Convergence not achieved.", call. = FALSE)
  list(theta = theta, ll = cur$ll, g = cur$g, H = cur$H, log = log_ll, flags = flags,
       converged = converged)
}

# Inverse of the negative Hessian. If it is not positive definite, its
# diagonal is increased (by 1.1, 2.2, 4.4, ... times its absolute value) until
# it is, and the inverse is rescaled to the trace of the generalized inverse.
st_ml_inverse <- function(H) {
  A <- -(H + t(H)) / 2
  ch <- tryCatch(chol(A), error = function(e) NULL)
  if (!is.null(ch)) return(list(V = chol2inv(ch), notcc = FALSE))
  orig <- sum(diag(st_syminv(A)))
  amt <- 1.1
  repeat {
    diag(A) <- diag(A) + amt * abs(diag(A))
    amt <- 2 * amt
    ch <- tryCatch(chol(A), error = function(e) NULL)
    if (!is.null(ch) || amt > 1e10) break
  }
  V <- if (!is.null(ch)) chol2inv(ch) else st_syminv(A)
  adj <- orig / sum(diag(V))
  if (is.finite(adj) && adj > 0) V <- adj * V
  list(V = V, notcc = TRUE)
}

# Generalized inverse of a symmetric matrix by sweeping the rows in order;
# rows whose pivot is not positive are set to zero (as Stata's syminv()).
st_syminv <- function(A) {
  p <- nrow(A)
  M <- A
  swept <- logical(p)
  tol <- 1e-13 * max(abs(diag(A)), 1e-300)
  for (k in seq_len(p)) {
    piv <- M[k, k]
    if (!is.finite(piv) || piv <= tol) next
    rk <- M[k, ]
    ck <- M[, k]
    M <- M - outer(ck, rk) / piv
    M[k, ] <- rk / piv
    M[, k] <- ck / piv
    M[k, k] <- -1 / piv
    swept[k] <- TRUE
  }
  V <- -M
  V[!swept, ] <- 0
  V[, !swept] <- 0
  V
}

# Variance of ML estimates: inverse of the negative Hessian, or the sandwich
# with the scores `S` (one row per observation, weighted).
st_ml_vce <- function(H, S, D, set, N, k) {
  A <- -(H + t(H)) / 2
  bread <- tryCatch(solve(A), error = function(e) matrix(NA_real_, nrow(A), ncol(A)))
  if (set$vce == "ols") return(list(V = bread, G = NA_integer_))
  if (anyNA(bread)) return(list(V = bread, G = NA_integer_))
  st_sandwich(bread, S, D$w, set$weight_type, D$clus, N, k, "ml")
}

st_ml_N <- function(set, D) {
  if (set$weighted && set$weight_type %in% c("fweight", "iweight")) sum(D$w) else D$n
}

# Outcomes of a categorical dependent variable: labels, codes (values as
# Stata shows them in predict(outcome(#))) and the index of every observation.
st_outcomes <- function(y_raw) {
  f <- droplevels(st_as_levels(y_raw))
  raw <- st_strip(y_raw)
  codes <- if (is.numeric(raw) && !is.factor(y_raw)) {
    st_format_values(sort(unique(raw[!is.na(raw)])))
  } else levels(f)
  list(labels = levels(f), codes = codes, y = as.integer(f))
}

st_cons_index <- function(D, ok) if (!is.null(D$cons) && ok[D$cons]) match(D$cons, which(ok))

# Multinomial logit ------------------------------------------------------------

st_mlogit_fit <- function(D, set, level, rrr, noconstant, baseoutcome) {
  out <- st_outcomes(set$data[[set$y_name]][D$idx])
  # the outcomes name the equations; Stata turns their labels into valid
  # names ("not.work" becomes "not_work")
  out$value_labels <- out$labels
  out$labels <- substr(gsub("[^A-Za-z0-9_]", "_", out$labels), 1L, 32L)
  J <- length(out$labels)
  if (J < 2L) rlang::abort("The outcome does not vary.")
  yi <- out$y
  w <- D$w
  freq <- vapply(seq_len(J), function(j) sum(w[yi == j]), numeric(1))
  bj <- if (is.null(baseoutcome)) which.max(freq) else {
    b <- match(as.character(baseoutcome), out$codes)
    if (is.na(b)) b <- match(as.character(baseoutcome), out$labels)
    if (is.na(b)) rlang::abort(sprintf("The base outcome %s does not occur.", baseoutcome))
    b
  }
  ok <- st_estimable(D, w)
  Xk <- D$X[, ok, drop = FALSE]
  k <- sum(ok)
  eqs <- setdiff(seq_len(J), bj)
  Y <- outer(yi, eqs, `==`) * 1
  cons_k <- st_cons_index(D, ok)
  fn <- function(theta) {
    B <- matrix(theta, k, J - 1L)
    E <- Xk %*% B
    m <- pmax(apply(E, 1, max), 0)
    den <- exp(-m) + rowSums(exp(E - m))
    P <- exp(E - m) / den
    lp_obs <- ifelse(yi == bj, -m - log(den), rowSums(Y * (E - m)) - log(den))
    R <- (Y - P) * w
    g <- as.vector(crossprod(Xk, R))
    H <- matrix(0, k * (J - 1L), k * (J - 1L))
    for (a in seq_len(J - 1L)) {
      for (b in a:(J - 1L)) {
        wab <- w * P[, a] * ((a == b) - P[, b])
        blk <- -crossprod(Xk, Xk * wab)
        ia <- (a - 1L) * k + seq_len(k)
        ib <- (b - 1L) * k + seq_len(k)
        H[ia, ib] <- blk
        H[ib, ia] <- t(blk)
      }
    }
    list(ll = sum(w * lp_obs), g = g, H = H, P = P, R = R)
  }
  # constant-only model: log of the outcome frequencies relative to the base
  theta0 <- rep(0, k * (J - 1L))
  ll0 <- sum(freq[freq > 0] * log(freq[freq > 0] / sum(freq)))
  if (!is.null(cons_k)) {
    for (a in seq_along(eqs)) theta0[(a - 1L) * k + cons_k] <- log(freq[eqs[a]] / freq[bj])
  } else {
    ll0 <- sum(w) * log(1 / J)
  }
  opt <- st_ml_max(fn, theta0)
  fin <- fn(opt$theta)
  N <- st_ml_N(set, D)
  S <- do.call(cbind, lapply(seq_along(eqs), function(a) Xk * fin$R[, a]))
  vc <- st_ml_vce(fin$H, S, D, set, N, length(opt$theta))
  V <- vc$V
  noncons <- unlist(lapply(seq_along(eqs), function(a) (a - 1L) * k + setdiff(seq_len(k), cons_k)))
  df_m <- length(noncons)
  chi2 <- if (set$vce == "ols") 2 * (opt$ll - ll0) else
    if (anyNA(V)) NA_real_ else st_wald(opt$theta[noncons], V[noncons, noncons, drop = FALSE])
  wr <- st_wald_rank(V, length(opt$theta), df_m, set$vce)
  if (wr$deficient) chi2 <- NA_real_
  stats <- list(N = N, df_m = df_m, df_test = wr$df %||% df_m, chi2 = chi2,
                p = if (df_m > 0) stats::pchisq(chi2, df_m, lower.tail = FALSE) else NA_real_,
                r2_p = 1 - opt$ll / ll0, ll = opt$ll, ll_0 = ll0, k_out = J,
                n_clust = vc$G, iterations = length(opt$log) - 1L, converged = opt$converged)
  # observations whose outcome is predicted with certainty (Stata's footnote)
  pobs <- ifelse(yi == bj, 1 - rowSums(fin$P), rowSums(Y * fin$P))
  n_cd <- sum(pobs > 1 - 1e-7)
  # rows: one block per outcome
  base_rows <- st_design_rows(D)
  rows <- list()
  okall <- logical()
  ncol_full <- length(D$cols)
  for (a in seq_along(eqs)) {
    lab <- out$labels[eqs[a]]
    rows[[length(rows) + 1L]] <- list(variable = lab, level = NA_character_, type = "block",
                                      col = NA_integer_, block = lab, title = lab, term = NA_integer_)
    for (r in base_rows) {
      r$block <- lab
      if (!is.na(r$col)) r$col <- r$col + (a - 1L) * ncol_full
      rows[[length(rows) + 1L]] <- r
    }
    okall <- c(okall, ok)
  }
  list(D = D, ok = okall, ok_cols = ok, b = opt$theta, V = V,
       coefs = st_ml_coefs(opt$theta, V, level, if (rrr) "rrr"),
       stats = stats, log = opt$log, flags = opt$flags, df_t = Inf, rows = rows,
       outcomes = out, base_outcome = bj, eq_index = eqs, k = k,
       notes = c(st_empty_notes(D), st_omit_notes(D, ok)), n_cd = n_cd)
}

# Ordered logit ----------------------------------------------------------------

st_ologit_fit <- function(D, set, level, or, link = "logit") {
  out <- st_outcomes(set$data[[set$y_name]][D$idx])
  J <- length(out$labels)
  if (J < 2L) rlang::abort("The outcome does not vary.")
  yi <- out$y
  w <- D$w
  ok <- st_estimable(D, w)
  Xk <- D$X[, ok, drop = FALSE]
  k <- sum(ok)
  n <- D$n
  nc <- J - 1L
  # upper and lower cut point of every observation (0 = infinite)
  up <- ifelse(yi == J, 0L, yi)
  lo <- ifelse(yi == 1L, 0L, yi - 1L)
  Fd <- st_ord_dist(if (link == "probit") "oprobit" else "ologit")
  cdf <- Fd$F
  f <- Fd$f
  fn <- function(theta) {
    b <- theta[seq_len(k)]
    cut <- theta[k + seq_len(nc)]
    xb <- if (k) drop(Xk %*% b) else rep(0, n)
    a_up <- ifelse(up > 0, cut[pmax(up, 1L)] - xb, Inf)
    a_lo <- ifelse(lo > 0, cut[pmax(lo, 1L)] - xb, -Inf)
    # probability of the observed category, computed on the tail that keeps
    # precision
    P <- ifelse(a_lo > 0, cdf(-a_lo) - cdf(-a_up), cdf(a_up) - cdf(a_lo))
    P <- pmax(P, 1e-300)
    u <- ifelse(is.finite(a_up), f(a_up), 0)
    ul <- ifelse(is.finite(a_lo), f(a_lo), 0)
    du <- ifelse(is.finite(a_up), Fd$df(a_up), 0)
    dl <- ifelse(is.finite(a_lo), Fd$df(a_lo), 0)
    p <- k + nc
    A <- matrix(0, n, p)
    Al <- matrix(0, n, p)
    if (k) {
      A[, seq_len(k)] <- -Xk * (up > 0)
      Al[, seq_len(k)] <- -Xk * (lo > 0)
    }
    A[cbind(which(up > 0), k + up[up > 0])] <- 1
    Al[cbind(which(lo > 0), k + lo[lo > 0])] <- 1
    Nm <- A * u - Al * ul
    g <- colSums(Nm * (w / P))
    H <- crossprod(A, A * (w * du / P)) - crossprod(Al, Al * (w * dl / P)) -
      crossprod(Nm, Nm * (w / P^2))
    list(ll = sum(w * log(P)), g = g, H = H, S = Nm * (w / P), P = P)
  }
  freq <- vapply(seq_len(J), function(j) sum(w[yi == j]), numeric(1))
  cum <- cumsum(freq)[seq_len(nc)] / sum(freq)
  theta0 <- c(rep(0, k), if (link == "probit") stats::qnorm(cum) else stats::qlogis(cum))
  ll0 <- sum(freq[freq > 0] * log(freq[freq > 0] / sum(freq)))
  opt <- st_ml_max(fn, theta0)
  fin <- fn(opt$theta)
  N <- st_ml_N(set, D)
  vc <- st_ml_vce(fin$H, fin$S, D, set, N, length(opt$theta))
  V <- vc$V
  df_m <- k
  chi2 <- if (set$vce == "ols") 2 * (opt$ll - ll0) else
    if (anyNA(V) || !k) NA_real_ else st_wald(opt$theta[seq_len(k)], V[seq_len(k), seq_len(k), drop = FALSE])
  wr <- st_wald_rank(V, length(opt$theta), df_m, set$vce)
  if (wr$deficient) chi2 <- NA_real_
  stats <- list(N = N, df_m = df_m, df_test = wr$df %||% df_m, chi2 = chi2,
                p = if (df_m > 0) stats::pchisq(chi2, df_m, lower.tail = FALSE) else NA_real_,
                r2_p = 1 - opt$ll / ll0, ll = opt$ll, ll_0 = ll0, k_cat = J,
                n_clust = vc$G, iterations = length(opt$log) - 1L, converged = opt$converged)
  ncol_full <- length(D$cols)
  rows <- st_design_rows(D)
  rows[[length(rows) + 1L]] <- list(variable = "/", level = NA_character_, type = "block",
                                    col = NA_integer_, block = "/", title = "/", term = NA_integer_)
  for (j in seq_len(nc)) {
    rows[[length(rows) + 1L]] <- list(variable = paste0("cut", j), level = NA_character_, type = "coef",
                                      col = ncol_full + j, block = "/", label = paste0("/cut", j),
                                      term = NA_integer_)
  }
  cut_idx <- k + seq_len(nc)
  list(D = D, ok = c(ok, rep(TRUE, nc)), ok_cols = ok, b = opt$theta, V = V,
       coefs = st_ml_coefs(opt$theta, V, level, if (or) "or", cut = cut_idx),
       cut_cols = ncol_full + seq_len(nc),
       stats = stats, log = opt$log, flags = opt$flags, df_t = Inf, rows = rows, outcomes = out, k = k,
       notes = c(st_empty_notes(D), st_omit_notes(D, ok)), n_cd = sum(fin$P > 1 - 1e-7))
}

# Poisson ----------------------------------------------------------------------

st_poisson_fit <- function(D, set, level, irr, noconstant, maxit = 300L, quiet = FALSE) {
  y <- D$y
  if (any(y < 0)) rlang::abort(sprintf("`%s` must be greater than or equal to zero.", set$y_name))
  w <- D$w
  off <- D$offset %||% rep(0, D$n)
  ok <- st_estimable(D, w)
  Xk <- D$X[, ok, drop = FALSE]
  k <- sum(ok)
  cons_k <- st_cons_index(D, ok)
  lgy <- lgamma(y + 1)
  ll_of <- function(eta) sum(w * (y * eta - exp(eta) - lgy))
  fn <- function(theta) {
    eta <- drop(Xk %*% theta) + off
    mu <- exp(eta)
    list(ll = ll_of(eta), g = as.vector(crossprod(Xk, w * (y - mu))),
         H = -crossprod(Xk, Xk * (w * mu)), mu = mu)
  }
  mean_y <- sum(w * y) / sum(w)
  if (mean_y <= 0) rlang::abort("The dependent variable is zero for all observations.")
  # constant-only model
  c0 <- log(sum(w * y) / sum(w * exp(off)))
  ll0 <- if (!noconstant) ll_of(rep(c0, D$n) + off) else NA_real_
  theta0 <- st_poisson_start(Xk, y, w, off, mean_y, cons_k, ll_of)
  opt <- if (quiet) suppressWarnings(st_ml_max(fn, theta0, maxit)) else st_ml_max(fn, theta0, maxit)
  fin <- fn(opt$theta)
  N <- st_ml_N(set, D)
  vc <- st_ml_vce(fin$H, Xk * (w * (y - fin$mu)), D, set, N, k)
  V <- vc$V
  noncons <- setdiff(seq_len(k), cons_k)
  df_m <- length(noncons)
  chi2 <- if (set$vce == "ols") 2 * (opt$ll - ll0) else
    if (anyNA(V)) NA_real_ else st_wald(opt$theta[noncons], V[noncons, noncons, drop = FALSE])
  wr <- st_wald_rank(V, k, df_m, set$vce)
  if (wr$deficient) chi2 <- NA_real_
  stats <- list(N = N, df_m = df_m, df_test = wr$df %||% df_m, chi2 = chi2,
                p = if (df_m > 0) stats::pchisq(chi2, df_m, lower.tail = FALSE) else NA_real_,
                r2_p = 1 - opt$ll / ll0, ll = opt$ll, ll_0 = ll0,
                n_clust = vc$G, iterations = length(opt$log) - 1L, converged = opt$converged)
  rows <- st_design_rows(D)
  if (!is.null(set$off_type)) {
    lab <- if (set$off_type == "exposure") sprintf("ln(%s)", set$off_name) else set$off_name
    r <- list(variable = lab, level = NA_character_, type = set$off_type, col = NA_integer_,
              term = NA_integer_, label = lab)
    rows <- append(rows, list(r))
  }
  notes <- c(st_empty_notes(D), st_omit_notes(D, ok))
  if (any(y != round(y))) notes <- c("note: you are responsible for interpretation of non-count dep. variable", notes)
  list(D = D, ok = ok, ok_cols = ok, b = opt$theta, V = V,
       coefs = st_ml_coefs(opt$theta, V, level, if (irr) "irr"),
       stats = stats, log = opt$log, flags = opt$flags, df_t = Inf, rows = rows, notes = notes)
}

# Starting values as in Stata's poisson: the coefficients of a linear
# regression divided by the mean of y, with the constant solved for the
# Poisson likelihood, and one iteratively reweighted least-squares step; the
# best of these by the log likelihood.
st_poisson_start <- function(X, y, w, off, mean_y, cons_k, ll_of) {
  k <- ncol(X)
  wls <- function(z, v) {
    q <- qr(X * sqrt(v), tol = 1e-9)
    b <- qr.coef(q, z * sqrt(v))
    b[is.na(b)] <- 0
    b
  }
  ynew <- y - off * mean_y
  b1 <- wls(ynew, w) / mean_y
  xb1 <- drop(X %*% b1) + off
  cand <- list(b1)
  lls <- ll_of(xb1)
  c_adj <- 0
  if (!is.null(cons_k)) {
    c_adj <- log(sum(w * y) / sum(w * exp(xb1)))
    b1c <- b1
    b1c[cons_k] <- b1c[cons_k] + c_adj
    cand <- c(cand, list(b1c))
    lls <- c(lls, ll_of(xb1 + c_adj))
  }
  eta <- xb1 + c_adj
  z <- y * exp(-eta) - 1 + eta - off
  b2 <- tryCatch(wls(z, w * exp(eta)), error = function(e) NULL)
  if (!is.null(b2)) {
    cand <- c(cand, list(b2))
    lls <- c(lls, ll_of(drop(X %*% b2) + off))
  }
  lls[!is.finite(lls)] <- -Inf
  # order of preference as in Stata: the IRLS step, then b1, then b1 with
  # the adjusted constant
  pref <- c(length(cand), 1L, if (!is.null(cons_k)) 2L)
  best <- pref[which.max(lls[pref])]
  cand[[best]]
}

# Output -----------------------------------------------------------------------

#' @export
print.st_mlogit <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

#' @export
print.st_ologit <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

#' @export
print.st_poisson <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

st_parts_ml <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  kind <- spec$kind
  parts <- list()
  for (m in attr(x, "st_model", exact = TRUE)) {
    if (!is.na(m$label)) parts <- c(parts, list(st_by_heading(m$label)))
    eq_names <- if (kind == "mlogit") m$outcomes$labels
    W <- st_ct_width_of(spec, extra = eq_names, rows = m$D$rows)
    pre <- m$notes
    if (!spec$nolog) {
      if (!is.null(m$logs)) {
        # nbreg: the Poisson, constant-only and full model, each with a title
        for (lg in m$logs) {
          pre <- c(pre, if (!is.null(lg$title)) c(if (length(pre)) "", lg$title, ""),
                   st_iter_log(lg$ll, spec$vce, lg$flags))
        }
      } else {
        pre <- c(pre, st_iter_log(m$log, spec$vce, m$flags))
      }
    }
    while (length(pre) && !nzchar(pre[length(pre)])) pre <- pre[-length(pre)]
    if (length(pre)) parts <- c(parts, list(st_part_text(pre, mono = TRUE)))
    title <- switch(kind, mlogit = "Multinomial logistic regression",
                    ologit = "Ordered logistic regression", oprobit = "Ordered probit regression",
                    poisson = "Poisson regression", nbreg = "Negative binomial regression",
                    tobit = "Tobit regression")
    head <- switch(kind, nbreg = st_nbreg_header(m, spec), tobit = st_tobit_header(m, spec),
                   st_ml_header(title, m$stats, spec))
    if (spec$vce == "cluster") head <- c(head, "", st_cluster_note(m$stats, spec, W))
    parts <- c(parts, list(st_part_text(head, mono = TRUE)))
    cells <- st_cells_fun(m, spec, cut = m$cut_cols %||% integer())
    rows <- m$D$rows
    eqs <- list()
    if (kind == "mlogit") {
      for (j in seq_along(m$outcomes$labels)) {
        lab <- m$outcomes$labels[j]
        if (j == m$base_outcome) {
          eqs[[length(eqs) + 1L]] <- list(name = lab, base = TRUE, terms = list())
          next
        }
        r_j <- Filter(function(r) identical(r$block, lab) && r$type != "block", rows)
        eqs[[length(eqs) + 1L]] <- list(name = lab, terms = st_ct_terms(m, spec, r_j, cells, W))
      }
    } else {
      main <- Filter(function(r) is.null(r$block), rows)
      eqs[[1]] <- list(name = NULL, terms = st_ct_terms(m, spec, main, cells, W))
      # further equations: cut points, ln(alpha) and alpha, the variance of tobit
      for (b in c("/", "alpha")) {
        rb <- Filter(function(r) identical(r$block, b) && r$type != "block", rows)
        if (length(rb)) eqs[[length(eqs) + 1L]] <- list(name = NULL, terms = st_ct_terms(m, spec, rb, cells, W))
      }
    }
    eform <- isTRUE(spec$or) || isTRUE(spec$eform)
    est_lab <- if (!eform) "Coef." else if (kind == "mlogit") "RRR" else
      if (kind %in% c("poisson", "nbreg")) "IRR" else "Odds Ratio"
    tab <- st_ct_build(eqs, W, spec$y, est_lab, stat = if (kind == "tobit") "t" else "z", level = spec$level,
                       top = st_vce_top(spec$vce),
                       one = if (eform) "1" else "0")
    tab$attach <- spec$vce == "cluster"
    parts <- c(parts, st_parts_coefs(m, spec, tab))
    has_cons <- !is.null(m$D$cons) && isTRUE(m$ok_cols[m$D$cons])
    if (kind == "nbreg" && eform) {
      parts <- c(parts, list(st_part_text("Note: Estimates are transformed only in the first equation.",
                                          mono = TRUE, attach = TRUE)))
    }
    if (eform && has_cons) {
      note <- switch(kind, mlogit = "Note: _cons estimates baseline relative risk for each outcome.",
                     poisson = , nbreg = "Note: _cons estimates baseline incidence rate.", NULL)
      if (!is.null(note)) parts <- c(parts, list(st_part_text(note, mono = TRUE, attach = TRUE)))
    }
    if (kind == "nbreg" && length(st_nbreg_lr_line(m$stats))) {
      parts <- c(parts, list(st_part_text(st_nbreg_lr_line(m$stats), mono = TRUE, attach = TRUE)))
    }
    if (kind == "ologit" && eform) {
      parts <- c(parts, list(st_part_text("Note: Estimates are transformed only in the first equation.",
                                          mono = TRUE, attach = TRUE)))
    }
    if (kind %in% c("mlogit", "ologit", "oprobit") && isTRUE(m$n_cd > 0)) {
      parts <- c(parts, list(st_part_text(
        sprintf("Note: %d observation%s completely determined.  Standard errors questionable.",
                m$n_cd, if (m$n_cd == 1) "" else "s"), mono = TRUE, attach = TRUE)))
    }
    parts <- c(parts, st_inter_part(spec))
  }
  parts
}
