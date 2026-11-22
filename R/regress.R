#' Linear regression (Stata: `regress`)
#'
#' Re-implementation of Stata's
#' `regress depvar [indepvars] [weight] [, vce(robust|cluster) level(#) beta noconstant]`
#' with Stata's output. The analysis-of-variance table with the model
#' statistics and the coefficient table, in the layout of Stata 15.
#'
#' * Regressors are selected with tidyselect, e.g. `s.regress(df, y, x1, x2)`.
#' * Whether a regressor enters as a factor or as a numeric variable follows
#'   from its type. Factors and character variables are entered as indicator
#'   variables with the first category as base, numeric variables as they are.
#'   Numeric variables can be entered as factors with `categorical`. Value
#'   labels are shown. As in Stata, the base category is not displayed, and
#'   `base` chooses another base category.
#' * Interactions are given as strings in `...` or in `interact`. `"x#g"`
#'   adds the interaction only, `"x##g"` the interaction and the main effects,
#'   e.g. `"age##sex"`. A numeric variable times itself (`"age#age"`) gives a
#'   squared term. Below the output a note tells how every variable enters the
#'   interaction (factor or numeric). Unlike Stata, no prefixes `i.` and `c.`
#'   are needed (they are understood, see [statsalike-stata-vs-r]).
#' * Observations with a missing value in any variable are excluded
#'   (listwise deletion). Collinear variables are reported as omitted, cells
#'   of interactions without observations as empty.
#' * Weights. `fweight`, `aweight`, `iweight` and `pweight` as in Stata.
#'   `pweight` implies robust standard errors.
#' * `vce = "robust"` gives heteroskedasticity-robust (HC1) standard errors,
#'   `vce = "hc2"` and `vce = "hc3"` the HC2 and HC3 variants, `vce =
#'   "cluster"` (or `cluster = var`) cluster-robust standard errors with
#'   Stata's small-sample adjustment.
#' * `ame = TRUE` shows the average marginal effects of all regressors in
#'   place of the coefficients (see [s.margins()]), an addition to Stata.
#' * `by x:` becomes `group_by(x)` before the call.
#'
#' Postestimation commands are [s.margins()], [s.predict()], [s.estat()], [s.test()],
#' [s.rvfplot()] and [s.ereturn()].
#'
#' @param .data A data frame or tibble (may be grouped with [dplyr::group_by()]).
#' @param y Dependent variable.
#' @param ... Independent variables (tidyselect).
#' @param categorical Independent variables to enter as categorical
#'   (indicator) variables even though they are numeric (Stata's `i.`).
#' @param interact Interactions, e.g. `"x#g"`, `"x##g"` or
#'   `c("age#age", "sex#east")`. They can also be given as strings in `...`.
#' @param base Base categories of categorical variables, a named vector of
#'   values or value labels, e.g. `c(rep78 = 3)`.
#' @param wt Optional weighting variable.
#' @param weight_type `"fweight"`, `"aweight"`, `"iweight"` or `"pweight"`
#'   (see [statsalike-weights]).
#' @param vce Type of standard errors, `"ols"` (default), `"robust"`,
#'   `"cluster"` or, for linear regression, `"hc2"` or `"hc3"`.
#' @param cluster Cluster variable for `vce = "cluster"`. Giving it implies
#'   cluster-robust standard errors.
#' @param level Confidence level in percent (Stata's `level(#)`).
#' @param beta Report standardized coefficients instead of confidence
#'   intervals.
#' @param ame Report the average marginal effects of all regressors (Stata's
#'   `margins, dydx(*)`) instead of the coefficients. The result can be shown
#'   with [s.marginsplot()] and in [s.etable()]. An addition to Stata.
#' @param noconstant Estimate without constant.
#' @param export Optional file name ending in `.html` or `.docx` (see
#'   [s.export()]).
#'
#' @return A tibble of class `st_regress` with one row per coefficient (with
#'   `ame = TRUE` one row per marginal effect, as [s.margins()]).
#'   `variable` (term, e.g. `"mpg"` or `"foreign#mpg"`), `level` (category
#'   of categorical terms), `estimate`, `std_error`, `statistic`, `p_value`,
#'   `conf_low`, `conf_high`, `beta` (with `beta = TRUE`) and `note`
#'   (`"base"`, `"empty"` or `"omitted"`). Model statistics are returned by
#'   [s.ereturn()]. It prints in Stata's layout.
#'
#' @examples
#' # regress mpg weight foreign
#' s.regress(mtcars, mpg, wt, am)
#'
#' # regress mpg weight i.cyl, vce(robust)
#' s.regress(mtcars, mpg, wt, cyl, categorical = cyl, vce = "robust")
#'
#' # interaction of a numeric variable and a factor (regress mpg c.wt##i.am)
#' d <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
#' s.regress(d, mpg, "wt##am")
#'
#' # HC3 standard errors
#' s.regress(mtcars, mpg, wt, hp, vce = "hc3")
#'
#' # regress mpg weight, beta
#' s.regress(mtcars, mpg, wt, hp, beta = TRUE)
#'
#' s.ereturn(s.regress(mtcars, mpg, wt, am))
#' @export
s.regress <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                      wt = NULL, weight_type = c("fweight", "aweight", "iweight", "pweight"),
                      vce = c("ols", "robust", "cluster", "hc2", "hc3"), cluster = NULL,
                      level = 95, beta = FALSE, ame = FALSE, noconstant = FALSE, export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "regress", rlang::current_env())
  fits <- st_fit_groups(set, function(D) st_ols(D, set, level, beta, noconstant), noconstant)
  st_reg_output(set, fits, "st_regress",
                opts = c(if (beta) "beta", if (noconstant) "noconstant"),
                export = export, ame = ame)
}

#' Logistic regression (Stata: `logit`)
#'
#' Re-implementation of Stata's
#' `logit depvar [indepvars] [weight] [, or vce(robust|cluster) level(#) nolog noconstant]`
#' with Stata's output. Iteration log, model statistics (likelihood-ratio
#' test, pseudo R-squared) and the coefficient table with z tests.
#'
#' The dependent variable must be numeric or logical. As in Stata, 0 is
#' treated as failure and every other nonmissing value as success. The model
#' is estimated by Newton-Raphson, starting from the constant-only model. As
#' in Stata, a variable whose nonzero values predict the outcome perfectly
#' (typically a category of a factor variable) is dropped together with these
#' observations, with a note. Regressors, categorical variables, interactions,
#' missing values, weights (`fweight`, `iweight`, `pweight`), standard errors
#' and groups are handled as in [s.regress()]. Stata does not allow `aweight`
#' with `logit`.
#'
#' @inheritParams s.regress
#' @param y Dependent variable (0 = failure, other values = success).
#' @param weight_type `"fweight"`, `"iweight"` or `"pweight"` (see
#'   [statsalike-weights]).
#' @param or Report odds ratios instead of coefficients.
#' @param nolog Do not show the iteration log.
#'
#' @return A tibble of class `st_logit` with one row per coefficient (as in
#'   [s.regress()]. With `or = TRUE`, `estimate` and the confidence limits are
#'   odds ratios). Model statistics are returned by [s.ereturn()].
#'
#' @examples
#' # logit am mpg wt
#' s.logit(mtcars, am, mpg, wt)
#'
#' # logit am mpg, or nolog
#' s.logit(mtcars, am, mpg, or = TRUE, nolog = TRUE)
#'
#' # average marginal effects in place of the coefficients
#' s.logit(mtcars, am, mpg, wt, nolog = TRUE, ame = TRUE)
#'
#' s.ereturn(s.logit(mtcars, am, mpg, wt))
#' @export
s.logit <- function(.data, y, ..., categorical = NULL, interact = NULL, base = NULL,
                    wt = NULL, weight_type = c("fweight", "iweight", "pweight"),
                    vce = c("ols", "robust", "cluster"), cluster = NULL,
                    level = 95, or = FALSE, ame = FALSE, nolog = FALSE, noconstant = FALSE,
                    export = NULL) {
  set <- st_reg_setup(.data, rlang::enquo(y), rlang::enquos(...), rlang::enquo(categorical),
                      interact, base, rlang::enquo(wt), match.arg(weight_type), match.arg(vce),
                      rlang::enquo(cluster), level, "logit", rlang::current_env())
  fits <- st_fit_groups(set, function(D) st_logit_fit(D, set, level, or, noconstant), noconstant)
  st_reg_output(set, fits, "st_logit",
                opts = c(if (or) "or", if (nolog) "nolog", if (noconstant) "noconstant"),
                export = export, nolog = nolog, or = or, ame = ame)
}

#' Stored estimation results (Stata: `ereturn list`)
#'
#' Returns the model statistics of the estimation commands ([s.regress()],
#' [s.logit()], [s.probit()], [s.mlogit()], [s.ologit()], [s.oprobit()],
#' [s.poisson()], [s.nbreg()], [s.tobit()]) as a tibble, the equivalent of the
#' results Stata stores in `e()`. Among them are the number of observations,
#' degrees of freedom, test statistics, R-squared, root mean squared error, log
#' likelihoods, the number of clusters and, after logit and probit, Tjur's
#' R-squared (`r2_t`, an addition to Stata).
#'
#' @param x An estimation result of statsalike.
#' @return A tibble with one row (per group).
#' @examples
#' s.ereturn(s.regress(mtcars, mpg, wt, hp))
#' @export
s.ereturn <- function(x) {
  if (!inherits(x, st_est_classes)) {
    rlang::abort("`x` must be a result of an estimation command of statsalike, e.g. s.regress() or s.logit().")
  }
  attr(x, "st_meta", exact = TRUE)
}

st_est_classes <- c("st_regress", "st_logit", "st_probit", "st_mlogit", "st_ologit", "st_oprobit",
                    "st_poisson", "st_nbreg", "st_tobit")

# Setup ------------------------------------------------------------------------

st_reg_setup <- function(.data, y_quo, x_expr, cat_quo, interact, base, wt_quo, weight_type, vce,
                         cl_quo, level, kind, env, offset_quo = NULL, exposure_quo = NULL) {
  st_check_data(.data)
  if (!is.numeric(level) || length(level) != 1L || level <= 10 || level >= 99.99) {
    rlang::abort("`level` must be a number between 10 and 99.99, e.g. 95.")
  }
  grp <- st_groups(.data)
  data <- dplyr::ungroup(.data)
  y_name <- st_select1(y_quo, data, "y")
  # regressors in Stata's factor-variable notation may be given as strings
  # in place, e.g. s.regress(df, y, "c.age##i.sex", educ)
  items <- c(st_fv_items(x_expr, data, env, y_name), st_parse_interact(interact, data))
  x_names <- unlist(Filter(is.character, items))
  terms <- st_make_terms(items)
  inter <- Filter(is.list, items)
  vars <- unique(unlist(terms))
  if (y_name %in% vars) rlang::abort("The dependent variable cannot also be a regressor.")
  cat_names <- st_select(cat_quo, data)
  if (length(setdiff(cat_names, vars))) {
    rlang::abort("Variables in `categorical` must also be independent variables.")
  }
  is_cat <- lapply(stats::setNames(vars, vars), function(v) {
    v %in% cat_names || is.factor(data[[v]]) || is.character(st_strip(data[[v]]))
  })
  for (it in inter) {
    for (k in seq_along(it$vars)) {
      v <- it$vars[k]
      if (it$prefix[k] == "i") is_cat[[v]] <- TRUE
      if (it$prefix[k] == "c" && is_cat[[v]]) {
        rlang::abort(sprintf("`%s` is categorical and cannot be used as continuous (c.%s).", v, v))
      }
    }
  }
  if (length(vars) && any(vars %in% names(grp$keys))) {
    rlang::abort("A model variable cannot also be a grouping variable.")
  }
  # dates and times enter as numbers; the output notes their origin
  dates <- unlist(lapply(vars[!unlist(is_cat[vars])], function(v) {
    if (st_is_datetime(data[[v]])) stats::setNames(st_datetime_unit(data[[v]]), v)
  }))
  as_c <- unlist(lapply(inter, function(it) it$vars[it$prefix == "c"]))
  st_labelled_hint(data, setdiff(vars[!unlist(is_cat[vars])], as_c))
  base <- st_check_base(base, vars, is_cat)
  for (it in inter) for (v in names(it$base)) if (is.null(base[[v]])) base[[v]] <- it$base[[v]]
  w <- st_eval_weights(wt_quo, data, if (weight_type == "pweight") "aweight" else weight_type)
  if (!is.null(w) && any(w < 0, na.rm = TRUE)) {
    rlang::abort("Negative weights are not allowed in regressions.")
  }
  weighted <- !is.null(w)
  if (!weighted) w <- rep(1, nrow(data))
  clus <- NULL
  cl_name <- NULL
  if (!rlang::quo_is_null(cl_quo)) {
    cl_name <- st_select1(cl_quo, data, "cluster")
    clus <- data[[cl_name]]
    vce <- "cluster"
  } else if (vce == "cluster") {
    rlang::abort("`vce = \"cluster\"` needs a cluster variable in `cluster`.")
  }
  if (weighted && weight_type == "pweight" && vce == "ols") vce <- "robust"
  offset <- NULL
  off_name <- NULL
  off_type <- NULL
  for (o in list(list(q = offset_quo, type = "offset"), list(q = exposure_quo, type = "exposure"))) {
    if (is.null(o$q) || rlang::quo_is_null(o$q)) next
    if (!is.null(offset)) rlang::abort("`offset` and `exposure` cannot be combined.")
    off_name <- st_select1(o$q, data, o$type)
    off <- st_num(data[[off_name]])
    if (o$type == "exposure") {
      if (any(off <= 0, na.rm = TRUE)) rlang::abort("`exposure` must be positive.")
      off <- log(off)
    }
    offset <- off
    off_type <- o$type
  }
  list(data = data, groups = grp, y_name = y_name, x_names = x_names, terms = terms,
       vars = vars, is_cat = is_cat, base = base, cat_names = names(Filter(isTRUE, is_cat)),
       dates = dates, w = w, weighted = weighted, weight_type = weight_type,
       wt_quo = wt_quo, vce = vce, clus = clus, cl_name = cl_name, level = level,
       kind = kind, offset = offset, off_name = off_name, off_type = off_type)
}

st_check_base <- function(base, vars, is_cat) {
  if (is.null(base)) return(list())
  if (is.null(names(base)) || any(!nzchar(names(base)))) {
    rlang::abort("`base` must be named, e.g. base = c(rep78 = 3).")
  }
  bad <- setdiff(names(base), vars[unlist(is_cat[vars])])
  if (length(bad)) {
    rlang::abort(sprintf("`base` names categorical regressors only; not: %s.", paste(bad, collapse = ", ")))
  }
  as.list(base)
}

st_fit_groups <- function(set, fit, noconstant) {
  lapply(seq_along(set$groups$rows), function(g) {
    idx <- set$groups$rows[[g]]
    if (!length(idx) && !is.null(set$groups$keys)) return(NULL)
    D <- st_design(set, idx, noconstant)
    fit(D)
  })
}

# Notes before the estimation, as Stata gives them.
st_empty_notes <- function(D) {
  i <- which(D$empty & vapply(D$cells, function(cl) length(cl$factors) > 1L, logical(1)))
  vapply(i, function(k) {
    sprintf("note: %s identifies no observations in the sample", st_cell_name(D$cells[[k]], D$info))
  }, character(1))
}

st_omit_notes <- function(D, ok) {
  out <- character()
  for (i in seq_along(D$cells)) {
    j <- D$cell_col[i]
    if (is.na(j) || D$empty[i] || ok[j]) next
    out <- c(out, sprintf("note: %s omitted because of collinearity", st_cell_name(D$cells[[i]], D$info)))
  }
  out
}

# Columns that are not collinear with earlier ones.
st_independent <- function(X, w) {
  q <- qr(X * sqrt(w), tol = 1e-9)
  ok <- rep(FALSE, ncol(X))
  ok[q$pivot[seq_len(q$rank)]] <- TRUE
  ok
}

# (Z'Z)^-1 from the QR decomposition of Z. Unlike solve(crossprod(Z)), this
# does not square the condition number, so it also works for regressors on
# very different scales (as lm() and glm() do).
st_qr_inv <- function(q) {
  inv <- chol2inv(qr.R(q))
  o <- order(q$pivot)
  inv[o, o, drop = FALSE]
}

# With few clusters the (cluster-)robust variance has a lower rank than the
# number of coefficients; Stata then reports no model test and as its degrees
# of freedom the rank minus the untested coefficients (e.g. F(0, 1) = .).
st_wald_rank <- function(V, p, q, vce) {
  if (vce == "ols" || anyNA(V)) return(list(deficient = FALSE, df = NULL))
  e <- eigen((V + t(V)) / 2, symmetric = TRUE, only.values = TRUE)$values
  rk <- sum(e > max(e) * 1e-12)
  if (rk >= p) return(list(deficient = FALSE, df = NULL))
  list(deficient = TRUE, df = max(0L, rk - (p - q)))
}

# Wald statistic b' V^-1 b; NA if V is singular, e.g. with
# fewer clusters than coefficients.
st_wald <- function(b, V) {
  if (!length(b)) return(NA_real_)
  q <- qr(V, tol = 1e-10)
  if (q$rank < length(b)) return(NA_real_)
  drop(crossprod(b, qr.coef(q, b)))
}

# Sandwich variance (robust or cluster-robust) with Stata's adjustments;
# `S` holds the scores (one row per observation, already weighted).
st_sandwich <- function(bread, S, w, weight_type, clus, N, k, kind) {
  if (is.null(clus)) {
    # with frequency weights an observation stands for w identical ones
    meat <- if (weight_type == "fweight") crossprod(S / sqrt(w)) else crossprod(S)
    adj <- if (kind == "regress") N / (N - k) else N / (N - 1)
    list(V = adj * bread %*% meat %*% bread, G = NA_integer_)
  } else {
    U <- rowsum(S, clus)
    G <- nrow(U)
    adj <- if (kind == "regress") (G / (G - 1)) * ((N - 1) / (N - k)) else G / (G - 1)
    list(V = adj * bread %*% crossprod(U) %*% bread, G = G)
  }
}

# Linear regression ------------------------------------------------------------

st_ols <- function(D, set, level, beta, noconstant) {
  wt <- set$weight_type
  y <- D$y
  w <- D$w
  n <- D$n
  if (set$weighted && wt %in% c("aweight", "pweight")) w <- w * n / sum(w)
  ok <- st_estimable(D, w, "pivot")
  Xk <- D$X[, ok, drop = FALSE]
  k <- sum(ok)
  q <- qr(Xk * sqrt(w), tol = 1e-9)
  bk <- qr.coef(q, y * sqrt(w))
  bread <- st_qr_inv(q)
  e <- drop(y - Xk %*% bk)
  N <- if (set$weighted && wt %in% c("fweight", "iweight")) sum(w) else n
  df_r <- N - k
  if (df_r <= 0) rlang::abort("Not enough observations for the number of regressors.")
  rss <- sum(w * e^2)
  ybar <- sum(w * y) / sum(w)
  has_cons <- !noconstant && ok[D$cons %||% 0L]
  tss <- if (has_cons) sum(w * (y - ybar)^2) else sum(w * y^2)
  mss <- tss - rss
  df_m <- k - has_cons
  s2 <- rss / df_r
  if (set$vce == "ols") {
    V <- s2 * bread
    G <- NA_integer_
  } else if (set$vce %in% c("hc2", "hc3")) {
    # HC2 and HC3: squared residuals divided by (1 - h) or (1 - h)^2, without
    # a further small-sample adjustment
    # (with weights, the leverage is x (X'WX)^-1 x' without the weight)
    h <- rowSums((Xk %*% bread) * Xk)
    S <- Xk * (w * e / (1 - h)^(if (set$vce == "hc2") 0.5 else 1))
    meat <- if (wt == "fweight" && set$weighted) crossprod(S / sqrt(w)) else crossprod(S)
    V <- bread %*% meat %*% bread
    G <- NA_integer_
  } else {
    sw <- st_sandwich(bread, Xk * (w * e), w, wt, D$clus, N, k, "regress")
    V <- sw$V
    G <- sw$G
  }
  se <- sqrt(diag(V))
  df_t <- if (is.na(G)) df_r else G - 1
  stat <- bk / se
  crit <- stats::qt(1 - (1 - level / 100) / 2, df_t)
  noncons <- setdiff(seq_len(k), if (has_cons) match(D$cons, which(ok)))
  if (set$vce == "ols") {
    Fs <- if (df_m > 0) (mss / df_m) / s2 else NA_real_
    F_df2 <- df_r
  } else {
    Fs <- st_wald(bk[noncons], V[noncons, noncons, drop = FALSE]) / length(noncons)
    F_df2 <- df_t
  }
  wr <- st_wald_rank(V, k, length(noncons), set$vce)
  if (wr$deficient) Fs <- NA_real_
  coefs <- list(estimate = bk, se = se, stat = stat,
                p = 2 * stats::pt(-abs(stat), df_t),
                low = bk - crit * se, high = bk + crit * se)
  if (beta) {
    wsd <- function(v) sqrt(sum(w * (v - sum(w * v) / sum(w))^2) / (sum(w) - 1))
    b_std <- bk * apply(Xk, 2, wsd) / wsd(y)
    if (has_cons) b_std[match(D$cons, which(ok))] <- NA_real_
    coefs$beta <- b_std
  }
  r2 <- if (tss > 0) mss / tss else NA_real_
  stats <- list(
    N = N, df_m = df_m, df_r = df_r, F = Fs, F_df2 = F_df2, df_test = wr$df %||% df_m,
    p = stats::pf(Fs, df_m, F_df2, lower.tail = FALSE),
    r2 = r2, r2_a = 1 - (1 - r2) * (N - has_cons) / df_r, rmse = sqrt(s2),
    mss = mss, rss = rss, n_clust = G,
    # Stata's e(ll) of the normal linear model (not defined here for
    # importance and sampling weights)
    ll = if (!set$weighted || wt %in% c("fweight", "aweight")) {
      -N / 2 * (1 + log(2 * pi) + log(rss / N))
    } else NA_real_,
    ll_0 = if (has_cons && (!set$weighted || wt %in% c("fweight", "aweight"))) {
      -N / 2 * (1 + log(2 * pi) + log(tss / N))
    } else NA_real_
  )
  list(D = D, ok = ok, b = bk, V = V, coefs = coefs, stats = stats, df_t = df_t,
       notes = c(st_empty_notes(D), st_omit_notes(D, ok)),
       sum_w = if (set$weighted && wt %in% c("aweight", "pweight")) st_seq_sum(D$w))
}

# Sum in double precision in the order of the data, as Stata adds up (R's
# sum() uses extended precision, which can differ in the last digits).
st_seq_sum <- function(x) {
  s <- 0
  for (v in x) s <- s + v
  s
}

# Logistic regression ----------------------------------------------------------

# Stata drops a variable whose nonzero values all have the same outcome,
# together with these observations (e.g. a category without successes).
st_perfect_predictors <- function(D, y) {
  notes <- character()
  repeat {
    hit <- FALSE
    for (i in seq_along(D$cells)) {
      if (D$empty[i]) next
      v <- st_cell_value(D$cells[[i]], D$vals, D$n)
      nz <- v != 0
      if (!any(nz) || all(nz)) next
      yy <- y[nz]
      if (all(yy == 0) || all(yy != 0)) {
        name <- sub("b\\.", ".", st_cell_name(D$cells[[i]], D$info))
        notes <- c(notes,
                   sprintf("note: %s != 0 predicts %s perfectly", name, if (all(yy == 0)) "failure" else "success"),
                   sprintf("      %s dropped and %s obs not used", name, format(sum(nz), big.mark = ",")),
                   "")
        D <- st_design_subset(D, !nz)
        y <- y[!nz]
        hit <- TRUE
        break
      }
    }
    if (!hit) break
  }
  list(D = D, notes = notes)
}

st_logit_fit <- function(D, set, level, or, noconstant, link = "logit") {
  wt <- set$weight_type
  pp <- st_perfect_predictors(D, as.numeric(D$y != 0))
  D <- pp$D
  y <- as.numeric(D$y != 0)
  w <- D$w
  n <- D$n
  if (n == 0L || length(unique(y)) < 2L) rlang::abort("The outcome does not vary.")
  ok <- st_estimable(D, w)
  X <- D$X
  Xk <- X[, ok, drop = FALSE]
  k <- sum(ok)
  has_cons <- !noconstant && ok[D$cons %||% 0L]
  cons_k <- if (has_cons) match(D$cons, which(ok))
  probit <- link == "probit"
  # log likelihood of the binary model on the scale of the linear predictor;
  # probit with the sign q = 2y - 1, so that the tail is computed precisely
  q <- 2 * y - 1
  ll_of <- if (probit) {
    function(eta) sum(w * stats::pnorm(q * eta, log.p = TRUE))
  } else {
    function(eta) sum(w * (y * stats::plogis(eta, log.p = TRUE) + (1 - y) * stats::plogis(-eta, log.p = TRUE)))
  }
  cdf <- if (probit) stats::pnorm else stats::plogis
  p0 <- sum(w * y) / sum(w)
  ll0 <- if (has_cons) sum(w * (y * log(p0) + (1 - y) * log(1 - p0))) else sum(w) * log(0.5)

  # Newton-Raphson as in Stata's ml, starting from the constant-only model
  b <- rep(0, k)
  if (has_cons) b[cons_k] <- if (probit) stats::qnorm(p0) else stats::qlogis(p0)
  # scores of the observations (d ll_i / d eta) and the weights of the
  # negative Hessian; for probit the observed Hessian (Stata's d2 evaluator)
  score_of <- function(eta) {
    if (!probit) {
      p <- stats::plogis(eta)
      return(list(u = y - p, v = p * (1 - p)))
    }
    z <- q * eta
    mills <- exp(stats::dnorm(z, log = TRUE) - stats::pnorm(z, log.p = TRUE))
    list(u = q * mills, v = mills * (mills + z))
  }
  fn <- function(theta) {
    eta <- drop(Xk %*% theta)
    sc <- score_of(eta)
    list(ll = ll_of(eta), g = as.vector(crossprod(Xk, w * sc$u)),
         H = -crossprod(Xk, Xk * (w * sc$v)))
  }
  opt <- st_ml_max(fn, b)
  b <- opt$theta
  ll <- opt$ll
  eta <- drop(Xk %*% b)
  p <- cdf(eta)
  # as in Stata (r(2000)): no estimates if the outcome is predicted perfectly
  if (all(p < 1e-8 | p > 1 - 1e-8) || -ll / sum(w) < 1e-6) {
    rlang::abort(paste("The regressors predict the outcome perfectly (complete separation);",
                       "the model cannot be estimated."))
  }
  converged <- opt$converged
  log_ll <- opt$log
  sc <- score_of(eta)
  # inverse of the negative Hessian from the QR decomposition (precise also
  # for regressors on very different scales)
  qf <- qr(Xk * sqrt(w * pmax(sc$v, 1e-12)), tol = 1e-9)
  bread <- if (qf$rank == k) st_qr_inv(qf) else matrix(NA_real_, k, k)
  N <- if (set$weighted && wt %in% c("fweight", "iweight")) sum(w) else n
  if (set$vce == "ols") {
    V <- bread
    G <- NA_integer_
  } else {
    sw <- st_sandwich(bread, Xk * (w * sc$u), w, wt, D$clus, N, k, "logit")
    V <- sw$V
    G <- sw$G
  }
  noncons <- setdiff(seq_len(k), cons_k)
  df_m <- length(noncons)
  chi2 <- if (set$vce == "ols") 2 * (ll - ll0) else
    if (anyNA(V)) NA_real_ else st_wald(b[noncons], V[noncons, noncons, drop = FALSE])
  wr <- st_wald_rank(V, k, df_m, set$vce)
  if (wr$deficient) chi2 <- NA_real_
  stats <- list(
    N = N, df_m = df_m, df_test = wr$df %||% df_m, chi2 = chi2,
    p = if (df_m > 0) stats::pchisq(chi2, df_m, lower.tail = FALSE) else NA_real_,
    r2_p = 1 - ll / ll0, r2_t = st_tjur(p, y, w), ll = ll, ll_0 = ll0, n_clust = G,
    iterations = length(log_ll) - 1L, converged = converged
  )
  # completely determined outcomes (Stata's footnote); for probit Stata
  # counts only probabilities closer to 0 or 1
  eps <- if (probit) 1e-9 else 1e-8
  cd <- c(sum(p < eps & y == 0), sum(1 - p < eps & y == 1))
  list(D = D, ok = ok, b = b, V = V, coefs = st_ml_coefs(b, V, level, if (or) "or"), stats = stats,
       log = log_ll, flags = opt$flags, df_t = Inf, link = link,
       notes = c(st_empty_notes(D), pp$notes, st_omit_notes(D, ok)), cd = cd)
}

# Tjur's R-squared (coefficient of discrimination): the mean predicted
# probability of the observations with the outcome minus that of the
# observations without it (weighted). An addition to Stata.
st_tjur <- function(p, y, w) {
  sum(w * p * y) / sum(w * y) - sum(w * p * (1 - y)) / sum(w * (1 - y))
}

# Coefficients with z tests; `eform` reports exp(b) with delta-method
# standard errors and exponentiated confidence limits.
st_ml_coefs <- function(b, V, level, eform = NULL, cut = integer()) {
  se <- sqrt(pmax(diag(V), 0))
  se[is.na(diag(V))] <- NA_real_
  z <- b / se
  crit <- stats::qnorm(1 - (1 - level / 100) / 2)
  out <- list(estimate = b, se = se, stat = z, p = 2 * stats::pnorm(-abs(z)),
              low = b - crit * se, high = b + crit * se)
  if (length(cut)) {
    out$stat[cut] <- NA_real_
    out$p[cut] <- NA_real_
  }
  if (!is.null(eform)) {
    e <- setdiff(seq_along(b), cut)
    out$se[e] <- exp(b[e]) * se[e]
    out$estimate[e] <- exp(b[e])
    out$low[e] <- exp(out$low[e])
    out$high[e] <- exp(out$high[e])
  }
  out
}

# Result object ----------------------------------------------------------------

# Rows of the result tibble and of the coefficient table for one fit; `block`
# names the equation of multi-equation models.
st_fit_rows <- function(f, eform = FALSE) {
  rows <- f$rows %||% st_design_rows(f$D)
  pos <- cumsum(f$ok)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (r$type == "coef" && !is.na(r$col) && !f$ok[r$col]) rows[[i]]$type <- "omitted"
  }
  rows
}

st_tidy_rows <- function(f, rows, eform) {
  pos <- cumsum(f$ok)
  tidy_rows <- Filter(function(r) !r$type %in% c("header", "block"), rows)
  val <- function(r, name) {
    if (r$type != "coef") return(NA_real_)
    f$coefs[[name]][pos[r$col]]
  }
  df <- tibble::tibble(
    equation = vapply(tidy_rows, function(r) r$block %||% NA_character_, character(1)),
    variable = vapply(tidy_rows, `[[`, character(1), "variable"),
    level = vapply(tidy_rows, `[[`, character(1), "level"),
    estimate = vapply(tidy_rows, function(r) {
      if (r$type != "coef") return(if (eform) 1 else 0)
      val(r, "estimate")
    }, numeric(1)),
    std_error = vapply(tidy_rows, val, numeric(1), name = "se"),
    statistic = vapply(tidy_rows, val, numeric(1), name = "stat"),
    p_value = vapply(tidy_rows, val, numeric(1), name = "p"),
    conf_low = vapply(tidy_rows, val, numeric(1), name = "low"),
    conf_high = vapply(tidy_rows, val, numeric(1), name = "high")
  )
  if (!is.null(f$coefs$beta)) df$beta <- vapply(tidy_rows, val, numeric(1), name = "beta")
  df$note <- vapply(tidy_rows, function(r) if (r$type == "coef") "" else r$type, character(1))
  if (all(is.na(df$equation))) df$equation <- NULL
  df
}

st_reg_output <- function(set, fits, class, opts, export, nolog = FALSE, or = FALSE,
                          extra_spec = list(), ame = FALSE) {
  if (!isTRUE(ame) && !isFALSE(ame)) rlang::abort("`ame` must be TRUE or FALSE.")
  if (ame && (or || isTRUE(extra_spec$eform))) {
    rlang::abort("`ame = TRUE` cannot be combined with odds ratios or other exponentiated coefficients.")
  }
  keep <- !vapply(fits, is.null, logical(1))
  pieces <- list()
  meta <- list()
  models <- list()
  eform <- or || isTRUE(extra_spec$eform)
  for (g in which(keep)) {
    f <- fits[[g]]
    f$D$rows <- st_fit_rows(f, eform)
    df <- st_tidy_rows(f, f$D$rows, eform)
    st <- tibble::as_tibble(f$stats)
    if (!is.null(set$groups$keys)) {
      df <- dplyr::bind_cols(set$groups$keys[rep(g, nrow(df)), , drop = FALSE], df)
      st <- dplyr::bind_cols(set$groups$keys[g, , drop = FALSE], st)
    }
    pieces[[length(pieces) + 1L]] <- df
    meta[[length(meta) + 1L]] <- st
    models[[length(models) + 1L]] <- c(f, list(label = set$groups$labels[g]))
  }
  cmd_name <- sub("^st_", "", class)
  spec <- c(list(
    y = set$y_name, vce = set$vce, cl_name = set$cl_name, level = set$level,
    weighted = set$weighted, weight_type = set$weight_type, nolog = nolog, or = or,
    beta = "beta" %in% opts, ame = ame, kind = set$kind, terms = set$terms, is_cat = set$is_cat,
    dates = set$dates, vars = set$vars, off_name = set$off_name, off_type = set$off_type, n_data = nrow(set$data),
    labels = vapply(c(set$y_name, set$vars), function(v) st_var_label(set$data[[v]]),
                    character(1)),
    cmd = st_make_cmd(cmd_name, c(set$y_name, st_terms_cmd(set$terms, set$is_cat)),
                      weight = st_weight_label(set$wt_quo, set$weight_type),
                      opts = c(if (set$vce %in% c("robust", "hc2", "hc3")) sprintf("vce(%s)", set$vce),
                               if (set$vce == "cluster") sprintf("vce(cluster %s)", set$cl_name),
                               if (!is.null(set$off_type)) sprintf("%s(%s)", set$off_type, set$off_name),
                               if (set$level != 95) sprintf("level(%s)", set$level), opts),
                      by = names(set$groups$keys))
  ), extra_spec)
  res <- st_new_output(dplyr::bind_rows(pieces), class, st_model = models, st_spec = spec,
                       st_meta = dplyr::bind_rows(meta))
  if (ame) {
    # average marginal effects of all regressors (margins, dydx(*)) in place
    # of the coefficients; an addition to Stata
    if (!length(set$vars)) rlang::abort("`ame = TRUE` needs at least one regressor.")
    pieces <- list()
    for (g in seq_along(models)) {
      one <- res
      attr(one, "st_model") <- models[g]
      models[[g]]$ame <- s.margins(one, dydx = "*", level = set$level)
      d <- st_plain(models[[g]]$ame)
      if (!is.null(set$groups$keys)) {
        d <- dplyr::bind_cols(set$groups$keys[rep(which(keep)[g], nrow(d)), , drop = FALSE], d)
      }
      pieces[[g]] <- d
    }
    res <- st_new_output(dplyr::bind_rows(pieces), class, st_model = models, st_spec = spec,
                         st_meta = dplyr::bind_rows(meta))
  }
  st_export_if(res, export)
}

# Parts of the coefficient table of one model, or with ame = TRUE the table
# of the average marginal effects (the output of s.margins()).
st_parts_coefs <- function(m, spec, tab) {
  if (isTRUE(spec$ame)) return(st_parts_margins(m$ame))
  list(tab)
}

# Output -----------------------------------------------------------------------

# Stata's %w.0g format: as many significant digits as fit (at most w - 2),
# without leading zero. As in Stata, one of the w characters is reserved for
# the sign, so positive and negative numbers get the same number of digits
# (e.g. -.0645707 and .0680008).
st_stata_g <- function(x, w = 9L) {
  # at most w - 2 significant digits; integers with more digits switch to
  # exponential format (e.g. 1.4354e+09 in %11.0g)
  fits <- function(s) {
    nchar(sub("^-", "", s)) <= w - 1L &&
      (grepl("e", s, fixed = TRUE) || nchar(sub("^0+", "", gsub("[^0-9]", "", s))) <= w - 2L)
  }
  vapply(x, function(v) {
    if (is.na(v)) return("")
    if (!is.finite(v)) return(if (v > 0) "Inf" else "-Inf")
    if (v == 0) return("0")
    fixed <- NULL
    fixed_d <- 0L
    for (d in seq(min(w - 2L, 15L), 1L)) {
      s <- formatC(v, digits = d, format = "fg", flag = "#")
      if (grepl(".", s, fixed = TRUE)) s <- sub("\\.$", "", sub("0+$", "", s))
      s <- sub("^(-?)0\\.", "\\1.", s)
      if (fits(s)) {
        fixed <- s
        fixed_d <- nchar(sub("^0+", "", gsub("[^0-9]", "", s)))
        break
      }
    }
    # the exponential format is used where it shows more significant digits
    # (e.g. 1.25e-07 instead of .0000001)
    for (d in seq(max(w - 7L, 0L), 0L)) {
      s <- formatC(v, digits = d, format = "e")
      if (fits(s)) {
        if (is.null(fixed) || (d + 1L > fixed_d && signif(v, fixed_d) != signif(v, d + 1L))) return(s)
        break
      }
    }
    fixed %||% s
  }, character(1), USE.NAMES = FALSE)
}

# %w.0gc: %w.0g with thousands separators in the integer part.
st_stata_gc <- function(x, w = 12L) {
  s <- st_stata_g(x, w)
  ok <- !grepl("e", s, fixed = TRUE)
  s[ok] <- vapply(s[ok], function(v) {
    parts <- strsplit(v, ".", fixed = TRUE)[[1]]
    int <- formatC(as.numeric(parts[1]), format = "d", big.mark = ",")
    if (grepl("^-0?$", parts[1])) int <- parts[1]
    paste(c(int, parts[-1]), collapse = ".")
  }, character(1))
  s
}

# Test statistics with two decimals (Stata's %9.2f) and the root MSE with five
# significant digits; extreme values switch to the compact %9.0g format.
st_fmt_2f <- function(x) {
  ifelse(is.na(x), ".", ifelse(abs(x) >= 1e6, st_stata_g(x, 9L), sprintf("%.2f", x)))
}

st_fmt_rmse <- function(x) {
  if (is.na(x)) return(".")
  if (x != 0 && (abs(x) < 1e-4 || abs(x) >= 1e6)) st_stata_g(x, 9L) else
    trimws(formatC(x, digits = 5, format = "fg"))
}

st_fmt_4f <- function(v) if (is.na(v)) "." else sprintf("%.4f", v)

#' @export
print.st_regress <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

#' @export
print.st_logit <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

# Displayed terms of one fit for st_ct_build(): one equation (or the rows of
# one `block` of a multi-equation model).
st_ct_terms <- function(m, spec, rows, cells_of, W) {
  terms <- list()
  i <- 1L
  while (i <= length(rows)) {
    r <- rows[[i]]
    if (r$type == "header") {
      t <- spec$terms[[r$term]]
      label <- st_ct_term_label(t, spec$is_cat, W)
      sub <- list()
      codes <- character()
      i <- i + 1L
      while (i <= length(rows) && identical(rows[[i]]$term, r$term) && rows[[i]]$type != "header") {
        q <- rows[[i]]
        sub[[length(sub) + 1L]] <- list(label = q$level, status = q$type, cells = cells_of(q))
        cl <- m$D$cells[[q$cell]]
        codes <- c(codes, if (length(t) == 1L) m$D$info[[t]]$codes[cl$levels[1]] else NA_character_)
        i <- i + 1L
      }
      # a factor variable without value labels and with a single category
      # shown is one row "1.am", as in Stata
      shown <- vapply(sub, function(s) s$status != "base", logical(1))
      if (length(t) == 1L && sum(shown) == 1L && identical(sub[[which(shown)]]$label, codes[which(shown)])) {
        s1 <- sub[[which(shown)]]
        lab1 <- paste0(codes[which(shown)], ".", st_abbrev_name(t, W))
        terms[[length(terms) + 1L]] <- list(kind = "plain", label = lab1, inter = FALSE,
                                            rows = list(list(label = lab1, status = s1$status, cells = s1$cells)))
        next
      }
      terms[[length(terms) + 1L]] <- list(kind = "factor", label = label, rows = sub,
                                          inter = length(t) > 1L)
      next
    }
    t <- if (!is.na(r$term %||% NA)) spec$terms[[r$term]] else NULL
    label <- if (is.null(t)) r$label %||% r$variable else st_ct_term_label(t, spec$is_cat, W)
    kind <- if (!is.null(t) && length(t) > 1L) "cint" else "plain"
    terms[[length(terms) + 1L]] <- list(kind = kind, label = label, inter = kind == "cint",
                                        wide = isTRUE(r$wide),
                                        rows = list(list(label = label, status = r$type,
                                                         cells = cells_of(r))))
    i <- i + 1L
  }
  terms
}

# Width of the label column: variable names, the dependent variable and
# factor-variable terms count, interactions of continuous variables do not
# (they are broken into several lines instead), as in Stata.
st_ct_width_of <- function(spec, extra = character(), rows = list()) {
  vars <- c(spec$y, unlist(lapply(spec$terms, function(t) if (length(t) == 1L) t)), extra)
  inter <- unlist(lapply(spec$terms, function(t) {
    if (length(t) > 1L && any(unlist(spec$is_cat[t]))) st_term_label(t, spec$is_cat)
  }))
  levels <- unlist(lapply(rows, function(r) if (!r$type %in% c("header", "block", "base")) r$level))
  st_ct_width(vars, inter, levels)
}

st_cells_fun <- function(m, spec, cut = integer()) {
  pos <- cumsum(m$ok)
  function(r) {
    if (r$type != "coef") return(NULL)
    j <- pos[r$col]
    cf <- m$coefs
    st_ct_cells(cf$estimate[j], cf$se[j], cf$stat[j], cf$p[j], cf$low[j], cf$high[j],
                beta = if (spec$beta) cf$beta[j], cut = r$col %in% cut)
  }
}

st_cluster_note <- function(s, spec, W) {
  st_pad(sprintf("(Std. Err. adjusted for %s clusters in %s)",
                 formatC(s$n_clust, format = "d", big.mark = ","), spec$cl_name), W + 66L)
}

st_parts_reg <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  logit <- inherits(x, c("st_logit", "st_probit"))
  parts <- list()
  for (m in attr(x, "st_model", exact = TRUE)) {
    if (!is.na(m$label)) parts <- c(parts, list(st_by_heading(m$label)))
    W <- st_ct_width_of(spec, rows = m$D$rows)
    pre <- character()
    if (!is.null(m$sum_w)) pre <- sprintf("(sum of wgt is %s)", st_stata_gc(m$sum_w, 17L))
    pre <- c(pre, m$notes)
    if (logit && !spec$nolog) {
      pre <- c(pre, st_iter_log(m$log, spec$vce, m$flags))
    }
    while (length(pre) && !nzchar(pre[length(pre)])) pre <- pre[-length(pre)]
    if (length(pre)) parts <- c(parts, list(st_part_text(pre, mono = TRUE)))
    head <- if (logit) st_ml_header(if (identical(m$link, "probit")) "Probit regression" else "Logistic regression",
                                    m$stats, spec) else st_regress_header(m, spec)
    if (spec$vce == "cluster") head <- c(head, "", st_cluster_note(m$stats, spec, W))
    parts <- c(parts, list(st_part_text(head, mono = TRUE)))
    terms <- st_ct_terms(m, spec, m$D$rows, st_cells_fun(m, spec), W)
    est_lab <- if (logit && spec$or) "Odds Ratio" else "Coef."
    tab <- st_ct_build(list(list(name = NULL, terms = terms)), W, spec$y, est_lab,
                       stat = if (logit) "z" else "t", level = spec$level, beta = spec$beta,
                       top = st_vce_top(spec$vce),
                       one = if (logit && spec$or) "1" else "0")
    tab$attach <- spec$vce == "cluster"
    parts <- c(parts, st_parts_coefs(m, spec, tab))
    if (logit && spec$or && !is.null(m$D$cons) && m$ok[m$D$cons]) {
      parts <- c(parts, list(st_part_text("Note: _cons estimates baseline odds.", mono = TRUE, attach = TRUE)))
    }
    if (logit) parts <- c(parts, st_cd_note(m$cd))
    parts <- c(parts, st_inter_part(spec))
  }
  parts
}

# Line above the column headings of the coefficient table.
st_vce_top <- function(vce) {
  switch(vce, ols = NULL, hc2 = "             Robust HC2", hc3 = "             Robust HC3", "               Robust")
}

# Notes on how the variables enter the interactions (see st_inter_notes()).
st_inter_part <- function(spec) {
  notes <- st_inter_notes(spec$terms, spec$is_cat, spec$dates)
  if (length(notes)) list(st_part_text(notes, mono = TRUE, attach = TRUE)) else list()
}

# Heading of a group as Stata's by prefix shows it: a line, then "-> x = 1".
st_by_heading <- function(label) {
  p <- st_part_heading(label)
  p$rule <- strrep("-", getOption("width", 80L))
  p
}

# Stata's footnote on completely determined outcomes.
st_cd_note <- function(cd) {
  if (is.null(cd) || sum(cd) == 0) return(list())
  nf <- cd[1]
  ns <- cd[2]
  txt <- sprintf("Note: %d failure%s and %d success%s completely determined.",
                 nf, if (nf == 1) "" else "s", ns, if (ns == 1) "" else "es")
  list(st_part_text(txt, mono = TRUE, attach = TRUE))
}

# Iteration log as in Stata 15: "Iteration 0:   log likelihood = -45.03321".
st_iter_log <- function(ll, vce, flags = NULL) {
  it <- sprintf("Iteration %d:", seq_along(ll) - 1L)
  out <- sprintf("%s log %s = %s", st_pad(it, 14L, "left"),
                 if (vce == "ols") "likelihood" else "pseudolikelihood",
                 st_pad(st_stata_g(ll, 10L), 10L))
  if (!is.null(flags)) out <- paste0(out, ifelse(nzchar(flags), paste0("  ", flags), ""))
  out
}

# Model statistics to the right of the header: label in 18 columns, "=" and
# the value in 11 columns (78 characters with the 48 columns on the left).
st_hdr_r <- function(label, value, sign = "=") paste0(st_pad(label, 18L, "left"), sign, st_pad(value, 11L))

st_hdr_F <- function(fstat) {
  if (!is.na(fstat) && fstat > 99999) list(sign = ">", value = "99999.00") else list(sign = "=", value = st_fmt_2f(fstat))
}

st_regress_header <- function(m, spec) {
  s <- m$stats
  n_txt <- formatC(s$N, format = "d", big.mark = ",")
  if (s$N != round(s$N)) n_txt <- st_stata_g(s$N, 10L)
  # as in Stata, a test that cannot be computed has 0 degrees of freedom
  df_F <- s$df_test %||% s$df_m
  F_lab <- sprintf("F(%s, %s)", st_stata_g(df_F, 9L), st_stata_g(s$F_df2, 9L))
  Fv <- st_hdr_F(s$F)
  rmse <- st_fmt_rmse(s$rmse)
  if (spec$vce == "ols") {
    right <- function(label, value, sign = "=") paste0("   ", st_pad(label, 16L, "left"), sign, st_pad(value, 10L))
    row <- function(src, ss, df, ms) {
      paste0(st_pad(src, 12L), " |", st_pad(ss, 12L), st_pad(df, 10L), st_pad(ms, 12L))
    }
    rule <- "-------------+----------------------------------"
    tot_df <- s$df_m + s$df_r
    c(paste0("      Source |       SS           df       MS   ", right("Number of obs", n_txt)),
      paste0(rule, right(F_lab, Fv$value, Fv$sign)),
      paste0(row("Model", st_stata_g(s$mss, 11L), st_stata_gc(s$df_m, 9L),
                 st_stata_g(s$mss / s$df_m, 11L)), right("Prob > F", st_fmt_4f(s$p))),
      paste0(row("Residual", st_stata_g(s$rss, 11L), st_stata_gc(s$df_r, 9L),
                 st_stata_g(s$rss / s$df_r, 11L)), right("R-squared", st_fmt_4f(s$r2))),
      paste0(rule, right("Adj R-squared", st_fmt_4f(s$r2_a))),
      paste0(row("Total", st_stata_g(s$mss + s$rss, 11L), st_stata_gc(tot_df, 9L),
                 st_stata_g((s$mss + s$rss) / tot_df, 11L)), right("Root MSE", rmse)))
  } else {
    sp <- strrep(" ", 48L)
    c(paste0(st_pad("Linear regression", 48L, "left"), st_hdr_r("Number of obs", n_txt)),
      paste0(sp, st_hdr_r(F_lab, Fv$value, Fv$sign)),
      paste0(sp, st_hdr_r("Prob > F", st_fmt_4f(s$p))),
      paste0(sp, st_hdr_r("R-squared", st_fmt_4f(s$r2))),
      paste0(sp, st_hdr_r("Root MSE", rmse)))
  }
}

# Header of maximum-likelihood models (logit, mlogit, ologit, poisson).
st_ml_header <- function(title, s, spec) {
  n_txt <- formatC(s$N, format = "d", big.mark = ",")
  if (s$N != round(s$N)) n_txt <- st_stata_g(s$N, 10L)
  df_chi <- as.integer(s$df_test %||% s$df_m)
  chi_lab <- sprintf("%s chi2(%d)", if (spec$vce == "ols") "LR" else "Wald", df_chi)
  ll_lab <- if (spec$vce == "ols") "Log likelihood" else "Log pseudolikelihood"
  sp <- strrep(" ", 48L)
  c(paste0(st_pad(title, 48L, "left"), st_hdr_r("Number of obs", n_txt)),
    paste0(sp, st_hdr_r(chi_lab, st_fmt_2f(s$chi2))),
    paste0(sp, st_hdr_r("Prob > chi2", st_fmt_4f(s$p))),
    paste0(st_pad(paste(ll_lab, "=", st_pad(st_stata_g(s$ll, 10L), 10L)), 48L, "left"),
           st_hdr_r("Pseudo R2", st_fmt_4f(s$r2_p))))
}

st_logit_header <- function(m, spec) st_ml_header("Logistic regression", m$stats, spec)
