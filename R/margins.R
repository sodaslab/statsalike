#' Marginal means, predictive margins and marginal effects (Stata: `margins`)
#'
#' Re-implementation of Stata's
#' `margins [fvarlist] [, dydx(varlist) at(...) atmeans predict(...) pwcompare level(#)]`
#' after the estimation commands of statsalike ([s.regress()], [s.logit()],
#' [s.probit()], [s.mlogit()], [s.ologit()], [s.oprobit()], [s.poisson()],
#' [s.nbreg()], [s.tobit()]), with Stata's output in the layout of Stata 15. Standard
#' errors are computed with the delta method from the variance of the model
#' (including robust and cluster-robust variances).
#'
#' * Without `dydx`, `s.margins()` gives predictive margins. The prediction
#'   averaged over the estimation sample (with the estimation weights).
#'   Factor variables in `...` give one margin per category
#'   (`margins foreign`). Every observation is set to that category.
#' * `dydx` gives average marginal effects (AME) of the named variables, or of
#'   all regressors with `dydx = "*"`. The derivative of the prediction for
#'   continuous variables, the discrete change from the base category for
#'   factor variables. With `atmeans = TRUE` they are marginal effects at the
#'   means (MEM).
#' * `at` fixes variables at values, e.g. `at = list(age = c(20, 40, 60))`.
#'   Several variables give all combinations (the first one varies
#'   slowest), as in Stata's `at(age=(20 40 60) educ=(10 13))`. Factor
#'   variables are given by value or value label.
#' * `predict` chooses the prediction. `"xb"` (linear prediction), `"pr"`
#'   (probability, the default after logit, mlogit and ologit), `"n"`
#'   (number of events, the default after poisson) or `"ir"` (incidence rate). After mlogit
#'   and ologit every outcome gets its own margins unless `outcome` names
#'   one. After tobit the prediction is the linear prediction.
#' * `pwcompare = TRUE` compares the margins pairwise (contrast, standard
#'   error and confidence interval), `pwcompare = "effects"` adds z tests and
#'   p-values, as Stata's `pwcompare` and `pwcompare(effects)`. The p-values
#'   are not adjusted for multiple comparisons. The comparisons are between
#'   the categories of one factor variable in `...` (at most one value per
#'   `at` variable) or between several `at` values without factor variables.
#'   Stata's comparisons across all combinations of both are not available.
#'   After regress and tobit the tests are t tests.
#' * The average marginal effects of all regressors can also be requested
#'   directly in the estimation command with `ame = TRUE`.
#'
#' The result can be drawn with [s.marginsplot()].
#'
#' @param x Result of an estimation command of statsalike (one model, not
#'   grouped).
#' @param ... Factor variables for margins by category (tidyselect).
#' @param dydx Variables for marginal effects (tidyselect, e.g.
#'   `c(age, educ)`), or `"*"` for all regressors.
#' @param at Named list of values at which to evaluate the margins.
#' @param atmeans Set all variables not given in `at` to their means (for
#'   factor variables the shares of the categories), as Stata's `atmeans`.
#' @param predict Prediction. `"xb"`, `"pr"`, `"n"` or `"ir"` (see Details).
#' @param outcome Outcome (value or value label) after mlogit and ologit.
#' @param pwcompare Pairwise comparisons of the margins, `FALSE` (default),
#'   `TRUE` or `"effects"` (see Details).
#' @param level Confidence level in percent.
#' @param export Optional file name ending in `.html` or `.docx`.
#' @return A tibble of class `st_margins` with one row per margin and the columns
#'   `term` (variable of the marginal effect, `NA` for predictive margins),
#'   `level` (category of a factor variable), `outcome`, `at` (number of the
#'   `at` combination), one column per `at` variable and per factor variable
#'   in `...`, `estimate`, `std_error`, `statistic`, `p_value`, `conf_low`,
#'   `conf_high` and `note` (`"not estimable"`). It prints in Stata's layout.
#' @examples
#' m <- s.logit(mtcars, am, wt, hp, nolog = TRUE)
#' # margins, dydx(*)
#' s.margins(m, dydx = "*")
#' # margins, at(wt=(2 3 4))
#' s.margins(m, at = list(wt = c(2, 3, 4)))
#' # margins, dydx(wt) atmeans
#' s.margins(m, dydx = wt, atmeans = TRUE)
#'
#' r <- s.regress(mtcars, mpg, wt, cyl, categorical = cyl)
#' s.margins(r, cyl)
#' # margins cyl, pwcompare(effects)
#' s.margins(r, cyl, pwcompare = "effects")
#' @export
s.margins <- function(x, ..., dydx = NULL, at = NULL, atmeans = FALSE, predict = NULL,
                      outcome = NULL, pwcompare = FALSE, level = 95, export = NULL) {
  if (!inherits(x, st_est_classes)) {
    rlang::abort("`x` must be a result of an estimation command of statsalike, e.g. s.regress() or s.logit().")
  }
  models <- attr(x, "st_model", exact = TRUE)
  if (length(models) != 1L) {
    rlang::abort("s.margins() needs a single model; estimate grouped models one group at a time.")
  }
  if (!is.numeric(level) || length(level) != 1L || level <= 10 || level >= 99.99) {
    rlang::abort("`level` must be a number between 10 and 99.99, e.g. 95.")
  }
  m <- models[[1]]
  spec <- attr(x, "st_spec", exact = TRUE)
  D <- m$D
  info <- D$info
  vars <- names(info)
  proto <- as.data.frame(stats::setNames(rep(list(numeric()), length(vars)), vars))
  # factor variables: every name is a block of its own, "a#b" the cells of
  # an interaction (Stata's margins a b and margins a#b)
  over_terms <- list()
  for (q in rlang::enquos(...)) {
    e <- rlang::quo_get_expr(q)
    if (is.character(e)) {
      for (s in e) over_terms[[length(over_terms) + 1L]] <- sub("^i\\.", "", strsplit(gsub("[[:space:]]", "", s), "#+")[[1]])
    } else {
      for (v in names(tidyselect::eval_select(q, proto))) over_terms[[length(over_terms) + 1L]] <- v
    }
  }
  over <- unique(unlist(over_terms))
  for (v in over) {
    if (!v %in% vars) rlang::abort(sprintf("`%s` is not a variable of the model.", v))
    if (!info[[v]]$cat) rlang::abort(sprintf("`%s` is not a factor variable of the model; use `at` for continuous variables.", v))
  }
  dq <- rlang::enquo(dydx)
  dvars <- if (rlang::quo_is_null(dq)) character() else {
    val <- tryCatch(rlang::eval_tidy(dq), error = function(e) NULL)
    if (is.character(val) && identical(val, "*")) vars else names(tidyselect::eval_select(dq, proto))
  }
  type <- st_mg_type(spec$kind, predict)
  outs <- st_mg_outcomes(m, spec, type, outcome)
  at_list <- st_mg_at(at, info, vars)

  # scenarios: values of the variables for every at combination, factor
  # category and (for factor dydx) category of the marginal effect
  w <- D$w
  n <- D$n
  base_vals <- D$vals
  # Stata evaluates the predictions at the means in single precision. The
  # legend shows the mean of a variable stored as float in single precision,
  # that of a byte, int, long or double variable in double precision. R does
  # not know the storage type: a variable with non-integer values that all fit
  # into a float is taken to be a float variable, integer values to be stored
  # as byte, int or long.
  means <- lapply(base_vals, function(v) {
    if (is.matrix(v)) matrix(st_float(colSums(v * w) / sum(w)), n, ncol(v), byrow = TRUE) else
      rep(st_float(sum(v * w) / sum(w)), n)
  })
  shown_means <- lapply(base_vals, function(v) {
    if (is.matrix(v)) return(st_float(colSums(v * w) / sum(w)))
    m <- sum(v * w) / sum(w)
    if (all(st_float(v) == v) && any(v != round(v))) st_float(m) else m
  })
  set_var <- function(vals, v, value) {
    if (info[[v]]$cat) {
      M <- matrix(0, n, length(info[[v]]$levels))
      M[, value] <- 1
      vals[[v]] <- M
    } else vals[[v]] <- rep(value, n)
    vals
  }
  combo_vals <- lapply(seq_len(nrow(at_list$grid) %||% 1L), function(a) {
    vals <- base_vals
    if (atmeans) for (v in setdiff(vars, at_list$vars)) vals[[v]] <- means[[v]]
    for (v in at_list$vars) vals <- set_var(vals, v, at_list$grid[[v]][a])
    vals
  })
  n_at <- length(combo_vals)
  over_levels <- lapply(stats::setNames(over, over), function(v) {
    which(colSums(base_vals[[v]] * w) > 0)
  })
  # cells of every factor term (the first variable varies slowest)
  over_grids <- lapply(over_terms, function(t) {
    g <- as.matrix(expand.grid(rev(over_levels[t]), KEEP.OUT.ATTRS = FALSE))[, rev(seq_along(t)), drop = FALSE]
    colnames(g) <- t
    g
  })
  if (!length(over_grids)) over_grids <- list(matrix(integer(), 1L, 0L))

  scen <- list()
  add_scen <- function(vals, dx = NULL) {
    scen[[length(scen) + 1L]] <<- list(X = st_X(D, vals, n), dX = if (!is.null(dx)) st_dX(D, vals, dx, n))
    length(scen)
  }
  # margins: list(parts = list(c(scenario, sign)), outcome, deriv)
  defs <- list()
  rowinfo <- list()
  dterms <- st_mg_dterms(dvars, info, vars, D)
  for (dt in if (length(dterms)) dterms else list(NULL)) {
    for (o in seq_along(over_grids)) {
      og <- over_grids[[o]]
      for (j in seq_along(outs$index)) {
        for (a in seq_len(n_at)) {
          for (g in seq_len(nrow(og))) {
            vals <- combo_vals[[a]]
            for (k in seq_len(ncol(og))) vals <- set_var(vals, colnames(og)[k], og[g, k])
            if (is.null(dt)) {
              def <- list(list(s = add_scen(vals), sign = 1, deriv = FALSE))
            } else if (!dt$cat) {
              def <- list(list(s = add_scen(vals, dt$var), sign = 1, deriv = TRUE))
            } else if (dt$is_base) {
              def <- NULL
            } else {
              def <- list(list(s = add_scen(set_var(vals, dt$var, dt$level)), sign = 1, deriv = FALSE),
                          list(s = add_scen(set_var(vals, dt$var, info[[dt$var]]$base)), sign = -1, deriv = FALSE))
            }
            defs[[length(defs) + 1L]] <- list(parts = def, j = outs$index[j])
            rowinfo[[length(rowinfo) + 1L]] <- list(dt = dt, j = j, a = a, o = o, g = g)
          }
        }
      }
    }
  }
  theta <- m$b
  fun <- function(th) {
    pred <- lapply(seq_along(scen), function(s) NULL)
    vapply(defs, function(d) {
      if (is.null(d$parts)) return(NA_real_)
      sum(vapply(d$parts, function(p) {
        sc <- scen[[p$s]]
        v <- st_mg_eval(m, spec, th, sc$X, if (p$deriv) sc$dX, type, d$j)
        p$sign * sum(w * v) / sum(w)
      }, numeric(1)))
    }, numeric(1))
  }
  est <- fun(theta)
  # analytic derivatives of the margins with respect to the parameters
  Jac <- do.call(rbind, lapply(defs, function(d) {
    if (is.null(d$parts)) return(rep(0, length(theta)))
    Reduce(`+`, lapply(d$parts, function(p) {
      sc <- scen[[p$s]]
      p$sign * st_mg_grad(m, spec, theta, sc$X, if (p$deriv) sc$dX, type, d$j, w)
    }))
  }))
  V <- m$V
  se <- sqrt(pmax(rowSums((Jac %*% V) * Jac), 0))
  # estimability: the rows of every scenario must lie in the row space of the
  # estimation design (Stata's "not estimable")
  estimable <- st_mg_estimable(m, spec, scen, defs)
  est[!estimable] <- NA_real_
  se[!estimable] <- NA_real_
  df <- if (spec$kind %in% c("regress", "tobit")) m$df_t else Inf
  stat <- est / se
  p <- 2 * if (is.finite(df)) stats::pt(-abs(stat), df) else stats::pnorm(-abs(stat))
  crit <- if (is.finite(df)) stats::qt(1 - (1 - level / 100) / 2, df) else stats::qnorm(1 - (1 - level / 100) / 2)

  tidy <- tibble::tibble(
    term = vapply(rowinfo, function(r) r$dt$var %||% NA_character_, character(1)),
    level = vapply(rowinfo, function(r) {
      if (is.null(r$dt) || !r$dt$cat) NA_character_ else info[[r$dt$var]]$levels[r$dt$level]
    }, character(1)),
    outcome = vapply(rowinfo, function(r) outs$labels[r$j] %||% NA_character_, character(1)),
    at = vapply(rowinfo, function(r) r$a, integer(1))
  )
  for (v in at_list$vars) {
    vv <- at_list$grid[[v]][tidy$at]
    tidy[[v]] <- if (info[[v]]$cat) info[[v]]$levels[vv] else vv
  }
  for (v in over) {
    tidy[[v]] <- factor(vapply(rowinfo, function(r) {
      og <- over_grids[[r$o]]
      if (v %in% colnames(og)) info[[v]]$levels[og[r$g, v]] else NA_character_
    }, character(1)), levels = info[[v]]$levels)
  }
  tidy$estimate <- est
  tidy$std_error <- se
  tidy$statistic <- stat
  tidy$p_value <- p
  tidy$conf_low <- est - crit * se
  tidy$conf_high <- est + crit * se
  tidy$note <- ifelse(estimable, "", "not estimable")
  base_rows <- vapply(rowinfo, function(r) !is.null(r$dt) && isTRUE(r$dt$is_base), logical(1))
  tidy$note[base_rows] <- "base"
  if (!length(at_list$vars)) tidy$at <- NULL

  # pairwise comparisons of the margins (Stata's pwcompare)
  pw <- NULL
  if (!isFALSE(pwcompare)) {
    mode <- if (isTRUE(pwcompare)) "ci" else if (identical(pwcompare, "effects")) "effects" else
      rlang::abort("`pwcompare` must be TRUE, FALSE or \"effects\".")
    if (length(dterms) || isTRUE(outs$multi)) {
      rlang::abort("`pwcompare` compares predictive margins of one outcome; it cannot be combined with `dydx`.")
    }
    if (length(over_terms) == 1L && length(over_terms[[1]]) == 1L && n_at == 1L) {
      v <- over_terms[[1]]
      g <- vapply(rowinfo, `[[`, integer(1), "g")
      labs <- info[[v]]$levels[over_grids[[1]][g, 1L]]
      head_lab <- v
    } else if (!length(over_terms) && n_at > 1L) {
      g <- vapply(rowinfo, `[[`, integer(1), "a")
      labs <- as.character(g)
      head_lab <- "_at"
    } else {
      rlang::abort("`pwcompare` needs one factor variable in `...` or several `at` values (not both).")
    }
    K <- length(est)
    rows <- list()
    for (i in seq_len(K - 1L)) for (j in (i + 1L):K) {
      gr <- Jac[j, ] - Jac[i, ]
      e <- est[j] - est[i]
      s <- sqrt(max(sum((gr %*% V) * gr), 0))
      rows[[length(rows) + 1L]] <- list(label = paste(labs[j], "vs", labs[i]), estimate = e, se = s)
    }
    pe <- vapply(rows, `[[`, numeric(1), "estimate")
    ps <- vapply(rows, `[[`, numeric(1), "se")
    pz <- pe / ps
    pw <- list(mode = mode, head = head_lab, labels = vapply(rows, `[[`, character(1), "label"))
    tidy <- tibble::tibble(contrast = pw$labels, estimate = pe, std_error = ps, statistic = pz,
                           p_value = 2 * if (is.finite(df)) stats::pt(-abs(pz), df) else stats::pnorm(-abs(pz)),
                           conf_low = pe - crit * ps, conf_high = pe + crit * ps)
  }
  averaged <- setdiff(vars, c(at_list$vars, over, if (atmeans) vars))
  spec_m <- list(
    kind = spec$kind, y = spec$y, type = type, predict = predict, outs = outs, dterms = dterms,
    dvars = dvars, at = at_list, atmeans = atmeans, means = means, shown_means = shown_means, over = over,
    over_terms = over_terms, over_grids = over_grids, info = info, vars = vars, level = level, rowinfo = rowinfo,
    n_at = n_at, N = m$stats$N, vce = spec$vce, averaged = length(averaged) > 0L, pw = pw,
    stat = if (spec$kind %in% c("regress", "tobit")) "t" else "z", model_spec = spec,
    cmd = st_mg_cmd(vapply(over_terms, paste, "", collapse = "#"), dvars, at, atmeans, predict, outcome)
  )
  res <- st_new_output(tidy, "st_margins", st_spec = spec_m)
  st_export_if(res, export)
}

# Default prediction of every model.
st_mg_type <- function(kind, predict) {
  def <- switch(kind, regress = "xb", logit = "pr", probit = "pr", poisson = "n", nbreg = "n",
                mlogit = "pr", ologit = "pr", oprobit = "pr", tobit = "xb")
  if (is.null(predict)) return(def)
  ok <- switch(kind, regress = "xb", logit = , probit = c("pr", "xb"), poisson = , nbreg = c("n", "ir", "xb"),
               mlogit = c("pr", "xb"), ologit = , oprobit = c("pr", "xb"), tobit = "xb")
  if (!predict %in% ok) {
    rlang::abort(sprintf("`predict` must be one of %s after %s.", paste0("\"", ok, "\"", collapse = ", "), kind))
  }
  predict
}

st_mg_outcomes <- function(m, spec, type, outcome) {
  if (!spec$kind %in% c("mlogit", "ologit", "oprobit")) return(list(index = NA_integer_))
  o <- m$outcomes
  if (type == "xb" && spec$kind %in% c("ologit", "oprobit")) return(list(index = NA_integer_))
  if (is.null(outcome)) {
    if (type == "xb") rlang::abort("Please give the `outcome` for predict = \"xb\" after mlogit.")
    return(list(index = seq_along(o$labels), labels = o$labels, codes = o$codes, multi = TRUE))
  }
  j <- match(as.character(outcome), o$codes)
  if (is.na(j)) j <- match(as.character(outcome), o$labels)
  if (is.na(j)) rlang::abort(sprintf("Outcome %s not found.", outcome))
  list(index = j, labels = o$labels[j], codes = o$codes[j], multi = FALSE)
}

# at(): values of continuous variables, categories (value or label) of
# factor variables; all combinations with the first variable varying slowest.
st_mg_at <- function(at, info, vars) {
  if (is.null(at)) return(list(vars = character(), grid = NULL))
  if (!is.list(at) || is.null(names(at)) || any(!nzchar(names(at)))) {
    rlang::abort("`at` must be a named list, e.g. at = list(age = c(20, 40)).")
  }
  bad <- setdiff(names(at), vars)
  if (length(bad)) rlang::abort(sprintf("`at` names variables that are not in the model: %s.", paste(bad, collapse = ", ")))
  vals <- lapply(names(at), function(v) {
    x <- at[[v]]
    if (!info[[v]]$cat) {
      if (!is.numeric(x)) rlang::abort(sprintf("`at` values of `%s` must be numeric.", v))
      return(as.numeric(x))
    }
    k <- match(as.character(x), info[[v]]$codes)
    k2 <- match(as.character(x), info[[v]]$levels)
    k[is.na(k)] <- k2[is.na(k)]
    if (anyNA(k)) rlang::abort(sprintf("`at` values of `%s` must be categories of the variable.", v))
    k
  })
  names(vals) <- names(at)
  grid <- expand.grid(rev(vals), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)[, rev(seq_along(vals)), drop = FALSE]
  names(grid) <- names(at)
  list(vars = names(at), grid = grid)
}

# Terms of the marginal effects: continuous variables, and for factor
# variables every category (the base category is kept for the display).
st_mg_dterms <- function(dvars, info, vars, D) {
  out <- list()
  for (v in vars[vars %in% dvars]) {
    if (!info[[v]]$cat) {
      out[[length(out) + 1L]] <- list(var = v, cat = FALSE)
    } else {
      for (l in seq_along(info[[v]]$levels)) {
        out[[length(out) + 1L]] <- list(var = v, cat = TRUE, level = l, is_base = l == info[[v]]$base)
      }
    }
  }
  out
}

# Prediction (or its derivative with dX) for every observation.
st_mg_eval <- function(m, spec, theta, X, dX, type, j) {
  kind <- spec$kind
  ok <- m$ok_cols %||% m$ok
  ncol <- length(ok)
  k <- sum(ok)
  if (kind == "mlogit") {
    J <- length(m$outcomes$labels)
    B <- matrix(0, ncol, J - 1L)
    B[ok, ] <- matrix(theta, k, J - 1L)
    E <- X %*% B
    if (type == "xb") {
      if (j == m$base_outcome) return(rep(0, nrow(X)))
      a <- match(j, m$eq_index)
      return(if (is.null(dX)) E[, a] else drop(dX %*% B[, a]))
    }
    mx <- pmax(apply(E, 1, max), 0)
    den <- exp(-mx) + rowSums(exp(E - mx))
    P <- cbind(exp(E - mx) / den)
    pj <- if (j == m$base_outcome) exp(-mx) / den else P[, match(j, m$eq_index)]
    if (is.null(dX)) return(pj)
    dE <- dX %*% B
    dj <- if (j == m$base_outcome) 0 else dE[, match(j, m$eq_index)]
    return(pj * (dj - rowSums(P * dE)))
  }
  b <- numeric(ncol)
  b[ok] <- theta[seq_len(k)]
  xb <- drop(X %*% b)
  dxb <- if (!is.null(dX)) drop(dX %*% b)
  if (kind %in% c("ologit", "oprobit")) {
    if (type == "xb") return(if (is.null(dX)) xb else dxb)
    Fd <- st_ord_dist(kind)
    cut <- theta[k + seq_len(length(m$outcomes$labels) - 1L)]
    J <- length(cut) + 1L
    hi <- if (j < J) cut[j] - xb else Inf
    lo <- if (j > 1L) cut[j - 1L] - xb else -Inf
    if (is.null(dX)) {
      return(if (j > 1L && all(lo > 0)) Fd$F(-lo) - Fd$F(-hi) else Fd$F(hi) - Fd$F(lo))
    }
    fh <- if (j < J) Fd$f(hi) else 0
    fl <- if (j > 1L) Fd$f(lo) else 0
    return(-(fh - fl) * dxb)
  }
  off <- if (kind %in% c("poisson", "nbreg") && type %in% c("n", "xb")) m$D$offset %||% 0 else 0
  switch(type,
    xb = if (is.null(dX)) xb + off else dxb,
    pr = {
      if (kind == "probit") return(if (is.null(dX)) stats::pnorm(xb) else stats::dnorm(xb) * dxb)
      p <- stats::plogis(xb)
      if (is.null(dX)) p else p * (1 - p) * dxb
    },
    n = {
      mu <- exp(xb + off)
      if (is.null(dX)) mu else mu * dxb
    },
    ir = {
      mu <- exp(xb)
      if (is.null(dX)) mu else mu * dxb
    }
  )
}

# Central differences of a vector function.
# Gradient of the (weighted) mean prediction, or of the mean derivative with
# dX, with respect to the estimated parameters.
st_mg_grad <- function(m, spec, theta, X, dX, type, j, w) {
  kind <- spec$kind
  ok <- m$ok_cols %||% m$ok
  k <- sum(ok)
  Xk <- X[, ok, drop = FALSE]
  dXk <- if (!is.null(dX)) dX[, ok, drop = FALSE]
  W <- w / sum(w)
  wm <- function(v, M) colSums(M * (W * v))
  if (kind == "mlogit") {
    J <- length(m$outcomes$labels)
    B <- matrix(theta, k, J - 1L)
    E <- Xk %*% B
    mx <- pmax(apply(E, 1, max), 0)
    den <- exp(-mx) + rowSums(exp(E - mx))
    P <- exp(E - mx) / den
    aj <- if (j == m$base_outcome) 0L else match(j, m$eq_index)
    if (type == "xb") {
      g <- matrix(0, k, J - 1L)
      if (aj) g[, aj] <- if (is.null(dX)) colSums(Xk * W) else colSums(dXk * W)
      return(as.vector(g))
    }
    pj <- if (aj) P[, aj] else exp(-mx) / den
    if (!is.null(dX)) {
      dE <- dXk %*% B
      s <- rowSums(P * dE)
      dEj <- if (aj) dE[, aj] else 0
    }
    g <- vapply(seq_len(J - 1L), function(a) {
      da <- as.numeric(a == aj)
      if (is.null(dX)) return(wm(pj * (da - P[, a]), Xk))
      wm(pj * (da - P[, a]) * (dEj - s) - pj * P[, a] * (dE[, a] - s), Xk) +
        wm(pj * (da - P[, a]), dXk)
    }, numeric(k))
    return(as.vector(g))
  }
  b <- theta[seq_len(k)]
  xb <- drop(Xk %*% b)
  dxb <- if (!is.null(dX)) drop(dXk %*% b)
  if (kind %in% c("ologit", "oprobit")) {
    Fd <- st_ord_dist(kind)
    nc <- length(m$outcomes$labels) - 1L
    if (type == "xb") return(c(if (is.null(dX)) colSums(Xk * W) else colSums(dXk * W), rep(0, nc)))
    cut <- theta[k + seq_len(nc)]
    fh <- if (j <= nc) Fd$f(cut[j] - xb) else rep(0, length(xb))
    fl <- if (j > 1L) Fd$f(cut[j - 1L] - xb) else rep(0, length(xb))
    dfh <- if (j <= nc) Fd$df(cut[j] - xb) else 0
    dfl <- if (j > 1L) Fd$df(cut[j - 1L] - xb) else 0
    gc <- numeric(nc)
    if (is.null(dX)) {
      gb <- wm(-(fh - fl), Xk)
      if (j <= nc) gc[j] <- sum(W * fh)
      if (j > 1L) gc[j - 1L] <- -sum(W * fl)
    } else {
      gb <- wm((dfh - dfl) * dxb, Xk) + wm(-(fh - fl), dXk)
      if (j <= nc) gc[j] <- -sum(W * dfh * dxb)
      if (j > 1L) gc[j - 1L] <- sum(W * dfl * dxb)
    }
    return(c(gb, gc))
  }
  off <- if (kind %in% c("poisson", "nbreg") && type == "n") m$D$offset %||% 0 else 0
  g <- switch(type,
    xb = if (is.null(dX)) colSums(Xk * W) else colSums(dXk * W),
    pr = {
      if (kind == "probit") {
        v <- stats::dnorm(xb)
        if (is.null(dX)) wm(v, Xk) else wm(-v * xb * dxb, Xk) + wm(v, dXk)
      } else {
        p <- stats::plogis(xb)
        v <- p * (1 - p)
        if (is.null(dX)) wm(v, Xk) else wm(v * (1 - 2 * p) * dxb, Xk) + wm(v, dXk)
      }
    },
    n = , ir = {
      mu <- exp(xb + off)
      if (is.null(dX)) wm(mu, Xk) else wm(mu * dxb, Xk) + wm(mu, dXk)
    })
  # further parameters (ln(alpha) of nbreg, the variance of tobit) do not
  # enter the prediction
  c(g, rep(0, length(theta) - k))
}

# Distribution of the ordered models: cdf, density and the derivative of the
# density.
st_ord_dist <- function(kind) {
  if (kind == "oprobit") {
    return(list(F = stats::pnorm, f = stats::dnorm, df = function(a) -a * stats::dnorm(a)))
  }
  list(F = stats::plogis, f = stats::dlogis, df = function(a) stats::dlogis(a) * (1 - 2 * stats::plogis(a)))
}

st_mg_estimable <- function(m, spec, scen, defs) {
  X0 <- m$D$X
  if (spec$kind %in% c("ologit", "oprobit")) X0 <- cbind(X0, 1)
  sv <- svd(X0 * sqrt(m$D$w))
  tol <- max(sv$d) * max(dim(X0)) * 1e-12
  nul <- sv$v[, sv$d <= max(tol, max(sv$d) * 1e-9), drop = FALSE]
  if (!ncol(nul)) return(rep(TRUE, length(defs)))
  ok_s <- vapply(scen, function(sc) {
    chk <- function(X) {
      if (spec$kind %in% c("ologit", "oprobit")) X <- cbind(X, if (identical(X, sc$dX)) 0 else 1)
      max(abs(X %*% nul)) <= 1e-7 * max(1, max(abs(X)))
    }
    chk(sc$X) && (is.null(sc$dX) || chk(sc$dX))
  }, logical(1))
  vapply(defs, function(d) {
    if (is.null(d$parts)) return(TRUE)
    all(ok_s[vapply(d$parts, `[[`, integer(1), "s")])
  }, logical(1))
}

st_mg_cmd <- function(over, dvars, at, atmeans, predict, outcome) {
  opts <- c(if (length(dvars)) sprintf("dydx(%s)", paste(dvars, collapse = " ")),
            if (!is.null(at)) sprintf("at(%s)", paste(vapply(names(at), function(v) {
              sprintf("%s=(%s)", v, paste(at[[v]], collapse = " "))
            }, character(1)), collapse = " ")),
            if (atmeans) "atmeans",
            if (!is.null(predict) || !is.null(outcome)) sprintf("predict(%s)", paste(c(predict, if (!is.null(outcome)) sprintf("outcome(%s)", outcome)), collapse = " ")))
  st_make_cmd("margins", over, opts = opts)
}

# Output -----------------------------------------------------------------------

#' @export
print.st_margins <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

# Description of the prediction, as in Stata's "Expression" line.
st_mg_expression <- function(sp, j = NULL, multi = FALSE) {
  y <- sp$y
  pr <- if (is.null(sp$predict)) "predict()" else sprintf("predict(%s)", sp$predict)
  switch(sp$kind,
    regress = , tobit = paste("Linear prediction,", pr),
    logit = , probit = if (sp$type == "xb") paste("Linear prediction (log odds),", pr) else sprintf("Pr(%s), %s", y, pr),
    poisson = , nbreg = switch(sp$type, n = paste("Predicted number of events,", pr),
                     ir = paste("Predicted incidence rate,", pr), xb = paste("Linear prediction,", pr)),
    ologit = , oprobit = , mlogit = {
      if (sp$type == "xb" && sp$kind != "mlogit") return(paste("Linear prediction (cutpoints excluded),", pr))
      o <- sp$outs
      code <- o$codes[j]
      # mlogit names the outcome by its equation, ologit by its value
      lab <- if (sp$kind == "mlogit") o$labels[j] else code
      what <- if (sp$type == "xb") sprintf("Linear prediction, predict(xb outcome(%s))", code) else
        sprintf("Pr(%s==%s), predict(%s)", y, lab,
                if (multi) sprintf("pr outcome(%s)", code) else sprintf("outcome(%s)", code))
      what
    })
}

st_parts_margins <- function(x) {
  sp <- attr(x, "st_spec", exact = TRUE)
  info <- sp$info
  deriv <- length(sp$dterms) > 0L
  title <- if (deriv) {
    if (sp$averaged) "Average marginal effects" else "Conditional marginal effects"
  } else {
    if (sp$averaged) "Predictive margins" else "Adjusted predictions"
  }
  n_txt <- formatC(sp$N, format = "d", big.mark = ",")
  vce <- if (sp$vce != "ols") "Robust" else if (sp$kind == "regress") "OLS" else "OIM"
  head <- c(if (!is.null(sp$pw)) paste("Pairwise comparisons of", tolower(title)) else
              paste0(st_pad(title, 48L, "left"), st_hdr_r("Number of obs", n_txt)),
            paste0("Model VCE    : ", vce), "")
  multi <- isTRUE(sp$outs$multi)
  if (!multi) head <- c(head, paste0("Expression   : ", st_mg_expression(sp, 1L)))
  if (deriv) {
    dl <- vapply(sp$dterms[!vapply(sp$dterms, function(d) isTRUE(d$is_base), logical(1))], function(d) {
      if (d$cat) paste0(info[[d$var]]$codes[d$level], ".", d$var) else d$var
    }, character(1))
    # a long list is wrapped as a paragraph with a hanging indent and a right
    # margin of two columns (Stata's {p 0 15 2})
    head <- c(head, st_wrap_par(paste0("dy/dx w.r.t. : ", paste(dl, collapse = " ")), 0L, 15L,
                                width = getOption("width", 80L) - 2L))
  }
  if (multi) {
    head <- c(head, vapply(seq_along(sp$outs$index), function(j) {
      paste0(st_pad(sprintf("%d._predict", j), 13L, "left"), ": ", st_mg_expression(sp, j, multi = TRUE))
    }, character(1)))
  }
  at_lines <- function(a) {
    vs <- if (sp$atmeans) sp$vars else sp$at$vars
    out <- character()
    for (v in vs) {
      if (v %in% sp$at$vars) {
        val <- sp$at$grid[[v]][a]
        txt <- if (info[[v]]$cat) info[[v]]$codes[val] else st_stata_g(val, 9L)
        out <- c(out, paste0(st_pad(st_abbrev_name(v, 12L), 16L, "left"), "=", st_pad(txt, 12L)))
      } else if (info[[v]]$cat) {
        mv <- sp$means[[v]][1, ]
        # categories without observations in the estimation sample are not listed
        for (l in which(mv > 0)) {
          out <- c(out, paste0(st_pad(st_abbrev_name(paste0(info[[v]]$codes[l], ".", v), 12L), 16L, "left"), "=",
                               st_pad(st_stata_g(mv[l], 9L), 12L), " (mean)"))
        }
      } else {
        mv <- (sp$shown_means %||% sp$means)[[v]][1]
        out <- c(out, paste0(st_pad(st_abbrev_name(v, 12L), 16L, "left"), "=", st_pad(st_stata_g(mv, 9L), 12L), " (mean)"))
      }
    }
    out
  }
  if (sp$n_at == 1L && (length(sp$at$vars) || sp$atmeans)) {
    l <- at_lines(1L)
    head <- c(head, paste0(c("at           : ", rep(strrep(" ", 15L), length(l) - 1L)), l))
  } else if (sp$n_at > 1L) {
    for (a in seq_len(sp$n_at)) {
      l <- at_lines(a)
      head <- c(head, "", paste0(c(st_pad(sprintf("%d._at", a), 13L, "left"), rep(strrep(" ", 13L), length(l) - 1L)),
                                 c(": ", rep("  ", length(l) - 1L)), l))
    }
  }
  while (!nzchar(head[length(head)])) head <- head[-length(head)]
  parts <- list(st_part_text(head, mono = TRUE))
  if (!is.null(sp$pw)) return(c(parts, list(st_pw_table(x, sp))))

  # dimensions of the rows: outcomes, at combinations, factor categories
  # (one block per factor term)
  base_dims <- c(if (multi) "_predict", if (sp$n_at > 1L) "_at")
  term_dims <- function(o) c(base_dims, if (length(sp$over_terms)) sp$over_terms[[o]])
  has_dims <- length(base_dims) > 0L || length(sp$over_terms) > 0L
  row_label <- function(r) {
    lab <- character()
    num <- logical()
    if (multi) { lab <- c(lab, as.character(r$j)); num <- c(num, TRUE) }
    if (sp$n_at > 1L) { lab <- c(lab, as.character(r$a)); num <- c(num, TRUE) }
    og <- sp$over_grids[[r$o]]
    for (k in seq_len(ncol(og))) {
      v <- colnames(og)[k]
      l <- info[[v]]$levels[og[r$g, k]]
      lab <- c(lab, l)
      num <- c(num, l == info[[v]]$codes[og[r$g, k]])
    }
    # labels are joined by "#", plain numbers by a space, as in Stata
    paste(lab, collapse = if (all(num)) " " else "#")
  }
  dim_labels <- vapply(seq_along(sp$over_grids), function(o) paste(term_dims(o), collapse = "#"), "")
  W <- st_ct_width(c(sp$dvars, sp$over, unlist(lapply(sp$dterms, function(d) {
    if (d$cat) paste0(info[[d$var]]$codes[d$level], ".", d$var)
  }))), dim_labels[grepl("#", dim_labels, fixed = TRUE)],
  c(if (has_dims) vapply(sp$rowinfo, row_label, ""),
    unlist(lapply(sp$dterms, function(d) if (d$cat && !isTRUE(d$is_base)) info[[d$var]]$levels[d$level]))))
  ri <- sp$rowinfo
  cell_of <- function(i) {
    if (x$note[i] == "not estimable") return(list(status = "notest"))
    list(status = "coef", cells = st_ct_cells(x$estimate[i], x$std_error[i], x$statistic[i], x$p_value[i],
                                              x$conf_low[i], x$conf_high[i]))
  }
  # one block per factor term for the rows `idx`
  block_terms <- function(idx) {
    os <- vapply(ri[idx], `[[`, integer(1), "o")
    lapply(unique(os), function(o) {
      ii <- idx[os == o]
      rows <- lapply(ii, function(i) c(list(label = row_label(ri[[i]])), cell_of(i)))
      # variable names are abbreviated to the width of the label column
      lab <- paste(c(base_dims, if (length(sp$over_terms)) st_abbrev_name(sp$over_terms[[o]], W)), collapse = "#")
      list(kind = "factor", label = lab, rows = rows, inter = length(term_dims(o)) > 1L)
    })
  }
  dims <- if (has_dims) "x" else character()
  eqs <- list()
  if (!deriv) {
    if (!length(dims)) {
      eqs <- list(list(name = NULL, terms = list(list(kind = "plain", label = "_cons",
                                                    rows = list(c(list(label = "_cons"), cell_of(1L)))))))
    } else {
      eqs <- list(list(name = NULL, terms = block_terms(seq_along(ri))))
    }
  } else {
    dkey <- vapply(ri, function(r) paste(r$dt$var, r$dt$level %||% 0L), character(1))
    keys <- unique(dkey)
    if (!length(dims)) {
      # one row per variable; factor variables as a block of categories
      terms <- list()
      done <- character()
      for (kk in keys) {
        i <- which(dkey == kk)
        dt <- ri[[i[1]]]$dt
        if (!dt$cat) {
          terms[[length(terms) + 1L]] <- list(kind = "plain", label = st_abbrev_name(dt$var, W),
                                              rows = list(c(list(label = dt$var), cell_of(i))))
        } else if (!dt$var %in% done) {
          done <- c(done, dt$var)
          lv <- which(vapply(ri, function(r) identical(r$dt$var, dt$var), logical(1)))
          rows <- lapply(lv, function(q) {
            d <- ri[[q]]$dt
            if (isTRUE(d$is_base)) {
              # the base category is shown only when it has no observations
              empty <- sum(sp$means[[d$var]][1, d$level]) == 0
              return(list(label = info[[d$var]]$levels[d$level], status = if (empty) "empty" else "base"))
            }
            c(list(label = info[[d$var]]$levels[d$level]), cell_of(q))
          })
          # a factor variable without value labels and a single category is
          # one row "1.am", as in Stata
          shown <- which(vapply(rows, function(r) r$status != "base", logical(1)))
          lvls <- vapply(lv, function(q) ri[[q]]$dt$level, integer(1))
          if (length(shown) == 1L && identical(rows[[shown]]$label, info[[dt$var]]$codes[lvls[shown]])) {
            lab1 <- paste0(info[[dt$var]]$codes[lvls[shown]], ".", st_abbrev_name(dt$var, W))
            terms[[length(terms) + 1L]] <- list(kind = "plain", label = lab1,
                                                rows = list(c(list(label = lab1), rows[[shown]][-1])))
            next
          }
          terms[[length(terms) + 1L]] <- list(kind = "factor", label = st_abbrev_name(dt$var, W), rows = rows)
        }
      }
      eqs <- list(list(name = NULL, terms = terms))
    } else {
      for (kk in keys) {
        i <- which(dkey == kk)
        dt <- ri[[i[1]]]$dt
        name <- if (dt$cat) paste0(info[[dt$var]]$codes[dt$level], ".", dt$var) else dt$var
        if (isTRUE(dt$is_base)) {
          eqs[[length(eqs) + 1L]] <- list(name = name, base = TRUE, terms = list())
        } else {
          eqs[[length(eqs) + 1L]] <- list(name = name, terms = block_terms(i))
        }
      }
    }
  }
  tab <- st_ct_build(eqs, W, "", if (deriv) "dy/dx" else "Margin", stat = sp$stat,
                     level = sp$level, top = "            Delta-method")
  parts <- c(parts, list(tab))
  if (any(vapply(sp$dterms, `[[`, logical(1), "cat"))) {
    parts <- c(parts, list(st_part_text("Note: dy/dx for factor levels is the discrete change from the base level.",
                                        mono = TRUE, attach = TRUE)))
  }
  parts
}

# Table of pairwise comparisons: contrasts with delta-method standard errors
# and unadjusted confidence intervals (and with mode "effects" the tests).
st_pw_table <- function(x, sp) {
  W <- st_ct_width(sp$pw$head, levels = sp$pw$labels)
  eff <- sp$pw$mode == "effects"
  stat <- sp$stat
  g <- function(v) if (is.na(v)) "." else st_stata_g(v, 9L)
  rule_w <- if (eff) 64L else 48L
  top1 <- if (eff) "            Delta-method    Unadjusted           Unadjusted" else
    "            Delta-method         Unadjusted"
  top2 <- if (eff) {
    paste0("   Contrast   Std. Err.", st_pad(stat, 7L), st_pad(sprintf("P>|%s|", stat), 9L),
           st_pad(sprintf("[%s%% Conf. Interval]", format(sp$level)), 25L))
  } else {
    paste0("   Contrast   Std. Err.", st_pad(sprintf("[%s%% Conf. Interval]", format(sp$level)), 25L))
  }
  blank <- paste0(strrep(" ", W), " |")
  text <- c(strrep("-", W + 2L + rule_w), paste0(blank, top1), paste0(blank, top2),
            paste0(strrep("-", W + 1L), "+", strrep("-", rule_w)),
            paste0(st_pad(st_abbrev_name(sp$pw$head, W), W), " |"))
  body <- list()
  for (i in seq_len(nrow(x))) {
    cells <- if (eff) {
      c(st_pad(g(x$estimate[i]), 11L), st_pad(g(x$std_error[i]), 11L), st_pad(st_fmt_t(x$statistic[i]), 9L),
        st_pad(sprintf("%.3f", x$p_value[i]), 8L), st_pad(g(x$conf_low[i]), 13L), st_pad(g(x$conf_high[i]), 12L))
    } else {
      c(st_pad(g(x$estimate[i]), 11L), st_pad(g(x$std_error[i]), 11L), st_pad(g(x$conf_low[i]), 14L),
        st_pad(g(x$conf_high[i]), 12L))
    }
    lab <- sp$pw$labels[i]
    # a label that is too long is shown on three lines ("a", "vs", "b")
    pieces <- if (st_width(lab) > W - 1L) strsplit(lab, " vs ", fixed = TRUE)[[1]] else lab
    if (length(pieces) == 2L) pieces <- c(pieces[1], "vs", pieces[2])
    if (length(pieces) > 1L) {
      text <- c(text, paste0(st_pad(st_ct_trunc(pieces[-length(pieces)], W - 1L), W - 1L), "  |"))
    }
    text <- c(text, paste0(st_pad(st_ct_trunc(pieces[length(pieces)], W - 1L), W - 1L), "  |", paste(cells, collapse = "")))
    body[[i]] <- trimws(cells)
  }
  text <- c(text, strrep("-", W + 2L + rule_w))
  head_cells <- if (eff) c("Contrast", "Std. Err.", stat, sprintf("P>|%s|", stat), sprintf("[%s%% Conf.", format(sp$level)), "Interval]") else
    c("Contrast", "Std. Err.", sprintf("[%s%% Conf.", format(sp$level)), "Interval]")
  part <- st_part_grid(stub = c(sp$pw$head, paste0("  ", sp$pw$labels)),
                       body = rbind(rep("", length(head_cells)), do.call(rbind, body)),
                       header = matrix(head_cells, 1L), stub_header = "", rules = 0L, frame = TRUE)
  part$text <- st_rtrim(text)
  part
}

#' Plot margins (Stata: `marginsplot`)
#'
#' Draws the result of [s.margins()] as Stata's `marginsplot` does. The
#' margins or marginal effects with their confidence intervals, over the
#' values of the first `at` variable (or the categories of the first factor
#' variable), with one line per outcome, further `at` value or category.
#' The plot uses the default design of ggplot2, not Stata's graph scheme, and
#' can be changed with the usual ggplot2 functions. Lines get their own color
#' only if there are several of them.
#'
#' @param x Result of [s.margins()] or of an estimation command with
#'   `ame = TRUE`.
#' @param title,xlab,ylab Optional title and axis labels (defaults as in
#'   Stata).
#' @return A ggplot object (package 'ggplot2' required).
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   m <- s.logit(mtcars, am, wt, nolog = TRUE)
#'   s.marginsplot(s.margins(m, at = list(wt = seq(2, 5, 0.5))))
#' }
#' @export
s.marginsplot <- function(x, title = NULL, xlab = NULL, ylab = NULL) {
  # a model estimated with ame = TRUE: its average marginal effects
  if (inherits(x, st_est_classes) && isTRUE(attr(x, "st_spec", exact = TRUE)$ame)) {
    models <- attr(x, "st_model", exact = TRUE)
    if (length(models) != 1L) rlang::abort("Please draw grouped models one group at a time.")
    x <- models[[1]]$ame
  }
  if (!inherits(x, "st_margins")) rlang::abort("`x` must be a result of s.margins() or of a model with `ame = TRUE`.")
  if (!is.null(attr(x, "st_spec", exact = TRUE)$pw)) rlang::abort("s.marginsplot() does not draw pairwise comparisons.")
  st_need("ggplot2", "s.marginsplot()")
  sp <- attr(x, "st_spec", exact = TRUE)
  d <- st_plain(x)
  d <- d[d$note != "base", , drop = FALSE]
  deriv <- length(sp$dterms) > 0L
  # x axis: first at variable with several values, else the first factor
  # variable, else the terms of the marginal effects
  xvar <- NULL
  if (length(sp$at$vars)) {
    nv <- vapply(sp$at$vars, function(v) length(unique(sp$at$grid[[v]])), integer(1))
    if (any(nv > 1L)) xvar <- sp$at$vars[nv > 1L][1]
  }
  if (is.null(xvar) && length(sp$over)) xvar <- sp$over[1]
  groups <- character()
  if (isTRUE(sp$outs$multi)) groups <- c(groups, "outcome")
  if (deriv && length(unique(paste(d$term, d$level))) > 1L && !is.null(xvar)) groups <- c(groups, "term_level")
  if (!is.null(xvar)) {
    others <- setdiff(c(sp$at$vars, sp$over), xvar)
    others <- others[vapply(others, function(v) length(unique(d[[v]])) > 1L, logical(1))]
    groups <- c(groups, others)
  }
  d$term_level <- ifelse(is.na(d$level), d$term, paste0(d$level, ".", d$term))
  if (is.null(xvar)) {
    d$.x <- factor(if (deriv) d$term_level else "_cons", levels = unique(if (deriv) d$term_level else "_cons"))
  } else {
    d$.x <- d[[xvar]]
  }
  d$.g <- if (length(groups)) interaction(d[groups], sep = ", ", drop = TRUE, lex.order = TRUE) else factor("1")
  what <- if (deriv) {
    if (sp$averaged) "Average Marginal Effects" else "Conditional Marginal Effects"
  } else if (sp$averaged) "Predictive Margins" else "Adjusted Predictions"
  if (deriv && length(sp$dvars) == 1L) what <- paste(what, "of", sp$dvars)
  title <- title %||% sprintf("%s with %s%% CIs", what, format(sp$level))
  pred <- switch(sp$type, xb = "Linear Prediction", pr = if (sp$kind %in% c("logit", "probit")) sprintf("Pr(%s)", sp$y) else "Probability",
                 n = "Predicted Number Of Events", ir = "Predicted Incidence Rate")
  if (sp$kind == "regress") pred <- "Linear Prediction"
  ylab <- ylab %||% if (deriv) paste("Effects on", pred) else pred
  xlab <- xlab %||% if (is.null(xvar)) "" else xvar
  # ggplot2's default design; colours only distinguish several lines
  aes_g <- if (length(groups)) {
    ggplot2::aes(x = .data$.x, y = .data$estimate, colour = .data$.g, group = .data$.g)
  } else {
    ggplot2::aes(x = .data$.x, y = .data$estimate, group = .data$.g)
  }
  p <- ggplot2::ggplot(d, aes_g) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$conf_low, ymax = .data$conf_high),
                           width = if (is.numeric(d$.x)) 0.02 * diff(range(d$.x)) else 0.1, na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE)
  if (!is.null(xvar) && is.numeric(d$.x)) p <- p + ggplot2::geom_line(na.rm = TRUE)
  if (deriv) p <- p + ggplot2::geom_hline(yintercept = 0, colour = "grey50")
  p + ggplot2::labs(title = title, x = xlab, y = ylab, colour = NULL)
}
