#' Pairwise correlations with significance levels (Stata: `pwcorr`)
#'
#' Re-implementation of Stata's
#' `pwcorr [varlist] [weight] [, obs sig star(#) print(#) bonferroni sidak listwise]`.
#' The lower triangle of the correlation matrix shows Pearson's correlation
#' coefficient for each pair of variables. Each coefficient uses all
#' observations without a missing value in the two variables (pairwise
#' deletion), so the number of observations can differ between pairs.
#'
#' * `sig = TRUE` adds the p-value of the test that the correlation is zero
#'   in the population, `obs = TRUE` the number of observations.
#' * `star = 0.05` marks coefficients significant at the 5% level with `*`,
#'   `print = 0.05` shows only those coefficients.
#' * `bonferroni` and `sidak` adjust the p-values for multiple comparisons.
#' * `listwise = TRUE` uses only observations without a missing value in any
#'   of the variables (Stata's `listwise` or `casewise`, the sample of
#'   `correlate`).
#' * Weights. `fweight` and `aweight`, as in Stata (see [statsalike-weights]).
#' * Variables that are not numeric (strings, factors) are not allowed. If no
#'   variables are given, all numeric variables are used.
#' * `by x:` becomes `group_by(x)` before the call.
#'
#' The p-value follows Stata. With the correlation \eqn{r} and the number of
#' observations \eqn{n}, \eqn{t = r \sqrt{n - 2} / \sqrt{1 - r^2}}{t = r *
#' sqrt(n - 2) / sqrt(1 - r^2)} is compared with a t distribution with
#' \eqn{n - 2} degrees of freedom (two-sided). With `bonferroni` it is
#' multiplied by the number of pairs \eqn{k} (at most 1), with `sidak` it
#' becomes \eqn{1 - (1 - p)^k}. With `fweight`, \eqn{n} is the sum of the
#' weights. With `aweight`, the number of observations.
#'
#' @param .data A data frame or tibble (may be grouped with [dplyr::group_by()]).
#' @param ... Numeric variables (tidyselect). All numeric variables by default
#'   (without the grouping variables).
#' @param wt Optional weighting variable or expression.
#' @param weight_type `"fweight"` or `"aweight"`, as in Stata.
#' @param obs Display the number of observations of each pair.
#' @param sig Display the significance level (p-value) of each coefficient.
#' @param star Significance level, e.g. `0.05`. Coefficients with a p-value
#'   at or below it are marked with `*` (Stata's `star(#)`).
#' @param print Significance level, e.g. `0.05`. Only coefficients with a
#'   p-value at or below it are displayed (Stata's `print(#)`).
#' @param bonferroni,sidak Adjust the p-values for multiple comparisons.
#' @param listwise Use only observations that are complete in all variables.
#' @param export Optional file name ending in `.html` or `.docx`. The table
#'   is also exported to this file (see [s.export()]).
#'
#' @return A tibble of class `st_pwcorr` with one row per pair of the lower
#'   triangle (including the diagonal) and the columns `var1` (row variable), `var2`
#'   (column variable), `r` (correlation), `p_value` (adjusted with
#'   `bonferroni` or `sidak`, `NA` on the diagonal) and `obs`. For grouped
#'   data the grouping variables come first. It prints in Stata's layout.
#'
#' @examples
#' # pwcorr mpg hp wt
#' s.pwcorr(mtcars, mpg, hp, wt)
#'
#' # pwcorr mpg hp wt qsec, sig obs star(.05)
#' s.pwcorr(mtcars, mpg, hp, wt, qsec, sig = TRUE, obs = TRUE, star = 0.05)
#'
#' # pwcorr mpg hp wt, sig bonferroni
#' s.pwcorr(mtcars, mpg, hp, wt, sig = TRUE, bonferroni = TRUE)
#'
#' # by am: pwcorr mpg wt
#' mtcars |> dplyr::group_by(am) |> s.pwcorr(mpg, wt, sig = TRUE)
#'
#' # the coefficients as an ordinary tibble
#' s.pwcorr(mtcars, mpg, hp, wt, sig = TRUE) |> dplyr::filter(var1 != var2)
#' @export
s.pwcorr <- function(.data, ..., wt = NULL, weight_type = c("fweight", "aweight"),
                     obs = FALSE, sig = FALSE, star = NULL, print = NULL,
                     bonferroni = FALSE, sidak = FALSE, listwise = FALSE,
                     export = NULL) {
  st_check_data(.data)
  weight_type <- match.arg(weight_type)
  grp <- st_groups(.data)
  data <- dplyr::ungroup(.data)
  vars <- if (...length()) {
    names(tidyselect::eval_select(rlang::expr(c(...)), data, allow_rename = FALSE))
  } else {
    v <- setdiff(names(data), names(grp$keys))
    v[vapply(data[v], st_is_summable, logical(1))]
  }
  if (!length(vars)) rlang::abort("No numeric variables to correlate.")
  bad <- vars[!vapply(data[vars], st_is_summable, logical(1))]
  if (length(bad)) {
    rlang::abort(paste0("Correlations require numeric variables; not numeric: ",
                        paste(bad, collapse = ", "), "."))
  }
  if (bonferroni && sidak) rlang::abort("Only one of `bonferroni` and `sidak` can be used.")
  for (a in c("star", "print")) {
    v <- get(a)
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || is.na(v) || v <= 0 || v >= 1)) {
      rlang::abort(sprintf("`%s` must be a significance level between 0 and 1, e.g. 0.05.", a))
    }
  }
  wt_quo <- rlang::enquo(wt)
  w <- st_eval_weights(wt_quo, data, weight_type) %||% rep(1, nrow(data))
  X <- do.call(cbind, lapply(vars, function(v) st_num(data[[v]])))
  adjust <- if (bonferroni) "bonferroni" else if (sidak) "sidak" else "none"

  models <- list()
  pieces <- list()
  for (g in seq_along(grp$rows)) {
    idx <- grp$rows[[g]]
    if (!length(idx) && !is.null(grp$keys)) next
    m <- st_pwcorr_model(X[idx, , drop = FALSE], w[idx], weight_type, listwise, adjust)
    m$label <- grp$labels[g]
    models[[length(models) + 1L]] <- m
    low <- lower.tri(m$r, diag = TRUE)
    ij <- which(t(low), arr.ind = TRUE)[, 2:1, drop = FALSE]
    df <- tibble::tibble(var1 = vars[ij[, 1]], var2 = vars[ij[, 2]],
                         r = m$r[ij], p_value = m$p[ij], obs = m$n[ij])
    if (!is.null(grp$keys)) {
      df <- dplyr::bind_cols(grp$keys[rep(g, nrow(df)), , drop = FALSE], df)
    }
    pieces[[length(pieces) + 1L]] <- df
  }

  spec <- list(
    vars = vars, obs = obs, sig = sig, star = star, print = print,
    cmd = st_make_cmd("pwcorr", if (...length()) vars,
                      weight = st_weight_label(wt_quo, weight_type),
                      opts = c(if (obs) "obs", if (sig) "sig",
                               if (!is.null(star)) sprintf("star(%g)", star),
                               if (!is.null(print)) sprintf("print(%g)", print),
                               if (bonferroni) "bonferroni", if (sidak) "sidak",
                               if (listwise) "listwise"),
                      by = names(grp$keys))
  )
  res <- st_new_output(dplyr::bind_rows(pieces), "st_pwcorr", st_model = models, st_spec = spec)
  st_export_if(res, export)
}

# Correlations, p-values and numbers of observations of all pairs in one group.
st_pwcorr_model <- function(X, w, weight_type, listwise, adjust) {
  p <- ncol(X)
  k <- p * (p - 1) / 2
  use <- !is.na(w) & w != 0
  if (listwise) use <- use & stats::complete.cases(X)
  R <- P <- N <- matrix(NA_real_, p, p)
  for (i in seq_len(p)) for (j in seq_len(i)) {
    ok <- use & !is.na(X[, i]) & !is.na(X[, j])
    wi <- w[ok]
    n <- if (weight_type == "fweight") sum(wi) else sum(ok)
    r <- st_wcor(X[ok, i], X[ok, j], wi)
    N[i, j] <- n
    R[i, j] <- r
    if (i != j) P[i, j] <- st_adjust_p(st_cor_p(r, n), k, adjust)
  }
  list(r = R, p = P, n = N)
}

# Weighted Pearson correlation; NA without variation.
st_wcor <- function(x, y, w) {
  if (length(x) < 2L) return(NA_real_)
  W <- sum(w)
  dx <- x - sum(w * x) / W
  dy <- y - sum(w * y) / W
  sxx <- sum(w * dx^2)
  syy <- sum(w * dy^2)
  if (sxx <= 0 || syy <= 0) return(NA_real_)
  max(-1, min(1, sum(w * dx * dy) / sqrt(sxx * syy)))
}

# Two-sided p-value of H0: rho = 0 (t test with n - 2 degrees of freedom).
st_cor_p <- function(r, n) {
  if (is.na(r) || n <= 2) return(NA_real_)
  if (abs(r) >= 1) return(0)
  t <- r * sqrt(n - 2) / sqrt(1 - r^2)
  2 * stats::pt(-abs(t), df = n - 2)
}

st_adjust_p <- function(p, k, adjust) {
  if (is.na(p)) return(p)
  switch(adjust,
    bonferroni = min(1, p * k),
    sidak = min(1, 1 - (1 - p)^k),
    p
  )
}

# Variable names as Stata abbreviates them: "displacement" -> "displa~t".
st_abbrev_name <- function(x, n) {
  long <- nchar(x) > n
  x[long] <- paste0(substr(x[long], 1L, n - 2L), "~",
                    substr(x[long], nchar(x[long]), nchar(x[long])))
  x
}

# Output -----------------------------------------------------------------------

#' @export
print.st_pwcorr <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

st_parts_pwcorr <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  parts <- list()
  for (m in attr(x, "st_model", exact = TRUE)) {
    if (!is.na(m$label)) parts <- c(parts, list(st_part_heading(m$label)))
    parts <- c(parts, list(st_part_pwcorr(m, spec)))
  }
  parts
}

# Stata's layout: one block per row variable with the coefficients and,
# with `sig` and `obs`, the p-values and numbers of observations below them,
# followed by an empty line.
st_part_pwcorr <- function(m, spec) {
  p <- length(spec$vars)
  per <- 1L + spec$sig + spec$obs
  gap <- per > 1L
  stub <- character()
  rows <- list()
  for (i in seq_len(p)) {
    coef <- sig <- n <- rep("", p)
    for (j in seq_len(i)) {
      pv <- m$p[i, j]
      if (!is.null(spec$print) && i != j && !(isTRUE(pv <= spec$print))) next
      r <- m$r[i, j]
      coef[j] <- if (is.na(r)) st_na() else sprintf("%.4f", r)
      if (!is.null(spec$star)) coef[j] <- paste0(coef[j], if (isTRUE(pv <= spec$star)) "*" else " ")
      if (i != j) sig[j] <- if (is.na(pv)) st_na() else sprintf("%.4f", pv)
      n[j] <- st_fmt_g(m$n[i, j])
    }
    block <- list(coef)
    if (spec$sig) block <- c(block, list(sig))
    if (spec$obs) block <- c(block, list(n))
    if (gap && i < p) block <- c(block, list(rep("", p)))
    rows <- c(rows, block)
    stub <- c(stub, st_abbrev_name(spec$vars[i], 12L), rep("", length(block) - 1L))
  }
  st_part_grid(stub = stub, body = do.call(rbind, rows),
               header = st_abbrev_name(spec$vars, 8L), rules = 0L)
}
