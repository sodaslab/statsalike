# Line of text with fields at fixed columns, as Stata's _col(#): a field
# starts at its column, or right after the previous field if that is longer.
st_cols <- function(...) {
  f <- list(...)
  out <- ""
  for (i in seq(1L, length(f), by = 2L)) {
    col <- f[[i]]
    pad <- col - 1L - st_width(out)
    if (pad > 0L) out <- paste0(out, strrep(" ", pad))
    out <- paste0(out, f[[i + 1L]])
  }
  out
}

# Three strings centred about the columns 11, 40 and 70, as the lines of the
# alternative hypotheses below Stata's ttest (the third further right if it
# is long).
st_center3 <- function(a, b, c) {
  out <- ""
  last <- 0L
  centers <- c(11L, 40L, if (nchar(c) > 20L) 79L - nchar(c) %/% 2L else 70L)
  for (i in 1:3) {
    s <- c(a, b, c)[i]
    skip <- max(as.integer(last != 0L), centers[i] - nchar(s) %/% 2L - 1L - last)
    out <- paste0(out, strrep(" ", skip), s)
    last <- last + skip + nchar(s)
  }
  out
}

#' t tests of means (Stata: `ttest`)
#'
#' Re-implementation of Stata's `ttest` with its output.
#'
#' * One-sample test, `s.ttest(df, x, mu = 20)` (Stata's `ttest x == 20`).
#' * Two-sample test of the groups of a variable, `s.ttest(df, x, by = g)`
#'   (`ttest x, by(g)`), with `unequal = TRUE` for unequal variances
#'   (Satterthwaite's degrees of freedom) or `welch = TRUE` (Welch's).
#' * Paired test of two variables, `s.ttest(df, x, y)` (`ttest x == y`), or
#'   with `unpaired = TRUE` the two-sample test of two variables.
#'
#' The table shows the number of observations, mean, standard error,
#' standard deviation and confidence interval of every group (or variable)
#' and of the difference, below it the t statistic, its degrees of freedom
#' and the p-values of the three alternative hypotheses.
#'
#' @param .data A data frame.
#' @param x The variable to test (unquoted).
#' @param y Second variable for the paired (or unpaired) test.
#' @param mu Hypothesized mean of the one-sample test.
#' @param by Grouping variable with two groups.
#' @param unequal,welch Unequal variances with Satterthwaite's or Welch's
#'   degrees of freedom.
#' @param unpaired Compare `x` and `y` as two independent samples.
#' @param level Confidence level in percent.
#' @param export Optional file name ending in `.html` or `.docx`.
#' @return A tibble of class `st_test` with the statistic, degrees of freedom
#'   and the three p-values (`p_lower`, `p`, `p_upper`). It prints in
#'   Stata's layout.
#' @examples
#' s.ttest(mtcars, mpg, mu = 20)
#' s.ttest(mtcars, mpg, by = am)
#' s.ttest(mtcars, mpg, by = am, unequal = TRUE)
#' s.ttest(mtcars, drat, wt)
#' @export
s.ttest <- function(.data, x, y = NULL, mu = NULL, by = NULL, unequal = FALSE, welch = FALSE,
                    unpaired = FALSE, level = 95, export = NULL) {
  st_check_data(.data)
  data <- dplyr::ungroup(.data)
  xn <- st_select1(rlang::enquo(x), data, "x")
  yq <- rlang::enquo(y)
  bq <- rlang::enquo(by)
  yn <- if (!rlang::quo_is_null(yq)) st_select1(yq, data, "y")
  bn <- if (!rlang::quo_is_null(bq)) st_select1(bq, data, "by")
  if (sum(!is.null(yn), !is.null(bn), !is.null(mu)) != 1L) {
    rlang::abort("Please give exactly one of `mu` (one sample), `y` (two variables) and `by` (two groups).")
  }
  if (welch) unequal <- TRUE
  xv <- st_num(data[[xn]])
  q <- function(df) stats::qt(1 - (1 - level / 100) / 2, df)
  # one row of the table: name, n, mean, se, sd and the interval
  stats_of <- function(v) {
    v <- v[!is.na(v)]
    n <- length(v)
    m <- mean(v)
    s <- if (n > 1) stats::sd(v) else 0
    list(n = n, mean = m, sd = s, se = s / sqrt(n))
  }
  row_txt <- function(name, s, df = s$n - 1, se = s$se, n = TRUE, sd = TRUE) {
    st_cols(1L, paste0(st_pad(st_abbrev_name(name, 8L), 8L), " |"),
            12L, if (n) formatC(s$n, width = 7L, format = "d", big.mark = ",") else "",
            22L, st_pad(st_stata_g(s$mean, 9L), 9L), 34L, st_pad(st_stata_g(se, 9L), 9L),
            46L, if (sd) st_pad(st_stata_g(s$sd, 9L), 9L) else "",
            58L, st_pad(st_stata_g(s$mean - q(df) * se, 9L), 9L),
            70L, st_pad(st_stata_g(s$mean + q(df) * se, 9L), 9L))
  }
  lv <- format(level)
  header <- function(col1) {
    c(strrep("-", 78L),
      st_cols(1L, paste0(st_pad(col1, 8L), " |"), 16L, "Obs", 27L, "Mean", 35L, "Std. Err.", 47L, "Std. Dev.",
              61L - nchar(lv) + (nchar(lv) == 5L), sprintf("[%s%% Conf. Interval]", lv)),
      paste0(strrep("-", 9L), "+", strrep("-", 68L)))
  }
  div <- paste0(strrep("-", 9L), "+", strrep("-", 68L))
  pfmt <- function(p) sprintf("%6.4f", p)
  p3 <- function(t, df) c(stats::pt(t, df), 2 * stats::pt(-abs(t), df), stats::pt(t, df, lower.tail = FALSE))
  if (!is.null(mu)) {
    if (!is.numeric(mu) || length(mu) != 1L) rlang::abort("`mu` must be a number.")
    s <- stats_of(xv)
    df <- s$n - 1
    t <- (s$mean - mu) / s$se
    p <- p3(t, df)
    m0 <- format(mu)
    lines <- c("One-sample t test", header("Variable"), row_txt(xn, s), strrep("-", 78L),
               st_cols(5L, sprintf("mean = mean(%s)", st_abbrev_name(xn, 16L)), 67L, sprintf("t = %8.4f", t)),
               st_cols(1L, paste0("Ho: mean = ", m0), 50L, sprintf("degrees of freedom = %8.0f", df)), "",
               st_center3(sprintf("Ha: mean < %s", m0), sprintf("Ha: mean != %s", m0), sprintf("Ha: mean > %s", m0)),
               st_center3(paste("Pr(T < t) =", pfmt(p[1])), paste("Pr(|T| > |t|) =", pfmt(p[2])),
                          paste("Pr(T > t) =", pfmt(p[3]))))
    tidy <- tibble::tibble(test = "one-sample", t = t, df = df, p_lower = p[1], p = p[2], p_upper = p[3])
    cmd <- sprintf("ttest %s == %s", xn, m0)
  } else if (!is.null(yn) && !unpaired) {
    yv <- st_num(data[[yn]])
    ok <- !is.na(xv) & !is.na(yv)
    sx <- stats_of(xv[ok])
    sy <- stats_of(yv[ok])
    sd_ <- stats_of(xv[ok] - yv[ok])
    df <- sd_$n - 1
    t <- sd_$mean / sd_$se
    p <- p3(t, df)
    lines <- c("Paired t test", header("Variable"), row_txt(xn, sx), row_txt(yn, sy), div,
               row_txt("diff", sd_), strrep("-", 78L),
               st_cols(6L, sprintf("mean(diff) = mean(%s - %s)", st_abbrev_name(xn, 16L), st_abbrev_name(yn, 16L)),
                       67L, sprintf("t = %8.4f", t)),
               st_cols(1L, " Ho: mean(diff) = 0", 50L, sprintf("degrees of freedom = %8.0f", df)), "",
               st_center3("Ha: mean(diff) < 0", "Ha: mean(diff) != 0", "Ha: mean(diff) > 0"),
               st_center3(paste("Pr(T < t) =", pfmt(p[1])), paste("Pr(|T| > |t|) =", pfmt(p[2])),
                          paste("Pr(T > t) =", pfmt(p[3]))))
    tidy <- tibble::tibble(test = "paired", t = t, df = df, p_lower = p[1], p = p[2], p_upper = p[3])
    cmd <- sprintf("ttest %s == %s", xn, yn)
  } else {
    if (!is.null(bn)) {
      g <- st_as_levels(data[[bn]])
      ok <- !is.na(g) & !is.na(xv)
      g <- droplevels(g[ok])
      if (nlevels(g) != 2L) rlang::abort(sprintf("`%s` must have exactly two groups (%d found).", bn, nlevels(g)))
      a <- xv[ok][g == levels(g)[1]]
      b <- xv[ok][g == levels(g)[2]]
      names2 <- levels(g)
      col1 <- "Group"
      cmd <- sprintf("ttest %s, by(%s)", xn, bn)
    } else {
      a <- xv[!is.na(xv)]
      b <- st_num(data[[yn]])
      b <- b[!is.na(b)]
      names2 <- c(xn, yn)
      col1 <- "Variable"
      cmd <- sprintf("ttest %s == %s, unpaired", xn, yn)
    }
    sa <- stats_of(a)
    sb <- stats_of(b)
    sc <- stats_of(c(a, b))
    diff <- sa$mean - sb$mean
    if (unequal) {
      va <- sa$sd^2 / sa$n
      vb <- sb$sd^2 / sb$n
      se <- sqrt(va + vb)
      df <- if (welch) (va + vb)^2 / (va^2 / (sa$n + 1) + vb^2 / (sb$n + 1)) - 2 else
        (va + vb)^2 / (va^2 / (sa$n - 1) + vb^2 / (sb$n - 1))
    } else {
      df <- sa$n + sb$n - 2
      sp <- sqrt(((sa$n - 1) * sa$sd^2 + (sb$n - 1) * sb$sd^2) / df)
      se <- sp * sqrt(1 / sa$n + 1 / sb$n)
    }
    t <- diff / se
    p <- p3(t, df)
    dft <- if (welch) "Welch's degrees of freedom = " else if (unequal) "Satterthwaite's degrees of freedom = " else
      "degrees of freedom = "
    title <- paste("Two-sample t test with", if (unequal) "unequal variances" else "equal variances")
    lines <- c(title, header(col1), row_txt(names2[1], sa), row_txt(names2[2], sb), div,
               row_txt("combined", sc), div,
               row_txt("diff", list(mean = diff), df = df, se = se, n = FALSE, sd = FALSE), strrep("-", 78L),
               st_cols(5L, sprintf("diff = mean(%s) - mean(%s)", st_abbrev_name(names2[1], 16L),
                                   st_abbrev_name(names2[2], 16L)), 67L, sprintf("t = %8.4f", t)),
               st_cols(1L, "Ho: diff = 0", 71L - nchar(dft), paste0(dft, st_pad(st_stata_g(df, 8L), 8L))), "",
               st_center3("Ha: diff < 0", "Ha: diff != 0", "Ha: diff > 0"),
               st_center3(paste("Pr(T < t) =", pfmt(p[1])), paste("Pr(|T| > |t|) =", pfmt(p[2])),
                          paste("Pr(T > t) =", pfmt(p[3]))))
    tidy <- tibble::tibble(test = if (unequal) "two-sample, unequal variances" else "two-sample",
                           t = t, df = df, p_lower = p[1], p = p[2], p_upper = p[3])
  }
  res <- st_new_output(tidy, "st_test", st_spec = list(parts = list(st_part_text(lines, mono = TRUE)), cmd = cmd))
  st_export_if(res, export)
}

#' One-way analysis of variance (Stata: `oneway`)
#'
#' Re-implementation of Stata's
#' `oneway response factor [, tabulate bonferroni scheffe sidak]` with its
#' output. The analysis-of-variance table (between and within groups),
#' Bartlett's test for equal variances and, on request, the means of the
#' groups and multiple-comparison tests of all pairs of groups.
#'
#' @param .data A data frame.
#' @param y The response variable (unquoted).
#' @param g The grouping variable (unquoted).
#' @param tabulate Show the mean, standard deviation and frequency of every
#'   group.
#' @param bonferroni,scheffe,sidak Multiple-comparison tests. The difference
#'   of the means of every pair of groups (row minus column) with its
#'   adjusted p-value.
#' @param export Optional file name ending in `.html` or `.docx`.
#' @return A tibble of class `st_test` with the F test and Bartlett's test.
#'   It prints in Stata's layout.
#' @examples
#' s.oneway(mtcars, mpg, cyl)
#' s.oneway(mtcars, mpg, cyl, tabulate = TRUE, bonferroni = TRUE)
#' @export
s.oneway <- function(.data, y, g, tabulate = FALSE, bonferroni = FALSE, scheffe = FALSE,
                     sidak = FALSE, export = NULL) {
  st_check_data(.data)
  data <- dplyr::ungroup(.data)
  yn <- st_select1(rlang::enquo(y), data, "y")
  gn <- st_select1(rlang::enquo(g), data, "g")
  yv <- st_num(data[[yn]])
  gv <- st_as_levels(data[[gn]])
  ok <- !is.na(yv) & !is.na(gv)
  yv <- yv[ok]
  gv <- droplevels(gv[ok])
  k <- nlevels(gv)
  if (k < 2L) rlang::abort("`g` must have at least two groups.")
  n <- tapply(yv, gv, length)
  m <- tapply(yv, gv, mean)
  v <- tapply(yv, gv, function(z) if (length(z) > 1) stats::var(z) else NA_real_)
  N <- length(yv)
  grand <- mean(yv)
  ssb <- sum(n * (m - grand)^2)
  ssw <- sum((yv - m[as.integer(gv)])^2)
  dfb <- k - 1
  dfw <- N - k
  msb <- ssb / dfb
  msw <- ssw / dfw
  Fv <- msb / msw
  pF <- stats::pf(Fv, dfb, dfw, lower.tail = FALSE)
  # Bartlett's test of equal variances (groups with more than one observation)
  use <- n > 1
  sp2 <- sum((n[use] - 1) * v[use]) / sum(n[use] - 1)
  kb <- sum(use)
  chi_b <- (sum(n[use] - 1) * log(sp2) - sum((n[use] - 1) * log(v[use]))) /
    (1 + (sum(1 / (n[use] - 1)) - 1 / sum(n[use] - 1)) / (3 * (kb - 1)))
  p_b <- stats::pchisq(chi_b, kb - 1, lower.tail = FALSE)
  g11 <- function(x) st_pad(st_stata_g(x, 11L), 10L)
  aov_lines <- c(
    "                        Analysis of Variance",
    "    Source              SS         df      MS            F     Prob > F",
    strrep("-", 72L),
    paste0("Between groups", st_pad(st_stata_g(ssb, 11L), 16L), formatC(dfb, width = 7L, format = "d"),
           st_pad(st_stata_g(msb, 11L), 13L), sprintf("%10.2f", Fv), sprintf("%11.4f", pF)),
    paste0(" Within groups", st_pad(st_stata_g(ssw, 11L), 16L), formatC(dfw, width = 7L, format = "d"),
           st_pad(st_stata_g(msw, 11L), 13L)),
    strrep("-", 72L),
    paste0("    Total     ", st_pad(st_stata_g(ssb + ssw, 11L), 16L), formatC(N - 1, width = 7L, format = "d"),
           st_pad(st_stata_g((ssb + ssw) / (N - 1), 11L), 13L)),
    "",
    sprintf("Bartlett's test for equal variances:  chi2(%d) = %8.4f  Prob>chi2 = %5.3f", kb - 1, chi_b, p_b))
  ylab <- st_label_or_name(data[[yn]], yn)
  glab <- st_label_or_name(data[[gn]], gn)
  parts <- list()
  if (tabulate) {
    w <- 11L
    # the label of the grouping variable over several lines, as Stata
    words <- st_wrap_heading(glab, w)
    ttl <- paste("Summary of", ylab)
    sd_all <- stats::sd(yv)
    gw <- max(w, st_width(levels(gv)))
    hl <- c(rep("", length(words) - 1L), "")
    ctr <- paste0(strrep(" ", max(0L, ceiling((36 - nchar(ttl)) / 2))), ttl)
    top <- c(if (length(words) > 1L) paste0(st_pad(words[1:(length(words) - 1L)], gw), " |", c(ctr, rep("", length(words) - 2L))),
             paste0(st_pad(words[length(words)], gw), " |        Mean   Std. Dev.       Freq."))
    if (length(words) == 1L) top <- c(paste0(strrep(" ", gw), " |", ctr), top)
    rowf <- function(lab, mm, ss, ff) paste0(st_pad(lab, gw), " |", st_pad(st_stata_g(mm, 10L), 12L),
                                             st_pad(st_stata_g(ss, 10L), 12L), st_pad(formatC(ff, format = "d", big.mark = ","), 12L))
    rule <- paste0(strrep("-", gw + 1L), "+", strrep("-", 36L))
    tab <- c(top, rule, mapply(rowf, levels(gv), m, sqrt(v), n), rule, rowf("Total", grand, sd_all, N))
    parts <- c(parts, list(st_part_text(st_rtrim(tab), mono = TRUE)))
  }
  parts <- c(parts, list(st_part_text(aov_lines, mono = TRUE)))
  for (meth in c("bonferroni", "scheffe", "sidak")[c(bonferroni, scheffe, sidak)]) {
    parts <- c(parts, list(st_part_text(st_oneway_mc(meth, m, n, msw, dfw, ylab, glab, levels(gv)), mono = TRUE)))
  }
  tidy <- tibble::tibble(F = Fv, df_m = dfb, df_r = dfw, p = pF, chi2_bartlett = chi_b, p_bartlett = p_b)
  res <- st_new_output(tidy, "st_test", st_spec = list(parts = parts, cmd = sprintf("oneway %s %s", yn, gn)))
  st_export_if(res, export)
}

# Table of multiple comparisons below oneway: differences of the means (row
# minus column) and adjusted p-values, at most six columns per block.
st_oneway_mc <- function(meth, m, n, msw, dfw, ylab, glab, lev) {
  k <- length(m)
  npair <- k * (k - 1) / 2
  title <- sprintf("Comparison of %s by %s", ylab, glab)
  name <- switch(meth, bonferroni = "(Bonferroni)", scheffe = "(Scheffe)", sidak = "(Sidak)")
  # the title is centred on 78 columns, the name of the method on 77
  cen <- function(s, w) paste0(strrep(" ", max(0L, (w - nchar(s)) %/% 2L)), s)
  out <- c(cen(title, 78L), cen(name, 77L))
  # value labels are cut to 8 characters
  lab <- function(s) st_pad(substr(s, 1L, 8L), 8L)
  cols <- seq_len(k - 1L)
  blocks <- split(cols, ceiling(seq_along(cols) / 6L))
  for (bl in blocks) {
    out <- c(out, "Row Mean-|", paste0("Col Mean |", paste0(st_pad(vapply(lev[bl], lab, ""), 11L), collapse = "")),
             paste0(strrep("-", 9L), "+", strrep("-", 11L * length(bl))))
    for (i in seq(min(bl) + 1L, k)) {
      js <- bl[bl < i]
      if (!length(js)) next
      d <- m[i] - m[js]
      se <- sqrt(msw * (1 / n[i] + 1 / n[js]))
      t <- d / se
      p <- switch(meth,
        bonferroni = pmin(1, 2 * stats::pt(-abs(t), dfw) * npair),
        sidak = pmin(1, 1 - (1 - 2 * stats::pt(-abs(t), dfw))^npair),
        scheffe = stats::pf(t^2 / (k - 1), k - 1, dfw, lower.tail = FALSE))
      out <- c(out, paste0(lab(lev[i]), " |", paste0(st_pad(st_stata_g(d, 8L), 11L), collapse = "")),
               paste0("         |", paste0(st_pad(sprintf("%.3f", p), 11L), collapse = "")))
      if (i < k) out <- c(out, "         |")
    }
  }
  out
}

#' Analysis of variance and covariance (Stata: `anova`)
#'
#' Re-implementation of Stata's `anova` with its output. The number of
#' observations, R-squared and root MSE, and the table with the partial sum
#' of squares, degrees of freedom, mean square and F test of the model and
#' of every term. As in Stata, the regressors are factors. Variables named in
#' `continuous` enter as continuous covariates (analysis of covariance).
#' Interactions are written as `"a#b"` or `"a##b"` (with the main effects).
#'
#' As in Stata, the partial sums of squares are Type III sums of squares
#' (each term tested after all other terms, with sum-to-zero coding of the
#' factors). In designs with empty cells the estimable interaction terms are
#' chosen in Stata's order. In balanced designs they equal the sequential sums
#' of squares of [stats::anova()].
#'
#' @param .data A data frame.
#' @param y The response variable (unquoted).
#' @param ... Factors and interactions (unquoted names or strings such as
#'   `"a##b"`).
#' @param continuous Variables that enter as continuous covariates.
#' @param export Optional file name ending in `.html` or `.docx`.
#' @return A tibble of class `st_test` with one row per source and the columns
#'   `source`, `ss`, `df`, `ms`, `F` and `p`. It prints in Stata's layout. The
#'   regression behind it is available with [s.regress()].
#' @examples
#' s.anova(mtcars, mpg, cyl, am)
#' s.anova(mtcars, mpg, "cyl##am")
#' s.anova(mtcars, mpg, am, wt, continuous = wt)
#' @export
s.anova <- function(.data, y, ..., continuous = NULL, export = NULL) {
  st_check_data(.data)
  data <- dplyr::ungroup(.data)
  yq <- rlang::enquo(y)
  y_name <- st_select1(yq, data, "y")
  items <- st_fv_items(rlang::enquos(...), data, rlang::caller_env(), y_name)
  vars <- unique(unlist(lapply(items, function(it) if (is.character(it)) it else it$vars)))
  cont <- st_select(rlang::enquo(continuous), data)
  if (length(setdiff(cont, vars))) rlang::abort("Variables in `continuous` must also be in the model.")
  cats <- setdiff(vars, cont)
  for (v in cont) if (is.factor(data[[v]]) || is.character(data[[v]])) {
    rlang::abort(sprintf("`%s` is categorical and cannot be continuous.", v))
  }
  m <- rlang::inject(s.regress(data, !!rlang::sym(y_name), ..., categorical = tidyselect::all_of(!!cats)))
  fit <- attr(m, "st_model", exact = TRUE)[[1]]
  spec <- attr(m, "st_spec", exact = TRUE)
  s <- fit$stats
  msr <- s$rss / s$df_r
  D <- fit$D
  ok <- fit$ok
  pos <- cumsum(ok)
  # partial SS of every term, as Stata: effect coding of the factors (the
  # last category -1), the columns of a term removed from the full model
  E <- st_effect_design(D, spec$terms)
  rss_of <- function(M) {
    q <- qr(M * sqrt(D$w), tol = 1e-9)
    sum(qr.resid(q, D$y * sqrt(D$w))^2)
  }
  rows <- list()
  for (ti in seq_along(spec$terms)) {
    ct <- E$term == ti
    if (!any(ct)) next
    ss <- rss_of(E$X[, !ct, drop = FALSE]) - s$rss
    rows[[length(rows) + 1L]] <- list(source = st_term_label(spec$terms[[ti]], spec$is_cat), ss = ss, df = sum(ct))
  }
  src <- vapply(rows, `[[`, character(1), "source")
  W <- max(10L, st_width(src)) + 1L
  fmt <- function(v) st_pad(st_stata_g(v, 10L), 10L)
  line <- function(lab, ss, df, ms, Fv = NA, p = NA) {
    paste0(st_pad(lab, 24L), " |", " ", fmt(ss), formatC(df, width = 11L, format = "d", big.mark = ","),
           " ", st_pad(st_stata_g(ms, 10L), 11L),
           if (is.na(Fv)) "  " else paste0(sprintf("%10.2f", Fv), sprintf("%8.4f", p)))
  }
  pF <- function(Fv, df) stats::pf(Fv, df, s$df_r, lower.tail = FALSE)
  n_txt <- formatC(s$N, format = "d", big.mark = ",")
  hdr <- c(paste0(strrep(" ", 25L), "Number of obs = ", st_pad(n_txt, 10L), "    R-squared     =  ", sprintf("%6.4f", s$r2)),
           paste0(strrep(" ", 25L), "Root MSE      = ", st_pad(st_stata_g(s$rmse, 8L), 10L), "    Adj R-squared =  ", sprintf("%6.4f", s$r2_a)),
           "",
           paste0(st_pad("Source", 24L), " | Partial SS         df         MS        F    Prob>F"),
           paste0(strrep(" ", 25L - W), strrep("-", W), "+", strrep("-", 52L)))
  body <- c(line("Model", s$mss, s$df_m, s$mss / s$df_m, s$F, s$p), paste0(strrep(" ", 24L), " |"))
  for (r in rows) {
    Fv <- (r$ss / r$df) / msr
    body <- c(body, line(r$source, r$ss, r$df, r$ss / r$df, Fv, pF(Fv, r$df)))
  }
  tot_df <- s$df_m + s$df_r
  body <- c(body, paste0(strrep(" ", 24L), " |"), line("Residual", s$rss, s$df_r, msr),
            paste0(strrep(" ", 25L - W), strrep("-", W), "+", strrep("-", 52L)),
            line("Total", s$mss + s$rss, tot_df, (s$mss + s$rss) / tot_df))
  tidy <- tibble::tibble(
    source = c("Model", src, "Residual", "Total"),
    ss = c(s$mss, vapply(rows, `[[`, numeric(1), "ss"), s$rss, s$mss + s$rss),
    df = c(s$df_m, vapply(rows, `[[`, numeric(1), "df"), s$df_r, tot_df))
  tidy$ms <- tidy$ss / tidy$df
  tidy$F <- c(s$F, (tidy$ms[seq_along(rows) + 1L]) / msr, NA, NA)
  tidy$p <- c(s$p, pF(tidy$F[seq_along(rows) + 1L], tidy$df[seq_along(rows) + 1L]), NA, NA)
  parts <- list(st_part_text(st_rtrim(c(hdr, body)), mono = TRUE))
  inter <- st_inter_notes(spec$terms, spec$is_cat, spec$dates)
  if (length(inter)) parts <- c(parts, list(st_part_text(inter, mono = TRUE)))
  res <- st_new_output(tidy, "st_test", st_spec = list(parts = parts, cmd = sub("^regress", "anova", spec$cmd)))
  st_export_if(res, export)
}

# Design with effect-coded factors (columns: category c minus the last
# category) for the partial sums of squares of s.anova(); interaction columns
# are products. Columns of categories with empty cells come last within
# their term and collinear columns are dropped in order, as Stata does.
st_effect_design <- function(D, terms) {
  info <- D$info
  cols <- list(rep(1, D$n))
  term <- 0L
  for (ti in seq_along(terms)) {
    t <- terms[[ti]]
    cats <- t[vapply(t, function(v) info[[v]]$cat, logical(1))]
    conts <- setdiff(t, cats)
    cv <- if (length(conts)) Reduce(`*`, lapply(conts, function(v) D$vals[[v]])) else rep(1, D$n)
    grids <- lapply(cats, function(v) seq_len(length(info[[v]]$levels) - 1L))
    g <- if (length(cats)) as.matrix(expand.grid(grids, KEEP.OUT.ATTRS = FALSE)) else matrix(integer(), 1L, 0L)
    # categories that occur in empty cells of this term
    late <- rep(FALSE, nrow(g))
    if (length(cats) > 1L) {
      lev <- vapply(cats, function(v) max.col(D$vals[[v]], ties.method = "first"), integer(D$n))
      full <- as.matrix(expand.grid(lapply(cats, function(v) seq_along(info[[v]]$levels)), KEEP.OUT.ATTRS = FALSE))
      key <- apply(lev, 1, paste, collapse = "_")
      empty <- full[!apply(full, 1, paste, collapse = "_") %in% key, , drop = FALSE]
      for (r in seq_len(nrow(g))) late[r] <- any(vapply(seq_along(cats), function(k) g[r, k] %in% empty[, k], logical(1)))
    }
    ord <- c(which(!late), which(late))
    for (r in ord) {
      v <- cv
      for (k in seq_along(cats)) {
        M <- D$vals[[cats[k]]]
        v <- v * (M[, g[r, k]] - M[, ncol(M)])
      }
      cols[[length(cols) + 1L]] <- v
      term <- c(term, ti)
    }
  }
  X <- matrix(unlist(cols), nrow = D$n)
  q <- qr(X * sqrt(D$w), tol = 1e-9)
  keep <- sort(q$pivot[seq_len(q$rank)])
  list(X = X[, keep, drop = FALSE], term = term[keep])
}

#' Cronbach's alpha (Stata: `alpha`)
#'
#' Re-implementation of Stata's `alpha` with its output. The reliability
#' coefficient (Cronbach's alpha) of the scale formed by the mean of the
#' items, the average interitem covariance (or correlation with
#' `std = TRUE`) and, with `item = TRUE`, the item-test and item-rest
#' correlations and the alpha of the scale without each item.
#'
#' As in Stata, items that are negatively related to the scale are reversed
#' automatically (their sign is shown as `-`), unless `asis = TRUE`. The
#' direction is taken from the first principal factor of the items.
#' Observations with missing values in some items are used for the items
#' they have (pairwise), unless `casewise = TRUE`.
#'
#' @param .data A data frame.
#' @param ... The items (tidyselect).
#' @param item Show the statistics of every item.
#' @param std Standardize the items (alpha of the standardized items).
#' @param asis Take the items as they are, without reversing any.
#' @param casewise Use only observations without missing values in any item.
#' @param export Optional file name ending in `.html` or `.docx`.
#' @return A tibble of class `st_test` with one row per item (with `item =
#'   TRUE`) and the scale: `item`, `obs`, `sign`, `item_test`, `item_rest`,
#'   `interitem` and `alpha`. It prints in Stata's layout.
#' @examples
#' s.alpha(mtcars, mpg, disp, hp, wt)
#' s.alpha(mtcars, mpg, disp, hp, wt, item = TRUE)
#' @export
s.alpha <- function(.data, ..., item = FALSE, std = FALSE, asis = FALSE, casewise = FALSE, export = NULL) {
  st_check_data(.data)
  data <- dplyr::ungroup(.data)
  vars <- names(tidyselect::eval_select(rlang::expr(c(...)), data))
  if (length(vars) < 2L) rlang::abort("Please give at least two items.")
  X <- vapply(vars, function(v) st_num(data[[v]]), numeric(nrow(data)))
  X <- matrix(X, ncol = length(vars), dimnames = list(NULL, vars))
  nobs <- rowSums(!is.na(X))
  keep <- if (casewise) nobs == ncol(X) else nobs > 0
  X <- X[keep, , drop = FALSE]
  if (!nrow(X)) rlang::abort("No observations.")
  const <- apply(X, 2, function(z) { z <- z[!is.na(z)]; length(unique(z)) < 2L })
  notes <- character()
  if (any(const)) {
    notes <- paste(paste(vars[const], collapse = " "), "constant in analysis sample, dropped from analysis")
    X <- X[, !const, drop = FALSE]
    vars <- vars[!const]
  }
  k <- ncol(X)
  if (k < 2L) rlang::abort("Fewer than two items vary.")
  # signs: from the first principal factor of the items (complete cases)
  sgn <- rep(1, k)
  if (!asis) {
    cc <- stats::complete.cases(X)
    if (sum(cc) < 3L) rlang::abort("Cannot determine the sense empirically; use `asis = TRUE`.")
    R <- stats::cor(X[cc, , drop = FALSE])
    load <- eigen(R, symmetric = TRUE)$vectors[, 1]
    if (sum(load) < 0) load <- -load
    sgn <- ifelse(load < 0, -1, 1)
  }
  complete <- all(!is.na(X))
  Obs <- matrix(0, k, k)
  C <- matrix(0, k, k)
  for (i in seq_len(k)) for (j in seq_len(k)) {
    ok <- !is.na(X[, i]) & !is.na(X[, j])
    Obs[i, j] <- sum(ok)
    C[i, j] <- if (std) stats::cor(X[ok, i], X[ok, j]) else stats::cov(X[ok, i], X[ok, j])
    if (i != j) C[i, j] <- sgn[i] * sgn[j] * C[i, j]
  }
  if (std && complete) C <- stats::cov2cor(C)
  mu <- colMeans(X, na.rm = TRUE)
  sdv <- apply(X, 2, stats::sd, na.rm = TRUE)
  cnt <- rowSums(!is.na(X))
  Z <- if (std) sweep(sweep(X, 2, mu), 2, sdv, "/") else X
  Z <- sweep(Z, 2, sgn, "*")
  test <- rowSums(Z, na.rm = TRUE) / cnt
  if (std) {
    avar <- 1
    nvar <- 1
  } else {
    avar <- sum(diag(Obs) * diag(C))
    nvar <- sum(diag(Obs))
  }
  lo <- lower.tri(C)
  acor <- sum(Obs[lo] * C[lo])
  ncor <- sum(Obs[lo])
  acorr <- acor / ncor
  alpha <- (k * acorr) / (avar / nvar + (k - 1) * acorr)
  if (alpha < 0) alpha <- NA_real_
  lab <- if (std) "correlation" else "covariance"
  gfmt <- function(v) if (std) sprintf("%9.4f", v) else st_pad(st_stata_g(v, 9L), 9L)
  lines <- c(notes, "", sprintf("Test scale = mean(%sstandardized items)", if (std) "" else "un"))
  tidy <- tibble::tibble(item = "Test scale", obs = NA_real_, sign = NA_character_, item_test = NA_real_,
                         item_rest = NA_real_, interitem = acorr, alpha = alpha)
  if (!item || k == 2L) {
    if (any(sgn < 0)) {
      rev <- vars[sgn < 0]
      lines <- c(lines, sprintf("Reversed %s:  %s", if (length(rev) > 1L) "items" else "item", paste(rev, collapse = " ")))
    }
    lines <- c(lines, "",
               st_cols(1L, sprintf("Average interitem %s:", lab), 34L, gfmt(acorr)),
               st_cols(1L, "Number of items in the scale:", 34L, sprintf("%9.0f", k)),
               st_cols(1L, "Scale reliability coefficient:", 34L, if (is.na(alpha)) "        ." else sprintf("%9.4f", alpha)))
  } else {
    rows <- character()
    it <- list()
    for (i in seq_len(k)) {
      oth <- setdiff(seq_len(k), i)
      ac <- acor - sum(Obs[i, oth] * C[i, oth])
      nc <- ncor - sum(Obs[i, oth])
      if (std) {
        av <- 1
        nv <- 1
      } else {
        av <- avar - Obs[i, i] * C[i, i]
        nv <- nvar - Obs[i, i]
      }
      mi <- ac / nc
      al <- ((k - 1) * mi) / (av / nv + (k - 2) * mi)
      if (al < 0) al <- NA_real_
      ok <- !is.na(X[, i])
      itc <- sgn[i] * stats::cor(X[ok, i], test[ok])
      rest <- (cnt * test - Z[, i]) / (cnt - 1)
      ok2 <- ok & cnt > 1
      irc <- sgn[i] * stats::cor(X[ok2, i], rest[ok2])
      rows <- c(rows, st_cols(1L, st_abbrev_name(vars[i], 12L), 14L, "|", 16L, sprintf("%4.0f", Obs[i, i]),
                              24L, if (sgn[i] > 0) "+" else "-", 31L, sprintf("%7.4f", itc),
                              45L, sprintf("%7.4f", irc), 59L, gfmt(mi), 73L,
                              if (is.na(al)) "      ." else sprintf("%7.4f", al)))
      it[[i]] <- tibble::tibble(item = vars[i], obs = Obs[i, i], sign = if (sgn[i] > 0) "+" else "-",
                                item_test = itc, item_rest = irc, interitem = mi, alpha = al)
    }
    lines <- c(lines, "",
               "                                                            average",
               "                             item-test     item-rest       interitem",
               sprintf("Item         |  Obs  Sign   correlation   correlation     %s     alpha",
                       if (std) "correlation" else "covariance "),
               paste0(strrep("-", 13L), "+", strrep("-", 65L)), rows,
               paste0(strrep("-", 13L), "+", strrep("-", 65L)),
               st_cols(1L, "Test scale", 14L, "|", 59L, gfmt(acorr), 73L, if (is.na(alpha)) "      ." else sprintf("%7.4f", alpha)),
               strrep("-", 79L))
    tidy <- dplyr::bind_rows(c(it, list(tidy)))
  }
  res <- st_new_output(tidy, "st_test", st_spec = list(parts = list(st_part_text(st_rtrim(lines), mono = TRUE)),
                                                      cmd = paste("alpha", paste(vars, collapse = " "))))
  st_export_if(res, export)
}
