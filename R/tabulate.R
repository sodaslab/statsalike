#' One- and two-way tables of frequencies (Stata: `tabulate`)
#'
#' Re-implementation of Stata's `tabulate` (short form `tab`), covering
#' `tabulate oneway`, `tabulate twoway` and `tabulate, summarize()`.
#' Options have the same names as in Stata and are set as logical arguments,
#' e.g. `row = TRUE` instead of `, row`.
#'
#' * Missing values are shown as categories by default, i.e. `s.tab(df, x)`
#'   corresponds to Stata's `tabulate x, missing`. `missing = FALSE` gives
#'   Stata's default.
#' * Weights. `[fweight = w]` becomes `wt = w` (together with `weight_type`).
#' * `by x:` becomes `group_by(x)` before the call.
#' * `if`/`in` are expressed with `dplyr::filter()` or `dplyr::slice()`.
#' * `generate()` is provided as a separate function, [s.tabgen()], because
#'   tidyverse functions do not modify the data as a side effect.
#'
#' @section Addons beyond Stata:
#' `entropy = TRUE` is an extension of statsalike and has no counterpart in
#' Stata's `tabulate`. The printed and exported output shows the entropy
#' values without marking them as an addon, so results that use this option
#' cannot be reproduced with Stata itself.
#'
#' @param .data A data frame or tibble (may be grouped with [dplyr::group_by()]).
#' @param var1 Row variable (unquoted).
#' @param var2 Optional column variable. Produces a two-way table.
#' @param wt Optional weighting variable or expression.
#' @param weight_type Type of weights, as in Stata. `"fweight"` (frequency
#'   weights, must be integers), `"aweight"` (analytic weights, rescaled to
#'   sum to the number of observations) or `"iweight"` (importance weights).
#'   See [statsalike-weights].
#' @param subpop Expression or variable. Observations for which it is 0,
#'   `FALSE` or missing are excluded (Stata's `subpop()`).
#' @param missing Treat missing values as categories. **Unlike Stata, this is
#'   the default** (since statsalike 0.1.2), so that tables always show how
#'   many observations are missing. `missing = FALSE` excludes them, which is
#'   Stata's default. Missing categories count towards percentages, totals and
#'   test statistics, as with Stata's option `missing`. For percentages and
#'   tests based on valid observations only, use `missing = FALSE`. Missing
#'   values are `NA`, empty strings `""` (Stata's string missing value),
#'   Stata's extended missing values `.a`-`.z` and SPSS user-defined missing
#'   values (both as imported by haven). They are shown as R prints them,
#'   `<NA>` and `NA(a)`, ..., or with their value label. The category columns
#'   of the result contain a genuine `NA`, so `is.na()` and `dplyr::filter()`
#'   work as usual.
#' @param nolabel Show numeric codes instead of value labels (haven labels or
#'   factor codes).
#' @param sort Sort a one-way table in descending order of frequency.
#' @param plot Show a one-way table as a bar chart made of asterisks.
#' @param nofreq Do not display frequencies.
#' @param entropy *Addon, not an option of Stata's `tabulate`.* For one-way
#'   tables, report the (absolute) Shannon entropy
#'   \eqn{H = -\sum_i p_i \ln p_i}{H = -sum(p * log(p))} of the distribution
#'   and the relative entropy \eqn{H / \ln k}{H / log(k)}, where `k` is the
#'   number of categories in the table. The relative entropy ranges from 0
#'   (all observations in one category) to 1 (all categories equally
#'   frequent) and does not depend on the base of the logarithm. Weights and
#'   `missing = TRUE` are taken into account. The values are also returned by
#'   [s.tests()].
#' @param row,column,cell Display row, column or cell percentages.
#' @param expected Display expected frequencies.
#' @param chi2 Pearson's chi-squared test.
#' @param lrchi2 Likelihood-ratio chi-squared test.
#' @param exact Fisher's exact test. For large r x c tables, where the exact
#'   computation would take too long, a Monte Carlo p-value with 100,000
#'   replications is reported as "Fisher's exact (simulated)". It uses R's
#'   random number generator, so call [set.seed()] first for reproducible
#'   results.
#' @param gamma Goodman and Kruskal's gamma (with asymptotic standard error).
#' @param taub Kendall's tau-b (with asymptotic standard error).
#' @param V Cramér's V.
#' @param cchi2 Contribution of each cell to Pearson's chi-squared.
#' @param clrchi2 Contribution of each cell to the likelihood-ratio chi-squared.
#' @param all Shorthand for `chi2`, `lrchi2`, `V`, `gamma` and `taub`.
#' @param nokey Do not display the key (legend).
#' @param summarize Numeric variable for which the mean, standard deviation
#'   and frequency are reported in each category (Stata's
#'   `tabulate x, summarize(y)`).
#' @param nomeans,nostandard With `summarize`, do not display means or
#'   standard deviations.
#' @param obs With `summarize`, also display the number of observations.
#' @param export Optional file name ending in `.html` or `.docx`. The table
#'   is also exported to this file, laid out as printed (see [s.export()]).
#'
#' @return A tibble of class `st_tabulate` in tidy format that prints in
#'   Stata's layout.
#'   * One-way tables have one row per category with `freq`, `percent`, `cum`.
#'   * Two-way tables have one row per cell with `freq`, `expected`, `row_pct`,
#'     `col_pct`, `cell_pct`, `cchi2`, `clrchi2`.
#'   * With `summarize` the columns are `mean`, `sd`, `freq` and `obs` per category or cell.
#'
#'   Test statistics (and the entropy of one-way tables) are returned by
#'   [s.tests()]. For grouped data the
#'   grouping variables come first.
#'
#' @examples
#' df <- tibble::as_tibble(mtcars)
#'
#' # tabulate cyl
#' s.tab(df, cyl)
#'
#' # tabulate cyl, sort plot
#' s.tab(df, cyl, sort = TRUE, plot = TRUE)
#'
#' # tabulate cyl am, row chi2 exact
#' s.tab(df, cyl, am, row = TRUE, chi2 = TRUE, exact = TRUE)
#'
#' # tabulate cyl, summarize(mpg)
#' s.tab(df, cyl, summarize = mpg)
#'
#' # by am: tabulate gear
#' df |> dplyr::group_by(am) |> s.tab(gear)
#'
#' # the result can be processed like any other tibble
#' s.tab(df, cyl, am) |> dplyr::filter(freq > 2)
#'
#' # missing values are shown as a category; missing = FALSE excludes them
#' s.tab(tibble::tibble(x = c("a", "b", NA, "b")), x)
#' s.tab(tibble::tibble(x = c("a", "b", NA, "b")), x, missing = FALSE)
#'
#' # export to Word or HTML
#' s.tab(df, cyl, am, row = TRUE, export = tempfile(fileext = ".html"))
#' @export
s.tabulate <- function(.data, var1, var2 = NULL, wt = NULL,
                       weight_type = c("fweight", "aweight", "iweight"),
                       subpop = NULL, missing = TRUE, nolabel = FALSE,
                       sort = FALSE, plot = FALSE, nofreq = FALSE,
                       entropy = FALSE,
                       row = FALSE, column = FALSE, cell = FALSE,
                       expected = FALSE, chi2 = FALSE, lrchi2 = FALSE,
                       exact = FALSE, gamma = FALSE, taub = FALSE,
                       V = FALSE, cchi2 = FALSE, clrchi2 = FALSE,
                       all = FALSE, nokey = FALSE, summarize = NULL,
                       nomeans = FALSE, nostandard = FALSE, obs = FALSE,
                       export = NULL) {
  st_check_data(.data)
  weight_type <- match.arg(weight_type)
  grp <- st_groups(.data)
  data <- dplyr::ungroup(.data)

  x_name <- st_select1(rlang::enquo(var1), data, "var1")
  y_quo <- rlang::enquo(var2)
  twoway <- !rlang::quo_is_null(y_quo)
  y_name <- if (twoway) st_select1(y_quo, data, "var2")
  s_quo <- rlang::enquo(summarize)
  s_name <- if (!rlang::quo_is_null(s_quo)) st_select1(s_quo, data, "summarize")
  wt_quo <- rlang::enquo(wt)

  if (twoway && identical(x_name, y_name)) {
    rlang::abort("`var1` and `var2` must be different variables.")
  }
  if (any(c(x_name, y_name) %in% names(grp$keys))) {
    rlang::abort("A table variable cannot also be a grouping variable.")
  }

  # Check option combinations as Stata does
  if (all) chi2 <- lrchi2 <- gamma <- taub <- V <- TRUE
  stat_opts <- c(chi2 = chi2, lrchi2 = lrchi2, exact = exact, gamma = gamma,
                 taub = taub, V = V)
  cell_opts <- c(row = row, column = column, cell = cell, expected = expected,
                 cchi2 = cchi2, clrchi2 = clrchi2)
  two_only <- c(stat_opts, cell_opts, nokey = nokey)
  if (!twoway && any(two_only)) {
    rlang::abort(paste0("Option(s) ", paste(names(which(two_only)), collapse = ", "),
                        " only allowed with two-way tables (`var2`)."))
  }
  if (twoway && (sort || plot || entropy)) {
    rlang::abort("`sort`, `plot` and `entropy` are only allowed with one-way tables.")
  }
  if (!is.null(s_name)) {
    if (any(stat_opts) || any(cell_opts) || sort || plot || entropy) {
      rlang::abort("`summarize` cannot be combined with tests, percentages, `sort`, `plot` or `entropy`.")
    }
    if (nomeans && nostandard && nofreq && !obs) {
      rlang::abort("With `summarize`, at least one statistic must be displayed.")
    }
  } else if (nomeans || nostandard || obs) {
    rlang::abort("`nomeans`, `nostandard` and `obs` are only allowed with `summarize`.")
  }
  if (!rlang::quo_is_null(wt_quo) && weight_type != "fweight" && any(stat_opts)) {
    rlang::abort("As in Stata, test statistics are only allowed unweighted or with fweights.")
  }

  w <- st_eval_weights(wt_quo, data, weight_type) %||% rep(1, nrow(data))
  sp_quo <- rlang::enquo(subpop)
  if (!rlang::quo_is_null(sp_quo)) {
    s <- st_strip(rlang::eval_tidy(sp_quo, data))
    if (length(s) == 1L) s <- rep(s, nrow(data))
    w[is.na(s) | s == 0] <- NA
  }

  fx <- st_as_levels(data[[x_name]], nolabel = nolabel, missing = missing)
  fy <- if (twoway) st_as_levels(data[[y_name]], nolabel = nolabel, missing = missing)
  sv <- NULL
  if (!is.null(s_name)) {
    sv <- st_strip(data[[s_name]])
    if (!(is.numeric(sv) || is.logical(sv))) {
      rlang::abort("`summarize` requires a numeric variable.")
    }
    sv <- st_num(data[[s_name]])
  }

  want <- as.list(stat_opts)
  models <- list()
  pieces <- list()
  tests <- list()
  for (g in seq_along(grp$rows)) {
    idx <- grp$rows[[g]]
    if (!length(idx) && !is.null(grp$keys)) next
    m <- if (!is.null(s_name)) {
      st_model_sum(fx[idx], if (twoway) fy[idx], sv[idx], w[idx], weight_type)
    } else if (twoway) {
      st_model_twoway(fx[idx], fy[idx], w[idx], weight_type, want)
    } else {
      st_model_oneway(fx[idx], w[idx], weight_type, sort, entropy)
    }
    m$label <- grp$labels[g]
    models[[length(models) + 1L]] <- m

    df <- m$data
    names(df)[1] <- x_name
    if (twoway) names(df)[2] <- y_name
    if (!is.null(grp$keys)) {
      df <- dplyr::bind_cols(grp$keys[rep(g, nrow(df)), , drop = FALSE], df)
    }
    pieces[[length(pieces) + 1L]] <- df
    if (!is.null(m$tests) && nrow(m$tests)) {
      t <- m$tests
      if (!is.null(grp$keys)) {
        t <- dplyr::bind_cols(grp$keys[rep(g, nrow(t)), , drop = FALSE], t)
      }
      tests[[length(tests) + 1L]] <- t
    }
  }
  res <- dplyr::bind_rows(pieces)
  # With groups, restore Stata's ordering of the categories
  if (!is.null(grp$keys)) {
    res[[x_name]] <- factor(as.character(res[[x_name]]), levels = levels(fx))
    if (twoway) res[[y_name]] <- factor(as.character(res[[y_name]]), levels = levels(fy))
  }

  spec <- list(
    x = x_name, y = y_name, summarize = s_name,
    sum_label = if (!is.null(s_name)) st_label_or_name(data[[s_name]], s_name),
    plot = plot, nofreq = nofreq, nokey = nokey,
    show = list(freq = !nofreq, expected = expected, cchi2 = cchi2,
                clrchi2 = clrchi2, row = row, column = column, cell = cell),
    means = !nomeans, standard = !nostandard, freq = !nofreq, obs = obs
  )
  flags <- c(missing = missing, nolabel = nolabel, sort = sort, plot = plot,
             nofreq = nofreq, entropy = entropy, row = row, column = column, cell = cell,
             expected = expected, chi2 = chi2, lrchi2 = lrchi2, exact = exact,
             gamma = gamma, taub = taub, V = V, cchi2 = cchi2,
             clrchi2 = clrchi2, nokey = nokey, nomeans = nomeans,
             nostandard = nostandard, obs = obs)
  spec$cmd <- st_make_cmd("tabulate", c(x_name, y_name),
                          weight = st_weight_label(wt_quo, weight_type),
                          opts = c(if (!is.null(s_name)) paste0("summarize(", s_name, ")"),
                                   names(flags)[flags]),
                          by = names(grp$keys))
  res <- st_new_output(st_na_to_r(res, c(x_name, y_name)), "st_tabulate", st_model = models, st_spec = spec,
                       st_tests = if (length(tests)) dplyr::bind_rows(tests))
  st_export_if(res, export)
}

#' @rdname s.tabulate
#' @export
s.tab <- s.tabulate

#' Retrieve the test statistics of a two-way table
#'
#' Returns the statistics requested with `chi2`, `lrchi2`, `exact`, `gamma`,
#' `taub` or `V` from an [s.tabulate()] result as a tibble (the equivalent of
#' the results Stata stores in `r()`). For one-way tables with
#' `entropy = TRUE` (a statsalike addon), it returns the absolute and relative
#' entropy.
#'
#' @param x Result of [s.tabulate()].
#' @return A tibble with the columns `test`, `statistic`, `df`, `p` and `ase`
#'   (plus the grouping variables for grouped data).
#' @examples
#' tab <- s.tab(mtcars, cyl, am, chi2 = TRUE, V = TRUE)
#' s.tests(tab)
#'
#' s.tests(s.tab(mtcars, cyl, entropy = TRUE))
#' @export
s.tests <- function(x) {
  t <- attr(x, "st_tests", exact = TRUE)
  if (is.null(t)) {
    tibble::tibble(test = character(), statistic = numeric(), df = numeric(),
                   p = numeric(), ase = numeric())
  } else {
    t
  }
}

# Models (computation per group) ---------------------------------------------

st_model_oneway <- function(f, w, weight_type, sort, entropy = FALSE) {
  keep <- !is.na(f) & !is.na(w) & w != 0
  f <- droplevels(f[keep])
  w <- w[keep]
  if (weight_type == "aweight" && length(w)) w <- w * length(w) / sum(w)
  freq <- unname(vapply(split(w, f), sum, numeric(1)))
  lev <- levels(f)
  if (sort && length(freq)) {
    o <- order(-freq)
    freq <- freq[o]
    lev <- lev[o]
  }
  tot <- sum(freq)
  pct <- if (tot != 0) 100 * freq / tot else rep(NA_real_, length(freq))
  data <- tibble::tibble(value = factor(lev, levels = lev), freq = freq,
                         percent = pct, cum = cumsum(pct))
  list(type = "oneway", data = data, tests = if (entropy && length(freq)) st_entropy(freq))
}

# Shannon entropy (natural log) and relative entropy H / ln(k) of a
# frequency distribution; statsalike addon, not part of Stata's tabulate.
st_entropy <- function(freq) {
  k <- length(freq)
  p <- freq[freq > 0] / sum(freq)
  h <- -sum(p * log(p))
  tibble::tibble(test = c("entropy", "relative entropy"),
                 statistic = c(h, if (k > 1) h / log(k) else NA_real_),
                 df = NA_real_, p = NA_real_, ase = NA_real_)
}

st_model_twoway <- function(fx, fy, w, weight_type, want) {
  keep <- !is.na(fx) & !is.na(fy) & !is.na(w) & w != 0
  fx <- droplevels(fx[keep])
  fy <- droplevels(fy[keep])
  w <- w[keep]
  if (weight_type == "aweight" && length(w)) w <- w * length(w) / sum(w)
  r <- nlevels(fx)
  k <- nlevels(fy)
  O <- matrix(0, r, k, dimnames = list(levels(fx), levels(fy)))
  if (length(w)) {
    s <- tapply(w, list(fx, fy), sum)
    s[is.na(s)] <- 0
    O[] <- s
  }
  N <- sum(O)
  rs <- rowSums(O)
  cs <- colSums(O)
  E <- if (N != 0) outer(rs, cs) / N else O
  pct <- function(num, den) ifelse(den != 0, 100 * num / den, NA_real_)
  RP <- pct(O, matrix(rs, r, k))
  CP <- pct(O, matrix(cs, r, k, byrow = TRUE))
  TP <- pct(O, matrix(N, r, k))
  CC <- (O - E)^2 / E
  CC[!is.finite(CC)] <- 0
  LC <- 2 * O * log(O / E)
  LC[O == 0 | !is.finite(LC)] <- 0
  v <- function(M) as.vector(t(M))
  data <- tibble::tibble(
    x = factor(rep(levels(fx), each = k), levels = levels(fx)),
    y = factor(rep(levels(fy), times = r), levels = levels(fy)),
    freq = v(O), expected = v(E), row_pct = v(RP), col_pct = v(CP),
    cell_pct = v(TP), cchi2 = v(CC), clrchi2 = v(LC)
  )
  list(type = "twoway", data = data, O = O, tests = st_tab2_tests(O, want))
}

st_model_sum <- function(fx, fy, x, w, weight_type) {
  keep <- !is.na(fx) & !is.na(x) & !is.na(w) & w != 0
  if (!is.null(fy)) keep <- keep & !is.na(fy)
  fx <- droplevels(fx[keep])
  x <- x[keep]
  w <- w[keep]
  if (!is.null(fy)) fy <- droplevels(fy[keep])
  all_obs <- rep(TRUE, length(x))
  cell <- function(sel) st_summ(x[sel], w[sel], weight_type)
  lx <- levels(fx)
  stat_names <- c("mean", "sd", "freq", "obs")

  if (is.null(fy)) {
    S <- t(vapply(lx, function(l) cell(fx == l), numeric(4)))
    S <- rbind(S, Total = cell(all_obs))
    colnames(S) <- stat_names
    i <- seq_along(lx)
    data <- tibble::tibble(value = factor(lx, levels = lx),
                           mean = unname(S[i, "mean"]), sd = unname(S[i, "sd"]),
                           freq = unname(S[i, "freq"]), obs = unname(S[i, "obs"]))
    return(list(type = "sum1", data = data, S = S))
  }

  ly <- levels(fy)
  r <- length(lx)
  k <- length(ly)
  A <- array(NA_real_, c(r + 1L, k + 1L, 4L),
             dimnames = list(c(lx, "Total"), c(ly, "Total"), stat_names))
  for (i in seq_len(r + 1L)) for (j in seq_len(k + 1L)) {
    si <- if (i <= r) fx == lx[i] else all_obs
    sj <- if (j <= k) fy == ly[j] else all_obs
    A[i, j, ] <- cell(si & sj)
  }
  vv <- function(s) as.vector(t(matrix(A[seq_len(r), seq_len(k), s], r, k)))
  data <- tibble::tibble(
    x = factor(rep(lx, each = k), levels = lx),
    y = factor(rep(ly, times = r), levels = ly),
    mean = vv("mean"), sd = vv("sd"), freq = vv("freq"), obs = vv("obs")
  )
  list(type = "sum2", data = data, A = A)
}

# Output -----------------------------------------------------------------------

#' @export
print.st_tabulate <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

st_parts_tabulate <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  parts <- list()
  for (m in attr(x, "st_model", exact = TRUE)) {
    if (!is.na(m$label)) parts <- c(parts, list(st_part_heading(m$label)))
    parts <- c(parts, switch(m$type,
      oneway = st_parts_oneway(m, spec),
      twoway = st_parts_twoway(m, spec),
      sum1 = st_parts_sum1(m, spec),
      sum2 = st_parts_sum2(m, spec)
    ))
  }
  parts
}

st_parts_oneway <- function(m, spec) {
  d <- m$data
  if (!nrow(d)) return(list(st_part_text("no observations")))
  lev <- as.character(d$value)
  if (spec$plot) {
    # at most 50 stars as in Stata, fewer if the console is narrower
    width <- getOption("width", 80L)
    freq <- st_fmt_freq(d$freq)
    freqw <- max(st_width(c(freq, "Freq.")))
    # row labels may be shortened, but leave room for at least 10 stars
    stubw <- min(max(st_width(c(lev, spec$x))),
                 max(12L, floor(width * 0.45), width - freqw - 18L))
    maxbar <- max(10L, min(50L, width - stubw - freqw - 8L))
    bar <- strrep("*", pmax(round(maxbar * d$freq / max(d$freq)), 0))
    body <- cbind(freq, st_pad(bar, max(nchar(bar)), "left"))
    return(c(list(st_part_grid(stub = lev, body = body, header = c("Freq.", ""),
                               stub_header = spec$x, rules = 0L, vbars = 1L,
                               col_align = c("right", "left"))),
             st_parts_entropy(m$tests, nrow(d))))
  }
  freq <- st_fmt_freq(c(d$freq, sum(d$freq)))
  pct <- st_fmt_fixed(c(d$percent, sum(d$percent)), 2)
  cum <- c(st_fmt_fixed(d$cum, 2), "")
  if (spec$nofreq) {
    body <- cbind(pct, cum)
    hdr <- c("Percent", "Cum.")
  } else {
    body <- cbind(freq, pct, cum)
    hdr <- c("Freq.", "Percent", "Cum.")
  }
  c(list(st_part_grid(stub = c(lev, "Total"), body = body, header = hdr,
                      stub_header = spec$x, rules = c(0L, nrow(d)))),
    st_parts_entropy(m$tests, nrow(d)))
}

st_parts_entropy <- function(tests, k) {
  if (is.null(tests)) return(list())
  tl <- st_format_tests(tests)
  tl[1] <- paste0(tl[1], "   (natural log)")
  if (k > 1) tl[2] <- paste0(tl[2], sprintf("   (entropy / ln(%d))", k))
  list(st_part_text(tl, mono = TRUE))
}

# Multi-line cells (frequency, percentages, ...) as in Stata's two-way tables.
st_stack_cells <- function(items, row_labels) {
  K <- length(items)
  r1 <- nrow(items[[1]])
  body <- matrix("", r1 * K, ncol(items[[1]]))
  stub <- character(r1 * K)
  for (i in seq_len(r1)) for (k in seq_len(K)) {
    li <- (i - 1L) * K + k
    body[li, ] <- items[[k]][i, ]
    if (k == 1L) stub[li] <- row_labels[i]
  }
  rules <- if (K > 1L) c(0L, seq_len(r1 - 1L) * K) else c(0L, r1 - 1L)
  list(body = body, stub = stub, rules = rules)
}

st_parts_twoway <- function(m, spec) {
  O <- m$O
  r <- nrow(O)
  k <- ncol(O)
  out <- list()
  if (r && k) {
    N <- sum(O)
    E <- outer(rowSums(O), colSums(O)) / N
    tot <- function(M) rbind(cbind(M, rowSums(M)), c(colSums(M), sum(M)))
    fm <- function(M, f) {
      o <- f(as.vector(M))
      dim(o) <- dim(M)
      o
    }
    f1 <- function(x) st_fmt_fixed(x, 1)
    f2 <- function(x) st_fmt_fixed(x, 2)
    Ot <- tot(O)
    sh <- spec$show
    items <- list()
    labels <- character()
    add <- function(lab, M) {
      items[[length(items) + 1L]] <<- M
      labels <<- c(labels, lab)
    }
    if (sh$freq) add("frequency", fm(Ot, st_fmt_freq))
    if (sh$expected) add("expected frequency", fm(tot(E), f1))
    if (sh$cchi2) {
      cc <- (O - E)^2 / E
      cc[!is.finite(cc)] <- 0
      add("chi2 contribution", fm(tot(cc), f1))
    }
    if (sh$clrchi2) {
      lc <- 2 * O * log(O / E)
      lc[O == 0 | !is.finite(lc)] <- 0
      add("LR chi2 contribution", fm(tot(lc), f1))
    }
    if (sh$row) add("row percentage", fm(100 * Ot / Ot[, k + 1L], f2))
    if (sh$column) add("column percentage", fm(100 * sweep(Ot, 2, Ot[r + 1L, ], "/"), f2))
    if (sh$cell) add("cell percentage", fm(100 * Ot / N, f2))
    if (length(items)) {
      if (length(items) > 1L && !spec$nokey) out <- list(st_part_key(labels))
      st <- st_stack_cells(items, c(rownames(O), "Total"))
      out <- c(out, list(st_part_grid(stub = st$stub, body = st$body,
                                      header = c(colnames(O), "Total"),
                                      stub_header = spec$x, span = spec$y,
                                      rules = st$rules, vbars = k)))
    }
  } else {
    return(list(st_part_text("no observations")))
  }
  tl <- st_format_tests(m$tests)
  if (length(tl)) out <- c(out, list(st_part_text(tl, mono = TRUE)))
  out
}

st_sum_items <- function(spec) {
  c(Mean = spec$means, `Std. dev.` = spec$standard, Freq. = spec$freq,
    Obs. = spec$obs)
}

st_parts_sum1 <- function(m, spec) {
  S <- m$S
  if (nrow(S) <= 1L) return(list(st_part_text("no observations")))
  show <- st_sum_items(spec)
  cols <- list(Mean = st_fmt_g(S[, "mean"], ","),
               `Std. dev.` = st_fmt_g(S[, "sd"], ","),
               Freq. = st_fmt_freq(S[, "freq"]),
               Obs. = st_fmt_freq(S[, "obs"]))[show]
  list(st_part_grid(stub = rownames(S), body = do.call(cbind, cols),
                    header = names(cols), stub_header = spec$x,
                    span = paste("Summary of", spec$sum_label),
                    rules = c(0L, nrow(S) - 1L)))
}

st_parts_sum2 <- function(m, spec) {
  A <- m$A
  r1 <- dim(A)[1]
  k1 <- dim(A)[2]
  if (r1 <= 1L || k1 <= 1L) return(list(st_part_text("no observations")))
  show <- st_sum_items(spec)
  mat <- function(v) matrix(v, r1, k1)
  items <- list(mat(st_fmt_g(A[, , "mean"], ",")),
                mat(st_fmt_g(A[, , "sd"], ",")),
                mat(st_fmt_freq(A[, , "freq"])),
                mat(st_fmt_freq(A[, , "obs"])))[show]
  title <- c("Means", "Standard deviations", "Frequencies",
             "Number of obs")[show]
  title <- paste(st_and(title), "of", spec$sum_label)
  st <- st_stack_cells(items, dimnames(A)[[1]])
  list(st_part_text(title),
       st_part_grid(stub = st$stub, body = st$body, header = dimnames(A)[[2]],
                    stub_header = spec$x, span = spec$y, rules = st$rules,
                    vbars = k1 - 1L))
}

# tab1 / tab2 / generate -------------------------------------------------------

#' One-way tables for several variables (Stata: `tab1`)
#'
#' Creates a separate one-way table with [s.tabulate()] for each selected
#' variable.
#'
#' @inheritParams s.tabulate
#' @param ... Variables (tidyselect, e.g. `c(cyl, gear)` or `starts_with("q")`).
#' @return A list of [s.tabulate()] results (class `st_tab_list`), named after
#'   the variables.
#' @examples
#' s.tab1(mtcars, cyl, gear, am)
#'
#' # entropy of each distribution (statsalike addon)
#' s.tab1(mtcars, cyl, gear, am, entropy = TRUE)
#' @export
s.tab1 <- function(.data, ..., wt = NULL,
                   weight_type = c("fweight", "aweight", "iweight"),
                   subpop = NULL, missing = TRUE, nolabel = FALSE,
                   sort = FALSE, plot = FALSE, nofreq = FALSE, entropy = FALSE,
                   export = NULL) {
  st_check_data(.data)
  weight_type <- match.arg(weight_type)
  vars <- names(tidyselect::eval_select(rlang::expr(c(...)), dplyr::ungroup(.data)))
  if (!length(vars)) rlang::abort("Please specify at least one variable.")
  wt <- rlang::enquo(wt)
  subpop <- rlang::enquo(subpop)
  opts <- list(weight_type = weight_type, missing = missing, nolabel = nolabel,
               sort = sort, plot = plot, nofreq = nofreq, entropy = entropy)
  out <- lapply(vars, function(v) {
    rlang::inject(s.tabulate(.data, !!v, wt = !!wt, subpop = !!subpop, !!!opts))
  })
  names(out) <- vars
  flags <- c(missing = missing, nolabel = nolabel, sort = sort, plot = plot,
             nofreq = nofreq, entropy = entropy)
  cmd <- st_make_cmd("tab1", vars, weight = st_weight_label(wt, weight_type),
                     opts = names(flags)[flags], by = dplyr::group_vars(.data))
  st_export_if(structure(out, class = "st_tab_list", st_cmd = cmd), export)
}

#' All two-way tables of a variable list (Stata: `tab2`)
#'
#' Creates two-way tables for all pairs of the selected variables.
#'
#' @inheritParams s.tabulate
#' @param ... Variables (tidyselect).
#' @param firstonly Only form pairs that include the first variable.
#' @return A list of [s.tabulate()] results (class `st_tab_list`).
#' @examples
#' s.tab2(mtcars, cyl, gear, am, chi2 = TRUE)
#' s.tab2(mtcars, cyl, gear, am, firstonly = TRUE, row = TRUE)
#' @export
s.tab2 <- function(.data, ..., firstonly = FALSE, wt = NULL,
                   weight_type = c("fweight", "aweight", "iweight"),
                   subpop = NULL, missing = TRUE, nolabel = FALSE,
                   nofreq = FALSE, row = FALSE, column = FALSE,
                   cell = FALSE, expected = FALSE, chi2 = FALSE,
                   lrchi2 = FALSE, exact = FALSE, gamma = FALSE,
                   taub = FALSE, V = FALSE, cchi2 = FALSE,
                   clrchi2 = FALSE, all = FALSE, nokey = FALSE, export = NULL) {
  st_check_data(.data)
  weight_type <- match.arg(weight_type)
  vars <- names(tidyselect::eval_select(rlang::expr(c(...)), dplyr::ungroup(.data)))
  if (length(vars) < 2L) rlang::abort("Please specify at least two variables.")
  pairs <- utils::combn(vars, 2L, simplify = FALSE)
  if (firstonly) pairs <- Filter(function(p) p[1] == vars[1], pairs)
  wt <- rlang::enquo(wt)
  subpop <- rlang::enquo(subpop)
  opts <- list(weight_type = weight_type, missing = missing, nolabel = nolabel,
               nofreq = nofreq, row = row, column = column, cell = cell,
               expected = expected, chi2 = chi2, lrchi2 = lrchi2,
               exact = exact, gamma = gamma, taub = taub, V = V,
               cchi2 = cchi2, clrchi2 = clrchi2, all = all, nokey = nokey)
  out <- lapply(pairs, function(p) {
    rlang::inject(s.tabulate(.data, !!p[1], !!p[2], wt = !!wt,
                             subpop = !!subpop, !!!opts))
  })
  names(out) <- vapply(pairs, paste, character(1), collapse = " by ")
  flags <- unlist(opts[-1L])
  flags <- c(firstonly = firstonly, flags[vapply(flags, isTRUE, logical(1))])
  cmd <- st_make_cmd("tab2", vars, weight = st_weight_label(wt, weight_type),
                     opts = names(flags)[flags], by = dplyr::group_vars(.data))
  st_export_if(structure(out, class = "st_tab_list", st_cmd = cmd), export)
}

#' @export
print.st_tab_list <- function(x, ...) {
  for (i in seq_along(x)) {
    if (i > 1L) cat("\n")
    cat("-> tabulation of ", names(x)[i], "\n\n", sep = "")
    print(x[[i]])
  }
  invisible(x)
}

#' Create indicator variables for each category (Stata: `tabulate x, generate()`)
#'
#' Appends one 0/1 variable per category of `var` to the data (`prefix1`,
#' `prefix2`, ...). As in Stata, the new variables get the variable label
#' `var==value`. Observations with missing `var` get `NA` (unless
#' `missing = TRUE`).
#'
#' @inheritParams s.tabulate
#' @param missing Also create an indicator variable for missing values
#'   (default `FALSE`, as in Stata). Unlike the table functions, `s.tabgen()`
#'   keeps `NA` in the new variables by default.
#' @param var Variable whose categories are coded.
#' @param prefix Prefix of the new variables. Defaults to `"<var>_"`.
#' @return `.data` with the additional columns (grouping is kept).
#' @examples
#' s.tabgen(mtcars, cyl, prefix = "cyl") |> dplyr::select(cyl, cyl1:cyl3)
#' @export
s.tabgen <- function(.data, var, prefix = NULL, missing = FALSE,
                     nolabel = FALSE) {
  st_check_data(.data)
  data <- dplyr::ungroup(.data)
  nm <- st_select1(rlang::enquo(var), data, "var")
  prefix <- prefix %||% paste0(nm, "_")
  f <- st_as_levels(data[[nm]], nolabel = nolabel, missing = missing)
  lev <- levels(f)
  new <- lapply(lev, function(l) {
    d <- as.integer(f == l)
    attr(d, "label") <- paste0(nm, "==", l)
    d
  })
  names(new) <- paste0(prefix, seq_along(lev))
  clash <- intersect(names(new), names(data))
  if (length(clash)) {
    rlang::abort(paste0("Variable(s) already exist: ",
                        paste(clash, collapse = ", "),
                        ". Please choose a different `prefix`."))
  }
  for (v in names(new)) data[[v]] <- new[[v]]
  if (dplyr::is_grouped_df(.data)) {
    data <- dplyr::group_by(data, !!!rlang::syms(dplyr::group_vars(.data)))
  }
  data
}
