#' Summary statistics (Stata: `summarize`)
#'
#' Re-implementation of Stata's
#' `summarize [varlist] [weight] [, detail format separator(#)]`.
#' Without `detail`, one line per variable shows the number of observations,
#' mean, standard deviation, minimum and maximum. With `detail`, each variable
#' gets Stata's block with percentiles, the four smallest and largest values,
#' variance, skewness and kurtosis.
#'
#' * Variables that are not numeric (strings, factors, dates) are shown with
#'   0 observations, as in Stata.
#' * Missing values follow Stata (see [s.tabulate()]). `NA`, extended missing
#'   values and SPSS user-defined missing values are excluded.
#' * Weights. With `fweight` the observations are the sum of the weights.
#'   With `aweight` and `iweight` an additional column shows the sum of the
#'   weights. As in Stata, `detail` is not allowed with `iweight`.
#' * `by x:` becomes `group_by(x)` before the call.
#' * Stata's option `meanonly` is not needed. All statistics are returned as
#'   a tibble.
#'
#' Percentiles, variance, skewness and kurtosis use the same formulas as
#' Stata (and as [s.table()]).
#'
#' @param .data A data frame or tibble (may be grouped with [dplyr::group_by()]).
#' @param ... Variables (tidyselect). All variables by default (without the
#'   grouping variables).
#' @param wt Optional weighting variable or expression.
#' @param weight_type `"fweight"`, `"aweight"` or `"iweight"`, as in Stata
#'   (see [statsalike-weights]).
#' @param detail Show detailed statistics for each variable.
#' @param format Display the statistics in the Stata display format of each
#'   variable (`format.stata`, as imported by haven), if it exists.
#' @param separator Draw a separator line after every `separator` variables.
#'   `0` for none (Stata's `separator(#)`, default 5).
#' @param export Optional file name ending in `.html` or `.docx`. The output
#'   is also exported to this file (see [s.export()]).
#'
#' @return A tibble of class `st_summarize` with one row per variable (and
#'   group) and the columns `variable`, `obs`, `sum_w`, `mean`, `sd`, `min`, `max`, and with
#'   `detail` additionally `p1` ... `p99`, `var`, `skewness` and `kurtosis`.
#'   It prints in Stata's layout.
#'
#' @examples
#' # summarize mpg hp wt
#' s.summarize(mtcars, mpg, hp, wt)
#'
#' # summarize mpg, detail
#' s.summarize(mtcars, mpg, detail = TRUE)
#'
#' # by am: summarize mpg hp
#' mtcars |> dplyr::group_by(am) |> s.summarize(mpg, hp)
#'
#' # summarize mpg [aweight = wt]
#' s.summarize(mtcars, mpg, wt = wt, weight_type = "aweight")
#' @export
s.summarize <- function(.data, ..., wt = NULL,
                        weight_type = c("fweight", "aweight", "iweight"),
                        detail = FALSE, format = FALSE, separator = 5L,
                        export = NULL) {
  st_check_data(.data)
  weight_type <- match.arg(weight_type)
  grp <- st_groups(.data)
  data <- dplyr::ungroup(.data)
  vars <- if (...length()) {
    names(tidyselect::eval_select(rlang::expr(c(...)), data))
  } else {
    setdiff(names(data), names(grp$keys))
  }
  if (!length(vars)) rlang::abort("No variables to summarize.")
  if (!is.numeric(separator) || length(separator) != 1L || is.na(separator) || separator < 0) {
    rlang::abort("`separator` must be a single number >= 0.")
  }
  wt_quo <- rlang::enquo(wt)
  weighted <- !rlang::quo_is_null(wt_quo)
  if (weighted && detail && weight_type == "iweight") {
    rlang::abort("As in Stata, `detail` is not allowed with iweights.")
  }
  w <- st_eval_weights(wt_quo, data, weight_type) %||% rep(1, nrow(data))
  xs <- lapply(vars, function(v) if (st_is_summable(data[[v]])) st_num(data[[v]]))

  models <- list()
  pieces <- list()
  for (g in seq_along(grp$rows)) {
    idx <- grp$rows[[g]]
    if (!length(idx) && !is.null(grp$keys)) next
    res <- lapply(xs, st_summ_var, idx = idx, w = w, weight_type = weight_type,
                  weighted = weighted, detail = detail)
    stats <- do.call(rbind, lapply(res, `[[`, "stats"))
    df <- tibble::as_tibble(as.data.frame(stats))
    df <- tibble::add_column(df, variable = vars, .before = 1L)
    if (!is.null(grp$keys)) {
      df <- dplyr::bind_cols(grp$keys[rep(g, nrow(df)), , drop = FALSE], df)
    }
    pieces[[length(pieces) + 1L]] <- df
    models[[length(models) + 1L]] <- list(
      label = grp$labels[g], stats = stats,
      smallest = lapply(res, `[[`, "smallest"), largest = lapply(res, `[[`, "largest"),
      numeric = !vapply(xs, is.null, logical(1))
    )
  }

  formats <- vapply(vars, function(v) {
    f <- attr(data[[v]], "format.stata", exact = TRUE)
    if (is.null(f) || !grepl("^%-?[0-9]*\\.?[0-9]*[fge]c?$", f)) "" else as.character(f)[1]
  }, character(1))
  spec <- list(
    vars = vars,
    titles = vapply(vars, function(v) st_label_or_name(data[[v]], v), character(1)),
    formats = if (format) formats else rep("", length(vars)),
    weighted = weighted, weight_type = weight_type, detail = detail,
    separator = as.integer(separator),
    cmd = st_make_cmd("summarize", if (...length()) vars,
                      weight = st_weight_label(wt_quo, weight_type),
                      opts = c(if (detail) "detail", if (format) "format",
                               if (separator != 5) sprintf("separator(%d)", as.integer(separator))),
                      by = names(grp$keys))
  )
  res <- st_new_output(dplyr::bind_rows(pieces), "st_summarize", st_model = models, st_spec = spec)
  st_export_if(res, export)
}

#' @rdname s.summarize
#' @export
s.sum <- s.summarize

st_is_summable <- function(x) {
  !is.list(x) && !is.factor(x) && (is.numeric(st_strip(x)) || is.logical(x))
}

st_detail_p <- c(1, 5, 10, 25, 50, 75, 90, 95, 99)

# Statistics of one variable in one group.
st_summ_var <- function(x, idx, w, weight_type, weighted, detail) {
  na <- NA_real_
  names_d <- c(paste0("p", st_detail_p), "var", "skewness", "kurtosis")
  if (is.null(x)) {
    stats <- c(obs = 0, sum_w = 0, mean = na, sd = na, min = na, max = na)
    if (detail) stats <- c(stats, stats::setNames(rep(na, length(names_d)), names_d))
    return(list(stats = stats, smallest = numeric(), largest = numeric()))
  }
  xi <- x[idx]
  wi <- w[idx]
  ok <- !is.na(xi) & !is.na(wi) & wi != 0
  xi <- xi[ok]
  wi <- wi[ok]
  n <- length(xi)
  stat <- function(s) st_var_stat(s, xi, wi, weight_type)
  stats <- c(obs = if (weighted && weight_type == "fweight") sum(wi) else n,
             sum_w = sum(wi), mean = stat("mean"), sd = stat("sd"),
             min = if (n) min(xi) else na, max = if (n) max(xi) else na)
  o <- order(xi)
  srt <- xi[o]
  if (detail) {
    # sort once for all nine percentiles
    ws <- wi[o]
    d <- c(vapply(st_detail_p, function(p) if (n) st_wquantile(srt, ws, p, sorted = TRUE) else na,
                  numeric(1)),
           stat("var"), stat("skewness"), stat("kurtosis"))
    stats <- c(stats, stats::setNames(d, names_d))
  }
  list(stats = stats, smallest = utils::head(srt, 4L), largest = utils::tail(srt, 4L))
}

# Output -----------------------------------------------------------------------

#' @export
print.st_summarize <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

st_parts_summarize <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  parts <- list()
  for (m in attr(x, "st_model", exact = TRUE)) {
    if (!is.na(m$label)) parts <- c(parts, list(st_part_heading(m$label)))
    parts <- c(parts, if (spec$detail) st_parts_summ_detail(m, spec) else list(st_part_summ_table(m, spec)))
  }
  parts
}

st_summ_fmt <- function(v, fmt) {
  if (nzchar(fmt)) st_apply_fmt(v, fmt) else st_fmt_g(v)
}

st_part_summ_table <- function(m, spec) {
  s <- m$stats
  n <- nrow(s)
  show_w <- spec$weighted && spec$weight_type != "fweight"
  cols <- list(Obs = st_fmt_freq(s[, "obs"]))
  if (show_w) cols$Weight <- st_fmt_g(s[, "sum_w"])
  cols$Mean <- character(n)
  cols$`Std. dev.` <- character(n)
  cols$Min <- character(n)
  cols$Max <- character(n)
  for (i in seq_len(n)) {
    f <- spec$formats[i]
    cols$Mean[i] <- st_summ_fmt(s[i, "mean"], f)
    cols$`Std. dev.`[i] <- st_summ_fmt(s[i, "sd"], f)
    cols$Min[i] <- st_summ_fmt(s[i, "min"], f)
    cols$Max[i] <- st_summ_fmt(s[i, "max"], f)
  }
  sep <- spec$separator
  rules <- c(0L, if (sep > 0L && n > sep) seq(sep, n - 1L, by = sep))
  st_part_grid(stub = st_abbrev(spec$vars, 12L), body = do.call(cbind, cols),
               header = names(cols), stub_header = "Variable", rules = rules)
}

# Stata's `summarize, detail` block for each numeric variable.
st_parts_summ_detail <- function(m, spec) {
  parts <- list()
  s <- m$stats
  for (i in which(m$numeric)) {
    f <- spec$formats[i]
    v <- function(name) st_summ_fmt(s[i, name], f)
    sm <- st_summ_fmt(m$smallest[[i]], f)
    lg <- rev(st_summ_fmt(m$largest[[i]], f))
    sm <- c(sm, rep("", 4L - length(sm)))
    lg <- c(lg, rep("", 4L - length(lg)))
    pct <- vapply(paste0("p", st_detail_p), v, character(1))
    row <- function(lab, p, ext = "", slab = "", sval = "") {
      line <- paste0(st_pad(lab, 3L), st_pad(p, 13L), st_pad(ext, 15L))
      if (nzchar(slab)) line <- paste0(line, "       ", st_pad(slab, 12L, "left"), st_pad(sval, 11L))
      st_rtrim(line)
    }
    obs <- st_fmt_freq(s[i, "obs"])
    title <- st_trunc(spec$titles[i], 61L)
    lines <- c(
      st_rtrim(st_center(title, 61L)),
      strrep("-", 61L),
      "      Percentiles      Smallest",
      row("1%", pct[1], sm[1]),
      row("5%", pct[2], sm[2]),
      row("10%", pct[3], sm[3], "Obs", obs),
      row("25%", pct[4], sm[4], "Sum of wgt.", st_fmt_freq(s[i, "sum_w"])),
      "",
      row("50%", pct[5], "", "Mean", v("mean")),
      st_rtrim(paste0(strrep(" ", 24L), "Largest       ", st_pad("Std. dev.", 12L, "left"),
                      st_pad(v("sd"), 11L))),
      row("75%", pct[6], lg[4]),
      row("90%", pct[7], lg[3], "Variance", st_fmt_g(s[i, "var"])),
      row("95%", pct[8], lg[2], "Skewness", st_fmt_g(s[i, "skewness"])),
      row("99%", pct[9], lg[1], "Kurtosis", st_fmt_g(s[i, "kurtosis"]))
    )
    parts <- c(parts, list(st_part_text(lines, mono = TRUE)))
  }
  if (!length(parts)) parts <- list(st_part_text("no numeric variables"))
  parts
}
