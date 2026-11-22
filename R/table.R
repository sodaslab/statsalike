#' Tables of frequencies and summary statistics (Stata: `table`)
#'
#' Re-implementation of Stata's `table` using the syntax introduced in
#' Stata 17:
#' `table (rowvars) (colvars) (tabvars), statistic(...) nototals missing nformat()`.
#' Row, column and table variables can be nested. As in Stata, totals and
#' subtotals (`Total`) are computed for every level by default.
#'
#' Mapping of the Stata syntax.
#'
#' | Stata                                   | statsalike |
#' |-----------------------------------------|-----------|
#' | `table rep78`                           | `s.table(df, rep78)` |
#' | `table (rep78) (foreign)`               | `s.table(df, rep78, foreign)` |
#' | `table (rep78 foreign) ()`              | `s.table(df, c(rep78, foreign))` |
#' | `table () (foreign) (rep78)`            | `s.table(df, cols = foreign, tabs = rep78)` |
#' | `table foreign, stat(mean price mpg) stat(sd price mpg)` | `s.table(df, foreign, statistic = c("mean", "sd"), vars = c(price, mpg))` |
#' | `table foreign, stat(freq) stat(percent)` | `s.table(df, foreign, statistic = c("frequency", "percent"))` |
#' | `..., nototals`                         | `totals = FALSE` |
#' | `..., nformat(%9.2f mean)`              | `nformat = c(mean = "%9.2f")` |
#' | `table ... [aweight = w]`               | `wt = w, weight_type = "aweight"` |
#'
#' The following statistics are supported (`statistic`).
#'
#' * Without a variable, `frequency` (or `freq`), `percent`, `proportion`,
#'   `rowpercent`, `colpercent` (percentages within rows or columns, the
#'   equivalent of Stata's `percent, across()`).
#' * With variables (`vars`), `mean`, `sd`, `var`, `semean`, `cv`,
#'   `skewness`, `kurtosis`, `min`, `max`, `range`, `sum`, `total`, `rawsum`,
#'   `sumw`, `count`, `median`, `iqr`, `q1`, `q2`, `q3`, `p1` ... `p99`,
#'   `first`, `last`.
#'
#' Each variable-based statistic is computed for every variable in `vars`
#' (ordered as in Stata, statistic first, then variable). Quantiles follow
#' Stata's definition, also with weights.
#'
#' @param .data A data frame or tibble. Groups ([dplyr::group_by()]) are
#'   treated like Stata's `by` prefix. One table per group, without a total
#'   across groups.
#' @param rows Row variable(s) (tidyselect, e.g. `c(a, b)` for nesting).
#' @param cols Column variable(s).
#' @param tabs Table variable(s). A separate table for each value.
#' @param statistic Character vector of statistics (see above).
#' @param vars Variables for which variable-based statistics are computed.
#' @param totals Compute total rows and columns (`Total`). `FALSE` is the
#'   equivalent of Stata's `nototals`.
#' @param missing Treat missing values of the row, column and table variables
#'   as categories (see [s.tabulate()] for what counts as missing). Unlike
#'   Stata, this is the default (since statsalike 0.1.2). `missing = FALSE`
#'   excludes them. Missing values of the variables in `vars` are always
#'   excluded from statistics.
#' @param nolabel Show numeric codes instead of value labels.
#' @param nformat Display format in Stata or `sprintf()` style, e.g.
#'   `"%9.2f"`. Named to target individual statistics, e.g.
#'   `c(mean = "%9.2f", sd = "%9.3f")`. Affects printing only. See the
#'   section on display formats in [statsalike-stata-vs-r].
#' @param firstcell Text in the empty cell at the top left of the table, e.g.
#'   `firstcell = "Mean of mpg"`. An addition to Stata, whose `table` leaves
#'   this cell empty.
#' @param wt Optional weighting variable.
#' @param weight_type `"fweight"`, `"aweight"` or `"iweight"` (see
#'   [statsalike-weights]).
#' @param export Optional file name ending in `.html` or `.docx`. The table
#'   is also exported to this file, laid out as printed (see [s.export()]).
#'
#' @return A tibble of class `st_table` in long format, with one column for each
#'   table, row and column variable (factors, with totals as level `"Total"`),
#'   plus `statistic`, `variable` and `value`. It prints in Stata's layout and
#'   can be processed directly with dplyr/tidyr.
#'
#' @examples
#' df <- tibble::as_tibble(mtcars)
#'
#' # table cyl
#' s.table(df, cyl)
#'
#' # table (cyl) (am)
#' s.table(df, cyl, am)
#'
#' # table (cyl) (am), statistic(mean mpg) statistic(sd mpg) nformat(%9.2f)
#' s.table(df, cyl, am, statistic = c("mean", "sd"), vars = mpg,
#'          nformat = "%9.2f")
#'
#' # table (cyl gear) (), statistic(frequency) statistic(percent)
#' s.table(df, c(cyl, gear), statistic = c("frequency", "percent"))
#'
#' # table cyl, statistic(median mpg hp) nototals
#' s.table(df, cyl, statistic = "median", vars = c(mpg, hp), totals = FALSE)
#'
#' # table (cyl) (am), then export to Word or HTML
#' s.table(df, cyl, am, export = tempfile(fileext = ".html"))
#' @export
s.table <- function(.data, rows = NULL, cols = NULL, tabs = NULL,
                    statistic = "frequency", vars = NULL, totals = TRUE,
                    missing = TRUE, nolabel = FALSE, nformat = NULL,
                    wt = NULL, weight_type = c("fweight", "aweight", "iweight"),
                    firstcell = NULL, export = NULL) {
  st_check_data(.data)
  weight_type <- match.arg(weight_type)
  firstcell <- st_check_firstcell(firstcell)
  gv <- dplyr::group_vars(.data)
  data <- dplyr::ungroup(.data)

  row_v <- st_select(rlang::enquo(rows), data)
  col_v <- st_select(rlang::enquo(cols), data)
  tab_v <- unique(c(gv, st_select(rlang::enquo(tabs), data)))
  dims <- c(tab_v, row_v, col_v)
  if (anyDuplicated(dims)) {
    rlang::abort("Each variable may appear in only one dimension (rows, cols, tabs, grouping).")
  }
  if (any(dims %in% c("statistic", "variable", "value"))) {
    rlang::abort("The variable names `statistic`, `variable` and `value` are reserved.")
  }

  stats <- st_parse_stats(statistic)
  stat_v <- st_select(rlang::enquo(vars), data)
  if (any(!stats %in% st_freq_stats) && !length(stat_v)) {
    rlang::abort(paste0("Statistic(s) ",
                        paste(stats[!stats %in% st_freq_stats], collapse = ", "),
                        " require variables in `vars`."))
  }
  if ("rowpercent" %in% stats && !length(col_v)) {
    rlang::abort("`rowpercent` requires column variables (`cols`).")
  }
  if ("colpercent" %in% stats && !length(row_v)) {
    rlang::abort("`colpercent` requires row variables (`rows`).")
  }
  for (v in stat_v) {
    x <- st_strip(data[[v]])
    if (!(is.numeric(x) || is.logical(x))) {
      rlang::abort(sprintf("Variable `%s` is not numeric.", v))
    }
  }
  if (!is.null(nformat) && !is.character(nformat)) {
    rlang::abort("`nformat` must be a character vector, e.g. \"%9.2f\".")
  }
  pairs <- st_stat_pairs(stats, stat_v)

  w <- st_eval_weights(rlang::enquo(wt), data, weight_type) %||% rep(1, nrow(data))
  fs <- lapply(dims, function(d) st_as_levels(data[[d]], nolabel = nolabel, missing = missing))
  names(fs) <- dims
  keep <- !is.na(w) & w != 0
  for (f in fs) keep <- keep & !is.na(f)
  if (!any(keep)) rlang::abort("No observations.")
  fs <- lapply(fs, function(f) droplevels(f[keep]))
  w <- w[keep]
  n <- length(w)
  X <- lapply(stat_v, function(v) st_num(data[[v]])[keep])
  names(X) <- stat_v
  wf <- if (weight_type == "aweight") w * n / sum(w) else w

  # All combinations of full dimensions and (sub)totals. Groups from
  # group_by() behave like Stata's `by`: no total across the groups.
  depth <- function(k, min = 0L) if (totals) k:min else k
  spec_grid <- expand.grid(jt = depth(length(tab_v), length(gv)), jr = depth(length(row_v)),
                           jc = depth(length(col_v)), KEEP.OUT.ATTRS = FALSE)
  res_dims <- list()
  res_vals <- list()
  for (s in seq_len(nrow(spec_grid))) {
    tt <- tab_v[seq_len(spec_grid$jt[s])]
    rr <- row_v[seq_len(spec_grid$jr[s])]
    cc <- col_v[seq_len(spec_grid$jc[s])]
    act <- c(tt, rr, cc)
    groups <- split(seq_len(n), st_key(fs[act], n))
    den_pct <- st_group_sum(wf, fs[tt], n)
    den_row <- st_group_sum(wf, fs[c(tt, rr)], n)
    den_col <- st_group_sum(wf, fs[c(tt, cc)], n)
    for (idx in groups) {
      if (!length(idx)) next
      i1 <- idx[1]
      fr <- sum(wf[idx])
      vals <- vapply(seq_len(nrow(pairs)), function(p) {
        switch(pairs$statistic[p],
          frequency = fr,
          percent = 100 * fr / den_pct[i1],
          proportion = fr / den_pct[i1],
          rowpercent = 100 * fr / den_row[i1],
          colpercent = 100 * fr / den_col[i1],
          st_var_stat(pairs$statistic[p], X[[pairs$variable[p]]][idx],
                      w[idx], weight_type)
        )
      }, numeric(1))
      res_dims[[length(res_dims) + 1L]] <- vapply(dims, function(d) {
        if (d %in% act) as.character(fs[[d]][i1]) else "Total"
      }, character(1))
      res_vals[[length(res_vals) + 1L]] <- vals
    }
  }

  G <- length(res_vals)
  P <- nrow(pairs)
  out <- list()
  for (d in dims) {
    labs <- vapply(res_dims, `[[`, character(1), d)
    lv <- levels(fs[[d]])
    if (totals && !d %in% gv) lv <- unique(c(lv, "Total"))
    out[[d]] <- factor(rep(labs, each = P), levels = lv)
  }
  out$statistic <- factor(rep(pairs$statistic, G), levels = unique(pairs$statistic))
  out$variable <- rep(pairs$variable, G)
  out$value <- unlist(res_vals, use.names = FALSE)
  res <- tibble::as_tibble(out)
  ord <- do.call(order, c(unname(lapply(res[dims], as.integer)),
                          list(rep(seq_len(P), G))))
  res <- res[ord, ]

  labels <- lapply(dims, function(d) st_label_or_name(data[[d]], d))
  names(labels) <- dims
  spec <- list(tabs = tab_v, rows = row_v, cols = col_v, pairs = pairs,
               totals = totals, nformat = nformat, labels = labels,
               levels = lapply(res[dims], levels), firstcell = firstcell)
  res <- st_na_to_r(res, dims)
  dims_txt <- c(paste0("(", paste(row_v, collapse = " "), ")"),
                paste0("(", paste(col_v, collapse = " "), ")"),
                if (length(setdiff(tab_v, gv))) paste0("(", paste(setdiff(tab_v, gv), collapse = " "), ")"))
  stat_txt <- vapply(stats, function(s) {
    v <- if (s %in% st_freq_stats) character() else stat_v
    paste0("statistic(", paste(c(s, v), collapse = " "), ")")
  }, character(1))
  spec$cmd <- st_make_cmd("table", dims_txt,
                          weight = st_weight_label(rlang::enquo(wt), weight_type),
                          opts = c(unname(stat_txt), if (!totals) "nototals",
                                   if (missing) "missing", if (nolabel) "nolabel"),
                          by = gv)
  st_export_if(st_new_output(res, "st_table", st_spec = spec), export)
}

# Statistic x variable combinations in Stata's order.
st_stat_pairs <- function(stats, vars) {
  p <- do.call(rbind, lapply(stats, function(s) {
    if (s %in% st_freq_stats) {
      data.frame(statistic = s, variable = NA_character_)
    } else {
      data.frame(statistic = s, variable = vars)
    }
  }))
  p$label <- ifelse(is.na(p$variable), st_stat_label(p$statistic),
                    paste(st_stat_label(p$statistic), p$variable))
  p$key <- paste(p$statistic, ifelse(is.na(p$variable), "", p$variable), sep = "\r")
  p
}

st_key <- function(flist, n) {
  if (!length(flist)) return(factor(rep.int(1L, n)))
  if (length(flist) == 1L) return(droplevels(flist[[1]]))
  interaction(flist, drop = TRUE, lex.order = TRUE)
}

# Sum of w within the groups defined by flist, returned per observation.
st_group_sum <- function(w, flist, n) {
  key <- st_key(flist, n)
  s <- vapply(split(w, key), sum, numeric(1))
  unname(s[as.integer(key)])
}

st_fmt_stat <- function(value, stat, nformat = NULL) {
  out <- character(length(value))
  nm <- if (is.null(nformat)) character() else names(nformat) %||% rep("", length(nformat))
  for (s in unique(stat)) {
    i <- stat == s
    fmt <- NULL
    if (s %in% nm) {
      fmt <- nformat[[match(s, nm)]]
    } else if ("" %in% nm && s != "frequency") {
      fmt <- nformat[[match("", nm)]]
    }
    out[i] <- if (!is.null(fmt)) {
      st_apply_fmt(value[i], fmt)
    } else if (s %in% c("frequency", "count", "sumw")) {
      st_fmt_freq(value[i])
    } else if (s %in% c("percent", "rowpercent", "colpercent")) {
      st_fmt_fixed(value[i], 2)
    } else if (s == "proportion") {
      st_fmt_fixed(value[i], 4)
    } else {
      st_fmt_g(value[i])
    }
  }
  out
}

#' @export
print.st_table <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

st_parts_table <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  d <- st_plain(x)
  if (!nrow(d)) return(list(st_part_text("no observations")))
  # genuine NA back to the category "<NA>"
  for (v in names(spec$levels)) {
    if (anyNA(d[[v]])) {
      d[[v]] <- factor(ifelse(is.na(d[[v]]), "<NA>", as.character(d[[v]])),
                       levels = spec$levels[[v]])
    }
  }
  if (!length(spec$tabs)) return(list(st_table_part(d, spec)))
  tk <- do.call(paste, c(lapply(spec$tabs, function(v) as.character(d[[v]])), sep = "\r"))
  rows <- split(seq_len(nrow(d)), factor(tk, levels = unique(tk)))
  parts <- list()
  for (idx in rows) {
    sub <- d[idx, ]
    label <- paste0("-> ", paste(spec$tabs, "=", vapply(spec$tabs, function(v) {
      as.character(sub[[v]][1])
    }, character(1)), collapse = ", "))
    parts <- c(parts, list(st_part_heading(label), st_table_part(sub, spec)))
  }
  parts
}

# Layout as in Stata 17: row variables indented in the stub on the left,
# column variables on top; with several statistics and column variables,
# one more row level per statistic.
st_table_part <- function(d, spec) {
  rv <- spec$rows
  cv <- spec$cols
  pairs <- spec$pairs
  P <- nrow(pairs)
  k <- length(rv)
  has_cols <- length(cv) > 0L
  chr <- function(v) as.character(d[[v]])
  rk <- if (k) do.call(paste, c(lapply(rv, chr), sep = "\r")) else rep("", nrow(d))
  ck <- if (has_cols) do.call(paste, c(lapply(cv, chr), sep = "\r")) else rep("", nrow(d))
  pk <- paste(d$statistic, ifelse(is.na(d$variable), "", d$variable), sep = "\r")
  txt <- st_fmt_stat(d$value, as.character(d$statistic), spec$nformat)
  cell_key <- paste(rk, ck, pk, sep = "\t")
  lookup <- function(r, c, p) {
    v <- txt[match(paste(r, c, p, sep = "\t"), cell_key)]
    v[is.na(v)] <- ""
    v
  }

  if (has_cols) {
    cu <- unique(d[cv])
    cu <- cu[do.call(order, unname(lapply(cu, as.integer))), , drop = FALSE]
    cols_key <- do.call(paste, c(lapply(cv, function(v) as.character(cu[[v]])), sep = "\r"))
    hdr <- matrix("", length(cv), length(cols_key))
    for (j in seq_along(cv)) hdr[j, ] <- as.character(cu[[cv[j]]])
    span <- paste(vapply(cv, function(v) spec$labels[[v]], character(1)), collapse = " / ")
    ncols <- length(cols_key)
  } else {
    hdr <- matrix(pairs$label, 1L)
    span <- NULL
    ncols <- P
  }

  value_cells <- function(r, p) {
    if (has_cols) lookup(r, cols_key, pairs$key[p]) else lookup(r, "", pairs$key)
  }
  empty <- rep("", ncols)
  stub <- character()
  body <- list()
  add_line <- function(label, cells) {
    stub <<- c(stub, label)
    body[[length(body) + 1L]] <<- cells
  }
  ind <- function(n) strrep(" ", n)
  emit_leaf <- function(label, r, indent) {
    if (has_cols && P > 1L) {
      add_line(paste0(ind(indent), label), empty)
      for (p in seq_len(P)) add_line(paste0(ind(indent + 2L), pairs$label[p]), value_cells(r, p))
    } else {
      add_line(paste0(ind(indent), label), value_cells(r, 1L))
    }
  }

  if (k == 0L) {
    if (has_cols) {
      for (p in seq_len(P)) add_line(pairs$label[p], value_cells("", p))
    } else {
      add_line("", value_cells("", 1L))
    }
  } else {
    urk <- unique(rk)
    first <- match(urk, rk)
    prev <- NULL
    for (i in seq_along(urk)) {
      labs <- vapply(rv, function(v) as.character(d[[v]][first[i]]), character(1))
      tot <- spec$totals & labs == "Total"
      t <- if (any(tot)) which(tot)[1] else k + 1L
      if (t <= k) {
        emit_leaf("Total", urk[i], 4L * (t - 1L) + 2L)
        next
      }
      c0 <- if (is.null(prev)) 1L else which(labs != prev)[1]
      if (is.na(c0)) c0 <- k
      for (dd in c0:k) {
        if ((dd == 1L && is.null(prev)) || dd > c0) {
          add_line(paste0(ind(4L * (dd - 1L)), spec$labels[[rv[dd]]]), empty)
        }
        if (dd < k) {
          add_line(paste0(ind(4L * (dd - 1L) + 2L), labs[dd]), empty)
        } else {
          emit_leaf(labs[dd], urk[i], 4L * (dd - 1L) + 2L)
        }
      }
      prev <- labs
    }
  }

  # the text of `firstcell` stands on the last line of the headings
  st_part_grid(stub = stub, body = do.call(rbind, body), header = hdr,
               stub_header = c(rep("", nrow(hdr) - 1L), spec$firstcell %||% ""),
               span = span, rules = 0L, stub_align = "left", frame = TRUE,
               collapse_header = TRUE)
}
