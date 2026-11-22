#' Describe the contents of variables (Stata: `codebook`)
#'
#' Re-implementation of Stata's
#' `codebook [varlist] [, compact header tabulate(#) problems]`.
#' For each variable it shows the type, value labels, range, units, the
#' number of unique and missing values and, depending on the variable, a
#' frequency table, summary statistics or examples.
#'
#' * Up to `tabulate` distinct values, a tabulation with frequencies, values
#'   and value labels (including the missing categories).
#' * More numeric values, mean, standard deviation and percentiles.
#' * More text values, examples, plus warnings about blanks.
#'
#' Missing values follow Stata (see [s.tabulate()]). Extended missing values
#' and SPSS user-defined missing values are listed separately in the
#' tabulation. Grouping by [dplyr::group_by()] is ignored, as Stata's
#' `codebook` does not allow `by`.
#'
#' @param .data A data frame or tibble.
#' @param ... Variables (tidyselect). All variables by default.
#' @param compact Show a compact overview with one line per variable.
#' @param header Show a header with information about the data set.
#' @param tabulate Maximum number of distinct values for which a tabulation
#'   is shown (Stata's `tabulate(#)`, default 9).
#' @param problems Only report potential problems in the data. Constant (or
#'   entirely missing) variables, incompletely labeled variables and text
#'   variables with leading, trailing or embedded blanks.
#' @param export Optional file name ending in `.html` or `.docx`. The output
#'   is also exported to this file (see [s.export()]).
#'
#' @return A tibble of class `st_codebook` with one row per variable and the columns
#'   `variable`, `label`, `type`, `n`, `missing`, `unique`, `range`, `units`,
#'   `mean`, `sd`, `p10` ... `p90` and `problems`. It prints in Stata's layout.
#'
#' @examples
#' d <- tibble::tibble(
#'   id = 1:6,
#'   sex = factor(c("male", "female", "female", NA, "male", "female")),
#'   income = c(1200, 2500, NA, 1800, 3100, 2200),
#'   city = c("Erlangen", "Nürnberg", "Fürth", "Erlangen", " Erlangen", NA)
#' )
#' s.codebook(d)
#' s.codebook(d, compact = TRUE)
#' s.codebook(d, problems = TRUE)
#' @export
s.codebook <- function(.data, ..., compact = FALSE, header = FALSE,
                       tabulate = 9L, problems = FALSE, export = NULL) {
  data_name <- rlang::as_label(rlang::enquo(.data))
  st_check_data(.data)
  data <- dplyr::ungroup(.data)
  vars <- if (...length()) names(tidyselect::eval_select(rlang::expr(c(...)), data)) else names(data)
  if (!length(vars)) rlang::abort("No variables selected.")
  if (!is.numeric(tabulate) || length(tabulate) != 1L || is.na(tabulate) || tabulate < 0) {
    rlang::abort("`tabulate` must be a single number >= 0.")
  }
  info <- lapply(vars, function(v) st_cb_info(data[[v]], v, tabulate))
  num <- function(f) vapply(info, function(i) i[[f]] %||% NA_real_, numeric(1))
  res <- tibble::tibble(
    variable = vars,
    label = vapply(info, `[[`, character(1), "varlab"),
    type = vapply(info, `[[`, character(1), "type"),
    n = num("n"), missing = num("n_miss"), unique = num("n_unique"),
    range = vapply(info, function(i) i$range %||% NA_character_, character(1)),
    units = num("units"), mean = num("mean"), sd = num("sd"),
    p10 = num("p10"), p25 = num("p25"), p50 = num("p50"), p75 = num("p75"), p90 = num("p90"),
    problems = vapply(info, function(i) paste(i$problems, collapse = "; "), character(1))
  )
  meta <- list(
    data_name = data_name, nobs = nrow(data), nvars = ncol(data),
    size = format(utils::object.size(data), units = "auto"),
    label = attr(data, "label", exact = TRUE)
  )
  spec <- list(
    compact = compact, header = header, problems = problems, meta = meta,
    cmd = st_make_cmd("codebook", if (...length()) vars,
                      opts = c(if (compact) "compact", if (header) "header",
                               if (tabulate != 9) sprintf("tabulate(%d)", as.integer(tabulate)),
                               if (problems) "problems"))
  )
  out <- st_new_output(res, "st_codebook", st_model = info, st_spec = spec)
  st_export_if(out, export)
}

# Everything codebook reports about one variable.
st_cb_info <- function(x, name, tabulate) {
  i <- list(name = name, varlab = st_var_label(x), n = length(x), problems = character())
  if (is.list(x) && !is.data.frame(x)) {
    i$kind <- "list"
    i$type <- "List"
    i$n_miss <- sum(vapply(x, function(e) length(e) == 0L || all(is.na(e)), logical(1)))
    return(i)
  }
  miss <- st_is_missing(x)
  raw <- st_strip(x)
  labs <- attr(x, "labels", exact = TRUE)
  i$kind <- if (is.factor(x)) "factor" else if (inherits(raw, "Date")) "date" else
    if (inherits(raw, "POSIXt")) "datetime" else if (is.logical(raw)) "logical" else
    if (is.character(raw)) "string" else if (is.numeric(raw)) "numeric" else "other"
  i$type <- switch(i$kind,
    factor = sprintf("Factor (%d levels)", nlevels(x)),
    date = "Date", datetime = "Date-time", logical = "Logical",
    string = {
      len <- max(c(0L, st_width(raw[!miss])))
      sprintf("String (chr), longest is %d character%s", len, if (len == 1L) "" else "s")
    },
    numeric = sprintf("Numeric (%s)", if (is.integer(raw)) "integer" else "double"),
    class(x)[1])
  i$n_miss <- sum(miss)
  vals <- if (is.factor(x)) as.integer(x)[!miss] else raw[!miss]
  i$n_unique <- length(unique(vals))
  i$missing_name <- paste("Missing", st_na())

  if (!is.null(labs) && !is.factor(x) && length(vals)) {
    # as in Stata: number of distinct nonmissing values without a label
    unlabelled <- sum(!unique(vals) %in% unname(labs))
    i$label_line <- sprintf("%d value label%s", length(labs), if (length(labs) == 1L) "" else "s")
    if (unlabelled) {
      i$label_line <- sprintf("%s, but %s nonmissing value%s not labeled", i$label_line,
                              formatC(unlabelled, format = "d", big.mark = ","),
                              if (unlabelled == 1L) " is" else "s are")
      i$problems <- c(i$problems, "incompletely labeled")
    }
  }
  if (i$n_unique <= 1L) i$problems <- c(i$problems, "constant (or all missing)")

  if (i$kind %in% c("numeric", "date", "datetime") && length(vals)) {
    rng <- range(vals)
    fr <- if (i$kind == "numeric") function(v) st_stata_g(v, 9L) else st_format_values
    i$range <- sprintf("[%s,%s]", fr(rng[1]), fr(rng[2]))
    if (i$kind == "numeric") {
      i$min <- rng[1]
      i$max <- rng[2]
    }
    if (i$kind == "numeric") i$units <- st_units(vals)
  }
  if (i$n_unique <= tabulate && i$n_unique > 0L) {
    i$tab <- st_cb_tab(x, i$kind)
  } else if (i$kind == "numeric" && length(vals)) {
    i$mean <- mean(vals)
    i$sd <- if (length(vals) > 1L) stats::sd(vals) else NA_real_
    sv <- sort(vals)
    one <- rep(1, length(sv))
    for (p in c(10, 25, 50, 75, 90)) i[[paste0("p", p)]] <- st_wquantile(sv, one, p, sorted = TRUE)
  } else if (i$kind %in% c("string", "factor") && length(vals)) {
    u <- if (i$kind == "factor") levels(droplevels(x[!miss])) else
      unique(vals)[order(enc2utf8(unique(vals)), method = "radix")]
    pos <- unique(pmax(1L, round(length(u) * c(1, 3, 5, 7) / 8)))
    i$examples <- if (i$kind == "string") paste0("\"", u[pos], "\"") else u[pos]
  }
  if (i$kind == "string" && length(vals)) {
    if (any(grepl("^\\s|\\s$", vals, useBytes = TRUE))) {
      i$warnings <- c(i$warnings, "Variable has leading and/or trailing blanks.")
      i$problems <- c(i$problems, "leading or trailing blanks")
    }
    if (any(grepl("\\S\\s+\\S", vals, useBytes = TRUE))) {
      i$warnings <- c(i$warnings, "Variable has embedded blanks.")
      i$problems <- c(i$problems, "embedded blanks")
    }
  }
  i
}

# Tabulation of a variable with few values: frequency, value (code) and
# label, including missing categories as in Stata.
st_cb_tab <- function(x, kind) {
  codes <- st_as_levels(x, nolabel = TRUE, missing = TRUE)
  freq <- as.vector(table(codes))
  code <- levels(codes)
  # labels only for one observation per category (fast for large data)
  first <- match(code, as.character(codes))
  lab <- as.character(st_as_levels(x[first], nolabel = FALSE, missing = TRUE))
  has_labels <- kind == "factor" || !is.null(attr(x, "labels", exact = TRUE))
  lab[lab == code] <- ""
  if (kind == "string") code[code != st_na_level()] <- paste0("\"", code[code != st_na_level()], "\"")
  data.frame(freq = freq, code = unname(code), label = unname(lab),
             labelled = has_labels, stringsAsFactors = FALSE)
}

# Units as in Stata: the largest power of 10 of which all values are multiples.
st_units <- function(x) {
  x <- unique(x[is.finite(x)])
  if (!length(x) || all(x == 0)) return(1)
  nz <- x != 0
  # no unit can be larger than the smallest nonzero absolute value
  top <- min(8L, floor(log10(min(abs(x[nz])))))
  if (top < -8) return(NA_real_)
  for (k in top:-8) {
    u <- 10^k
    q <- x / u
    if (all(round(q[nz]) != 0) && all(abs(q - round(q)) < 1e-8 * pmax(1, abs(q)))) return(u)
  }
  NA_real_
}

# Output -----------------------------------------------------------------------

#' @export
print.st_codebook <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

st_parts_codebook <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  info <- attr(x, "st_model", exact = TRUE)
  parts <- list()
  if (spec$header) parts <- c(parts, list(st_part_text(st_cb_header(spec$meta))))
  if (spec$problems) return(c(parts, st_cb_problems(info)))
  if (spec$compact) return(c(parts, list(st_cb_compact(info))))
  c(parts, lapply(info, function(i) st_part_text(st_cb_block(i), mono = TRUE)))
}

st_cb_header <- function(meta) {
  src <- meta$data_name
  st_trunc(c(if (is.null(src) || src %in% c(".", "", ".x")) "Dataset" else paste0("Dataset: ", src),
             if (!is.null(meta$label) && nzchar(meta$label)) paste0("Label: ", meta$label),
             paste0("Number of variables: ", meta$nvars),
             paste0("Number of observations: ", formatC(meta$nobs, format = "d", big.mark = ",")),
             paste0("Size: ", meta$size)),
           getOption("width", 80L))
}

st_cb_block <- function(i) {
  L <- min(getOption("width", 80L), 79L)
  rule <- strrep("-", L)
  lab <- st_trunc(i$varlab, max(L - st_width(i$name) - 2L, 2L))
  title <- paste0(i$name, strrep(" ", max(1L, L - st_width(i$name) - st_width(lab))), lab)
  f <- function(label, value) sprintf("%22s %s", label, value)
  two <- function(label1, value1, label2, value2) {
    st_rtrim(paste0(st_pad(f(label1, value1), 45L, "left"), sprintf("%13s %s", label2, value2)))
  }
  # six significant digits as in Stata; very large or small values in %9.0g
  g6 <- function(v) {
    ifelse(is.na(v), st_na(),
           ifelse(v != 0 & (abs(v) >= 1e7 | abs(v) < 1e-4), st_stata_g(v, 9L),
                  trimws(formatC(v, digits = 6, format = "fg"))))
  }
  # continuation lines start below the values (column 24)
  cont <- function(label, text) {
    w <- strwrap(text, width = max(L - 23L, 20L))
    c(f(label, w[1]), if (length(w) > 1L) paste0(strrep(" ", 23L), w[-1]))
  }
  lines <- c(rule, st_rtrim(title), rule, "", cont("Type:", i$type))
  if (!is.null(i$label_line)) lines <- c(lines, cont("Label:", i$label_line))
  if (i$kind == "list") {
    return(c(lines, "", f(i$missing_name %||% "Missing:", sprintf("%s/%s", i$n_miss, i$n))))
  }
  lines <- c(lines, "")
  miss <- sprintf("%s/%s", formatC(i$n_miss, format = "d", big.mark = ","),
                  formatC(i$n, format = "d", big.mark = ","))
  if (!is.null(i$range)) {
    lines <- c(lines, if (!is.null(i$units)) two("Range:", i$range, "Units:", st_stata_g(i$units, 9L)) else
      f("Range:", i$range))
  }
  lines <- c(lines, two("Unique values:", formatC(i$n_unique, format = "d", big.mark = ","),
                        paste0(i$missing_name, ":"), miss))
  if (!is.null(i$tab)) {
    t <- i$tab
    fq <- formatC(t$freq, format = "d", big.mark = ",")
    fw <- max(5L, st_width(fq))
    room <- max(L - 23L - fw - 2L, 10L)
    t$code <- st_trunc(t$code, if (t$labelled[1]) min(room %/% 2L, 20L) else room)
    if (t$labelled[1]) {
      cw <- max(7L, st_width(t$code))
      t$label <- st_trunc(t$label, max(room - cw - 2L, 5L))
      lines <- c(lines, "", paste0(f("Tabulation:", ""), st_pad("Freq.", fw), "  ",
                                   st_pad("Numeric", cw), "  Label"))
      lines <- c(lines, st_rtrim(paste0(strrep(" ", 23L), st_pad(fq, fw), "  ",
                                        st_pad(t$code, cw), "  ", t$label)))
    } else {
      right <- i$kind %in% c("numeric", "date", "datetime", "logical")
      cw <- max(5L, st_width(t$code))
      lines <- c(lines, "", paste0(f("Tabulation:", ""), st_pad("Freq.", fw), "  ",
                                   if (right) st_pad("Value", cw) else "Value"))
      lines <- c(lines, st_rtrim(paste0(strrep(" ", 23L), st_pad(fq, fw), "  ",
                                        if (right) st_pad(t$code, cw) else t$code)))
    }
  } else if (!is.null(i$mean)) {
    pct <- c(i$p10, i$p25, i$p50, i$p75, i$p90)
    lines <- c(lines, "", f("Mean:", g6(i$mean)), f("Std. dev.:", g6(i$sd)), "",
               paste0(f("Percentiles:", ""), paste(st_pad(c("10%", "25%", "50%", "75%", "90%"), 10L), collapse = "")),
               paste0(strrep(" ", 23L), paste(st_pad(g6(pct), 10L), collapse = "")))
  } else if (!is.null(i$examples)) {
    ex <- st_trunc(i$examples, L - 24L)
    lines <- c(lines, "", f("Examples:", ex[1]), if (length(ex) > 1L) paste0(strrep(" ", 23L), ex[-1]))
  }
  if (length(i$warnings)) {
    lines <- c(lines, "", f("Warning:", i$warnings[1]),
               if (length(i$warnings) > 1L) paste0(strrep(" ", 23L), i$warnings[-1]))
  }
  lines
}

st_cb_compact <- function(info) {
  g <- function(v) if (is.null(v) || is.na(v)) st_na() else st_fmt_g(v)
  num_range <- function(i, k) {
    v <- if (k == 1L) i$min else i$max
    if (is.null(v)) st_na() else st_fmt_g(v)
  }
  mean_of <- function(i) {
    if (!is.null(i$mean)) return(g(i$mean))
    if (i$kind == "numeric" && !is.null(i$tab)) {
      v <- suppressWarnings(as.numeric(i$tab$code))
      ok <- !is.na(v)
      return(g(sum(v[ok] * i$tab$freq[ok]) / sum(i$tab$freq[ok])))
    }
    st_na()
  }
  body <- cbind(
    vapply(info, function(i) formatC(i$n - i$n_miss, format = "d", big.mark = ","), character(1)),
    vapply(info, function(i) if (is.null(i$n_unique)) st_na() else formatC(i$n_unique, format = "d", big.mark = ","), character(1)),
    vapply(info, mean_of, character(1)),
    vapply(info, num_range, character(1), k = 1L),
    vapply(info, num_range, character(1), k = 2L),
    vapply(info, `[[`, character(1), "varlab")
  )
  st_part_grid(stub = vapply(info, `[[`, character(1), "name"), body = body,
               header = c("Obs", "Unique", "Mean", "Min", "Max", "Label"),
               stub_header = "Variable", stub_align = "left", rules = 0L, frame = TRUE,
               col_align = c(rep("right", 5L), "left"))
}

st_cb_problems <- function(info) {
  kinds <- c("constant (or all missing)" = "constant (or all missing) vars",
             "incompletely labeled" = "incompletely labeled vars",
             "leading or trailing blanks" = "string vars with leading or trailing blanks",
             "embedded blanks" = "string vars with embedded blanks")
  rows <- lapply(names(kinds), function(k) {
    v <- vapply(info, function(i) k %in% i$problems, logical(1))
    if (any(v)) c(kinds[[k]], paste(vapply(info[v], `[[`, character(1), "name"), collapse = " "))
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(list(st_part_text("No potential problems in the data.")))
  # wrap long variable lists over several lines
  width <- getOption("width", 80L)
  stub_w <- max(st_width(vapply(rows, `[`, character(1), 1L)))
  stub <- character()
  body <- character()
  for (r in rows) {
    w <- strwrap(r[2], width = max(20L, width - stub_w - 6L))
    stub <- c(stub, r[1], rep("", length(w) - 1L))
    body <- c(body, w)
  }
  list(st_part_text("Potential problems in the data"),
       st_part_grid(stub = stub, body = matrix(body, ncol = 1L),
                    header = "Variables", stub_header = "Potential problem",
                    stub_align = "left", rules = 0L, frame = TRUE, col_align = "left"))
}
