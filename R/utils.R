# Internal helpers -------------------------------------------------------------

#' @importFrom rlang %||%
NULL

st_check_data <- function(.data) {
  if (!is.data.frame(.data)) {
    rlang::abort("`.data` must be a data frame or tibble.")
  }
  invisible(.data)
}

# Text of the top left cell of a table (`firstcell`): NULL or one string.
st_check_firstcell <- function(firstcell) {
  if (is.null(firstcell)) return(NULL)
  if (!is.character(firstcell) || length(firstcell) != 1L || is.na(firstcell)) {
    rlang::abort("`firstcell` must be a single string, e.g. firstcell = \"Models of income\".")
  }
  firstcell
}

# Select exactly one variable with tidyselect; returns its name.
st_select1 <- function(quo, data, arg) {
  pos <- tidyselect::eval_select(quo, data, allow_rename = FALSE)
  if (length(pos) != 1L) {
    rlang::abort(sprintf(
      "`%s` must select exactly one variable (selected: %d).",
      arg, length(pos)
    ))
  }
  names(pos)
}

# Select any number of variables with tidyselect; returns their names.
st_select <- function(quo, data) {
  names(tidyselect::eval_select(quo, data, allow_rename = FALSE))
}

# Remove haven labels and classes, leaving a bare vector.
# Factors are returned unchanged.
st_strip <- function(x) {
  if (is.factor(x)) return(x)
  for (a in c("labels", "label", "na_values", "na_range", "format.stata",
              "format.spss", "display_width")) {
    attr(x, a) <- NULL
  }
  cls <- setdiff(oldClass(x), c("haven_labelled", "haven_labelled_spss",
                                "labelled", "vctrs_vctr", "double",
                                "integer", "character", "numeric"))
  oldClass(x) <- if (length(cls)) cls else NULL
  x
}

# Missing values as Stata sees them: NA, Stata's extended missing values
# (.a-.z, haven tagged NA), SPSS user-defined missing values
# (haven_labelled_spss; haven's is.na() method covers both) and, for strings,
# "" (Stata's string missing value).
st_is_missing <- function(x) {
  m <- is.na(x)
  if (is.character(x)) m <- m | x == ""
  m
}

# Numeric values with all missing values (see st_is_missing()) set to NA.
st_num <- function(x) {
  m <- st_is_missing(x)
  x <- as.numeric(st_strip(x))
  x[m] <- NA
  x
}

# Missing values are displayed as R prints them: "NA" for numbers, "<NA>"
# for categories and "NA(a)" for extended missing values (tagged NAs).

# Missing number in printed output.
st_na <- function() "NA"

# Missing category (missing = TRUE).
st_na_level <- function() "<NA>"

# Extended missing value .a etc. (haven's tagged NA).
st_na_tag <- function(tag) paste0("NA(", tag, ")")

# Result tibbles hold a genuine NA instead of the category "<NA>"; `cols`
# are factor columns built with st_as_levels().
st_na_to_r <- function(res, cols) {
  for (v in intersect(cols, names(res))) {
    f <- res[[v]]
    if (is.factor(f) && "<NA>" %in% levels(f)) {
      res[[v]] <- factor(as.character(f), levels = setdiff(levels(f), "<NA>"))
    }
  }
  res
}

# Labels for missing values when `missing = TRUE`: "<NA>" for NA and "",
# "NA(a)" etc. for extended missing values, the value itself for SPSS
# user-defined missing values; value labels are used where they exist.
# Levels are ordered as in Stata: "<NA>", "NA(a)", "NA(b)", ..., then SPSS
# codes.
st_missing_labels <- function(x, miss, labs, nolabel) {
  raw <- st_strip(x)
  n <- length(raw)
  lab <- rep(NA_character_, n)
  grp <- key <- rep(NA_real_, n)
  lab[miss] <- st_na_level()
  grp[miss] <- 0
  key[miss] <- 0
  use_labels <- !nolabel && !is.null(labs)
  if (is.double(raw) && requireNamespace("haven", quietly = TRUE)) {
    tag <- haven::na_tag(raw)
    i <- which(miss & !is.na(tag))
    if (length(i)) {
      lab[i] <- st_na_tag(tag[i])
      grp[i] <- 1
      key[i] <- match(tag[i], letters)
      if (use_labels && is.double(labs)) {
        k <- match(tag[i], haven::na_tag(unname(unclass(labs))))
        lab[i[!is.na(k)]] <- names(labs)[k[!is.na(k)]]
      }
    }
  }
  user <- miss & !is.na(raw)
  if (is.character(raw)) user <- user & raw != ""
  i <- which(user)
  if (length(i)) {
    lab[i] <- st_format_values(raw[i])
    grp[i] <- 2
    key[i] <- if (is.numeric(raw)) raw[i] else 0
    if (use_labels) {
      k <- match(raw[i], unname(labs))
      lab[i[!is.na(k)]] <- names(labs)[k[!is.na(k)]]
    }
  }
  o <- which(miss)
  o <- o[order(grp[o], key[o])]
  list(labels = lab, levels = unique(lab[o]))
}

st_var_label <- function(x) {
  lab <- attr(x, "label", exact = TRUE)
  if (is.null(lab) || !length(lab)) "" else as.character(lab)[1]
}

st_label_or_name <- function(x, name) {
  lab <- st_var_label(x)
  if (nzchar(lab)) lab else name
}

# Format values the way Stata shows them in tables (no scientific
# notation, no superfluous zeros).
st_format_values <- function(v) {
  if (is.numeric(v) && is.null(oldClass(v))) {
    vapply(v, function(z) {
      format(z, trim = TRUE, scientific = FALSE, digits = 15,
             drop0trailing = TRUE)
    }, character(1))
  } else {
    as.character(v)
  }
}

# Convert a variable into a factor of the categories Stata would display:
# * sorted by value (for factors: in level order),
# * value labels (haven) are shown unless `nolabel = TRUE`,
# * observed values only,
# * missing values (see st_is_missing()) are excluded, or shown as separate
#   categories ("<NA>", "NA(a)", ...) if `missing = TRUE`.
st_as_levels <- function(x, nolabel = FALSE, missing = FALSE) {
  ml <- NULL
  if (is.factor(x)) {
    if (nolabel) {
      codes <- as.integer(x)
      vals <- sort(unique(codes[!is.na(codes)]))
      f <- factor(codes, levels = vals, labels = as.character(vals))
    } else {
      used <- tabulate(as.integer(x), nbins = nlevels(x)) > 0
      f <- factor(as.character(x), levels = levels(x)[used])
    }
  } else {
    labs <- attr(x, "labels", exact = TRUE)
    miss <- st_is_missing(x)
    if (missing && any(miss)) ml <- st_missing_labels(x, miss, labs, nolabel)
    x <- st_strip(x)
    x[miss] <- NA
    vals <- unique(x[!is.na(x)])
    # Byte order (C locale) like Stata. Radix sorting rejects strings in
    # "unknown" encoding, which read.csv() returns for umlauts on Windows.
    vals <- if (is.character(vals)) vals[order(enc2utf8(vals), method = "radix")] else sort(vals)
    lab <- st_format_values(vals)
    if (!nolabel && !is.null(labs) && length(vals)) {
      m <- match(vals, unname(labs))
      lab[!is.na(m)] <- names(labs)[m[!is.na(m)]]
    }
    lab <- make.unique(lab, sep = " ")
    f <- factor(match(x, vals), levels = seq_along(vals), labels = lab)
  }
  if (missing && anyNA(f)) {
    if (is.null(ml)) ml <- list(labels = rep(st_na_level(), length(f)), levels = st_na_level())
    f <- factor(ifelse(is.na(f), ml$labels, as.character(f)),
                levels = unique(c(levels(f), ml$levels)))
  }
  f
}

# Evaluate weights (column, expression or column name as a string).
st_eval_weights <- function(wt, data, weight_type) {
  if (rlang::quo_is_null(wt)) return(NULL)
  w <- rlang::eval_tidy(wt, data)
  if (is.character(w) && length(w) == 1L && w %in% names(data)) w <- data[[w]]
  if (!is.numeric(st_strip(w))) rlang::abort("Weights (`wt`) must be numeric.")
  w <- st_num(w)
  if (length(w) == 1L) w <- rep(w, nrow(data))
  if (length(w) != nrow(data)) {
    rlang::abort("`wt` must have length 1 or nrow(.data).")
  }
  ok <- !is.na(w)
  if (weight_type != "iweight" && any(w[ok] < 0)) {
    rlang::abort("Negative weights are only allowed with weight_type = \"iweight\".")
  }
  if (weight_type == "fweight" && any(w[ok] != round(w[ok]))) {
    rlang::abort(paste0("fweights must be integers; for analytic weights use ",
                        "weight_type = \"aweight\"."))
  }
  w
}

# Prepare dplyr groups like Stata's `by` prefix.
st_groups <- function(.data) {
  if (!dplyr::is_grouped_df(.data)) {
    return(list(keys = NULL, rows = list(seq_len(nrow(.data))),
                labels = NA_character_))
  }
  keys <- dplyr::group_keys(.data)
  rows <- dplyr::group_rows(.data)
  labs <- lapply(keys, function(k) as.character(st_as_levels(k, missing = TRUE)))
  labels <- vapply(seq_len(nrow(keys)), function(i) {
    paste0("-> ", paste(names(keys), vapply(labs, `[`, "", i),
                        sep = " = ", collapse = ", "))
  }, character(1))
  list(keys = keys, rows = rows, labels = labels)
}

# Result class -----------------------------------------------------------------

st_output_attrs <- c("st_model", "st_spec", "st_tests", "st_meta")

st_new_output <- function(df, subclass, ...) {
  attrs <- list(...)
  for (nm in names(attrs)) attr(df, nm) <- attrs[[nm]]
  class(df) <- c(subclass, "st_output", "tbl_df", "tbl", "data.frame")
  df
}

# Turn a result back into an ordinary tibble.
st_plain <- function(x) {
  for (a in st_output_attrs) attr(x, a) <- NULL
  # every class of statsalike starts with "st_"
  class(x) <- class(x)[!startsWith(class(x), "st_")]
  x
}

# Once a result is modified (filter, mutate, [ ...), the Stata layout no
# longer applies: it becomes an ordinary tibble.

#' @export
`[.st_output` <- function(x, ...) {
  st_plain(NextMethod())
}

#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.st_output <- function(data, template) {
  st_plain(NextMethod())
}

#' @importFrom tibble as_tibble
#' @export
as_tibble.st_output <- function(x, ...) {
  tibble::as_tibble(st_plain(x), ...)
}

#' @export
as.data.frame.st_output <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(st_plain(x), row.names = row.names, optional = optional, ...)
}

# Formatting -------------------------------------------------------------------

st_fmt_freq <- function(x) {
  whole <- !is.na(x) & abs(x - round(x)) < 1e-9
  out <- formatC(x, format = "f", digits = 2, big.mark = ",")
  out[whole] <- formatC(round(x[whole]), format = "f", digits = 0, big.mark = ",")
  out[is.na(x)] <- ""
  out
}

st_fmt_fixed <- function(x, digits) {
  out <- formatC(x, format = "f", digits = digits, big.mark = ",")
  out[is.na(x)] <- ""
  out
}

# Roughly equivalent to Stata's default format %9.0g.
st_fmt_g <- function(x, big.mark = "") {
  out <- trimws(formatC(x, digits = 7, format = "fg", big.mark = big.mark))
  # very small numbers in exponential format, as %9.0g shows them (9.36e-09
  # instead of 0.000000009361398)
  tiny <- !is.na(x) & x != 0 & abs(x) < 1e-4
  if (any(tiny)) out[tiny] <- st_stata_g(x[tiny], 9L)
  out[is.na(x)] <- ""
  out
}

# Stata formats such as "%9.2f", "%9.0gc" or "%8.3e"; otherwise sprintf().
st_apply_fmt <- function(x, fmt) {
  m <- regmatches(fmt, regexec("^%-?([0-9]*)\\.?([0-9]*)([fge])(c?)$", fmt))[[1]]
  if (length(m)) {
    digits <- if (nzchar(m[3])) as.integer(m[3]) else 0L
    mark <- if (nzchar(m[5])) "," else ""
    out <- if (m[4] == "g" && digits == 0L) {
      st_fmt_g(x, mark)
    } else {
      formatC(x, format = m[4], digits = digits, big.mark = mark)
    }
  } else {
    out <- sprintf(fmt, x)
  }
  out <- trimws(out)
  out[is.na(x)] <- ""
  out
}

# Display width of strings; strings with an invalid encoding (e.g. a Latin-1
# file read as UTF-8) fall back to their byte count instead of failing.
st_width <- function(s) {
  s <- as.character(s)
  w <- nchar(s, type = "width", allowNA = TRUE)
  bad <- is.na(w) & !is.na(s)
  w[bad] <- nchar(s[bad], type = "bytes")
  w[is.na(s)] <- 2L
  w
}

st_rtrim <- function(s) sub("\\s+$", "", s, useBytes = TRUE)

# Shorten strings wider than n to n characters, ending in "~" as Stata does.
st_trunc <- function(s, n) {
  n <- max(as.integer(n), 2L)
  long <- which(st_width(s) > n)
  if (!length(long)) return(s)
  ok <- validUTF8(s[long])
  i <- long[ok]
  if (length(i)) s[i] <- paste0(strtrim(s[i], n - 1L), "~")
  # strings with an invalid encoding are cut byte-wise
  j <- long[!ok]
  if (length(j)) {
    s[j] <- vapply(s[j], function(x) paste0(rawToChar(utils::head(charToRaw(x), n - 1L)), "~"),
                   character(1), USE.NAMES = FALSE)
  }
  s
}

st_pad <- function(s, w, align = "right") {
  s[is.na(s)] <- ""
  pad <- strrep(" ", pmax(w - st_width(s), 0L))
  if (align == "right") paste0(pad, s) else paste0(s, pad)
}

st_center <- function(s, w) {
  n <- st_width(s)
  l <- pmax((w - n) %/% 2L, 0L)
  paste0(strrep(" ", l), s, strrep(" ", pmax(w - n - l, 0L)))
}

st_and <- function(x) {
  n <- length(x)
  if (n <= 1L) return(x)
  paste(paste(x[-n], collapse = ", "), "and", x[n])
}

st_key_box <- function(items) {
  w <- max(st_width(c("Key", items))) + 4L
  c(paste0("+", strrep("-", w), "+"),
    paste0("| ", st_pad("Key", w - 1L, "left"), "|"),
    paste0("|", strrep("-", w), "|"),
    paste0("|", st_center(items, w), "|"),
    paste0("+", strrep("-", w), "+"))
}

# Draw a table in Stata's style:
#
#   stub_header |  header ...
#   ------------+-------------
#   stub        |  body ...
#
# `rules`: row indices followed by a horizontal rule (0 = after the header).
# `vbars`: column indices followed by a vertical bar.
# `span`:  heading above the data columns (e.g. the column variable).
# `collapse_header`: in multi-line headers, show repeated labels only at the
#   first column of a group (per block).
# Tables that are too wide are wrapped into blocks, as in Stata.
st_grid <- function(stub, body, header, stub_header = NULL, span = NULL,
                    rules = 0L, vbars = integer(), stub_align = "right",
                    frame = FALSE, collapse_header = FALSE, col_align = NULL,
                    width = getOption("width", 80L)) {
  body <- matrix(as.character(body), nrow = length(stub))
  body[is.na(body)] <- ""
  header <- matrix(as.character(header), ncol = ncol(body))
  header[is.na(header)] <- ""
  if (is.null(stub_header)) stub_header <- rep("", nrow(header))
  stub[is.na(stub)] <- ""
  m <- ncol(body)

  # Keep every line within `width`: long row labels, column labels and the
  # heading above the columns are shortened with "~". Row labels keep their
  # full length as long as the widest data column still fits next to them.
  natural <- vapply(seq_len(m), function(j) {
    max(1L, st_width(c(header[, j], body[, j])))
  }, integer(1)) + 2L
  max_stub <- max(12L, floor(width * 0.45), width - 4L - max(natural))
  stub <- st_trunc(stub, max_stub)
  stub_header <- st_trunc(stub_header, max_stub)
  stubw <- max(1L, st_width(c(stub, stub_header)))
  avail <- max(width - stubw - 2L, 12L)
  header[] <- st_trunc(header, avail - 2L)
  if (!is.null(span)) span <- st_trunc(span, avail - 1L)
  colw <- vapply(seq_len(m), function(j) {
    max(1L, st_width(c(header[, j], body[, j])))
  }, integer(1)) + 2L
  left <- if (is.null(col_align)) rep(FALSE, m) else col_align == "left"
  # cells of columns B, right-aligned or (for text columns) left-aligned
  pad_cells <- function(cells, B) {
    ifelse(left[B], paste0("  ", st_pad(cells, colw[B] - 2L, "left")), st_pad(cells, colw[B]))
  }

  blocks <- list()
  cur <- integer()
  curw <- 0L
  for (j in seq_len(m)) {
    extra <- colw[j] + if (j %in% vbars) 2L else 0L
    if (length(cur) && curw + extra > avail) {
      blocks[[length(blocks) + 1L]] <- cur
      cur <- integer()
      curw <- 0L
    }
    cur <- c(cur, j)
    curw <- curw + extra
  }
  blocks[[length(blocks) + 1L]] <- cur

  out <- character()
  for (b in seq_along(blocks)) {
    B <- blocks[[b]]
    bar <- B %in% vbars & seq_along(B) < length(B)
    join <- function(cells, sep) paste0(cells, ifelse(bar, sep, ""), collapse = "")
    bspan <- NULL
    if (!is.null(span) && nzchar(span)) {
      k0 <- if (any(bar)) which(bar)[1] else length(B)
      S <- B[seq_len(k0)]
      # A heading wider than its columns widens them, so that all rules have
      # the same length.
      room <- avail - sum(colw[setdiff(B, S)]) - 2L * sum(bar)
      bspan <- st_trunc(span, max(1L, room - 1L))
      short <- st_width(bspan) + 1L - sum(colw[S])
      if (short > 0L) {
        colw[S] <- colw[S] + short %/% length(S) + (rev(seq_along(S)) <= short %% length(S))
      }
    }
    rule <- paste0(strrep("-", stubw + 1L), "+", join(strrep("-", colw[B]), "-+"))
    hdr <- header[, B, drop = FALSE]
    if (collapse_header && length(B) > 1L) {
      same <- rep(TRUE, length(B) - 1L)
      for (i in seq_len(nrow(hdr))) {
        same <- same & hdr[i, -1L] == hdr[i, -length(B)]
        hdr[i, c(FALSE, same)] <- ""
      }
    }
    lines <- character()
    if (!is.null(bspan)) {
      lp <- max(1L, (sum(colw[S]) - st_width(bspan)) %/% 2L)
      lines <- c(lines, paste0(strrep(" ", stubw), " |", strrep(" ", lp), bspan))
    }
    for (i in seq_len(nrow(header))) {
      lines <- c(lines, paste0(st_pad(stub_header[i], stubw, stub_align), " |",
                               join(pad_cells(hdr[i, ], B), " |")))
    }
    if (0L %in% rules) lines <- c(lines, rule)
    for (i in seq_len(nrow(body))) {
      lines <- c(lines, paste0(st_pad(stub[i], stubw, stub_align), " |",
                               join(pad_cells(body[i, B], B), " |")))
      if (i %in% rules && i < nrow(body)) lines <- c(lines, rule)
    }
    if (frame) {
      fw <- max(st_width(st_rtrim(lines)))
      lines <- c(strrep("-", fw), lines, strrep("-", fw))
    }
    if (b > 1L) out <- c(out, "")
    out <- c(out, lines)
  }
  st_rtrim(out)
}
