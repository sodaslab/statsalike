#' Describe a data set (Stata: `describe`)
#'
#' Re-implementation of Stata's
#' `describe [varlist] [, simple short fullnames numbers]`.
#' Shows the number of observations and variables and, for each variable, its
#' storage type, display format, value label and variable label. Labels
#' imported with `haven` (e.g. via `haven::read_dta()`) are used.
#'
#' The result corresponds to the data set Stata creates with
#' `describe, replace` and can be used directly for further processing.
#'
#' @param .data A data frame or tibble.
#' @param ... Optional variables (tidyselect). All variables by default.
#' @param simple Only list the variable names, compactly.
#' @param short Only show the header (observations, variables).
#' @param fullnames Do not abbreviate long variable names.
#' @param numbers Also show variable numbers.
#' @param export Optional file name ending in `.html` or `.docx`. The
#'   description is also exported to this file (see [s.export()]).
#'
#' @return A tibble of class `st_describe` with the columns `position`,
#'   `name`, `type` (R storage type, i.e. dbl, int, chr, fct, lgl, date, ...,
#'   with `+lbl` if value labels exist), `isnumeric`, `format`, `vallab` and
#'   `varlab`.
#'
#' @examples
#' s.describe(mtcars)
#' s.describe(mtcars, mpg:hp, numbers = TRUE)
#' s.describe(iris, simple = TRUE)
#' @export
s.describe <- function(.data, ..., simple = FALSE, short = FALSE,
                       fullnames = FALSE, numbers = FALSE, export = NULL) {
  data_name <- rlang::as_label(rlang::enquo(.data))
  st_check_data(.data)
  pos <- if (...length()) {
    tidyselect::eval_select(rlang::expr(c(...)), .data)
  } else {
    rlang::set_names(seq_along(.data), names(.data))
  }
  vars <- names(pos)
  cols <- lapply(vars, function(v) .data[[v]])
  info <- tibble::tibble(
    position = unname(as.integer(pos)),
    name = vars,
    type = vapply(cols, st_storage, character(1)),
    isnumeric = vapply(cols, function(x) {
      is.numeric(unclass(x)) || is.logical(x)
    }, logical(1)),
    format = vapply(cols, st_display_format, character(1)),
    vallab = vapply(cols, st_value_label_desc, character(1)),
    varlab = vapply(cols, st_var_label, character(1))
  )
  meta <- list(
    data_name = data_name, nobs = nrow(.data), nvars = ncol(.data),
    size = format(utils::object.size(.data), units = "auto"),
    label = attr(.data, "label", exact = TRUE),
    groups = dplyr::group_vars(.data), simple = simple, short = short,
    fullnames = fullnames, numbers = numbers,
    cmd = st_make_cmd("describe", if (...length()) vars,
                      opts = c("simple", "short", "fullnames", "numbers")[
                        c(simple, short, fullnames, numbers)])
  )
  st_export_if(st_new_output(info, "st_describe", st_meta = meta), export)
}

st_parts_describe <- function(x) {
  meta <- attr(x, "st_meta", exact = TRUE)
  d <- st_plain(x)
  src <- meta$data_name
  head <- c(if (is.null(src) || src %in% c(".", "", ".x")) "Contains data" else
              paste0("Contains data from ", src),
            paste0("Observations: ", formatC(meta$nobs, format = "d", big.mark = ",")),
            paste0("Variables: ", meta$nvars),
            if (!is.null(meta$label) && nzchar(meta$label)) paste0("Label: ", meta$label))
  parts <- list(st_part_text(head))
  if (meta$simple) return(c(parts, list(st_part_text(paste(d$name, collapse = "  ")))))
  if (!meta$short && nrow(d)) {
    stub <- if (meta$numbers) paste0(d$position, ". ", d$name) else d$name
    parts <- c(parts, list(st_part_grid(
      stub = stub, body = cbind(d$type, d$format, d$vallab, d$varlab),
      header = c("Storage type", "Display format", "Value label", "Variable label"),
      stub_header = "Variable name", stub_align = "left", rules = 0L,
      frame = TRUE, col_align = rep("left", 4L))))
  }
  if (length(meta$groups)) {
    parts <- c(parts, list(st_part_text(paste0("Grouped by: ", paste(meta$groups, collapse = "  ")))))
  }
  parts
}

st_storage <- function(x) {
  base <- if (is.factor(x)) {
    if (is.ordered(x)) "ord" else "fct"
  } else if (inherits(x, "Date")) {
    "date"
  } else if (inherits(x, "POSIXt")) {
    "dttm"
  } else if (inherits(x, "difftime")) {
    "drtn"
  } else if (is.logical(x)) {
    "lgl"
  } else if (is.integer(x)) {
    "int"
  } else if (is.double(x)) {
    "dbl"
  } else if (is.character(x)) {
    "chr"
  } else if (is.list(x)) {
    "list"
  } else {
    class(x)[1]
  }
  if (!is.factor(x) && !is.null(attr(x, "labels", exact = TRUE))) {
    base <- paste0(base, "+lbl")
  }
  base
}

# Stata display format: existing format.stata (haven) or Stata's default.
st_display_format <- function(x) {
  f <- attr(x, "format.stata", exact = TRUE)
  if (!is.null(f)) return(as.character(f)[1])
  if (is.character(x)) {
    w <- max(c(1L, st_width(x[!is.na(x)])))
    return(paste0("%-", w, "s"))
  }
  if (inherits(x, "Date")) return("%td")
  if (inherits(x, "POSIXt")) return("%tc")
  if (is.factor(x) || is.integer(x) || is.logical(x)) return("%8.0g")
  if (is.double(x)) return("%10.0g")
  ""
}

st_value_label_desc <- function(x) {
  if (is.factor(x)) return(sprintf("levels(%d)", nlevels(x)))
  labs <- attr(x, "labels", exact = TRUE)
  if (!is.null(labs)) return(sprintf("labels(%d)", length(labs)))
  ""
}

# Abbreviate long names as Stata does (start~end).
st_abbrev <- function(x, n = 15L) {
  long <- nchar(x) > n
  x[long] <- paste0(substr(x[long], 1L, n - 5L), "~",
                    substr(x[long], nchar(x[long]) - 3L, nchar(x[long])))
  x
}

#' @export
print.st_describe <- function(x, ...) {
  meta <- attr(x, "st_meta", exact = TRUE)
  d <- st_plain(x)
  if (meta$simple) {
    cat(formatC(d$name, width = -max(nchar(d$name), 1L)),
        fill = getOption("width"))
    return(invisible(x))
  }

  src <- meta$data_name
  full <- getOption("width", 80L)
  cat(st_trunc(if (is.null(src) || src %in% c(".", "", ".x")) "Contains data" else
                 paste0("Contains data from ", src), full), "\n", sep = "")
  lab <- st_trunc(meta$label %||% "", full - 46L)
  cat(st_rtrim(sprintf(" Observations: %13s                  %s",
                       formatC(meta$nobs, format = "d", big.mark = ","), lab)), "\n", sep = "")
  cat(sprintf("    Variables: %13s\n", meta$nvars))
  cat(sprintf("         Size: %13s\n", meta$size))
  width <- min(getOption("width"), 100L)
  rule <- strrep("-", width)

  if (!meta$short) {
    nm <- if (meta$fullnames) d$name else st_abbrev(d$name)
    if (meta$numbers) {
      nm <- paste0(formatC(d$position, width = nchar(max(c(d$position, 1L)))), ". ", nm)
    }
    w1 <- max(13L, st_width(nm))
    w4 <- max(9L, st_width(d$vallab))
    # variable labels are shortened so that rows fit the console width
    lw <- max(10L, full - w1 - w4 - 26L)
    d$varlab <- st_trunc(d$varlab, lw)
    row <- function(a, b, c, e, f) {
      st_rtrim(paste(st_pad(a, w1, "left"), st_pad(b, 8L, "left"),
                     st_pad(c, 10L, "left"), st_pad(e, w4, "left"), f,
                     sep = "  "))
    }
    cat(rule, "\n", sep = "")
    cat(row("Variable", "Storage", "Display", "Value", ""), "\n", sep = "")
    cat(row("    name", "   type", " format", "label", st_trunc("Variable label", lw)), "\n", sep = "")
    cat(rule, "\n", sep = "")
    for (i in seq_len(nrow(d))) {
      cat(row(nm[i], d$type[i], d$format[i], d$vallab[i], d$varlab[i]), "\n", sep = "")
    }
  }
  cat(rule, "\n", sep = "")
  if (length(meta$groups)) {
    cat("Grouped by: ", paste(meta$groups, collapse = "  "), "\n", sep = "")
  }
  invisible(x)
}
