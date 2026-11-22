# Output parts -----------------------------------------------------------------
# Printing and exporting share one description of the output: a list of parts
# (tables, text, keys, headings) that is rendered as text, HTML or DOCX.

st_part_grid <- function(...) list(type = "grid", args = list(...))
# `attach = TRUE`: printed directly below the previous part, without a blank
# line (e.g. "Note: ..." below a coefficient table, as in Stata).
st_part_text <- function(lines, mono = FALSE, attach = FALSE) {
  list(type = "text", lines = lines, mono = mono, attach = attach)
}
st_part_key <- function(items) list(type = "key", items = items)
st_part_heading <- function(text) list(type = "heading", text = text)
st_part_title <- function(text) list(type = "title", text = text)

# Parts are separated by a blank line, as in Stata's output.
st_render_text <- function(parts) {
  out <- character()
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    if (i > 1L && !isTRUE(p$attach)) out <- c(out, "")
    out <- c(out, switch(p$type,
      # tables in Stata's own layout carry their text (see coeftable.R)
      grid = if (!is.null(p$text)) p$text else do.call(st_grid, p$args),
      text = if (p$mono) p$lines else st_trunc(p$lines, getOption("width", 80L)),
      key = st_key_box(p$items),
      heading = c(p$rule, p$text),
      title = p$text
    ))
  }
  out
}

# Parts of any result object.
st_parts <- function(x) {
  if (inherits(x, "st_tab_list")) {
    parts <- list()
    for (nm in names(x)) {
      parts <- c(parts, list(st_part_heading(paste("-> tabulation of", nm))),
                 st_parts_tabulate(x[[nm]]))
    }
    return(parts)
  }
  if (inherits(x, "st_tabulate")) return(st_parts_tabulate(x))
  if (inherits(x, "st_table")) return(st_parts_table(x))
  if (inherits(x, "st_describe")) return(st_parts_describe(x))
  if (inherits(x, "st_summarize")) return(st_parts_summarize(x))
  if (inherits(x, "st_pwcorr")) return(st_parts_pwcorr(x))
  if (inherits(x, "st_etable")) return(st_parts_etable(x))
  if (inherits(x, "st_codebook")) return(st_parts_codebook(x))
  if (inherits(x, c("st_regress", "st_logit", "st_probit"))) return(st_parts_reg(x))
  if (inherits(x, c("st_mlogit", "st_ologit", "st_oprobit", "st_poisson", "st_nbreg", "st_tobit"))) {
    return(st_parts_ml(x))
  }
  if (inherits(x, "st_margins")) return(st_parts_margins(x))
  # tests and other results carry their parts (s.estat(), s.ttest(), s.anova(), ...)
  if (inherits(x, c("st_estat", "st_test"))) return(attr(x, "st_spec", exact = TRUE)$parts)
  rlang::abort("Unsupported object.")
}

# Command that produced a result, used as the default export title.
st_cmd_of <- function(x) {
  if (inherits(x, "st_tab_list")) return(attr(x, "st_cmd", exact = TRUE) %||% "")
  if (inherits(x, "st_describe")) return(attr(x, "st_meta", exact = TRUE)$cmd %||% "")
  attr(x, "st_spec", exact = TRUE)$cmd %||% ""
}

st_make_cmd <- function(cmd, vars = character(), weight = NULL,
                        opts = character(), by = character()) {
  s <- paste(c(cmd, vars), collapse = " ")
  if (!is.null(weight)) s <- paste0(s, " [", weight, "]")
  if (length(opts)) s <- paste0(s, ", ", paste(opts, collapse = " "))
  if (length(by)) s <- paste0("by ", paste(by, collapse = " "), ": ", s)
  s
}

st_weight_label <- function(wt_quo, weight_type) {
  if (rlang::quo_is_null(wt_quo)) return(NULL)
  paste(weight_type, "=", rlang::as_label(wt_quo))
}

# Normalized table description for HTML/DOCX (same arguments as st_grid()).
st_grid_norm <- function(stub, body, header, stub_header = NULL, span = NULL,
                         rules = 0L, vbars = integer(), stub_align = "right",
                         frame = FALSE, collapse_header = FALSE,
                         col_align = NULL) {
  body <- matrix(st_sanitize(body), nrow = length(stub))
  body[is.na(body)] <- ""
  header <- matrix(st_sanitize(header), ncol = ncol(body))
  header[is.na(header)] <- ""
  if (is.null(stub_header)) stub_header <- rep("", nrow(header))
  stub_header <- st_sanitize(stub_header)
  stub <- st_sanitize(stub)
  stub[is.na(stub)] <- ""
  m <- ncol(body)
  if (collapse_header && m > 1L) {
    same <- rep(TRUE, m - 1L)
    for (i in seq_len(nrow(header))) {
      same <- same & header[i, -1L] == header[i, -m]
      header[i, c(FALSE, same)] <- ""
    }
  }
  indent <- attr(regexpr("^ *", stub, useBytes = TRUE), "match.length")
  list(stub = sub("^ +", "", stub, useBytes = TRUE), indent = indent,
       body = body, header = header, stub_header = stub_header,
       span = if (!is.null(span) && nzchar(span)) st_sanitize(span),
       rules = rules[rules < nrow(body)], vbars = vbars[vbars < m],
       stub_align = stub_align, frame = frame,
       col_align = col_align %||% rep("right", m))
}

# Text for HTML/DOCX must be valid UTF-8; invalid strings are usually
# Latin-1 that was read as UTF-8, so they are converted from Latin-1.
st_sanitize <- function(s) {
  s <- as.character(s)
  ok <- is.na(s) | validUTF8(s)
  s[!ok] <- iconv(s[!ok], "latin1", "UTF-8")
  enc2utf8(s)
}

# HTML -------------------------------------------------------------------------

st_html_escape <- function(s) {
  s <- st_sanitize(s)
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  gsub("\"", "&quot;", s, fixed = TRUE)
}

st_html_css <- c(
  "body { font-family: Arial, Helvetica, sans-serif; font-size: 10pt; color: #000; margin: 2em; }",
  "h2 { font-family: Consolas, 'Courier New', monospace; font-size: 11pt; font-weight: bold; margin: 2em 0 0.6em; }",
  "table.st { border-collapse: collapse; margin: 0.4em 0 1em; }",
  "table.st th, table.st td { padding: 2px 10px; text-align: right; white-space: nowrap; font-weight: normal; }",
  "table.st .stub { text-align: right; border-right: 1px solid #000; }",
  "table.st .left { text-align: left; }",
  "table.st .vbar { border-right: 1px solid #000; }",
  "table.st tr.rule > * { border-bottom: 1px solid #000; }",
  "table.st.frame { border-top: 1px solid #000; border-bottom: 1px solid #000; }",
  "table.st th.span { text-align: center; }",
  "pre.st-note { font-family: Consolas, 'Courier New', monospace; font-size: 9.5pt; margin: 0.3em 0 1em; }",
  "p.st-heading { font-weight: bold; margin: 1.2em 0 0.3em; }",
  "p.st-key { font-style: italic; margin: 0.3em 0; }"
)

st_html_grid <- function(args) {
  g <- do.call(st_grid_norm, args)
  m <- ncol(g$body)
  cell <- function(tag, txt, cls = character(), extra = "") {
    cls <- cls[nzchar(cls)]
    paste0("<", tag, if (length(cls)) paste0(" class=\"", paste(cls, collapse = " "), "\""),
           extra, ">", st_html_escape(txt), "</", tag, ">")
  }
  col_cls <- function(j) c(if (j %in% g$vbars) "vbar", if (g$col_align[j] == "left") "left")
  stub_cls <- c("stub", if (g$stub_align == "left") "left")
  row <- function(cells, rule = FALSE) {
    paste0("<tr", if (rule) " class=\"rule\"", ">", paste(cells, collapse = ""), "</tr>")
  }
  rows <- character()
  if (!is.null(g$span)) {
    k0 <- if (length(g$vbars)) min(g$vbars) else m
    cells <- c(cell("th", "", stub_cls),
               cell("th", g$span, c("span", if (k0 %in% g$vbars) "vbar"),
                    sprintf(" colspan=\"%d\"", k0)))
    if (m > k0) cells <- c(cells, cell("th", "", character(), sprintf(" colspan=\"%d\"", m - k0)))
    rows <- c(rows, row(cells))
  }
  h <- nrow(g$header)
  for (i in seq_len(h)) {
    cells <- c(cell("th", g$stub_header[i], stub_cls),
               vapply(seq_len(m), function(j) cell("th", g$header[i, j], col_cls(j)), ""))
    rows <- c(rows, row(cells, rule = i == h && 0L %in% g$rules))
  }
  for (i in seq_len(nrow(g$body))) {
    pad <- if (g$indent[i] > 0L) sprintf(" style=\"padding-left:%.1fem\"", 0.8 + 0.5 * g$indent[i]) else ""
    cells <- c(cell("td", g$stub[i], stub_cls, pad),
               vapply(seq_len(m), function(j) cell("td", g$body[i, j], col_cls(j)), ""))
    rows <- c(rows, row(cells, rule = i %in% g$rules))
  }
  c(paste0("<table class=\"st", if (g$frame) " frame", "\">"), rows, "</table>")
}

st_render_html <- function(parts) {
  unlist(lapply(parts, function(p) switch(p$type,
    grid = st_html_grid(p$args),
    text = if (p$mono) {
      paste0("<pre class=\"st-note\">", paste(st_html_escape(p$lines), collapse = "\n"), "</pre>")
    } else {
      paste0("<p>", st_html_escape(p$lines), "</p>")
    },
    key = paste0("<p class=\"st-key\">Key: ", st_html_escape(paste(p$items, collapse = " / ")), "</p>"),
    heading = paste0("<p class=\"st-heading\">", st_html_escape(p$text), "</p>"),
    title = paste0("<h2>", st_html_escape(p$text), "</h2>")
  )), use.names = FALSE)
}

st_write_html <- function(parts, file, doc_title) {
  html <- c("<!DOCTYPE html>", "<html>", "<head>", "<meta charset=\"utf-8\">",
            paste0("<title>", st_html_escape(doc_title), "</title>"),
            "<style>", st_html_css, "</style>", "</head>", "<body>",
            st_render_html(parts), "</body>", "</html>")
  con <- suppressWarnings(file(file, open = "wb"))
  on.exit(close(con))
  writeLines(html, con, useBytes = TRUE)
}

# DOCX -------------------------------------------------------------------------

st_need <- function(pkgs, what) {
  miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) {
    rlang::abort(sprintf("%s requires the package(s) %s. Install with install.packages(c(%s)).",
                         what, paste(miss, collapse = ", "),
                         paste0("\"", miss, "\"", collapse = ", ")))
  }
}

st_docx_grid <- function(args, caption = list()) {
  g <- do.call(st_grid_norm, args)
  m <- ncol(g$body)
  n <- nrow(g$body)
  h <- nrow(g$header)
  keys <- paste0("c", 0:m)
  df <- as.data.frame(cbind(g$stub, g$body), stringsAsFactors = FALSE)
  names(df) <- keys
  ft <- flextable::flextable(df, col_keys = keys)
  ft <- flextable::set_header_labels(
    ft, values = stats::setNames(as.list(c(g$stub_header[h], g$header[h, ])), keys))
  for (i in rev(seq_len(h - 1L))) {
    ft <- flextable::add_header_row(ft, values = c(g$stub_header[i], g$header[i, ]),
                                    colwidths = rep(1L, m + 1L), top = TRUE)
  }
  if (!is.null(g$span)) {
    k0 <- if (length(g$vbars)) min(g$vbars) else m
    vals <- c("", g$span)
    widths <- c(1L, k0)
    if (m > k0) {
      vals <- c(vals, "")
      widths <- c(widths, m - k0)
    }
    ft <- flextable::add_header_row(ft, values = vals, colwidths = widths, top = TRUE)
  }
  # Lines before the table (command, heading, key) become borderless top rows:
  # Word does not reliably keep a paragraph on the page of the next table.
  nc <- length(caption)
  for (k in rev(seq_len(nc))) {
    ft <- flextable::add_header_row(ft, values = caption[[k]]$text,
                                    colwidths = m + 1L, top = TRUE)
  }
  nh <- nc + h + !is.null(g$span)
  grid_hdr <- seq(nc + 1L, nh)
  b <- officer::fp_border(color = "black", width = 0.75)
  ft <- flextable::border_remove(ft)
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::bold(ft, bold = FALSE, part = "header")
  ft <- flextable::align(ft, j = 1, align = g$stub_align, part = "all")
  for (j in seq_len(m)) ft <- flextable::align(ft, j = j + 1L, align = g$col_align[j], part = "all")
  if (!is.null(g$span)) ft <- flextable::align(ft, i = nc + 1L, align = "center", part = "header")
  if (0L %in% g$rules) ft <- flextable::hline_bottom(ft, border = b, part = "header")
  body_rules <- g$rules[g$rules > 0L]
  if (length(body_rules)) ft <- flextable::hline(ft, i = body_rules, border = b, part = "body")
  if (g$frame) {
    ft <- if (nc) flextable::hline(ft, i = nc, border = b, part = "header") else
      flextable::hline_top(ft, border = b, part = "header")
    ft <- flextable::hline_bottom(ft, border = b, part = "body")
  }
  vl <- c(1L, g$vbars + 1L)
  ft <- flextable::vline(ft, i = grid_hdr, j = vl, border = b, part = "header")
  ft <- flextable::vline(ft, j = vl, border = b, part = "body")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1,
                           padding.left = 4, padding.right = 4, part = "all")
  for (k in seq_len(nc)) {
    ft <- flextable::style(ft, i = k, part = "header", pr_t = caption[[k]]$prop)
    ft <- flextable::align(ft, i = k, align = "left", part = "header")
    ft <- flextable::padding(ft, i = k, part = "header", padding.left = 0,
                             padding.top = caption[[k]]$top)
  }
  for (l in setdiff(unique(g$indent), 0L)) {
    ft <- flextable::padding(ft, i = which(g$indent == l), j = 1,
                             padding.left = 4 + 3 * l, part = "body")
  }
  ft <- flextable::autofit(ft)
  # widen narrow tables so that the lines on top do not wrap (up to the
  # usable width of an A4/Letter page)
  need <- max(0, vapply(caption, function(q) st_text_inches(q$text, q$prop), numeric(1)))
  widths <- dim(ft)$widths
  if (need > sum(widths)) {
    ft <- flextable::width(ft, j = seq_along(widths),
                           width = widths * min(need, 6.3) / sum(widths))
  }
  # keep short tables on one page; long tables repeat their header
  flextable::paginate(ft, init = TRUE, hdr_ftr = TRUE)
}

# Approximate printed width of a text line in inches (exact for monospaced
# fonts, generous for proportional ones so that capitals fit too).
st_text_inches <- function(text, prop) {
  em <- if (grepl("courier|mono|consol", prop$font.family, ignore.case = TRUE)) 0.6 else 0.65
  nchar(text, type = "chars", allowNA = TRUE) * em * prop$font.size / 72 + 0.15
}

st_write_docx <- function(parts, file) {
  st_need(c("officer", "flextable"), "Export to DOCX")
  doc <- officer::read_docx()
  mono <- officer::fp_text(font.family = "Courier New", font.size = 9)
  norm <- officer::fp_text(font.family = "Arial", font.size = 10)
  bold <- officer::fp_text(font.family = "Arial", font.size = 10, bold = TRUE)
  ital <- officer::fp_text(font.family = "Arial", font.size = 9, italic = TRUE)
  cmd <- officer::fp_text(font.family = "Courier New", font.size = 11, bold = TRUE)
  par <- function(txt, prop, top = 0) {
    fp <- officer::fp_par(padding.top = top, padding.bottom = 2)
    doc <<- officer::body_add_fpar(doc, officer::fpar(officer::ftext(st_sanitize(txt), prop), fp_p = fp))
  }
  # Lines before a table are collected and placed inside the table.
  pending <- list()
  hold <- function(text, prop, top, kind) {
    pending[[length(pending) + 1L]] <<- list(text = st_sanitize(text), prop = prop, top = top, kind = kind)
  }
  flush <- function() {
    for (q in pending) par(q$text, q$prop, q$top)
    pending <<- list()
  }
  for (p in parts) {
    kinds <- vapply(pending, `[[`, character(1), "kind")
    if (p$type == "title" || (p$type == "heading" && any(kinds %in% c("text", "key")))) flush()
    switch(p$type,
      title = hold(p$text, cmd, 14, "title"),
      heading = hold(p$text, bold, 6, "heading"),
      key = hold(paste("Key:", paste(p$items, collapse = " / ")), ital, 1, "key"),
      text = if (p$mono) {
        flush()
        for (line in p$lines) par(line, mono)
      } else {
        for (line in p$lines) hold(line, norm, 1, "text")
      },
      grid = {
        doc <- flextable::body_add_flextable(doc, st_docx_grid(p$args, pending), align = "left")
        pending <- list()
        # Word merges tables that directly follow each other
        par("", officer::fp_text(font.size = 4))
      }
    )
  }
  flush()
  print(doc, target = file)
}

# Export -----------------------------------------------------------------------

#' Export tables to HTML or Word (DOCX)
#'
#' Writes one or more results of the statsalike commands
#' ([s.tabulate()], [s.tab1()], [s.tab2()], [s.table()], [s.describe()],
#' [s.summarize()], [s.pwcorr()], [s.codebook()], [s.regress()], [s.logit()],
#' [s.etable()]) to an HTML
#' file or a Word document, laid out as in the printed output (the
#' equivalent of Stata's `collect export` or `putdocx`). Each table is
#' preceded by the command that produced it.
#'
#' Instead of calling `s.export()`, you can also use the `export` argument of
#' the table functions, e.g. `s.tab(df, x, y, row = TRUE, export = "tab.docx")`.
#'
#' HTML files are self-contained and need no extra packages. Word files
#' require the packages 'officer' and 'flextable'.
#'
#' @param x A result of one of the statsalike commands, or a list
#'   of such results (exported into one file, in order).
#' @param file Path of the output file. The format is taken from the file
#'   extension (`.html`, `.htm` or `.docx`) unless `format` is given.
#' @param format `"html"` or `"docx"`. By default derived from `file`.
#' @param title `TRUE` (default) to put the command above each table,
#'   `FALSE` for no titles, or a character vector with one title per table.
#' @return `x`, invisibly, so `s.export()` can be used at the end of a pipe.
#' @examples
#' tab <- s.tab(mtcars, cyl, am, row = TRUE, chi2 = TRUE)
#' f <- tempfile(fileext = ".html")
#' s.export(tab, f)
#'
#' # several tables in one file
#' s.export(list(s.describe(mtcars), tab, s.table(mtcars, cyl, am)), f)
#'
#' # directly as an option of the table functions
#' s.tab(mtcars, gear, export = f)
#'
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE)) {
#'   s.export(tab, tempfile(fileext = ".docx"))
#' }
#' @export
s.export <- function(x, file, format = NULL, title = TRUE) {
  is_result <- function(o) inherits(o, c("st_output", "st_tab_list"))
  objs <- if (is_result(x)) list(x) else x
  if (!is.list(objs) || !length(objs) || !all(vapply(objs, is_result, logical(1)))) {
    rlang::abort(paste0("`x` must be a result of a statsalike command (s.tab(), s.table(), ",
                        "s.describe(), s.summarize(), s.pwcorr(), s.codebook(), s.regress(), ...) ",
                        "or a list of such results."))
  }
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
    rlang::abort("`file` must be a single file path.")
  }
  if (is.null(format)) {
    ext <- tolower(sub(".*\\.", "", basename(file)))
    format <- if (grepl(".", basename(file), fixed = TRUE)) ext else ""
  }
  format <- tolower(format)
  if (format == "htm") format <- "html"
  if (!format %in% c("html", "docx")) {
    rlang::abort("Unknown export format; use a file ending in .html or .docx (or set `format`).")
  }
  if (!dir.exists(dirname(file))) {
    rlang::abort(sprintf("The directory '%s' does not exist.", dirname(file)))
  }

  titles <- if (isTRUE(title)) {
    vapply(objs, st_cmd_of, character(1))
  } else if (isFALSE(title) || is.null(title)) {
    rep(NA_character_, length(objs))
  } else {
    if (!length(title) %in% c(1L, length(objs))) {
      rlang::abort(sprintf("`title` must be TRUE, FALSE or have length 1 or %d (one per table).",
                           length(objs)))
    }
    rep_len(as.character(title), length(objs))
  }
  parts <- list()
  for (i in seq_along(objs)) {
    if (!is.na(titles[i]) && nzchar(titles[i])) parts <- c(parts, list(st_part_title(titles[i])))
    parts <- c(parts, st_parts(objs[[i]]))
  }

  if (format == "docx") st_need(c("officer", "flextable"), "Export to DOCX")
  tryCatch(
    if (format == "html") {
      doc_title <- titles[!is.na(titles) & nzchar(titles)]
      st_write_html(parts, file, doc_title = if (length(doc_title)) doc_title[1] else "statsalike")
    } else {
      st_write_docx(parts, file)
    },
    error = function(e) {
      rlang::abort(sprintf("Could not write '%s'. Is the file open in another program?", file),
                   parent = e)
    }
  )
  message("Exported to ", file)
  invisible(x)
}

st_export_if <- function(x, export) {
  if (!is.null(export)) s.export(x, export)
  x
}
