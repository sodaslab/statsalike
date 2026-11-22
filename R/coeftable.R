# Coefficient tables -------------------------------------------------------------
# Tables of estimation results (regress, logit, mlogit, ologit, poisson,
# margins) in the layout of Stata 15. The text is built line by line, as Stata
# draws it; the same rows also form a grid for the export to HTML and DOCX.
#
# A table consists of equations (list(name, base, terms)); every term is
# list(kind, label, rows) with kind
#   "plain"  a single row labelled with the term (continuous variable, _cons),
#   "factor" a heading and one row per category (factor variables and their
#            interactions),
#   "cint"   a single row of an interaction of continuous variables,
# and every row is list(label, cells, status) with status "coef", "base"
# (hidden), "empty", "omitted", "notest" (not estimable), "exposure",
# "offset" or "cut" (cut points without test statistic).

# Width of the label column: 12 characters, widened for longer variable names
# up to the console width, as Stata does with `set lstretch on` (at most 13
# characters for 80 columns).
# Variable names longer than that are abbreviated; labels of interactions
# (`labels`) count only if they fit, otherwise they are broken into lines.
# Labels of categories (`levels`) need one character more (they end one
# column before the bar).
st_ct_width <- function(names, labels = character(), levels = character()) {
  cap <- min(32L, max(12L, getOption("width", 80L) - 67L))
  lw <- c(st_width(labels), st_width(levels[!is.na(levels)]) + 1L)
  min(max(12L, st_width(names), lw[lw <= cap]), cap)
}

# Value labels longer than the column end in "..", variable names in "~".
st_ct_trunc <- function(s, n) {
  long <- st_width(s) > n
  s[long] <- paste0(substr(s[long], 1L, n - 2L), "..")
  s
}

# Label of a term for display, with variable names abbreviated to `W`.
st_ct_term_label <- function(term, is_cat, W) {
  if (length(term) == 1L) return(st_abbrev_name(term, W))
  cats <- term[unlist(is_cat[term])]
  conts <- term[!unlist(is_cat[term])]
  paste(c(st_abbrev_name(cats, W), if (length(conts)) paste0(st_cprefix(), st_abbrev_name(conts, W))),
        collapse = "#")
}

# A label that is too wide is broken after "#" (and after "c."), as in Stata:
#   c.mpg#|
#   c.weight |
st_ct_split <- function(label, W) {
  if (st_width(label) <= W) return(label)
  comps <- strsplit(label, "#", fixed = TRUE)[[1]]
  pieces <- paste0(comps, c(rep("#", length(comps) - 1L), ""))
  fits <- function(s) st_width(s) <= if (endsWith(s, "#")) W + 1L else W
  lines <- character()
  cur <- ""
  for (p in pieces) {
    cand <- paste0(cur, p)
    if (fits(cand)) {
      cur <- cand
      next
    }
    if (nzchar(cur)) lines <- c(lines, cur)
    cur <- p
    if (!fits(cur) && startsWith(cur, "c.")) {
      lines <- c(lines, "c.")
      cur <- substring(cur, 3L)
    }
  }
  c(lines, cur)
}

st_ct_stub_line <- function(s, W) {
  if (endsWith(s, "#")) paste0(st_pad(s, W + 1L), "|") else paste0(st_pad(s, W), " |")
}

# Cells of one coefficient: estimate, standard error, test statistic, p-value
# and the confidence interval (or the standardized coefficient).
st_ct_cells <- function(est, se, stat, p, low = NA, high = NA, beta = NULL, cut = FALSE) {
  # values that overflow (e.g. exp() of a huge coefficient) are missing in Stata
  g <- function(v) if (is.na(v) || !is.finite(v)) "." else st_stata_g(v, 9L)
  out <- c(g(est), g(se),
           if (cut) "" else if (is.na(stat)) "." else st_fmt_t(stat),
           if (cut) "" else if (is.na(p)) "." else sprintf("%.3f", p))
  if (is.null(beta)) c(out, g(low), g(high)) else c(out, g(beta))
}

# Test statistics in Stata's %8.2f; numbers that do not fit are shown in
# exponential format.
st_fmt_t <- function(x) {
  s <- sprintf("%.2f", x)
  if (nchar(s) > 8L) s <- formatC(x, digits = 1L, format = "e")
  if (nchar(s) > 8L) s <- formatC(x, digits = 0L, format = "e")
  s
}

st_ct_status_text <- c(empty = "(empty)", omitted = "(omitted)", notest = "(not estimable)",
                       exposure = "(exposure)", offset = "(offset)")

# Build the table. `colhead` are the five column headings (estimate, standard
# error, statistic, p-value, interval) and `top` an optional line above them
# ("Robust", "Delta-method"); `one` is the value shown for empty and omitted
# rows (1 for odds ratios).
st_ct_build <- function(eqs, W, stub_head, est_lab, stat = "t", level = 95, beta = FALSE,
                        top = NULL, one = "0") {
  widths <- if (beta) c(11L, 11L, 9L, 8L, 25L) else c(11L, 11L, 9L, 8L, 13L, 12L)
  interval <- if (beta) "Beta" else sprintf("[%s%% Conf. Interval]", format(level))
  hdr <- paste0(st_pad(est_lab, 11L), "   Std. Err.", st_pad(stat, 7L),
                st_pad(sprintf("P>|%s|", stat), 9L), st_pad(interval, 25L))
  vw <- sum(widths)
  rule <- paste0(strrep("-", W + 1L), "+", strrep("-", vw))
  blank <- paste0(strrep(" ", W), " |")
  text <- c(strrep("-", W + 2L + vw),
            if (!is.null(top)) paste0(blank, top),
            paste0(st_pad(st_abbrev_name(stub_head, W), W), " |", hdr),
            rule)
  # grid for HTML/DOCX
  g_stub <- character()
  g_body <- list()
  g_rules <- integer()
  cells_txt <- function(cells) paste0(st_pad(cells, widths), collapse = "")
  g_add <- function(stub, cells) {
    g_stub <<- c(g_stub, stub)
    g_body[[length(g_body) + 1L]] <<- c(cells, rep("", length(widths) - length(cells)))
  }
  status_cells <- function(r) {
    if (r$status == "notest") c(".", st_ct_status_text[[r$status]]) else
      if (r$status %in% c("exposure", "offset")) c("1", st_ct_status_text[[r$status]]) else
        c(one, st_ct_status_text[[r$status]])
  }
  row_text <- function(r) {
    if (r$status %in% names(st_ct_status_text)) {
      sc <- status_cells(r)
      paste0(st_pad(sc[1], 11L), "  ", sc[2])
    } else cells_txt(r$cells)
  }
  row_grid <- function(r) if (r$status %in% names(st_ct_status_text)) status_cells(r) else r$cells

  for (e in seq_along(eqs)) {
    eq <- eqs[[e]]
    if (e > 1L) {
      text <- c(text, rule)
      g_rules <- c(g_rules, length(g_stub))
    }
    if (!is.null(eq$name)) {
      name <- st_abbrev_name(eq$name, W)
      text <- c(text, paste0(st_pad(name, W, "left"), " |", if (isTRUE(eq$base)) "  (base outcome)"))
      g_add(eq$name, if (isTRUE(eq$base)) c("(base outcome)") else character())
      if (isTRUE(eq$base)) next
    }
    terms <- eq$terms
    first <- is.null(eq$name) || TRUE
    last_blank <- TRUE  # no separator at the top of an equation
    for (ti in seq_along(terms)) {
      t <- terms[[ti]]
      shown <- Filter(function(r) r$status != "base", t$rows)
      if (!length(shown) && t$kind == "factor") next
      fancy <- t$kind %in% c("factor", "cint")
      if (fancy && !last_blank) text <- c(text, blank)
      last_blank <- FALSE
      if (t$kind == "plain") {
        r <- shown[[1]]
        # labels of ancillary parameters (var(e.y) of tobit) reach up to the bar
        text <- c(text, if (isTRUE(t$wide)) paste0(st_pad(t$label, W + 1L), "|", row_text(r)) else
          paste0(st_pad(st_abbrev_name(t$label, W), W), " |", row_text(r)))
        g_add(t$label, row_grid(r))
        next
      }
      lines <- st_ct_split(t$label, W)
      if (t$kind == "cint") {
        r <- shown[[1]]
        hl <- lines[-length(lines)]
        text <- c(text, vapply(hl, st_ct_stub_line, "", W = W),
                  paste0(st_pad(lines[length(lines)], W), " |", row_text(r)))
        g_add(t$label, row_grid(r))
      } else {
        text <- c(text, vapply(lines, st_ct_stub_line, "", W = W))
        g_add(t$label, character())
        for (r in shown) {
          lab <- r$label
          # long labels of interactions are broken after "#"
          if (st_width(lab) > W - 1L && grepl("#", lab, fixed = TRUE)) {
            pcs <- strsplit(lab, "#", fixed = TRUE)[[1]]
            first <- st_ct_trunc(pcs[-length(pcs)], W - 2L)
            text <- c(text, paste0(st_pad(paste0(first, " #"), W + 1L), "|"))
            lab <- pcs[length(pcs)]
          }
          text <- c(text, paste0(st_pad(st_ct_trunc(lab, W - 1L), W - 1L), "  |", row_text(r)))
          g_add(paste0("  ", r$label), row_grid(r))
        }
      }
      more <- ti < length(terms)
      if (more && (t$kind == "cint" || isTRUE(t$inter) || length(shown) > 1L)) {
        text <- c(text, blank)
        last_blank <- TRUE
      }
    }
  }
  text <- c(text, strrep("-", W + 2L + vw))
  head_cells <- c(est_lab, "Std. Err.", stat, sprintf("P>|%s|", stat),
                  if (beta) "Beta" else c(sprintf("[%s%% Conf.", format(level)), "Interval]"))
  header <- if (is.null(top)) matrix(head_cells, 1L) else
    rbind(c("", trimws(top), rep("", length(head_cells) - 2L)), head_cells)
  body <- do.call(rbind, g_body)
  if (is.null(body)) body <- matrix("", 0L, length(widths))
  part <- st_part_grid(stub = g_stub, body = body, header = header,
                       stub_header = c(if (!is.null(top)) "", stub_head),
                       rules = c(0L, g_rules), frame = TRUE)
  part$text <- st_rtrim(text)
  part
}
