#' Contrasts and joint tests of factor variables (Stata: `contrast`)
#'
#' Re-implementation of Stata's `contrast` after the estimation commands of
#' statsalike ([s.regress()], [s.logit()], [s.probit()], [s.ologit()],
#' [s.oprobit()], [s.poisson()], [s.nbreg()], [s.tobit()]), with Stata's
#' output in the layout of Stata 15. As in Stata, the contrasts are contrasts
#' of marginal linear predictions with all other factor variables treated as
#' balanced (`asbalanced`); continuous variables keep their observed values.
#'
#' * A factor variable without operator (`"rep78"`) gives the joint test of
#'   its main effect (all categories equal), an interaction (`"rep78#foreign"`)
#'   the joint test of the interaction. With interactions in the model, these
#'   are the tests of Type III sums of squares (as [s.anova()]).
#' * An operator in front of the variable shows the single contrasts with
#'   their tests and, in a second table, their estimates and confidence
#'   intervals:
#'   `r.` (difference from the base category, Stata's default),
#'   `a.` (difference from the next category), `ar.` (difference from the
#'   previous category), `g.` (difference from the mean of all categories),
#'   `h.` (difference from the mean of the following categories, Helmert),
#'   `j.` (difference from the mean of the previous categories, reverse
#'   Helmert), `p.` (orthogonal polynomials in the values of the categories)
#'   and `q.` (orthogonal polynomials in the order of the categories).
#'   Polynomial contrasts are orthonormal, as [stats::contr.poly()].
#'   The base category of `r.` is the base of the model (see `base` in
#'   [s.regress()]).
#' * Operators combine in interactions, e.g. `"r.rep78#r.foreign"`.
#' * `"rep78@foreign"` gives the simple effects: the test of `rep78` within
#'   every category of `foreign` and the joint test of all of them,
#'   `"r.rep78@foreign"` the single contrasts within every category.
#'
#' After [s.regress()] and [s.tobit()] the tests are F tests (the
#' denominator degrees of freedom are shown below the table) and the
#' contrasts have t statistics, after the other models chi-squared and z
#' statistics. The p-values are not adjusted for multiple comparisons
#' (Stata's `mcompare()` is not available). Not available after
#' [s.mlogit()].
#'
#' @param x Result of an estimation command of statsalike (one model, not
#'   grouped).
#' @param ... Terms to contrast, as strings or names, e.g. `"rep78"`,
#'   `r.rep78`, `"a.rep78#foreign"` or `"r.rep78@foreign"`.
#' @param effects Show the test statistics and p-values of the single
#'   contrasts in the second table, as Stata's option `effects`.
#' @param level Confidence level in percent.
#' @param export Optional file name ending in `.html` or `.docx`.
#' @return A tibble of class `st_test` with one row per test, i.e. per
#'   single contrast and per joint test (`contrast = "Joint"`, or the term for
#'   a term without operator), and the columns `term`, `contrast`, `df`, `F`
#'   (after regress and tobit) or `chi2`, `p_value`, `estimate`, `std_error`,
#'   `statistic` (t or z), `conf_low`, `conf_high` and `note`
#'   (`"not estimable"`, `"omitted"`). Estimates are missing for joint tests.
#'   It prints in Stata's layout.
#' @examples
#' df <- mtcars
#' df$cyl <- factor(df$cyl)
#' df$am <- factor(df$am, labels = c("automatic", "manual"))
#' m <- s.regress(df, mpg, cyl, am, wt)
#'
#' # contrast cyl am
#' s.contrast(m, cyl, am)
#'
#' # contrast r.cyl, effects
#' s.contrast(m, r.cyl, effects = TRUE)
#'
#' # contrast ar.cyl (each category against the previous one)
#' s.contrast(m, ar.cyl)
#'
#' # interaction and simple effects
#' m2 <- s.regress(df, mpg, wt, interact = "cyl##am")
#' s.contrast(m2, "cyl#am", "r.am@cyl")
#' @export
s.contrast <- function(x, ..., effects = FALSE, level = 95, export = NULL) {
  m <- st_single_model(x)
  spec <- attr(x, "st_spec", exact = TRUE)
  if (spec$kind == "mlogit") {
    rlang::abort("s.contrast() after mlogit is not supported yet; use s.margins() with `pwcompare`.")
  }
  if (!is.numeric(level) || length(level) != 1L || level <= 10 || level >= 99.99) {
    rlang::abort("`level` must be a number between 10 and 99.99, e.g. 95.")
  }
  if (!isTRUE(effects) && !isFALSE(effects)) rlang::abort("`effects` must be TRUE or FALSE.")
  # names (r.rep78), strings and expressions that give strings; a call that
  # cannot be evaluated is read as written (rep78@foreign)
  terms <- unlist(lapply(rlang::enquos(...), function(q) {
    e <- rlang::quo_get_expr(q)
    if (is.character(e)) return(e)
    if (is.call(e)) {
      val <- tryCatch(rlang::eval_tidy(q), error = function(err) NULL)
      if (is.character(val)) return(val)
    }
    paste(deparse(e), collapse = "")
  }))
  if (!length(terms)) {
    rlang::abort("Please name the factor variables to contrast, e.g. s.contrast(m, \"r.rep78\").")
  }
  D <- m$D
  info <- D$info
  parsed <- lapply(terms, st_contrast_parse, info = info)
  regress <- spec$kind %in% c("regress", "tobit")
  df_r <- if (regress) m$df_t else Inf
  crit <- if (regress) stats::qt(1 - (1 - level / 100) / 2, df_r) else stats::qnorm(1 - (1 - level / 100) / 2)

  blocks <- lapply(parsed, st_contrast_term, m = m, spec = spec)
  rows <- list()
  for (i in seq_along(blocks)) {
    # rows of the result that belong to the term
    blocks[[i]]$at_rows <- length(rows) + seq_along(blocks[[i]]$rows)
    for (r in blocks[[i]]$rows) rows[[length(rows) + 1L]] <- c(list(term = blocks[[i]]$label), r)
  }
  get <- function(name, type = numeric(1)) {
    vapply(rows, function(r) if (is.null(r[[name]])) type[NA_integer_] else r[[name]], type)
  }
  tidy <- tibble::tibble(term = get("term", character(1)), contrast = get("label", character(1)),
                         df = get("df"))
  test <- get("chi2")
  if (regress) tidy$F <- test / tidy$df else tidy$chi2 <- test
  tidy$p_value <- if (regress) stats::pf(test / tidy$df, tidy$df, df_r, lower.tail = FALSE) else
    stats::pchisq(test, tidy$df, lower.tail = FALSE)
  tidy$estimate <- get("estimate")
  tidy$std_error <- get("se")
  tidy$statistic <- tidy$estimate / tidy$std_error
  tidy$conf_low <- tidy$estimate - crit * tidy$std_error
  tidy$conf_high <- tidy$estimate + crit * tidy$std_error
  tidy$note <- get("note", character(1))
  na <- tidy$note != ""
  tidy[na, c("df", if (regress) "F" else "chi2", "p_value", "statistic", "conf_low", "conf_high")] <- NA

  sp <- list(blocks = blocks, regress = regress, df_r = df_r, effects = effects, level = level,
             stat = if (regress) "t" else "z", kind = spec$kind,
             cmd = st_make_cmd("contrast", gsub("[[:space:]]", "", terms), opts = c(if (effects) "effects",
                                                                         if (level != 95) sprintf("level(%s)", level))))
  sp$parts <- st_parts_contrast(tidy, sp)
  res <- st_new_output(tidy, "st_test", st_spec = sp)
  st_export_if(res, export)
}

st_contrast_ops <- c("r", "a", "ar", "g", "h", "j", "p", "q")

# A term "op.x#op.y@g#h": the contrasted factor variables with their operators
# (NA without operator) and the variables of the simple effects.
st_contrast_parse <- function(s, info) {
  s0 <- gsub("[[:space:]]", "", s)
  sides <- strsplit(s0, "@", fixed = TRUE)[[1]]
  if (length(sides) > 2L || !all(nzchar(sides)) || endsWith(s0, "@")) {
    rlang::abort(sprintf("Cannot read the term \"%s\"; use e.g. \"rep78\", \"r.rep78\" or \"r.rep78@foreign\".", s))
  }
  check <- function(v) {
    if (!v %in% names(info)) rlang::abort(sprintf("`%s` (in \"%s\") is not a variable of the model.", v, s))
    if (!info[[v]]$cat) {
      rlang::abort(sprintf("`%s` (in \"%s\") is not a factor variable of the model; contrasts need factor variables.", v, s))
    }
    v
  }
  pieces <- strsplit(sides[1], "#", fixed = TRUE)[[1]]
  if (!all(nzchar(pieces))) rlang::abort(sprintf("Cannot read the term \"%s\".", s))
  vars <- character()
  ops <- character()
  for (p in pieces) {
    p <- sub("^i\\.", "", p)
    op <- NA_character_
    # a variable whose name looks like an operator (e.g. "a.x") wins
    if (!p %in% names(info) && grepl("^[a-z]+\\.", p)) {
      op <- sub("\\..*$", "", p)
      if (!op %in% st_contrast_ops) {
        rlang::abort(sprintf("Unknown contrast operator \"%s.\" in \"%s\" (allowed: %s).", op, s,
                             paste0(st_contrast_ops, ".", collapse = ", ")))
      }
      p <- sub("^[a-z]+\\.", "", p)
    }
    vars <- c(vars, check(p))
    ops <- c(ops, op)
  }
  at <- if (length(sides) == 2L) vapply(sub("^i\\.", "", strsplit(sides[2], "#", fixed = TRUE)[[1]]), check, "") else character()
  if (anyDuplicated(c(vars, at))) rlang::abort(sprintf("A variable appears twice in \"%s\".", s))
  list(vars = vars, ops = ops, at = unname(at),
       label = paste0(paste(vars, collapse = "#"), if (length(at)) paste0("@", paste(at, collapse = "#"))))
}

# Contrast matrix of an operator for K categories (rows: contrasts) with
# Stata's labels.
st_contrast_matrix <- function(op, inf) {
  lv <- inf$levels
  K <- length(lv)
  e <- diag(K)
  b <- inf$base
  vs <- function(i, txt) sprintf("(%s vs %s)", lv[i], txt)
  if (is.na(op)) op <- "r"
  out <- switch(op,
    r = list(C = e[-b, , drop = FALSE] - matrix(e[b, ], K - 1L, K, byrow = TRUE),
             labels = vs(seq_len(K)[-b], lv[b])),
    a = list(C = e[-K, , drop = FALSE] - e[-1L, , drop = FALSE], labels = vs(seq_len(K - 1L), lv[-1L])),
    ar = list(C = e[-1L, , drop = FALSE] - e[-K, , drop = FALSE], labels = vs(seq_len(K)[-1L], lv[-K])),
    g = list(C = e - 1 / K, labels = vs(seq_len(K), "mean")),
    h = list(C = do.call(rbind, lapply(seq_len(K - 1L), function(i) e[i, ] - colMeans(e[(i + 1L):K, , drop = FALSE]))),
             labels = vapply(seq_len(K - 1L), function(i) vs(i, if (i == K - 1L) lv[K] else paste0(">", lv[i])), "")),
    j = list(C = do.call(rbind, lapply(seq_len(K)[-1L], function(i) e[i, ] - colMeans(e[seq_len(i - 1L), , drop = FALSE]))),
             labels = vapply(seq_len(K)[-1L], function(i) vs(i, if (i == 2L) lv[1L] else paste0("<", lv[i])), "")),
    p = , q = {
      # values of the categories: numeric codes for p., the order for q.
      sc <- suppressWarnings(as.numeric(inf$codes))
      if (op == "q" || anyNA(sc) || anyDuplicated(sc)) sc <- seq_len(K)
      nm <- c("linear", "quadratic", "cubic", "quartic")
      list(C = t(stats::contr.poly(K, scores = sc)),
           labels = sprintf("(%s)", ifelse(seq_len(K - 1L) <= 4L, nm[seq_len(K - 1L)], paste0("p", seq_len(K - 1L)))))
    }
  )
  if (K < 2L) out <- list(C = matrix(0, 0L, K), labels = character())
  out
}

# Gradient (with respect to the parameters) of the marginal linear
# prediction for every combination of the categories of `vars` (the first
# varies slowest), all other factor variables balanced. Returns the matrix
# of gradients (one row per combination) and their estimability.
st_contrast_cells <- function(m, spec, vars) {
  D <- m$D
  info <- D$info
  n <- D$n
  vals <- D$vals
  for (v in names(info)) {
    if (info[[v]]$cat && !v %in% vars) {
      K <- length(info[[v]]$levels)
      vals[[v]] <- matrix(1 / K, n, K)
    }
  }
  K <- vapply(vars, function(v) length(info[[v]]$levels), integer(1))
  grid <- as.matrix(expand.grid(rev(lapply(K, seq_len)), KEEP.OUT.ATTRS = FALSE))[, rev(seq_along(K)), drop = FALSE]
  scen <- list()
  G <- do.call(rbind, lapply(seq_len(nrow(grid)), function(r) {
    vv <- vals
    for (k in seq_along(vars)) {
      M <- matrix(0, n, K[k])
      M[, grid[r, k]] <- 1
      vv[[vars[k]]] <- M
    }
    X <- st_X(D, vv, n)
    scen[[r]] <<- list(X = X)
    st_mg_grad(m, spec, m$b, X, NULL, "xb", NA_integer_, D$w)
  }))
  defs <- lapply(seq_len(nrow(grid)), function(r) list(parts = list(list(s = r))))
  list(G = G, grid = grid, ok = st_mg_estimable(m, spec, scen, defs))
}

# Tests and estimates of one term: the rows of the table of tests (single
# contrasts and joint tests) with the contrast estimates.
st_contrast_term <- function(p, m, spec) {
  info <- m$D$info
  all_vars <- c(p$at, p$vars)
  cells <- st_contrast_cells(m, spec, all_vars)
  mats <- lapply(seq_along(p$vars), function(k) st_contrast_matrix(p$ops[k], info[[p$vars[k]]]))
  C <- Reduce(kronecker, lapply(mats, `[[`, "C"))
  clab <- Reduce(function(a, b) as.vector(t(outer(a, b, paste))), lapply(mats, `[[`, "labels"))
  show_each <- any(!is.na(p$ops))
  # blocks of the simple effects: one per combination of the `at` categories
  na <- length(p$at)
  key <- if (na) do.call(paste, c(lapply(seq_len(na), function(k) cells$grid[, k]), sep = "\r")) else rep("", nrow(cells$grid))
  keys <- unique(key)
  V <- m$V
  b <- m$b
  L_all <- list()
  rows <- list()
  # a term that is not in the model has only contrasts of zero
  only_omitted <- TRUE
  untested <- function(w) if (w$df) "" else if (only_omitted) "omitted" else "not estimable"
  wald <- function(L) {
    if (!nrow(L)) return(list(chi2 = NA_real_, df = 0L))
    Vc <- L %*% V %*% t(L)
    q <- qr(Vc, tol = 1e-10)
    if (!q$rank) return(list(chi2 = NA_real_, df = 0L))
    keep <- sort(q$pivot[seq_len(q$rank)])
    list(chi2 = st_wald(drop(L[keep, , drop = FALSE] %*% b), Vc[keep, keep, drop = FALSE]), df = length(keep))
  }
  for (kk in keys) {
    idx <- which(key == kk)
    at_lab <- if (na) {
      paste(vapply(seq_len(na), function(k) info[[p$at[k]]]$levels[cells$grid[idx[1], k]], ""), collapse = " ")
    }
    L <- C %*% cells$G[idx, , drop = FALSE]
    # a contrast is estimable if all margins it uses are
    est_ok <- vapply(seq_len(nrow(C)), function(i) all(cells$ok[idx][abs(C[i, ]) > 1e-12]), logical(1))
    se <- sqrt(pmax(rowSums((L %*% V) * L), 0))
    est <- drop(L %*% b)
    omitted <- est_ok & se <= 1e-12 * max(1, abs(est))
    use <- est_ok & !omitted
    only_omitted <- only_omitted && all(omitted)
    if (show_each) {
      for (i in seq_len(nrow(C))) {
        note <- if (!est_ok[i]) "not estimable" else if (omitted[i]) "omitted" else ""
        rows[[length(rows) + 1L]] <- list(label = paste(c(clab[i], at_lab), collapse = " "), df = 1,
                                          chi2 = (est[i] / se[i])^2, estimate = est[i], se = se[i],
                                          note = note, kind = "single")
      }
    } else if (na) {
      w <- wald(L[use, , drop = FALSE])
      rows[[length(rows) + 1L]] <- list(label = at_lab, df = w$df, chi2 = w$chi2,
                                        note = if (w$df) "" else if (all(omitted)) "omitted" else "not estimable",
                                        kind = "at")
    }
    L_all[[length(L_all) + 1L]] <- L[use, , drop = FALSE]
  }
  w <- wald(do.call(rbind, L_all))
  joint <- list(label = if (show_each || na) "Joint" else p$label, df = w$df, chi2 = w$chi2,
                note = untested(w), kind = "joint")
  rows[[length(rows) + 1L]] <- joint
  list(label = p$label, rows = rows, show_each = show_each, single = !show_each && !na)
}

# Output -----------------------------------------------------------------------

# Width of the label column: at least 12 characters, wide enough for the
# labels of the contrasts up to 32 characters and as long as the table fits
# into the console; longer labels are broken (st_contrast_lines()).
st_contrast_width <- function(labels, vw) {
  W <- max(12L, st_width(labels))
  min(W, max(12L, min(32L, getOption("width", 80L) - 2L - vw)))
}

# Lines of a row: a label wider than the column is broken at spaces into at
# most three lines (the rest ends in ".."), the values stand on its last line.
st_contrast_lines <- function(label, W, values) {
  pieces <- st_wrap_heading(label, W - 1L)
  if (length(pieces) > 3L) pieces <- c(pieces[1:2], st_ct_trunc(paste(pieces[-(1:2)], collapse = " "), W - 1L))
  n <- length(pieces)
  c(if (n > 1L) paste0(st_pad(pieces[-n], W - 1L), "  |"),
    paste0(st_pad(pieces[n], W - 1L), "  |", values))
}

st_parts_contrast <- function(x, sp) {
  head <- c("Contrasts of marginal linear predictions", "", "Margins      : asbalanced")
  parts <- list(st_part_text(head, mono = TRUE), st_contrast_tests(x, sp))
  if (any(vapply(sp$blocks, `[[`, logical(1), "show_each"))) parts <- c(parts, list(st_contrast_estimates(x, sp)))
  parts
}

# Table of tests: df, F (or chi2) and p-value of every contrast and of the
# joint tests, with the denominator degrees of freedom of F tests.
st_contrast_tests <- function(x, sp) {
  stat <- if (sp$regress) "F" else "chi2"
  vw <- 34L
  inner <- x$contrast[x$contrast != x$term & x$contrast != "Joint"]
  W <- st_contrast_width(c(vapply(sp$blocks, `[[`, "", "label"), paste0(inner, " ")), vw)
  blank <- paste0(strrep(" ", W), " |")
  text <- c(strrep("-", W + 2L + vw),
            paste0(blank, "         df", st_pad(stat, 12L), st_pad(sprintf("P>%s", stat), 11L)),
            paste0(strrep("-", W + 1L), "+", strrep("-", vw)))
  g_stub <- character()
  g_body <- list()
  add <- function(stub, cells) {
    g_stub <<- c(g_stub, stub)
    g_body[[length(g_body) + 1L]] <<- c(cells, rep("", 3L - length(cells)))
  }
  cells_of <- function(i) {
    if (x$note[i] != "") return(paste0("  (", x$note[i], ")"))
    c(formatC(x$df[i], format = "d"), sprintf("%.2f", x[[stat]][i]), sprintf("%.4f", x$p_value[i]))
  }
  row_text <- function(i) {
    cl <- cells_of(i)
    if (length(cl) == 1L) cl else paste0(st_pad(cl[1], 11L), st_pad(cl[2], 12L), st_pad(cl[3], 11L))
  }
  i <- 0L
  for (bi in seq_along(sp$blocks)) {
    blk <- sp$blocks[[bi]]
    if (bi > 1L) {
      text <- c(text, blank)
      add("", character())
    }
    nr <- length(blk$rows)
    if (blk$single) {
      i <- i + 1L
      text <- c(text, paste0(st_pad(st_abbrev_name(blk$label, W), W), " |", row_text(i)))
      add(blk$label, cells_of(i))
      next
    }
    text <- c(text, vapply(st_ct_split(blk$label, W), st_ct_stub_line, "", W = W))
    add(blk$label, character())
    for (k in seq_len(nr)) {
      i <- i + 1L
      if (blk$rows[[k]]$kind == "joint") {
        text <- c(text, paste0(st_pad("Joint", W), " |", row_text(i)))
        add("Joint", cells_of(i))
      } else {
        text <- c(text, st_contrast_lines(x$contrast[i], W, row_text(i)))
        add(paste0("  ", x$contrast[i]), cells_of(i))
      }
    }
  }
  if (sp$regress) {
    dtxt <- if (sp$df_r == round(sp$df_r)) formatC(sp$df_r, format = "d") else st_stata_g(sp$df_r, 9L)
    text <- c(text, blank, paste0(st_pad("Denominator", W), " |", st_pad(dtxt, 11L)))
    add("", character())
    add("Denominator", dtxt)
  }
  text <- c(text, strrep("-", W + 2L + vw))
  part <- st_part_grid(stub = g_stub, body = do.call(rbind, g_body),
                       header = matrix(c("df", stat, sprintf("P>%s", stat)), 1L), stub_header = "",
                       rules = 0L, frame = TRUE)
  part$text <- st_rtrim(text)
  part
}

# Table of the single contrasts: estimate, standard error, (with `effects`
# the t or z statistic and p-value) and confidence interval.
st_contrast_estimates <- function(x, sp) {
  eff <- sp$effects
  stat <- sp$stat
  blocks <- Filter(function(b) b$show_each, sp$blocks)
  rows_of <- function(b) b$at_rows[x$contrast[b$at_rows] != "Joint"]
  widths <- if (eff) c(11L, 11L, 9L, 8L, 13L, 12L) else c(11L, 11L, 14L, 12L)
  vw <- sum(widths)
  W <- st_contrast_width(c(vapply(blocks, `[[`, "", "label"),
                           paste0(x$contrast[unlist(lapply(blocks, rows_of))], " ")), vw)
  interval <- sprintf("[%s%% Conf. Interval]", format(sp$level))
  hdr <- if (eff) {
    paste0("   Contrast   Std. Err.", st_pad(stat, 7L), st_pad(sprintf("P>|%s|", stat), 9L), st_pad(interval, 25L))
  } else {
    paste0("   Contrast   Std. Err.", st_pad(interval, 26L))
  }
  blank <- paste0(strrep(" ", W), " |")
  text <- c(strrep("-", W + 2L + vw), paste0(blank, hdr), paste0(strrep("-", W + 1L), "+", strrep("-", vw)))
  g <- function(v) if (is.na(v)) "." else st_stata_g(v, 9L)
  g_stub <- character()
  g_body <- list()
  nc <- length(widths)
  add <- function(stub, cells) {
    g_stub <<- c(g_stub, stub)
    g_body[[length(g_body) + 1L]] <<- c(cells, rep("", nc - length(cells)))
  }
  for (bi in seq_along(blocks)) {
    blk <- blocks[[bi]]
    if (bi > 1L) {
      text <- c(text, blank)
      add("", character())
    }
    text <- c(text, vapply(st_ct_split(blk$label, W), st_ct_stub_line, "", W = W))
    add(blk$label, character())
    for (i in rows_of(blk)) {
      if (x$note[i] != "") {
        cells <- c(if (x$note[i] == "omitted") "0" else ".", sprintf("(%s)", x$note[i]))
        line <- paste0(st_pad(cells[1], 11L), "  ", cells[2])
      } else {
        cells <- c(g(x$estimate[i]), g(x$std_error[i]),
                   if (eff) c(st_fmt_t(x$statistic[i]), sprintf("%.3f", x$p_value[i])),
                   g(x$conf_low[i]), g(x$conf_high[i]))
        line <- paste0(st_pad(cells, widths), collapse = "")
      }
      text <- c(text, st_contrast_lines(x$contrast[i], W, line))
      add(paste0("  ", x$contrast[i]), cells)
    }
  }
  text <- c(text, strrep("-", W + 2L + vw))
  head_cells <- c("Contrast", "Std. Err.", if (eff) c(stat, sprintf("P>|%s|", stat)),
                  sprintf("[%s%% Conf.", format(sp$level)), "Interval]")
  part <- st_part_grid(stub = g_stub, body = do.call(rbind, g_body), header = matrix(head_cells, 1L),
                       stub_header = "", rules = 0L, frame = TRUE)
  part$text <- st_rtrim(text)
  part
}
