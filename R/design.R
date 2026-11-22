# Model design -----------------------------------------------------------------
# Terms, factor variables and interactions as in Stata's factor-variable
# notation, the design matrix of an estimation sample and its rebuilding for
# other values of the variables (margins, predict).
#
# A term is a character vector of variable names: "x" (main effect) or
# c("x", "g") (the interaction x#g). Every column of the design matrix is a
# product of factors: the value of a continuous variable or the indicator of
# one category of a categorical variable; the constant has no factor.

# Interactions: "x#g" (interaction only) or "x##g" (interaction and all
# lower-order terms). Whether a variable enters as factor or as numeric
# follows from its type (factor and character variables are factors); Stata's
# prefixes "i." (factor) and "c." (numeric) are understood as well.
st_parse_interact <- function(interact, data) {
  if (is.null(interact)) return(list())
  if (!is.character(interact)) {
    rlang::abort("`interact` must be a character vector such as \"x#g\" or \"c.x##i.g\".")
  }
  lapply(interact, function(s) {
    s0 <- gsub("[[:space:]]", "", s)
    parts <- strsplit(s0, "#+")[[1]]
    single <- length(parts) == 1L && grepl("^([ic]|ib[^.]+)\\.", s0)
    if ((length(parts) < 2L && !single) || any(!nzchar(parts)) || grepl("#{3,}", s0)) {
      rlang::abort(sprintf("Cannot read the interaction \"%s\"; use e.g. \"x#g\" or \"c.x##i.g\".", s))
    }
    # ib#. sets the base category, as in Stata (ib3.rep78)
    bases <- ifelse(grepl("^ib[^.]+\\.", parts), sub("^ib([^.]+)\\..*$", "\\1", parts), NA_character_)
    parts <- sub("^ib[^.]+\\.", "i.", parts)
    pre <- ifelse(grepl("^[ic]\\.", parts), substr(parts, 1L, 1L), "")
    vars <- sub("^[ic]\\.", "", parts)
    miss <- setdiff(vars, names(data))
    if (length(miss)) {
      rlang::abort(sprintf("Variable(s) %s of the interaction \"%s\" not found.",
                           paste0("`", miss, "`", collapse = ", "), s))
    }
    list(vars = vars, prefix = pre, full = grepl("##", s0, fixed = TRUE),
         base = stats::setNames(bases, vars)[!is.na(bases)])
  })
}

# Regressors of `...`: variable selections (tidyselect) and, as strings,
# terms in Stata's factor-variable notation ("i.rep78", "c.mpg##i.foreign"),
# in the order given. Returns a list of character vectors (variable names)
# and parsed terms (lists, see st_parse_interact()).
st_fv_items <- function(quos, data, env, y_name) {
  out <- list()
  for (q in quos) {
    a <- rlang::quo_get_expr(q)
    if (is.character(a)) {
      for (s in a) {
        if (grepl("#", s, fixed = TRUE)) {
          out <- c(out, st_parse_interact(s, data))
        } else if (grepl("^([ic]|ib[^.]+)\\.", s)) {
          out <- c(out, st_parse_interact(s, data))
        } else {
          out[[length(out) + 1L]] <- setdiff(names(tidyselect::eval_select(s, data, env = env)), y_name)
        }
      }
    } else {
      out[[length(out) + 1L]] <- setdiff(names(tidyselect::eval_select(q, data)), y_name)
    }
  }
  out
}

# Terms in Stata's order: the regressors as given, an interaction with "##"
# expanded to its lower-order terms and the interaction itself. Duplicates
# are dropped.
st_make_terms <- function(items) {
  terms <- list()
  key <- function(t) paste(sort(t), collapse = "#")
  have <- character()
  add <- function(t) {
    if (!key(t) %in% have) {
      terms[[length(terms) + 1L]] <<- t
      have <<- c(have, key(t))
    }
  }
  for (it in items) {
    if (is.character(it)) {
      for (v in it) add(v)
      next
    }
    v <- it$vars
    if (it$full) {
      for (k in seq_along(v)) {
        for (s in utils::combn(seq_along(v), k, simplify = FALSE)) add(v[s])
      }
    } else {
      add(v)
    }
  }
  terms
}

# TRUE if the output uses Stata's factor-variable notation, i.e. "c." for
# numeric variables in interactions (options(statsalike.stata_notation = TRUE)).
# By default statsalike names the variables without prefixes; how each one
# enters an interaction is noted below the output (st_inter_notes()).
st_stata_notation <- function() isTRUE(getOption("statsalike.stata_notation", FALSE))

st_cprefix <- function() if (st_stata_notation()) "c." else ""

# Label of a term: categorical variables first, then the numeric ones, e.g.
# "foreign#mpg" ("foreign#c.mpg" in Stata's notation) or "mpg#weight".
st_term_label <- function(term, is_cat) {
  if (length(term) == 1L) return(term)
  cat_t <- unlist(is_cat[term])
  conts <- term[!cat_t]
  paste(c(term[cat_t], if (length(conts)) paste0(st_cprefix(), conts)), collapse = "#")
}

# Notes below the output on how the variables enter the interactions, e.g.
# "Note: rep78#mpg = rep78 (factor) x mpg (numeric)". None in Stata's notation,
# where the prefixes say it. Dates and times (`dates`, see
# st_datetime_unit()) get a note on their unit and origin in any notation.
st_inter_notes <- function(terms, is_cat, dates = NULL) {
  out <- character()
  for (v in names(dates)) {
    out <- c(out, st_wrap_par(sprintf("Note: %s enters as %s.", v, dates[[v]]), 0L, 6L))
  }
  if (st_stata_notation()) return(out)
  for (t in terms) {
    if (length(t) < 2L) next
    cat_t <- unlist(is_cat[t])
    ord <- c(t[cat_t], t[!cat_t])
    kinds <- ifelse(unlist(is_cat[ord]), "factor", "numeric")
    txt <- sprintf("Note: %s = %s", st_term_label(t, is_cat), paste0(ord, " (", kinds, ")", collapse = " x "))
    if (length(unique(t)) == 1L) txt <- paste0(txt, ", the square of ", t[1])
    out <- c(out, st_wrap_par(paste0(txt, "."), 0L, 6L))
  }
  unique(out)
}

# Dates (Date), times (POSIXct, POSIXlt) and time differences (difftime) are
# numeric variables of the model: st_num() turns them into the number R
# stores, counted from R's origin 1970-01-01 (Stata counts from 1960-01-01,
# so the constant differs from Stata's, the slope does not).
st_is_datetime <- function(x) inherits(x, c("Date", "POSIXt", "difftime"))

st_datetime_unit <- function(x) {
  if (inherits(x, "Date")) return("days since 1970-01-01 (Stata: since 1960-01-01)")
  if (inherits(x, "POSIXt")) return("seconds since 1970-01-01 00:00:00 UTC (Stata: milliseconds since 1960)")
  sprintf("a time difference in %s", units(x))
}

# Hint for numeric variables with value labels (e.g. imported from Stata or
# SPSS) that enter as numeric variables although they may be meant as
# categories. Shown once per variable and session if at least two labelled
# values occur; options(statsalike.labelled_hint = FALSE) turns it off.
st_labelled_hint <- function(data, vars) {
  if (!isTRUE(getOption("statsalike.labelled_hint", TRUE))) return(invisible())
  for (v in vars) {
    x <- data[[v]]
    labs <- attr(x, "labels", exact = TRUE)
    if (is.null(labs) || is.factor(x) || !is.numeric(st_strip(x))) next
    vals <- st_num(x)
    if (sum(unname(unclass(labs)) %in% vals[!is.na(vals)]) < 2L) next
    rlang::inform(
      sprintf(paste0("`%s` has value labels but enters the model as a numeric variable (Stata: c.%s). ",
                     "To enter it as a factor (Stata: i.%s), use `categorical = %s` or haven::as_factor(). ",
                     "This hint is shown once per session; options(statsalike.labelled_hint = FALSE) ",
                     "turns it off."), v, v, v, v),
      .frequency = "once", .frequency_id = paste0("statsalike_labelled_hint_", v)
    )
  }
  invisible()
}

# Variable information of the estimation sample: categories, their codes
# (values as in Stata, used in notes) and the base category.
st_var_info <- function(x, name, is_cat, base = NULL) {
  if (!is_cat) return(list(name = name, cat = FALSE))
  f <- droplevels(st_as_levels(x))
  lv <- levels(f)
  raw <- st_strip(x)
  codes <- if (is.numeric(raw) && !is.factor(x)) {
    v <- sort(unique(raw[!is.na(raw)]))
    st_format_values(v)[seq_along(lv)]
  } else {
    as.character(seq_along(lv))
  }
  b <- 1L
  if (!is.null(base)) {
    b <- match(as.character(base), lv)
    if (is.na(b)) b <- match(as.character(base), codes)
    if (is.na(b)) {
      rlang::abort(sprintf("The base category %s of `%s` does not occur in the estimation sample.",
                           as.character(base), name))
    }
  }
  list(name = name, cat = TRUE, levels = lv, codes = codes, base = b)
}

# Values of the model variables for the design: continuous variables as
# numbers, categorical ones as indicator matrices (one column per category).
st_vals <- function(info, data) {
  lapply(stats::setNames(names(info), names(info)), function(v) {
    x <- data[[v]]
    if (!info[[v]]$cat) return(st_num(x))
    lab <- as.character(st_as_levels(x))
    k <- match(lab, info[[v]]$levels)
    M <- matrix(0, length(k), length(info[[v]]$levels))
    ok <- !is.na(k)
    M[cbind(which(ok), k[ok])] <- 1
    M[!ok, ] <- NA
    M
  })
}

# Columns (cells) of the terms. Every cell holds its factors as a list of
# c(variable, category index) (category NA for continuous variables). A cell
# is a base cell if it cannot be estimated together with the lower-order terms
# in the model: main effects always drop their base category; a cell of an
# interaction is a base cell if a lower-order term (or the constant) that the
# model contains already covers it, as in Stata.
st_cells <- function(terms, info, has_cons) {
  key <- function(t) paste(sort(t), collapse = "#")
  keys <- vapply(terms, key, character(1))
  out <- list()
  for (ti in seq_along(terms)) {
    t <- terms[[ti]]
    cats <- t[vapply(t, function(v) info[[v]]$cat, logical(1))]
    conts <- t[!t %in% cats]
    # Stata lists the categorical variables first
    ord <- c(cats, conts)
    grid <- if (length(cats)) {
      as.matrix(expand.grid(rev(lapply(cats, function(v) seq_along(info[[v]]$levels))),
                            KEEP.OUT.ATTRS = FALSE))[, rev(seq_along(cats)), drop = FALSE]
    } else matrix(integer(), 1L, 0L)
    for (r in seq_len(nrow(grid))) {
      lev <- grid[r, ]
      at_base <- vapply(seq_along(cats), function(i) lev[i] == info[[cats[i]]]$base, logical(1))
      base <- FALSE
      if (any(at_base)) {
        if (length(t) == 1L) {
          base <- TRUE
        } else {
          S <- cats[at_base]
          for (k in seq_along(S)) {
            for (sub in utils::combn(seq_along(S), k, simplify = FALSE)) {
              rest <- t[!t %in% S[sub]]
              if ((!length(rest) && has_cons) || (length(rest) && key(rest) %in% keys)) base <- TRUE
            }
          }
        }
      }
      factors <- c(lapply(seq_along(cats), function(i) list(var = cats[i], level = lev[i])),
                   lapply(conts, function(v) list(var = v, level = NA_integer_)))
      out[[length(out) + 1L]] <- list(term = ti, factors = factors, base = base,
                                      levels = lev, cats = cats, conts = conts)
    }
  }
  out
}

# Value of one cell for given variable values.
st_cell_value <- function(cell, vals, n) {
  v <- rep(1, n)
  for (f in cell$factors) {
    v <- v * if (is.na(f$level)) vals[[f$var]] else vals[[f$var]][, f$level]
  }
  v
}

# Derivative of one cell with respect to the continuous variable `x`.
st_cell_deriv <- function(cell, vals, n, x) {
  fs <- cell$factors
  out <- rep(0, n)
  for (i in seq_along(fs)) {
    if (!is.na(fs[[i]]$level) || fs[[i]]$var != x) next
    v <- rep(1, n)
    for (j in seq_along(fs)[-i]) {
      f <- fs[[j]]
      v <- v * if (is.na(f$level)) vals[[f$var]] else vals[[f$var]][, f$level]
    }
    out <- out + v
  }
  out
}

# Design matrix (all estimated cells and the constant) for variable values.
st_X <- function(D, vals, n = NULL) {
  n <- n %||% D$n
  cols <- lapply(D$cols, function(cl) if (is.null(cl)) rep(1, n) else st_cell_value(cl, vals, n))
  matrix(unlist(cols), nrow = n)
}

st_dX <- function(D, vals, x, n = NULL) {
  n <- n %||% D$n
  cols <- lapply(D$cols, function(cl) if (is.null(cl)) rep(0, n) else st_cell_deriv(cl, vals, n, x))
  matrix(unlist(cols), nrow = n)
}

# Stata's name of a cell in notes, e.g. "5.rep78", "1b.rep78#1.foreign",
# "c.mpg#c.weight".
st_cell_name <- function(cell, info) {
  parts <- vapply(cell$factors, function(f) {
    if (is.na(f$level)) {
      if (length(cell$factors) == 1L) f$var else paste0(st_cprefix(), f$var)
    } else {
      inf <- info[[f$var]]
      paste0(inf$codes[f$level], if (f$level == inf$base) "b", ".", f$var)
    }
  }, character(1))
  paste(parts, collapse = "#")
}

# Design of one estimation sample ---------------------------------------------

st_design <- function(set, idx, noconstant) {
  d <- set$data[idx, , drop = FALSE]
  if (set$kind %in% c("mlogit", "ologit")) {
    # outcomes may also be factors or labelled values
    y <- as.numeric(as.integer(st_as_levels(d[[set$y_name]])))
  } else {
    if (!st_is_summable(d[[set$y_name]])) {
      rlang::abort(sprintf("The dependent variable `%s` must be numeric.", set$y_name))
    }
    y <- st_num(d[[set$y_name]])
  }
  w <- set$w[idx]
  keep <- !is.na(y) & !is.na(w) & w != 0
  clus <- if (!is.null(set$clus)) set$clus[idx]
  if (!is.null(clus)) keep <- keep & !st_is_missing(clus)
  vars <- unique(unlist(set$terms))
  for (v in vars) {
    if (!set$is_cat[[v]] && !st_is_summable(d[[v]]) && !st_is_datetime(d[[v]])) {
      rlang::abort(sprintf("Variable `%s` is neither numeric nor categorical.", v))
    }
    keep <- keep & !st_is_missing(d[[v]])
  }
  if (!is.null(set$offset)) {
    off <- set$offset[idx]
    keep <- keep & !is.na(off)
  }
  n <- sum(keep)
  if (n == 0L) rlang::abort("No observations.")
  dk <- d[keep, , drop = FALSE]
  info <- lapply(stats::setNames(vars, vars), function(v) {
    st_var_info(dk[[v]], v, set$is_cat[[v]], set$base[[v]])
  })
  vals <- st_vals(info, dk)
  cells <- st_cells(set$terms, info, !noconstant)
  # estimated cells become columns; base cells are shown but not estimated
  cols <- list()
  cell_col <- integer(length(cells))
  for (i in seq_along(cells)) {
    if (cells[[i]]$base) {
      cell_col[i] <- NA_integer_
    } else {
      cols[[length(cols) + 1L]] <- cells[[i]]
      cell_col[i] <- length(cols)
    }
  }
  cons <- NULL
  if (!noconstant) {
    cols[length(cols) + 1L] <- list(NULL)
    cons <- length(cols)
  }
  if (!length(cols)) rlang::abort("The model has no regressors.")
  D <- list(n = n, info = info, vals = vals, cells = cells, cell_col = cell_col, cols = cols,
            cons = cons, terms = set$terms, is_cat = set$is_cat,
            idx = idx[keep])
  D$X <- st_X(D, vals, n)
  D$y <- y[keep]
  D$w <- w[keep]
  D$clus <- if (!is.null(clus)) as.character(st_strip(clus)[keep])
  D$offset <- if (!is.null(set$offset)) set$offset[idx][keep]
  D$empty <- st_empty_cells(D)
  D
}

# Cells that identify no observations (all values 0).
st_empty_cells <- function(D) {
  vapply(seq_along(D$cells), function(i) {
    all(st_cell_value(D$cells[[i]], D$vals, D$n) == 0)
  }, logical(1))
}

# Drop observations from a design (e.g. perfect predictors in logit).
st_design_subset <- function(D, keep) {
  D$n <- sum(keep)
  D$vals <- lapply(D$vals, function(v) if (is.matrix(v)) v[keep, , drop = FALSE] else v[keep])
  D$X <- D$X[keep, , drop = FALSE]
  D$y <- D$y[keep]
  D$w <- D$w[keep]
  D$idx <- D$idx[keep]
  if (!is.null(D$clus)) D$clus <- D$clus[keep]
  if (!is.null(D$offset)) D$offset <- D$offset[keep]
  D$empty <- st_empty_cells(D)
  D
}

# Columns that can be estimated: not empty and not collinear with earlier
# columns (Stata omits the later of collinear variables).
st_estimable <- function(D, w, method = c("order", "pivot")) {
  method <- match.arg(method)
  ok <- rep(TRUE, length(D$cols))
  for (i in which(D$empty)) if (!is.na(D$cell_col[i])) ok[D$cell_col[i]] <- FALSE
  if (method == "pivot") return(st_estimable_pivot(D, w, ok))
  # as in Stata, the constant is kept and the other variables are checked
  # against it
  ord <- c(D$cons, setdiff(seq_along(ok), D$cons))
  idx <- ord[ok[ord]]
  if (length(idx)) ok[idx] <- st_independent(D$X[, idx, drop = FALSE], w)
  ok
}

# Collinearity as Stata's regress removes it: the (centered) cross-product
# matrix is swept with the largest remaining pivot first, and the variables
# whose remaining variation is (numerically) zero are omitted. Of collinear
# variables, the one with the smallest scale is therefore dropped (of wt and
# 2 * wt, it is wt), while the maximum-likelihood commands drop the later one.
st_estimable_pivot <- function(D, w, ok) {
  idx <- setdiff(which(ok), D$cons)
  if (!length(idx)) return(ok)
  X <- D$X[, idx, drop = FALSE]
  if (!is.null(D$cons)) X <- sweep(X, 2L, colSums(X * w) / sum(w))
  M <- crossprod(X * sqrt(w))
  d0 <- diag(M)
  done <- rep(FALSE, length(idx))
  keep <- rep(FALSE, length(idx))
  repeat {
    piv <- diag(M)
    cand <- which(!done & d0 > 0 & piv > 1e-13 * d0)
    if (!length(cand)) break
    best <- max(piv[cand])
    k <- cand[piv[cand] >= best * (1 - 1e-10)][1]
    rk <- M[k, ]
    M <- M - outer(M[, k], rk) / rk[k]
    done[k] <- TRUE
    keep[k] <- TRUE
  }
  ok[idx] <- keep
  ok
}

# Rows of the result: for every term a heading (categorical terms) and one row
# per cell, with the type "coef", "base", "empty" or "omitted" (set later).
st_design_rows <- function(D) {
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- list(...)
  for (ti in seq_along(D$terms)) {
    t <- D$terms[[ti]]
    label <- st_term_label(t, D$is_cat)
    in_t <- which(vapply(D$cells, `[[`, integer(1), "term") == ti)
    has_cat <- any(vapply(t, function(v) D$info[[v]]$cat, logical(1)))
    if (has_cat) add(variable = label, level = NA_character_, type = "header", col = NA_integer_,
                     term = ti, cell = NA_integer_)
    for (i in in_t) {
      cl <- D$cells[[i]]
      lev <- if (length(cl$cats)) st_cell_level(D$info, cl$cats, cl$levels) else NA_character_
      type <- if (D$empty[i]) "empty" else if (cl$base) "base" else "coef"
      add(variable = label, level = lev, type = type, col = D$cell_col[i], term = ti, cell = i)
    }
  }
  if (!is.null(D$cons)) {
    add(variable = "_cons", level = NA_character_, type = "coef", col = D$cons,
        term = NA_integer_, cell = NA_integer_)
  }
  rows
}

# Label of a cell of factor variables: the categories joined by "#"
# ("3#Foreign"); categories without value labels are joined by a space ("1 0"),
# as in Stata.
st_cell_level <- function(info, cats, levels) {
  labs <- vapply(seq_along(cats), function(k) info[[cats[k]]]$levels[levels[k]], character(1))
  codes <- vapply(seq_along(cats), function(k) info[[cats[k]]]$codes[levels[k]], character(1))
  paste(labs, collapse = if (length(labs) > 1L && all(labs == codes)) " " else "#")
}

# Command text of the regressors, e.g. "mpg rep78 foreign#mpg" (in Stata's
# notation "mpg i.rep78 i.foreign#c.mpg").
st_terms_cmd <- function(terms, is_cat) {
  if (!st_stata_notation()) return(vapply(terms, paste, character(1), collapse = "#"))
  vapply(terms, function(t) {
    if (length(t) == 1L) return(if (is_cat[[t]]) paste0("i.", t) else t)
    paste(ifelse(unlist(is_cat[t]), paste0("i.", t), paste0("c.", t)), collapse = "#")
  }, character(1))
}
