# Models estimated with R for s.etable() -------------------------------------
#
# Every model becomes one column in the same form as a model of s.regress():
# `m$D$rows` (one entry per row with variable, level, type, col and, for
# further equations, block), `m$ok`, `m$coefs` (estimate, se, stat, p, low,
# high of the estimated coefficients) and `m$stats`.
#
# lm() and glm() are read directly, so that confidence intervals (Wald) and
# model statistics follow Stata. All other model classes are read with
# parameters::model_parameters(), the same source that sjPlot::tab_model()
# uses; model statistics then come from insight and performance.

st_etable_from_r <- function(x, level) {
  native <- class(x)[1] %in% c("lm", "glm")
  res <- if (native) st_etable_native(x, level) else st_etable_parameters(x, level)
  mf <- tryCatch(stats::model.frame(x), error = function(e) NULL)
  if (is.null(mf) && requireNamespace("insight", quietly = TRUE)) {
    mf <- tryCatch(insight::get_data(x, verbose = FALSE), error = function(e) NULL)
  }
  y <- st_etable_response(x, mf)
  labels <- if (!is.null(mf)) {
    vapply(names(mf), function(v) st_var_label(mf[[v]]), character(1))
  } else character()
  # the model frame loses the label of factors whose unused levels were
  # dropped (e.g. haven::as_factor() with labels of missing-value codes), so
  # missing labels are taken from the data the model was estimated with
  if (any(!nzchar(labels))) {
    dat <- tryCatch(eval(stats::getCall(x)$data, environment(stats::formula(x))),
                    error = function(e) NULL)
    if (!is.data.frame(dat) && requireNamespace("insight", quietly = TRUE)) {
      dat <- tryCatch(insight::get_data(x, verbose = FALSE), error = function(e) NULL)
    }
    for (v in intersect(names(labels)[!nzchar(labels)], names(dat))) {
      labels[[v]] <- st_var_label(dat[[v]])
    }
  }
  rows <- st_etable_rows(res$params, res$blocks, mf)
  ylab <- if (!is.na(y) && y %in% names(labels) && nzchar(labels[[y]])) labels[[y]] else y
  list(
    m = list(D = list(rows = rows), ok = res$ok, coefs = res$coefs, stats = res$stats,
             label = NA_character_),
    spec = list(y = y, labels = labels, level = level, or = FALSE, vce = "ols"),
    labels = labels, kind = res$kind,
    heading = if (!is.na(ylab) && nzchar(ylab)) ylab else class(x)[1]
  )
}

st_etable_response <- function(x, mf) {
  y <- if (requireNamespace("insight", quietly = TRUE)) {
    tryCatch(insight::find_response(x)[1], error = function(e) NULL)
  }
  if (is.null(y) || is.na(y)) {
    y <- tryCatch(all.vars(stats::formula(x))[1], error = function(e) NA_character_)
  }
  # as written in the formula, e.g. "log(mpg)" (the column name of the model frame)
  y <- as.character(y)
  if (!is.na(y) && !is.null(mf) && length(names(mf)) && !y %in% names(mf) &&
      grepl(y, names(mf)[1], fixed = TRUE)) {
    y <- names(mf)[1]
  }
  y
}

# lm() and glm(): coefficients from summary(), Wald confidence intervals with
# the t distribution (lm, glm with estimated dispersion) or the normal
# distribution (binomial, Poisson), as in Stata.
st_etable_native <- function(x, level) {
  s <- summary(x)
  b <- stats::coef(x)
  ok <- !is.na(b)
  ct <- s$coefficients[names(b)[ok], , drop = FALSE]
  z <- colnames(ct)[3] == "z value"
  crit <- if (z) stats::qnorm(1 - (1 - level / 100) / 2) else
    stats::qt(1 - (1 - level / 100) / 2, stats::df.residual(x))
  est <- unname(ct[, 1])
  se <- unname(ct[, 2])
  coefs <- list(estimate = est, se = se, stat = unname(ct[, 3]), p = unname(ct[, 4]),
                low = est - crit * se, high = est + crit * se)
  ll <- tryCatch(as.numeric(stats::logLik(x)), error = function(e) NA_real_)
  if (inherits(x, "glm")) {
    fam <- x$family$family
    stats <- list(N = stats::nobs(x), ll = ll)
    if (fam %in% c("binomial", "poisson")) {
      # LR test against the constant-only model and McFadden's pseudo R2
      chi2 <- x$null.deviance - x$deviance
      stats$chi2 <- chi2
      stats$ll_0 <- ll - chi2 / 2
      stats$r2_p <- if (x$df.null > x$df.residual) 1 - ll / stats$ll_0 else NA_real_
    } else if (fam == "gaussian" && x$family$link == "identity") {
      stats$r2 <- 1 - x$deviance / x$null.deviance
    }
  } else {
    f <- s$fstatistic
    stats <- list(N = stats::nobs(x), r2 = s$r.squared, r2_a = s$adj.r.squared,
                  F = if (is.null(f)) NA_real_ else unname(f[1]), rmse = s$sigma, ll = ll)
  }
  params <- data.frame(name = names(b), stringsAsFactors = FALSE)
  params$col <- seq_len(nrow(params))
  list(params = params, blocks = rep(NA_character_, length(b)), ok = ok, coefs = coefs,
       stats = stats, kind = if (z) "z" else "t")
}

# Display names of further equations (components) as reported by parameters.
st_etable_block_titles <- c(
  zero_inflated = "Zero-inflation", zi = "Zero-inflation", zero = "Zero-inflation",
  alpha = "Cut points", intercept = "Cut points", dispersion = "Dispersion",
  precision = "Precision", phi = "Precision", scale = "Scale", sigma = "Sigma",
  nu = "Nu", tau = "Tau", random = "Random effects"
)
st_etable_main <- c("conditional", "beta", "location", "mean", "count", "fixed", "mu", "main")

# All other models: parameters::model_parameters() (fixed effects only).
st_etable_parameters <- function(x, level) {
  for (p in c("parameters", "insight")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      rlang::abort(sprintf(paste("Models of class '%s' need the package 'parameters'",
                                 "(as sjPlot::tab_model() does). Install with install.packages(\"parameters\")."),
                           class(x)[1]))
    }
  }
  mp <- tryCatch(parameters::model_parameters(x, ci = level / 100, effects = "fixed", verbose = FALSE),
                 error = function(e) NULL)
  if (is.null(mp)) {
    mp <- tryCatch(parameters::model_parameters(x, ci = level / 100, verbose = FALSE),
                   error = function(e) {
                     rlang::abort(sprintf("Models of class '%s' are not supported: %s",
                                          class(x)[1], conditionMessage(e)))
                   })
  }
  mp <- as.data.frame(mp)
  if ("Effects" %in% names(mp)) mp <- mp[is.na(mp$Effects) | mp$Effects == "fixed", , drop = FALSE]
  est_col <- intersect(c("Coefficient", "Median", "Mean", "MAP", "Estimate"), names(mp))[1]
  if (is.na(est_col)) rlang::abort(sprintf("No coefficients found for models of class '%s'.", class(x)[1]))
  # terms without a coefficient (e.g. smooth terms of mgcv::gam()) are left out
  mp <- mp[!is.na(mp[[est_col]]), , drop = FALSE]
  if (!nrow(mp)) rlang::abort(sprintf("No coefficients found for models of class '%s'.", class(x)[1]))
  col_or_na <- function(nm) if (!is.na(nm) && nm %in% names(mp)) as.numeric(mp[[nm]]) else rep(NA_real_, nrow(mp))
  stat_col <- intersect(c("t", "z", "t / F", "Chi2", "F", "W"), names(mp))[1]

  comp <- if ("Component" %in% names(mp)) as.character(mp$Component) else rep(NA_character_, nrow(mp))
  resp <- if ("Response" %in% names(mp)) as.character(mp$Response) else rep(NA_character_, nrow(mp))
  block <- ifelse(is.na(comp) | comp %in% st_etable_main, NA_character_, comp)
  title <- ifelse(block %in% names(st_etable_block_titles), st_etable_block_titles[block], block)
  # several outcomes (e.g. nnet::multinom()): one equation per outcome
  if (any(!is.na(resp)) && length(unique(resp)) > 1L) {
    title <- ifelse(is.na(block), paste("Outcome", resp), paste0("Outcome ", resp, ": ", title))
    block <- ifelse(is.na(block), resp, paste(resp, block, sep = ":"))
  }
  name <- as.character(mp$Parameter)
  # pscl::zeroinfl() and hurdle() prefix the terms with their equation
  if (inherits(x, c("zeroinfl", "hurdle"))) name <- sub("^(count|zero)_", "", name)

  coefs <- list(estimate = col_or_na(est_col), se = col_or_na("SE"), stat = col_or_na(stat_col),
                p = col_or_na("p"), low = col_or_na("CI_low"), high = col_or_na("CI_high"))
  stats <- st_etable_r_stats(x)
  params <- data.frame(name = name, title = unname(title), stringsAsFactors = FALSE)
  params$col <- seq_len(nrow(params))
  list(params = params, blocks = block, ok = rep(TRUE, nrow(mp)), coefs = coefs, stats = stats,
       kind = if (identical(stat_col, "t")) "t" else "z")
}

# Model statistics of other models (insight and performance), NA where a
# model does not have them.
st_etable_r_stats <- function(x) {
  quiet <- function(expr) tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  num <- function(v) if (is.null(v) || !length(v) || !is.numeric(unlist(v)[1])) NA_real_ else as.numeric(unlist(v)[1])
  n <- num(quiet(insight::n_obs(x)))
  if (is.na(n)) n <- num(quiet(stats::nobs(x)))
  out <- list(N = n, ll = num(quiet(stats::logLik(x))))
  if (requireNamespace("performance", quietly = TRUE)) {
    r2 <- quiet(performance::r2(x, verbose = FALSE))
    if (!is.null(r2)) {
      # exact names: `$` would match "R2" to "R2_Nagelkerke"
      get <- function(nm) num(r2[[nm, exact = TRUE]])
      out$r2 <- get("R2")
      out$r2_a <- get("R2_adjusted")
      out$r2_m <- get("R2_marginal")
      out$r2_c <- get("R2_conditional")
      out$r2_p <- get("R2_McFadden")
      out$r2_n <- get("R2_Nagelkerke")
    }
    if (is.null(out$r2_p) || is.na(out$r2_p)) out$r2_p <- num(quiet(performance::r2_mcfadden(x)$R2_McFadden))
  }
  out
}

# Rows of the table from the coefficient names: the constant, categories of
# factors (heading row, base category, one row per category) and further
# equations (heading row per equation).
st_etable_rows <- function(params, blocks, mf) {
  xlev <- list()
  if (!is.null(mf)) {
    for (v in names(mf)) {
      col <- mf[[v]]
      if (is.factor(col)) xlev[[v]] <- levels(col)
      else if (is.character(col)) xlev[[v]] <- sort(unique(col[!is.na(col)]))
      else if (is.logical(col)) xlev[[v]] <- c("FALSE", "TRUE")
    }
  }
  # longer variable names first, so that "ab" + "1" wins over "a" + "b1"
  xlev <- xlev[order(-nchar(names(xlev)))]
  split <- function(nm) {
    for (v in names(xlev)) {
      for (l in xlev[[v]]) if (identical(nm, paste0(v, l))) return(c(v, l))
    }
    NULL
  }
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- list(...)
  seen_block <- character()
  seen_var <- character()
  for (i in seq_len(nrow(params))) {
    b <- blocks[i]
    if (!is.na(b) && !b %in% seen_block) {
      seen_block <- c(seen_block, b)
      title <- if (!is.null(params$title) && !is.na(params$title[i])) params$title[i] else b
      add(variable = b, level = NA_character_, type = "block", col = NA_integer_, block = b, title = title)
    }
    nm <- params$name[i]
    if (nm %in% c("(Intercept)", "Intercept")) {
      add(variable = "_cons", level = NA_character_, type = "coef", col = params$col[i], block = b)
      next
    }
    sp <- split(nm)
    if (is.null(sp)) {
      add(variable = nm, level = NA_character_, type = "coef", col = params$col[i], block = b)
      next
    }
    vkey <- paste(b, sp[1])
    if (!vkey %in% seen_var) {
      seen_var <- c(seen_var, vkey)
      add(variable = sp[1], level = NA_character_, type = "header", col = NA_integer_, block = b)
      # treatment contrasts: the first category is the base
      others <- vapply(xlev[[sp[1]]][-1L], function(l) paste0(sp[1], l) %in% params$name, logical(1))
      if (!paste0(sp[1], xlev[[sp[1]]][1]) %in% params$name && any(others)) {
        add(variable = sp[1], level = xlev[[sp[1]]][1], type = "base", col = NA_integer_, block = b)
      }
    }
    add(variable = sp[1], level = sp[2], type = "coef", col = params$col[i], block = b)
  }
  rows
}
