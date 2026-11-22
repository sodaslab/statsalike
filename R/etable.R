#' Tables of several estimation results (Stata: `etable`)
#'
#' Re-implementation of Stata's `etable` (Stata 17 and later). The results of
#' several estimation commands of statsalike ([s.regress()], [s.logit()],
#' [s.probit()], [s.poisson()], [s.nbreg()] and the others) are shown side by
#' side, one column per model, in the layout of a publication table. By default each cell shows
#' the coefficient with significance stars and, below it, the standard error
#' in parentheses (Stata's `cstat(_r_b) cstat(_r_se)`). The number of
#' observations and R-squared follow at the bottom.
#'
#' * Row labels are the variable labels (as imported by haven), or the
#'   variable names if there is none. `rowlabels` sets any row label directly,
#'   including spaces and special characters, without renaming variables
#'   (like `pred.labels` in `sjPlot::tab_model()`).
#' * Categorical regressors get a heading row and one row per category. The
#'   base category is hidden unless `showbase = TRUE`, as in Stata.
#' * The constant (`_cons`) is shown last, labeled "Intercept".
#' * For grouped models ([dplyr::group_by()]) each group becomes a column.
#' * Models estimated with `ame = TRUE` and results of [s.margins()] with
#'   `dydx` give a column of average marginal effects. Together with the
#'   coefficients and odds ratios of the same model this gives a table with
#'   three columns, e.g. `s.etable("Log-Odds" = m, "Odds Ratios" = m_or, "AME" = m_ame)`.
#'   Marginal effects are never exponentiated, the notes name the columns
#'   with marginal effects.
#' * Below the table a note tells how the variables of interactions enter the
#'   model (factor or numeric).
#'
#' @section Addons beyond Stata, models estimated with R:
#' Besides [s.regress()] and [s.logit()], `s.etable()` accepts models
#' estimated with R, also mixed with each other in one table. This has no
#' counterpart in Stata, whose `etable` works on Stata's own estimation
#' results only.
#'
#' * `lm()` and `glm()` are read directly (no further packages needed).
#'   Coefficients, standard errors, t/z statistics and p-values as in
#'   `summary()`, Wald confidence intervals (t distribution for `lm()` and
#'   for `glm()` with estimated dispersion, normal distribution for binomial
#'   and Poisson models), N, R-squared, adjusted R-squared, F, root MSE and
#'   log likelihood for `lm()` (for weighted models R's `logLik()`, which
#'   differs from Stata's definition), and for binomial and Poisson `glm()`
#'   the log likelihood,
#'   the likelihood-ratio chi-squared and McFadden's pseudo R-squared, as
#'   Stata's `logit` and `poisson` report them.
#' * All other model classes are read with `parameters::model_parameters()`
#'   (package 'parameters' required), the source that
#'   `sjPlot::tab_model()` uses as well. Every class supported there works,
#'   e.g. `MASS::glm.nb()`, `MASS::polr()`, `nnet::multinom()`,
#'   `ordinal::clm()`, `lme4::lmer()`/`glmer()`, `nlme::lme()`,
#'   `glmmTMB::glmmTMB()`, `survival::coxph()`/`survreg()`,
#'   `betareg::betareg()`, `pscl::zeroinfl()`/`hurdle()`,
#'   `fixest::feols()`, `plm::plm()`, `estimatr::lm_robust()`,
#'   `survey::svyglm()`, `mgcv::gam()`, `quantreg::rq()`, `AER::ivreg()`
#'   and Bayesian models of 'rstanarm' or 'brms'. Model statistics come from
#'   'insight' and 'performance'.
#' * Factor and logical regressors are shown like categorical variables of
#'   [s.regress()]. A heading row with one row per category and the base
#'   category (treatment contrasts) hidden unless `showbase = TRUE`.
#'   Interactions and transformations (`wt:hp`, `I(wt^2)`) keep R's names.
#' * Further equations get their own block with a heading row. The
#'   zero-inflation part of `pscl::zeroinfl()` or `glmmTMB::glmmTMB()`, the
#'   cut points of ordinal models, the outcomes of `nnet::multinom()`. Their
#'   rows are addressed in `rowlabels` as `"equation::term"`, e.g.
#'   `"zero_inflated::wt"`, the heading as `"zero_inflated::"`.
#'
#' Differences to `sjPlot::tab_model()`. Coefficients are shown on the
#' scale of the model (log odds, log counts) unless `eform = TRUE`, whereas
#' `tab_model()` exponentiates logit and count models by default.
#' Confidence intervals of `lm()`/`glm()` are Wald intervals (as in Stata),
#' whereas `tab_model()` uses profile-likelihood intervals for `glm()`. Of
#' mixed models only the fixed effects are shown, the variance components,
#' ICC and number of groups that `tab_model()` adds are not. The standard
#' errors are those the model reports by default (e.g. HC2 for
#' `estimatr::lm_robust()`).
#'
#' @param ... Results of the estimation commands of statsalike or of
#'   [s.margins()], models estimated with R (see the section on addons), or
#'   lists of them. Named arguments set
#'   the column headings, e.g. `s.etable("Model 1" = m1, "Model 2" = m2)`.
#' @param rowlabels Named character vector of row labels. Names are variable
#'   names, `"_cons"` for the constant, `"variable:category"` for a
#'   category of a categorical regressor, or `"equation::term"` for rows of
#'   further equations, e.g.
#'   `c(dw15 = "Working hours per week", "sex:FRAU" = "Women", "_cons" = "Constant")`.
#'   Labels not given here are taken from the variable labels (see
#'   `varlabel`).
#' @param collabels Column headings, one per model. Overrides the names of
#'   `...`. By default the label (or name) of the dependent variable.
#' @param firstcell Text in the empty cell at the top left of the table, e.g.
#'   the dependent variable or a short description of the models
#'   (`firstcell = "Models of income"`). An addition to Stata, whose `etable`
#'   leaves this cell empty. Long texts are shortened with `~` in the console
#'   (not in exported files).
#' @param cstat Statistic shown below each coefficient. `"se"` (standard
#'   error, default), `"ci"` (confidence interval), `"t"` (t or z
#'   statistic), `"p"` (p-value) or `"none"`.
#' @param mstat Model statistics at the bottom, any of `"N"`, `"r2"`,
#'   `"r2_a"`, `"r2_p"` (McFadden's pseudo R-squared), `"r2_n"`
#'   (Nagelkerke's R-squared), `"r2_m"` and `"r2_c"` (marginal and
#'   conditional R-squared of mixed models), `"r2_t"` (Tjur's R-squared of
#'   logit and probit models, the difference of the mean predicted
#'   probabilities of the cases with and without the outcome), `"F"`,
#'   `"chi2"`, `"ll"` (log likelihood), `"rmse"` and `"n_clust"`. `r2_n`,
#'   `r2_m`, `r2_c` and `r2_t` are additions to Stata. Names set the row labels, e.g.
#'   `c("Cases" = "N", "R-squared" = "r2")`. Statistics that a model does not have
#'   are left empty.
#' @param stars Significance stars. `TRUE` (default) for
#'   `* p < 0.05, ** p < 0.01, *** p < 0.001`, `FALSE` for none, or a
#'   decreasing numeric vector of up to three levels.
#' @param varlabel Use variable labels as row labels where `rowlabels` gives
#'   none. With `FALSE`, variable names are used.
#' @param showbase Show the base categories of categorical regressors.
#' @param keep,drop Variable names (or `"_cons"`) to show or to hide.
#' @param eform Show exponentiated coefficients (odds
#'   ratios for logit models) with standard errors by the delta method and exponentiated
#'   confidence limits. Models estimated with `or = TRUE` are already shown
#'   as odds ratios.
#' @param nformat Display format of coefficients and statistics in Stata or
#'   `sprintf()` style. Default `"%9.3f"` (three decimals), e.g. `"%9.2f"`
#'   for two decimals or `"%9.0g"` for Stata's general format. See the
#'   section on display formats in [statsalike-stata-vs-r].
#' @param level Confidence level in percent for models estimated with R.
#'   [s.regress()] and [s.logit()] keep the level they were estimated with.
#' @param title Optional title shown above the table and used as the
#'   heading of exported files.
#' @param notes Notes below the table. `TRUE` (default) for the automatic
#'   notes on the cell statistic and the stars, `FALSE` for none, or a
#'   character vector with your own lines, e.g. in another language.
#' @param export Optional file name ending in `.html` or `.docx` (see
#'   [s.export()]).
#'
#' @return A tibble of class `st_etable` with one row per model and term and the columns
#'   `model` (column heading), `term` (variable, `variable:category` or
#'   `_cons`), `label`, `estimate`, `std_error`, `statistic`, `p_value`,
#'   `conf_low`, `conf_high` and `note` (`"base"` or `"omitted"`). It prints in
#'   the layout of Stata's `etable`.
#'
#' @examples
#' m1 <- s.regress(mtcars, mpg, wt)
#' m2 <- s.regress(mtcars, mpg, wt, hp, am)
#'
#' # etable, estimates(m1 m2)
#' s.etable(m1, m2)
#'
#' # own row and column labels, with spaces
#' s.etable("Model 1" = m1, "Model 2" = m2,
#'          rowlabels = c(wt = "Weight (1000 lbs)", hp = "Gross horsepower",
#'                     am = "Manual transmission", "_cons" = "Constant"))
#'
#' # text in the top left cell, e.g. the dependent variable
#' s.etable("Model 1" = m1, "Model 2" = m2, firstcell = "Miles per gallon")
#'
#' # own notes below the table
#' s.etable(m1, m2, notes = c("Standardfehler in Klammern.",
#'                            "* p < 0.05, ** p < 0.01, *** p < 0.001"))
#'
#' # confidence intervals, more model statistics
#' s.etable(m1, m2, cstat = "ci", mstat = c("N", "r2", "r2_a"))
#'
#' # logit models as odds ratios
#' l1 <- s.logit(mtcars, am, wt, nolog = TRUE)
#' s.etable(l1, eform = TRUE, mstat = c("N", "r2_p"))
#'
#' # log odds, odds ratios and average marginal effects of one model
#' l1_or <- s.logit(mtcars, am, wt, nolog = TRUE, or = TRUE)
#' l1_ame <- s.logit(mtcars, am, wt, nolog = TRUE, ame = TRUE)
#' s.etable("Log-Odds" = l1, "Odds Ratios" = l1_or, "AME" = l1_ame,
#'          mstat = c("N", "r2_p", "r2_t"))
#'
#' # models estimated with R (addon beyond Stata), mixed with s.regress()
#' fit_lm <- lm(mpg ~ wt + factor(cyl), data = mtcars)
#' fit_glm <- glm(am ~ wt, family = binomial, data = mtcars)
#' s.etable("OLS" = fit_lm, "s.regress" = m1, "Logit" = fit_glm,
#'          rowlabels = c("factor(cyl)" = "Cylinders"), mstat = c("N", "r2", "r2_p"))
#'
#' # other model classes need the package 'parameters', as sjPlot::tab_model()
#' if (requireNamespace("parameters", quietly = TRUE) &&
#'     requireNamespace("MASS", quietly = TRUE)) {
#'   s.etable(MASS::glm.nb(carb ~ wt, data = mtcars))
#' }
#' @export
s.etable <- function(..., rowlabels = NULL, collabels = NULL, firstcell = NULL,
                     cstat = c("se", "ci", "t", "p", "none"), mstat = c("N", "r2"),
                     stars = TRUE, varlabel = TRUE, showbase = FALSE,
                     keep = NULL, drop = NULL, eform = FALSE, nformat = "%9.3f",
                     level = 95, title = NULL, notes = TRUE, export = NULL) {
  cstat <- match.arg(cstat)
  firstcell <- st_check_firstcell(firstcell)
  if (!(isTRUE(notes) || isFALSE(notes) || is.character(notes))) {
    rlang::abort("`notes` must be TRUE, FALSE or a character vector.")
  }
  if (!is.numeric(level) || length(level) != 1L || level <= 10 || level >= 99.99) {
    rlang::abort("`level` must be a number between 10 and 99.99, e.g. 95.")
  }
  cols <- st_etable_models(list(...), level)
  if (!is.null(collabels)) {
    if (length(collabels) != length(cols)) {
      rlang::abort(sprintf("`collabels` must have one label per model (%d).", length(cols)))
    }
    for (i in seq_along(cols)) cols[[i]]$heading <- as.character(collabels[i])
  } else {
    # identical headings (e.g. the same dependent variable) are numbered as
    # in Stata's `column(index)`
    h <- vapply(cols, `[[`, character(1), "heading")
    dup <- h %in% h[duplicated(h)]
    for (i in which(dup)) cols[[i]]$heading <- sprintf("%s (%d)", h[i], i)
  }
  if (!is.null(rowlabels) && (!is.character(rowlabels) || is.null(names(rowlabels)) ||
                           any(!nzchar(names(rowlabels))))) {
    rlang::abort("`rowlabels` must be a named character vector, e.g. c(age = \"Age in years\").")
  }
  mstat <- st_etable_mstat(mstat)
  stars <- st_etable_stars(stars)
  for (v in c("keep", "drop")) {
    x <- get(v)
    if (!is.null(x) && !is.character(x)) rlang::abort(sprintf("`%s` must be a character vector of variable names.", v))
  }

  # all terms in the order of their first appearance, the constant last
  terms <- list()
  for (cl in cols) {
    for (r in cl$m$D$rows) {
      key <- st_etable_key(r)
      # the heading of a factor variable is kept apart from the row of the same
      # variable entered as a continuous variable in another model
      if (r$type == "header") key <- paste0(key, "\u0001")
      # a category that is the base in one model but a coefficient in another
      # is kept as a coefficient row
      if (is.null(terms[[key]]) || (terms[[key]]$type == "base" && r$type == "coef")) terms[[key]] <- r
    }
  }
  vars <- vapply(terms, `[[`, character(1), "variable")
  types <- vapply(terms, `[[`, character(1), "type")
  shown <- types == "block" | ((is.null(keep) | vars %in% keep) & !(vars %in% drop))
  terms <- terms[shown]
  if (!showbase) terms <- Filter(function(r) r$type != "base", terms)
  # equations in the order of their first appearance; within each equation
  # the constant comes last
  block_of <- function(r) r$block %||% NA_character_
  blocks <- unique(vapply(terms, block_of, character(1)))
  ordered <- list()
  for (b in blocks) {
    in_b <- Filter(function(r) identical(block_of(r), b), terms)
    is_cons <- vapply(in_b, function(r) r$variable == "_cons", logical(1))
    ordered <- c(ordered, in_b[!is_cons], in_b[is_cons])
  }
  terms <- ordered
  # a categorical heading without any category left is dropped, as is the
  # heading of an equation without any row left
  for (h in names(terms)) {
    r <- terms[[h]]
    if (!r$type %in% c("header", "block")) next
    left <- vapply(terms, function(q) {
      !q$type %in% c("header", "block") && identical(block_of(q), block_of(r)) &&
        (r$type == "block" || q$variable == r$variable)
    }, logical(1))
    if (!any(left)) terms[[h]] <- NULL
  }
  if (!length(terms)) rlang::abort("No coefficients left to show (check `keep` and `drop`).")
  unused <- setdiff(names(rowlabels), vapply(terms, st_etable_key, character(1)))
  if (length(unused)) {
    keys <- vapply(terms, st_etable_key, character(1))
    lv <- keys[grepl(":", keys, fixed = TRUE)]
    warning(sprintf(paste0("`rowlabels` for terms that are not in the table: %s. Names must be ",
                           "variable names, \"_cons\", \"variable:category\" (e.g. \"%s\") ",
                           "or \"equation::term\"."),
                    paste0("\"", unused, "\"", collapse = ", "),
                    if (length(lv)) lv[1] else "sex:FRAU"),
            call. = FALSE)
  }

  var_labels <- unlist(lapply(cols, function(cl) cl$labels))
  label_of <- function(r) {
    key <- st_etable_key(r)
    if (!is.null(rowlabels) && key %in% names(rowlabels)) return(unname(rowlabels[key]))
    if (r$type == "block") return(r$title %||% r$block)
    if (r$type %in% c("base", "coef") && !is.na(r$level)) return(r$level)
    if (r$variable == "_cons") return("Intercept")
    if (varlabel) {
      l <- var_labels[names(var_labels) == r$variable]
      l <- l[nzchar(l)]
      if (length(l)) return(unname(l[1]))
    }
    r$variable
  }
  row_labels <- vapply(terms, label_of, character(1))

  cells <- lapply(cols, function(cl) lapply(terms, st_etable_cell, cl = cl, eform = eform))
  get_cell <- function(i, ok, name) {
    vapply(cells[[i]][ok], `[[`, numeric(1), name, USE.NAMES = FALSE)
  }
  tidy <- dplyr::bind_rows(lapply(seq_along(cols), function(i) {
    # one row per term that the model contains
    ok <- vapply(cells[[i]], `[[`, logical(1), "present")
    tibble::tibble(
      model = cols[[i]]$heading, term = names(terms)[ok], label = unname(row_labels[ok]),
      estimate = get_cell(i, ok, "estimate"), std_error = get_cell(i, ok, "se"),
      statistic = get_cell(i, ok, "stat"), p_value = get_cell(i, ok, "p"),
      conf_low = get_cell(i, ok, "low"), conf_high = get_cell(i, ok, "high"),
      note = vapply(cells[[i]][ok], `[[`, character(1), "note", USE.NAMES = FALSE)
    )
  }))
  stats <- dplyr::bind_rows(lapply(cols, function(cl) {
    s <- cl$m$stats
    v <- vapply(unname(mstat), function(k) {
      x <- s[[k, exact = TRUE]]
      # Nagelkerke's R2 from the log likelihoods of the model and of the
      # constant-only model (logit models and glm())
      if (k == "r2_n" && is.null(x) && !is.null(s$ll) && !is.null(s$ll_0)) {
        cs <- 1 - exp(2 * (s$ll_0 - s$ll) / s$N)
        x <- cs / (1 - exp(2 * s$ll_0 / s$N))
      }
      if (is.null(x) || !length(x)) NA_real_ else as.numeric(x)
    }, numeric(1))
    names(v) <- unname(mstat)
    tibble::as_tibble(c(list(model = cl$heading), as.list(v)))
  }))

  spec <- list(
    headings = vapply(cols, `[[`, character(1), "heading"),
    row_labels = row_labels, types = vapply(terms, `[[`, character(1), "type"),
    is_level = vapply(terms, function(r) !is.na(r$level), logical(1)),
    cells = cells, cstat = cstat, mstat = mstat, stars = stars, nformat = nformat,
    level = vapply(cols, function(cl) cl$spec$level, numeric(1)),
    test = unique(vapply(cols, function(cl) cl$kind, character(1))),
    eform = eform, or = vapply(cols, function(cl) isTRUE(cl$spec$or), logical(1)),
    ame = vapply(cols, function(cl) isTRUE(cl$spec$ame), logical(1)),
    inter = unique(unlist(lapply(cols, `[[`, "inter"))),
    vce = vapply(cols, function(cl) cl$spec$vce, character(1)),
    title = title, notes = notes, firstcell = firstcell, cmd = title %||% "etable"
  )
  res <- st_new_output(tidy, "st_etable", st_spec = spec, st_meta = stats)
  st_export_if(res, export)
}

# One entry per column: the model of one group of an s.regress()/s.logit()
# result, or a model estimated with R (see etable_models.R), with its heading.
st_etable_models <- function(args, level = 95) {
  # model objects have a class (lm, glm, ...); a plain list holds several models
  is_model <- function(o) {
    inherits(o, c(st_est_classes, "st_margins")) ||
      (!is.null(oldClass(o)) && !is.data.frame(o) && !identical(oldClass(o), "list"))
  }
  flat <- list()
  nms <- character()
  for (i in seq_along(args)) {
    a <- args[[i]]
    nm <- names(args)[i] %||% ""
    if (is_model(a)) {
      flat[[length(flat) + 1L]] <- a
      nms <- c(nms, nm)
    } else if (is.list(a) && !is.data.frame(a) && length(a) && all(vapply(a, is_model, logical(1)))) {
      inner <- names(a) %||% rep("", length(a))
      flat <- c(flat, a)
      nms <- c(nms, ifelse(nzchar(inner), inner, nm))
    } else {
      rlang::abort(paste("All arguments in `...` must be estimation results (s.regress(), s.logit(),",
                         "lm(), glm() or other models) or lists of them."))
    }
  }
  if (!length(flat)) rlang::abort("Please give at least one estimation result.")
  cols <- list()
  for (i in seq_along(flat)) {
    x <- flat[[i]]
    if (inherits(x, "st_margins")) {
      cl <- st_etable_from_margins(x)
      if (nzchar(nms[i])) cl$heading <- nms[i]
      cols[[length(cols) + 1L]] <- cl
      next
    }
    if (!inherits(x, st_est_classes)) {
      cl <- st_etable_from_r(x, level)
      cl$heading <- if (nzchar(nms[i])) nms[i] else cl$heading
      cols[[length(cols) + 1L]] <- cl
      next
    }
    spec <- attr(x, "st_spec", exact = TRUE)
    if (is.null(spec$labels)) {
      rlang::abort("Please re-estimate the models with this version of statsalike (missing variable labels).")
    }
    ylab <- spec$labels[[spec$y]]
    for (m in attr(x, "st_model", exact = TRUE)) {
      heading <- if (nzchar(nms[i])) nms[i] else if (nzchar(ylab)) ylab else spec$y
      if (!is.na(m$label)) heading <- paste(heading, sub("^-> ", "", m$label))
      if (isTRUE(spec$ame)) {
        # models estimated with ame = TRUE show their average marginal effects
        cl <- st_etable_from_margins(m$ame, stats = m$stats)
        cl$heading <- heading
        cols[[length(cols) + 1L]] <- cl
        next
      }
      cols[[length(cols) + 1L]] <- list(m = m, spec = spec, labels = spec$labels, heading = heading,
                                        kind = if (inherits(x, c("st_regress", "st_tobit"))) "t" else "z",
                                        inter = st_inter_notes(spec$terms, spec$is_cat, spec$dates))
    }
  }
  cols
}

# A column of average marginal effects (a result of s.margins() with dydx, or
# of a model estimated with ame = TRUE), in the form of a model column: one
# row per effect, factor variables with a heading and one row per category,
# and one block per outcome after mlogit, ologit and oprobit.
st_etable_from_margins <- function(x, stats = NULL) {
  sp <- attr(x, "st_spec", exact = TRUE)
  d <- st_plain(x)
  if (!length(sp$dterms) || sp$n_at > 1L || length(sp$over)) {
    rlang::abort(paste("s.etable() shows marginal effects of s.margins() with `dydx` only,",
                       "without `at` values and without factor variables in `...`."))
  }
  multi <- isTRUE(sp$outs$multi)
  info <- sp$info
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- list(...)
  vals <- list(estimate = numeric(), se = numeric(), stat = numeric(), p = numeric(),
               low = numeric(), high = numeric())
  seen <- character()
  for (i in seq_len(nrow(d))) {
    b <- if (multi) as.character(d$outcome[i]) else NA_character_
    if (!is.na(b) && !paste0(b, "::") %in% seen) {
      seen <- c(seen, paste0(b, "::"))
      add(variable = b, level = NA_character_, type = "block", col = NA_integer_, block = b,
          title = paste("Outcome", b))
    }
    v <- d$term[i]
    blk <- if (multi) b else NULL
    if (isTRUE(info[[v]]$cat)) {
      key <- paste(b, v)
      if (!key %in% seen) {
        seen <- c(seen, key)
        add(variable = v, level = NA_character_, type = "header", col = NA_integer_, block = blk)
      }
      if (identical(d$note[i], "base")) {
        add(variable = v, level = d$level[i], type = "base", col = NA_integer_, block = blk)
        next
      }
    }
    lev <- if (isTRUE(info[[v]]$cat)) d$level[i] else NA_character_
    for (k in names(vals)) {
      src <- c(estimate = "estimate", se = "std_error", stat = "statistic", p = "p_value",
               low = "conf_low", high = "conf_high")[[k]]
      vals[[k]] <- c(vals[[k]], d[[src]][i])
    }
    add(variable = v, level = lev, type = if (is.na(d$estimate[i])) "omitted" else "coef",
        col = length(vals$estimate), block = blk)
  }
  ms <- sp$model_spec
  st <- stats %||% list(N = sp$N)
  list(
    m = list(D = list(rows = rows), ok = rep(TRUE, length(vals$estimate)), coefs = vals,
             stats = st, label = NA_character_),
    spec = list(y = sp$y, labels = ms$labels, level = sp$level, or = FALSE, vce = sp$vce, ame = TRUE),
    labels = ms$labels, kind = sp$stat,
    heading = if (nzchar(ms$labels[[sp$y]] %||% "")) ms$labels[[sp$y]] else sp$y,
    inter = st_inter_notes(ms$terms, ms$is_cat, ms$dates)
  )
}

# Key of a row: "variable", "variable:category" or "_cons"; rows of further
# equations (e.g. the zero-inflation part) are prefixed with "equation::", the
# heading of an equation is "equation::".
st_etable_key <- function(r) {
  base <- if (!r$type %in% c("header", "block") && !is.na(r$level)) paste0(r$variable, ":", r$level) else r$variable
  block <- r$block %||% NA_character_
  if (is.na(block)) return(base)
  if (r$type == "block") paste0(block, "::") else paste0(block, "::", base)
}

st_etable_mstat_labels <- c(N = "Number of observations", r2 = "R-squared",
                            r2_a = "Adjusted R-squared", r2_p = "Pseudo R-squared",
                            r2_n = "Nagelkerke R-squared", r2_t = "Tjur's R-squared",
                            r2_m = "Marginal R-squared", r2_c = "Conditional R-squared",
                            F = "F statistic", chi2 = "Chi-squared", ll = "Log likelihood",
                            rmse = "Root MSE", n_clust = "Number of clusters")

st_etable_mstat <- function(mstat) {
  if (is.null(mstat) || !length(mstat)) return(character())
  if (!is.character(mstat)) rlang::abort("`mstat` must be a character vector, e.g. c(\"N\", \"r2\").")
  bad <- setdiff(mstat, names(st_etable_mstat_labels))
  if (length(bad)) {
    rlang::abort(paste0("Unknown model statistic: ", paste(bad, collapse = ", "), ". Allowed are: ",
                        paste(names(st_etable_mstat_labels), collapse = ", "), "."))
  }
  lab <- names(mstat) %||% rep("", length(mstat))
  lab[!nzchar(lab)] <- st_etable_mstat_labels[mstat[!nzchar(lab)]]
  stats::setNames(unname(mstat), lab)
}

st_etable_stars <- function(stars) {
  if (isTRUE(stars)) return(c(0.05, 0.01, 0.001))
  if (isFALSE(stars) || is.null(stars)) return(numeric())
  if (!is.numeric(stars) || length(stars) > 3L || anyNA(stars) || any(stars <= 0 | stars >= 1) ||
      is.unsorted(rev(stars), strictly = TRUE)) {
    rlang::abort("`stars` must be TRUE, FALSE or up to three decreasing levels, e.g. c(0.05, 0.01).")
  }
  stars
}

# Estimate and statistics of one term in one model (NA if the model does not
# contain the term).
st_etable_cell <- function(r, cl, eform) {
  na <- NA_real_
  out <- list(estimate = na, se = na, stat = na, p = na, low = na, high = na, note = "", present = FALSE)
  if (r$type %in% c("header", "block")) return(out)
  m <- cl$m
  key <- st_etable_key(r)
  hit <- Filter(function(q) st_etable_key(q) == key && q$type == r$type, m$D$rows)
  if (!length(hit)) {
    # a category that is the base in another model may be a coefficient here
    # (a heading of a factor variable is no coefficient: the same variable can
    # be continuous in one model and categorical in another)
    hit <- Filter(function(q) st_etable_key(q) == key && !q$type %in% c("header", "block"), m$D$rows)
    if (!length(hit)) return(out)
  }
  q <- hit[[1]]
  out$present <- TRUE
  is_or <- isTRUE(cl$spec$or) || isTRUE(cl$spec$eform)
  # marginal effects are never exponentiated
  if (isTRUE(cl$spec$ame)) eform <- FALSE
  if (q$type %in% c("exposure", "offset")) {
    out$estimate <- 1
    out$note <- q$type
    return(out)
  }
  if (q$type %in% c("base", "empty", "omitted") || is.na(q$col) || !m$ok[q$col]) {
    out$estimate <- if (eform || is_or) 1 else 0
    out$note <- if (q$type %in% c("base", "empty")) q$type else "omitted"
    return(out)
  }
  j <- cumsum(m$ok)[q$col]
  cf <- m$coefs
  out[c("estimate", "se", "stat", "p", "low", "high")] <-
    list(cf$estimate[j], cf$se[j], cf$stat[j], cf$p[j], cf$low[j], cf$high[j])
  if (eform && !is_or) {
    out$estimate <- exp(cf$estimate[j])
    out$se <- out$estimate * cf$se[j]
    out$low <- exp(cf$low[j])
    out$high <- exp(cf$high[j])
  }
  out
}

# Output -----------------------------------------------------------------------

#' @export
print.st_etable <- function(x, ...) {
  title <- attr(x, "st_spec", exact = TRUE)$title
  # exported files show the title as their heading (see s.export())
  if (!is.null(title)) cat(title, "", sep = "\n")
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

st_parts_etable <- function(x) {
  spec <- attr(x, "st_spec", exact = TRUE)
  meta <- attr(x, "st_meta", exact = TRUE)
  fmt <- function(v) if (is.na(v)) "" else st_apply_fmt(v, spec$nformat)
  star_of <- function(p) {
    if (!length(spec$stars) || is.na(p)) return("")
    strrep("*", sum(p < spec$stars))
  }
  k <- length(spec$headings)
  # notes only on what the table shows (Bayesian models have no p-values)
  any_second <- FALSE
  any_p <- FALSE
  stub <- character()
  body <- list()
  rules <- 0L
  for (i in seq_along(spec$row_labels)) {
    type <- spec$types[i]
    lab <- spec$row_labels[i]
    if (type %in% c("header", "block")) {
      stub <- c(stub, lab)
      body[[length(body) + 1L]] <- rep("", k)
      next
    }
    indent <- if (spec$is_level[i]) "  " else ""
    est <- second <- character(k)
    for (j in seq_len(k)) {
      c1 <- spec$cells[[j]][[i]]
      if (!c1$present) next
      if (c1$note == "base") {
        est[j] <- "(base)"
      } else if (c1$note == "omitted") {
        est[j] <- "(omitted)"
      } else {
        est[j] <- paste0(fmt(c1$estimate), star_of(c1$p))
        second[j] <- switch(spec$cstat,
          se = if (is.na(c1$se)) "" else paste0("(", fmt(c1$se), ")"),
          ci = if (is.na(c1$low)) "" else paste0("[", fmt(c1$low), ", ", fmt(c1$high), "]"),
          t = if (is.na(c1$stat)) "" else paste0("(", sprintf("%.2f", c1$stat), ")"),
          p = if (is.na(c1$p)) "" else paste0("(", sprintf("%.3f", c1$p), ")"),
          none = ""
        )
      }
    }
    any_second <- any_second || any(nzchar(second))
    any_p <- any_p || any(vapply(spec$cells, function(cc) !is.na(cc[[i]]$p), logical(1)))
    stub <- c(stub, paste0(indent, lab))
    body[[length(body) + 1L]] <- est
    # base and omitted rows have no second line
    if (spec$cstat != "none" && any(nzchar(second))) {
      stub <- c(stub, "")
      body[[length(body) + 1L]] <- second
    }
  }
  if (length(spec$mstat)) {
    rules <- c(rules, length(stub))
    for (s in seq_along(spec$mstat)) {
      key <- spec$mstat[[s]]
      v <- meta[[key]]
      stub <- c(stub, names(spec$mstat)[s])
      body[[length(body) + 1L]] <- vapply(v, function(z) {
        # N is not a whole number with non-integer importance weights
        if (is.na(z)) "" else if (key %in% c("N", "n_clust")) st_fmt_freq(z) else fmt(z)
      }, character(1))
    }
  }
  notes <- if (is.character(spec$notes)) spec$notes else
    if (isTRUE(spec$notes)) st_etable_notes(spec, any_second, any_p)
  body <- do.call(rbind, body)
  # long column headings (e.g. variable labels) are wrapped to the width of
  # their column instead of widening it, as Stata's etable does
  # only where the table would not fit into the console width
  width <- getOption("width", 80L)
  stubw <- min(max(st_width(c(stub, spec$firstcell))), max(12L, floor(width * 0.45)))
  room <- floor((width - stubw - 2L) / k) - 2L
  colw <- apply(matrix(st_width(body), nrow = nrow(body)), 2L, max)
  colw <- pmax(colw, room, 12L)
  lines <- lapply(seq_len(k), function(j) st_wrap_heading(spec$headings[j], colw[j]))
  nl <- max(lengths(lines))
  header <- matrix(unlist(lapply(lines, function(l) c(rep("", nl - length(l)), l))), nrow = nl)
  # the text of `firstcell` stands on the last line of the headings
  parts <- list(st_part_grid(stub = stub, body = body, header = header,
                             stub_header = c(rep("", nl - 1L), spec$firstcell %||% ""), rules = rules,
                             stub_align = "left", frame = TRUE))
  if (length(notes)) parts <- c(parts, list(st_part_text(notes)))
  parts
}

# Break a heading into lines of at most `w` characters, at spaces where
# possible; longer words are cut.
st_wrap_heading <- function(s, w) {
  out <- character()
  cur <- ""
  for (wd in strsplit(s, " ", fixed = TRUE)[[1]]) {
    while (st_width(wd) > w) {
      if (nzchar(cur)) {
        out <- c(out, cur)
        cur <- ""
      }
      out <- c(out, substr(wd, 1L, w))
      wd <- substr(wd, w + 1L, nchar(wd))
    }
    if (!nzchar(wd)) next
    if (!nzchar(cur)) {
      cur <- wd
    } else if (st_width(cur) + 1L + st_width(wd) <= w) {
      cur <- paste(cur, wd)
    } else {
      out <- c(out, cur)
      cur <- wd
    }
  }
  if (nzchar(cur) || !length(out)) out <- c(out, cur)
  out
}

st_etable_notes <- function(spec, any_second = TRUE, any_p = TRUE) {
  out <- character()
  if (spec$cstat != "none" && any_second) {
    vce_name <- c(robust = "Robust standard errors", cluster = "Cluster-robust standard errors",
                  hc2 = "Robust (HC2) standard errors", hc3 = "Robust (HC3) standard errors")
    one_vce <- length(unique(spec$vce)) == 1L
    stat_name <- switch(spec$cstat,
      se = if (one_vce && spec$vce[1] != "ols") vce_name[[spec$vce[1]]] else "Standard errors",
      ci = if (length(unique(spec$level)) == 1L) {
        sprintf("%s%% confidence intervals", format(spec$level[1]))
      } else {
        lv <- unique(spec$level)
        sprintf("Confidence intervals (%s)", paste(vapply(lv, function(l) {
          sprintf("%s%% in model(s) %s", format(l), paste(which(spec$level == l), collapse = ", "))
        }, character(1)), collapse = "; "))
      },
      t = paste(paste(spec$test, collapse = "/"), "statistics"),
      p = "p-values"
    )
    out <- c(out, paste(stat_name, if (spec$cstat == "ci") "in brackets." else "in parentheses."))
    if (spec$cstat == "se" && !one_vce) {
      for (v in intersect(c("robust", "cluster", "hc2", "hc3"), spec$vce)) {
        out <- c(out, sprintf("%s in model(s) %s.", vce_name[[v]],
                              paste(which(spec$vce == v), collapse = ", ")))
      }
    }
  }
  ame <- spec$ame %||% logical(length(spec$or))
  if ((spec$eform && !all(ame)) || any(spec$or)) {
    out <- c(out, if (spec$eform && !any(ame)) "Exponentiated coefficients." else if (spec$eform)
      sprintf("Exponentiated coefficients in model(s) %s.", paste(which(!ame), collapse = ", ")) else
      sprintf("Odds ratios in model(s) %s.", paste(which(spec$or), collapse = ", ")))
  }
  if (any(ame)) {
    out <- c(out, if (all(ame)) "Average marginal effects." else
      sprintf("Average marginal effects in model(s) %s.", paste(which(ame), collapse = ", ")))
  }
  if (length(spec$stars) && any_p) {
    out <- c(out, paste(sprintf("%s p < %s", vapply(seq_along(spec$stars), function(i) strrep("*", i), ""),
                                format(spec$stars, scientific = FALSE, drop0trailing = TRUE)),
                        collapse = ", "))
  }
  c(out, spec$inter)
}
