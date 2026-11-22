#' Linear combinations of coefficients (Stata: `lincom`)
#'
#' Estimates a linear combination of the coefficients of a model with its
#' standard error, test and confidence interval, as Stata's `lincom`, e.g.
#' the sum of two coefficients or the slope of a variable in another group
#' of an interaction model. Coefficients are named as in [s.test()]. A
#' variable (`"mpg"`), a category by its value (`"2.rep78"`), an interaction
#' (`"1.foreign#mpg"`) or the constant (`"_cons"`), each possibly multiplied by
#' a number (`"2*mpg"`, `"mpg*1000"`).
#'
#' @param x An estimation result of statsalike (one model, not after
#'   [s.mlogit()]).
#' @param expr The linear combination as a string, e.g. `"mpg + weight"` or
#'   `"2*mpg - weight"`.
#' @param eform Report `exp()` of the combination (odds ratio after
#'   [s.logit()], incidence-rate ratio after [s.poisson()] and [s.nbreg()]),
#'   as Stata's `or`, `irr` and `eform` options.
#' @param level Confidence level in percent.
#' @return A tibble of class `st_test` with the estimate, its standard error,
#'   test statistic, p-value and confidence limits. It prints in Stata's
#'   layout.
#' @examples
#' m <- s.regress(mtcars, mpg, wt, hp)
#' s.lincom(m, "wt + hp")
#' s.lincom(m, "2*wt - hp")
#' @export
s.lincom <- function(x, expr, eform = FALSE, level = 95) {
  m <- st_single_model(x)
  spec <- attr(x, "st_spec", exact = TRUE)
  if (spec$kind == "mlogit") rlang::abort("s.lincom() after mlogit is not supported.")
  if (!is.character(expr) || length(expr) != 1L) rlang::abort("`expr` must be a string, e.g. \"mpg + weight\".")
  ok <- m$ok_cols %||% m$ok
  pos <- cumsum(ok)
  names_all <- st_coef_names(m)
  eq <- if (spec$kind %in% c("regress")) "" else paste0("[", spec$y, "]")
  s0 <- gsub("[[:space:]]", "", expr)
  # terms with their signs; numbers in exponential notation keep their sign
  toks <- regmatches(s0, gregexpr("[+-]?[^+-]+(e[+-]?[0-9]+)?", s0, perl = TRUE))[[1]]
  if (!length(toks) || paste(toks, collapse = "") != s0) rlang::abort(sprintf("Cannot read \"%s\".", expr))
  cvec <- numeric(length(m$b))
  labs <- character(length(m$b))
  const <- 0
  shown <- character()
  num_re <- "^[0-9]*\\.?[0-9]+(e[+-]?[0-9]+)?$"
  for (tk in toks) {
    sgn <- if (startsWith(tk, "-")) -1 else 1
    body <- sub("^[+-]", "", tk)
    parts <- strsplit(body, "*", fixed = TRUE)[[1]]
    nums <- parts[grepl(num_re, parts)]
    nm <- parts[!grepl(num_re, parts)]
    if (length(nm) > 1L) rlang::abort(sprintf("Cannot read \"%s\": products of coefficients are not linear.", tk))
    f <- sgn * prod(as.numeric(nums))
    if (!length(nm)) {
      const <- const + f
      shown <- c(shown, paste0(if (f < 0) "- " else "+ ", st_stata_g(abs(f), 9L)))
      next
    }
    if (nm == "_cons") {
      if (is.null(m$D$cons) || !ok[m$D$cons]) rlang::abort("The model has no constant.")
      j <- pos[m$D$cons]
      lab <- "_cons"
    } else {
      sel <- st_test_select(m, spec, nm)
      if (length(sel$idx) != 1L) {
        rlang::abort(sprintf("`%s` names %d coefficients; give one category, e.g. \"2.%s\".",
                             nm, length(sel$idx), nm))
      }
      j <- sel$idx
      lab <- sub("^\\[[^]]*\\]", "", sel$labels)
    }
    cvec[j] <- cvec[j] + f
    labs[j] <- lab
  }
  # the combination in the order of the coefficients, as Stata shows it
  nz <- which(cvec != 0)
  shown <- c(vapply(nz, function(j) {
    f <- cvec[j]
    paste0(if (f < 0) "- " else "+ ", if (abs(f) == 1) "" else paste0(st_stata_g(abs(f), 9L), "*"), eq, labs[j])
  }, character(1)), shown)
  txt <- sub("^\\+ ", "", paste(shown, collapse = " "))
  est <- sum(cvec * m$b) + const
  se <- sqrt(max(drop(t(cvec) %*% m$V %*% cvec), 0))
  tdist <- spec$kind %in% c("regress", "tobit")
  df <- if (tdist) m$df_t else Inf
  stat <- est / se
  p <- 2 * if (tdist) stats::pt(-abs(stat), df) else stats::pnorm(-abs(stat))
  crit <- if (tdist) stats::qt(1 - (1 - level / 100) / 2, df) else stats::qnorm(1 - (1 - level / 100) / 2)
  low <- est - crit * se
  high <- est + crit * se
  lab <- "Coef."
  if (eform) {
    lab <- switch(spec$kind, logit = , ologit = "Odds Ratio", poisson = , nbreg = "IRR", "exp(b)")
    se <- exp(est) * se
    est <- exp(est)
    low <- exp(low)
    high <- exp(high)
  }
  W <- st_ct_width(spec$y)
  terms <- list(list(kind = "plain", label = "(1)",
                     rows = list(list(label = "(1)", status = "coef",
                                      cells = st_ct_cells(est, se, stat, p, low, high)))))
  tab <- st_ct_build(list(list(name = NULL, terms = terms)), W, spec$y, lab,
                     stat = if (tdist) "t" else "z", level = level)
  parts <- list(st_part_text(sprintf(" ( 1)  %s = 0", txt), mono = TRUE), tab)
  tidy <- tibble::tibble(estimate = est, std_error = se, statistic = stat, p_value = p,
                         conf_low = low, conf_high = high)
  st_new_output(tidy, "st_test", st_spec = list(parts = parts, cmd = paste("lincom", expr)))
}

#' @export
print.st_test <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  p <- attr(x, "st_plot", exact = TRUE)
  if (!is.null(p)) print(p)
  invisible(x)
}

#' Specification link test (Stata: `linktest`)
#'
#' Re-estimates the model with the linear prediction (`_hat`) and its square
#' (`_hatsq`) as the only regressors, as Stata's `linktest`. If the model is
#' correctly specified, `_hatsq` should not be significant. A significant
#' `_hatsq` points to omitted variables or a wrong functional form (or link
#' function). Available after [s.regress()], [s.logit()] and [s.probit()].
#' Weights and standard errors are those of the model.
#'
#' @param x An estimation result of [s.regress()], [s.logit()] or
#'   [s.probit()] (one model).
#' @param nolog Do not show the iteration log of logit and probit.
#' @return The result of the re-estimated model.
#' @examples
#' s.linktest(s.regress(mtcars, mpg, wt, hp))
#' @export
s.linktest <- function(x, nolog = FALSE) {
  m <- st_single_model(x)
  spec <- attr(x, "st_spec", exact = TRUE)
  if (!spec$kind %in% c("regress", "logit", "probit")) {
    rlang::abort("s.linktest() is available after s.regress(), s.logit() and s.probit().")
  }
  ok <- m$ok_cols %||% m$ok
  # Stata stores the prediction and its square as float variables
  hat <- st_float(drop(m$D$X[, ok, drop = FALSE] %*% m$b[seq_len(sum(ok))]))
  d <- tibble::tibble(.y = m$D$y, `_hat` = hat, `_hatsq` = st_float(hat^2), .w = m$D$w,
                      .c = if (!is.null(m$D$clus)) m$D$clus else NA)
  vce <- if (spec$vce %in% c("ols", "robust", "cluster", "hc2", "hc3")) spec$vce else "ols"
  if (spec$weighted && spec$weight_type == "pweight") vce <- "ols"
  # the cluster column keeps its name for the note below the table
  cl_name <- if (vce == "cluster" && !is.null(spec$cl_name)) spec$cl_name else ".c"
  names(d)[c(1, 5)] <- c(spec$y, cl_name)
  wt_arg <- if (spec$weighted) rlang::sym(".w") else NULL
  cl_arg <- if (vce == "cluster") rlang::sym(cl_name) else NULL
  cmd <- switch(spec$kind, regress = s.regress, logit = s.logit, probit = s.probit)
  args <- list(d, rlang::sym(spec$y), "_hat", "_hatsq",
               wt = wt_arg, weight_type = spec$weight_type, vce = if (vce == "cluster") "ols" else vce,
               cluster = cl_arg)
  if (spec$kind != "regress") args$nolog <- nolog
  rlang::inject(cmd(!!!args))
}

#' ROC curve and its area after logit or probit (Stata: `lroc`)
#'
#' Computes the area under the receiver operating characteristic (ROC)
#' curve of a logit or probit model, as Stata's `lroc`, and draws the curve.
#' Sensitivity against 1 - specificity for all cutoffs of the predicted
#' probability. An area of 0.5 means no discrimination, 1 perfect
#' discrimination.
#'
#' @param x An estimation result of [s.logit()] or [s.probit()] (one model).
#' @param graph Draw the ROC curve (package 'ggplot2' required).
#' @return A tibble of class `st_test` with `N` and `area`. It prints in
#'   Stata's layout and draws the curve.
#' @examples
#' s.lroc(s.logit(mtcars, am, wt, nolog = TRUE), graph = FALSE)
#' @export
s.lroc <- function(x, graph = TRUE) {
  m <- st_single_model(x)
  spec <- attr(x, "st_spec", exact = TRUE)
  if (!spec$kind %in% c("logit", "probit")) rlang::abort("s.lroc() is available after s.logit() and s.probit().")
  lp <- st_logit_pred(m)
  roc <- st_roc(lp$p, lp$y, lp$w)
  N <- if (spec$weighted && spec$weight_type %in% c("fweight", "iweight")) sum(lp$w) else length(lp$y)
  lines <- c(paste(st_binary_title(m), "model for", spec$y), "",
             paste0("number of observations = ", sprintf("%8.0f", N)),
             paste0("area under ROC curve   = ", sprintf("%8.4f", roc$area)))
  res <- st_new_output(tibble::tibble(N = N, area = roc$area), "st_test",
                       st_spec = list(parts = list(st_part_text(lines, mono = TRUE)), cmd = "lroc"))
  if (graph) {
    st_need("ggplot2", "s.lroc()")
    attr(res, "st_plot") <- ggplot2::ggplot(roc$curve, ggplot2::aes(x = .data$fpr, y = .data$tpr)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, colour = "grey50") +
      ggplot2::geom_line() + ggplot2::geom_point(shape = 1) +
      ggplot2::coord_equal() +
      ggplot2::labs(x = "1 - Specificity", y = "Sensitivity",
                    caption = sprintf("Area under ROC curve = %.4f", roc$area))
  }
  res
}

# ROC curve (one point per distinct predicted probability, from the highest
# cutoff down) and its area by the trapezoidal rule, which equals the
# probability that a positive observation has a higher prediction than a
# negative one (ties count one half).
st_roc <- function(p, y, w) {
  cut <- sort(unique(p), decreasing = TRUE)
  P <- sum(w[y == 1])
  Nn <- sum(w[y == 0])
  tpr <- c(0, vapply(cut, function(c) sum(w[p >= c & y == 1]) / P, numeric(1)))
  fpr <- c(0, vapply(cut, function(c) sum(w[p >= c & y == 0]) / Nn, numeric(1)))
  area <- sum(diff(fpr) * (utils::head(tpr, -1) + utils::tail(tpr, -1)) / 2)
  list(area = area, curve = data.frame(fpr = fpr, tpr = tpr))
}

#' Sensitivity and specificity against the cutoff (Stata: `lsens`)
#'
#' Draws the sensitivity and specificity of a logit or probit model for all
#' cutoffs of the predicted probability, as Stata's `lsens`. Where the two
#' curves cross, the classification is equally good for both outcomes. It is
#' a common choice for the cutoff in [s.estat()] `"classification"`.
#'
#' @param x An estimation result of [s.logit()] or [s.probit()] (one model).
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   s.lsens(s.logit(mtcars, am, wt, nolog = TRUE))
#' }
#' @export
s.lsens <- function(x) {
  st_need("ggplot2", "s.lsens()")
  m <- st_single_model(x)
  spec <- attr(x, "st_spec", exact = TRUE)
  if (!spec$kind %in% c("logit", "probit")) rlang::abort("s.lsens() is available after s.logit() and s.probit().")
  lp <- st_logit_pred(m)
  cut <- sort(unique(c(0, lp$p, 1)))
  sens <- vapply(cut, function(c) sum(lp$w[lp$p >= c & lp$y == 1]) / sum(lp$w[lp$y == 1]), numeric(1))
  spec_ <- vapply(cut, function(c) sum(lp$w[lp$p < c & lp$y == 0]) / sum(lp$w[lp$y == 0]), numeric(1))
  d <- rbind(data.frame(cutoff = cut, value = sens, curve = "Sensitivity"),
             data.frame(cutoff = cut, value = spec_, curve = "Specificity"))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$cutoff, y = .data$value, colour = .data$curve)) +
    ggplot2::geom_step() +
    ggplot2::labs(x = "Probability cutoff", y = "Sensitivity/Specificity", colour = NULL)
}

#' Added-variable plots (Stata: `avplot`, `avplots`)
#'
#' Draws the added-variable plot of a regressor after [s.regress()], as
#' Stata's `avplot`. The residuals of the dependent variable on all other
#' regressors against the residuals of the regressor on all other regressors.
#' The slope of the line is the coefficient of the regressor in the full
#' model. Points far from the line are influential for this coefficient.
#' `s.avplots()` draws the plots of all regressors (for factor variables one
#' per category).
#'
#' @param model Result of [s.regress()] (one model).
#' @param x A regressor of the model (unquoted), or for a category of a factor
#'   variable its name as in [s.test()], e.g. `"2.rep78"`.
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   m <- s.regress(mtcars, mpg, wt, hp)
#'   s.avplot(m, wt)
#'   s.avplots(m)
#' }
#' @export
s.avplot <- function(model, x) {
  st_need("ggplot2", "s.avplot()")
  m <- st_single_model(model)
  spec <- attr(model, "st_spec", exact = TRUE)
  if (spec$kind != "regress") rlang::abort("s.avplot() needs a result of s.regress().")
  e <- rlang::enexpr(x)
  nm <- if (is.character(e)) e else rlang::as_string(e)
  d <- st_av_data(m, spec, nm)
  st_av_plot(d, spec, nm)
}

#' @rdname s.avplot
#' @export
s.avplots <- function(model) {
  st_need("ggplot2", "s.avplots()")
  m <- st_single_model(model)
  spec <- attr(model, "st_spec", exact = TRUE)
  if (spec$kind != "regress") rlang::abort("s.avplots() needs a result of s.regress().")
  nms <- st_coef_names(m)[m$ok]
  cons <- st_cons_index(m$D, m$ok)
  nms <- if (!is.null(cons)) nms[-cons] else nms
  d <- do.call(rbind, lapply(nms, function(v) {
    a <- st_av_data(m, spec, v, by_name = TRUE)
    a$panel <- sprintf("%s (coef = %s, se = %s)", v, st_stata_g(a$coef[1], 9L), st_stata_g(a$se[1], 9L))
    a
  }))
  d$panel <- factor(d$panel, levels = unique(d$panel))
  ggplot2::ggplot(d, ggplot2::aes(x = .data$ex, y = .data$ey)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggplot2::facet_wrap(~ panel, scales = "free") +
    ggplot2::labs(x = "e( x | X )", y = sprintf("e( %s | X )", spec$y))
}

# Residuals of y and of one column on all other columns of the design.
st_av_data <- function(m, spec, nm, by_name = FALSE) {
  names_ok <- st_coef_names(m)[m$ok]
  j <- match(nm, names_ok)
  if (is.na(j)) {
    sel <- tryCatch(st_test_select(m, spec, nm), error = function(e) NULL)
    if (is.null(sel) || length(sel$idx) != 1L) {
      rlang::abort(sprintf("`%s` is not a single coefficient of the model.", nm))
    }
    j <- sel$idx
  }
  fr <- st_fit_resid(m)
  w <- st_reg_w(m, spec)
  X <- fr$Xk
  others <- X[, -j, drop = FALSE]
  res <- function(z) {
    q <- qr(others * sqrt(w), tol = 1e-9)
    drop(z - others %*% qr.coef(q, z * sqrt(w)))
  }
  ey <- res(m$D$y)
  ex <- res(X[, j])
  data.frame(ex = ex, ey = ey, coef = m$coefs$estimate[j], se = m$coefs$se[j], t = m$coefs$stat[j])
}

st_av_plot <- function(d, spec, nm) {
  ggplot2::ggplot(d, ggplot2::aes(x = .data$ex, y = .data$ey)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggplot2::labs(x = sprintf("e( %s | X )", nm), y = sprintf("e( %s | X )", spec$y),
                  caption = sprintf("coef = %s, %sse = %s, t = %s", st_stata_g(d$coef[1], 9L),
                                    if (spec$vce != "ols") "(robust) " else "",
                                    st_stata_g(d$se[1], 9L), sprintf("%.2f", d$t[1])))
}

#' Component-plus-residual plot (Stata: `cprplot`)
#'
#' Draws the component-plus-residual (partial residual) plot of a continuous
#' regressor after [s.regress()], as Stata's `cprplot`. The residual plus the
#' regressor's part of the prediction (b * x) against the regressor, with the
#' line of slope b. A curved pattern of the points around the line points to
#' a nonlinear relation. `lowess = TRUE` adds a smooth curve.
#'
#' @param model Result of [s.regress()] (one model).
#' @param x A continuous regressor of the model (unquoted).
#' @param lowess Add a lowess curve.
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   m <- s.regress(mtcars, mpg, wt, hp)
#'   s.cprplot(m, wt)
#'   s.cprplot(m, hp, lowess = TRUE)
#' }
#' @export
s.cprplot <- function(model, x, lowess = FALSE) {
  st_need("ggplot2", "s.cprplot()")
  m <- st_single_model(model)
  spec <- attr(model, "st_spec", exact = TRUE)
  v <- rlang::as_string(rlang::ensym(x))
  if (spec$kind != "regress" || is.null(m$D$vals[[v]]) || is.matrix(m$D$vals[[v]])) {
    rlang::abort("`x` must be a continuous regressor of a model of s.regress().")
  }
  j <- match(v, st_coef_names(m)[m$ok])
  if (is.na(j)) rlang::abort(sprintf("`%s` has no estimated coefficient as a main effect.", v))
  fr <- st_fit_resid(m)
  b <- m$coefs$estimate[j]
  d <- data.frame(x = m$D$vals[[v]], cpr = fr$e + b * m$D$vals[[v]])
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$cpr)) +
    ggplot2::geom_point(shape = 1) +
    ggplot2::geom_abline(intercept = 0, slope = b)
  if (lowess) p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE)
  p + ggplot2::labs(x = v, y = "Component plus residual")
}
