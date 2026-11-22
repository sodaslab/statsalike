#' @importFrom rlang .data
NULL

#' Predictions, residuals and influence statistics (Stata: `predict`)
#'
#' Re-implementation of Stata's `predict newvar [, statistic]` after
#' [s.regress()], [s.logit()], [s.mlogit()], [s.ologit()] and [s.poisson()].
#' Adds a variable with predictions, residuals or influence statistics to the
#' data, as `predict` adds it to the data in memory.
#'
#' * After [s.regress()]. `"xb"` (fitted values, default), `"residuals"`,
#'   `"rstandard"` (standardized residuals), `"rstudent"` (studentized
#'   residuals), `"cooksd"` (Cook's distance), `"leverage"` (diagonal of the
#'   hat matrix) and `"stdp"` (standard error of the prediction).
#' * After [s.logit()]. `"pr"` (probability, default), `"xb"` and `"stdp"`.
#' * After [s.mlogit()] and [s.ologit()]. `"pr"` (default, with `outcome` one
#'   probability, otherwise one variable per outcome named `newvar_value`)
#'   and `"xb"`.
#' * After [s.poisson()]. `"n"` (number of events, default), `"ir"`
#'   (incidence rate) and `"xb"`.
#'
#' Fitted values, probabilities and linear predictions are computed for every
#' observation with nonmissing regressors, also outside the estimation sample.
#' Residuals need the dependent variable. As in Stata, `rstandard`,
#' `rstudent`, `cooksd` and `leverage` are given for the estimation sample
#' only, so `.data` must be the data the model was estimated with.
#'
#' @param .data The data (usually the data the model was estimated with).
#' @param model An estimation result of statsalike (one model, not grouped).
#' @param newvar Name of the new variable.
#' @param type The statistic (see Details). Default as in Stata.
#' @param outcome Outcome (value or value label) after mlogit and ologit.
#' @return `.data` with the new variable(s).
#' @examples
#' m <- s.regress(mtcars, mpg, wt, hp)
#' d <- s.predict(mtcars, m, "yhat")
#' d <- s.predict(d, m, "r", type = "residuals")
#' s.summarize(d, yhat, r)
#' @export
s.predict <- function(.data, model, newvar, type = NULL, outcome = NULL) {
  st_check_data(.data)
  m <- st_single_model(model)
  spec <- attr(model, "st_spec", exact = TRUE)
  if (!is.character(newvar) || length(newvar) != 1L || !nzchar(newvar)) {
    rlang::abort("`newvar` must be the name of the new variable, e.g. \"yhat\".")
  }
  kind <- spec$kind
  allowed <- switch(kind,
    regress = c("xb", "residuals", "rstandard", "rstudent", "cooksd", "leverage", "stdp"),
    logit = , probit = c("pr", "xb", "stdp"), poisson = , nbreg = c("n", "ir", "xb"),
    mlogit = c("pr", "xb"), ologit = , oprobit = c("pr", "xb"), tobit = c("xb", "stdp"))
  type <- type %||% allowed[1]
  if (!type %in% allowed) {
    rlang::abort(sprintf("`type` must be one of %s after %s.", paste0("\"", allowed, "\"", collapse = ", "), kind))
  }
  D <- m$D
  data <- dplyr::ungroup(.data)
  missing_vars <- setdiff(c(D$terms |> unlist(), if (type == "residuals") spec$y), names(data))
  if (length(missing_vars)) {
    rlang::abort(sprintf("Variable(s) %s not found in `.data`.", paste(missing_vars, collapse = ", ")))
  }
  vals <- st_vals(D$info, data)
  n <- nrow(data)
  X <- st_X(D, vals, n)
  complete <- stats::complete.cases(X)
  Xc <- X
  Xc[!complete, ] <- 0
  in_sample <- function() {
    if (n != spec$n_data) {
      rlang::abort(sprintf("`type = \"%s\"` needs the data the model was estimated with (%d rows).",
                           type, spec$n_data))
    }
    s <- rep(FALSE, n)
    s[D$idx] <- TRUE
    s
  }
  out <- list()
  if (kind %in% c("mlogit", "ologit", "oprobit") && type == "pr") {
    o <- m$outcomes
    js <- if (is.null(outcome)) seq_along(o$labels) else {
      j <- match(as.character(outcome), o$codes)
      if (is.na(j)) j <- match(as.character(outcome), o$labels)
      if (is.na(j)) rlang::abort(sprintf("Outcome %s not found.", outcome))
      j
    }
    for (j in js) {
      v <- st_mg_eval(m, spec, m$b, Xc, NULL, "pr", j)
      v[!complete] <- NA
      nm <- if (is.null(outcome)) paste0(newvar, "_", o$codes[j]) else newvar
      out[[nm]] <- v
    }
  } else if (type %in% c("xb", "pr", "n", "ir")) {
    if (kind == "mlogit" && type == "xb" && is.null(outcome)) {
      rlang::abort("Please give the `outcome` for type = \"xb\" after mlogit.")
    }
    j <- if (kind == "mlogit") match(as.character(outcome), m$outcomes$codes) else NA
    if (kind == "mlogit" && is.na(j)) j <- match(as.character(outcome), m$outcomes$labels)
    off <- 0
    if (kind %in% c("poisson", "nbreg") && type %in% c("n", "xb") && !is.null(spec$off_name)) {
      off <- st_num(data[[spec$off_name]])
      if (spec$off_type == "exposure") off <- log(off)
    }
    mm <- m
    mm$D$offset <- off
    v <- st_mg_eval(mm, spec, m$b, Xc, NULL, type, j)
    v[!complete] <- NA
    out[[newvar]] <- v
  } else if (type == "stdp") {
    ok <- m$ok_cols %||% m$ok
    Xk <- Xc[, ok, drop = FALSE]
    V <- m$V[seq_len(ncol(Xk)), seq_len(ncol(Xk)), drop = FALSE]
    v <- sqrt(rowSums((Xk %*% V) * Xk))
    v[!complete] <- NA
    out[[newvar]] <- v
  } else {
    # residuals and influence statistics of the linear regression
    ok <- m$ok
    Xk <- Xc[, ok, drop = FALSE]
    b <- m$b
    y <- st_num(data[[spec$y]])
    e <- y - drop(Xk %*% b)
    e[!complete] <- NA
    if (type == "residuals") {
      out[[newvar]] <- e
    } else {
      # as in Stata
      if (spec$weighted) rlang::abort(sprintf("`type = \"%s\"` is not possible with weighted data.", type))
      s <- in_sample()
      w <- rep(1, n)
      Vb <- m$V / m$stats$rmse^2
      h <- w * rowSums((Xk %*% Vb) * Xk)
      s2 <- m$stats$rmse^2
      k <- sum(ok)
      df_r <- m$stats$df_r
      v <- switch(type,
        leverage = h,
        rstandard = e * sqrt(w) / sqrt(s2 * (1 - h)),
        rstudent = {
          s2i <- (df_r * s2 - w * e^2 / (1 - h)) / (df_r - 1)
          e * sqrt(w) / sqrt(s2i * (1 - h))
        },
        cooksd = w * e^2 * h / (k * s2 * (1 - h)^2))
      v[!s] <- NA
      out[[newvar]] <- v
    }
  }
  for (nm in names(out)) data[[nm]] <- out[[nm]]
  data
}

st_single_model <- function(x) {
  if (!inherits(x, st_est_classes)) {
    rlang::abort("`model` must be a result of an estimation command of statsalike, e.g. s.regress() or s.logit().")
  }
  models <- attr(x, "st_model", exact = TRUE)
  if (length(models) != 1L) rlang::abort("Please use a single model (not estimated by groups).")
  models[[1]]
}

#' Postestimation statistics (Stata: `estat`)
#'
#' Re-implementation of Stata's `estat` subcommands for checking regression
#' models, with Stata's output.
#'
#' * `"hettest"`. Breusch-Pagan / Cook-Weisberg test for heteroskedasticity
#'   after [s.regress()] (`estat hettest`), on the fitted values or, with
#'   `rhs = TRUE`, on the regressors. `iid = TRUE` gives the version without
#'   the normality assumption (N R-squared), `fstat = TRUE` the F test.
#' * `"vif"`. Variance inflation factors after [s.regress()] (multicollinearity).
#' * `"ovtest"`. Ramsey RESET test for omitted variables after [s.regress()],
#'   with the powers 2 to 4 of the fitted values.
#' * `"ic"`. Akaike's and Schwarz's Bayesian information criteria (AIC, BIC).
#' * `"gof"`. Goodness-of-fit test. After [s.logit()] the Pearson test over
#'   covariate patterns or, with `group`, the Hosmer-Lemeshow test. After
#'   [s.poisson()] the deviance and Pearson tests.
#' * `"classification"`. Classification table after [s.logit()] with
#'   sensitivity, specificity and the share of correctly classified cases.
#'   `cutoff` sets the threshold.
#'
#' @param x An estimation result of statsalike (one model, not grouped).
#' @param what The subcommand (see Details).
#' @param rhs,iid,fstat Options of `"hettest"`.
#' @param group Number of groups for the Hosmer-Lemeshow test (`"gof"`).
#' @param cutoff Threshold for `"classification"` (default 0.5).
#' @return A tibble of class `st_estat` with the statistics. It prints in
#'   Stata's layout.
#' @examples
#' m <- s.regress(mtcars, mpg, wt, hp)
#' s.estat(m, "hettest")
#' s.estat(m, "vif")
#' s.estat(m, "ovtest")
#' l <- s.logit(mtcars, am, wt, nolog = TRUE)
#' s.estat(l, "classification")
#' s.estat(l, "gof", group = 4)
#' @export
s.estat <- function(x, what = c("hettest", "vif", "ovtest", "ic", "gof", "classification"),
                    rhs = FALSE, iid = FALSE, fstat = FALSE, group = NULL, cutoff = 0.5) {
  what <- match.arg(what)
  m <- st_single_model(x)
  spec <- attr(x, "st_spec", exact = TRUE)
  need <- function(kinds) {
    if (!spec$kind %in% kinds) {
      rlang::abort(sprintf("estat %s is not available after %s.", what, spec$kind))
    }
  }
  res <- switch(what,
    hettest = { need("regress"); st_estat_hettest(m, spec, rhs, iid, fstat) },
    vif = { need("regress"); st_estat_vif(m, spec) },
    ovtest = { need("regress"); st_estat_ovtest(m, spec) },
    ic = st_estat_ic(m, spec),
    gof = { need(c("logit", "probit", "poisson")); if (spec$kind == "poisson") st_estat_pgof(m, spec) else st_estat_lgof(m, spec, group) },
    classification = { need(c("logit", "probit")); st_estat_class(m, spec, cutoff) })
  st_new_output(res$tidy, "st_estat", st_spec = list(parts = res$parts, cmd = paste("estat", what)))
}

#' @export
print.st_estat <- function(x, ...) {
  cat(st_render_text(st_parts(x)), sep = "\n")
  invisible(x)
}

# Weighted linear regression of z on the columns of X (with constant):
# model and residual sums of squares as Stata's regress gives them.
st_aux_ols <- function(z, X, w) {
  q <- qr(X * sqrt(w), tol = 1e-9)
  e <- qr.resid(q, z * sqrt(w))
  zbar <- sum(w * z) / sum(w)
  tss <- sum(w * (z - zbar)^2)
  rss <- sum(e^2)
  list(mss = tss - rss, rss = rss, tss = tss, rank = q$rank, r2 = 1 - rss / tss)
}

# Fitted values and residuals of the estimation sample (regress).
st_fit_resid <- function(m) {
  Xk <- m$D$X[, m$ok, drop = FALSE]
  yhat <- drop(Xk %*% m$b)
  list(yhat = yhat, e = m$D$y - yhat, Xk = Xk)
}

st_reg_w <- function(m, spec) {
  w <- m$D$w
  if (spec$weighted && spec$weight_type %in% c("aweight", "pweight")) w <- w * m$D$n / sum(w)
  w
}

st_estat_hettest <- function(m, spec, rhs, iid, fstat) {
  if (spec$weight_type == "pweight" && spec$weighted) rlang::abort("hettest not appropriate with pweighted data.")
  if (spec$vce != "ols") rlang::abort("hettest not appropriate after robust or cluster standard errors.")
  if (iid && fstat) rlang::abort("`iid` and `fstat` cannot be combined.")
  fr <- st_fit_resid(m)
  w <- st_reg_w(m, spec)
  N <- m$stats$N
  u <- fr$e^2 / (m$stats$rss / N)
  if (rhs) {
    Z <- fr$Xk[, setdiff(seq_len(ncol(fr$Xk)), st_cons_index(m$D, m$ok)), drop = FALSE]
    vars <- st_hett_names(m, spec)
  } else {
    Z <- matrix(fr$yhat)
    vars <- paste("fitted values of", spec$y)
  }
  a <- st_aux_ols(u, cbind(Z, 1), w)
  df <- a$rank - 1L
  if (fstat) {
    df_r <- N - a$rank
    Fv <- (a$mss / df) / (a$rss / df_r)
    p <- stats::pf(Fv, df, df_r, lower.tail = FALSE)
    lines <- c(sprintf("%s=%s", st_pad(sprintf("         F(%d , %s)", df, st_stata_g(df_r, 9L)), 22L, "left"), st_pad(sprintf("%.2f", Fv), 9L)),
               sprintf("%s=%s", st_pad("         Prob > F", 22L, "left"), st_pad(sprintf("%.4f", p), 9L)))
    tidy <- tibble::tibble(F = Fv, df_m = df, df_r = df_r, p = p)
  } else {
    chi2 <- if (iid) N * a$r2 else a$mss / 2
    p <- stats::pchisq(chi2, df, lower.tail = FALSE)
    lines <- c(sprintf("%s=%s", st_pad(sprintf("         chi2(%d)", df), 22L, "left"), st_pad(sprintf("%.2f", chi2), 9L)),
               sprintf("%s=%s", st_pad("         Prob > chi2", 22L, "left"), st_pad(sprintf("%.4f", p), 9L)))
    tidy <- tibble::tibble(chi2 = chi2, df = df, p = p)
  }
  txt <- c("Breusch-Pagan / Cook-Weisberg test for heteroskedasticity",
           "         Ho: Constant variance",
           st_wrap_par(paste("Variables:", paste(vars, collapse = " ")), 9L, 20L),
           "", lines)
  list(tidy = tidy, parts = list(st_part_text(txt, mono = TRUE)))
}

# Paragraph with a first-line and a hanging indent (Stata's {p # #}).
st_wrap_par <- function(s, first, rest, width = getOption("width", 80L) - 1L) {
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  out <- character()
  cur <- strrep(" ", first)
  start <- TRUE
  for (wd in words) {
    cand <- if (start) paste0(cur, wd) else paste(cur, wd)
    if (!start && nchar(cand) > width) {
      out <- c(out, cur)
      cur <- paste0(strrep(" ", rest), wd)
    } else cur <- cand
    start <- FALSE
  }
  c(out, cur)
}

# Names of the regressors as Stata lists them ("mpg", "0b.foreign", "1.foreign").
st_hett_names <- function(m, spec) {
  D <- m$D
  out <- character()
  for (i in seq_along(D$cells)) {
    out <- c(out, st_cell_name(D$cells[[i]], D$info))
  }
  out
}

st_coef_names <- function(m, eq = NULL) {
  D <- m$D
  nm <- rep("_cons", length(D$cols))
  for (i in seq_along(D$cells)) {
    j <- D$cell_col[i]
    if (!is.na(j)) nm[j] <- sub("b\\.", ".", st_cell_name(D$cells[[i]], D$info))
  }
  nm
}

st_estat_vif <- function(m, spec) {
  fr <- st_fit_resid(m)
  w <- st_reg_w(m, spec)
  cons <- st_cons_index(m$D, m$ok)
  if (is.null(cons)) rlang::abort("estat vif needs a model with constant.")
  nms <- st_coef_names(m)[m$ok]
  idx <- setdiff(seq_len(ncol(fr$Xk)), cons)
  vif <- vapply(idx, function(j) {
    a <- st_aux_ols(fr$Xk[, j], fr$Xk[, setdiff(seq_len(ncol(fr$Xk)), j), drop = FALSE], w)
    1 / (1 - a$r2)
  }, numeric(1))
  nms <- nms[idx]
  # Stata stores the VIFs in single precision
  vif <- st_float(vif)
  # Stata sorts by VIF unless the model has factor variables
  if (!any(unlist(spec$is_cat))) {
    o <- order(-vif, nms)
    vif <- vif[o]
    nms <- nms[o]
  }
  # factor variables with several categories get a heading and one row per
  # category (by value), as in Stata
  cell <- function(v) paste0(" ", st_pad(sprintf("%.2f", v), 9L), "    ", sprintf("%8.6f", 1 / v))
  body <- character()
  lev_var <- ifelse(grepl("^[0-9.-]+\\.", nms) & !grepl("#", nms), sub("^[0-9.-]+\\.", "", nms), NA)
  multi <- lev_var %in% lev_var[duplicated(lev_var) & !is.na(lev_var)]
  prev <- ""
  for (i in seq_along(nms)) {
    if (multi[i]) {
      if (!identical(lev_var[i], prev)) {
        body <- c(body, paste0(st_pad(st_abbrev_name(lev_var[i], 12L), 12L), " |"))
      }
      code <- sub("\\..*$", "", nms[i])
      body <- c(body, paste0(st_pad(code, 11L), "  |", cell(vif[i])))
    } else {
      body <- c(body, paste0(st_pad(st_abbrev_name(nms[i], 12L), 12L), " |", cell(vif[i])))
    }
    prev <- if (multi[i]) lev_var[i] else ""
  }
  lines <- c("    Variable |       VIF       1/VIF  ", "-------------+----------------------", body,
             "-------------+----------------------",
             sprintf("    Mean VIF | %s", st_pad(sprintf("%.2f", mean(vif)), 9L)))
  list(tidy = tibble::tibble(variable = nms, vif = vif, inv_vif = 1 / vif),
       parts = list(st_part_text(lines, mono = TRUE)))
}

# Single precision, as Stata's "gen float".
st_float <- function(x) readBin(writeBin(as.numeric(x), raw(), size = 4L), "numeric", size = 4L, n = length(x))

st_estat_ovtest <- function(m, spec) {
  fr <- st_fit_resid(m)
  w <- st_reg_w(m, spec)
  yh <- (fr$yhat - min(fr$yhat)) / (max(fr$yhat) - min(fr$yhat))
  Z <- cbind(st_float(yh^2), st_float(yh^3), st_float(yh^4))
  X <- fr$Xk
  q0 <- qr(X * sqrt(w), tol = 1e-9)
  rss0 <- sum(qr.resid(q0, m$D$y * sqrt(w))^2)
  q1 <- qr(cbind(X, Z) * sqrt(w), tol = 1e-9)
  rss1 <- sum(qr.resid(q1, m$D$y * sqrt(w))^2)
  df <- q1$rank - q0$rank
  N <- m$stats$N
  df_r <- N - q1$rank
  Fv <- ((rss0 - rss1) / df) / (rss1 / df_r)
  p <- stats::pf(Fv, df, df_r, lower.tail = FALSE)
  ttl <- sprintf("F(%d, %s)", df, st_stata_g(df_r, 9L))
  lines <- c(paste("Ramsey RESET test using powers of the fitted values of", spec$y),
             "       Ho:  model has no omitted variables",
             paste0(strrep(" ", max(0L, 26L - nchar(ttl))), ttl, " = ", st_pad(sprintf("%.2f", Fv), 9L)),
             paste0(strrep(" ", 18L), "Prob > F =      ", sprintf("%6.4f", p)))
  list(tidy = tibble::tibble(F = Fv, df = df, df_r = df_r, p = p),
       parts = list(st_part_text(lines, mono = TRUE)))
}

st_estat_ic <- function(m, spec) {
  s <- m$stats
  k <- length(m$b)
  N <- s$N
  ll <- s$ll
  aic <- -2 * ll + 2 * k
  bic <- -2 * ll + log(N) * k
  g <- function(v) if (is.na(v)) "." else st_stata_g(v, 9L)
  lines <- c(
    "Akaike's information criterion and Bayesian information criterion", "",
    strrep("-", 77L),
    "       Model |        Obs  ll(null)  ll(model)      df         AIC        BIC",
    paste0("-------------+", strrep("-", 63L)),
    paste0("           . |", st_pad(st_stata_g(N, 9L), 11L), st_pad(g(s$ll_0 %||% NA), 10L),
           st_pad(g(ll), 11L), st_pad(as.character(k), 8L), st_pad(g(aic), 12L), st_pad(g(bic), 11L)),
    strrep("-", 77L),
    "               Note: N=Obs used in calculating BIC; see [R] BIC note.")
  list(tidy = tibble::tibble(N = N, ll_0 = s$ll_0 %||% NA_real_, ll = ll, df = k, aic = aic, bic = bic),
       parts = list(st_part_text(lines, mono = TRUE)))
}

# "Logistic" or "Probit", as Stata names the binary models in estat and lroc.
st_binary_title <- function(m) if (identical(m$link, "probit")) "Probit" else "Logistic"

st_logit_pred <- function(m) {
  Xk <- m$D$X[, m$ok, drop = FALSE]
  cdf <- if (identical(m$link, "probit")) stats::pnorm else stats::plogis
  list(p = cdf(drop(Xk %*% m$b)), y = as.numeric(m$D$y != 0), w = m$D$w, Xk = Xk)
}

st_estat_lgof <- function(m, spec, group) {
  lp <- st_logit_pred(m)
  w <- lp$w
  N <- sum(w)
  k <- length(m$b)
  g <- function(v) st_stata_g(v, 9L)
  head <- paste0(st_binary_title(m), " model for ", spec$y, ", goodness-of-fit test")
  if (is.null(group)) {
    key <- apply(lp$Xk, 1, function(r) paste(format(r, digits = 17), collapse = "\r"))
    pat <- split(seq_along(key), factor(key, levels = unique(key)))
    J <- length(pat)
    chi2 <- sum(vapply(pat, function(i) {
      mj <- sum(w[i])
      yj <- sum(w[i] * lp$y[i])
      pj <- lp$p[i[1]]
      (yj - mj * pj)^2 / (mj * pj * (1 - pj))
    }, numeric(1)))
    df <- J - k
    id <- "Pearson"
    body <- c(paste0("       number of observations = ", st_pad(g(N), 9L)),
              paste0(" number of covariate patterns = ", st_pad(g(J), 9L)))
    tidy <- tibble::tibble(N = N, patterns = J, chi2 = chi2, df = df)
  } else {
    if (!is.numeric(group) || group < 1) rlang::abort("`group` must be a positive number.")
    grp <- st_hl_groups(lp$p, w, group)
    G <- length(unique(grp))
    O <- tapply(w * lp$y, grp, sum)
    E <- tapply(w * lp$p, grp, sum)
    n <- tapply(w, grp, sum)
    chi2 <- sum((O - E)^2 / (E * (1 - E / n)))
    df <- G - 2L
    id <- "Hosmer-Lemeshow"
    body <- c("  (Table collapsed on quantiles of estimated probabilities)",
              if (G < group) sprintf("  (There are only %d distinct quantiles because of ties)", G),
              "",
              paste0("       number of observations = ", st_pad(g(N), 9L)),
              paste0("             number of groups = ", st_pad(g(G), 9L)))
    tidy <- tibble::tibble(N = N, groups = G, chi2 = chi2, df = df)
  }
  p <- stats::pchisq(chi2, df, lower.tail = FALSE)
  lab <- sprintf("%s chi2(%d)", id, df)
  lines <- c(head, "", body,
             paste0(strrep(" ", max(0L, 29L - nchar(lab))), lab, " = ", st_pad(sprintf("%.2f", chi2), 12L)),
             paste0(strrep(" ", 18L), "Prob > chi2 = ", st_pad(sprintf("%.4f", p), 14L)))
  tidy$p <- p
  list(tidy = tidy, parts = list(st_part_text(lines, mono = TRUE)))
}

# Groups of the Hosmer-Lemeshow test: quantiles of the predicted
# probabilities by cumulative weight; observations with the same probability
# stay in one group.
st_hl_groups <- function(p, w, G) {
  N <- sum(w)
  u <- sort(unique(p))
  cw <- cumsum(tapply(w, factor(p, levels = u), sum))
  q <- G * cw / N
  eps <- G * 1e-8
  k <- ifelse(abs(q - round(q)) < eps, round(q), floor(q))
  k[length(k)] <- G
  # the cut points are the values where the quantile number increases
  cut_val <- u[!duplicated(k) & k > 0]
  findInterval(p, c(-Inf, cut_val), left.open = TRUE)
}

st_estat_pgof <- function(m, spec) {
  Xk <- m$D$X[, m$ok, drop = FALSE]
  off <- m$D$offset %||% 0
  mu <- exp(drop(Xk %*% m$b) + off)
  y <- m$D$y
  w <- m$D$w
  dev <- 2 * sum(w * (ifelse(y > 0, y * log(y / mu), 0) - (y - mu)))
  pear <- sum(w * (y - mu)^2 / mu)
  df <- m$stats$N - length(m$b)
  pd <- stats::pchisq(dev, df, lower.tail = FALSE)
  pp <- stats::pchisq(pear, df, lower.tail = FALSE)
  row <- function(lab, v) paste0("         ", st_pad(lab, 25L, "left"), "= ", st_pad(v, 9L))
  lines <- c(row("Deviance goodness-of-fit", st_stata_g(dev, 9L)),
             row(sprintf("Prob > chi2(%d)", df), sprintf("%.4f", pd)), "",
             row("Pearson goodness-of-fit", st_stata_g(pear, 9L)),
             row(sprintf("Prob > chi2(%d)", df), sprintf("%.4f", pp)))
  list(tidy = tibble::tibble(deviance = dev, p_deviance = pd, pearson = pear, p_pearson = pp, df = df),
       parts = list(st_part_text(lines, mono = TRUE)))
}

st_estat_class <- function(m, spec, cutoff) {
  lp <- st_logit_pred(m)
  w <- lp$w
  pos <- lp$p >= cutoff
  D <- lp$y == 1
  a <- sum(w[pos & D])
  b <- sum(w[pos & !D])
  c <- sum(w[!pos & D])
  d <- sum(w[!pos & !D])
  f <- function(v) st_pad(st_stata_g(v, 9L), 10L)
  pct <- function(num, den) if (den > 0) sprintf("%8.2f%%", 100 * num / den) else "       .%"
  row <- function(lab, pr, v) paste0(st_pad(lab, 32L, "left"), pr, v)
  rule <- strrep("-", 50L)
  lines <- c(paste(st_binary_title(m), "model for", spec$y), "",
             "              -------- True --------",
             "Classified |         D            ~D  |      Total",
             "-----------+--------------------------+-----------",
             paste0("     +     |", f(a), st_pad(st_stata_g(b, 9L), 14L), "  |", st_pad(st_stata_g(a + b, 9L), 11L)),
             paste0("     -     |", f(c), st_pad(st_stata_g(d, 9L), 14L), "  |", st_pad(st_stata_g(c + d, 9L), 11L)),
             "-----------+--------------------------+-----------",
             paste0("   Total   |", f(a + c), st_pad(st_stata_g(b + d, 9L), 14L), "  |",
                    st_pad(st_stata_g(a + b + c + d, 9L), 11L)),
             "",
             paste0("Classified + if predicted Pr(D) >= ", st_stata_g(cutoff, 9L)),
             paste0("True D defined as ", spec$y, " != 0"),
             rule,
             row("Sensitivity", "Pr( +| D)", pct(a, a + c)),
             row("Specificity", "Pr( -|~D)", pct(d, b + d)),
             row("Positive predictive value", "Pr( D| +)", pct(a, a + b)),
             row("Negative predictive value", "Pr(~D| -)", pct(d, c + d)),
             rule,
             row("False + rate for true ~D", "Pr( +|~D)", pct(b, b + d)),
             row("False - rate for true D", "Pr( -| D)", pct(c, a + c)),
             row("False + rate for classified +", "Pr(~D| +)", pct(b, a + b)),
             row("False - rate for classified -", "Pr( D| -)", pct(c, c + d)),
             rule,
             paste0(st_pad("Correctly classified", 41L, "left"), pct(a + d, a + b + c + d)),
             rule)
  tidy <- tibble::tibble(sensitivity = a / (a + c), specificity = d / (b + d),
                         ppv = a / (a + b), npv = d / (c + d), correct = (a + d) / (a + b + c + d))
  list(tidy = tidy, parts = list(st_part_text(lines, mono = TRUE)))
}

#' Wald tests of coefficients (Stata: `test`, `testparm`)
#'
#' Tests the hypothesis that the named coefficients are all zero, as Stata's
#' `test` and `testparm`, with an F test after [s.regress()] and [s.tobit()]
#' and a chi-squared test after the other maximum-likelihood models. A term
#' can be a continuous variable (`"mpg"`), a factor variable for all its categories
#' (`"rep78"`, Stata's `testparm i.rep78`), one category by its value
#' (`"2.rep78"`), or an interaction (`"rep78#mpg"`). Bare names
#' work as well, e.g. `s.test(m, mpg, weight)`.
#'
#' @param x An estimation result of statsalike (one model).
#' @param ... Terms to test (names or strings).
#' @return A tibble of class `st_estat` with the test statistic, its degrees
#'   of freedom and the p-value. It prints in Stata's layout.
#' @examples
#' m <- s.regress(mtcars, mpg, wt, hp, cyl, categorical = cyl)
#' s.test(m, wt, hp)
#' s.test(m, "cyl")
#' @export
s.test <- function(x, ...) {
  m <- st_single_model(x)
  spec <- attr(x, "st_spec", exact = TRUE)
  ex <- rlang::enexprs(...)
  terms <- unlist(lapply(ex, function(e) if (is.character(e)) e else rlang::as_string(e)))
  if (!length(terms)) rlang::abort("Please name the coefficients to test.")
  sel <- st_test_select(m, spec, terms)
  b <- m$b[sel$idx]
  V <- m$V[sel$idx, sel$idx, drop = FALSE]
  q <- qr(V, tol = 1e-12)
  keep <- sort(q$pivot[seq_len(q$rank)])
  dropped <- setdiff(seq_along(sel$idx), keep)
  stat <- st_wald(b[keep], V[keep, keep, drop = FALSE])
  r <- length(keep)
  lines <- sprintf(" (%2d)  %s = 0", seq_along(sel$labels), sel$labels)
  if (length(dropped)) lines <- c(lines, sprintf("       Constraint %d dropped", dropped))
  lines <- c(lines, "")
  if (spec$kind %in% c("regress", "tobit")) {
    Fv <- stat / r
    df_r <- m$df_t
    p <- stats::pf(Fv, r, df_r, lower.tail = FALSE)
    df_txt <- if (df_r == round(df_r)) formatC(df_r, format = "d", width = 6L) else st_pad(st_stata_g(df_r, 7L), 6L)
    lines <- c(lines, sprintf("       F(%3d,%s) =%8.2f", r, df_txt, Fv),
               sprintf("            Prob > F =%10.4f", p))
    tidy <- tibble::tibble(F = Fv, df = r, df_r = df_r, p = p)
  } else {
    p <- stats::pchisq(stat, r, lower.tail = FALSE)
    lines <- c(lines, sprintf("           chi2(%3d) =%8.2f", r, stat),
               sprintf("         Prob > chi2 =%10.4f", p))
    tidy <- tibble::tibble(chi2 = stat, df = r, p = p)
  }
  st_new_output(tidy, "st_estat", st_spec = list(parts = list(st_part_text(lines, mono = TRUE)),
                                                 cmd = paste("test", paste(terms, collapse = " "))))
}

# Estimated coefficients of the named terms and their names in Stata's
# constraint notation.
st_test_select <- function(m, spec, terms) {
  D <- m$D
  ok <- m$ok_cols %||% m$ok
  pos <- cumsum(ok)
  names_all <- st_coef_names(m)
  is_cat <- spec$is_cat
  key <- function(t) paste(sort(t), collapse = "#")
  cols <- integer()
  for (s in terms) {
    s0 <- gsub("[[:space:]]", "", s)
    parts <- strsplit(s0, "#", fixed = TRUE)[[1]]
    # categories, e.g. "2.rep78" or "1.foreign#mpg"
    lev <- ifelse(grepl("^-?[0-9.]*[0-9]b?\\.[A-Za-z_]", parts), sub("^(-?[0-9.]*[0-9])b?\\..*$", "\\1", parts), NA)
    parts <- sub("^-?[0-9.]*[0-9]b?\\.", "", parts)
    vars <- sub("^[ic]\\.", "", parts)
    ti <- which(vapply(D$terms, key, character(1)) == key(vars))
    if (!length(ti)) rlang::abort(sprintf("`%s` is not a term of the model.", s))
    for (i in which(vapply(D$cells, `[[`, integer(1), "term") == ti)) {
      j <- D$cell_col[i]
      if (is.na(j) || !ok[j]) next
      cl <- D$cells[[i]]
      hit <- TRUE
      for (k in which(!is.na(lev))) {
        v <- vars[k]
        if (!v %in% cl$cats || !identical(D$info[[v]]$codes[cl$levels[match(v, cl$cats)]], lev[k])) hit <- FALSE
      }
      if (hit) cols <- c(cols, j)
    }
  }
  cols <- unique(cols)
  if (!length(cols)) rlang::abort("No estimated coefficients to test.")
  idx <- pos[cols]
  labels <- names_all[cols]
  eq <- if (spec$kind %in% c("logit", "probit", "poisson", "nbreg", "ologit", "oprobit", "tobit")) paste0("[", spec$y, "]") else ""
  if (spec$kind == "mlogit") {
    rlang::abort("s.test() after mlogit is not supported yet; compare models with s.lrtest().")
  }
  list(idx = idx, labels = paste0(eq, labels))
}

#' Likelihood-ratio test of nested models (Stata: `lrtest`)
#'
#' Compares two nested models estimated on the same observations, as
#' Stata's `lrtest`. Twice the difference of the log likelihoods, with as
#' many degrees of freedom as the models differ in estimated parameters.
#'
#' @param full,reduced Two estimation results of statsalike. The order does not
#'   matter, the model with fewer parameters is taken as the nested one.
#' @return A tibble of class `st_estat` with the statistic, degrees of freedom
#'   and p-value. It prints in Stata's layout.
#' @examples
#' m1 <- s.logit(mtcars, am, wt, nolog = TRUE)
#' m2 <- s.logit(mtcars, am, wt, hp, nolog = TRUE)
#' s.lrtest(m2, m1)
#' @export
s.lrtest <- function(full, reduced) {
  n1 <- rlang::as_label(rlang::enexpr(full))
  n2 <- rlang::as_label(rlang::enexpr(reduced))
  a <- st_single_model(full)
  b <- st_single_model(reduced)
  if (abs(a$stats$N - b$stats$N) > 1e-8) {
    rlang::abort("The models were estimated on different numbers of observations; lrtest needs the same sample.")
  }
  if (is.na(a$stats$ll) || is.na(b$stats$ll)) rlang::abort("The models have no log likelihood (e.g. weighted regressions).")
  ka <- length(a$b)
  kb <- length(b$b)
  if (ka < kb) {
    tmp <- a; a <- b; b <- tmp
    tn <- n1; n1 <- n2; n2 <- tn
    tk <- ka; ka <- kb; kb <- tk
  }
  df <- ka - kb
  chi2 <- 2 * (a$stats$ll - b$stats$ll)
  p <- stats::pchisq(chi2, df, lower.tail = FALSE)
  lines <- c(paste0(st_pad("Likelihood-ratio test", 54L, "left"), st_pad(sprintf("LR chi2(%d)", df), 12L, "left"),
                    "=", st_pad(sprintf("%.2f", chi2), 10L)),
             # Stata abbreviates the model names to 14 characters
             paste0(st_pad(sprintf("(Assumption: %s nested in %s)", st_abbrev_name(n2, 14L),
                                   st_abbrev_name(n1, 14L)), 54L, "left"), "Prob > chi2 =",
                    st_pad(sprintf("%.4f", p), 10L)))
  st_new_output(tibble::tibble(chi2 = chi2, df = df, p = p), "st_estat",
                st_spec = list(parts = list(st_part_text(lines, mono = TRUE)), cmd = paste("lrtest", n1, n2)))
}

# Diagnostic plots -----------------------------------------------------------

#' Residual-versus-fitted plot (Stata: `rvfplot`)
#'
#' Draws the residuals of a linear regression against the fitted values, as
#' Stata's `rvfplot`, with a horizontal line at zero (`yline(0)`) and, with
#' `smooth = TRUE`, a lowess curve. A systematic pattern points to a
#' nonlinear relation, a funnel shape to heteroskedasticity.
#'
#' @param model Result of [s.regress()].
#' @param smooth Add a lowess curve.
#' @return A ggplot object (package 'ggplot2' required).
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   s.rvfplot(s.regress(mtcars, mpg, wt, hp))
#' }
#' @export
s.rvfplot <- function(model, smooth = FALSE) {
  st_need("ggplot2", "s.rvfplot()")
  m <- st_single_model(model)
  spec <- attr(model, "st_spec", exact = TRUE)
  if (spec$kind != "regress") rlang::abort("s.rvfplot() needs a result of s.regress().")
  fr <- st_fit_resid(m)
  d <- data.frame(fitted = fr$yhat, resid = fr$e)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$fitted, y = .data$resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_point(shape = 1)
  if (smooth) p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE)
  p + ggplot2::labs(x = "Fitted values", y = "Residuals")
}

#' Residual-versus-predictor plot (Stata: `rvpplot`)
#'
#' Draws the residuals of a linear regression against one regressor, as
#' Stata's `rvpplot`.
#'
#' @param model Result of [s.regress()].
#' @param x A continuous regressor of the model (unquoted).
#' @param smooth Add a lowess curve.
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   m <- s.regress(mtcars, mpg, wt, hp)
#'   s.rvpplot(m, wt)
#' }
#' @export
s.rvpplot <- function(model, x, smooth = FALSE) {
  st_need("ggplot2", "s.rvpplot()")
  m <- st_single_model(model)
  spec <- attr(model, "st_spec", exact = TRUE)
  v <- rlang::as_string(rlang::ensym(x))
  if (spec$kind != "regress" || is.null(m$D$vals[[v]]) || is.matrix(m$D$vals[[v]])) {
    rlang::abort("`x` must be a continuous regressor of a model of s.regress().")
  }
  fr <- st_fit_resid(m)
  d <- data.frame(x = m$D$vals[[v]], resid = fr$e)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
    ggplot2::geom_point(shape = 1)
  if (smooth) p <- p + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE)
  p + ggplot2::labs(x = v, y = "Residuals")
}

#' Leverage-versus-squared-residual plot (Stata: `lvr2plot`)
#'
#' Draws the leverage of every observation against its normalized squared
#' residual, as Stata's `lvr2plot`. The lines mark the means of both.
#' Observations far to the right have large residuals, observations high up
#' are unusual in their regressors. Both together are influential.
#'
#' @param model Result of [s.regress()].
#' @param label Optional variable of the estimation data to label the points
#'   (e.g. an id), given as a vector of the length of the data.
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   s.lvr2plot(s.regress(mtcars, mpg, wt, hp))
#' }
#' @export
s.lvr2plot <- function(model, label = NULL) {
  st_need("ggplot2", "s.lvr2plot()")
  m <- st_single_model(model)
  spec <- attr(model, "st_spec", exact = TRUE)
  if (spec$kind != "regress") rlang::abort("s.lvr2plot() needs a result of s.regress().")
  fr <- st_fit_resid(m)
  w <- st_reg_w(m, spec)
  Vb <- m$V / m$stats$rmse^2
  h <- w * rowSums((fr$Xk %*% Vb) * fr$Xk)
  r2 <- w * fr$e^2 / sum(w * fr$e^2)
  d <- data.frame(r2 = r2, h = h)
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$r2, y = .data$h)) +
    ggplot2::geom_hline(yintercept = mean(h), colour = "grey40") +
    ggplot2::geom_vline(xintercept = mean(r2), colour = "grey40") +
    ggplot2::geom_point(shape = 1)
  if (!is.null(label)) {
    d$lab <- as.character(label)[m$D$idx]
    p <- p + ggplot2::geom_text(data = d, ggplot2::aes(label = .data$lab), size = 3, vjust = -0.6)
  }
  p + ggplot2::labs(x = "Normalized residual squared", y = "Leverage")
}

#' Quantile-normal plot (Stata: `qnorm`)
#'
#' Plots the quantiles of a variable against the quantiles of a normal
#' distribution with the same mean and standard deviation, as Stata's
#' `qnorm`. Points on the line indicate a normal distribution. Useful for
#' residuals saved with [s.predict()].
#'
#' @param .data A data frame.
#' @param x A numeric variable (unquoted).
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   m <- s.regress(mtcars, mpg, wt)
#'   d <- s.predict(mtcars, m, "r", type = "residuals")
#'   s.qnorm(d, r)
#' }
#' @export
s.qnorm <- function(.data, x) {
  st_need("ggplot2", "s.qnorm()")
  v <- st_select1(rlang::enquo(x), .data, "x")
  z <- sort(st_num(.data[[v]]))
  z <- z[!is.na(z)]
  n <- length(z)
  d <- data.frame(normal = stats::qnorm((seq_len(n)) / (n + 1), mean(z), stats::sd(z)), x = z)
  ggplot2::ggplot(d, ggplot2::aes(x = .data$normal, y = .data$x)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = "grey40") +
    ggplot2::geom_point(shape = 1) +
    ggplot2::labs(x = "Inverse Normal", y = st_label_or_name(.data[[v]], v))
}

#' Kernel density estimate (Stata: `kdensity`)
#'
#' Draws a kernel density estimate of a variable as Stata's `kdensity`
#' (Epanechnikov kernel and Stata's default bandwidth). `normal = TRUE`
#' adds the normal density with the same mean and standard deviation, as
#' Stata's `kdensity x, normal`.
#'
#' @param .data A data frame.
#' @param x A numeric variable (unquoted).
#' @param normal Overlay the normal density.
#' @return A ggplot object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   s.kdensity(mtcars, mpg)
#'   s.kdensity(mtcars, mpg, normal = TRUE)
#' }
#' @export
s.kdensity <- function(.data, x, normal = FALSE) {
  st_need("ggplot2", "s.kdensity()")
  v <- st_select1(rlang::enquo(x), .data, "x")
  z <- st_num(.data[[v]])
  z <- z[!is.na(z)]
  n <- length(z)
  # Stata's default (optimal for Gaussian data): 0.9 * min(sd, iqr/1.349) * n^(-1/5)
  bw <- 0.9 * min(stats::sd(z), stats::IQR(z, type = 2) / 1.349) * n^(-1 / 5)
  # R's kernels, like Stata's, are scaled to a standard deviation of 1
  dens <- stats::density(z, bw = bw, kernel = "epanechnikov", n = 50L,
                         from = min(z), to = max(z))
  d <- data.frame(x = dens$x, y = dens$y)
  if (!normal) {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) + ggplot2::geom_line()
  } else {
    # ggplot2's default colours distinguish the two curves
    d$curve <- "Kernel density estimate"
    nd <- data.frame(x = dens$x, y = stats::dnorm(dens$x, mean(z), stats::sd(z)), curve = "Normal density")
    p <- ggplot2::ggplot(rbind(d, nd), ggplot2::aes(x = .data$x, y = .data$y, colour = .data$curve)) +
      ggplot2::geom_line()
  }
  p + ggplot2::labs(title = "Kernel density estimate", x = st_label_or_name(.data[[v]], v), y = "Density",
                    colour = NULL, caption = sprintf("kernel = epanechnikov, bandwidth = %.4f", bw))
}
