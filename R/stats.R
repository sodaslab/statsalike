# Statistics following the formulas in Stata's Methods and formulas sections ---

# Quantiles as in Stata's `summarize`/`table` (also weighted):
# P = W * p / 100; if W_(i) == P, the mean of x_(i) and x_(i+1),
# otherwise x_(i+1) with W_(i) < P < W_(i+1).
st_wquantile <- function(x, w, p, sorted = FALSE) {
  n <- length(x)
  if (!n) return(NA_real_)
  if (!sorted) {
    o <- order(x)
    x <- x[o]
    w <- w[o]
  }
  if (p <= 0) return(x[1])
  if (p >= 100) return(x[n])
  W <- cumsum(w)
  P <- W[n] * p / 100
  eps <- 1e-12 * abs(W[n])
  i <- sum(W < P - eps)
  if (i + 1L < n && abs(W[i + 1L] - P) <= eps) {
    (x[i + 1L] + x[i + 2L]) / 2
  } else {
    x[i + 1L]
  }
}

# Mean, standard deviation, frequency, observations
# (for `tabulate, summarize()`).
st_summ <- function(x, w, weight_type) {
  n <- length(x)
  if (!n) return(c(mean = NA_real_, sd = NA_real_, freq = 0, obs = 0))
  freq <- if (weight_type == "aweight") n else sum(w)
  if (weight_type == "aweight") w <- w * n / sum(w)
  W <- sum(w)
  m <- sum(w * x) / W
  s <- if (W > 1) sqrt(sum(w * (x - m)^2) / (W - 1)) else NA_real_
  c(mean = m, sd = s, freq = freq, obs = n)
}

st_freq_stats <- c("frequency", "percent", "proportion", "rowpercent",
                   "colpercent")
st_var_stats <- c("mean", "sd", "var", "semean", "cv", "skewness",
                  "kurtosis", "min", "max", "range", "sum", "total",
                  "rawsum", "sumw", "count", "median", "iqr", "q1", "q2",
                  "q3", "first", "last")

st_parse_stats <- function(statistic) {
  if (!is.character(statistic) || !length(statistic)) {
    rlang::abort("`statistic` must be a character vector, e.g. c(\"frequency\", \"mean\").")
  }
  s <- tolower(trimws(statistic))
  s[s == "freq"] <- "frequency"
  s[s == "se"] <- "semean"
  valid <- s %in% c(st_freq_stats, st_var_stats) |
    grepl("^p([1-9]|[1-9][0-9])$", s)
  if (!all(valid)) {
    rlang::abort(paste0(
      "Unknown statistic: ", paste(statistic[!valid], collapse = ", "),
      ".\nAllowed are: ", paste(c(st_freq_stats, st_var_stats), collapse = ", "),
      " and p1 to p99."
    ))
  }
  unique(s)
}

st_ordinal <- function(n) {
  suf <- c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]
  suf[n %% 100 %in% 11:13] <- "th"
  paste0(n, suf)
}

st_stat_label <- function(stat) {
  lab <- c(frequency = "Frequency", percent = "Percent",
           proportion = "Proportion", rowpercent = "Row percent",
           colpercent = "Column percent", mean = "Mean", sd = "Std. dev.",
           var = "Variance", semean = "Std. error", cv = "Coef. of var.",
           skewness = "Skewness", kurtosis = "Kurtosis", min = "Minimum",
           max = "Maximum", range = "Range", sum = "Sum", total = "Total",
           rawsum = "Raw sum", sumw = "Sum of wgt.", count = "Count",
           median = "Median", iqr = "IQR", q1 = "Q1", q2 = "Q2", q3 = "Q3",
           first = "First", last = "Last")
  out <- unname(lab[stat])
  p <- grepl("^p[0-9]+$", stat)
  out[p] <- paste(st_ordinal(as.integer(substring(stat[p], 2))), "percentile")
  out
}

st_quantile_p <- function(stat) {
  if (stat %in% c("median", "q2")) return(50)
  if (stat == "q1") return(25)
  if (stat == "q3") return(75)
  if (grepl("^p[0-9]+$", stat)) return(as.numeric(substring(stat, 2)))
  NA_real_
}

# One variable-based statistic for one cell.
st_var_stat <- function(stat, x, w, weight_type) {
  ok <- !is.na(x)
  x <- x[ok]
  w <- w[ok]
  n <- length(x)
  if (stat == "first") return(if (n) x[1] else NA_real_)
  if (stat == "last") return(if (n) x[n] else NA_real_)
  if (stat == "rawsum") return(sum(x))
  if (stat == "count") return(if (weight_type == "aweight") as.numeric(n) else sum(w))
  if (!n) return(if (stat %in% c("sum", "total", "sumw")) 0 else NA_real_)
  if (weight_type == "aweight") w <- w * n / sum(w)
  W <- sum(w)
  if (stat == "sumw") return(W)
  if (stat %in% c("sum", "total")) return(sum(w * x))
  if (stat == "min") return(min(x))
  if (stat == "max") return(max(x))
  if (stat == "range") return(max(x) - min(x))
  q <- st_quantile_p(stat)
  if (!is.na(q)) return(st_wquantile(x, w, q))
  if (stat == "iqr") return(st_wquantile(x, w, 75) - st_wquantile(x, w, 25))
  m <- sum(w * x) / W
  if (stat == "mean") return(m)
  v <- if (W > 1) sum(w * (x - m)^2) / (W - 1) else NA_real_
  m2 <- sum(w * (x - m)^2) / W
  switch(stat,
    var = v,
    sd = sqrt(v),
    semean = sqrt(v / W),
    cv = sqrt(v) / m,
    skewness = (sum(w * (x - m)^3) / W) / m2^1.5,
    kurtosis = (sum(w * (x - m)^4) / W) / m2^2,
    NA_real_
  )
}

# Concordance/discordance matrices for gamma and tau-b (Agresti).
st_concordance <- function(O) {
  r <- nrow(O)
  k <- ncol(O)
  A <- D <- matrix(0, r, k)
  ri <- seq_len(r)
  ci <- seq_len(k)
  for (i in ri) for (j in ci) {
    A[i, j] <- sum(O[ri > i, ci > j]) + sum(O[ri < i, ci < j])
    D[i, j] <- sum(O[ri > i, ci < j]) + sum(O[ri < i, ci > j])
  }
  list(A = A, D = D, P = sum(O * A), Q = sum(O * D))
}

# Test statistics of a two-way table (options chi2, lrchi2, V, gamma,
# taub, exact).
st_tab2_tests <- function(O, want) {
  if (!any(unlist(want))) return(NULL)
  r <- nrow(O)
  k <- ncol(O)
  N <- sum(O)
  ok <- r >= 2 && k >= 2 && N > 0
  rs <- rowSums(O)
  cs <- colSums(O)
  E <- if (N > 0) outer(rs, cs) / N else O
  df <- (r - 1) * (k - 1)
  out <- list()
  add <- function(test, statistic = NA_real_, df = NA_real_, p = NA_real_,
                  ase = NA_real_) {
    out[[length(out) + 1L]] <<- tibble::tibble(
      test = test, statistic = statistic, df = df, p = p, ase = ase
    )
  }
  chi <- if (ok) sum(((O - E)^2 / E)[E > 0]) else NA_real_
  if (want$chi2) {
    add("Pearson chi2", chi, df, stats::pchisq(chi, df, lower.tail = FALSE))
  }
  if (want$lrchi2) {
    lr <- if (ok) 2 * sum((O * log(O / E))[O > 0]) else NA_real_
    add("likelihood-ratio chi2", lr, df, stats::pchisq(lr, df, lower.tail = FALSE))
  }
  if (want$V) {
    v <- if (!ok) {
      NA_real_
    } else if (r == 2 && k == 2) {
      (O[1, 1] * O[2, 2] - O[1, 2] * O[2, 1]) / sqrt(prod(rs) * prod(cs))
    } else {
      sqrt(chi / (N * min(r - 1, k - 1)))
    }
    add("Cram\u00e9r's V", v)
  }
  if ((want$gamma || want$taub) && ok) cd <- st_concordance(O)
  if (want$gamma) {
    if (ok) {
      P <- cd$P
      Q <- cd$Q
      g <- (P - Q) / (P + Q)
      ase <- 4 / (P + Q)^2 * sqrt(sum(O * (Q * cd$A - P * cd$D)^2))
      add("gamma", g, ase = ase)
    } else {
      add("gamma")
    }
  }
  if (want$taub) {
    if (ok) {
      wr <- N^2 - sum(rs^2)
      wc <- N^2 - sum(cs^2)
      w <- sqrt(wr * wc)
      tau <- (cd$P - cd$Q) / w
      v <- outer(rs * wc, rep(1, k)) + outer(rep(1, r), cs * wr)
      inner <- sum(O * (2 * w * (cd$A - cd$D) + tau * v)^2) -
        N^3 * tau^2 * (wr + wc)^2
      add("Kendall's tau-b", tau, ase = sqrt(max(inner, 0)) / w^2)
    } else {
      add("Kendall's tau-b")
    }
  }
  if (want$exact) {
    if (ok) {
      Oi <- round(O)
      # A larger workspace makes FEXACT run for minutes on medium-sized r x c
      # tables (it cannot be interrupted); with 2e6 such tables fail fast and
      # fall back to a Monte Carlo p-value instead. The simulation uses the
      # user's random number stream (as stats::fisher.test() does), so
      # set.seed() before the call makes it reproducible.
      p <- tryCatch(stats::fisher.test(Oi, workspace = 2e6)$p.value,
                    error = function(e) NULL)
      if (is.null(p)) {
        p <- stats::fisher.test(Oi, simulate.p.value = TRUE, B = 1e5)$p.value
        add("Fisher's exact (simulated)", p = p)
      } else {
        add("Fisher's exact", p = p)
      }
      if (r == 2 && k == 2) {
        p1 <- min(stats::fisher.test(Oi, alternative = "less")$p.value,
                  stats::fisher.test(Oi, alternative = "greater")$p.value)
        add("1-sided Fisher's exact", p = p1)
      }
    } else {
      add("Fisher's exact")
    }
  }
  dplyr::bind_rows(out)
}

st_format_tests <- function(tests) {
  if (is.null(tests) || !nrow(tests)) return(character())
  vapply(seq_len(nrow(tests)), function(i) {
    t <- tests[i, ]
    lab <- t$test
    if (!is.na(t$df)) lab <- sprintf("%s(%d)", lab, as.integer(t$df))
    left <- st_pad(lab, 26L)
    if (grepl("Fisher", t$test)) {
      return(sprintf("%s = %17s", left,
                     if (is.na(t$p)) st_na() else sprintf("%.3f", t$p)))
    }
    line <- sprintf("%s = %8s", left,
                    if (is.na(t$statistic)) st_na() else sprintf("%.4f", t$statistic))
    if (!is.na(t$p)) line <- paste0(line, sprintf("   Pr = %5.3f", t$p))
    if (!is.na(t$ase)) line <- paste0(line, sprintf("  ASE = %5.3f", t$ase))
    line
  }, character(1))
}
